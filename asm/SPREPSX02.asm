*          DATA SET SPREPSX02  AT LEVEL 158 AS OF 02/18/21                      
*PHASE SPSX02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE NSIWEEK                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SPLDCPTR                                                               
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* HWON SPEC-25164  03/15/19 ADD US-SBTK RECORDS                       *         
* AKAT SPEC-18882  01/05/18 MOVE MEDIA T CLRST FOR CANADIAN MED C REQ *         
* AKAT MOSTK-19    07/18/16 IHEART RADIO SUPPORT                      *         
***********************************************************************         
* LEV 64    JAN08/97 PUT IN LIST OF RECORDS CHANGED                             
* LEV 65-82 JAN09/97 STOP MOVEMENT OF BUYRECS ETC FOR MARKETS WITH              
*                    PW ACTIVITY. ALSO CHANGE THE STATION                       
*                    RECORD LENGTH                                              
* LEV 88    JAN29/97 CHECK FOR STEQ MARKET OVERRIDE BEFORE FINDING              
*                    NEXT AVAILABLE LINE NUMBER                                 
* LEV 89    JUL24/97 CHECK FOR PW LOCKED DOLLAR ELEMS                           
* LEV 95    SEP24/97 PUT OUT WILA PW CONSOLIDATED REPORT                        
* LEV 96    NOV06/97 PUT OUT WILA PW CONSOLIDATED REPORT BY PRODUCT             
* LEV 97    FEB05/98 MOVE PWTAB CLR, EXPAND CANADA NET FROM 5 TO 8 BITS         
* LEV 98    MAR31/98 FIX PWTAB AND EXPAND STATN REC EVEN IF IN NEW MKT          
* LEV 99    APR22/98 STOP PRINTING BLANK LINES                                  
* LEV 100   AUG14/98 ENABLE NWS - GET IT WORKING                                
* LEV 101   SEP29/98 DISABL NWS - FIX STATION LOCK-IN                           
* LEV 102   DEC10/98 ENABLE NWS, SHOW NEW MEDIA SAVED BUYS - X'08' ON           
* LEV 103   MAR11/99 BYPASS DELETED MATCHING STATUS RECORDS                     
* LEV 104   AUG17/99 BYPASS CANADIAN SPILL POINTERS                             
* LEV 105   AUG20/99 FIX RCWRITE BE TO BNE                                      
* LEV 106   AUG25/99 FIX NW100 BNE-BE MINERR, PT KEY NO STA IN HDR              
*                    AND BYPASS NWS HEADERS WITH NO STATIONS                    
* LEV 107   AUG26/99 FIX NW100 BNE-BE MINERR, PT KEY NO STA IN HDR              
* LEV 108   OCT11/99 FIX STEQ CALLING RTN USED R2, SHOULD BE R6                 
* LEV 109   SEP07/00 SET UP MINIO TO LOAD, NOT LINK                             
* LEV 110   SEP28/00 MOVE STATION LOCKIN RECORDS                                
* LEV 111   JAN25/01 FIX ST400 - MATCHING STATUS RECORDS                        
* LEV 113   FEB09/01 MOVE PW RECORDS IF COS2                                    
* LEV 114   MAR21/01 DEL CODE CKG FOR ERROR ON DMRDHI AFTER ST334               
* LEV 130   APR03/03 ADD PID TO MFIX REC                                        
* LEV 131   APR03/03 ADD SPGENDRBTC TO LIST OF RECS TO CHANGE                   
* LEV 134   JUL01/04 USE SPFMTINO TO DISPLAY STABINV                            
* LEV 135   OCT07/04 USE PACKED STA WITH SUFFIX IF CANADIAN CABLE REQ           
* LEV 138   SEP16/05 DON'T DIE IF BUY RECORD NOT FOUND                          
* LEV 143   JUL05/07 TURN OFF ADDED BY DESKTOP FLAG WHEN ADDING A BUY           
* LEV 144   JUL11/07 READ ALL CLIENTS UPFRONT AND PROCESS EVERYTHING            
*                    AT PROCREC SO IT CAN RUN MUCH MUCH FASTER                  
* LEV 145   AUG14/07 FIX RECUP BUG EXPOSED BY PREVIOUS VERSION                  
* LEV 146   DEC18/07 TURN OFF ADDED BY DESKTOP FLAG WHEN ADDING A BUY           
* LEV 147   DEC26/07 ADDED MASTER RECORD PASSIVE K KEYS                         
* LEV 148   JAN23/07 LEVEL 146 NOT MERGED IN!!!!                                
* LEV 151   OCT01/09 MAKE SURE 3RD & 4TH PARM TO LDCPTR IS 0                    
* LEV 152   MAR11/11 ADD NEW DTM RECORDS, RE-WRITE ORDER HISTORY CODE,          
*                    FIX STA BILLING BUCKET BUG AND ADD NINV RECORDS            
* LEV 153   JUL14/11 2-BYTE BUYLINE SUPPORT                                     
* LEV 153   NOV26/12 FIX DTM DEMO RECORDS MOVED TO WRONG LINE# DUE TO           
*                    NOT IGNORING SPILL                                         
*                                                                               
* QOPT5 = Y = TRACE ON                                                          
* QOPT1 = Y = WIFM DOESN'T CARE IF THERE IS PW - MOVE ANYWAY                    
*================================================================               
         TITLE 'SPSX02 - MARKET FIX PROGRAM - APPLICATION'                      
***********************************************************************         
*                                                                     *         
* RECORDS UPDATED                     SOURCE BOOK                     *         
*                                                                     *         
* SPOT FILE                                                           *         
*                                                                     *         
* SPOT BUYS                           SPGENBUY                        *         
* SPOT STATION BILLING BUCKET RECORD  SPGENSTAB                       *         
* NEW BUYERS WORKSHEET RECORDS -      SPNWSCAM, HDR, DTL              *         
* STATUS RECORDS                      SPGENSTAT                       *         
* STATION LOCKIN HEADER               SPGENSLH               0D72     *         
* STATION CLEARED STATUS              SPGENCLST                       *         
* SIR - STATION INVENTORY RECORDS     SPGENSIR                        *         
* MARKET FIX RECORDS                  SPGENMKTFX                      *         
* DARE BATCH RECORDS                  SPGENDRBTC                      *         
* DARE ORDER RECORDS                  SPGENDRORD                      *         
*                                                                     *         
* XPOT FILE                                                           *         
*                                                                     *         
* MATCHING STATUS RECORDS             SPGENMSR                        *         
* STATION LOCKIN HEADER  NEW          SPGENXLK    0D73                *         
* NEW INVOICE RECORDS                 SPGENSNV    0E03,0E83           *         
* CAN-DTM RECORDS                     SPGENCDORD  0D05,0D06,0D07,0D08 *         
* DARE ORDER HISTORY RECORDS          SPGENORHIS  0D1B                          
* US-SBTK REVISION/WORK RECORDS       SPGENDREV   0E10,0E11           *         
*                                                                     *         
* STRAFFIC FILE                                                       *         
*                                                                     *         
* TRAFFIC INST RECAP                  SPTRINST                        *         
* TRAFFIC BUYS                        SPTRBUY                         *         
* TRAFFIC SHIPPING RECAPS             SPTRSHIP                        *         
* TRAFFIC BUY ACTIVITY                SPTRTBAE                        *         
* TRAFFIC MARKET STATION LIST         SPTRSTAL                        *         
*                                                                     *         
***********************************************************************         
SPSX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*SPSX02*,R7,R8                                                 
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SXWORKD,RC                                                       
*                                                                               
* CONTROL SECTION *                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    SP100               UPDATE THE SPTDIR RECORDS                    
         CLI   MODE,REQFRST                                                     
         BE    SP000               GET THE NEW MARKET CODE                      
         CLI   MODE,REQLAST                                                     
         BE    SP500               UPDATE THE STATION FILE RECORDS              
         CLI   MODE,RUNLAST                                                     
         BE    SP600                                                            
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         L     RE,SSB                                                           
         USING SSBD,RE                                                          
         OI    SSOSTAT2,SSOSROLC   RECOVER OFFLINE COPIES/CHANGES               
         MVI   SSORPRG,C'M'        SET PROGRAM ID FOR RCVRHDR                   
         DROP  RE              *** ALL LETTERS SET IN FASSBOFF ***              
*                                                                               
         LA    R0,SPSXHDHK                                                      
         ST    R0,HEADHOOK                                                      
         STM   R7,RB,SPSXR7                                                     
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         DS    0D                                                               
PATCH1   DS    CL16                                                             
*                                                                               
*                                                                               
*                                                                               
* THIS SECTION GETS THE NEW MARKET NUMBER FROM QBOOK1 *                         
*  AND CONVERTS IT INTO BINARY AND SAVES IT IN NEWMKT *                         
*                                                                               
* *** REQFRST ***                                                               
*                                                                               
SP000    LA    R0,4                                                             
         LA    R1,QBOOK1                                                        
         MVI   MKTFFLAG,0          RESET THE MARKET FIX ERROR FLAG              
SP010    CLI   0(R1),C'0'          IS THIS A VALID MARKET DIGIT?                
         JL    BADMKT                                                           
         CLI   0(R1),C'9'                                                       
         JH    BADMKT                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,SP010                                                         
         L     R5,=A(SPBUFF)                                                    
*                                                                               
         MVI   BUYSW,C'N'          NO BUY UPDATES YET                           
         MVI   CANAGY,C'N'                                                      
         MVI   RQDEL,C'Y'          PROCESS DELETED BUY RECORDS                  
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   XFF,X'FF'           INIT TO X'FF'S                               
         MVC   XFF+1(L'XFF-1),XFF  INIT TO X'FF'S                               
*                                                                               
* CLEAR REQUEST ACCUMS                                                          
*                                                                               
         LA    R0,NACCUMS                                                       
         L     R1,=A(ACCUMS)                                                    
         ZAP   0(4,R1),=P'0'                                                    
         LA    R1,L'ACCUMS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
* CLEAR WILA PW TABLE                                                           
*                                                                               
         LA    R0,PWTABSIZ                                                      
         L     R1,=A(PWTAB)                                                     
         USING PWTABD,R1                                                        
         XC    PWTABENT,PWTABENT                                                
         LA    R1,PWTABNXT                                                      
         BCT   R0,*-10                                                          
         DROP  R1                                                               
*                                                                               
         PACK  DUB,QBOOK1                                                       
         CVB   R0,DUB                                                           
         STH   R0,NEWMKT                                                        
*                                                                               
         CLI   QSTA+4,C'/'         IS THIS A CABLE STATION                      
         BE    SP010A1                                                          
         CLI   QSTA+4,C'C'         CHANGE MEDIA C                               
         BNE   SP010A2                                                          
         CLI   QMED,C'R'           MEDIA R?                                     
         BE    SP010A2             YES - BAND C IS IHEART - NOT CANADA!         
SP010A1  MVI   QSTA+4,C'T'         FORCE MEDIA TO T SO MSPACK WILL WORK         
*                                                                               
SP010A2  GOTO1 MSPACK,DMCB,QMKT,QSTA,OLDMSTA                                    
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    *+8                  NO                                          
         NI    OLDMSTA+4,X'80'     DO ALL NETS FOR CABLE HEAD                   
*                                                                               
         CLC   RCPROG,=C'CM'                                                    
         BNE   SP010A                                                           
         CLC   QOPT2(2),=C'  '                                                  
         BE    *+8                                                              
         BRAS  RE,BLDMKSTA                                                      
*                                                                               
*  WE'RE CHECKING IF WE HAVE A PREVIOUS "OPPOSITE" REQUEST THAT'S...            
*  ...WITHIN THE SAME WEEK IN THE MARKET FIX RECORDS   MHC  06/11/02            
SP010A   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTFXRCD,R4                                                      
         MVI   MKFKTYPE,MKFKTYPQ   (X'0D') RECORD TYPE                          
         MVI   MKFKSBTY,MKFKSBTQ   (X'6D') SUBRECORD TYPE                       
         MVC   MKFKAGMD,BAGYMD     AGENCY/MEDIA                                 
         CLC   QCLT,=CL3'ALL'                                                   
         BNE   SP011                                                            
         MVC   MKFKCLT,=XL2'FFFF'                                               
         B     SP012                                                            
*                                                                               
SP011    GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         MVC   MKFKCLT,BCLT        CLIENT (PACKED)                              
*                                                                               
SP012    MVC   MKFKOMKT,NEWMKT     SEE IF THE "REVERSE" WAS REQUESTED           
         MVC   MKFKNMKT,OLDMKT     ...ALREADY                                   
         MVC   BSTA,OLDMSTA+2      SAVE OFF BINARY STATION                      
         MVC   MKFKSTA,BSTA        STATION (PACKED)                             
         CLC   RCPROG,=C'CM'       CANADIAN CABLE REQ?                          
         BNE   *+14                NO                                           
         L     R1,=A(CBLMKSTA)                                                  
         MVC   MKFKSTA,2(R1)       STATION WITH SUFFIX                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SP015                                                            
*                                                                               
         MVC   SVAREC,AREC         SAVE OFF AREC                                
         MVC   AREC,=A(SPBUFF)                                                  
         GOTO1 GET                                                              
         L     R4,AREC                                                          
         LA    R6,MKFFRST                                                       
         LR    R1,R6               USE R1 TO HELP FIND LAST ELEMENT             
SP013    LLC   R0,1(R6)                                                         
         AR    R1,R0               R1 POINTS TO NEXT ELEM (IF EXISTS)           
         CLI   0(R1),MKFIDELQ      HAVE MORE DETAILS ELEMENTS (X'10')?          
         BNE   SP014                - NOPE, WE GOOD TO GO                       
         LR    R6,R1                                                            
         B     SP013                                                            
*                                                                               
         USING MKFIDELD,R6                                                      
SP014    MVI   GETDAY,1            MAKE SUNDAY FIRST DAY OF WEEK                
*                                                                               
         GOTO1 DATCON,DMCB,(2,MKFIDDAT),(0,EDATE)   SET YYMMDD DATE             
         GOTO1 =V(NSIWEEK),DMCB,EDATE,GETDAY,ADDAY,DATCON                       
         MVC   WEEK1(1),0(R1)      WEEK NUMBER IN BINARY FOR PREV REQ           
         MVC   WEEK1+1(1),4(R1)    YEAR NUMBER IN BINARY FOR PREV REQ           
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,EDATE)   SET THE EBCDIC DATE           
         GOTO1 =V(NSIWEEK),DMCB,EDATE,GETDAY,ADDAY,DATCON                       
         MVC   WEEK2(1),0(R1)      WEEK NUMBER IN BINARY FOR TODAY              
         MVC   WEEK2+1(1),4(R1)    YEAR NUMBER IN BINARY FOR TODAY              
*                                                                               
         MVC   AREC,SVAREC         RESTORE AREC                                 
         XC    SAVEKEY,SAVEKEY                                                  
         CLC   WEEK1,WEEK2         ARE THEY BOTH IN THE SAME WEEK?              
         BNE   SP015                NO, EVERYTHING IS COOL                      
*   THE REVERSE MARKET FIX EXISTS (SAME WEEK)                                   
         OI    MKTFFLAG,RVRSMKTF   X'80'                                        
         J     MKTFIXNG                                                         
         DROP  R4,R6                                                            
*  DONE CHECKING MARKET FIX RECORDS... CONTINUE                                 
SP015    XC    SAVEKEY(40),SAVEKEY                                              
         XC    M(4),M                                                           
         MVC   KEY(17),=17C'0'                                                  
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,ADMARKET                         
         TM    DM3,X'FD'                                                        
         JNZ   ERROR1              ERROR                                        
         L     R4,ADMARKET                                                      
         USING MKTRECD,R4                                                       
         CLC   KEY(8),0(R4)                                                     
         JNE   ERROR1              ERROR                                        
         MVC   MARKETNM(24),MKTNAME  GET THE NAME OF THE MARKET                 
         MVC   KEY+2(4),QBOOK1                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,ADMARKET                         
         TM    DM3,X'ED'                                                        
         JNZ   ERROR1              ERROR                                        
         MVC   NEWNAME(24),SPACES                                               
         XC    MKTALPH,MKTALPH                                                  
         CLC   KEY(8),0(R4)                                                     
         BNE   SP020                                                            
         MVC   NEWNAME(24),MKTNAME  GET THE NAME OF THE NEW MARKET              
         MVC   MKTALPH,MKTALF                                                   
         DROP  R4                                                               
*                                                                               
SP020    L     R3,ADAGY                                                         
         USING AGYHDRD,R3                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   SPCL1ST                                                          
         MVI   CANAGY,C'Y'         FLAG AS CANADIAN AGENCY REQUEST              
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   SPCL1ST             NO - EXIT                                    
         MVC   AGYMDT,BAGYMD       SET AGENCY/MEDIA T                           
         NI    AGYMDT,X'F0'        STRIP MEDIA C                                
         MVC   AGYMDN,AGYMDT       COPY AGENCY NIBBLE FOR MEDIA N               
         OI    AGYMDT,X'01'        MEDIA T BITS                                 
         OI    AGYMDN,X'03'        MEDIA N BITS                                 
         XC    DUB,DUB             CLEAR DUB                                    
         MVC   DUB(4),QSTA         MOVE OLD STATION INTO DUB                    
         MVI   DUB+4,C'N'          PACK IT FOR MEDIA N                          
         GOTO1 MSPACK,DMCB,QMKT,DUB,OLDMSTAN                                    
*                                                                               
         B     SPCL1ST                                                          
         DROP  R3                                                               
*                                                                               
* SP100 - THIS SECTION REPLACES THE MARKET NUMBER *                             
*  FOR AMERICAN BUYS WITH THE NEW MARKET NUMBER   *                             
*                                                                               
* *** PROCBUY ***                                                               
*                                                                               
SP100    MVC   HALF,KEY+1          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   EXIT                NO                                           
*                                                                               
         GOTO1 =A(ESTCHK),DMCB,BUYKCLT-BUYKEY+KEY,BUYKEST-BUYKEY+KEY            
*                                                                               
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   BUYSW,C'Y'          SET THE 'BUY PROCESSED' SWITCH               
*                                                                               
         CLI   CANAGY,C'Y'         ARE WE DEALING WITH A CANADIAN AGY?          
         BE    SP150                YES                                         
*                                                                               
SP110    CLI   KEY+10,0                                                         
         BE    SP200               ACTIVE POINTER                               
*                                                                               
         MVI   TRACECDE,1                                                       
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTER                   
         B     EXIT                                                             
*                                                                               
* SP150 - THIS SECTION REPLACES THE MARKET NUMBER  *                            
*  FOR COMBINED TV BUYS WITH THE NEW MARKET NUMBER *                            
*                                                                               
SP150    CLC   RCPROG,=C'CM'       IF IT'S CANADIAN CABLE REQ                   
         BNE   SP160                                                            
         CLC   QOPT2(2),=C'  '                                                  
         BE    SP160                                                            
         LAY   R1,CBLMKSTA                                                      
         CLC   0(5,R1),KEY+4       MATCH ON STATION W/SUFFIX (PWES/TO)          
         BNE   EXIT                                                             
*                                                                               
SP160    IC    R0,KEY                                                           
         N     R0,=X'0000000F'                                                  
         CHI   R0,8                IS THIS A COMBINED BUY?                      
         BNE   SP110                NO.                                         
         CLI   KEY+3,X'FF'         TEST ACTIVE POINTER                          
         BNE   EXIT                 NO - IGNORE                                 
         TM    KEY+10,X'80'        THIS A SPILL POINTER                         
         BO    EXIT                 YES - IGNORE                                
* DELETE THE POL POINTER IN THE ORIGINAL MEDIA                                  
         MVC   SAVEKEY2(18),KEY                                                 
         NI    KEY,X'F0'           DROP MEDIA                                   
         OC    KEY(1),KEY+10       SET TO ORIGINAL MEDIA                        
         MVI   KEY+10,0            KEY+11 & KEY+12 REMAINS THE SAME             
*****    MVI   KEY+12,1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SP170                                                            
         MVI   TRACECDE,38                                                      
         BAS   RE,SPDEL                                                         
*                                                                               
SP170    MVC   KEY(18),SAVEKEY2    RESTORE PREVIOUS KEY                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SP200                                                            
*&&DO                                                                           
* THIS CODE IS NOT NEEDED. THE BUY RECORD IS DELETED.                           
* ALL PROGRAMS EXPECT NON-DELETED DIRECTORY WITH DELETED FILE                   
* MH 30AUG04                                                                    
         MVC   DUB(1),KEY          RESTORE ORIGINAL MEDIA IN KEY                
         NI    DUB,X'F0'                                                        
         OC    DUB(1),KEY+10                                                    
         MVC   KEY(1),DUB                                                       
         MVI   KEY+10,X'FF'                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACECDE,4                                                       
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
*                                                                               
         MVC   KEY(13),SAVEKEY2                                                 
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         OI    DMOUTBTS,X'02'                                                   
         CLC   KEYSAVE(13),KEY                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
* THIS SECTION REPLACES THE ACTIVE POINTERS *                                   
*                                                                               
SP200    XC    STEQMKT,STEQMKT                                                  
         CLC   SAVEKEY(10),KEY     1ST TIME FOR THIS CL/PR/MK/ST/ES             
         BE    SP210                NO.                                         
         BAS   RE,HIGHBUY          CALCULATE HIGHEST BUY NUMBER FOR STA         
         CLI   USRSW1,C'Y'         ARE THERE MORE THAN 255 BUYS?                
         BE    EXIT                 YES.                                        
*                                                                               
SP210    MVC   SAVEKEY(13),KEY                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,2                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         CLI   MOVECODE,C'N'       DONE IF RECORD NOT BEING MOVED               
         BE    SP314                                                            
*                                                                               
         L     RE,ADBUY                                                         
         CLC   KEY+4(2),4(RE)      MAKE SURE NOT A SPILL MARKET                 
         BNE   EXIT                                                             
         TM    15(RE),X'80'        TEST BUY IS DELETED                          
         BO    EXIT                                                             
*                                                                               
         GOTO1 BINSRCH,CLTBPARS,KEY+1                                           
         CLI   0(R1),1             RECORD FOUND?                                
         BE    SP216               NO                                           
         SR    R3,R3                                                            
         ICM   R3,7,1(R1)          RECORD FOUND?                                
         BZ    SP216               NO                                           
         USING CLTBUFFD,R3                                                      
         CLI   CBEXTRA,C'A'        MUST BE MARKET GROUP SCHEME                  
         BL    SP216                                                            
         CLI   CBEXTRA,C'Z'        MUST BE MARKET GROUP SCHEME                  
         BH    SP216                                                            
         MVI   BYTE,X'70'          SEE IF ID ELEM                               
         L     R6,ADBUY                                                         
         BAS   RE,GETEL                                                         
         BNE   SP216               NOPE                                         
*                                                                               
         CLC   CBEXTRA,3(R6)       SAME MARKET GROUP                            
         BNE   SP216                                                            
         DROP  R3                                                               
*                                                                               
         MVC   SVEQMKT,STEQMKT     SAVE CURRENT OVERRIDE                        
         BRAS  RE,STEQ             GO READ STATION EQUIV REC                    
         CLC   STEQMKT,4(R6)       TEST IN RIGHT MARKET NOW                     
         BE    SP380               YES - ON TO NEXT BUY                         
         CLC   SVEQMKT,STEQMKT     TEST SAME OVERRIDE                           
         BE    SP216                                                            
         BAS   RE,HIGHBUY          NEED TO FIND STARTING LINE                   
*                                                                               
SP216    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP220                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,ADBUY,DMWORK                  
         TM    DM3,X'FD'                                                        
         BZ    SP220                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
SP220    L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
         CLI   CANAGY,C'Y'         FOR CAN AGY ONLY                             
         BNE   SP224                                                            
*                                                                               
         BAS   RE,SPCNETWK         FIX THE CANADIAN NETWORK ELEMENT             
*                                                                               
SP224    MVI   SVOLDLIN,0          1 BYTE BUYLINE                               
         MVC   SVOLDLIN+1(1),10(R2)                                             
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+10                NO                                           
         MVC   SVOLDLIN,10(R2)     YES - MOVE 2 BYTES OF BUYLINE                
         OC    M,M                                                              
         BZ    SP300               NO NEED TO CORRECT THE LINE NUMBERS          
*                                                                               
         CLI   CANAGY,C'Y'         FOR CAN AGY ONLY, NO NETWORK                 
         BNE   SP230                                                            
         MVI   WORK,0                                                           
         MVN   WORK(1),0(R2)                                                    
         CLI   WORK,3              IF MEDIA=NETWORK, NO SUBLINE CHANGE          
         BE    SP300                                                            
*                                                                               
SP230    LLC   R3,10(R2)           GET THE BUY LINE NUMBER                      
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         ICM   R3,3,10(R2)         YES - MOVE 2 BYTES OF BUYLINE                
         XR    R6,R6                                                            
         ICM   R6,3,M                                                           
         AR    R3,R6                                                            
         STC   R3,10(R2)           REPLACE WITH NEW BUY LINE NUMBER             
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         STCM  R3,3,10(R2)         YES - REPLACE WITH NEW BUY LINE NUM          
         BAS   RE,SPPKG            FIX THE PACKAGE ELEMENT, IF ANY              
*                                                                               
* THIS SECTION IS COMMON TO BOTH AMERICAN AND CANADIAN MARKET FIX *             
*                                                                               
SP300    DS    0H                                                               
*                                                                               
* DELETE ANY SPILL ELS WITH MKT EQ TO NEW MKT                                   
*                                                                               
         LA    R6,BDELEM                                                        
SP304    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),03                                                         
         BNE   SP306                                                            
         OC    STEQMKT,STEQMKT     IS THERE A STATION EQUIV MARKET              
         BZ    SP305                                                            
         CLC   STEQMKT,4(R6)       SAME AS SPECIAL MARKET                       
         BE    SP305A                                                           
         B     SP306                                                            
*                                                                               
SP305    CLC   NEWMKT,4(R6)        SPILL SAME AS NEWMKT                         
         BNE   SP306                                                            
SP305A   GOTO1 RECUP,DMCB,(R2),(R6)                                             
*                                                                               
         BRAS  RE,DEMSWERR                                                      
         IC    RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+53(0),1(R1)                                                    
         GOTO1 REPORT                                                           
*                                                                               
SP306    CLI   0(R6),0                                                          
         BNE   SP304                                                            
*                                                                               
* ADD MARKET FIX ELEMENT FOR AUDIT TRAIL *                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING MFXELEM,R6                                                       
         MVI   MFXCODE,X'9C'                                                    
         MVI   MFXLEN,MFXLENQ                                                   
         MVC   MFXMKT,OLDMKT                                                    
         MVC   MFXDATE,TODAYP                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,13(R2)                                                      
*                                                                               
         LR    RF,RE               SEE IF ROOM IN REC FOR ELEM                  
         AHI   RF,MFXLENQ                                                       
         CHI   RF,5975             MAX REC LENGTH                               
         BH    SP307                                                            
         LA    R3,0(R2,RE)                                                      
*                                                                               
         GOTO1 RECUP,DMCB,(R2),(R6),(R3)                                        
         DROP  R6                                                               
*                                                                               
SP307    LA    RF,ACCUBUYQ                                                      
         BAS   RE,BUMPACC                                                       
*                                                                               
         MVC   4(2,R2),NEWMKT      SET THE NEW MARKET CODE                      
*                                                                               
         OC    STEQMKT,STEQMKT     IS THERE A STATION EQUIV MARKET              
         BZ    *+10                                                             
         MVC   4(2,R2),STEQMKT     SET THE SPECIAL NEW MARKET CODE              
*                                                                               
         NI    15(R2),X'7F'        'UNSET' THE 'DELETED' FLAG                   
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   SP308                                                            
         MVC   KEY+4(2),NEWMKT                                                  
*                                                                               
         OC    STEQMKT,STEQMKT     IS THERE A STATION EQUIV MARKET              
         BZ    *+10                                                             
         MVC   KEY+4(2),STEQMKT    SET THE SPECIAL NEW MARKET CODE              
*                                                                               
         MVI   TRACECDE,8                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
SP308    NI    BDSTAT3-BUYREC(R2),X'FF'-BDST3_DSKADD                            
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP310                NO                                          
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,ADBUY,DMWORK                  
         TM    DM3,X'FD'                                                        
         BZ    SP310                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
SP310    L     R5,=A(SPBUFF)                                                    
         GOTO1 =V(LDCPTR),DMCB,ADBUY,(R5),0,0                                   
*                                                                               
SP314    GOTO1 =A(SPPRINT)                                                      
         MVI   USRSW2,C'Y'                                                      
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP380               DONE                                         
         CLI   BUYKEY+3,X'FF'                                                   
         BE    SP320                                                            
         CLI   BDTIME,0            TEST FOR PIGGYBACK                           
         BE    SP380                NO.                                         
         LA    R5,18(R5)           A PASSIVE POINTER WAS ALREADY ADDED          
         B     SP330                                                            
*                                                                               
SP320    CLI   BDMASPRD,0          IS THERE A POL MASTER PRODUCT CODE           
         BE    SP330                NO.                                         
         CLC   3(1,R5),BDMASPRD    IS THIS THE POOL MASTER PRD CODE?            
         BNE   SP330                NO.                                         
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP380                                                            
         CLI   BDMASPRD+1,0        ARE THERE 2 POOL MASTER PRD CODES?           
         BE    SP330                NO.                                         
         CLC   3(1,R5),BDMASPRD+1  IS THIS THE 2ND POOL MASTER PRD CODE         
         BNE   SP330                NO.                                         
         LA    R5,18(R5)                                                        
*                                                                               
SP330    CLI   0(R5),0                                                          
         BE    SP380                                                            
*                                                                               
SP340    MVC   KEY(13),0(R5)       SET KEY - DISK ADR ALREADY IN +14            
         NI    KEY+13,X'10'        CLEAR EXCEPT FOR 2-BYTE BUYLINE              
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   SP350                                                            
         MVI   TRACECDE,6                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
SP350    CLI   RCWRITE,C'Y'                                                     
         BNE   SP370                                                            
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+12                NO                                           
         CLI   KEY+10,X'FF'                                                     
         B     *+10                                                             
         CLC   =X'FF00',KEY+10                                                  
         BNE   SP360                                                            
         CLC   KEY+3(1),BDMASPRD                                                
         BE    SP370                                                            
         CLC   KEY+3(1),BDMASPRD+1                                              
         BE    SP370                                                            
*                                                                               
SP360    GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         TM    DM3,X'DD'                                                        
         BZ    SP370                                                            
         DC    H'0'                ERROR                                        
*                                                                               
SP370    LA    R5,18(R5)                                                        
         CLI   0(R5),0             IS THIS THE END OF THE SPBUFF?               
         BNE   SP340                NO.                                         
*                                                                               
SP380    MVC   KEY(13),SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  READ ALL CLT RECS UPFRONT AND BUFFER THE FOLLOWING FOR EACH CLIENT *         
*  ------------------------------------------------------------------ *         
*    1) CLIENT CODE                                                   *         
*    2) OFFICE                                                        *         
*    3) BUY-ID                                                        *         
*    4) FLAG IF CLT SPECIFIC CBLMKT REC EXISTS (DON'T PROCESS CLT)    *         
*    5) FLAG IF CLT SPECIFIC MASTER REC EXISTS (DON'T PROCESS CLT)    *         
*    6) FLAG IF CLIENT IS A PW CLIENT                                 *         
*                                                                     *         
*   IF CLIENT IS A PW CLIENT, MAINTAIN A SEPARATE BUFFER FOR EACH     *         
*   CLIENT/ESTIMATE AND DON'T MOVE RECORDS FOR THOSE ESTIMATES.       *         
*                                                                     *         
***********************************************************************         
SPCL1ST  MVI   MOVECODE,0           INIT RECORD MOVE CODE                       
*                                                                               
         XC    CLTBPARS(4),CLTBPARS                                             
         XC    CLTBPARS+8(4),CLTBPARS+8                                         
         L     RE,CLTBPARS+4        CLEAR THE CLIENT BUFFER                     
         CR    RF,RF                                                            
         ICM   RF,15,=A(CLTBUFFL)                                               
         XCEFL                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD      AGENCY/MEDIA                                
         CLC   QCLT,=CL3'ALL'       ALL CLIENT REQUEST?                         
         BE    *+10                 YES                                         
         MVC   KEY+2(2),BCLT        NO - PROCESS SINGLE CLIENT                  
*                                                                               
SPCL00   GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE       SAME A/M?                                   
         BNE   SP400                NO, DONE READING CLIENT RECORDS             
         OC    KEY+4(9),KEY+4       IS THIS REALLY A CLIENT RECORD?             
         BZ    SPCL20               YES                                         
         XC    KEY+4(9),KEY+4       NO, CLEAR EVERYTHING AFTER CLIENT           
         B     SPCL15               AND READ NEXT CLIENT RECORD                 
*                                                                               
SPCL10   CLC   QCLT,=CL3'ALL'       ALL CLIENT REQUEST                          
         BNE   SP400                NO, PROCESSED THE CLT WE NEEDED             
         L     R1,ADCLT             CLIENT RECORD JUST PROCESSED                
         XC    KEY,KEY              CLEAR KEY                                   
         MVC   KEY(13),0(R1)        CLIENT KEY JUST PROCESSED                   
*                                                                               
SPCL15   XR    RE,RE                                                            
         ICM   RE,3,KEY+2           CURRENT CLIENT                              
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+2           NEXT POSSIBLE CLIENT                        
         B     SPCL00                                                           
*                                                                               
SPCL20   MVC   AREC,ADCLT           GET CLIENT RECORD IN ADCLT                  
         GOTO1 GET                  GET THE CLIENT RECORD                       
*                                                                               
         XC    WORK,WORK            BUILD CLIENT BUFFER HERE                    
         USING CLTBUFFD,R2                                                      
         LA    R2,WORK                                                          
*                                                                               
         L     R1,ADCLT             CLIENT RECORD                               
         USING CLTHDRD,R1                                                       
         MVC   CBCLT,CKEYCLT        BINARY CLIENT                               
         MVC   CBOFF,COFFICE        CLIENT OFFICE                               
         MVC   CBEXTRA,CEXTRA+2     BUY ID                                      
         OC    CPWPCT,CPWPCT        PW CLIENT?                                  
         BZ    *+8                  NO                                          
         OI    CBFLAG,CBPW          YES - SET PW FLAG                           
         DROP  R1                                                               
*                                                                               
         CLC   QCLT,=C'ALL'         ALL CLT REQUEST                             
         BNE   SPCL100              NO, CLT SPECIFIC, NO CK NEEDED              
*                                                                               
         CLC   RCPROG,=C'CM'        CANADIAN CABLE                              
         BNE   SPCL50                                                           
*                                                                               
         LA    R6,KEY              ESTABLISH CBLMKT\KEY                         
         USING NDEFKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   NDEFKTYP,=XL2'0D11'                                              
         MVC   NDEFKAGY,QAGY                                                    
         MVC   NDEFKNET,QSTA       SET CALL LETTERS                             
         MVC   NDEFKCLT,CBCLT      SET CLIENT IN BASIC KEY                      
*                                                                               
         GOTO1 HIGH                                                             
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         CLC   KEY(13),KEYSAVE     NO CLIENT SPECIFIC                           
         BNE   SPCL100                                                          
         MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
         L     R6,AREC                                                          
         GOTO1 GET                                                              
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         MVI   BYTE,NDEFELQ        X'01'                                        
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
SPCL30   BRAS  RE,NEXTEL                                                        
         BNE   SPCL100             NO MATCHES                                   
*                                                                               
         CLI   0(R6),NDEFELQ                                                    
         BNE   SPCL30                                                           
         CLC   QOPT2(2),2(R6)      FIND RIGHT SUFFIX                            
         BNE   SPCL30                                                           
         CLC   OLDMKT,4(R6)        IF OLD MARKET IS A MATCH                     
         BE    SPCL100             THEN OK TO CHANGE                            
*                                                                               
         OI    CBFLAG,CBCBLMKT                                                  
         B     SPCL100                                                          
*                                                                               
SPCL50   MVC   KEY1(17),=17C'0'    READ FOR CLIENT SPECIFIC STAREC              
         MVI   KEY1,C'S'                                                        
         MVC   KEY1+1(1),QMED                                                   
         MVC   KEY1+2(5),QSTA                                                   
*                                                                               
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+8                  NO.                                         
         MVI   KEY1+6,C'C'         YES. ADJUST BAND                             
*                                                                               
         MVC   KEY1+7(2),QAGY                                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,CBCLT,CLT                                            
         MVC   KEY1+9(3),CLT                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY1,ADSTAT                          
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         L     R1,ADSTAT                                                        
         CLC   KEY1(13),0(R1)      CLT SPECIFIC                                 
         BNE   SPCL100              NO                                          
*                                                                               
         MVC   P+5(3),CLT                                                       
         MVC   P+10(30),=C'CLIENT SPECIFIC STATION RECORD'                      
         MVC   P+41(7),=C'(MARKET'                                              
         MVC   P+49(4),SMKT-STARECD(R1)                                         
         MVI   P+53,C')'                                                        
         MVC   P+55(13),=C'NOTHING MOVED'                                       
         GOTO1 REPORT                                                           
*                                                                               
         OI    CBFLAG,CBCLTSTA                                                  
         MVC   CBMKT,SMKT-STARECD(R1)                                           
*                                                                               
SPCL100  GOTO1 BINSRCH,CLTBPARS,(1,WORK)                                        
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
*        IF CLIENT USES PW FEATURE                                              
*              BUILD TABLE OF PRD/EST THAT HAVE PW                              
*              RECORDS FOR THIS STATION'S MARKET                                
*              LATER WE WILL NOT MOVE RECORDS FOR THESE PRD/ESTS                
*                                                                               
SPCLPW   TM    CBFLAG,CBPW         PW PCT SET?                                  
         BZ    SPCL10              NO, IGNORE                                   
*                                                                               
         CLI   QOPT1,C'Y'          IGNORE PW PCT?                               
         BE    SPCL10              YES                                          
*                                                                               
*        READ PW RECORDS AND BUILD PRD/EST TABLE                                
*                                                                               
         XC    KEY,KEY             ESTABLISH PW KEY                             
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
*                                                                               
         MVC   PWKTYP,=X'0D7A'     SET RECORD TYPE                              
         MVC   PWKAGMD,BAGYMD      SET AGY/MEDIA                                
         MVC   PWKCLT,CBCLT        SET CLIENT                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD FOR CLIENT                 
*                                                                               
SPCLPWLP CLC   PWFKEY(PWKPRD-PWFKEY),KEYSAVE  CLT CHANGE?                       
         BNE   SPCL10                         YES, READ NEXT CLT REC            
*                                                                               
         CLC   PWKMKT,OLDMKT       SKIP IF NOT THIS MARKET                      
         BNE   SPCLPWCN                                                         
*                                                                               
SPCLPW10 MVC   SVAREC,AREC                                                      
         MVC   AREC,ADBILL                                                      
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         MVC   AREC,SVAREC                                                      
         L     R6,ADBILL                                                        
         MVI   BYTE,06             SEE IF ANY LOCKED DOLLAR ELEMS               
         BAS   RE,GETEL                                                         
         BNE   SPCLPWCN            IGNORE THIS ONE                              
*                                                                               
* IF ANY LOCKED DOLLARS, STOP ANY MOVE                                          
* NOTE THAT COS2 RECS HAVE ONLY 04 ELEMENTS SO THIS WON'T STOP MOVE             
*                                                                               
         USING PWDOLEL,R6                                                       
SPCLPW20 OC    PWDOLWG,PWDOLWG                                                  
         BNZ   SPCLPW30                                                         
         OC    PWDOLWN,PWDOLWN                                                  
         BNZ   SPCLPW30                                                         
         OC    PWDOLCG,PWDOLCG                                                  
         BNZ   SPCLPW30                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    SPCLPW20                                                         
         B     SPCLPWCN            IGNORE IT                                    
         DROP  R6                                                               
*                                                                               
*        ADD NEW ENTRY TO TABLE                                                 
*                                                                               
SPCLPW30 LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING PWBUFFD,R2                                                       
         MVC   PWBCLT,PWKCLT                                                    
         MVC   PWBEST,PWKEST                                                    
         DROP  R2                                                               
*                                                                               
         GOTO1 BINSRCH,PWBPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   QOPT5,C'Y'          IF TRACING                                   
         BNE   *+12                                                             
         MVI   TRACECDE,32         SET RECORD ID                                
         BRAS  RE,SPTRACE          PRINT KEY                                    
*                                                                               
SPCLPWCN LA    R6,KEY                                                           
         MVC   PWKSTA,XFF          FORCE NEXT MKT                               
         GOTO1 HIGH                READ NEXT RECORD                             
         B     SPCLPWLP                                                         
         DROP  R6                                                               
*                                                                               
* PROCESS NON-BUY RECORDS AFTER WE BUILD CLIENT TABLES                          
*                                                                               
SP400    MVC   SVAREC,AREC         SAVE AREC                                    
         MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
         L     R6,AREC                                                          
*                                                                               
         BRAS  RE,FIXPW            FIX PW RECORDS                               
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OP                       
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
*                                                                               
         BAS   RE,T100             TRAFFIC INSTRUCTION RECAPS                   
*                                                                               
         BAS   RE,T200             TRAFFIC BUYS                                 
         BAS   RE,T300             TRAFFIC SHIPPING RECAPS                      
         BAS   RE,T400             TRAFFIC BUY ACTIVITY                         
         BAS   RE,T500             TRAFFIC MARKET STATION LIST                  
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
         BAS   RE,T700             STATUS RECORDS                               
*                                                                               
         BAS   RE,T800             STATION BILLING BUCKETS                      
         BRAS  RE,T900             DARE BATCH RECORDS                           
         BAS   RE,ST100            STATION LOCKIN HEADER - 0D72                 
*                                                                               
         BAS   RE,ST200            STATION CLEARED STATUS- 0D76                 
*                                                                               
         BRAS  RE,ST300     XSP    STATION LOCKIN HEADER - 0D73                 
*                                                                               
         BRAS  RE,ST400            SPOTPAK MATCHING STATUS RECORD               
*                                                                               
         BRAS  RE,T1000            DARE ORDER RECORDS                           
         BRAS  RE,T2000            XSP ORDER HISTORY RECORDS                    
*                                                                               
         CLI   CANAGY,C'Y'         CAN AGY?                                     
         BNE   *+12                                                             
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BE    SP410                                                            
*                                  XSPOT US-SBTK REVISION RECS X'0E10'          
         GOTOR USBTKREC,DMCB,DRVKSUBQ,ACCUUSRQ,50                               
*                                  XSPOT US-SBTK WORK RECS X'0E11'              
         GOTOR USBTKREC,DMCB,DWKKSUBQ,ACCUUSWQ,51                               
*                                                                               
SP410    BRAS  RE,NINV             XSP NEW INVOICE RECS X'0E03'                 
*                                                                               
         MVC   DATADISP,=H'42'     DATADISP NOW 42                              
*                                  XSPOT CANADIAN DTM RECORDS                   
         BRAS  RE,DTMPRG           -DTM PROG RECORDS X'0D05'                    
         BRAS  RE,DTMDEM           -DTM DEMO RECORDS X'0D06'                    
         BRAS  RE,DTMORD           -DTM ORDR RECORDS X'0D07'                    
***      BRAS  RE,DTMGOL           -DTM GOAL RECORDS X'0D08'                    
         MVC   DATADISP,=H'24'     DATADISP NOW 24                              
*                                                                               
         MVC   AREC,SVAREC         RESTORE AREC                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* UPDATE MARKET IN TRAFFIC INSTRUCTION RECAP RECORDS                            
*                                                                               
         USING INSRECD,R6                                                       
T100     NTR1                                                                   
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         MVC   SAVEKEY3(2),=X'0A24'                                             
         MVC   SAVEKEY3+2(1),BAGYMD                                             
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                 NO.                                         
         NI    SAVEKEY3+2,X'FF'-X'08'                                           
         OI    SAVEKEY3+2,X'01'                                                 
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
T110     GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
*                                                                               
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T111                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    T111                YES                                          
         MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T110                                                             
*                                                                               
T111     MVC   KEY+6(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA               
         XC    KEY+11(2),KEY+11                                                 
*                                                                               
T115     GOTO1 HIGH                                                             
*                                                                               
T120     CLC   KEY(11),KEYSAVE     TEST RECORD MUST BE UPDATED                  
         BE    T130                YES                                          
         CLC   KEY(6),KEYSAVE      TEST SAME TYPE/AGMD/CLT/PRD                  
         BNE   T110                                                             
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    T126                NO                                           
         MVC   DUB(5),KEY+6        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    T130                YES, PROCESS THIS                            
*                                                                               
T126     MVC   KEY+6(7),XFF        GET RECORDS FOR NEXT PRODUCT                 
         B     T110                                                             
*                                                                               
T130     GOTO1 =A(ESTCHK),DMCB,KEY+3,KEY+11  IS EST IN LIST?                    
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,5                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   MOVECODE,C'N'       DONE IF RECORD NOT BEING MOVED               
         BNE   T132                                                             
* BETTER NOT TO LOOP HERE WITH BE T110                                          
         MVI   KEY+12,X'FF'        FORCE NEXT ESTIMATE                          
         B     T115                                                             
*                                                                               
T132     LA    RF,ACCUTIRQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   KEY+6(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T135                                                             
         GOTO1 PUT                                                              
*                                                                               
T135     MVC   INSKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   T140                                                             
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T140     MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T150                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T150     GOTO1 SEQ                 NEXT RECORD                                  
         B     T120                                                             
*                                                                               
* UPDATE MARKET IN TRAFFIC BUY RECORDS                                          
*                                                                               
         USING TBYRECD,R6                                                       
T200     NTR1                                                                   
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         MVC   SAVEKEY3(2),=X'0A32'                                             
         MVC   SAVEKEY3+2(1),BAGYMD                                             
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                 NO.                                         
         NI    SAVEKEY3+2,X'FF'-X'08'                                           
         OI    SAVEKEY3+2,X'01'    SET TO TV                                    
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
T210     GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
*                                                                               
T210A    CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
*                                                                               
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T211                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    T211                YES                                          
         MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T210                                                             
*                                                                               
T211     CLC   KEY+6(5),OLDMSTA    OLD MKSTA MATCH?                             
         BE    T230                YES                                          
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    T212                NO                                           
         MVC   DUB(5),KEY+6        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    T230                YES, PROCESS THIS                            
*                                                                               
T212     CLC   KEY+6(5),OLDMSTA    PAST OLD MKSTA?                              
         BH    T213                YES, FORCE NEXT CLIENT                       
*                                                                               
         MVC   KEY+6(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA               
         XC    KEY+11(2),KEY+11                                                 
         B     T210                                                             
*                                                                               
T213     MVC   KEY+6(7),XFF        GET RECORDS FOR NEXT PRODUCT                 
         B     T210                                                             
*                                                                               
T230     LA    RF,ACCUTBYQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,9                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   KEY+6(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T235                                                             
         GOTO1 PUT                                                              
*                                                                               
T235     MVC   TBYKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   T240                                                             
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T240     MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T250                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T250     GOTO1 SEQ                 NEXT RECORD                                  
         B     T210A                                                            
*                                                                               
* UPDATE MARKET IN TRAFFIC SHIPPING RECAP RECORDS                               
*                                                                               
         USING SHPRECD,R6                                                       
T300     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'     SHIPPING RECAP RECORD                        
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                 NO.                                         
         NI    KEY+2,X'FF'-X'08'                                                
         OI    KEY+2,X'01'         SET TO TV                                    
*                                                                               
T305     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
T310     CLC   KEY(3),KEYSAVE      TEST SAME TYPE/AGMD                          
         BNE   EXIT                                                             
*                                                                               
         CLC   =X'94AE',KEY+3                                                   
         BNE   *+6                                                              
         LR    R1,R1                                                            
                                                                                
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T311                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   T313                NO                                           
*                                                                               
T311     CLC   KEY+5(5),OLDMSTA    OLD MKT/STA MATCH?                           
         BE    T314                YES                                          
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    T312                NO                                           
         MVC   DUB(5),KEY+5        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    T314                YES, PROCESS THIS                            
*                                                                               
T312     CLC   KEY+5(5),OLDMSTA    ALREADY PAST OLD MKT/STA?                    
         BH    T313                YES - FORCE NEXT CLIENT                      
*                                                                               
         MVC   KEY+5(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKT/STA             
         XC    KEY+10(3),KEY+10                                                 
         B     T305                                                             
*                                                                               
T313     MVC   KEY+5(8),XFF        FORCE NEXT CLIENT                            
         B     T305                                                             
*                                                                               
T314     LA    RF,ACCUTSPQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,10                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   KEY+5(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T320                                                             
         GOTO1 PUT                                                              
*                                                                               
T320     MVC   SHPKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   T330                                                             
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T330     MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T340                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T340     GOTO1 SEQ                 NEXT RECORD                                  
         B     T310                                                             
*                                                                               
* UPDATE MARKET IN TRAFFIC BUY ACTIVITY RECORDS                                 
*                                                                               
         USING TBARECD,R6                                                       
T400     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'     BUY ACTIVITY RECORDS                         
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                 NO.                                         
         NI    KEY+2,X'FF'-X'08'                                                
         OI    KEY+2,X'01'         SET TO TV                                    
*                                                                               
T405     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
T410     CLC   KEY(3),KEYSAVE      TEST SAME TYPE/AGMD                          
         BNE   EXIT                                                             
*                                                                               
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T411                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   T413                YES                                          
*                                                                               
T411     CLC   KEY+5(5),OLDMSTA    OLD MKSTA MATCH?                             
         BE    T414                YES                                          
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    T412                NO                                           
         MVC   DUB(5),KEY+5        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    T414                YES, PROCESS THIS                            
*                                                                               
T412     CLC   KEY+5(5),OLDMSTA    PAST OLD MKSTA?                              
         BH    T413                YES, FORCE NEXT CLIENT                       
*                                                                               
         MVC   KEY+5(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA               
         XC    KEY+10(3),KEY+10                                                 
         B     T405                                                             
*                                                                               
T413     MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T405                                                             
*                                                                               
T414     GOTO1 =A(ESTCHK),DMCB,KEY+3,KEY+11  IS EST IN LIST?                    
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,11                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   MOVECODE,C'N'       DONE IF RECORD NOT BEING MOVED               
         BE    T440                                                             
*                                                                               
         LA    RF,ACCUTBAQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   KEY+5(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T420                                                             
         GOTO1 PUT                                                              
*                                                                               
T420     MVC   TBAKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   T430                                                             
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T430     MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T440                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T440     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
         CLC   KEY(10),KEYSAVE     OLD MKSTA WITH NEXT PRD/EST?                 
         BE    T414                YES                                          
         B     T410                                                             
         EJECT                                                                  
* UPDATE STATION IN TBUY MARKET LIST                                            
*                                                                               
         USING STLRECD,R6                                                       
T500     NTR1                                                                   
         CLI   RCWRITE,C'Y'        WRITE=NO OPTION?                             
         BNE   EXIT                YES - EXIT THIS ONLY UPDATES ELEMS           
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         MVC   KEY(2),=X'0A31'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
*                                                                               
T505     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
T510     CLC   KEY(3),KEYSAVE      TEST SAME TYPE/AGMD                          
         BNE   EXIT                                                             
*                                                                               
         OC    KEY+3(2),KEY+3      HAVE A CLIENT?                               
         BZ    T511                NO                                           
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T511                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    T511                YES                                          
         MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T505                                                             
*                                                                               
T511     CLC   KEY+8(2),OLDMSTA    LOOK FOR RECORD WITH OLD MARKET              
         BNE   T550                 NO                                          
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
*                                                                               
         XC    ELEM,ELEM           BUILD SAVED X'10' ELEM HERE                  
*                                                                               
         L     R6,AREC                                                          
         MVI   BYTE,X'10'          LOOK FOR X'10' ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
T520     BAS   RE,NEXTEL           HAVE ANOTHER X'10' ELEMENT?                  
         BNE   T550                NO, DONE WITH THIS RECORD                    
*                                                                               
         USING STLDTAEL,R6                                                      
         CLC   STLSTA,OLDMSTA+2    IS THIS THE SAME STATION?                    
         BE    T530                YES                                          
*                                                                               
         CLI   QSTA,C'0'           IS THIS A CABLE STATION?                     
         BL    T520                NO                                           
         MVC   DUB(3),STLSTA       SAVE THE STATION                             
         NI    DUB+2,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB+2       THIS THE SAME STATION?                       
         BNE   T520                NO, GET THE NEXT ELEMENT                     
*                                                                               
T530     MVC   ELEM(STLDTAX-STLDTAEL),0(R6)   SAVE X'10' ELEM                   
         GOTO1 RECUP,DMCB,(C'S',AREC),(R6),0  DELETE FROM OLD REC               
*                                                                               
         GOTO1 PUT                                                              
*                                                                               
T536     MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   KEY+8(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
         CLC   KEY(13),KEYSAVE     HAVE RECORD WITH NEW MKSTA?                  
         BNE   T560                NO, GIVE ERROR MESSAGE                       
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC             UPDATE RECORD WITH NEW X'10' ELEM            
         MVI   BYTE,X'10'                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
T543     BAS   RE,NEXTEL           ANY MORE X'10' ELEMENTS?                     
         BNE   T544                NO, ADD THE RECORD AT THIS POINT             
*                                                                               
         USING STLDTAEL,R6                                                      
         CLC   STLDTAEL(STLDTAX-STLDTAEL),ELEM  HAVE ELEM ALREADY?              
         BE    T546                             YES, NO UPDATE                  
         BL    T543                TRY TO ADD ELEMS IN ALPHA ORDER              
*                                                                               
T544     GOTO1 RECUP,DMCB,(C'S',AREC),ELEM,(R6) UPDATE REC WITH ELEM            
*                                                                               
         GOTO1 PUT                 PUT REC BACK WITH NEW X'10' ELEM             
*                                                                               
T546     MVC   KEY(13),SAVEKEY     RESTORE OLD KEY                              
*                                                                               
         GOTO1 HIGH                SET FOR SEQ                                  
*                                                                               
         CLC   KEY(13),KEYSAVE     LOOK FOR RECORD WITH NEW MKSTA               
         BE    T550                MUST HAVE IT - WE JUST READ IT               
         DC    H'0'                                                             
*                                                                               
T550     GOTO1 SEQ                 NEXT RECORD                                  
         B     T510                                                             
*                                                                               
T560     GOTO1 CLUNPK,DMCB,KEY+STLKCLT-STLRECD,PCLT                             
*                                                                               
         MVC   PPRD,KEY+STLKPRD-STLKEY                                          
*                                                                               
         MVC   PEST(24),=C'MARKET 0000 HAD STATIONS'                            
         MVC   PEST+7(4),QMKT                                                   
*                                                                               
         L     R3,ADBILL                                                        
         LA    R4,PEST+25                                                       
T564     XC    DUB,DUB                                                          
         MVC   DUB+2(3),STLSTA-STLDTAEL(R3)                                     
*                                                                               
         GOTO1 MSUNPK,DMCB,DUB,WORK,(R4)                                        
*                                                                               
         LA    R3,STLDTAX-STLDTAEL(R3)                                          
         OC    0(STLDTAX-STLDTAEL,R3),0(R3)                                     
         BZ    T565                                                             
         MVI   5(R4),C','                                                       
         AHI   R4,7                                                             
         B     T564                                                             
*                                                                               
T565     GOTO1 REPORT                                                           
*                                                                               
         MVC   PEST(20),=C'NEW MARKET NOT FOUND'                                
         GOTO1 REPORT                                                           
         B     T546                                                             
         EJECT                                                                  
* THIS SECTION UPDATES THE MARKET NUMBER IN THE STATION BILL RECORD *           
*                                                                               
         USING STABUCKD,R6                                                      
T800     NTR1                                                                   
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         MVC   SAVEKEY3(2),=X'0E01'                                             
         MVC   SAVEKEY3+2(1),BAGYMD                                             
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                 NO.                                         
         NI    SAVEKEY3+2,X'FF'-X'08'                                           
         OI    SAVEKEY3+2,X'01'         SET TO TV                               
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
         GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
         B     T814                                                             
*                                                                               
T810     GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   T860                NO -- EXIT                                   
*                                                                               
T814     MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    T815                YES                                          
         MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T810                                                             
*                                                                               
T815     MVC   KEY+7(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA               
         MVI   KEY+12,0                                                         
         GOTO1 HIGH                                                             
*                                                                               
T820     CLC   KEY(12),KEYSAVE     TEST RECORD MUST BE UPDATED                  
         BE    T830                YES                                          
         CLC   KEY(7),KEYSAVE      TEST SAME TYPE/AGMD/CLT/PRD/EST              
         BNE   T810                                                             
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    T826                NO                                           
         MVC   DUB(5),KEY+7        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    T830                YES, PROCESS THIS                            
*                                                                               
T826     MVC   KEY+7(6),XFF        GET RECORDS FOR NEXT ESTIMATE                
         B     T810                                                             
*                                                                               
T830     GOTO1 =A(ESTCHK),DMCB,KEY+3,KEY+6  IS EST IN LIST?                     
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,12                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   MOVECODE,C'N'       DONE IF RECORD NOT BEING MOVED               
         BE    T850                                                             
*                                                                               
         LA    RF,ACCUSBLQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
*                                                                               
         CLI   RCSUBPRG,1          RESET SPROG                                  
         BE    T832                                                             
         GOTO1 REPORT                                                           
         L     RE,=A(MIDL1)                                                     
         MVC   P1(110),0(RE)                                                    
         MVC   P2(110),110(RE)                                                  
         MVI   RCSUBPRG,1          RESET SPROG                                  
         GOTO1 REPORT                                                           
*                                                                               
T832     GOTO1 CLUNPK,DMCB,KEY+STABKCLT-STABUCK,PCLT                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD     AGENCY/MEDIA                                 
         MVC   KEY+2(2),SAVEKEY+STABKCLT-STABUCK                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     HAVE CLIENT RECORD?                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVC   SVAREC,AREC         SAVE AREC                                    
         MVC   AREC,ADCLT                                                       
         GOTO1 GET                                                              
         MVC   AREC,SVAREC                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GET                                                              
*                                                                               
         L     R1,ADCLT                                                         
         CLC   KEY+STABKCLT-STABUCK(2),2(R1) SAME CLT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,220                                                           
         LA    R1,CLIST-CLTHDRD(R1)                                             
         CLC   KEY+STABKPRD-STABUCK(1),3(R1)                                    
         BE    *+22                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,*-22                                                          
         DC    H'0'                                                             
         MVC   PPRD,0(R1)                                                       
         LLC   R0,KEY+STABKEST-STABUCK                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         BRAS  RE,PRTELS           GO PRINT ANY ELEMENTS                        
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T834                                                             
         GOTO1 PUT                                                              
*                                                                               
T834     MVC   KEY+7(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
         MVC   STABKMKT,NEWMKT     MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
*                                                                               
         OI    DMINBTS,X'08'      PASS THE DELETED RECORDS                      
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE    DOES RECORD ALREADY EXIST                     
         BNE   T838                                                             
*                                                                               
         BAS   RE,MRGELS          DO GETREC AND                                 
*                                 MERGE ELEMENTS FROM BOTH RECS                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   T840                                                             
         GOTO1 PUT                                                              
         TM    KEY+13,X'80'                                                     
         BZ    T840                                                             
         NI    KEY+13,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     T840                                                             
*                                                                               
T838     CLI   RCWRITE,C'Y'                                                     
         BNE   T840                                                             
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T840     NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
*                                                                               
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T850                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T850     GOTO1 SEQ                 NEXT RECORD                                  
         B     T820                                                             
*                                                                               
T860     CLI   RCSUBPRG,0          SPROG EVER CHANGED                           
         BE    T870                 NO                                          
*                                                                               
         MVI   RCSUBPRG,0          RESET SPROG                                  
         GOTO1 REPORT                                                           
*                                                                               
T870     B     EXIT                                                             
*                                                                               
* THIS SECTION UPDATES THE STATUS RECORDS *                                     
*                                                                               
         USING STATD,R6                                                         
T700     NTR1                                                                   
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         MVC   SAVEKEY3(2),=X'0D71'                                             
         MVC   SAVEKEY3+2(1),BAGYMD                                             
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
T705     GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BE    T714                                                             
         B     EXIT                NO -- EXIT                                   
*                                                                               
T710     GOTO1 SEQ                                                              
         CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
         CLI   KEY+6,C'R'          TEST SPCL REP RECORD                         
         BE    T710                                                             
*                                                                               
T714     CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    T715                YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    T715                YES                                          
         MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     T705                                                             
*                                                                               
T715     CLC   KEY+8(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA               
         BE    T730                                                             
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BNL   T720                YES                                          
*                                                                               
         CLC   KEY+8(5),OLDMSTA                                                 
         BNL   *+14                                                             
         MVC   KEY+8(5),OLDMSTA                                                 
         B     T705                                                             
*                                                                               
         MVC   KEY+8(5),XFF                                                     
         B     T705                                                             
*                                                                               
T720     MVC   DUB(5),KEY+8                                                     
         NI    DUB+4,X'80'                                                      
         CLC   OLDMSTA,DUB                                                      
         BNE   T710                                                             
*                                                                               
T730     GOTO1 =A(ESTCHK),DMCB,STKCLT-STATKEY+KEY,STKEST-STATKEY+KEY            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,22  13                                                  
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   MOVECODE,C'N'       DONE IF RECORD NOT BEING MOVED               
         BE    T710                                                             
*                                                                               
         LA    RF,ACCUSTUQ                                                      
         BAS   RE,BUMPACC                                                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T734                                                             
         GOTO1 PUT                                                              
*                                                                               
T734     MVC   KEY+8(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
         MVC   8(2,R6),NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES RECORD ALREADY EXIST                    
         BE    T740                YES - GET ON WITH DELETING OLD               
*                                                                               
T738     CLI   RCWRITE,C'Y'                                                     
         BNE   T740                                                             
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
T740     NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
         SPACE                                                                  
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T750                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T750     B     T710                NEXT RECORD                                  
*                                                                               
* THIS SECTION UPDATES THE STATION LOCKIN HEADER RECORDS *                      
*                                                                               
ST100    NTR1                                                                   
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         LA    R6,SAVEKEY3                                                      
         USING SLHRECD,R6                                                       
         MVI   SLHKTYP,SLHKTYPQ                                                 
         MVI   SLHKSUB,SLHKSUBQ                                                 
         MVC   SLHKAGMD,BAGYMD                                                  
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
ST105    GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         B     ST111                                                            
*                                                                               
ST110    GOTO1 SEQ                                                              
*                                                                               
ST111    CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
*                                                                               
ST114    CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    ST115               YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   ST117               NO                                           
*                                                                               
ST115    CLC   KEY+5(5),OLDMSTA    OLDMSTA MATCH?                               
         BE    ST130               YES                                          
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    ST116               NO                                           
         MVC   DUB(5),KEY+5        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    ST130               YES, PROCESS THIS                            
*                                                                               
ST116    CLC   KEY+5(5),OLDMSTA    PAST OLD MKT/STA                             
         BH    ST117               YES, BUMP TO NEXT CLIENT                     
         MVC   KEY+5(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKT/STA             
         XC    KEY+10(3),KEY+10                                                 
         B     ST105                                                            
*                                                                               
ST117    MVC   KEY+5(8),XFF        NO - FORCE NEXT CLIENT                       
         B     ST105                                                            
*                                                                               
ST130    LA    RF,ACCUSLHQ                                                      
         BAS   RE,BUMPACC                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,23                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         L     R6,AREC                                                          
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST134                                                            
         GOTO1 PUT                                                              
*                                                                               
ST134    MVC   KEY+5(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
         MVC   5(2,R6),NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES RECORD ALREADY EXIST                    
         BE    ST140               YES - GET ON WITH DELETING OLD               
*                                                                               
ST138    CLI   RCWRITE,C'Y'                                                     
         BNE   ST140                                                            
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
ST140    NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
*                                                                               
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   ST110                                                            
         GOTO1 WRITE               DELETE OLD KEY                               
         B     ST110               NEXT RECORD                                  
         DROP  R6                                                               
*                                                                               
* THIS SECTION UPDATES THE CLEARED STATUS RECORDS *                             
*                                                                               
ST200    NTR1                                                                   
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/CLT                           
         LA    R6,SAVEKEY3                                                      
         USING CLRSTATD,R6                                                      
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,BAGYMD                                                  
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+12                NO                                           
         NI    CLSKAGMD,X'F7'      STRIP X'08' BIT                              
         OI    CLSKAGMD,X'01'      SET TO TV                                    
         MVC   KEY(3),SAVEKEY3                                                  
*                                                                               
ST205    GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         B     ST211                                                            
*                                                                               
ST210    GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE     OLD MKSTA?                                   
         BE    ST230               YES                                          
*                                                                               
ST211    CLC   KEY(3),SAVEKEY3     TEST SAME TYPE/AGMD                          
         BNE   EXIT                NO -- EXIT                                   
*                                                                               
         CLC   KEY+3(2),KEYSAVE+3  ALREADY CHECKED THIS CLIENT?                 
         BE    ST215               YES                                          
*                                                                               
         MVC   HALF,KEY+3          BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   ST217               NO, FORCE NEXT CLIENT                        
*                                                                               
ST215    CLC   KEY+5(5),OLDMSTA    HAVE OLD MKT/STA?                            
         BE    ST230               YES                                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    ST216               NO                                           
         MVC   DUB(5),KEY+5        MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    ST230               YES, PROCESS THIS                            
*                                                                               
ST216    CLC   KEY+5(5),OLDMSTA    PAST OLD MKT/STA?                            
         BH    ST217               YES, FORCE NEXT CLIENT                       
         MVC   KEY+5(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKT/STA             
         XC    KEY+10(3),KEY+10                                                 
         B     ST205                                                            
*                                                                               
ST217    MVC   KEY+5(8),XFF        FORCE NEXT CLIENT                            
         B     ST205                                                            
*                                                                               
ST230    LA    RF,ACCUCLSQ                                                      
         BAS   RE,BUMPACC                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,24                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         L     R6,AREC                                                          
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
*                                                                               
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST234                                                            
         GOTO1 PUT                                                              
*                                                                               
ST234    MVC   KEY+5(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
         MVC   5(2,R6),NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES RECORD ALREADY EXIST                    
         BE    ST240               YES - GET ON WITH DELETING OLD               
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   ST240                                                            
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
ST240    NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
*                                                                               
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   ST250                                                            
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
ST250    B     ST210               NEXT RECORD                                  
         DROP  R6                                                               
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
* THIS SECTION UPDATES THE NEW BUYERS WORKSHEET RECORDS                         
*                                                                               
NW100    NTR1                                                                   
         XC    SAVEKEY,SAVEKEY                                                  
* -- READ THROUGH CAMP RECORDS - AND IF CLT REQUESTED MATCH ON IT               
         LA    R6,KEY                                                           
         USING CAMRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   CAMKTYP,CAMKTYPQ   X'0D'                                         
         MVI   CAMKSUB,CAMKSUBQ   X'66'                                         
         MVC   CAMKAGMD,BAGYMD    A/M                                           
         GOTO1 HIGH                                                             
*                                                                               
NW105    CLC   KEY(3),KEYSAVE     ANY RECORDS FOR AGMD?                         
         BNE   EXIT                                                             
*                                                                               
         CLC   =C'ALL',QCLT       SPECIFIC CLIENT REQUESTED?                    
         BE    NW110              NO - USE THIS RECORD                          
*                                                                               
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         CLC   CAMCLT,BCLT        MUST MATCH ON CLIENT                          
         BE    NW110                                                            
         GOTO1 SEQ                TRY NEXT CAMPAIGN RECORD                      
         B     NW105                                                            
*                                                                               
NW110    MVC   SAVEKEY(13),KEY    SAVE CAMPAIGN KEY                             
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,13                                                      
         BRAS  RE,SPTRACE         ** TRACE **                                   
         DROP  R6                                                               
*                                                                               
* -- BUILD NWS HEADER KEY - LOOKING FOR OLD MKT/ STATION                        
*                                                                               
NW120    LA    R6,KEY                                                           
         USING BWHRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   BWHKTYP,BWHKTYPQ   X'0D'                                         
         MVI   BWHKSUB,BWHKSUBQ   X'67'                                         
         MVC   BWHKAGMD,BAGYMD    A/M                                           
         MVC   BWHKBYR,SAVEKEY+3  BUYER                                         
         MVC   BWHKCAM,SAVEKEY+4  CAMPAIGN                                      
         MVC   BWHKMKT,OLDMKT     OLD MARKET                                    
         GOTO1 HIGH                                                             
NW122    CLC   KEY(8),KEYSAVE                                                   
         BNE   NW190              NO HEADER FOR THIS CAMPAIGN/MKT               
         SPACE                                                                  
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FF'-X'02'                                             
         SPACE                                                                  
         GOTO1 GET                                                              
         SPACE                                                                  
         NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
         SPACE                                                                  
         L     R6,AREC                                                          
         SPACE                                                                  
         LA    R2,BWHFSTEL                                                      
NW124    CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         CLI   0(R2),0                                                          
         BE    NW190              STATION NOT IN CAMPAIGN - TRY NEXT            
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   NW124                                                            
         B     NW190              STATION NOT IN CAMPAIGN - TRY NEXT            
         SPACE                                                                  
NW126    CLC   QSTA,BWHSTA-BWHEL(R2)         OLD STATION IN RECORD              
         BE    NW130                                                            
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         B     NW190              STATION NOT IN CAMPAIGN - TRY NEXT            
*                                                                               
NW130    MVC   NOLDSTCD,BWHSEQ-BWHEL(R2)  SAVE OLD STATION CODE                 
         MVC   NNWSSTA,BWHSTA-BWHEL(R2)   SAVE NWS STATION CALL LETTERS         
         SPACE                                                                  
* REMOVE STATION ELEMENT                                                        
         SPACE                                                                  
         GOTO1 RECUP,DMCB,(C'S',AREC),(R2),0                                    
         SPACE                                                                  
* DELETE RECORD IF EMPTY                                                        
         SPACE                                                                  
         LA    R2,BWHFSTEL                                                      
         CLI   0(R2),X'02'                                                      
         BE    NW134                                                            
         SPACE                                                                  
         OI    15(R6),X'80'                                                     
NW134    DS    0H                                                               
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW136                                                            
         GOTO1 PUT                                                              
         SPACE                                                                  
NW136    DS    0H                                                               
         LA    RF,ACCUNCAQ                                                      
         BAS   RE,BUMPACC                                                       
         MVC   SAVEKEY2,KEY       SAVE HEADER KEY                               
         DROP  R6                                                               
*                                                                               
* -- LOOK FOR HEADER RECORD WITH NEW MARKET                                     
* -- IF REC EXISTS - & STATION EXISTS - USE EXISTING SEQ # TO ADD               
* -- IF REC EXISTS - & STATION DOESN'T - ADD IT TO LIST WITH NEXT SEQ #         
* -- IF REC DOESN'T EXIST - ADD RECORD WITH NEW STATION SEQ # 1                 
*                                                                               
         LA    R6,KEY                                                           
         USING BWHRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   BWHKTYP,BWHKTYPQ   X'0D'                                         
         MVI   BWHKSUB,BWHKSUBQ   X'67'                                         
         MVC   BWHKAGMD,BAGYMD    A/M                                           
         MVC   BWHKBYR,SAVEKEY+3  BUYER                                         
         MVC   BWHKCAM,SAVEKEY+4  CAMPAIGN NUMBER                               
         MVC   BWHKMKT,NEWMKT     NEW MARKET                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NW160              ADD NEW RECORD                                
         SPACE                                                                  
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         GOTO1 GET                                                              
         SPACE                                                                  
         L     R6,AREC                                                          
         LA    R2,BWHFSTEL                                                      
         CLI   0(R2),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'               NO STATION LIST IN RECORD?                    
         LA    RE,1                                                             
         LR    R4,R2               SAVE 1ST ELEM ADDR                           
         SPACE                                                                  
NW140    DS    0H                                                               
         LLC   R0,1(R2)                                                         
         CLC   NNWSSTA,BWHSTA-BWHEL(R2)  THIS STATION IN RECORD                 
         BE    NW150                                                            
         BL    *+8                                                              
         LR    R4,R2               SAVE INSERTION POINT FOR NEW ELEM            
         AR    R4,R0                (WHICH WILL BE AFTER THIS ELEMENT)          
         SPACE                                                                  
         CLM   RE,1,BWHSEQ-BWHEL(R2)                                            
         BH    NW142                                                            
         IC    RE,BWHSEQ-BWHEL(R2)       SAVE OLD BINARY STATION CODE           
         SPACE                                                                  
NW142    DS    0H                                                               
         AR    R2,R0                                                            
         CLI   0(R2),X'02'                                                      
         BE    NW140                                                            
         SPACE                                                                  
* BUILD AND INSERT NEW STATION ELEMENT INTO RECORD                              
         SPACE                                                                  
         LA    RE,1(,RE)                                                        
         STC   RE,NNEWSTCD          NEW STATION BINARY CODE                     
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'                                                       
         MVI   ELEM+1,BWHELLNQ                                                  
         MVC   BWHSEQ-BWHEL+ELEM,NNEWSTCD                                       
         MVC   BWHSTA-BWHEL+ELEM,NNWSSTA                                        
         GOTO1 RECUP,DMCB,(C'S',(R6)),ELEM,(R4)                                 
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW170                                                            
*                                                                               
         GOTO1 PUT                                                              
*                                                                               
         B     NW170                                                            
*                                                                               
NW150    MVC   NNEWSTCD,BWHSEQ-BWHEL(R2) SAVE EXISTING STATION CODE             
         MVC   NNWSSTA,BWHSTA-BWHEL(R2)   SAVE NWS STATION CALL LETTERS         
         SPACE                                                                  
*                                 STATION IS IN ELEMENT                         
         MVI   STAERR,C'Y'        HAVE FLAG INDICATING SO                       
         B     NW170              LOOK UP DETAIL RECORDS                        
*                                                                               
* -- NWS HEADER RECORD DIDN'T EXIST FOR CAMP/MKT                                
* -- NEED TO GET NEW SEQUENCE NUMBER BY READING PASSIVE PTR                     
* -- THEN ADD NEW HEADER RECORD AND PASSIVE POINTER (0DE7)                      
*                                                                               
NW160    XC    KEY,KEY                                                          
         MVI   BWHPTYP,BWHPTYPQ   X'0D'                                         
         MVI   BWHPSUB,BWHPSUBQ   X'E7'                                         
         MVC   BWHPAGMD,BAGYMD    A/M                                           
         MVC   BWHPBYR,SAVEKEY2+3 BUYER                                         
         GOTO1 HIGH               GET LATEST SEQUENCE NUMBER                    
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'               SHOULD BE ONE FOR THAT BUYER                  
         SR    R1,R1                                                            
         ICM   R1,3,BWHPSEQ       GET LATEST SEQUENCE NUMBER                    
         AHI   R1,-1              SUBTRACT 1 TO GET NEXT SEQ #                  
         STCM  R1,3,SEQNUM        SAVE NEXT AVAILABLE SEQ #                     
*                                                                               
         XC    KEY,KEY            ADD NEW HEADER RECORD                         
         MVI   BWHKTYP,BWHKTYPQ   X'0D'                                         
         MVI   BWHKSUB,BWHKSUBQ   X'67'                                         
         MVC   BWHKAGMD,BAGYMD    A/M                                           
         MVC   BWHKBYR,SAVEKEY+3  BUYER                                         
         MVC   BWHKCAM,SAVEKEY+4  CAMPAIGN NUMBER                               
         MVC   BWHKMKT,NEWMKT     NEW MARKET                                    
         MVC   BWHKSEQ,SEQNUM     NEW SEQUENCE NUMBER                           
         DROP  R6                                                               
         L     R6,AREC                                                          
         XC    0(256,R6),0(R6)                                                  
         SPACE                                                                  
         MVC   0(13,R6),KEY                                                     
         MVC   13(2,R6),=H'36'                                                  
         LA    R6,24(R6)          PT TO FIRST ELEMENT                           
         USING BWHEL,R6                                                         
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         MVI   BWHSEQ,1                                                         
         MVC   BWHSTA,NNWSSTA                                                   
         SPACE                                                                  
         MVI   NNEWSTCD,1         NEW STATION CODE                              
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,16                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         XC    FULL,FULL                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW165                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         MVC   FULL,KEY+14        DISK ADDRESS OF ADDED RECORD                  
*                                                                               
NW165    DS    0H                 ADD PASSIVE DIRECTORY POINTER                 
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BWHRECD,R6                                                       
         MVI   BWHPTYP,BWHPTYPQ   X'0D'                                         
         MVI   BWHPSUB,BWHPSUBQ   X'E7'                                         
         MVC   BWHPAGMD,BAGYMD    A/M                                           
         MVC   BWHPBYR,SAVEKEY2+3 BUYER                                         
         MVC   BWHPSEQ,SEQNUM     SEQUENCE NUMBER                               
         MVC   KEY+14(4),FULL     DISK ADDRESS                                  
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,17                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW170                                                            
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         TM    DM3,X'DD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* -- LOOK FOR NWS DETAIL RECORDS WITH OLD SEQ NUMBER                            
* -- AND OLD STATION CODE                                                       
*                                                                               
NW170    DS    0H                 FIX DTL RECORDS                               
         GOTO1 =A(SETMIN)         SET MINIO BLOCK & MASTER KEY                  
         L     R3,=A(MINBLK)                                                    
         USING MINBLKD,R3                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'01'                                                    
         MVC   MINEKEY+1(1),NOLDSTCD   LOOK FOR OLD STATION CODE                
         MVI   MINFILTL,2              MATCH ON ELCODE & STA CODE               
         MVC   SMINEKEY,MINEKEY        SAVE ELEMENT KEY                         
         GOTO1 =V(MINIO),DMCB,('MINHI',(R3))                                    
*                                                                               
         CLI   MINERR,MINERNF     RECORD FOUND?                                 
         BE    NW190              NO - GO NEXT CAMPAIGN                         
         SPACE                                                                  
         CLI   MINERR,MINEEOF     END OF FILE?                                  
         BE    NW190              NO - GO NEXT CAMPAIGN                         
         SPACE                                                                  
         CLI   MINERR,MINESNF     SET NOT FOUND?                                
         BE    NW190               YES - GO NEXT CAMPAIGN                       
         SPACE                                                                  
         CLI   MINERR,0           ANOTHER OTHER ERROR DIE                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   STAERR,C'Y'        WAS STATION IN HEADER?                        
         BNE   NW173                                                            
         SPACE                                                                  
* -- PROBLEM DETAIL RECORDS ALREADY EXIST                                       
* -- NEED TO PRINT OUT MESSAGE                                                  
         SPACE                                                                  
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         BRAS  RE,NWSERR                                                        
         IC    RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R1)                                                       
*                                                                               
         GOTO1 HEXOUT,DMCB,MINMKEY,P+40,20,=C'MIX',0                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE                                                                  
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         SPACE                                                                  
         B     NW190               GO TO NEXT CAMPAIGN                          
         SPACE                                                                  
*        DC    H'0'               *** TEMPORARY                                 
*                                                                               
NW173    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   NW176                                                            
         MVC   KEY,MINMKEY                                                      
         MVI   TRACECDE,18                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL MASTER ***                  
         MVC   KEY,MINEKEY                                                      
         MVI   TRACECDE,20                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL ELEMENT ***                 
*                                                                               
* - BUILD NEW DETAIL RECORDS BY COPYING THE OLD DTL RECS                        
*   TO A NEW MASTER KEY - THEN CHANGE THE NEW ELEMENTS AND                      
*   DELETE THE OLD RECORDS                                                      
*                                                                               
NW176    LA    R1,MINEKEY         BUILD NEW RECORDS                             
         USING BWDRECD,R1         SETTING MINEKEY WITH NEW MASTER KEY           
         MVC   MINEKEY,SMINMKEY   GET OLD MASTER KEY                            
         MVC   BWDKSEQ,SEQNUM     MOVE IN NEW SEQUENCE NUMBER                   
         DROP  R1                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   NW177                                                            
         MVC   KEY,MINEKEY                                                      
         MVI   TRACECDE,19                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL MASTER ***                  
*                                                                               
NW177    DS    0H                                                               
         LA    RF,ACCUNDTQ         COUNT DETAIL RECS                            
         BAS   RE,BUMPACC                                                       
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW178                                                            
         SPACE                                                                  
         GOTO1 =V(MINIO),DMCB,('MINCPY',(R3))                                   
*                                 DELETE OLD ONE                                
         GOTO1 =V(MINIO),DMCB,('MINDLF',(R3))                                   
*                                                                               
NW178    LA    R1,MINMKEY         SET NEW MASTER KEY IN MASTER KEY              
         USING BWDRECD,R1                                                       
         MVC   MINMKEY,SMINMKEY                                                 
         MVC   BWDKSEQ,SEQNUM     MOVE IN NEW SEQUENCE NUMBER                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'01'                                                    
         MVC   MINEKEY+1(1),NOLDSTCD   LOOK FOR OLD STATION CODE                
         MVI   MINFILTL,2              MATCH ON ELCODE & STA CODE               
         MVC   KEY,MINEKEY                                                      
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,20                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL ELEMENTS**                  
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW179                                                            
         GOTO1 =V(MINIO),DMCB,('MINHI',(R3))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'               JUST READ IT BEFORE                           
*                                                                               
NW179    MVC   MINEKEY+1(1),NNEWSTCD MOVE IN NEW STATION CODE                   
         MVC   KEY,MINEKEY                                                      
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,21                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL ELEMENTS**                  
*                                                                               
         L     R1,MINELEM         ALSO UPDATE X'01' ELEMENT                     
         MVC   2(1,R1),NNEWSTCD                                                 
         MVC   KEY,0(R1)                                                        
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,21                                                      
         BRAS  RE,SPTRACE          *** TRACE DETAIL ELEMENTS**                  
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW190                                                            
         GOTO1 =V(MINIO),DMCB,('MINWRT',(R3))                                   
*                                                                               
         GOTO1 =V(MINIO),DMCB,('MINSEQ',(R3))                                   
         CLI   MINERR,MINERNF     ANY MORE ELEMENTS TO CHANGE?                  
         BE    NW185              NO - CLOSE AND GET NEXT CAMPAIGN              
         CLI   MINERR,MINEEOF     ANY MORE ELEMENTS TO CHANGE?                  
         BE    NW185              NO - CLOSE AND GET NEXT CAMPAIGN              
         CLI   MINERR,0           ANOTHER OTHER ERROR DIE                       
         BE    NW179                                                            
         DC    H'0'                                                             
*                                                                               
NW185    GOTO1 =V(MINIO),DMCB,('MINCLS',(R3))                                   
*                                                                               
NW190    DS    0H                 NEXT CAMPAIGN RECORD                          
         MVC   KEY(13),SAVEKEY    RE-ESTABLISH CAMPAIGN KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         B     NW105              TRY NEXT CAMPAIGN                             
         DROP  R3                                                               
         SPACE 3                                                                
*&&                                                                             
BUMPACC  MHI   RF,L'ACCUMS                                                      
         A     RF,=A(ACCUMS)                                                    
         AP    0(4,RF),=P'1'                                                    
         BR    RE                                                               
         EJECT                                                                  
* THIS SECTION UPDATES THE MARKET NUMBER IN THE SIR AND STATION RECORDS         
*                                                                               
SP500    DS   0H                                                                
*   CHECK TO SEE IF MARKET SHOULDN'T BE TRANSFERRED BECAUSE THE REVERSE         
*   ...PROCESS ALREADY HAPPENED IN THE SAME WEEK     MHC 06/17/02               
         TM    MKTFFLAG,RVRSMKTF   REVERSE MARKET FIX EXISTS, SAME WEEK         
         BO    EXIT                                                             
*                                                                               
         MVC   SVAREC,AREC         SAVE AREC                                    
         MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
         NOP   *                                                                
*        BAS   RE,NW100            NEW BUYERS WORKSHEET RECORDS                 
*                                                                               
         MVC   AREC,SVAREC         RESTORE AREC                                 
*                                                                               
         CLC   QCLT,=C'ALL'        TEST ALL CLIENT REQUEST                      
         BNE   SP508               NO - DON'T UPDATE SIR RECORDS                
*                                                                               
         MVC   SVAREC,AREC         SAVE AREC                                    
         MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
*                                                                               
         USING SIRRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   SIRKTYPE,X'0C'      SIR RECORD                                   
         MVC   SIRKAM,BAGYMD                                                    
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
SP502    LA    R6,KEY                                                           
         CLC   KEY(2),KEYSAVE      TEST SAME TYPE/AGMD                          
         BNE   SP506               NO -- EXIT                                   
*                                                                               
         CLC   SIRKMS,OLDMSTA      TEST SAME MKT/STA                            
         BE    SP503                                                            
         SPACE                                                                  
         CLI   QSTA,C'0'           CABLE STATION?                               
         BL    SP504                NO                                          
         MVC   DUB(5),SIRKMS                                                    
         NI    DUB+4,X'80'                                                      
         CLC   OLDMSTA,DUB                                                      
         BNE   SP504                                                            
*                                                                               
SP503    LA    RF,ACCUSIRQ         INCREMENT PROCESSED RECORD COUNTER           
         BAS   RE,BUMPACC                                                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
         MVC   SIRKMKT,NEWMKT      MOVE NEW MARKET INTO KEY                     
*                                                                               
         L     R6,AREC                                                          
         OI    SIRRCNTL,X'80'      MARK OLD RECORD FOR DELETION                 
         GOTO1 PUT                                                              
*                                                                               
         MVC   SIRKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    SIRRCNTL,X'7F'      UNDELETE THE NEW RECORD                      
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
*                                                                               
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
SP504    GOTO1 SEQ                 NEXT RECORD                                  
         B     SP502                                                            
*                                                                               
SP506    MVC   AREC,SVAREC         RESTORE AREC                                 
         DROP  R6                                                               
         EJECT                                                                  
SP508    MVI   ALLOWLIN,10                                                      
         MVI   SPACING,2           PRINT SUMMARY STATS                          
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,=A(ACCUMS)                                                    
         LA    R5,NACCUMS-1                                                     
*                                                                               
SP509    EDIT  (P4,(R4)),(8,P),ZERO=NOBLANK                                     
         MVC   P+10(24),8(R4)                                                   
         GOTO1 REPORT                                                           
         LA    R4,L'ACCUMS(R4)                                                  
         BCT   R5,SP509                                                         
*                                                                               
* ADD REQUEST TOTALS TO RUN TOTALS AND CLEAR REQ TOTALS                         
*                                                                               
         L     R4,=A(ACCUMS)                                                    
         LA    R5,NACCUMS-1                                                     
*                                                                               
SP509A   AP    4(4,R4),0(4,R4)     ADD REQ TO RUN TOTALS                        
         ZAP   0(4,R4),=P'0'                                                    
         LA    R4,L'ACCUMS(R4)                                                  
         BCT   R5,SP509A                                                        
         AP    4(4,R4),=P'1'       BUMP REQUEST COUNT TOO                       
*                                                                               
* PRINT OUT ANY CONSOLIDATED PW NOT MOVED REPORT *                              
*                                                                               
         LA    R5,PWTABSIZ                                                      
         L     R6,=A(PWTAB)                                                     
         USING PWTABD,R6                                                        
         MVI   FORCEHED,C'Y'                                                    
SP516    OC    PWTABENT,PWTABENT     ANY ENTRIES IN TABLE                       
         BZ    SP518                                                            
         MVC   P+11(3),PWTABCLT                                                 
         MVC   P+47(3),PWTABPRD                                                 
         MVC   P+64(3),PWTABEST                                                 
         EDIT  (B2,PWTABCT),(5,P+70),COMMAS=YES                                 
         MVC   P+76(17),=CL17'PW BUYS NOT MOVED'                                
         GOTO1 REPORT                                                           
         LA    R6,PWTABNXT                                                      
         BCT   R5,SP516                                                         
         DROP  R6                                                               
         EJECT                                                                  
* NOW UPDATE MARKET NUMBER IN STATION MASTER RECORDS *                          
         SPACE                                                                  
SP518    DS    0H                                                               
*                                                                               
         CLC   RCPROG,=C'CM'                                                    
         BNE   SP519                                                            
         BRAS  RE,UPDCBLD                                                       
         B     SP579                                                            
*                                                                               
SP519    DS    0H                                                               
         LA    R6,KEY              ESTABLISH STATION KEY                        
         USING STAKEY,R6                                                        
*                                                                               
         MVC   STAKEY(STAKEYLN),=17C'0'   INIT KEY                              
         MVI   STAKTYPE,C'S'       SET RECORD TYPE                              
         MVC   STAKMED,QMED        SET MEDIA                                    
         MVC   STAKCALL,QSTA       SET CALL LETTERS                             
*                                                                               
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   *+8                  NO.                                         
         MVI   KEY+6,C'C'          YES. ADJUST BAND                             
*                                                                               
         MVC   STAKAGY,QAGY        SET AGENCY                                   
*                                                                               
         CLC   QCLT,=C'ALL'        IF DOING SINGLE CLIENT                       
         BE    *+10                                                             
         MVC   STAKCLT,QCLT           SET CLIENT IN BASIC KEY                   
*                                                                               
SP520    DS    0H                                                               
*                                                                               
         L     R6,ADSTAT                                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         SPACE                                                                  
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         USING STAREC,R6           ESTABLISH FOUND STATION RECORD               
*                                                                               
         CLC   0(12,R6),KEY        SAME MEDIA/STA/AGY/CLT?                      
         BNE   SP579                                                            
*                                                                               
         CLC   STAKLEN,=AL2(STANCLNQ) IF RECORD NOT UP TO NEW LENGTH            
         BNL   SPSTALNX                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,STAKLEN        GET CURRENT LENGTH                           
*                                                                               
         LA    R1,STAREC(RE)       POINT TO END OF RECORD                       
*                                                                               
         LA    RF,STANCLNQ         NEW LENGTH                                   
         SR    RF,RE               LENGTH OF ADDED PORTION                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       INIT NEW PORTION OF RECORD                   
*                                                                               
         MVC   STAKLEN,=AL2(STANCLNQ)    UPDATE RECORD LENGTH                   
*                                                                               
SPSTALNX DS    0H                                                               
         MVC   SVOLDMKT,SMKT       SAVE FOR COMPARE LATER                       
*                                                                               
         CLC   SMKT,QBOOK1         ALREADY IN NEW MARKET                        
         BE    SP522                                                            
*                                                                               
* IF IN WRONG MARKET, DO IT ANYWAY                                              
*                                                                               
         CLC   SMKT,QMKT           WRONG MARKET?                                
         NOP   SP522                                                            
*        BNE   SP522                                                            
*                                                                               
         OC    STOLDMK1,STOLDMK1   IF MARKET PREVIOUSLY CHANGED                 
         BZ    *+16                                                             
         MVC   STOLDMK2,STOLDMK1      SAVE OLD MARKET NUMBER                    
         MVC   STOLDDT2,STOLDDT1      SAVE OLD CHANGE DATE                      
*                                                                               
         PACK  DUB,SMKT                                                         
         CVB   RF,DUB                                                           
         STCM  RF,3,STOLDMK1       SAVE CURRENT MARKET                          
         MVC   STOLDDT1,TODAYB     SET DATE OF CHANGE (BINARY)                  
*                                                                               
         MVC   SMKT,QBOOK1         SET NEW MARKET NUMBER                        
*                                                                               
SP522    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,7                                                       
         BRAS  RE,SPTRACE          *** TRACE ***                                
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP524                NO.                                         
         GOTO1 DATAMGR,DMCB,DMWRT,STATION,KEY,ADSTAT                            
         TM    DM3,X'FD'                                                        
         BZ    SP524                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
SP524    DS   0H                                                                
         CLC   SVOLDMKT,QMKT           WRONG MARKET?                            
         BE    SP525                                                            
*                                                                               
         MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         MVC   P+10(18),=CL18'ERR - STATION MKT='                               
         MVC   P+28(4),SVOLDMKT                                                 
         B     SP526                                                            
*                                                                               
SP525    DS    0H                                                               
         CLC   SVOLDMKT,QBOOK1     ALREADY IN NEW MARKET                        
         BNE   SP526C                                                           
*                                                                               
         MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         MVC   P+10(32),=CL32'ERR - STATION ALREADY IN NEW MKT'                 
*                                                                               
SP526    GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
*                                                                               
SP526C   DS    0H                                                               
         CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   SP540                NO.                                         
         CLI   KEY+1,C'C'           YES. IS IT 1ST PASS?                        
         BNE   SP527                                                            
         MVI   KEY+1,C'N'          DO 'NETWORK STATION' RECORDS                 
         MVI   KEY+6,C'N'                                                       
         B     SP520                                                            
SP527    DS    0H                                                               
         CLI   KEY+1,C'N'                                                       
         BNE   SP528                                                            
         MVI   KEY+1,C'T'          DO 'SPOT STATION' RECORDS                    
         MVI   KEY+6,C'T'                                                       
         B     SP520                                                            
SP528    DS    0H                                                               
         CLI   KEY+1,C'T'                                                       
         BE    SP540                                                            
         DC    H'0'                                                             
         SPACE                                                                  
* NOW DELETE OLD MARKET NUMBER IN STATION 'N' PASSIVE POINTERS *                
         SPACE                                                                  
SP540    MVI   KEY,C'N'                                                         
         MVC   KEY+1(2),AGENCY                                                  
         MVC   KEY+3(1),QMED                                                    
SP541    DS    0H                                                               
         MVC   KEY+4(5),OLDMSTA                                                 
         MVC   KEY+9(3),=3C'0'                                                  
         SPACE                                                                  
         CLC   QCLT,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         XC    KEY+12(L'KEY-12),KEY+12                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,25                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLC   KEY(12),0(R6)                                                    
         BNE   SP566                                                            
*                                                                               
         MVC   KEY,0(R6)                                                        
         OI    KEY+17,X'80'        MARK OLD KEY FOR DELETION                    
         OI    17(R6),X'80'                                                     
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,26                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP542                NO.                                         
         GOTO1 DATAMGR,DMCB,DMWRT,STATION,KEY,(R6)                              
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
SP542    NI    KEY+17,X'FF'-X'80'  SET OFF DELETED                              
         NI    17(R6),X'FF'-X'80'                                               
         MVC   KEY+4(2),NEWMKT                                                  
         MVC   4(2,R6),NEWMKT                                                   
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,27                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP544                NO.                                         
         GOTO1 DATAMGR,DMCB,DMADD,STATION,KEY,ADSTAT,DMWORK                     
         TM    DM3,X'FD'                                                        
         BZ    SP544                                                            
         TM    DM3,X'20'           DUPLICATE RECORD?                            
         BO    *+6                  JUST DO ERR MSG                             
         DC    H'0'                DATAMGR ERROR                                
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         BRAS  RE,PASSNERR                                                      
         IC    RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R1)                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         SPACE                                                                  
SP544    CLI   QMED,C'C'           IS THIS A CANADIAN MARKET FIX?               
         BNE   SP570                NO.                                         
         CLI   KEY+3,C'C'           YES. IS IT 1ST PASS?                        
         BNE   SP546                                                            
         MVI   KEY+3,C'N'          DO 'NETWORK STATION' RECORDS                 
         B     SP541                                                            
SP546    DS    0H                                                               
         CLI   KEY+3,C'N'                                                       
         BNE   SP548                                                            
         MVI   KEY+3,C'T'          DO 'SPOT STATION' RECORDS                    
         BE    SP541                                                            
SP548    DS    0H                                                               
         CLI   KEY+3,C'T'                                                       
         BE    SP570                                                            
         DC    H'0'                                                             
*                                                                               
SP560    DS    0H                                                               
***   PREVIOUS SECTION MOVED TO SP578                                           
SP566    DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
SP570    DS    0H                                                               
****  DOING MASTER PASSIVE C'K' KEYS HERE                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'K'                                                         
         MVC   KEY+1(2),AGENCY                                                  
         MVC   KEY+3(1),QMED                                                    
SP571    DS    0H                                                               
         MVC   KEY+4(2),OLDMKT                                                  
         MVC   KEY+6(5),QSTA                                                    
         CLC   QCLT,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   KEY+11(3),QCLT                                                   
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
         CLC   KEY(15),0(R6)                                                    
         BE    SP572                                                            
         XC    0(STKKLNQ,R6),0(R6)   20 BYTES                                   
         MVC   0(15,R6),KEY        SET UP THE ADSTAT                            
         MVI   16(R6),STKKLNQ                                                   
         B     SP577                                                            
         SPACE                                                                  
SP572    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,43                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         MVC   KEY,0(R6)                                                        
         OI    KEY+17,X'80'        MARK OLD KEY FOR DELETION                    
         OI    17(R6),X'80'                                                     
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,44                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP575                NO.                                         
         GOTO1 DATAMGR,DMCB,DMWRT,STATION,KEY,(R6)                              
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
SP575    NI    KEY+17,X'FF'-X'80'  SET OFF DELETED                              
         NI    17(R6),X'FF'-X'80'                                               
SP577    MVC   KEY+4(2),NEWMKT                                                  
         MVC   4(2,R6),NEWMKT                                                   
         SPACE                                                                  
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,45                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         SPACE                                                                  
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP579                NO.                                         
         GOTO1 DATAMGR,DMCB,DMADD,STATION,KEY,ADSTAT,DMWORK                     
         TM    DM3,X'FD'                                                        
         BZ    SP579                                                            
         TM    DM3,X'20'           DUPLICATE RECORD?                            
         BO    *+6                  JUST DO ERR MSG                             
         DC    H'0'                                                             
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         BRAS  RE,PASSKERR                                                      
         IC    RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R1)                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         B     SP579                                                            
         SPACE                                                                  
****  DOING MASTER PASSIVE C'K' KEYS HERE   MHC  09/19/06                       
SP578    DS    0H                                                               
         MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         MVC   P+10(23),=CL23'ERR - NO STATION MASTER'                          
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
         SPACE                                                                  
*                                                                               
*  BUILDING MARKET FIX RECORD BELOW SINCE THE REVERSE DOESN'T EXIST             
*                                   MHC  06/12/02                               
SP579    DS    0H                                                               
         BRAS  RE,MKTFIXIT         MARKET FIX IT                                
         B     EXIT                                                             
*                                                                               
*                                                                               
* PRINT OVERALL TOTALS FOR MULTIPLE REQUESTS *                                  
         SPACE                                                                  
SP600    L     R1,=A(ACCUMREQ)                                                  
         CP    4(4,R1),=P'1'                                                    
         BNH   EXIT                BYPASS FOR 1 REQUEST ONLY                    
         MVI   ALLOWLIN,11                                                      
         MVI   SPACING,2           PRINT SUMMARY STATS                          
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,=A(ACCUMS)                                                    
         LA    R5,NACCUMS                                                       
*                                                                               
SP602    EDIT  (P4,4(R4)),(8,P),ZERO=NOBLANK                                    
         MVC   P+10(24),8(R4)                                                   
         GOTO1 REPORT                                                           
         LA    R4,L'ACCUMS(R4)                                                  
         BCT   R5,SP602                                                         
         B     EXIT                                                             
         EJECT                                                                  
* THIS SECTION FINDS THE HIGHEST LINE NUMBER (N) FOR THIS *                     
*  STATION IN ITS OLD MARKET AND THEN FINDS THE HIGHEST   *                     
*  LINE NUMBER (M) FOR THIS STATION IN ITS NEW MARKET     *                     
*                                                                               
HIGHBUY  NTR1                                                                   
         MVI   USRSW1,C'N'                                                      
         CLI   CANAGY,C'Y'         FOR CAN AGY ONLY, NO NETWORK                 
         BNE   HBUY04                                                           
*                                                                               
         MVI   WORK,0                                                           
         MVN   WORK(1),KEY                                                      
         CLI   WORK,8              IF MEDIA=COMBINED                            
         BNE   HBUY04                                                           
         CLI   KEY+10,3            BUT IT'S A NETWORK BUY(MED=+10)              
         BE    *+12                USE SAME BUYLINE NUM                         
         CLI   WORK,3              IF MEDIA=NETWORK, NO SUBLINE CHANGE          
         BNE   HBUY04                                                           
         XC    N,N                                                              
         XC    M,M                                                              
         B     EXIT                                                             
*                                                                               
HBUY04   MVC   SAVEKEY(13),KEY                                                  
         GOTO1 HIGH                                                             
HBUY06   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST?                
         BNE   HBUY10                                                           
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY06                                                           
*                                                                               
HBUY10   MVI   N,0                1 BYTE BUYLINE                                
         MVC   N+1(1),KEYSAVE+11  N HAS HIGHEST LINE NUM IN OLD MKT             
         CLI   VGETBUY,2          2-BYTE BUYLINE?                               
         BNE   *+10               NO                                            
         MVC   N,KEYSAVE+11       YES - 2-BYTE BUYLINE                          
         MVC   KEY(13),SAVEKEY                                                  
         MVC   KEY+4(2),NEWMKT                                                  
         OC    STEQMKT,STEQMKT     IS THERE A STATION EQUIV MARKET              
         BZ    *+10                                                             
         MVC   KEY+4(2),STEQMKT    SET THE SPECIAL NEW MARKET CODE              
         MVI   TRACECDE,8                                                       
         XC    KEY+11(2),KEY+11   CLEAR FOR 2 BYTES                             
         OI    DMINBTS,X'08'      PASS THE DELETED RECORDS                      
         NI    DMOUTBTS,X'FF'-X'02'                                             
         XC    M,M                SET LINE CT                                   
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    SAME A-M/CLT/PRD/MKT/STA/EST?                 
         BNE   HBUY40              NO, NO BUYS IN NEW MARKET                    
*                                                                               
HBUY20   CLC   KEY(10),KEYSAVE    SAME A-M/CLT/PRD/MKT/STA/EST?                 
         BNE   HBUY30                                                           
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY20                                                           
*                                                                               
HBUY30   MVI   M,0                 1-BYTE BUYLINE                               
         MVC   M+1(1),KEYSAVE+11   M HAS HIGHEST LINE NUM IN NEW MKT            
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+10                NO                                           
         MVC   M,KEYSAVE+11        YES - 2-BYTE BUYLINE                         
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,3,M              HIGHEST BUY IN NEW MKT                       
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,N              HIGHEST BUY IN OLD MKT                       
         AR    RF,RE               NEXT BUYLINE NUMBER                          
         LA    RE,255              MAX 255 FOR 1 BYTE BUYLINES                  
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         LA    RE,999              MAX 999 FOR 2-BYTE BUYLINES                  
         CR    RF,RE               ARE THERE TOO MANY BUYS?                     
         BH    HBERROR             YES - ERROR                                  
*                                                                               
HBUY40   MVC   KEY(13),SAVEKEY     RESTORE LAST KEY READ BY CONTROLLER          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         OI    DMOUTBTS,X'02'                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
HBERROR  MVC   P,SPACES                                                         
         BRAS  RE,MANYERR                                                       
         IC    RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),1(R1)                                                    
*                                                                               
         MVC   P+36(5),QSTA                                                     
         MVC   P+41(1),P+40                                                     
         MVI   P+40,C'-'                                                        
         MVI   P+42,C'V'                                                        
         CLI   P+41,C'T'                                                        
         BE    *+8                                                              
         MVI   P+42,C'M'                                                        
         LLC   R1,KEYSAVE+9                                                     
         EDIT  (R1),(3,P+76)                                                    
         MVC   P+80(20),CLTNM                                                   
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   USRSW1,C'Y'                                                      
         B     HBUY40                                                           
*                                                                               
* SPDEL - THIS SECTION DELETES THE PASSIVE POINTERS *                           
*                                                                               
SPDEL    NTR1                                                                   
         OI    KEY+13,X'80'        PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   SPDEL10                                                          
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
SPDEL10  CLI   MOVECODE,C'N'       SHOULD I NOT MOVE THIS RECORD?               
         BE    EXIT                                                             
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
* THIS ROUTINE FIXES THE BUY LINE NUMBERS IN THE PACKAGE ELEMENT *              
*                                                                               
SPPKG    NTR1                                                                   
         XR    R3,R3               CLEAR R3                                     
         LA    RE,24(R2)           POINT TO THE FIRST ELEMENT                   
*                                                                               
SPPKG10  LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             IS THIS END OF THE RECORD?                   
         BE    EXIT                YES                                          
         CLI   0(RE),5             IS THIS DESIRED ELEMENT?                     
         BNE   SPPKG10             NO                                           
         LLC   R0,1(RE)            GET THE ELEMENT LENGTH                       
         SHI   R0,3                R0 HAS THE NUMBER OF 'LINE NUMBERS'          
         BP    *+6                 AT LEAST ONE LINE NUMBER?                    
         DC    H'0'                NO - DEATH                                   
         CLI   VGETBUY,2           2-BYTE BUYLINES?                             
         BNE   *+8                 NO                                           
         SRL   R0,1                YES - DIVIDE BY 2                            
         LA    RE,3(RE)            POINT TO 1ST LINE NUMBER                     
*                                                                               
SPPKG20  CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BE    SPPKG30             YES                                          
         IC    R3,0(RE)            BUYLINE NUMBER                               
         AR    R3,R6               NEW BUYLINE                                  
         STC   R3,0(RE)            ADJUST BUY LINE NUMBER                       
         LA    RE,1(RE)            BUMP TO NEXT BUYLINE NUMBER                  
         B     SPPKG40             DONE PROCESSING THIS BUYLINE                 
*                                                                               
SPPKG30  ICM   R3,3,0(RE)          2-BYTE BUYLINE NUMBER                        
         AR    R3,R6               NEW BUYLINE                                  
         STCM  R3,3,0(RE)          ADJUST BUY LINE NUMBER                       
         LA    RE,2(RE)            BUMP TO NEXT BUYLINE NUMBER                  
*                                                                               
SPPKG40  BCT   R0,SPPKG20          PROCESS NEXT BUYLINE                         
         B     EXIT                EXIT                                         
***                                                                             
* THIS ROUTINE REPLACES THE MARKET NUMBER                                       
* IN THE CANADIAN NETWORK STATION ELEMENT                                       
***                                                                             
SPCNETWK NTR1                                                                   
         LA    R6,24(R2)           POINT TO THE FIRST ELEMENT                   
SPCNT10  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE END OF THE RECORD?               
         BE    EXIT                                                             
         CLC   =X'6806',0(R6)      IS THIS A CANADIAN NTWK ELT PTR?             
         BNE   SPCNT10                                                          
         MVC   CANNET(4),2(R6)     GET THE NETWORK CALL LETTERS                 
         MVI   CANNET+4,C'N'                                                    
         MVC   CANNMKT,=17C'0'     NETWORK'S MARKET NUMBER IS ZERO              
         GOTO1 MSPACK,DMCB,CANNMKT,CANNET,CNETMSTA                              
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(R2)                                                    
         MVC   KEY+11(2),10(R2)                                                 
         MVC   KEY+4(5),CNETMSTA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+11(1),KEYSAVE+11                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY                                                           
         LA    R6,24(R2)           POINT TO THE FIRST ELEMENT                   
SPCNT20  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE END OF THE RECORD?               
         BE    SPCNT40                                                          
         CLI   0(R6),X'68'         IS THIS A CANADIAN NETWORK ELEMENT?          
         BNE   SPCNT20              NO. TRY NEXT ELEMENT                        
*                                                                               
         CLC   RCPROG,=C'CM'       IF IT'S CANADIAN CABLE REQ                   
         BNE   SPCNT22                                                          
         CLC   QOPT2(2),=C'  '     ANY SUFFIX PASSED?                           
         BE    SPCNT22                                                          
*                                                                               
         L     R1,=A(CBLMKSTA)                                                  
         CLC   0(5,R1),2(R6)      NETWK ELEM FOR REQUETED SUFFIX?               
         BNE   SPCNT20                                                          
*                                                                               
SPCNT22  MVC   DOUBLE(5),OLDMSTA                                                
         NI    DOUBLE+4,X'00'      CANADIAN NET WAS 5, NOW 8                    
         MVC   DUB(5),2(R6)                                                     
         NI    DUB+4,X'00'                                                      
         CLC   DOUBLE(5),DUB       IS THIS THE RIGHT 'OLD' MKT/STA?             
         BNE   SPCNT20                                                          
         MVC   2(2,R6),NEWMKT      REPLACE 'OLD' MKT WITH 'NEW' MKT             
         OC    STEQMKT,STEQMKT     IS THERE A STATION EQUIV MARKET              
         BZ    *+10                                                             
         MVC   2(2,R6),STEQMKT     SET THE SPECIAL NEW MARKET CODE              
         MVI   TRACECDE,8                                                       
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SPCNT25              NO.                                         
         GOTO1 PUTBUY                                                           
*                                                                               
SPCNT25  CLI   QOPT5,C'Y'                                                       
         BNE   SPCNT30                                                          
         MVC   P(132),SAVEPRT                                                   
         MVC   SAVEPRT(132),P      *** TRACE ***                                
         MVC   P,SPACES                                                         
         MVI   P,C'9'                                                           
         GOTO1 HEXOUT,DMCB,0(R6),P+10,11,=C'MIX',0                              
         MVC   P+45(21),=CL21'<== NETWORK LEVEL BUY'                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
*                                                                               
SPCNT30  MVC   KEY(13),SAVEKEY     RESTORE THE BUY RECORD                       
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY                                                           
         NI    DMINBTS,X'F7'                                                    
         OI    DMOUTBTS,X'02'                                                   
         B     EXIT                                                             
*                                                                               
* PRINT OUT NETWORK REC WITH MISSING 680B ELEMENT                               
*                                                                               
SPCNT40  MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         MVC   P+50(29),=C'NETWORK REC MISSING 680B ELEM'                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
         B     SPCNT30                                                          
         LTORG                                                                  
*                                                                               
* MERGE NEW MARKET STATION BILLING RECORD ELEMS INTO OLD *                      
*                                                                               
MRGELS   NTR1                      ADD ELEMS FROM TAPE TO FILE                  
         MVC   AREC,ADBILL        READ NEW INTO BILL                            
         OI    DMINBTS,X'08'      PASS THE DELETED RECORDS                      
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 GET                                                              
         NI    DMINBTS,X'FF'-X'08'                                              
         OI    DMOUTBTS,X'02'                                                   
         MVC   AREC,=A(SPBUFF)                                                  
         TM    KEY+13,X'80'        IF DELETED, IGNORE IT                        
         BO    ME50                                                             
         MVC   PELEM-14(11),=C'MERGED WITH'                                     
         L     R6,ADBILL                                                        
         BRAS  RE,PRTELS                                                        
*                                                                               
         L     R6,=A(SPBUFF)       I/O BUFFER                                   
         LA    R6,24(R6)          1ST ELEM IN OLD                               
*                                                                               
ME10     CLI   0(R6),0             END OF OLD STA BILLING REC                   
         BE    ME20                                                             
         LLC   R0,1(R6)            NEXT OLD MKT REC ELEM                        
         AR    R6,R0                                                            
         B     ME10                                                             
*                                                                               
ME20     L     R3,ADBILL          NEW MARKET REC                                
         LA    R3,24(R3)                                                        
*                                                                               
ME30     CLI   0(R3),0             END OF REC                                   
         BE    ME40                                                             
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AREC),(R3),(R6)                                 
*                                                                               
         LLC   R0,1(R3)            NEXT FILE ELEM                               
         AR    R3,R0                                                            
         LLC   R0,1(R6)            NEXT INSERT ADDR                             
         AR    R6,R0                                                            
         B     ME30                                                             
*                                                                               
ME40     B     EXIT                                                             
*                                                                               
ME50     MVC   P1+5(33),=C'WRITTEN OVER DELETED STATION BILL'                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,BYTE                                                 
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES *                                                              
*                                                                               
MKTFIXNG DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   P+10(45),=CL45'MARKET FIX REVERSES ANOTHER REQUEST THIS +        
               WEEK'                                                            
         B     ERROR10                                                          
*                                                                               
BADMKT   DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   P+10(31),=CL31'BAD MARKET NUMBER - CHECK INPUT'                  
         B     ERROR10                                                          
*                                                                               
ERROR1   MVC   P,SPACES                                                         
         MVC   P+10(21),=CL21'MARKET NAME NOT FOUND'                            
*                                                                               
ERROR10  MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
* HEADHOOK ROUTINE *                                                            
*                                                                               
         DROP  RB,R7,R8                                                         
         USING *,RF                                                             
SPSXHDHK NTR1                                                                   
         LM    R7,RB,SPSXR7                                                     
         DROP  RF                                                               
         USING SPSX02,RB,R7,R8                                                  
         MVC   H3+51(5),QSTA                                                    
         CLC   RCPROG,=C'CM'       SHOW SUFFIX ON CM                            
         BNE   SPSXHD10                                                         
         MVI   H3+55,C'/'                                                       
         MVC   H3+56(2),QOPT2                                                   
         B     SPSXHD20                                                         
SPSXHD10 MVC   H3+56(1),H3+55                                                   
         MVI   H3+55,C'-'                                                       
         CLI   H3+56,C'T'                                                       
         BNE   *+8                                                              
         MVI   H3+57,C'V'                                                       
         CLI   H3+56,C'A'                                                       
         BE    *+12                                                             
         CLI   H3+56,C'F'                                                       
         BNE   *+8                                                              
         MVI   H3+57,C'M'                                                       
SPSXHD20 MVC   H4+49(4),QMKT                                                    
         MVC   H4+56(24),MARKETNM                                               
         MVC   H5+49(4),QBOOK1                                                  
         MVC   H5+56(24),NEWNAME                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* DATA *                                                                        
*                                                                               
SAVEKEY  DC    XL20'00'                                                         
SAVEKEY2 DC    XL20'00'                                                         
SAVEKEY3 DC    XL20'00'                                                         
BUYSW    DS    C                                                                
CANAGY   DS    C                                                                
M        DS    XL2                                                              
N        DS    XL2                                                              
SVOLDLIN DS    XL2                                                              
MOVECODE DS    C                   'N' DO NOT MOVE RECORD TO NEW STN            
MKTFFLAG DS    CL1                                                              
RVRSMKTF EQU   X'80'                - REVERSE MKTFIX EXISTS, SAVE WEEK          
MKTFHERE EQU   X'40'                - MKTFIX EXISTS, ADD ELEMENT ONLY           
WEEK1    DS    CL2                 WEEK/YEAR IN BINARY FOR OLD REQUEST          
WEEK2    DS    CL2                 WEEK/YEAR IN BINARY FOR TODAY                
SAVEBCLT DS    CL2                 SAVE THE BINARY CLIENT                       
EDATE    DS    CL6                 TEMPORARY EBCDIC DATE STORAGE                
*                                                                               
         DS    0F                                                               
SPSXR7   DC    5F'0'               SAVE AREA FOR R7, R8, R9, RA AND RB          
*                                                                               
SVOLDMKT DS    CL4                                                              
NEWMKT   DS    H                   REQUESTED NEW MARKET                         
STEQMKT  DS    H                   STATION EQUIVALENCE MARKET                   
SVEQMKT  DS    H                   SAVED STATION EQUIV MARKET                   
OLDMSTA  DS    0CL5                                                             
OLDMKT   DS    H                                                                
OLDSTA   DS    CL3                                                              
MARKETNM DS    CL24                                                             
NEWNAME  DS    CL24                                                             
CANNMKT  DS    CL4                                                              
CANNET   DS    CL5                                                              
CNETMSTA DS    CL5                 CANADIAN NETWORK CALL LETTERS                
SVAREC   DS    A                                                                
NOLDSTCD DS    XL1                 OLD STATION CODE                             
NNEWSTCD DS    XL1                 NEW STATION CODE                             
NNWSSTA  DS    CL8                 NWS STATION CALL LETTERS                     
SEQNUM   DS    XL2                 SEQUENCE NUMBER                              
STAERR   DS    C'N'                STATION TO SWITCH ALREADY IN HEADER          
SMINMKEY DS    CL(L'MINMKEY)       SAVED MINIO MASTER KEY                       
SMINEKEY DS    CL(L'MINEKEY)       SAVED MINIO ELEMENT KEY                      
ELEM     DS    CL128               NEW ELEMENT AREA                             
SVB1PROF DS    CL16                B1 PROFILE                                   
SVB1XPRF DS    CL16                B1X PROFILE                                  
SVBUYER  DS    XL2                 RC-PACKED BUYER CODE                         
MKTALPH  DS    XL3                 NEW MARKET'S ALPHA MARKET CODE               
*                                                                               
* THIS ROUTINE PRINTS THE OLD AND NEW LINE NUMBERS FOR EACH PRODUCT *           
*                                                                               
         USING BUYRECD,R2                                                       
SPPRINT  NMOD1 0,*SPPRNT*                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   QMED,C'C'           FOR COMBINED, PRINT MEDIA                    
         BNE   SPPRT00                                                          
         MVI   DUB,0                                                            
         MVN   DUB(1),0(R2)        GET MEDIA                                    
         CLI   DUB,1               TV?                                          
         BNE   *+12                                                             
         MVI   P+3,C'T'                                                         
         B     SPPRT00                                                          
         CLI   DUB,3               NETWORK?                                     
         BNE   *+12                                                             
         MVI   P+3,C'N'                                                         
         B     SPPRT00                                                          
         DC    H'0'                                                             
*                                                                               
SPPRT00  L     R4,ADCLT                                                         
         USING CLTHDRD,R4                                                       
         MVC   P+5(3),CLT          GET THE CLIENT CODE                          
         MVC   P+10(20),CLTNM      GET THE NAME OF THE CLIENT                   
*                                                                               
         OC    STEQMKT,STEQMKT     WAS THERE A SPECIAL MARKET                   
         BZ    SPPRT06              NO                                          
         LH    R0,STEQMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+32(4),DUB                                                      
*                                                                               
SPPRT06  LA    R6,CLIST                                                         
*                                                                               
SPPRT10  CLC   3(1,R6),BUYKEY+3    IS THIS THE SAME PRODUCT CODE?               
         BE    SPPRT20                                                          
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   SPPRT10                                                          
         MVC   P+37(20),=CL20'PRODUCT CODE UNKNOWN'                             
         B     SPPRT30                                                          
*                                                                               
SPPRT20  MVC   P+47(3),0(R6)       GET THE PRODUCT MNEMONIC                     
*                                                                               
SPPRT30  LLC   R0,9(R2)            GET THE ESTIMATE NUMBER                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+64),ZERO=NOBLANK                                   
*                                                                               
         TM    BUYKEY,X'08'        THIS SAVED BUYS REC                          
         BZ    *+8                                                              
         MVI   P+68,C'*'                                                        
         CLI   MOVECODE,C'N'       SHOULD I NOT MOVE THIS RECORD?               
         BE    SPPRT40                                                          
*                                                                               
         LLC   R0,10(R2)           GET THE NEW BUY NUMBER                       
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         ICM   R0,3,10(R2)         YES                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+111),ZERO=NOBLANK                                  
*                                                                               
SPPRT40  XR    R0,R0               CLEAR R0                                     
         ICM   R0,3,SVOLDLIN       OLD BUY LINE NUMBER                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+86),ZERO=NOBLANK                                   
*                                                                               
         CLI   MOVECODE,C'N'       SHOULD I NOT MOVE THIS RECORD?               
         BNE   SPPRT90                                                          
         MVC   P+90(17),=C'PW EST, NOT MOVED'                                   
         LA    R1,WORK                                                          
         USING PWTABD,R1                                                        
         MVC   PWTABCLT,CLT                                                     
         MVC   PWTABPRD,P+47                                                    
         MVC   PWTABEST,P+64                                                    
         XC    PWTABCT,PWTABCT                                                  
*                                                                               
         XC    ELEM,ELEM           SAVE AREA FOR PRODUCTS                       
         LA    R6,ELEM                                                          
*                                                                               
         CLI   BUYRECD+3,X'FF'     THIS A POOL BUY                              
         BNE   SPPRT60              NO, GOT PRODUCT ALREADY                     
*                                                                               
* SEARCH BUY FOR ALL PRODUCTS - 0B ELEMS                                        
*                                                                               
         LA    R1,BUYRECD+24                                                    
SPPRT50  CLI   0(R1),11                                                         
         BL    SPPRT54                                                          
         CLI   0(R1),13                                                         
         BH    SPPRT54                                                          
*                                                                               
         CLI   1(R1),10            UNALLOC SPOT                                 
         BE    SPPRT54                                                          
*                                                                               
         BAS   RE,SVPWPRD          SAVE PRODS IN ELEM                           
*                                                                               
SPPRT54  LLC   R0,1(R1)            NEXT ELEM                                    
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   SPPRT50              NO                                          
*                                                                               
SPPRT60  LA    R0,PWTABSIZ                                                      
         L     R1,=A(PWTAB)                                                     
SPPRT64  OC    PWTABENT,PWTABENT   EMPTY ENTRY                                  
         BZ    SPPRT66              YES, ADD THIS                               
         CLC   PWTABCOM,WORK       EQUAL TO PREVIOUS                            
         BE    SPPRT68              YES, DONE                                   
         LA    R1,PWTABNXT                                                      
         BCT   R0,SPPRT64                                                       
         DC    H'0'                                                             
SPPRT66  MVC   PWTABENT,WORK                                                    
*                                                                               
SPPRT68  SR    RF,RF                                                            
         ICM   RF,3,PWTABCT                                                     
         LA    RF,1(,RF)                                                        
         STCM  RF,3,PWTABCT                                                     
*                                                                               
         CLI   0(R6),0             ANY MORE PRODUCTS TO TABLE                   
         BE    SPPRT90              NO                                          
*                                                                               
         LA    R0,220                                                           
         LA    RE,CLIST                                                         
         DROP  R4                                                               
SPPRT70  CLC   0(1,R6),3(RE)                                                    
         BE    SPPRT74                                                          
         LA    RE,4(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         BCT   R0,SPPRT70                                                       
         DC    H'0'                                                             
*                                                                               
SPPRT74  MVC   PWTABPRD-PWTABD+WORK,0(RE)                                       
         LA    R6,1(,R6)                                                        
         B     SPPRT60                                                          
*                                                                               
SPPRT90  DS    0H                                                               
         GOTO1 REPORT                                                           
SPPRTX   J     EXIT                                                             
         DROP  R1,R2                                                            
*                                                                               
* GET AND STORE ALL PRODS FOR PW REPORT PRINTED REPORT FOR WILA                 
*                                                                               
SVPWPRD  NTR1                                                                   
         LA    R0,220                                                           
         LA    RE,ELEM                                                          
SVPWP20  CLI   0(RE),0             EMPTY SLOT IN TABLE                          
         BE    SVPWP40                                                          
         CLC   0(1,RE),10(R1)      SAME PROD                                    
         BE    SPPRTX                                                           
         LA    RE,1(,RE)                                                        
         BCT   R0,SVPWP20                                                       
         DC    H'0'                                                             
*                                                                               
SVPWP40  MVC   0(1,RE),10(R1)                                                   
         CLI   1(R1),14            P/B PROD                                     
         BE    SPPRTX               NO                                          
         LA    R0,220                                                           
         LA    RE,ELEM                                                          
*                                                                               
SVPWP60  CLI   0(RE),0                                                          
         BE    SVPWP80                                                          
         CLC   0(1,RE),14(R1)      SAME PROD                                    
         BE    SPPRTX                                                           
         LA    RE,1(,RE)                                                        
         BCT   R0,SVPWP60                                                       
         DC    H'0'                                                             
*                                                                               
SVPWP80  MVC   0(1,RE),14(R1)                                                   
         B     SPPRTX                                                           
         DROP  RB                                                               
*                                                                               
* THIS SECTION UPDATES THE STATION LOCKIN HEADER RECORDS *                      
*                                                                               
ST300    NMOD1 0,**ST300*                                                       
         LA    RC,SPACEND                                                       
*                                                                               
ST301    XC    XKEY,XKEY           BUILD KEY                                    
         LA    R6,XKEY                                                          
K        USING XSLKRECD,R6                                                      
         MVI   K.XSLKKTYP,XSLKKTYPQ SAVE TYPE/AGMD/CLT                          
         MVI   K.XSLKKSUB,XSLKKSUBQ                                             
         MVC   K.XSLKKAGMD,BAGYMD                                               
         MVC   SAVEKEYX,XKEY                                                    
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
ST305    GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         BE    ST320                                                            
         DC    H'0'                                                             
*                                                                               
ST300X   J     EXIT                                                             
*                                                                               
ST310    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',XKEY,XKEY                         
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ST320    LA    R6,XKEY                                                          
         CLC   XKEY(18),SAVEKEYX    TEST SAME TYPE/AGMD                         
         BNE   ST300X               NO -- EXIT                                  
*                                                                               
         CLC   XKEY+18(2),XKEYSAVE+18  ALREADY CHECKED THIS CLIENT?             
         BE    ST321                   YES                                      
*                                                                               
         MVC   HALF,XKEY+18        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BNE   ST322               NO, FORCE NEXT CLIENT                        
*                                                                               
ST321    CLC   K.XSLKKMKT(5),OLDMSTA   HAVE THIS MKT/STA?                       
         BE    ST330                   YES                                      
         CLC   OLDSTA,K.XSLKKSTA   IS THIS THE REQUESTED STATION?               
         BE    MKTERRX             YES - WHY IS IT IN WRONG MARKET?             
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE STATION?                        
         BL    ST321A              NO                                           
         MVC   DUB(5),K.XSLKKMKT   MKT/STA                                      
         NI    DUB+4,X'80'         STRIP NETWORK BITS                           
         CLC   OLDMSTA,DUB         DO THEY MATCH?                               
         BE    ST330               YES, PROCESS THIS                            
*                                                                               
ST321A   CLC   K.XSLKKMKT(5),OLDMSTA   PAST THIS MKT/STA?                       
         BH    ST322                   YES - FORCE NEXT CLIENT                  
*                                                                               
         MVC   K.XSLKKMKT(5),OLDMSTA   BUMP TO THIS MKT/STA                     
         XC    K.XSLKKPRD(7),K.XSLKKPRD CLEAR THE REST OF THE KEY               
         B     *+10                    AND READHI                               
*                                                                               
ST322    MVC   K.XSLKKMKT(12),XFF  FORCE NEXT CLIENT                            
         MVC   XKEYSAVE,XKEY                                                    
         B     ST305                                                            
*                                                                               
ST330    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   ST331                                                            
         MVI   TRACECDE,33                                                      
         BRAS  RE,XPTRAC           *** TRACE ***                                
         MVI   TRACECDE,34                                                      
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
ST331    LA    RF,ACCUSLXQ                                                      
         BAS   RE,BUMPACC                                                       
*                                                                               
         L     R6,AREC                                                          
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEYX,XKEY       SAVE OLD KEY                                 
*                                                                               
         OI    34(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST334                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ST334    LA    R6,XKEY                                                          
         MVC   K.XSLKKMKT(2),NEWMKT   MOVE NEW MARKET INTO KEY                  
         L     R6,AREC                                                          
         MVC   XSLKKMKT-XSLKRECD(2,R6),NEWMKT            & RECORD               
         NI    34(R6),X'7F'        UNDELETE THE NEW RECORD                      
*                                                                               
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY           
*                                                                               
         CLC   XKEY(32),XKEYSAVE   DOES RECORD ALREADY EXIST                    
         BNE   ST338                NO, ALL OKAY                                
*                                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,K.XSLKKCLT,P+14                                      
         GOTO1 MSUNPK,DMCB,K.XSLKKMKT,P+20,P+26                                 
         MVC   P+32(40),=CL40'* ERROR * LOCK REC EXISTS IN NEW MARKET'          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   XKEYSAVE,SAVEKEYX                                                
         MVC   XKEY,SAVEKEYX                                                    
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY             
*                                                                               
         CLC   XKEY(32),XKEYSAVE   RESTORE SEQ READ                             
         BE    ST310                OKAY                                        
         DC    H'0'                                                             
*                                                                               
ST338    CLI   RCWRITE,C'Y'                                                     
         BNE   ST340                                                            
         MVC   XKEY,XKEYSAVE                                                    
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0                                                            
         BE    ST340                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
ST340    NI    DMINBTS,X'FF'-X'08'  RESET PASS DELETED RECORDS                  
         OI    DMOUTBTS,X'02'                                                   
*                                                                               
         MVC   XKEY,SAVEKEYX       RESTORE KEY                                  
         MVC   XKEYSAVE,SAVEKEYX   RESTORE KEY                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY           
         CLI   DM3,0                                                            
         BE    ST344                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
ST344    CLC   XKEY(32),XKEYSAVE                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    XKEY+32,X'80'       MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   ST310                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
         CLI   DM3,0                                                            
         BE    ST310                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
MKTERRX  MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,K.XSLKKCLT,P+14                                      
         GOTO1 MSUNPK,DMCB,K.XSLKKMKT,P+20,P+25                                 
         MVC   P+32(37),=CL37'* ERROR * 0D73 REC STA IS IN DIFF MKT'            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     ST321A                                                           
* PRINT ELEMENTS *                                                              
PRTELS   NTR1  BASE=*,LABEL=*                                                   
         SR    R3,R3                                                            
         MVI   BYTE,X'0E'                                                       
         BRAS  RE,GETEL                                                         
         BNE   PRTELS40            EMPTY REC                                    
         USING STABELEM,R6                                                      
PRTELS10 LA    R3,1(,R3)           ADD TO ELEM CT                               
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PELEM,DUB                                                        
         MVC   WORK(2),STABPER                                                  
         MVI   WORK+1,01                                                        
         GOTO1 DATCON,DMCB,(3,WORK),(6,PBILLP)                                  
         GOTO1 (RF),(R1),(2,STABBDT),(5,PBILLD)                                 
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,DUB)                                  
*                                                                               
         GOTO1 BINSRCH,CLTBPARS,HALF                                            
         CLI   0(R1),1             RECORD FOUND?                                
         JE    EXIT                NO, EXIT                                     
         SR    R3,R3                                                            
         ICM   R3,7,1(R1)          RECORD FOUND                                 
         JZ    EXIT                YES                                          
*                                                                               
         USING CLTBUFFD,R3                                                      
         XC    WORK,WORK           GET THE B1 PROFILE                           
         MVC   WORK(4),=C'S0B1'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         GOTO1 CLUNPK,DMCB,CBCLT,WORK+7                                         
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CBOFF                                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),SVB1PROF,DATAMGR                       
         DROP  R3                                                               
*                                                                               
         MVC   WORK(4),=C'SB1X'    GET B1X PROFILE                              
         NI    WORK,255-X'40'                                                   
         GOTO1 GETPROF,DMCB,(X'C0',WORK),SVB1XPRF,DATAMGR                       
*                                                                               
         L     RF,ADCONLST                                                      
         L     RF,VSPFMINO-SPADCONS(RF)    A(SPFMTINO)                          
         GOTO1 (RF),DMCB,DUB,(2,STABINV),(MED,SVB1PROF),SVB1XPRF                
         L     RF,DMCB+4                                                        
         MVC   PINVNO(7),0(RF)                                                  
*                                                                               
         EDIT  (B4,STABGRS),(12,PGROSS),2,COMMAS=YES,MINUS=YES                  
         EDIT  (B4,STABNET),(12,PNET),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,STABSPTS),(6,PSPOTS),0,MINUS=YES                             
         CLI   1(R6),18                                                         
         BE    PRTELS20                                                         
         EDIT  (B3,STABTAX),(10,PTAX),2,MINUS=YES                               
PRTELS20 GOTO1 REPORT                                                           
         BRAS  RE,NEXTEL                                                        
         BE    PRTELS10                                                         
         J     EXIT                                                             
         DROP  R6                                                               
PRTELS40 CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PELEM,DUB                                                        
         MVC   PELEM-1(8),=C'ELEMENTS'                                          
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
XPTRAC   NTR1  BASE=*,LABEL=*                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         LA    RF,TRACEMSG                                                      
         SR    RE,RE                                                            
         ICM   RE,1,TRACECDE       CUSTOM TRACE MESSAGE?                        
         BZ    XPTRAC10            YES                                          
         BCTR  RE,0                                                             
         MHI   RE,24                                                            
         L     RF,=A(TRACEHD)                                                   
         AR    RF,RE                                                            
XPTRAC10 MVC   P(24),0(RF)                                                      
         MVC   P+26(3),=C'KEY '                                                 
         GOTO1 HEXOUT,DMCB,XKEY,P+32,40,=C'MIX',0                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+26(4),=C'KEYS'                                                 
         GOTO1 HEXOUT,DMCB,XKEYSAVE,P+32,40,=C'MIX',0                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+26(4),=C'SVKX'                                                 
         GOTO1 HEXOUT,DMCB,SAVEKEYX,P+32,40,=C'MIX',0                           
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* THIS SECTION UPDATES THE MATCHING STATUS RECORDS *                            
*                                                                               
*  NOTE, DO NOT TOUCH R8 - BASE TO ADCONS IN BASE PROGRAM                       
*                                                                               
ST400    NMOD1 0,**ST400*                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         XC    SMINEKEY,SMINEKEY   BUILD KEY                                    
         XC    SMINMKEY,SMINMKEY   SAVE TYPE/AGMD                               
         LA    R6,SMINMKEY                                                      
         USING MSRKEYD,R6                                                       
         MVI   MSRKTYPE,MSRKTYPQ                                                
         MVI   MSRKSUB,MSRKSUBQ                                                 
         MVC   MSRKAM,BAGYMD                                                    
         MVC   SMINEKEY(3),SMINMKEY SAVE KEY                                    
*                                                                               
ST405    GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',SMINEKEY,SMINMKEY                 
         B     ST415                                                            
*                                                                               
ST410    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',SMINEKEY,SMINMKEY                 
*                                                                               
ST415    LA    R6,SMINMKEY               POINT TO KEY AGAIN                     
*                                                                               
         CLC   SMINEKEY(3),SMINMKEY      TEST SAME TYPE/AGMD                    
         BNE   ST400X                     NO -- EXIT                            
*                                                                               
         TM    SMINMKEY+32,X'80'   THIS KEY DELETED                             
         BO    ST410                YES, BYPASS                                 
*                                                                               
         CLC   SMINEKEY+3(2),SMINMKEY+3  ALREADY CHECKED THIS CLIENT?           
         BE    ST420                     YES                                    
*                                                                               
         MVC   HALF,MSRKCLT          BINARY CLIENT                              
         BRAS  RE,CLTCHK             OK TO PROCESS THIS CLIENT?                 
         BE    ST420                 YES                                        
         MVC   MSRKPRD(27),XFF       FORCE NEXT CLIENT                          
         MVC   SMINEKEY(32),SMINMKEY                                            
         B     ST405                                                            
*                                                                               
ST420    CLC   MSRKMKT(5),OLDMSTA    RECORD WITH OLD MKSTA?                     
         BE    ST430                 YES                                        
         CLI   QSTA,C'0'             THIS A CABLE STATION?                      
         BL    ST421                 NO                                         
         MVC   DUB(5),MSRKMKT                                                   
         NI    DUB+4,X'80'                                                      
         CLC   OLDMSTA,DUB           REQUESTED CABLE STATION?                   
         BE    ST430                 YES                                        
*                                                                               
ST421    CLC   MSRKMKT(5),OLDMSTA    PAST OLD MKT/STA?                          
         BH    ST422                 YES, FORCE NEXT CLIENT                     
         MVC   MSRKMKT(5),OLDMSTA    LOOK FOR RECORD WITH OLD MKSTA             
         XC    MSRKMOS(19),MSRKMOS                                              
         MVC   SMINEKEY(32),SMINMKEY                                            
         B     ST405                                                            
*                                                                               
ST422    MVC   MSRKMKT(24),XFF       FORCE NEXT ESTIMATE                        
         MVC   SMINEKEY(32),SMINMKEY                                            
         B     ST405                                                            
*                                                                               
ST430    LA    RF,ACCUMSTQ                                                      
         MH    RF,=Y(L'ACCUMS)                                                  
         A     RF,=A(ACCUMS)                                                    
         AP    0(4,RF),=P'1'                                                    
         MVC   ELEM(32),SMINMKEY   SAVE OLD KEY                                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,28                                                      
         BAS   RE,XPTRACE          *** TRACE ***                                
         L     R6,ADCOMREC                                                      
         LR    R2,R6               SAVE REC ADDR                                
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',SMINMKEY+36,(R6),DMWORK           
*                                                                               
         OI    MSRRSTAT,X'80'      MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST434                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',SMINMKEY+36,(R6),DMWORK           
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
ST434    MVC   SMINMKEY+MSRKMKT-MSRKEY(2),NEWMKT    MOVE NEW MKT TO KEY         
         MVC   MSRKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    MSRRSTAT,X'7F'      UNDELETE THE NEW RECORD                      
         MVC   SMINEKEY,SMINMKEY                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',SMINEKEY,        C        
               SMINMKEY                                                         
*                                                                               
         CLC   SMINMKEY(32),SMINEKEY    DOES REC ALREADY EXIST                  
         BE    ST440                     YES - NEED TO MERGE OR WRITE           
         MVC   SMINMKEY(40),SMINEKEY                                            
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,29                                                      
         BAS   RE,XPTRACE          *** TRACE ***                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   ST490                                                            
*                                                                               
*        ADD NEW KEY AND RECORD TO XSPOT FILE                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',SMINMKEY+36,(R6),DMWORK           
         CLI   DM3,0                                                            
         BE    ST490                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
* RECORD ALREADY EXISTS IN NEW MARKET, SEE IF DELETED *                         
*                                                                               
ST440    TM    SMINMKEY+32,X'80'        IS IT DELETED                           
         BZ    ST450                     NO, MERGE IT                           
*                                                                               
         NI    SMINMKEY+32,X'7F'        RESTORE IT                              
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,30                                                      
         BAS   RE,XPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST442                                                            
*                                                                               
*        RESTORE NEW KEY                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',SMINMKEY,SMINMKEY                  
*                                                                               
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
ST442    L     R6,ADBUY                                                         
         GOTO1 (RF),(R1),(X'08',GETREC),=C'XSPFIL',SMINMKEY+36,(R6),   C        
               DMWORK                                                           
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST490                                                            
*                                                                               
         L     R6,ADCOMREC                                                      
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',SMINMKEY+36,(R6),DMWORK           
*                                                                               
         CLI   DM3,0                                                            
         BE    ST490                                                            
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
* MERGE 2 RECORDS HERE - REC WITH HIGHER MSRSTDAT IS USED WITH ALL              
*  ELEMENTS EXCEPT X'13' & X'14'                                                
*  HIGHER MSRICDAT AND HIGHER MSRIPDAT ARE USED                                 
*  R2 (ADBUY) IS RECORD IN NEW MARKET, R3 (ADCOMREC) IS OLD MARKET              
*                                                                               
ST450    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,31                                                      
         BAS   RE,XPTRACE          *** TRACE ***                                
         L     R6,ADBUY                                                         
         LR    R2,R6                                                            
*                                                                               
* GET EXISTING REC IN NEW MARKET                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),=C'XSPFIL',SMINMKEY+36,(R6),C        
               DMWORK                                                           
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         L     R3,ADCOMREC         OLD MARKET REC                               
*                                                                               
         MVI   BYTE,X'10'                                                       
         BAS   RE,GETELM                                                        
         BNE   ST452               IGNORE NEW RECORD                            
*                                                                               
         LR    R5,R6                                                            
*                                                                               
* THIS WAS DONE EARLIER                                                         
*                                                                               
         L     R6,ADCOMREC         OLD MARKET REC                               
         BAS   RE,GETELM                                                        
         BNE   ST410                                                            
*                                                                               
         CLC   MSRSTDAT-MSRSTELD(,R5),MSRSTDAT-MSRSTELD(R6)                     
         BH    ST454               USE NEW MSR REC                              
*                                                                               
* R2 POINTS TO RECORD USED FOR UPDATE, R3 TO RECORD TO COMPARE ELEMS            
*                                                                               
ST452    LR    RF,R2                                                            
         LR    R2,R3               REVERSE POINTERS, USE OLD                    
         LR    R3,RF                                                            
*                                                                               
ST454    MVI   BYTE,X'13'                                                       
         LR    R6,R2                                                            
         SR    R4,R4                                                            
         BAS   RE,GETELM                                                        
         BNE   *+6                                                              
         LR    R4,R6                                                            
*                                                                               
         LR    R6,R3                                                            
         SR    R5,R5                                                            
         BAS   RE,GETELM                                                        
         BE    ST456                                                            
         LTR   R4,R4               WAS THERE AN ELEM IN OLD                     
         BZ    ST460                NO ELEM IN EITHER REC                       
*                                                                               
*        ADD ELEM FROM OLD                                                      
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'XSPFIL'),(R2),(R4),0                     
         CLI   12(R1),0                                                         
         BE    ST460                                                            
         DC    H'0'                                                             
ST456    LTR   R4,R4               WAS THERE AN ELEM IN OLD                     
         BZ    ST460                NO                                          
         CLC   MSRICDAT-MSRICELD(,R4),MSRICDAT-MSRICELD(R6)                     
         BNH   ST460                                                            
         MVC   0(MSRICLNQ,R4),0(R6)                                             
*                                                                               
ST460    MVI   BYTE,X'14'                                                       
         LR    R6,R2                                                            
         SR    R4,R4                                                            
         BAS   RE,GETELM                                                        
         BNE   *+6                                                              
         LR    R4,R6                                                            
*                                                                               
         LR    R6,R3                                                            
         SR    R5,R5                                                            
         BAS   RE,GETELM                                                        
         BE    ST466                                                            
         LTR   R4,R4               WAS THERE AN ELEM IN OLD                     
         BZ    ST470                NO ELEM IN EITHER REC                       
*                                                                               
*        ADD ELEM FROM OLD                                                      
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'XSPFIL'),(R2),(R4),0                     
         CLI   12(R1),0                                                         
         BE    ST470                                                            
         DC    H'0'                                                             
*                                                                               
ST466    LTR   R4,R4               WAS THERE AN ELEM IN OLD                     
         BZ    ST470                NO                                          
         CLC   MSRIPDAT-MSRIPELD(,R4),MSRIPDAT-MSRIPELD(R6)                     
         BNH   ST470                                                            
         MVC   0(MSRIPLNQ,R4),0(R6)                                             
*                                                                               
ST470    CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   ST476                                                            
         LA    R4,=CL20'MERGED MSR RECORD'                                      
         SR    R5,R5                                                            
         ICM   R5,3,32(R2)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R2),C'DUMP',(R5),=C'0D'               
*                                                                               
ST476    CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   ST490                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',SMINMKEY+36,(R2),DMWORK           
*                                                                               
ST490    XC    SMINEKEY,SMINEKEY                                                
         MVC   SMINEKEY(32),ELEM         RESTORE KEY                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',SMINEKEY,SMINMKEY         
*                                                                               
         CLC   SMINMKEY(32),SMINEKEY                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    SMINMKEY+32,X'80'    MARK OLD KEY FOR DELETION                   
*                                                                               
         CLI   RCWRITE,C'Y'         TEST WRITE=NO                               
         BNE   ST496                                                            
*                                                                               
*        DELETE OLD KEY                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',SMINMKEY,SMINMKEY                  
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
ST496    B     ST410               NEXT RECORD                                  
*                                                                               
ST400X   J     EXIT                                                             
*                                                                               
GETELM   LA    R6,42(R6)                                                        
FIRSTELM CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   BYTE,0                                                           
         BCR   8,RE                                                             
         CLC   BYTE,0(R6)                                                       
         BCR   8,RE                                                             
NEXTELM  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R6),1                                                          
         BR    RE                                                               
         AR    R6,RF                                                            
         B     FIRSTELM                                                         
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
XPTRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         LLC   RE,TRACECDE                                                      
         BCTR  RE,0                                                             
         MHI   RE,24                                                            
         L     RF,=A(TRACEHD)                                                   
         AR    RF,RE                                                            
         MVC   P(24),0(RF)                                                      
         GOTO1 HEXOUT,DMCB,SMINMKEY,P+30,40,=C'MIX',0                           
*                                                                               
         CLI   MOVECODE,C'N'       IF RECORD NOT MOVED                          
         BNE   *+10                                                             
         MVC   P+75(30),=C'RECORD NOT MOVED TO NEW MARKET'                      
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         B     ST400X                                                           
*                                                                               
         LTORG                                                                  
***********************************************************************         
MKTFIXIT NTR1  BASE=*,LABEL=*                                                   
         MVC   SVAREC,AREC                                                      
         MVC   AREC,=A(SPBUFF)                                                  
*                                                                               
         L     RE,AREC             CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AREC                                                          
         USING MKTFXRCD,R6                                                      
         MVI   MKFKTYPE,MKFKTYPQ   (X'0D') RECORD TYPE                          
         MVI   MKFKSBTY,MKFKSBTQ   (X'6D') SUBRECORD TYPE                       
         MVC   MKFKAGMD,BAGYMD     AGENCY/MEDIA                                 
         CLC   QCLT,=CL3'ALL'                                                   
         BNE   *+14                                                             
         MVC   MKFKCLT,=XL2'FFFF'                                               
         B     *+10                                                             
         MVC   MKFKCLT,BCLT        CLIENT (PACKED)                              
         MVC   MKFKOMKT,OLDMKT     OLD MARKET (HEX)                             
         MVC   MKFKSTA,BSTA        STATION (PACKED)                             
         CLC   RCPROG,=C'CM'       CANADIAN CABLE REQ?                          
         BNE   *+14                NO                                           
         L     R1,=A(CBLMKSTA)                                                  
         MVC   MKFKSTA,2(R1)       STATION WITH SUFFIX                          
         MVC   MKFKNMKT,NEWMKT     NEW MARKET (HEX)                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                DOES RECORD PREVIOUSLY EXIST?                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SP585                - NO, CONTINUE LIKE NORMAL                  
         OI    MKTFFLAG,MKTFHERE    - YES, ADD ELEMENT ONLY (X'80')             
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,SPTFILE,KEY+14,AREC,DMWORK                   
*                                                                               
         LA    R7,MKFFRST          NEED R7 TO POINT TO FIRST ELEMENT            
SP580    CLI   0(R7),0             ANY ELEMENT HERE?                            
         BE    SP590                - NOPE, GOOD TO GO                          
*        LLC   R0,1(R7)             - YEAH, BUMP UNTIL WE GET PAST LAST         
         LLC   R0,1(R7)             - YEAH, BUMP UNTIL WE GET PAST LAST         
         AR    R7,R0                                                            
         B     SP580                                                            
*                                                                               
*  GOING TO BUILD INFO - DETAILS ELEMENT BELOW                                  
SP585    LA    R7,MKFFRST          R7 WILL NOW POINT RIGHT AFTER KEY            
SP590    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING MKFIDELD,R5                                                      
         MVI   MKFIDEL,MKFIDELQ    X'10' ELEMENT CODE                           
         MVI   MKFIDLEN,MKFIDLNQ   1+1+2+12+2+10=28 BYTES                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,MKFIDDAT)   SAVE CREATED DATE          
*        GOTO1 DATCON,DMCB,(5,0),(2,MKFIDDAT)   SAVE TODAY'S DATE               
         MVC   MKFIDRQR,QUESTOR    SAVE REQUESTOR'S NAME                        
*                                                                               
         L     R4,VMASTC                                                        
         USING MASTD,R4                                                         
         LA    R3,MCRFHDR                                                       
         USING RQHITRM,R3                                                       
         MVC   MKFIDATH,RQHPSWD    SAVE OFF PASSWORD                            
         DROP  R3,R4                                                            
*                                                                               
         GOTO1 RECUP,DMCB,AREC,ELEM,(R7)                                        
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         JNE   EXIT                                                             
*                                                                               
         TM    MKTFFLAG,MKTFHERE   RECORD EXISTS PREVIOUSLY?                    
         BZ    SP595                - NOPE, DO ADDREC INSTEAD OF PUTREC         
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,SPTFILE,KEY+14,AREC,DMWORK                   
         J     EXIT                                                             
*                                                                               
SP595    GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
*                                                                               
         DROP  R5,R6                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
SPTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         LLC   RE,TRACECDE                                                      
         BCTR  RE,0                                                             
         MHI   RE,24                                                            
         L     RF,=A(TRACEHD)                                                   
         AR    RF,RE                                                            
         MVC   P(24),0(RF)                                                      
         GOTO1 HEXOUT,DMCB,KEY,P+30,18,=C'MIX',0                                
         CLI   MOVECODE,C'N'       IF RECORD NOT MOVED                          
         BNE   *+10                                                             
         MVC   P+75(30),=C'RECORD NOT MOVED TO NEW MARKET'                      
SPTRACE2 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         J     EXIT                                                             
*                                                                               
         TITLE 'SPSX02 - MARKET FIX PROGRAM - APPLICATION - CLTCHK'             
***********************************************************************         
*                                                                     *         
*    CHECK IF CLIENT HAS CLT SPECIFIC CBLMKT OR STA RECS              *         
*                                                                     *         
*NTRY     HALF = BINARY CLIENT TO LOOK UP IN BINSRCH TABLE            *         
*                                                                     *         
*EXIT     CC NE  DO NOT PROCESS THIS ESTIMATE                         *         
*                                                                     *         
***********************************************************************         
CLTCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 BINSRCH,CLTBPARS,HALF                                            
         CLI   0(R1),1             CLT ENTRY FOUND?                             
         BE    CLTCHKNE            NO                                           
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)          CLT ENTRY FOUND?                             
         BZ    CLTCHKNE            NO                                           
         USING CLTBUFFD,R4                                                      
         TM    CBFLAG,CBCBLMKT     CLT SPECIFIC CBLMKT FOUND?                   
         BNZ   CLTCHKNE            YES                                          
         TM    CBFLAG,CBCLTSTA     CLT SPECIFIC MASTER RECORD?                  
         BZ    CLTCHKOK            NO, OK TO PROCESS THIS CLIENT                
*&&DO                                                                           
         CLI   QOPT3,C'Y'                                                       
         BNE   CLTCHKNE                                                         
         CLC   QMKT,CBMKT          SAME MARKET?                                 
         BE    CLTCHKOK                                                         
*&&                                                                             
         DROP  R4                                                               
*                                                                               
CLTCHKNE LTR   RB,RB                                                            
         B     *+6                                                              
CLTCHKOK CR    RB,RB               SET EQ CC                                    
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPSX02 - MARKET FIX PROGRAM - APPLICATION - ESTCHK'             
***********************************************************************         
*                                                                     *         
*    CHECK IF ESTIMATE NUMBER IS IN PW TABLE                          *         
*                                                                     *         
*NTRY     P1 ==> XL1 - ESTIMATE NUMBER TO BE CHECKED                  *         
*                                                                     *         
*EXIT     CC NE  ESTIMATE IS IN TABLE                                 *         
*                                                                     *         
***********************************************************************         
ESTCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   MOVECODE,0          INIT RECORD MOVE CODE                        
         L     RF,0(R1)                                                         
         MVC   FULL(2),0(RF)       CLIENT                                       
         L     RF,4(R1)                                                         
         MVC   FULL+2(1),0(RF)     ESTIMATE                                     
*                                                                               
         GOTO1 BINSRCH,CLTBPARS,FULL                                            
         CLI   0(R1),1             CLT ENTRY FOUND?                             
         BE    ESTCHKOK            NO                                           
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)          CLT ENTRY FOUND?                             
         BZ    ESTCHKOK            NO                                           
         USING CLTBUFFD,R4                                                      
         TM    CBFLAG,CBPW         PW CLIENT?                                   
         BZ    ESTCHKOK            NO                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 BINSRCH,PWBPARS,FULL                                             
         CLI   0(R1),1             PW ENTRY FOUND?                              
         BE    ESTCHKOK            NO                                           
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)          PW ENTRY FOUND?                              
         BNZ   ESTCHKNE            YES - EST IN PW TABLE                        
*                                                                               
ESTCHKOK DS    0H                  ESTIMATE NOT IN TABLE                        
         CR    RB,RB               SET EQ CC                                    
         B     ESTCHKX                                                          
*                                                                               
ESTCHKNE DS    0H                  ESTIMATE IN PW TABLE                         
         MVI   MOVECODE,C'N'       DO NOT MOVE RECORD TO NEW MARKET             
         LTR   RB,RB               SET NE CC                                    
*                                                                               
ESTCHKX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*&&DO                                                                           
*-------------------------------------*                                         
* SETMIN --   SETS UP MINIO BLOCK FOR *                                         
*             READING NWS DETAIL RECS *                                         
*-------------------------------------*                                         
*                                                                               
SETMIN   NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(MINBLK)                                                    
         USING MINBLKD,R3                                                       
         L     RE,=A(MINBLK)                                                    
         LA    RF,MINBLKL                                                       
         XCEF                                                                   
*                                                                               
         MVC   MINFIL,=CL8'SPTFIL'                                              
         MVC   MINDIR,=CL8'SPTDIR'                                              
         MVI   MINFKLEN,13                                                      
         MVI   MINNCTL,1                                                        
         MVC   MINFRCLM,=H'1976'  LENGTH OF EACH BUFFER                         
         MVI   MINEKLEN,L'BWDKEL                                                
         MVI   MINEKDSP,L'BWDKEY-L'BWDKEL                                       
         L     R1,=A(MRTAB)                                                     
         ST    R1,MINRTAB         ADDRESS OF MINIO RECORD TABLE                 
         LA    RE,L'MRTAB                                                       
         MH    RE,=Y(MRTABMAX)                                                  
         STH   RE,MINRTABL        LENGTH OF MINIO RECORD TABLE                  
         MVC   MINBUFF,=A(MIO1)   ADDRESS OF FIRST MINIO BUFFER                 
         MVI   MINNBUF,MINNBUFF   NUMBER OF MINIO BUFFERS                       
         MVC   MINCOMF,ACOMFACS   ADDRESS OF COMFACS                            
         MVC   MINRECUP,RECUP     ADDRESS OF RECUP                              
         L     RE,=A(MELEM)       ELEMENT AREA                                  
         LA    RF,L'MELEM                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=AL2(400) MAXIMUM LENGTH OF ELEMENT OF CLUSTER          
         MVI   MINNKLO,2          LOWEST ELEMENT IN RECORD                      
         MVI   MINNKHI,99         HIGHEST ELEMENT IN RECORD                     
         CLI   RCWRITE,C'N'        IS THIS AN UPDATE RUN                        
         BNE   *+8                  YES                                         
         MVI   MINWRITE,C'N'                                                    
         MVC   MINAGYC,AGY        AGENCY                                        
         MVI   MINAGYD,20         DISPLACEMENT TO AGENCY IN RECORD              
*                                                                               
         XC    MINMKEY,MINMKEY                                                  
         LA    R1,MINMKEY                                                       
         USING BWDRECD,R1         SET MASTER KEY                                
         MVI   BWDKTYP,BWDKTYPQ   X'0D'                                         
         MVI   BWDKSUB,BWDKSUBQ   X'68'                                         
         MVC   BWDKAGMD,BAGYMD    A/M                                           
         MVC   BWDKBYR,SAVEKEY2+3 BUYER FROM HEADER RECORD                      
         MVC   BWDKSEQ,SAVEKEY2+8 SEQUENCE NUMBER FROM HEADER RECORD            
         MVC   SMINMKEY,MINMKEY   SAVE MASTER KEY                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R1,R3                                                            
*&&                                                                             
*=====================================================================          
* READ STATION EQUIVALENCY RECORD AND SAVE SPECIAL MARKET IF APPLIES *          
*=====================================================================          
STEQ     NTR1  BASE=*,LABEL=*                                                   
         MVC   STEQSVRC,AREC                                                    
         MVC   STEQSVKY,KEY                                                     
*                                                                               
         MVC   DUB(1),3(R6)        R6 POINTS TO 70 MKTGRP ELEM                  
         PACK  DUB+1(3),4(5,R6)                                                 
*                                                                               
         XC    KEY,KEY             BUILD KEY OF MGREQ RECORD                    
         MVC   KEY(2),=X'0D44'                                                  
         MVC   KEY+2(3),STEQSVKY   A/M/CLT                                      
         MVC   KEY+5(5),QSTA                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    STEQ05                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+3(2),XFF        TRY FOR CLIENT ALL                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   STEQX                                                            
*                                                                               
STEQ05   L     R6,ADMKTGRP                                                      
         ST    R6,AREC                                                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         MVI   BYTE,03                                                          
         BRAS  RE,GETEL                                                         
         BNE   STEQ30                                                           
*                                                                               
STEQ10   CLC   2(3,R6),DUB         THIS THE RIGHT MARKET GROUP                  
         BE    STEQ20                                                           
         BRAS  RE,NEXTEL                                                        
         BE    STEQ10                                                           
         B     STEQ30                                                           
*                                                                               
STEQ20   MVC   STEQMKT,5(R6)       SAVE THIS MARKET                             
*                                                                               
STEQ30   DS    0H                                                               
         MVC   AREC,STEQSVRC       RESTORE AREC VALUE                           
         MVC   KEY,STEQSVKY                                                     
         GOTO1 GETBUY                                                           
*                                                                               
STEQX    MVC   KEY,STEQSVKY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
STEQSVRC DS    A                                                                
STEQSVKY DS    XL20                                                             
         LTORG                                                                  
*                                                                               
PASSNERR BASR  R1,RE                                                            
         DC    AL1(L'PASSNMSG-1)                                                
PASSNMSG DC    C'N-PASSIVE KEY ALREADY ON FILE FOR MARKET'                      
*                                                                               
PASSKERR BASR  R1,RE                                                            
         DC    AL1(L'PASSKMSG-1)                                                
PASSKMSG DC    C'K-PASSIVE KEY ALREADY ON FILE FOR MARKET'                      
*                                                                               
DEMSWERR BASR  R1,RE                                                            
         DC    AL1(L'DEMSWMSG-1)                                                
DEMSWMSG DC    C'DEMO SWEEP SPILL FOR NEW MKT DELETED FOR BUY BELOW'            
*                                                                               
NWSERR   BASR  R1,RE                                                            
         DC    AL1(L'NWSMSG-1)                                                  
NWSMSG   DC    C'NO STATION IN HEADER FOR NWS REC, KEY='                        
*                                                                               
NWSERR2  BASR  R1,RE                                                            
         DC    AL1(L'NWSMSG2-1)                                                 
NWSMSG2  DC    C'NWS STA ALREADY IN NEW MKT-NO SWITCH'                          
*                                                                               
MANYERR  BASR  R1,RE                                                            
         DC    AL1(MANYMSGX-MANYMSG-1)                                          
MANYMSG  DC    CL25'ERROR - TOO MANY BUYS FOR WABC-TV '                         
         DC    CL31'IN THE COMBINED MARKETS FOR EST'                            
MANYMSGX EQU   *                                                                
*                                                                               
PWTAB    DC    (PWTABSIZ)XL(L'PWTABENT)'00'                                     
PWTABSIZ EQU   2000                                                             
*                                                                               
* EOJ OVERALL TOTALS                                                            
         DS    0D                                                               
ACCUMS   DS    0XL32               ACCUMULATORS                                 
*                                                                               
ACCUBUY  DC    PL4'0',PL4'0',CL24'BUY RECORDS'                                  
ACCUBUYQ EQU   (ACCUBUY-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUTIR  DC    PL4'0',PL4'0',CL24'TRAFFIC INSTR RECAP RECS'                     
ACCUTIRQ EQU   (ACCUTIR-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUTBY  DC    PL4'0',PL4'0',CL24'TRAFFIC BUY RECS'                             
ACCUTBYQ EQU   (ACCUTBY-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUTSP  DC    PL4'0',PL4'0',CL24'TRAFFIC SHIP RECAP RECS'                      
ACCUTSPQ EQU   (ACCUTSP-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUTBA  DC    PL4'0',PL4'0',CL24'TRAFFIC BUY ACT RECS'                         
ACCUTBAQ EQU   (ACCUTBA-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUSBL  DC    PL4'0',PL4'0',CL24'STATION BILLING BUCKETS '                     
ACCUSBLQ EQU   (ACCUSBL-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUSIR  DC    PL4'0',PL4'0',CL24'SIR (NSID && DETAIL) RECS'                    
ACCUSIRQ EQU   (ACCUSIR-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUSTU  DC    PL4'0',PL4'0',CL24'STATUS RECORDS'                               
ACCUSTUQ EQU   (ACCUSTU-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUNCA  DC    PL4'0',PL4'0',CL24'NWS CAMPAIGN RECORDS'                         
ACCUNCAQ EQU   (ACCUNCA-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUNDT  DC    PL4'0',PL4'0',CL24'NWS DETAIL RECORDS'                           
ACCUNDTQ EQU   (ACCUNDT-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUSLH  DC    PL4'0',PL4'0',CL24'STATION LOCKIN HDR'                           
ACCUSLHQ EQU   (ACCUSLH-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUSLX  DC    PL4'0',PL4'0',CL24'STATION LOCKIN XHDR'                          
ACCUSLXQ EQU   (ACCUSLX-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUCLS  DC    PL4'0',PL4'0',CL24'CLEARED STATUS    '                           
ACCUCLSQ EQU   (ACCUCLS-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUMST  DC    PL4'0',PL4'0',CL24'MATCHING STATUS   '                           
ACCUMSTQ EQU   (ACCUMST-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUPW   DC    PL4'0',PL4'0',CL24'PW RECORDS        '                           
ACCUPWQ  EQU   (ACCUPW-ACCUMS)/L'ACCUMS                                         
*                                                                               
ACCUBTC  DC    PL4'0',PL4'0',CL24'BATCH RECORDS     '                           
ACCUBTCQ EQU   (ACCUBTC-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUDOR  DC    PL4'0',PL4'0',CL24'DARE ORDER RECORDS '                          
ACCUDORQ EQU   (ACCUDOR-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUOHS  DC    PL4'0',PL4'0',CL24'ORDER HISTORY RECORDS'                        
ACCUOHSQ EQU   (ACCUOHS-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUCDP  DC    PL4'0',PL4'0',CL24'CAN-DTM PROGRAM RECORDS'                      
ACCUCDPQ EQU   (ACCUCDP-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUCDD  DC    PL4'0',PL4'0',CL24'CAN-DTM DEMO RECORDS'                         
ACCUCDDQ EQU   (ACCUCDD-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUCDO  DC    PL4'0',PL4'0',CL24'CAN-DTM ORDER RECORDS'                        
ACCUCDOQ EQU   (ACCUCDO-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUNIN  DC    PL4'0',PL4'0',CL24'NEW INVOICE RECORDS'                          
ACCUNINQ EQU   (ACCUNIN-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUUSR  DC    PL4'0',PL4'0',CL24'US SBTK REVISION RECORDS'                     
ACCUUSRQ EQU   (ACCUUSR-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUUSW  DC    PL4'0',PL4'0',CL24'US SBTK WORK RECORDS'                         
ACCUUSWQ EQU   (ACCUUSW-ACCUMS)/L'ACCUMS                                        
*                                                                               
ACCUMREQ DC    PL4'0',PL4'0',CL24'REQUESTS'                                     
NACCUMS  EQU   (*-ACCUMS)/L'ACCUMS                                              
                                                                                
*&&DO                                                                           
*                                                                               
* MINIO AREAS                                                                   
*                                                                               
MELEM    DS    CL400                                                            
MINNBUFF EQU   2 NUMBER OF MINIO BUFFERS                                        
MRTAB    DS    (MRTABMAX)XL(6+L'BWDKEL)                                         
MRTABMAX EQU   100                                                              
MINBLK   DS    XL(MINBLKL)                                                      
MIO1     DS    2000C                                                            
MIO2     DS    2000C                                                            
*&&                                                                             
*                                                                               
* KEEP MIDL1 & MIDL2 TOGETHER                                                   
*                                                                               
MIDL1    DC    CL110'CLIENT   PROD   EST    ELEM  BILL     BILLDATE   IC        
               NVNO      GROSS AMT         NET AMT     SPOTS   TAX AMT'         
MIDL2    DC    CL110'------   ----   ---    ----  PERIOD   --------   -C        
               ----      ---------         -------     -----   -------'         
*                                                                               
         DS    0D                                                               
SPBUFF   DS    4000C                                                            
*                                                                               
TRACEHD  DC    CL24'ACTIVE BUY POINTER      '    1                              
         DC    CL24'REC FOR THIS KEY DEL    '    2                              
         DC    CL24'MEDIA C POINTER         '    3                              
         DC    CL24'REAL KEY FROM CAN COMB  '    4                              
         DC    CL24'TRAFFIC INST RECAP RECS '    5                              
         DC    CL24'KEY ADDED FOR PRD (POLS)'    6                              
         DC    CL24'KEY-UPDATED STATION REC '    7                              
         DC    CL24'KEY OF ADDED BUY-NEW MKT'    8                              
         DC    CL24'TRAFFIC BUY RECORDS     '    9                              
         DC    CL24'TRAFFIC SHIPPING RECAPS '   10                              
         DC    CL24'TRAFFIC BUY ACTIVITY    '   11                              
         DC    CL24'0E01 STATION BILLING REC'   12                              
         DC    CL24'NWS - CAMPAIGN RECORD   '   13                              
         DC    CL24'NWS - EXISTING HDR KEY  '   14                              
         DC    CL24'NWS - CHANGED HEADER KEY'   15                              
         DC    CL24'NWS - NEW HEADER KEY    '   16                              
         DC    CL24'NWS - NEW PASSIVE HEADER'   17                              
         DC    CL24'NWS - OLD DETAIL KEY    '   18                              
         DC    CL24'NWS - NEW DETAIL KEY    '   19                              
         DC    CL24'NWS - 1ST OLD DTL ELEM  '   20                              
         DC    CL24'NWS - 1ST NEW DTL ELEM  '   21                              
         DC    CL24'0D71 STATUS RECORD      '   22 - WAS 13                     
         DC    CL24'0D72 STATION LOCKIN HDR '   23                              
         DC    CL24'0D76 CLEARED STATUS     '   24                              
         DC    CL24'STAT FILE N KEY         '   25                              
         DC    CL24'STAT FILE N KEY-DELETED '   26                              
         DC    CL24'STAT FILE N KEY-NEW     '   27                              
         DC    CL24'XSP MATCH STATUS OLD KEY'   28                              
         DC    CL24'XSP MATCH STATUS NEW KEY'   29                              
         DC    CL24'XSP MATCH STATUS DEL KEY'   30                              
         DC    CL24'XSP MATCH STATUS MRG KEY'   31                              
         DC    CL24'PW RECORD KEY'              32                              
         DC    CL24'0D73 STATION LOCKIN HDRX'   33                              
         DC    CL24'0D73 STATION LOCKIN HDR1'   34                              
         DC    CL24'0D73 STATION LOCKIN HDR2'   35                              
         DC    CL24'CBLDEF RECORD UPDATED   '   36                              
         DC    CL24'DARE BATCH REC UPDATED  '   37                              
         DC    CL24'DEL FF PTR IN ORIG MED  '   38                              
         DC    CL24'DARE ORDER REC UPDATED  '   39                              
         DC    CL24'DARE ORDER OLD PASSV KEY'   40                              
         DC    CL24'DARE ORDER NEW PASSV KEY'   41                              
         DC    CL24'ORDER HIST. REC UPDATED '   42                              
         DC    CL24'STAT FILE K KEY         '   43                              
         DC    CL24'STAT FILE K KEY-DELETED '   44                              
         DC    CL24'STAT FILE K KEY-NEW     '   45                              
         DC    CL24'DTM PROGRAM REC UPDATED '   46                              
         DC    CL24'DTM DEMO REC UPDATED    '   47                              
         DC    CL24'DTM ORDER REC UPDATED   '   48                              
***      DC    CL24'DTM GOAL REC UPDATED    '                                   
         DC    CL24'NEW INVOICE REC UPDATED '   49                              
         DC    CL24'US-SBTK REV REC UPDATED '   50                              
         DC    CL24'US-SBTK WORK REC UPDATED'   51                              
         DROP  R7,RB                                                            
*                                                                               
*  THE CM VERSION OF THIS CODE FOR CANADIAN CABLE STATIONS (PWES/TO)            
*  THIS ROUTINE WILL CHANGE THE CBLMKT RECORDS                                  
*  IF IT IS AN 'ALL' CLIENT REQUEST THEN THE CLIENT SPECIFIC RECORD(S)          
*  WILL ALSO BE CHANGED IF THE SUFFIX IS IN THE SAME ORIGINAL MARKET            
*  WHEN CLIENT SPECIFIC RECORD CHANGED, ALL ITS ESTIMATE SPECIFIC ALSO          
*  CHANGED TO KEEP MARKETS IN SYNC (MKTS IN CLI & EST ALWAYS SAME)              
*                                                                               
UPDCBLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY              ESTABLISH STATION KEY                        
         USING NDEFKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   NDEFKTYP,=XL2'0D11'                                              
         MVC   NDEFKAGY,QAGY                                                    
         MVC   NDEFKNET,QSTA       SET CALL LETTERS                             
         CLC   QCLT,=C'ALL'        AGENCY LEVEL IS START FOR ALL CLI            
         BE    *+10                                                             
         MVC   NDEFKCLT,BCLT       ELSE CLIENT SPECIFIC                         
*                                                                               
         GOTO1 HIGH                READ FOR BASE RECORD                         
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
         CLC   KEY(L'NDEFKEY),KEYSAVE                                           
         BNE   UPCBER1             BASE RECORD MUST EXIST                       
         B     UPCB10                                                           
*                                                                               
UPCBSEQ  GOTO1 SEQ                                                              
UPCB5    TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         CLC   KEY(8),KEYSAVE      STILL DESIRED AGY/STA?                       
         BNE   UPCBX               DONE                                         
         CLC   QCLT,=C'ALL'        IF DOING ALL CLIENTS                         
         BE    UPCB10              SKIP CLIENT CHECK                            
         CLC   KEY+8(2),BCLT                                                    
         BNE   UPCBX               DONE                                         
*                                                                               
UPCB10   MVC   AREC,=A(SPBUFF)     I/O BUFFER                                   
         L     R6,AREC                                                          
         GOTO1 GET                                                              
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         MVI   BYTE,NDEFELQ        X'01'                                        
         BRAS  RE,GETEL                                                         
         BE    *+10                                                             
         DC    H'0'                                                             
UPCB20   BRAS  RE,NEXTEL                                                        
         CLI   0(R6),NDEFELQ                                                    
         BNE   UPCB30                                                           
         CLC   QOPT2(2),2(R6)      FIND SUFFIX CODE MATCH                       
         BNE   UPCB20                                                           
         CLC   OLDMKT,4(R6)        IF OLD MKT IS A MATCH, CHANGE MKT            
         BNE   UPCB30                                                           
*                                                                               
* SUFFIX/MARKET MATCH FOUND - CHANGE MARKET                                     
*                                                                               
         MVC   4(2,R6),NEWMKT                                                   
         MVC   12(3,R6),MKTALPH    UPDATE DLNK FIELD ON CBLMKT REC              
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   UPCB25                                                           
         GOTO1 PUT                                                              
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
UPCB25   CLI   QOPT5,C'Y'          TRACE REQ                                    
         JNE   UPCB27                                                           
         MVI   TRACECDE,36                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
         LA    R4,=CL20'CBLDEF RECORD:'                                         
         L     R2,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,13(R2)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R2),C'DUMP',(R5),=C'0D'               
UPCB27   B     UPCBSEQ             GO BACK FOR CLIENT/EST SPECIFIC              
*                                                                               
* SUFFIX/MARKET MATCH NOT FOUND - POSSIBLE ERROR                                
UPCB30   CLI   KEY+10,X'00'        EST REC - ALWAYS ERROR                       
         BE    UPCB35                                                           
         MVC   SAVEPRT(132),P      EST MKTS MUST MATCH PARENT CLI REC           
         MVC   P,SPACES                                                         
         MVC   P+10(30),=CL30'ERR - MARKET NOT IN EST LEVEL'                    
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
         B     UPCBSEQ             GO BACK FOR NEXT CLIENT/EST SPECIFIC         
*                                                                               
UPCB35   CLC   QCLT,=C'ALL'                                                     
         BNE   UPCBER2             IF CLIENT SPECIFIC THEN ERROR                
         OC    KEY+8(2),KEY+8      ALL REQUEST - ERROR ON AGY REC               
         BZ    UPCBER2             (ELSE MERELY NOT INTERESTED IN CLI)          
         MVC   KEY+10(3),XFF       IGNORE THIS CLI - SKIP ITS EST RECS          
         GOTO1 HIGH                TRY FOR NEXT CLIENT REC                      
         B     UPCB5                                                            
*                                                                               
UPCBER1  MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         MVC   P+10(23),=CL23'ERR - NO CBLDEF RECORD'                           
         B     UPCBERX                                                          
UPCBER2  MVC   SAVEPRT(132),P                                                   
         MVC   P,SPACES                                                         
         MVC   P+10(30),=CL30'ERR - MARKET NOT DEFINED'                         
UPCBERX  GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
UPCBX    XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * *                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * *                           
BLDMKSTA NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,STAWORK                                                       
         XC    0(L'STAWORK,R2),0(R2)                                            
         USING STAPACKD,R2                                                      
*                                                                               
         MVI   STAPACT,C'P'        ASK FOR NETWORK FILTER                       
         MVC   STAPAGY,AGY                                                      
         MVC   STAPCTRY,COUNTRY                                                 
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET(2),QOPT2                                                
         GOTO1 VSTAPACK,(R2)                                                    
*                                                                               
         MVC   CBLMKSTA,STAPMKST                                                
         J     EXIT                                                             
*                                                                               
CBLMKSTA DS    XL5                                                              
STAWORK  DS    XL32                                                             
         DROP  R2                                                               
         LTORG                                                                  
*================================================================               
* THIS SECTION UPDATES THE MARKET NUMBER IN THE DARE BATCH RECORD               
*================================================================               
         USING DARBTCHD,R6                                                      
T900     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         XC    SAVEKEY3,SAVEKEY3   SAVE TYPE/AGMD/MKT/STA/CLT                   
         MVC   SAVEKEY3(2),=X'0D35'                                             
         MVC   SAVEKEY3+2(1),BAGYMD                                             
         MVC   SAVEKEY3+3(5),OLDMSTA                                            
         MVC   KEY(8),SAVEKEY3                                                  
*                                                                               
T910     GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
*                                                                               
T920     CLC   KEY(8),SAVEKEY3     TEST SAME TYPE/AGMD/MSTA                     
         BNE   T960                NO -- EXIT                                   
*                                                                               
         CLC   KEY+8(2),KEYSAVE    PROCESSED THIS CLIENT ALREADY?               
         BE    T925                YES, SKIP CLIENT CHECK                       
*                                                                               
         MVC   HALF,KEY+8          BINARY CLIENT                                
         BRAS  RE,CLTCHK           PROCESS THIS CLIENT?                         
         BE    T925                YES                                          
         MVC   KEY+10(3),XFF       FORCE NEXT CLIENT                            
         B     T910                                                             
*                                                                               
T925     GOTO1 =A(ESTCHK),DMCB,KEY+8,KEY+11 IS EST IN LIST?                     
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,37                                                      
         BRAS  RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   MOVECODE,C'N'       REC BEING MOVED?                             
         BE    T950                NO, READ SEQ                                 
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
         L     R6,AREC                                                          
         MVC   SAVEKEY(13),KEY     SAVE OLD KEY                                 
*                                                                               
         MVC   KEY+3(2),NEWMKT     MOVE NEW MARKET INTO KEY                     
         NI    15(R6),X'7F'        UNDELETE THE NEW RECORD                      
         OI    DMINBTS,X'08'       PASS THE DELETED RECORDS                     
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET PASS DELETED RECORDS                   
         OI    DMOUTBTS,X'02'                                                   
         CLC   KEY(13),KEYSAVE     DOES REC WITH NEW MKT ALREADY EXIST?         
         BNE   T938                NO                                           
*                                                                               
         GOTO1 GET                                                              
         BAS   RE,PRTREC           PRINT THE RECORD                             
         B     T950                AND READ SEQ                                 
*                                                                               
T938     LA    RF,ACCUBTCQ         INCRIMENT # OF DARE BATCH RECS MOVED         
         BRAS  RE,BUMPACC                                                       
***                                                                             
* DELETE OLD RECORD                                                             
***                                                                             
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         MVC   SAVEKEY(13),KEYSAVE NEW KEY IN SAVEKEY NOW                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     FOUND ORIGINAL KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         GOTO1 GET                                                              
         OI    15(R6),X'80'        MARK OLD RECORD FOR DELETION                 
         CLI   RCWRITE,C'Y'        TEST WRITE=NO OPTION                         
         BNE   T939                                                             
         GOTO1 PUT                 DELETE OLD REC                               
***                                                                             
* ADD NEW KEY AND RECORD                                                        
***                                                                             
T939     MVC   DBTKMKT,NEWMKT      MOVE NEW MARKET INTO RECORD                  
         NI    15(R6),X'FF'-X'80'  NEW REC NOT DELETED!                         
         MVC   KEY(13),SAVEKEY     MOVE NEW KEY IN (WITH NEW MARKET)            
         MVC   SAVEKEY(13),KEYSAVE OLD KEY IN SAVEKEY NOW                       
         CLI   RCWRITE,C'Y'        WRITE=Y OPTION?                              
         BNE   T940                NO, DO NOT UPDATE FILES!                     
         GOTO1 ADD                 ADD NEW KEY AND RECORD TO SPOT FILE          
***                                                                             
* DELETE OLD KEY                                                                
***                                                                             
T940     MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                GET BACK OLD (OBSOLETE) KEY                  
         CLC   KEY(13),KEYSAVE     EXISTS?                                      
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         BNE   T950                                                             
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T950     GOTO1 SEQ                 NEXT RECORD                                  
         B     T920                                                             
*                                                                               
T960     J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* PRINT THE NEW AND OLD DARE BATCH RECORDS SINCE THEY MATCH!                    
*===========================================================                    
PRTREC   NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   SAVEPRT,SPACES                                                   
         MVI   SAVEPRT,C'*'                                                     
         MVC   SAVEPRT+1(78),SAVEPRT                                            
*                                                                               
         LA    R1,SAVEPRT+6                                                     
         LA    R2,5                                                             
*                                                                               
PREC10   MVC   0(11,R1),=C' ATTENTION '                                         
         LA    R1,14(R1)                                                        
         BCT   R2,PREC10                                                        
*                                                                               
         MVC   P,SAVEPRT                                                        
         MVC   P2+17(38),=C'BATCH RECORD ALREADY EXISTS FOR MARKET'             
         EDIT  (B2,NEWMKT),(4,P2+56),FILL=0                                     
         MVC   P3+19(39),=C'NEW BATCH REC DOES NOT NEED TO BE ADDED'            
         MVC   P4,SAVEPRT                                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         L     R6,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'DARE BATCH',(R6),C'DUMP',(R0),=C'1D'          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SAVEPRT                                                        
         MVC   P2+17(40),=C'WILL NOT DELETE OLD BATCH REC FOR MARKET'           
         EDIT  (B2,OLDMKT),(4,P2+58),FILL=0                                     
         MVC   P3+19(40),=C'CONTACT DDS IF YOU WANT THIS REC DELETED'           
         MVC   P4,SAVEPRT                                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'DARE BATCH',(R6),C'DUMP',(R0),=C'1D'          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* MOVE PW STATION RECORDS TO NEW MARKET IF CONVERTED TO COS2                    
*===========================================================                    
FIXPW    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
*                                                                               
FIXPW2   GOTO1 HIGH                FIRST RECORD FOR A PRODUCT                   
         CLC   KEY(3),KEYSAVE      TEST SAME TYPE/AGMD                          
         JNE   EXIT                NO -- EXIT                                   
*                                                                               
FIXPW3   MVC   KEY+7(5),OLDMSTA    MOVE MKT/STA AFTER PRD/EST                   
         MVI   KEY+12,0                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      TEST SAME TYPE/AGMD                          
         JNE   EXIT                NO -- EXIT                                   
*                                                                               
         CLC   KEY+3(2),KEYSAVE+3  HAVE SAME CLT?                               
         BNE   FIXPW3              NO, GET MKT/STA FOR THIS CLIENT              
*                                                                               
         CLC   KEY(12),KEYSAVE     RECORD EXIST FOR THIS PRD/EST                
         BNE   FIXPW20             NO                                           
*                                                                               
         LA    R1,KEY+3            POINT TO CLIENT                              
         ST    R1,DMCB                                                          
         LA    R1,KEY+6            POINT TO ESTIMATE                            
         ST    R1,DMCB+4                                                        
         LA    R1,DMCB             POINT R1 TO A(ESTIMATE) YSFI                 
         BRAS  RE,ESTCHK                                                        
         BNE   FIXPW20             EST IN TABLE SO SKIP IT                      
*                                                                               
         GOTO1 GETBUY              GTFR                                         
*                                                                               
* DELETE THE RECORD AND KEY                                                     
*                                                                               
         LA    RF,ACCUPWQ                                                       
         MH    RF,=Y(L'ACCUMS)                                                  
         A     RF,=A(ACCUMS)                                                    
         AP    0(4,RF),=P'1'                                                    
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   FIXPW10                                                          
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,ADBUY,DMWORK                  
         TM    DM3,X'FD'                                                        
         BZ    FIXPW10                                                          
         DC    H'0'                                                             
*                                                                               
FIXPW10  L     RE,ADBUY                                                         
         ST    RE,AREC             SET FOR ADD ROUTINE                          
         NI    15(RE),X'7F'        UNDELETE THE RECORD                          
         MVC   7(2,RE),NEWMKT      SET NEW MARKET                               
         GOTO1 ADD                 ADD THE NEW RECORD AND KEY                   
*                                                                               
FIXPW20  MVC   KEY+7(6),XFF        GET NEXT ESTIMATE                            
         B     FIXPW2                                                           
*                                                                               
*=================================================================              
* THIS SECTION UPDATES THE MARKET NUMBER IN THE DARE ORDER RECORDS              
*=================================================================              
T1000    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
KY       USING DAREORDD,KEY                                                     
         MVI   KY.DCKTYPE,DCKTYPQ     PP BY CLIENT                              
         MVI   KY.DCKSUBTY,DCKSTYPQ                                             
         MVC   KY.DCKAGMD,BAGYMD                                                
*                                                                               
T1010    XC    SAVEKEY3,SAVEKEY3                                                
         MVC   SAVEKEY3(L'DOKEY),KEY    SAVE KEY                                
         GOTO1 HIGH                                                             
*                                                                               
T1020    CLC   KEY(DCKCLT-DOKEY),SAVEKEY3   TEST SAME TYPE/AGMD                 
         BNE   T1900               NO -- EXIT                                   
*                                                                               
         CLC   KY.DCKCLT,SAVEKEY3+3   ALREADY CHECKED THIS CLIENT?              
         BE    T1030                  YES                                       
         MVC   HALF,KY.DCKCLT         BINARY CLIENT                             
         BRAS  RE,CLTCHK              OK TO PROCESS THIS CLIENT?                
         BE    T1030                  YES                                       
         MVC   KY.DCKPRD(8),XFF       FORCE NEXT CLIENT                         
         B     T1010                  READ HIGH                                 
*                                                                               
T1030    GOTO1 =A(ESTCHK),DMCB,KY.DCKCLT,KY.DCKEST                              
         CLI   MOVECODE,C'N'       REC BEING MOVED?                             
         BE    T1050               NO - GET NEXT ESTIMATE                       
*                                                                               
         CLC   OLDSTA,KY.DCKSTA    SAME STATION AS REQUESTED?                   
         BE    T1100               IF SAME - PROCESS THE RECORD                 
*                                                                               
         BL    T1050               IF PAST THAT STATION - GET NEXT EST          
         MVC   KY.DCKSTA,OLDSTA    IF NOT THERE YET - HIGH FOR THAT STA         
         B     T1010               READ HIGH                                    
*                                                                               
T1050    DS    0H                  GET NEXT ESTIMATE HERE                       
         MVC   KY.DCKSTA,XFF       "FF" THE STATION TO GET NEXT EST             
         B     T1010               READ HIGH                                    
*                                                                               
T1100    DS    0H                  GOT ESTIMATE AND STATION HERE                
         GOTO1 GET                 GET THE RECORD                               
         L     R6,AREC                                                          
         MVI   BYTE,DOSPELQ        X'03', SUPPLEMENTARY ID ELEMENT              
         BRAS  RE,GETEL                                                         
         BNE   T1800               NOT THERE - GO TO NEXT RECORD: SEQ           
*                                                                               
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPTLNQ    ELEM INCLUDES MKT?                           
         BL    T1800               NO - GET NEXT RECORD: SEQ                    
*                                                                               
         CLC   DOSPMKT,OLDMKT      SAME MARKET?                                 
         BNE   T1800               NO - GET NEXT RECORD: SEQ                    
*                                                                               
         MVC   DOSPMKT,NEWMKT      UPDATE MARKET CODE                           
         DROP  R6                                                               
*                                                                               
         LA    RF,ACCUDORQ                                                      
         BRAS  RE,BUMPACC                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   T1110                                                            
         MVC   SAVEKEY3,KEY                                                     
         L     R6,AREC                                                          
         MVC   KEY(L'SAVEKEY3),0(R6)                                            
         MVI   TRACECDE,39                                                      
         BRAS  RE,SPTRACE                                                       
         MVC   KEY(L'SAVEKEY3),SAVEKEY3                                         
*                                                                               
T1110    CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   T1150               NO - BYPASS THE PUTREC                       
         GOTO1 PUT                 WRITE RECORD TO DISK                         
*                                                                               
* READ THE DARE ID ELEMENT TO OBTAIN THE BUYER CODE                             
T1150    L     R6,AREC                                                          
         MVI   BYTE,DOIDELQ        X'01', DARE ID ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   T1800               NOT THERE - GO TO NEXT RECORD: SEQ           
*                                                                               
         USING DOIDELD,R6                                                       
         GOTO1 VRCPACK,DMCB,(C'P',DOIDBYR),(X'80',SVBUYER)                      
         BNE   T1800               BUYER ID DOESN'T PACK - SKIP P.P.            
         DROP  R6                                                               
*                                                                               
* TAKE CARE OF THE THREE PASSIVE KEYS HERE                                      
         XC    SAVEKEY3,SAVEKEY3                                                
         MVC   SAVEKEY3(L'DOKEY),KEY SAVE ORIGINAL PASSIVE KEY                  
*                                                                               
* PASSIVE POINTER BY CLT/PRD/MKT                                                
         XC    KEY,KEY                                                          
*                                                                               
         MVI   KY.DCMKTYPE,DCMKTYPQ                                             
         MVI   KY.DCMKSTYP,DCMKSTYQ                                             
*                                                                               
         LA    R6,SAVEKEY3                                                      
         USING DAREORDD,R6                                                      
         MVC   KY.DCMKAGMD(DCKSTA-DOKEY),DCKAGMD  A/M,CLT,PRD,EST               
         DROP  R6                                                               
*                                                                               
         MVC   KY.DCMKMKT,OLDMKT                                                
*                                                                               
         L     R6,AREC                                                          
         USING DAREORDD,R6                                                      
         MVC   KY.DCMKORDR,DOKORDER      ORDER NUMBER ('FF' COMPLIMENT)         
         DROP  R6                                                               
*                                                                               
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2(L'DOKEY),KEY                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),SAVEKEY2    PASSIVE KEY PRESENT?                    
         BNE   T1200               NO - DON'T ADD IT                            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,40         OLD PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1120                                                            
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T1120    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         MVC   KY.DCMKMKT,NEWMKT   REPLACE MARKET CODE                          
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,41         NEW PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1200                                                            
*                                                                               
         GOTO1 ADDIR              ADD THE PASSIVE POINTER                       
*                                                                               
* PASSIVE POINTER BY BYUER/CLT/MKT                                              
T1200    XC    KEY,KEY                                                          
         L     R6,AREC                                                          
         USING DAREORDD,R6                                                      
*                                                                               
         MVI   KY.DBCKTYPE,DBCKTYPQ                                             
         MVI   KY.DBCKSTYP,DBCKSTYQ                                             
         MVC   KY.DBCKAGMD,DOKAGMD            A/M                               
         MVC   KY.DBCKBYR,SVBUYER                                               
         MVC   KY.DBCKCLT,SAVEKEY3+3                                            
         MVC   KY.DBCKMKT,OLDMKT                                                
         MVC   KY.DBCKORDR,DOKORDER  ORDER NUMBER ('FF' COMPLIMENT)             
         DROP  R6                                                               
*                                                                               
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2(L'DOKEY),KEY                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),SAVEKEY2                                            
         BNE   T1300                                                            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,40         OLD PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1220                                                            
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T1220    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         MVC   KY.DBCKMKT,NEWMKT     REPLACE MARKET CODE                        
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,41         NEW PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1300                                                            
         GOTO1 ADDIR                                                            
*                                                                               
* PASSIVE POINTER BY BYUER/MKT/CLT                                              
T1300    XC    KEY,KEY                                                          
         L     R6,AREC                                                          
         USING DAREORDD,R6                                                      
*                                                                               
         MVI   KY.DBMKTYPE,DBMKTYPQ                                             
         MVI   KY.DBMKSTYP,DBMKSTYQ                                             
         MVC   KY.DBMKAGMD,DOKAGMD A/M                                          
         MVC   KY.DBMKBYR,SVBUYER                                               
         MVC   KY.DBMKMKT,OLDMKT                                                
         MVC   KY.DBMKCLT,SAVEKEY3+3                                            
         MVC   KY.DBMKORDR,DOKORDER  ORDER NUMBER ('FF' COMPLIMENT)             
         DROP  R6                                                               
*                                                                               
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2(L'DOKEY),KEY                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),SAVEKEY2                                            
         BNE   T1400                                                            
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,40         OLD PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1320                                                            
         OI    KEY+13,X'80'        MARK OLD KEY FOR DELETION                    
         GOTO1 WRITE               DELETE OLD KEY                               
*                                                                               
T1320    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         MVC   KY.DBMKMKT(2),NEWMKT                                             
*                                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,41         NEW PASSIVE KEY                              
         BRAS  RE,SPTRACE                                                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   T1400                                                            
         GOTO1 ADDIR                                                            
*                                                                               
T1400    XC    KEY,KEY             RESTORE ORIGINAL PASSIVE KEY                 
         MVC   KEY(L'DOKEY),SAVEKEY3                                            
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
*                                                                               
T1800    XC    SAVEKEY3,SAVEKEY3                                                
         MVC   SAVEKEY3(L'DOKEY),KEY    SAVE KEY BEFORE SEQ                     
         GOTO1 SEQ                                                              
         B     T1020                                                            
*                                                                               
T1900    J     EXIT                                                             
         LTORG                                                                  
         DROP  KY                                                               
*                                                                               
* THIS SECTION UPDATES THE DTM PROGRAM RECORDS X'0D05'                          
*                                                                               
DTMPRG   NTR1  BASE=*,LABEL=*                                                   
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = XKEY                                    
         USING DPRRECD,R2          DTM PROGRAM RECORD DSECT                     
*                                                                               
         MVI   DPRKTYPE,DPRKTYPQ         X'0D'                                  
         MVI   DPRKSTYP,DPRKSTYQ         X'05'                                  
         MVC   DPRKAGMD,BAGYMD     A/M                                          
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   *+10                NO - USE BAGYMD                              
         MVC   DPRKAGMD,AGYMDT     YES - PROCESS T AND N                        
*                                                                               
DPRG10   MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     DPRG30              TEST KEY                                     
*                                                                               
DPRG20   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
DPRG30   CLC   XKEY(18),SAVEKEYX   X'0D05' RECORD?                              
         BNE   DPRGX               NO - EXIT                                    
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BE    DPRG31              YES - TEST MEDIA T/N                         
         CLC   DPRKAGMD,BAGYMD     REQUESTED A/M?                               
         BE    DPRG35              YES                                          
         B     DPRGX               NO - EXIT                                    
*                                                                               
DPRG31   CLC   DPRKAGMD,AGYMDT     MEDIA T?                                     
         BE    DPRG35              YES                                          
         CLC   DPRKAGMD,AGYMDN     MEDIA N?                                     
         BE    DPRG35              YES                                          
         BH    DPRGX               NO - HIGHER - DONE                           
         MVC   DPRKAGMD,AGYMDN     BUMP KEY TO MEDIA N                          
         XC    DPRKCLT(13),DPRKCLT CLEAR EVERYTHING AFTER MEDIA                 
         B     DPRG10              GO READ HIGH FOR MEDIA N                     
*                                                                               
DPRG35   MVC   HALF,DPRKCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    *+14                YES                                          
         MVC   DPRKCAM(11),XFF     NO - FORCE NEXT CLIENT                       
         B     DPRG10              READ HIGH                                    
*                                                                               
         CLC   DPRKAGMD,AGYMDN     MEDIA N?                                     
         BE    DPRG41              YES - CHECK THE X'68' ELEMENT                
         CLC   DPRKMKT(5),OLDMKT       TEST SAME MKT/STA                        
         BE    DPRG40              YES                                          
         MVC   DPRKMKT(7),XFF      IF PAST REQUESTED MKT/STA THEN               
         BH    DPRG10              BUMP TO NEXT CAMPAIGN NUMBER                 
         MVC   DPRKMKT(5),OLDMKT       OTHERWISE BUMP TO REQ MKT/STA            
         XC    DPRKLIN,DPRKLIN         CLEAR EVERYTHING AFTER STA               
         B     DPRG10              AND READ HIGH ON THIS KEY                    
*                                                                               
DPRG40   CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,46         DTM PROGRAM RECORD                           
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
DPRG41   GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         CLC   DPRKAGMD,AGYMDN     MEDIA N?                                     
         BNE   DPRG45              NO - DON'T CHECK THE X'68' ELEMENT           
         LA    R1,OLDMSTAN         OLD MKT/STA                                  
         CLC   RCPROG,=C'CM'       CANADIAN CABLE REQ?                          
         BNE   DPRG41A             NO                                           
         CLC   QOPT2(2),=C'  '     SUFFIX?                                      
         BE    DPRG41A             NO                                           
         LAY   R1,CBLMKSTA         YES - USE THIS MKT/STA                       
*                                                                               
DPRG41A  L     R6,AREC             R6 = PROGRAM REC                             
         USING DPRNPELD,R6         NETWORK PRORATION ELEMENT DSECT              
         MVI   BYTE,DPRNPELQ       X'68' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'68' ELEMENT?                        
         B     *+8                 BRANCH OVER NEXTEL                           
DPRG42   BRAS  RE,NEXTEL           HAVE ANY MORE X'68' ELEMENTS?                
         BNE   DPRG20              NO - GO READ SEQ                             
         CLC   0(5,R1),DPRNPMKT     MATCH ON OLD MKT/STA?                       
         BNE   DPRG42              NO - GO CHECK FOR MORE X'68' ELEMS           
         MVC   DPRNPMKT,NEWMKT      YES - MOVE NEW MARKET IN                    
         DROP  R6                  DROP R6                                      
*                                                                               
         CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,46         DTM PROGRAM RECORD                           
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         LA    RF,ACCUCDPQ         DTM PROGRAM RECORD COUNTER                   
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DPRG20              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    DPRG20              NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
DPRG45   LA    RF,ACCUCDPQ         DTM PROGRAM RECORD COUNTER                   
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         L     R2,AREC             A(RECORD)                                    
         OI    34(R2),X'80'        MARK RECORD DELETED                          
         OI    XKEY+32,X'80'       MARK KEY DELETED                             
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DPRG50              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
*                                                                               
DPRG50   MVC   SAVEKEYX,XKEY       SAVE OFF THE KEY OLD                         
         MVC   DPRKMKT,NEWMKT      UPDATE MARKET IN RECORD                      
         NI    34(R2),X'7F'        UNDELETE RECORD                              
         LA    R2,XKEY             R2 = XKEY                                    
         MVC   DPRKMKT,NEWMKT      UPDATE MARKET IN KEY                         
         NI    XKEY+32,X'7F'       UNDELETE KEY                                 
*                                                                               
         XC    HALF,HALF           NO LINE NUMBER                               
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY             
*                                                                               
         CLC   XKEY(32),XKEYSAVE   RECORD ALREADY EXISTS?                       
         BE    DPRG52              YES, FIND NEXT AVAIL LINE#                   
         B     DPRG53              OTHERWISE, GO ADD IT                         
*                                                                               
DPRG51   GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=C'XSPDIR',XKEY,XKEY                 
*                                                                               
DPRG52   CLC   XKEY(30),XKEYSAVE   MATCH UP TO STATION?                         
         BNE   DPRG53              NO                                           
         MVC   HALF,DPRKLIN        YES - SAVE LAST LINE NUMBER                  
         B     DPRG51              READ SEQ                                     
*                                                                               
DPRG53   MVC   XKEY,XKEYSAVE       KEY WITH THE NEW MARKET                      
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,HALF           NEW LINE NUMBER?                             
         BZ    DPRG55              NO - LEAVE AS IS                             
         AHI   RE,1                BUMP TO NEXT AVAILABLE LINE NUMBER           
         L     R2,AREC             A(RECORD)                                    
         STCM  RE,3,DPRKLIN        UPDATE LINE NUMBER IN RECORD                 
         LA    R2,XKEY             R2 = KEY                                     
         STCM  RE,3,DPRKLIN        UPDATE LINE NUMBER IN KEY                    
*                                                                               
DPRG55   CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DPRG60              NO - BYPASS THE WRITE                        
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DATAMGR ERROR                          
*                                                                               
DPRG60   MVC   XKEY,SAVEKEYX       RESTORE KEY WITH OLD MARKET                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         B     DPRG20              READ SEQ                                     
*                                                                               
DPRGX    J     EXIT                EXIT                                         
         DROP  R2                  DROP R2                                      
         LTORG                                                                  
*                                                                               
* THIS SECTION UPDATES THE DTM DEMO RECORDS X'0D06'                             
*                                                                               
DTMDEM   NTR1  BASE=*,LABEL=*                                                   
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = XKEY                                    
         USING DDMRECD,R2          DTM DEMO RECORD DSECT                        
*                                                                               
         MVI   DDMKTYPE,DDMKTYPQ         X'0D'                                  
         MVI   DDMKSTYP,DDMKSTYQ         X'06'                                  
         MVC   DDMKAGMD,BAGYMD     A/M                                          
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   *+10                NO - USE BAGYMD                              
         MVC   DDMKAGMD,AGYMDT     YES - PROCESS T                              
*                                                                               
DDEM10   MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     DDEM30                                                           
*                                                                               
DDEM20   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
DDEM30   CLC   XKEY(14),SAVEKEYX   X'0D06' RECORD?                              
         BNE   DDEMX               NO - EXIT                                    
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BE    DDEM35              YES - TEST MEDIA T AND N                     
         CLC   DDMKAGMD,BAGYMD     REQUESTED A/M?                               
         BNE   DDEMX               NO                                           
*                                                                               
DDEM35   CLC   DDMKAGMD,AGYMDT     MEDIA T?                                     
         BE    *+14                YES                                          
         CLC   DDMKAGMD,AGYMDN     MEDIA N?                                     
         BNE   DDEMX               NO                                           
*                                                                               
         MVC   HALF,DDMKCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    *+14                YES                                          
         MVC   DDMKCAM(15),XFF     NO - FORCE NEXT CLIENT                       
         B     DDEM10              READ HIGH                                    
*                                                                               
         CLC   DDMKAGMD,AGYMDN     MEDIA N?                                     
         BNE   DDEM37              NO                                           
         CLC   DDMKMKT,OLDMKT      MATCH ON MKT?                                
         BNE   DDEM20              NO - READ SEQ                                
         LA    R1,OLDSTAN          OLD MEDIA N STATION                          
         CLC   RCPROG,=C'CM'       CANADIAN CABLE REQ?                          
         BNE   DDEM36              NO                                           
         CLC   QOPT2(2),=C'  '     SUFFIX PASSED?                               
         BE    DDEM36              NO                                           
         LAY   R1,CBLMKSTA+2       YES - USE NETWORK/SUFFIX                     
DDEM36   CLC   DDMKLCL,0(R1)       MATCH ON LOCAL STATION?                      
         BE    DDEM40              YES                                          
         B     DDEM20              NO - READ SEQ                                
*                                                                               
DDEM37   CLC   DDMKMKT(5),OLDMKT      TEST SAME MKT/STA                         
         BE    DDEM40              YES                                          
         MVC   DDMKMKT(11),XFF     IF PAST REQUESTED MKT/STA THEN               
         BH    DDEM10              BUMP TO NEXT CAMPAIGN NUMBER                 
         MVC   DDMKMKT(5),OLDMKT      OTHERWISE BUMP TO REQ MKT/STA             
         XC    DDMKLIN(6),DDMKLIN        CLEAR EVERYTHING AFTER STA             
         B     DDEM10              AND READ HIGH ON THIS KEY                    
*                                                                               
DDEM40   LA    RF,ACCUCDDQ         DTM DEMO RECORD COUNTER                      
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,47         DTM DEMO RECORD                              
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         CLC   DDMKAGMD,AGYMDN     MEDIA N?                                     
         BNE   DDEM45              NO - DON'T CHECK SPILL ELEMENT               
*                                                                               
         L     R6,AREC             R6 = A(RECORD)                               
         USING DDMDVELD,R6         SPILL DEMO VALUE ELEMENT DSECT               
         MVI   BYTE,DDMDVELQ       X'03' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'03' ELEMENT?                        
         B     *+8                 BRANCH OVER NEXTEL                           
DDEM42   BRAS  RE,NEXTEL           HAVE ANY MORE X'03' ELEMENTS?                
         BNE   DDEM20              NO - GO READ SEQ                             
         CLC   DDMDVMKT,OLDMKT     SPILL MARKET MATCHES OLD MARKET?             
         BNE   DDEM42              NO - READ NEXT X'03' ELEM                    
         MVC   DDMDVMKT,NEWMKT     UPDATE SPILL MARKET                          
         DROP  R6                  DROP SPILL DEMO ELEM USING                   
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DDEM20              NO - READ SEQ                                
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO - READ SEQ                                
         DC    H'0'                YES - DEATH                                  
*                                                                               
         OI    XKEY+32,X'80'       MARK KEY DELETED                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
         MVC   SAVEKEYX,XKEY       SAVE OFF THE KEY OLD                         
         MVC   DDMKMKT,NEWMKT      UPDATE MKT IN KEY AND ADD PASSV PTR          
         NI    XKEY+32,X'7F'       UNDELETE KEY                                 
         GOTO1 DATAMGR,DMCB,DMADD,=C'XSPDIR',XKEY,XKEY                          
         CLI   DM3,0               ANY ERRORS?                                  
         BE    DDEM60              NO                                           
         TM    DM3,X'20'           DUP KEY ON ADD?                              
         BNZ   DDEM60              YES - CAN'T CHANGE LINE NUMBER!              
         DC    H'0'                OTHER ERROR = DEATH                          
*                                                                               
DDEM45   L     R2,AREC             A(RECORD)                                    
         OI    34(R2),X'80'        MARK RECORD DELETED                          
         OI    XKEY+32,X'80'       MARK KEY DELETED                             
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DDEM50              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
*                                                                               
DDEM50   MVC   SAVEKEYX,XKEY       SAVE OFF THE KEY OLD                         
         MVC   DDMKMKT,NEWMKT      UPDATE MARKET IN RECORD                      
         NI    34(R2),X'7F'        UNDELETE RECORD                              
         LA    R2,XKEY             R2 = XKEY                                    
*                                                                               
         CLI   DDMKSPL,C'S'        SPILL POINTER FLAG?                          
         BE    DDEM60              YES - LET THE DUMP AND LOAD ADD THIS         
*                                                                               
         MVC   DDMKMKT,NEWMKT      UPDATE MARKET IN KEY                         
         NI    XKEY+32,X'7F'       UNDELETE KEY                                 
*                                                                               
         XC    HALF,HALF           NO LINE NUMBER                               
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY             
*                                                                               
         CLC   XKEY(32),XKEYSAVE   RECORD ALREADY EXISTS?                       
         BE    DDEM52               YES, FIND NEXT AVAIL LINE#                  
         B     DDEM53               OTHERWISE, GO ADD IT                        
*                                                                               
DDEM51   GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=C'XSPDIR',XKEY,XKEY                 
*                                                                               
DDEM52   CLC   XKEY(26),XKEYSAVE   SAME STATION?                                
         BNE   DDEM53              NO                                           
         CLC   DDMKLCL,XKEYSAVE+28     LOCAL STA MATCH?                         
         BNE   DDEM53              NO                                           
         CLI   DDMKSPL,C'S'        DID WE FIND SPILL?                           
         BE    DDEM51              YES - IGNORE IT                              
         MVC   HALF,DDMKLIN        YES - SAVE LAST LINE NUMBER                  
         B     DDEM51              READ SEQ                                     
*                                                                               
DDEM53   MVC   XKEY,XKEYSAVE       KEY WITH THE NEW MARKET                      
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,HALF           NEW LINE NUMBER?                             
         BZ    DDEM55              NO - LEAVE AS IS                             
         AHI   RE,1                BUMP TO NEXT AVAILABLE LINE NUMBER           
         L     R2,AREC             A(RECORD)                                    
         STCM  RE,3,DDMKLIN        UPDATE LINE NUMBER IN RECORD                 
         LA    R2,XKEY             R2 = KEY                                     
         STCM  RE,3,DDMKLIN        UPDATE LINE NUMBER IN KEY                    
*                                                                               
DDEM55   CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DDEM60              NO - BYPASS THE WRITE                        
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DATAMGR ERROR                          
*                                                                               
DDEM60   MVC   XKEY,SAVEKEYX       RESTORE KEY WITH OLD MARKET                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         B     DDEM20              READ SEQ                                     
*                                                                               
DDEMX    J     EXIT                EXIT                                         
         DROP  R2                  DROP DDMRECD USING                           
         LTORG                                                                  
*                                                                               
* THIS SECTION UPDATES THE DTM ORDER RECORDS X'0D07'                            
*                                                                               
DTMORD   NTR1  BASE=*,LABEL=*                                                   
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = XKEY                                    
         USING CORRECD,R2          DTM ORDER RECORD DSECT                       
*                                                                               
         MVI   CORKTYPE,CORKTYPQ         X'0D'                                  
         MVI   CORKSTYP,CORKSTYQ         X'07'                                  
         MVC   CORKAGMD,BAGYMD     A/M                                          
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   *+10                NO - USE BAGYMD                              
         MVC   CORKAGMD,AGYMDT     YES - PROCESS MEDIA T                        
*                                                                               
DORD10   MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     DORD30              TEST KEY                                     
*                                                                               
DORD20   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
DORD30   CLC   XKEY(20),SAVEKEYX   X'0D07' A/M?                                 
         BNE   DORDX               NO - EXIT                                    
*                                                                               
         MVC   HALF,CORKCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    *+14                YES                                          
         MVC   CORKCAM(10),XFF     NO - FORCE NEXT CLIENT                       
         B     DORD10              READ HIGH                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         L     R6,AREC             R6 = ORDER REC                               
         USING CORIDELD,R6         ORDER ID ELEMENT DSECT                       
         MVI   BYTE,CORIDELQ       X'02' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'02' ELEMENT?                        
         B     *+8                 BRANCH OVER NEXTEL                           
DORD40   BRAS  RE,NEXTEL           HAVE ANY MORE X'02' ELEMENTS?                
         BNE   DORD20              NO - GO READ SEQ                             
         CLC   OLDMSTA,CORIDMKT MATCH ON OLD MKT/STA?                           
         BNE   DORD40              NO - GO CHECK FOR MORE X'02' ELEMS           
         MVC   CORIDMKT,NEWMKT     YES - MOVE NEW MARKET IN                     
         DROP  R6                  DROP R6                                      
*                                                                               
         CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,46         DTM PROGRAM RECORD                           
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         LA    RF,ACCUCDOQ         DTM ORDER RECORD COUNTER                     
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DORD20              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    DORD20              NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
DORDX    J     EXIT                EXIT                                         
*&&DO                                                                           
*                                                                               
* THIS SECTION UPDATES THE DTM GOAL RECORDS X'0D08'                             
*                                                                               
DTMGOL   NTR1  BASE=*,LABEL=*                                                   
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = XKEY                                    
         USING DGLRECD,R2          DTM GOAL RECORD DSECT                        
*                                                                               
         MVI   DGLKTYPE,DGLKTYPQ   X'0D'                                        
         MVI   DGLKSTYP,DGLKSTYQ   X'08'                                        
         MVC   DGLKAGMD,BAGYMD     A/M                                          
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   *+10                NO - USE BAGYMD                              
         MVC   DGLKAGMD,AGYMDT     YES - PROCESS T/N/C                          
*                                                                               
DGOL10   MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     DGOL30                                                           
*                                                                               
DGOL20   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
DGOL30   CLC   XKEY(12),SAVEKEYX   X'0D08' RECORD?                              
         BNE   DGOLX               NO - EXIT                                    
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BE    DGOL31              YES - TEST MEDIA T/N/C                       
         CLC   DGLKAGMD,BAGYMD     REQUESTED A/M?                               
         BE    DGOL35              YES                                          
         B     DGOLX               NO - EXIT                                    
*                                                                               
DGOL31   CLC   DGLKAGMD,AGYMDT     MEDIA T?                                     
         BE    DGOL35              YES                                          
         CLC   DGLKAGMD,AGYMDN     MEDIA N?                                     
         BE    DGOL35              YES                                          
         BH    DGOL32              NO - HIGHER                                  
         MVC   DGLKAGMD,AGYMDN     BUMP KEY TO MEDIA N                          
         XC    DGLKCLT(17),DGLKCLT CLEAR EVERYTHING AFTER MEDIA                 
         B     DGOL10              GO READ HIGH FOR MEDIA N                     
*                                                                               
DGOL32   CLC   DGLKAGMD,BAGYMD     MEDIA C?                                     
         BE    DGOL35              YES                                          
         BH    DGOLX               NO - HIGHER SO EXIT                          
         MVC   DGLKAGMD,BAGYMD     BUMP KEY TO MEDIA C                          
         XC    DGLKCLT(17),DGLKCLT CLEAR EVERYTHING AFTER MEDIA                 
         B     DGOL10              GO READ HIGH FOR MEDIA N                     
*                                                                               
DGOL35   MVC   HALF,DGLKCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    *+14                YES                                          
         MVC   DGLKCAM(15),XFF     NO - FORCE NEXT CLIENT                       
         B     DGOL10              READ HIGH                                    
*                                                                               
         CLC   DGLMKT,OLDMKT       SAME MARKET?                                 
         BE    DGOL40              YES                                          
         MVC   DGLMKT(11),XFF      IF PAST REQUESTED MARKET THEN                
         BH    DGOL10              BUMP TO NEXT CAMPAIGN NUMBER                 
         MVC   DGLMKT,OLDMKT       OTHERWISE BUMP TO REQ MARKET                 
         XC    DGLDPT(9),DGLDPT    CLEAR EVERYTHING AFTER MARKET                
         B     DGOL10              AND READ HIGH ON THIS KEY                    
*                                                                               
DGOL40   LA    RF,ACCUDTMG         DTM GOAL RECORD COUNTER                      
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,48         DTM GOAL RECORD                              
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         L     R2,AREC             A(RECORD)                                    
         OI    34(R2),X'80'        MARK RECORD DELETED                          
         OI    XKEY+32,X'80'       MARK KEY DELETED                             
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DGOL50              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
*                                                                               
DGOL50   MVC   SAVEKEYX,XKEY       SAVE OFF THE KEY OLD                         
         MVC   DGLMKT,NEWMKT       UPDATE MARKET IN RECORD                      
         NI    34(R2),X'7F'        UNDELETE RECORD                              
         LA    R2,XKEY             R2 = XKEY                                    
         MVC   DGLMKT,NEWMKT       UPDATE MARKET IN KEY                         
         NI    XKEY+32,X'7F'       UNDELETE KEY                                 
*                                                                               
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY             
         CLC   XKEY(32),XKEYSAVE   DOES RECORD ALREADY EXIST                    
         BE    DGOLERR             YES - GIVE ERROR MESSAGE & READ SEQ          
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   DGOL60              NO - BYPASS THE WRITE                        
         MVC   XKEY,XKEYSAVE       KEY WITH THE NEW MARKET                      
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DATAMGR ERROR                          
*                                                                               
DGOL60   MVC   XKEY,SAVEKEYX       RESTORE KEY WITH OLD MARKET                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         B     DGOL20              READ SEQ                                     
*                                                                               
DGOLX    J     EXIT                EXIT                                         
*                                                                               
DGOLERR  MVC   P+10(4),=C'CLT='    CLT= IN HEADING                              
         GOTO1 CLUNPK,DMCB,DGLKCLT,P+14                                         
         MVC   P+32(36),=C'* ERROR * DTM GOAL EXISTS IN NEW MKT'                
         MVI   SPACING,2           2 SPACES                                     
         GOTO1 REPORT              PRINT THE ERROR MESSAGE                      
         MVI   TRACECDE,48         DTM GOAL RECORD                              
         BRAS  RE,XPTRAC           *** TRACE ***                                
         B     DGOL60              RESTORE READ SEQUENCE AND READ SEQ           
         DROP  R2                                                               
         LTORG                                                                  
*&&                                                                             
*                                                                               
* THIS SECTION UPDATES THE NEW INVOICE RECS X'0E03'/X'0E83'                     
*                                                                               
NINV     NTR1  BASE=*,LABEL=*                                                   
         MVC   DATADISP,=H'42'     DATADISP NOW 42                              
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = XKEY                                    
         USING SNVPKEY,R2          NINV PASSIVE KEY DSECT                       
*                                                                               
         MVI   SNVPTYP,SNVPTYPQ    X'0E'                                        
         MVI   SNVPSUB,SNVPSUBQ    X'83'                                        
         MVC   SNVPAM,BAGYMD       A/M                                          
*                                                                               
NINV10   MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     NINV30                                                           
*                                                                               
NINV20   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
NINV30   CLC   XKEY(3),SAVEKEYX    X'0E83' A/M?                                 
         BNE   NINVX               NO - EXIT                                    
*                                                                               
         MVC   HALF,SNVPCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    *+14                YES                                          
         MVC   SNVPPRD(20),XFF     NO - FORCE NEXT CLIENT                       
         B     NINV10              READ HIGH                                    
*                                                                               
         LA    R3,SNVPMKT          MARKET (OR BUYER SEQ NUMBER)                 
         OC    SNVPMKT2,SNVPMKT2   IS MARKET HERE?                              
         BZ    *+8                 NO                                           
         LA    R3,SNVPMKT2         YES - THIS IS THE MARKET                     
         CLC   OLDMKT,0(R3)        SAME MARKET?                                 
         BNE   NINV20              NO - READ SEQ                                
*                                                                               
         CLC   SNVPSTA,OLDSTA      MATCH ON STATION?                            
         BNE   NINV20              NO - READ SEQ                                
*                                                                               
NINV40   MVC   SAVEKEYX,XKEY       SAVE OFF THE KEY OLD                         
         LA    RF,ACCUNINQ         NINV RECORD COUNTER                          
         BRAS  RE,BUMPACC          BUMP COUNTER                                 
*                                                                               
         CLI   QOPT5,C'Y'          TRACE?                                       
         BNE   *+12                NO                                           
         MVI   TRACECDE,49         NINV RECORD                                  
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         L     R6,AREC             A(RECORD)                                    
         MVI   BYTE,X'E8'          LOOK FOR THE X'E8' ELEMENT                   
         BRAS  RE,GETEL            FOUND THE X'E8' ELEMENT                      
         BNE   NINVERR             NO - ERROR                                   
         USING SNVMMELD,R6         X'E8' ELEMENT DSECT                          
         MVC   SNVMMMKT,NEWMKT     NEW MARKET                                   
         DROP  R6                                                               
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   NINV60              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         OI    XKEY+32,X'80'       MARK KEY DELETED                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
*                                                                               
         MVC   0(2,R3),NEWMKT      UPDATE MARKET IN KEY                         
         NI    XKEY+32,X'7F'       UNDELETE KEY                                 
         GOTO1 DATAMGR,DMCB,DMADD,=C'XSPDIR',XKEY,XKEY                          
         CLI   DM3,0               ANY ERRORS?                                  
         BE    NINV60              NO                                           
         TM    DM3,X'20'           DUP KEY ON ADD?                              
         BNZ   NINVERR2            YES - GIVE ERROR MESSAGE & READ SEQ          
         DC    H'0'                OTHER DATAMGR ERROR                          
*                                                                               
NINV60   MVC   XKEY,SAVEKEYX       RESTORE KEY WITH OLD MARKET                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         B     NINV20              READ SEQ                                     
*                                                                               
NINVX    MVC   DATADISP,=H'24'     RESTORE DATADISP TO 24                       
         J     EXIT                EXIT                                         
*                                                                               
NINVERR  MVC   P(28),=C'* ERROR * NO NINV E8 ELEMENT'                           
         GOTO1 REPORT              PRINT THE ERROR MESSAGE                      
         B     NINVERRX            RESTORE READ SEQUENCE AND READ SEQ           
*                                                                               
NINVERR2 MVC   P(35),=C'* ERROR * NEW INV EXISTS IN NEW MKT'                    
         GOTO1 REPORT              PRINT THE ERROR MESSAGE                      
*                                                                               
NINVERRX MVI   TRACECDE,49         NINV RECORD                                  
         BRAS  RE,XPTRAC           *** TRACE ***                                
         B     NINV60              RESTORE READ SEQUENCE AND READ SEQ           
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
* THIS SECTION UPDATES THE ORDER HISTORY RECORDS *                              
*                                                                               
T2000    NTR1  BASE=*,LABEL=*                                                   
         XC    XKEY,XKEY           BUILD KEY                                    
KY2      USING OHISRECD,XKEY                                                    
*                                                                               
         MVI   KY2.OHISTYP,OHISTYQ                                              
         MVI   KY2.OHISSTYP,OHISSTYQ                                            
         MVC   KY2.OHISBKAM,BAGYMD                                              
*                                                                               
T2100    XC    SAVEKEYX,SAVEKEYX   SAVE TYPE/AGMD                               
         MVC   SAVEKEYX(L'OHISKEY),XKEY                                         
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     T2300                                                            
*                                                                               
T2200    GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
*                                                                               
T2300    CLC   XKEY(OHISORD-OHISKEY),SAVEKEYX    TEST SAME TYPE                 
         BNE   T2000X               NO -- EXIT                                  
*                                                                               
         LA    R6,SAVEKEYX                                                      
         USING OHISRECD,R6                                                      
*                                                                               
         CLC   KY2.OHISBKAM,OHISBKAM   TEST SAME A/M                            
         BE    T2310                   YES - CONTINUE TO THE CLIENT             
         MVC   KY2.OHISBKAM,BAGYMD     NOT THERE YET, HIGH FOR REQ A/M          
         BL    T2100                   READ HIGH                                
         MVI   KY2.OHISBKAM,X'FF'      FF A/M TO GET NEXT ORDER                 
         B     T2100                   READ HIGH                                
*                                                                               
T2310    MVC   HALF,KY2.OHISBKCL       BINARY CLIENT                            
         BRAS  RE,CLTCHK               OK TO PROCESS THIS CLIENT?               
         BE    T2320                   YES - CONTINUE TO THE PRODUCT            
         MVC   KY2.OHISBKPR(8),XFF     NO - FORCE NEXT CLIENT                   
         B     T2100                                                            
*                                                                               
T2320    CLC   KY2.OHISBKMK(5),OLDMKT  TEST SAME MKT/STA                        
         BE    T2340                   YES - CONTINUE TO THE STA                
         MVC   KY2.OHISBKMK(5),OLDMKT  BUMP TO MARKET/STA                       
         BL    T2100                   IF LESS THEN READ HIGH                   
         MVC   KY2.OHISBKMK(5),XFF     FF MKT/STA TO GET NEXT PRD               
         B     T2100                   READ HIGH                                
*                                                                               
T2340    GOTO1 =A(ESTCHK),DMCB,KY2.OHISBKCL,KY2.OHISBKES                        
         CLI   MOVECODE,C'N'           REC BEING MOVED?                         
         BNE   T2500                   YES - PROCEED                            
         MVI   KY2.OHISBKLN,X'FF'      FF LINE FOR NEXT EST                     
         B     T2100                   READ HIGH                                
*                                                                               
T2500    LA    RF,ACCUOHSQ             ORDER HISTORY ACCUM                      
         BRAS  RE,BUMPACC              BUMP RECORD COUNT                        
*                                                                               
         CLI   QOPT5,C'Y'              TRACE REQ?                               
         BNE   *+12                    NO                                       
         MVI   TRACECDE,42             ORDER HISTORY RECORDS                    
         BRAS  RE,XPTRAC               *** TRACE ***                            
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0                   ANY ERRORS?                              
         BE    *+6                     NO                                       
         DC    H'0'                    YES - DEATH                              
*                                                                               
         L     R6,AREC                 A(ORDER HISTORY RECORD)                  
         OI    34(R6),X'80'            MARK RECORD DELETED                      
         OI    XKEY+32,X'80'           MARK KEY DELETED                         
         CLI   RCWRITE,C'Y'            RUNNING UPDATIVE?                        
         BNE   T2510                   NO - BYPASS THE WRITE                    
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0                   ANY ERRORS?                              
         BE    *+6                     NO                                       
         DC    H'0'                    YES - DEATH                              
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
*                                                                               
T2510    MVC   SAVEKEYX,XKEY           SAVE OFF THE OLD KEY                     
         MVC   OHISBKMK,NEWMKT         UPDATE MARKET IN RECORD                  
         NI    34(R6),X'7F'            UNDELETE RECORD                          
         LA    R6,XKEY                 R2 = XKEY                                
         MVC   OHISBKMK,NEWMKT         UPDATE MARKET IN KEY                     
         NI    XKEY+32,X'7F'           UNDELETE KEY                             
*                                                                               
         MVC   XKEYSAVE,XKEY           SAVE OFF THE KEY                         
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY             
         CLC   XKEY(32),XKEYSAVE       DOES REC ALREADY EXIST                   
         BE    OHISTERR                YES - GIVE ERR MSG & READ SEQ            
*                                                                               
         CLI   RCWRITE,C'Y'            RUNNING UPDATIVE?                        
         BNE   T2530                   NO - BYPASS THE WRITE                    
         MVC   XKEY,XKEYSAVE           KEY WITH THE NEW MARKET                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0                   ANY ERRORS?                              
         BE    *+6                     NO                                       
         DC    H'0'                    YES - DATAMGR ERROR                      
*                                                                               
T2530    MVC   XKEY,SAVEKEYX           RESTORE KEY WITH OLD MARKET              
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         B     T2200                   READ SEQ                                 
*                                                                               
T2000X   J     EXIT                    EXIT                                     
*                                                                               
OHISTERR MVC   P+10(4),=C'CLT='        CLT= IN HEADING                          
         GOTO1 CLUNPK,DMCB,OHISBKCL,P+14                                        
         GOTO1 MSUNPK,DMCB,OHISBKMK,P+20,P+26                                   
         MVC   P+32(39),=C'* ERROR * ORD HISTORY EXISTS IN NEW MKT'             
         MVI   SPACING,2               2 SPACES                                 
         GOTO1 REPORT                  PRINT THE ERROR MESSAGE                  
         MVI   TRACECDE,42             ORD HISTORY RECORD                       
         BRAS  RE,XPTRAC               *** TRACE ***                            
         B     T2530                   RESTORE READ SEQ & READ SEQ              
         DROP  R6,KY2                  DROP ALL RECORD USINGS                   
         LTORG                                                                  
*                                                                               
*======================================================================         
* THIS ROUTINE UPDATES THE US-SBTK REVISION/WORK RECORDS                        
*======================================================================         
         USING USBRWKD,R5                                                       
USBTKREC NTR1  BASE=*,LABEL=*,WORK=(R5,USBRWKL)                                 
*                                                                               
         LR    R0,R5               CLEAR SAVED STORAGE                          
         LHI   R1,USBRWKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DATADISP,=H'42'     DATADISP NOW 42                              
         MVC   USBRSUBT,DM1+3      SAVE RECORD SUBTYPE                          
         MVC   USBRACCU,DM2+3      SAVE ACCUMULATOR EQUATE                      
         MVC   USBRTRCD,DM3+3      SAVE TRACEHD EQUATE                          
*                                                                               
         GOTO1 GTSTAEDT,DMCB,QSTA,USBSTA                                        
         OC    USBSTA,SPACES                                                    
*                                                                               
         XC    XKEY,XKEY           INIT XKEY                                    
         USING DRVRECD,R2          BUILD US-SBTK REVISION KEY                   
         LA    R2,XKEY                                                          
         MVI   DRVKTYP,DRVKTYPQ    X'0E'                                        
         MVC   DRVKSUB,USBRSUBT    SET SUB KEY                                  
         MVC   DRVKAM,BAGYMD       A/M                                          
         CLC   QCLT,=C'ALL'        AGENCY LEVEL IS START FOR ALL CLI            
         BE    *+10                                                             
         MVC   DRVKCLT,BCLT        ELSE CLIENT SPECIFIC                         
*                                                                               
USBR010  MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'XSPDIR',XKEY,XKEY                     
         B     USBR030                                                          
*                                                                               
USBR020  GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'XSPDIR',XKEY,XKEY                     
USBR030  CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
         CLI   QOPT4,C'Y'          TRACE REQ?                                   
         BNE   USBR031             NO                                           
         MVC   TRACEMSG,=CL24'TEST-US-SBTK SCAN READHI'  CUSTOM MESSAGE         
         L     RF,0(R1)                                                         
         CLC   DMRSEQ,0(RF)        SEQ?                                         
         BNE   *+10                                                             
         MVC   TRACEMSG,=CL24'TEST-US-SBTK SCAN READSQ'  CUSTOM MESSAGE         
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
USBR031  CLC   XKEY(DRVKAM-DRVKEY+L'DRVKAM),XKEYSAVE  SAME X'0E11'+A/M?         
         BNE   USBTRECX            NO -- EXIT                                   
*                                                                               
         CLC   DRVKCLT,USBRCLT     SAME CLIENT? ALREADY CHECKED IT?             
         BE    USBR040             -YES, SKIP CHECK                             
         MVC   HALF,DRVKCLT        BINARY CLIENT                                
         BRAS  RE,CLTCHK           OK TO PROCESS THIS CLIENT?                   
         BE    USBR035             -YES                                         
         CLC   QCLT,=C'ALL'        NO, PROCESS ALL CLIENTS?                     
         BNE   USBTRECX            -NO, EXIT                                    
         MVC   DRVKPRD,XFF         -YES, FORCE NEXT CLIENT                      
         B     USBR010             AND PROCESS NEXT CLIENT                      
*                                                                               
USBR035  MVC   USBRCLT,DRVKCLT                                                  
*                                                                               
USBR040  CLC   DRVKEST,USBREST     SAME EST? ALREADY CHECKED IT?                
         BE    USBR050             -YES, SKIP CHECK                             
         GOTO1 =A(ESTCHK),DMCB,DRVKCLT,DRVKEST                                  
         CLI   MOVECODE,C'N'       REC BEING MOVED?                             
         BE    USBR060             NO - BUMP MKT TO GET NEXT EST                
         MVC   USBREST,DRVKEST                                                  
*                                                                               
USBR050  CLC   DRVKMKT,OLDMKT      TEST SAME MKT                                
         BE    USBR070             YES - CONTINUE TO THE STA                    
         MVC   DRVKMKT,OLDMKT      BUMP TO MARKET/STA                           
         BL    *+10                IF LESS THEN READ HIGH                       
USBR060  MVC   DRVKMKT,XFF         FF MKT TO GET NEXT EST                       
         XC    DRVKREVS,DRVKREVS                                                
         B     USBR010             READ HIGH                                    
*                                                                               
USBR070  DS    0H                                                               
         CLI   DRVKFLG,0           SUPP RECORD?                                 
         BNE   USBR020              YES, SEQ FOR A LINE RECORD                  
*                                                                               
         OC    DRVKREVL,DRVKREVL   ARE WE PROCESSING A LINE?                    
         BZ    USBR020              NO, SEQ FOR A LINE RECORD                   
*                                                                               
         CLI   QOPT4,C'Y'          TRACE REQ?                                   
         BNE   *+14                NO                                           
         MVC   TRACEMSG,=CL24'TEST-US-SBTK SCAN GETREC'  CUSTOM MESSAGE         
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
         L     R6,AREC             A(ORDER WS/RS RECORD)                        
         MVI   BYTE,RLDELQ         LOOK FOR X'11' LINE DESC. ELEM               
         BAS   RE,GETEL                                                         
         BNE   USBR020                                                          
         USING RLDELD,R6                                                        
         CLC   RLDSTA,USBSTA       FOUND A MARKET/STATION MATCH?                
         BNE   USBR020             NO - READ SEQ NEXT RECORD                    
         DROP  R6                                                               
*                                                                               
* FOUND SHEET/LINES TO MOVE                                                     
*                                                                               
         MVC   XKEYSAVE,XKEY       SAVE KEY FOR READ SEQUENCE RESTORE           
*                                                                               
         BRAS  RE,USBRMVIT         MOVE THIS SHEET & LINES                      
*                                                                               
         MVC   DRVKEY,XKEYSAVE     RESTORE READ SEQUENCE                        
         MVC   DRVKREVL,XFF        FORCE NEXT SHEET                             
*                                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         CLI   DM3,0               ANY ERRORS?                                  
         JE    USBR030             NO, READ SEQ                                 
         DC    H'0'                YES - DATAMGR ERROR                          
*                                                                               
USBTRECX J     EXIT                EXIT                                         
         DROP  R2                                                               
***********************************************************************         
* MUST MODIFY STATION TO FIT REVISION/WORKSHEET FORMAT                          
*   ON ENTRY : P1   5 CHAR REQ STATION                                          
*   ON EXIT  : P2   MEDIA R    : CONVERT STATION TO SSSS-B OR SSS-B             
*            :      OTHER MEDIA: COPY AND REMOVE BAND T                         
***********************************************************************         
GTSTAEDT NTR1                                                                   
         L     R1,DMCB             R1 = A(INPUT STATION)                        
         L     R2,DMCB+4           R2 = A(OUTPUT STATION)                       
*                                                                               
         LA    RE,3                SET EX TO COPY 4 CHARS                       
         CLI   3(R1),C' '          3 CHAR STATION?                              
         BH    *+6                 NO                                           
         BCTR  RE,0                YES, SET EX TO COPY 3 CHARS                  
*                                                                               
         MVC   0(0,R2),0(R1)                                                    
         EX    RE,*-6              COPY THE STATION                             
*                                                                               
         LA    RF,1(RE,R2)         POINT TO END STATION                         
         CLI   4(R1),C' '          HAVE BAND?                                   
         BE    GTSTAEDTX           NO, DONE                                     
*                                                                               
         CLI   QMED,C'R'           MEDIA R?                                     
         BNE   GTSTA010                                                         
         MVI   0(RF),C'-'          MOVE IN -                                    
         MVC   1(1,RF),4(R1)       AND BAND(IE. WABC-F, DXNY-S, YYHW-C)         
         B     GTSTAEDTX                                                        
*                                                                               
GTSTA010 CLI   QMED,C'X'           MEDIA X?                                     
         BE    GTSTA020             MOVE IN BAND (IE WABCX)                     
*                                                                               
         CLI   QMED,C'T'           MEDIA T?                                     
         JNE   *+2                  NEW MEDIA? CHECK STATION FORMAT!!           
*                                                                               
         CLI   4(R1),C'T'          MEDIA T WITH 'T' BAND?                       
         BE    GTSTAEDTX           YES, LEAVE OFF THE T (IE. WABC)              
*                                                                               
GTSTA020 MVC   0(1,RF),4(R1)       MOVE IN BAND (IE CMNYD, YYSLL)               
*                                                                               
GTSTAEDTX J    EXIT                                                             
*                                                                               
*----------------------------------------------------------------------         
* FOUND SHEET WITH LINES THAT MATCH MKT/STA                                     
* 1-RE-READ THE SHEET AND COPY IT (NOT MOVE) BECAUSE THERE MAY BE               
*   LINES WITH OTHER STATIONS PRESENT IN THE SHEET                              
* 2-MOVE THE LINES (WITH THE STATION) TO NEW SHEET                              
* 3-IF NO OTHER STATIONS IN SHEET, THEN DELETE ORIGINAL MKT SHEET               
*----------------------------------------------------------------------         
*                                                                               
USBRMVIT NTR1  LABEL=*                                                          
*                                                                               
         MVI   USBFLAG,0                                                        
         XC    USBRNSS#,USBRNSS#                                                
         XC    USBRNLS#,USBRNLS#                                                
*                                                                               
USBRM005 XC    XKEY,XKEY                                                        
         USING DRVRECD,R2                                                       
         LA    R2,XKEY                                                          
         MVC   XKEY(DRVKREVL-DRVKEY),XKEYSAVE  READ AM/C/P/E/M/SHEET            
         MVC   SAVEKEYX,XKEY       SAVE SHEET KEY                               
*                                                                               
USBRM010 GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEY,XKEY                         
         B     USBRM025                                                         
*                                                                               
USBRM020 GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',XKEY,XKEY                         
USBRM025 CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
         CLI   QOPT4,C'Y'          TRACE REQ?                                   
         BNE   USBRM030            NO                                           
         MVC   TRACEMSG,=CL24'TEST-US-SBTK MOVE READHI'  CUSTOM MESSAGE         
         L     RF,0(R1)                                                         
         CLC   DMRSEQ,0(RF)        SEQ?                                         
         BNE   *+10                                                             
         MVC   TRACEMSG,=CL24'TEST-US-SBTK MOVE READSQ'  CUSTOM MESSAGE         
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
USBRM030 OC    USBRNSS#,USBRNSS#   HAVE WE PROCESSED SHEET?                     
         BNZ   USBRM045            YES                                          
         CLC   DRVKEY,SAVEKEYX     MUST HAVE A SHEET TO MOVE                    
         JNE   EXIT                NO, DON'T MOVE, EXIT MOVE                    
*                                                                               
USBRM045 CLC   DRVKEY(DRVKREVL-DRVKEY),SAVEKEYX SAME AM/C/P/E/M/SHEET?          
         JE    USBRM050             YES                                         
         TM    USBFLAG,USBFOSIS    FOUND OTHER STATIONS IN SHEET?               
         JO    EXIT                 YES, MOVE FINISHED.                         
         OI    USBFLAG,USBFDELS     NO, GO BACK AND DELETE SHEET                
         B     USBRM005                                                         
*                                  YES, KEEP PROCESSING THIS SHEET&LINE         
USBRM050 GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
         OC    DRVKREVL,DRVKREVL   ARE WE PROCESSING A SHEET?                   
         BNZ   USBRM080            -NO, HAVE LINE                               
*                                                                               
         TM    USBFLAG,USBFDELS    NEED TO DELETE SHEET RECORDS?                
         BZ    USBRM060             NO                                          
         L     R6,AREC             A(REVISION LINE RECORD)                      
         OI    34(R6),X'80'        MARK RECORD DELETED                          
         OI    DRVKSTAT,X'80'      MARK KEY DELETED                             
                                                                                
         CLI   QOPT4,C'Y'          TRACE REQ?                                   
         BE    *+8                                                              
         CLI   QOPT5,C'Y'          TRACE REQ?                                   
         BNE   *+14                                                             
         MVC   TRACECDE,USBRTRCD                                                
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   USBRM020            NO - BYPASS THE WRITE                        
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
                                                                                
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
         J     USBRM020                                                         
*                                                                               
* PROCESSING SHEET                                                              
*                                                                               
USBRM060 MVC   SAVEKEYX,XKEY       SAVE OLD KEY FOR READ SEQ RESTORE            
*                                                                               
         CLI   DRVKFLG,0           SUPP SHEET RECORD? (IE COMMENT REC)          
         BNE   USBRM070            -YES                                         
*                                                                               
         MVC   DRVKMKT,NEWMKT      -NO, SET NEW MKT IN KEY                      
         XC    DRVKREVS,DRVKREVS   CLEAR SHEET NUMBER                           
*                                                                               
* SEE IF SHEET ALREADY EXISTS IN NEW MARKET                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
*                                  PASS BACK THE DELETED RECORDS                
         TM    DM3,X'FF'-X'02'     ANY ERRORS EXCEPT DELETED?                   
         JNZ   *+2                 YES - DEATH                                  
*                                                                               
         MVC   USBRNSS#,=X'FFFFFE' INIT NEW SHEET SEQ# TO 1 (FF COMP)           
         CLC   DRVKEY(DRVKMKT-DRVKEY),SAVEKEYX  DOES AM/C/P/E &                 
         BNE   USBRM065            NO                                           
         CLC   DRVKMKT,NEWMKT      NEW MKT ALREADY EXIST?                       
         BNE   USBRM065            NO                                           
*                                                                               
         ICM   R1,7,DRVKREVS       YES, READ CURRENT HI SHEET SEQ# AND          
         BCTR  R1,0                GET NEXT AVAIL SHEET SEQ#                    
         STCM  R1,7,USBRNSS#       SAVE NEW SHEET SEQ#                          
*                                                                               
USBRM065 XC    XKEY,XKEY           CLEAR GARBAGE KEY (FROM PREV RDHI)           
USBRM070 MVC   DRVKEY(DRVKMKT-DRVKEY),SAVEKEYX RESTORE AM/C/P/E KEY &           
         MVC   DRVKMKT,NEWMKT      SET NEW MKT &                                
         MVC   DRVKREVS,USBRNSS#   SET NEW SHEET SEQ (FF COMP)                  
*                                                                               
         L     R6,AREC                                                          
         MVC   0(L'DRVKEY,R6),XKEY UPDATE NEW KEY IN SHEET RECORD               
         B     USBRM110            GO ADD THE SHEET                             
*                                                                               
* PROCESSING LINE                                                               
*                                                                               
USBRM080 TM    USBFLAG,USBFDELS    NEED TO DELETE SHEET RECORDS?                
         JO    EXIT                                                             
*                                                                               
         CLI   DRVKFLG,0           SUPP LINE RECORD? (IE COMMENT REC)           
         BE    USBRM085            -NO                                          
         CLC   USBCRSTA,USBSTA     -YES, CMMT BELONG TO MATCH STATION           
         BE    USBRM070            -YES, MOVE IT                                
*                                                                               
USBRM085 L     R6,AREC             A(REVISION LINE RECORD)                      
         MVI   BYTE,RLDELQ         LOOK FOR X'11' LINE DESC. ELEM               
         BAS   RE,GETEL                                                         
         JNE   USBRM090            IF NOT PRESENT, SKIP IT                      
         USING RLDELD,R6                                                        
         MVC   USBCRSTA,RLDSTA                                                  
         CLC   RLDSTA,USBSTA       MATCH ON STATION?                            
         BE    USBRM095                                                         
*                                                                               
USBRM090 OI    USBFLAG,USBFOSIS    FLAG OTHER STATIONS IN SHEET                 
         MVC   DRVKFLG,XFF         FORCE READ NEXT LINE                         
         MVC   SAVEKEYX,XKEY                                                    
         B     USBRM010                                                         
         DROP  R6                                                               
*                                                                               
* FOUND LINE WITH MATCHING MKT/STA                                              
* DELETE OLD LINE, AND ADD NEW LINE                                             
*                                                                               
USBRM095 MVC   SAVEKEYX,XKEY       SAVE OLD KEY FOR READ SEQ RESTORE            
*                                                                               
         L     R6,AREC             A(REVISION LINE RECORD)                      
         OI    34(R6),X'80'        MARK RECORD DELETED                          
         OI    DRVKSTAT,X'80'      MARK KEY DELETED                             
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   USBRM100            NO - BYPASS THE WRITE                        
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XKEY+36,(R6),DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,=C'XSPDIR',XKEY,XKEY                          
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DEATH                                  
*                                                                               
USBRM100 NI    DRVKSTAT,X'7F'      UNDELETE KEY                                 
         MVC   DRVKMKT,NEWMKT      SET NEW MKT &                                
         MVC   DRVKREVS,USBRNSS#   SET NEW SHEET SEQ (FF COMP)                  
*                                                                               
         MVC   DRVKREVL,=X'FFFFFE' SET LINE SEQ 1 (FF COMP)                     
         SR    R1,R1                                                            
         ICM   R1,7,USBRNLS#       HAVE A LINE ALREADY?                         
         BZ    USBRM105            NO                                           
         BCTR  R1,0                GET NEXT AVAIL LINE SEQ#                     
         STCM  R1,7,DRVKREVL       SET NEW LINE SEQ# (FF COMP)                  
USBRM105 MVC   USBRNLS#,DRVKREVL                                                
*                                                                               
         L     R6,AREC                                                          
         NI    34(R6),X'7F'        UNDELETE RECORD                              
         MVC   0(L'DRVKEY,R6),XKEY UPDATE NEW KEY IN LINE RECORD                
         DROP  R2                                                               
*                                                                               
USBRM110 DS    0H                                                               
         CLI   QOPT4,C'Y'          TRACE REQ?                                   
         BE    *+8                                                              
         CLI   QOPT5,C'Y'          TRACE REQ?                                   
         BNE   *+14                                                             
         MVC   TRACECDE,USBRTRCD                                                
         BRAS  RE,XPTRAC           *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        RUNNING UPDATIVE?                            
         BNE   USBRM120            NO - BYPASS THE WRITE                        
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',XKEY+36,AREC,DMWORK               
         CLI   DM3,0               ANY ERRORS?                                  
         JNE   *+2                 YES - DATAMGR ERROR                          
*                                                                               
USBRM120 DS    0H                                                               
         LLC   RF,USBRACCU         REVISION/WORK ACCUM                          
         BRAS  RE,BUMPACC          BUMP RECORD COUNT                            
*                                                                               
         MVC   XKEY,SAVEKEYX       RESTORE READ SEQ (&READ FOR DELETED)         
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',XKEY,XKEY                 
         TM    DM3,X'FF'-X'02'     ANY ERRORS EXCEPT DELETED?                   
         JZ    USBRM020                                                         
         DC    H'0'                YES - DEATH                                  
*                                                                               
USBRWKD  DSECT                                                                  
USBRSUBT DS    X                                                                
USBRACCU DS    X                                                                
USBRCLT  DS    XL2                                                              
USBREST  DS    X                                                                
USBRNSS# DS    XL3                 NEW SHEET SEQ#                               
USBRNLS# DS    XL3                 NEW LINE SEQ#                                
USBRTRCD DS    X                                                                
USBSTA   DS    CL(L'RLDSTA)                                                     
USBCRSTA DS    CL(L'RLDSTA)                                                     
USBFLAG  DS    X                                                                
USBFDELS EQU   X'80'                DELETE SHEET                                
USBFOSIS EQU   X'40'                OTHER STATIONS IN SHEET                     
USBRWKL  EQU   *-USBRWKD                                                        
SPSX02   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    XKEY,XKEY                                                        
         XC    XKEYSAVE,XKEYSAVE                                                
         XC    SAVEKEYX,SAVEKEYX                                                
         MVC   DATADISP,=H'24'                                                  
*                                                                               
         LA    R4,CLTBPARS+4                                                    
         ICM   R3,15,=A(CLTBUFFL)                                               
         GETMAIN  EC,LV=(R3),A=(R4)                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    CLTBPARS(4),CLTBPARS A(RECORD)                                   
         SR    R2,R2                R2 = NUMBER OF RECS IN TABLE SO FAR         
         LA    R3,CBUFFLEN          R3 = L'RECORD                               
         LA    R4,2                 R4 = DISPLACEMENT OF KEY INTO REC           
         LAY   R5,CBUFFMAX          R5 = MAX NUMBER OF RECS IN TABLE            
         STM   R2,R5,CLTBPARS+8     SET BINSRCH PARS FOR CLT TABLE              
*                                                                               
         LA    R4,PWBPARS+4                                                     
         ICM   R3,15,=A(PWBUFFL)                                                
         GETMAIN  EC,LV=(R3),A=(R4)                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PWBPARS(4),PWBPARS   A(RECORD)                                   
         SR    R2,R2                R2 = NUMBER OF RECS IN TABLE SO FAR         
         LA    R3,PBUFFLEN          R3 = L'RECORD                               
         LA    R4,3                 R4 = DISPLACEMENT OF KEY INTO REC           
         LAY   R5,PBUFFMAX          R5 = MAX NUMBER OF RECS IN TABLE            
         STM   R2,R5,PWBPARS+8      SET BINSRCH PARS FOR CLT TABLE              
*                                                                               
         J     EXIT                                                             
**********************************************************************          
         TITLE 'SPSX02 - MARKET FIX PROGRAM - APPLICATION - ESTBTABD'           
*                                                                               
* WILA PW BUYS NOT MOVED TABLE                                                  
*                                                                               
PWTABD   DSECT                                                                  
PWTABENT DS   0CL11                                                             
PWTABCOM DS   0CL9                                                              
PWTABCLT DS    CL3                                                              
PWTABPRD DS    CL3                                                              
PWTABEST DS    CL3                                                              
PWTABCT  DS    XL2                                                              
PWTABNXT EQU   *                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENMSR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRTBAE                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLH                                                       
         EJECT                                                                  
*PREFIX=X                                                                       
       ++INCLUDE SPGENXLK                                                       
*PREFIX=                                                                        
         EJECT                                                                  
       ++INCLUDE SPGENMKTFX                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRBTC                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         EJECT                                                                  
* NEW BUYERS WORKSHEET RECORDS                                                  
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPNWSDTL                                                       
* MINIO FOR NEW BUYERS WORKSHEET                                                
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENORHIS                                                     
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENCDORD                                                     
       ++INCLUDE SPGENDREV                                                      
       ++INCLUDE SPGENDDEV                                                      
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
*                                                                               
CLTBUFFD DSECT                                                                  
CBCLT    DS    CL2                  CLIENT CODE                                 
CBOFF    DS    CL1                  CLIENT OFFICE                               
CBEXTRA  DS    CL1                  BUY ID REQUIRED                             
CBFLAG   DS    CL1                  CLIENT FLAGS                                
CBCBLMKT EQU   X'80'                CBLMKT REC EXISTS, DO NOT PROCESS           
CBCLTSTA EQU   X'40'                CLT STA REC EXISTS, DO NOT PROCESS          
CBPW     EQU   X'20'                CLIENT IS A PW CLIENT                       
CBMKT    DS    CL4                  MARKET                                      
*                                                                               
CBUFFLEN EQU   *-CLTBUFFD           LENGTH OF CLIENT BUFFER ENTRY               
CBUFFMAX EQU   12000                MAX NUMBER OF CLIENT BUFFER ENTRIES         
CLTBUFFL EQU   CBUFFMAX*CBUFFLEN    MAX BUFFER SIZE                             
*                                                                               
PWBUFFD  DSECT                                                                  
PWBCLT   DS    CL2                  CLIENT CODE                                 
PWBEST   DS    CL1                  PW ESTIMATE                                 
*                                                                               
PBUFFLEN EQU   *-PWBUFFD            LENGTH OF PW BUFFER ENTRY                   
PBUFFMAX EQU   20000                MAX NUMBER OF PW BUFFER ENTRIES             
PWBUFFL  EQU   PBUFFMAX*PBUFFLEN    MAX BUFFER SIZE                             
*                                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL5                                                              
PPRD     DS    CL3                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL5                                                              
PELEM    DS    CL3                                                              
         DS    CL2                                                              
PBILLP   DS    CL5                                                              
         DS    CL4                                                              
PBILLD   DS    CL8                                                              
         DS    CL2                                                              
PINVNO   DS    CL7                                                              
         DS    CL3                                                              
PGROSS   DS    CL12                                                             
         DS    CL4                                                              
PNET     DS    CL12                                                             
         DS    CL4                                                              
PSPOTS   DS    CL6                                                              
PTAX     DS    CL10                                                             
*                                                                               
         PRINT ON                                                               
* ====================== LOCAL WORKING STORAGE ====================== *         
SXWORKD  DSECT                                                                  
*                                                                               
SAVEPRT  DS    CL132                                                            
TRACECDE DS    C                   RACE CODE                                    
TRACECPM EQU   0                   CUSTOM PASS MESSAGE                          
TRACEMSG DS    CL24                                                             
XKEY     DS    XL48                                                             
XKEYSAVE DS    XL48                                                             
SAVEKEYX DS    XL48                                                             
CLTBPARS DS    6F                  BINSRCH PARS FOR CLIENT BUFFER               
PWBPARS  DS    6F                  BINSRCH PARS FOR PW BUFFER                   
XFF      DS    XL27                X'FF'S                                       
AGYMDT   DS    XL1                 AGENCY/MEDIA T FOR MEDIA C REQUEST           
AGYMDN   DS    XL1                 AGENCY/MEDIA N FOR MEDIA C REQUEST           
*                                                                               
         DS    0H                                                               
OLDMSTAN DS    0CL5                                                             
OLDMKTN  DS    H                                                                
OLDSTAN  DS    CL3                                                              
*                                                                               
SXWORKX  EQU   *                                                                
* ====================== LOCAL WORKING STORAGE ====================== *         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158SPREPSX02 02/18/21'                                      
         END                                                                    
