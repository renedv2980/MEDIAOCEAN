*          DATA SET RECNT37    AT LEVEL 110 AS OF 11/06/13                      
*PHASE T80237A                                                                  
         TITLE 'T80237 - APPLY/REJECT/RECALL MAKEGOOD OFFERS'                   
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT37 (T80237) --- APPLY/REJECT/RECALL MAKEGOOD OFFERS    *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* 24SEP12 BOB FIRST AIR DATE PASSIVE POINTERS                     *             
* 21AUG12 KWA APPLY FROM LIST - UPDATE # OF OFFERS & MGO WIP STAT *             
* 01FEB12 SKU SKIP ADDING DUPLICATE ZERO MG SPLIT ELEMENT IN ORDER*             
*             TO MAKE ROOM. SKIP SAVING X'69' ELEMENT.            *             
* 07NOV06 SKU CHANGE SAVING X'69' TO SAVE ONLY THE GROUP CODE     *             
*             TO MAKEGOOD BUYS INSTEAD OF TARGET BUYS             *             
* 11JUN99 SKU SKIP ADDING X'76' ELEMENTS UNTIL MAX SIZE OF BUY    *             
*             RECORDS IS INCREASED                                *             
* 31AUG05 HQ  PREEMPT/CREDIT SHOULD SKIP THE MAX BUY# CHEKC       *             
* 19MAY05 HQ  DELETE REP MAKEGOOD DEMO IF DIFFERENT THAN CONTRACT *             
* 14MAY03 SKU SELF APPLY                                          *             
* 18APR03 SKU CHOICE APPLY BUG FIX                                *             
* 18JUL02 SKU DEMO RATINGS SUPPORT                                *             
* 01FEB02 SKU REPLACEMENT OFFER SUPPORT                           *             
* 18DEC01 SKU FIX TAKEOVER BUG                                    *             
* 29OCT01 SKU CHANGE X'56' HELLO CALL TO SORT ON ENTIRE ELEMENT   *             
* 15AUG01 SKU SUPPORT DATE RANGE FOR PREEMPT                      *             
* 05APR01 SKU ENABLE TRADE FLAG FOR MAKEGOOD OFFERS               *             
* 17JAN01 HWO IGNORE AGENCY OFFICE WHEN SENDING MAKEGOOD OK NOTICE*             
* 30NOV00 SKU BUILD SIBLING ELEMENT FOR MKGD BUYS OF SAME OFFER   *             
* 02OCT00 SKU MOD CODE CHANGE                                     *             
* 05JAN00 SKU CONCEPTION                                          *             
*                                                                 *             
*******************************************************************             
T80237   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T80237,R9,CLEAR=YES,RR=R3                        
         LR    R7,RC                                                            
         USING MYWORKD,R7          LOCAL WORK AREA                              
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R2,4(R1)            POINTS TO CURRENT SELECT FIELD               
         L     R8,ASPULAR                                                       
         USING SPOOLD,R8                                                        
         ST    R8,ASPOOLD                                                       
TD       USING TSARD,TSARBLK                                                    
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,CHECKMG          CHECK IF ACTION VALID                        
         BZ    MGAPP05             ALSO PROCESSES REJECT/RECALL                 
         CLI   0(R2),0             PASSED A FAKE HEADER?                        
         BNE   ERROR                                                            
         LA    R2,CONCACTH         YES, POINT R2 TO CON ACT FIELD               
         B     ERROR                                                            
                                                                                
MGAPP05  DS    0H                                                               
         CLI   8(R2),C'A'          ONLY APPLY DO THE FOLLOWING                  
         BNE   EXXMOD                                                           
*                                                                               
         GOTO1 =A(CHKDEMO),RR=RELO                                              
*                                                                               
* INITIALIZE TSAR BUFFER                                                        
*                                                                               
         GOTO1 =A(INITTSAR),RR=RELO                                             
*                                                                               
* FILL TSAR BUFFER WITH TARGET BUY RECORDS                                      
*                                                                               
         GOTO1 =A(GETRECS),RR=RELO                                              
*                                                                               
         BAS   RE,FINDHI#          FIND HIGHEST BUY NUMBER TO USE               
         XC    SIBELEM,SIBELEM     INIT SIBLING ELEMENT AREA                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),MGDTREC                                                  
         GOTO1 VHIGH                                                            
         B     MGAPP20                                                          
                                                                                
MGAPP10  DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
MGAPP20  DS    0H                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MGAPP300            ALL DONE                                     
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         TM    RMKGKRTY,X'F0'      ALL LINES?                                   
         BZ    MGAPP40                                                          
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'        IF CHOICE, GET STATUS ELEMENT                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    MGAPP10                                                          
         DROP  R6                                                               
*                                                                               
MGAPP40  DS    0H                  CHECK IF DIRECT CREDIT PROCESSING            
         TM    RMKGRTS,X'10'                                                    
         BO    MGAPP43                                                          
         TM    RMKGRTS,X'02'       IF REPLACEMENT OFFER, CHECK IF               
         BZ    MGAPP45             CREDIT FOR NA SPOTS                          
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    MGAPP45                                                          
*                                                                               
MGAPP43  DS    0H                                                               
         GOTO1 =A(CREDIT),RR=RELO                                               
         B     MGAPP300                                                         
*                                                                               
MGAPP45  DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'11'        MOVE DETAIL COMMENT TO BUY COMMENT           
         BRAS  RE,GETEL                                                         
         BNE   MGAPP70                                                          
         USING RMKGDCEL,R6                                                      
         ZIC   R1,RMKGDCLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RMKGDCEL                                                 
         MVI   ELEM,X'04'                                                       
         DROP  R6                                                               
*                                                                               
         LA    R6,RMKGELEM                                                      
*                                                                               
MGAPP50  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    MGAPP60                                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),X'04'                                                      
         BNH   MGAPP50                                                          
*                                                                               
MGAPP60  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(C'R',RMKGREC),ELEM,(R6),0                           
*                                                                               
MGAPP70  DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'10',RMKGREC)                                    
         GOTO1 VDELELEM,DMCB,(X'11',RMKGREC)                                    
         GOTO1 VDELELEM,DMCB,(X'20',RMKGREC)                                    
*                                                                               
* NOTE: RMKGREC IS ORG'ED ON TOP OF RBUYREC                                     
* BUILD BUY KEY WITH THE NEXT HIGHEST NUMBER                                    
* FROM THIS POINT FORWARD THE MAKEGOOD OFFER WILL BECOME A NEW BUY              
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   RBUYKPLN,=3X'FF'                                                 
         MVC   RBUYKMLN,TARGET#                                                 
         MVC   RBUYKLIN,HIGHBUY#                                                
         DROP  R4                                                               
*                                                                               
* CONSTRUCT MAKEGOOD SIBLING ELEMENT                                            
*                                                                               
         LA    R3,SIBELEM+2                                                     
         LA    R1,3                                                             
MGAPP71  DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    MGAPP72                                                          
         AHI   R3,1                                                             
         AHI   R1,1                                                             
         CHI   R1,256                                                           
         BL    MGAPP71                                                          
         B     MGAPP73A            TOO MANY SIBLINGS, SKIP                      
*                                                                               
MGAPP72  DS    0H                                                               
         MVC   0(1,R3),HIGHBUY#                                                 
         STC   R1,SIBELEM+1                                                     
*                                                                               
* UPDATE MAKEGOOD BUY'S CREATION DATE AND VERSION NUMBER                        
*                                                                               
MGAPP73A DS    0H                                                               
         BAS   RE,UPTMGBUY                                                      
*                                                                               
* ADD REFERENCE TO OFFER WHICH THIS BUY WAS CREATED FROM                        
*                                                                               
*        GOTO1 =A(ADDMGREF),DMCB,RBUYREC,RR=RELO                                
*                                                                               
* CHECK IF CONTRACT IS TRADE AND SET TRADE FLAG IN BUY ACCORDINGLY              
*                                                                               
         BAS   RE,SETTRADE                                                      
*                                                                               
* EACH LATE RUN HAVE THEIR OWN TARGET LINE                                      
*                                                                               
         TM    RBUYRTS,X'08'+X'04'                                              
         BZ    MGAPP73                                                          
         TM    RBUYRTS,X'20'       LATE RUN BONUS, NO TARGET                    
         BO    MGAPP73                                                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYMGEL,R6                                                      
         MVC   RBUYKMLN,RBUYMGLI   SET MASTER TO TARGET                         
         DROP  R6                                                               
*                                                                               
MGAPP73  DS    0H                                                               
         TM    RBUYRTS,X'20'       IF BONUS LINE,                               
         BZ    *+10                                                             
         MVC   RBUYKMLN,RBUYKLIN   BUY IS NOT LINKED TO ANY TARGET              
*                                                                               
         CLI   HIGHBUY#,255                                                     
         BNE   *+6                                                              
         DC    H'0'                NO MORE ROOM!                                
         ZIC   RE,HIGHBUY#                                                      
         AHI   RE,1                                                             
         STC   RE,HIGHBUY#                                                      
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RBUYRTS,X'20'       ALL DONE AT THIS POINT FOR BONUS             
         BO    MGAPP10             PROCESS NEXT MAKEGOOD BUY                    
*                                                                               
* FROM NEW MAKEGOOD BUY'S X'05' MAKEGOOD REFERENCE ELEMENT, RETRIEVE            
* TARGET BUYS IN TSAR                                                           
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYMGEL,R6                                                      
*                                                                               
MGAPP75  DS    0H                                                               
         NI    MSFLAG,X'FF'-MSF1PASS                                            
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(RBUYKMLN-RBUYKEY),RBUYREC                              
MISSBUYD USING RBUYKEY,TSARREC+2                                                
*        MVC   MISSBUYD.RBUYKMLN,RBUYMGLI                                       
*        MVC   MISSBUYD.RBUYKLIN,RBUYMGLI                                       
*                                                                               
         MVI   TD.TSACTN,TSARDH                                                 
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGAPP78  DS    0H                                                               
         CLC   MISSBUYD.RBUYKLIN,RBUYMGLI                                       
         BE    MGAPP80                                                          
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    MGAPP78                                                          
         DC    H'0'                                                             
*                                                                               
MGAPP80  DS    0H                  ADD MAKEGOOD SPLITOUT ELEMENT                
         MVC   MISSDATE,RBUYMGD1                                                
*                                                                               
         NI    MSFLAG,X'FF'-MSFALTQ                                             
         CLI   RMKGMGLN,RMKGMGLQ   NEW LONGER ELEMENT?                          
         BL    MGAPP83                                                          
         TM    RMKGMGFG,X'80'      ALTERNATING WEEKS?                           
         BZ    MGAPP83                                                          
         OI    MSFLAG,MSFALTQ                                                   
*                                                                               
MGAPP83  DS    0H                                                               
         XC    ELEM,ELEM                                                        
MGSD     USING RBYMGSEL,ELEM                                                    
         MVI   MGSD.RBYMGSCD,X'56'                                              
         MVI   MGSD.RBYMGSLN,8                                                  
         MVC   MGSD.RBYMGSDT,MISSDATE                                           
         MVC   MGSD.RBYMGSLI,RBUYKLIN                                           
         MVC   MGSD.RBYMGSSP,RBUYMGSP                                           
         TM    RBUYRTS,X'02'                                                    
         BZ    *+8                                                              
         OI    MGSD.RBYMGSFG,X'80' FLAG REPLACMENT NA                           
         DROP  MGSD                                                             
*                                                                               
BUGD     USING RBUYREC,TSARREC+2                                                
         CLC   BUGD.RBUYLEN,=AL2(964)                                           
         BL    MGAPP84                                                          
         LA    R3,1010             RECORD FULL                                  
         B     ERROR                                                            
         DROP  BUGD                                                             
*                                                                               
MGAPP84  DS    0H                                                               
*                                                                               
* SKIP ADDING POINTLESS DUPLICATE ZERO MG SPLITOUT ELEMENT                      
*                                                                               
MGSD     USING RBYMGSEL,ELEM                                                    
         TM    MSFLAG,MSF1PASS                                                  
         BZ    MGAPP84A                                                         
         CLI   MGSD.RBYMGSSP,0                                                  
         BE    MGAPP84B                                                         
         DROP  MGSD                                                             
*                                                                               
MGAPP84A DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,TSARREC+2),(0,ELEM),0           
         OI    MSFLAG,MSF1PASS                                                  
*                                                                               
* REMOVE MISSED SPOT FROM X'03' ELEMENTS                                        
*                                                                               
MGAPP84B DS    0H                                                               
         GOTO1 =A(SPLIT03S),RR=RELO                                             
*                                                                               
* CHECK IF MISSED DATE RANGE IN USE. THE PRESENCE OF AN END DATE MEANS          
* MAKEGOOD BUY IS TARGET A RANGE OF DATES. IF IT IS, WE WILL NEED TO            
* GENERATE MULTIPLE X'56'S TO COVER THE DATE RANGE                              
*                                                                               
         OC    RBUYMGD2,RBUYMGD2                                                
         BZ    MGAPP85                                                          
*                                                                               
         LA    R3,7                                                             
         GOTO1 DATCON,DMCB,(3,MISSDATE),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,(0,WORK),(3,MISSDATE)                                
         CLC   MISSDATE,RBUYMGD2                                                
         BNH   MGAPP83                                                          
*                                                                               
MGAPP85  DS    0H                                                               
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    MGAPP90                                                          
*                                                                               
* UPDATE MOD CODE AND VERSION NUMBER                                            
*                                                                               
         GOTO1 =A(MGADDCDE),RR=RELO                                             
*                                                                               
* ALL DONE, WRITE RECORD BACK TO TSAR                                           
*                                                                               
MGAPP88  DS    0H                                                               
BUYD     USING RBUYREC,TSARREC+2                                                
         SR    R1,R1                                                            
         ICM   R1,3,BUYD.RBUYLEN   ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD BACK                            
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    MGAPP10                                                          
         DC    H'0'                                                             
         DROP  BUYD                                                             
*                                                                               
MGAPP90  DS    0H                                                               
         CLC   RBUYMGLI,MISSBUYD.RBUYKLIN                                       
         BE    MGAPP80             SAME TARGET, DIFFERENT MISSED DATE           
         DROP  MISSBUYD,R6                                                      
*                                                                               
* UPDATE MOD CODE AND VERSION NUMBER                                            
*                                                                               
         GOTO1 =A(MGADDCDE),RR=RELO                                             
*                                                                               
* DIFFERENT TARGET, WRITE RECORD BACK TO TSAR                                   
*                                                                               
BUYD     USING RBUYREC,TSARREC+2                                                
         SR    R1,R1                                                            
         ICM   R1,3,BUYD.RBUYLEN   ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD BACK                            
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    MGAPP75                                                          
         DC    H'0'                                                             
         DROP  BUYD                                                             
*                                                                               
* UPDATE CONTRACT VERSION NUMBER                                                
*                                                                               
MGAPP300 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
MGAPP330 DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    MGAPP350                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         TM    TWADARE,X'08'                                                    
         BZ    MGAPP340                                                         
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  R4                                                               
*                                                                               
MGAPP340 DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   ERROR                                                            
                                                                                
MGAPP350 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    MGAPP400                                                         
*                                                                               
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
MGAPP400 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
*                                                                               
* DUMP RECORDS FROM TSAR TO FILE                                                
*                                                                               
         GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
         BAS   RE,TRUDATE                                                       
*                                                                               
         BRAS  RE,MGOCOUNT         UPDATE # OF MAKEGOOD OFFERS COUNT            
*                                                                               
* GET CONTRACT REC                                                              
* WRITE UPDATED CONTRACT BACK TO FILE                                           
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  R4                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         BAS   RE,UPDTHDR          UPDATE MAKEGOOD HEADER RECORD                
*                                                                               
* IF DARE, SEND CONFIRMATION TO AGENCY                                          
*                                                                               
         BAS   RE,DARECNF                                                       
*                                                                               
* UPDATE RIS PASSIVE KEYS                                                       
*                                                                               
         GOTOR PASSUPD,DMCB,IOAREA UPDATE PASSIVE POINTERS                      
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         TM    TWACFCFL,X'80'      NEED TO SET FLAG IN THE TWA                  
         BZ    MGAPPX              TO DELETE CFC COMMENTS                       
         DROP  R4                                                               
*                                                                               
MGAPPX   DS    0H                                                               
         B     EXXMOD              RA WILL BE RESTORED UPON EXIT                
         EJECT                                                                  
***********************************************************************         
* UPDATE MAKEGOOD BUY'S CREATION DATE AND VERSION NUMBER                        
***********************************************************************         
UPTMGBUY NTR1                                                                   
BUYD     USING RBUYREC,RBUYREC                                                  
*                                                                               
* BUYS CREATED THROUGH NORMAL CHANNELS (BUY, MBI, ETC) HAS RBUYDPT              
* INITIALIZED AS BLANK. CURRENT MAKEGOOD BUYS CREATED THROUGH                   
* MAKEGOOD APPLY DID NOT INITIALIZED THIS. SUBSEQUENTLY, COMPARSIONS            
* FOR UPDATES WILL THINK THAT THE BUY HAS CHANGED. THEREFORE, WE'LL             
* INITIALIZE THE DAYPART FIELD HERE FOR THE NEW MAKEGOOD BUYS. CURRENT          
* MAKEGOOD BUYS WILL BE CHECKED FOR BOTH NULL AND BLANK BYTE.                   
*                                                                               
         CLI   BUYD.RBUYDPT,0                                                   
         BNE   *+8                                                              
         MVI   BUYD.RBUYDPT,C' '                                                
*                                                                               
         MVC   BUYD.RBUYCREA,TODAY                                              
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* TAKE THE LATEST/HIGHEST VERSION NUMBER                                        
         TM    RCONSENF,X'20'                                                   
         BZ    UMGBUY10                                                         
*                                                                               
         ZIC   R1,RCONSRV          NEXT REP VERSION NUMBER                      
         LA    R1,2(R1)            SHOULD BE THE GREATER OF                     
         STC   R1,BUYD.RBUYVER     REP VERSION NUMBER + 2                       
         CLC   BUYD.RBUYVER,RCONSSV                                             
         BH    UMGBUYX             OR                                           
         ZIC   R1,RCONSSV                                                       
         LA    R1,1(R1)            STA VERSION NUMBER + 1                       
         STC   R1,BUYD.RBUYVER                                                  
         B     UMGBUYX                                                          
*                                                                               
UMGBUY10 DS    0H                  REP VERSION ADVANCED                         
         CLC   RCONSRV,BUYD.RBUYVER                                             
         BE    UMGBUYX                                                          
         MVC   BUYD.RBUYVER,RCONSRV                                             
         DROP  R6,BUYD                                                          
*                                                                               
UMGBUYX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* IF TRADE CONTRACT, SET BUY TRADE FLAG                                         
***********************************************************************         
SETTRADE NTR1                                                                   
BUYD     USING RBUYREC,RBUYREC                                                  
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONRFEL,R6                                                      
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   SETTRX                                                           
                                                                                
         TM    RCONRF1,X'08'       TRADE ORDER?                                 
         BZ    SETTRX                                                           
*                                                                               
         OI    BUYD.RBUYFLG2,X'02' TRADE BUY                                    
         DROP  R6,BUYD                                                          
*                                                                               
SETTRX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK IF ANY 'OR' MAKEGOODS OFFERS IN A GROUP. IF FOUND, MAKE SURE            
* AT LEAST ONE RECORD WITHIN THE OFFER HAS BEEN SELECTED                        
*                                                                               
* ALSO PROCESS REJECT/RECALL BY UPDATING FLAGS IN HEADER REC                    
* AND REFRESH LIST WITH NEW STATUS                                              
*                                                                               
* NOTE THAT 'R' CAN BE REJECT OR RECALL DEPENDING ON WHO THE                    
* OFFERER IS AND WHO IS CURRENTLY LOOKING AT THE OFFER                          
***********************************************************************         
CHECKMG  NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         TM    RCONMODR+1,X'80'    IF 'ACE' THERE ARE SOME                      
         BZ    CHKMG02             SPECIAL TESTS TO BE DONE                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
*                                                                               
* CAN'T MAKE CHANGES IF OTHER SIDE IS IN PROCESS OF MAKING CHANGES              
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    CHKMG01                                                          
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    CHKMGNO                                                          
         B     CHKMG02                                                          
*                                                                               
CHKMG01  EQU   *                                                                
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         TM    RCONSENF,X'20'      X'20'=REP VERS. NOT ADVANCED                 
         BZ    CHKMGNO                                                          
         DROP  R6                                                               
*                                                                               
CHKMG02  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),TWAMKGD2  SET TO HEADER RECORD                         
                                                                                
*                                                                               
* CHECK OFFER STATUS IN GROUP COMMENT REC                                       
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         GOTOR MKGSAV,DMCB,RMKGREC SAVE STARTING MKG VALUES                     
*                                                                               
*                                  CHECK IF:                                    
         LA    R3,480              ERROR IF                                     
         TM    RMKGSCST,RMKGSAPQ   ALREADY APPLIED                              
         BO    CHKMGNO             ELSE,                                        
         CLI   8(R2),C'A'          APPLY?                                       
         BE    CHKMG140                                                         
         CLI   8(R2),C'R'          REJECT/RECALL?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG05                                                          
*                                                                               
* USER IS REP. IF MG IS A DARE MG AND MG WAS SENT TO AGENCY, WE MUST            
* CANCEL OR RECALL FROM AGENCY FIRST                                            
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG03                                                          
         LA    R3,588                                                           
         TM    RMKGSFG1,RMGF1MRR+RMGF1MCN+RMGF1MCM                              
         BZ    CHKMGNO                                                          
*                                                                               
CHKMG03  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BO    CHKMG50             OFFERER IS REP: RECALL                       
         B     CHKMG10             OFFERER IS STA: REJECT                       
*                                                                               
CHKMG05  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BZ    CHKMG50             OFFERER IS STA: RECALL                       
*                                  OFFERER IS REP: REJECT                       
         EJECT                                                                  
*                                                                               
* FOR ACTION REJECT, ONLY THE OFFEREE CAN REJECT AND OFFERER IS NOT             
* WORKING ON THE OFFER                                                          
*                                                                               
CHKMG10  DS    0H                                                               
         LA    R3,481                                                           
         TM    RMKGSCST,RMKGSRJQ   ALREADY REJECTED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,482                                                           
         TM    RMKGSCST,RMKGSRCQ   ALREADY RECALLED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,501              OFFERER CANNOT REJECT A MGO                  
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG20                                                          
*                                                                               
* IF DARE, CURRENT DARE STATUS MUST BE CANCELLED OR RECALLED OR                 
* REJECTED FROM AGENCY                                                          
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG15                                                          
         TM    RMKGSFG1,RMGF1MCN+RMGF1MCM+RMGF1MRR                              
         BNZ   CHKMG15                                                          
         LA    R3,588                                                           
         B     CHKMGNO                                                          
*                                                                               
CHKMG15  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BO    CHKMGNO             MUST BE OFFERED BY STATION                   
*                                                                               
         LA    R3,628              STATION MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
         B     CHKMG40                                                          
*                                                                               
CHKMG20  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BZ    CHKMGNO             MUST BE OFFERED BY REP                       
*                                                                               
         LA    R3,629              REP MUST NOT BE WORKING ON THIS              
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
*                                                                               
CHKMG40  DS    0H                                                               
         OI    RMKGSCST,RMKGSRJQ   MARK OFFER REJECTED                          
*                                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   12(3,RF),=C'REJ'     REFRESH SCREEN STATUS                       
         OI    6(RF),X'80'         XMIT                                         
*                                                                               
         B     CHKMG100                                                         
         EJECT                                                                  
*                                                                               
* FOR ACTION RECALL, ONLY THE OFFERER CAN RECALL AND OFFEREE MUST               
* NOT BE WORKING ON THE OFFER                                                   
*                                                                               
CHKMG50  DS    0H                                                               
         LA    R3,481                                                           
         TM    RMKGSCST,RMKGSRJQ   ALREADY REJECTED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,482                                                           
         TM    RMKGSCST,RMKGSRCQ   ALREADY RECALLED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,500              ONLY OFFERER CAN RECALL MGO                  
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG60                                                          
*                                                                               
* IF DARE, CURRENT DARE STATUS MUST BE CANCELLED OR RECALLED OR                 
* REJECTED FROM AGENCY                                                          
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG55                                                          
         TM    RMKGSFG1,RMGF1MCN+RMGF1MCM+RMGF1MRR                              
         BNZ   CHKMG55                                                          
         LA    R3,588                                                           
         B     CHKMGNO                                                          
*                                                                               
CHKMG55  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BZ    CHKMGNO             MUST BE OFFERED BY REP                       
*                                                                               
         LA    R3,628              AND STA MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
         B     CHKMG70                                                          
*                                                                               
CHKMG60  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BO    CHKMGNO             MUST BE OFFERED BY STATION                   
*                                                                               
         LA    R3,629              AND STA MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
*                                                                               
CHKMG70  DS    0H                  MARK OFFER RECALLED                          
         OI    RMKGSCST,RMKGSRCQ                                                
*                                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   12(3,RF),=C'REC'     REFRESH SCREEN STATUS                       
         OI    6(RF),X'80'         XMIT                                         
*                                                                               
CHKMG100 DS    0H                  RECORD RECALL/REJECT DATE/TIME               
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGSLRD)                                   
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         LA    R3,RMKGSLRT                                                      
         STCM  R0,4,1(R3)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R3)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R3),2,0                                      
*                                                                               
* RECORD LAST DATE/TIME ACTIVITY ALSO                                           
*                                                                               
         MVC   RMKGSLAD,RMKGSLRD                                                
         MVC   RMKGSLAT,RMKGSLRT                                                
*                                                                               
         GOTO1 VPUTREC,DMCB,RMKGREC WRITE OUT HEADER RECORD                     
         B     CHKMGYES                                                         
         EJECT                                                                  
*                                                                               
* CHECK FOR ACTION APPLY                                                        
*                                                                               
CHKMG140 DS    0H                  ONLY REP CAN APPLY                           
         LA    R3,505                                                           
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMGNO                                                          
*                                                                               
         LA    R6,RCONREC          FOR A DARE ORDER                             
         MVI   ELCODE,X'1D'        REP CAN APPLY APPROVAL RECEIVED FROM         
         BRAS  RE,GETEL            AGENCY                                       
         BNE   CHKMG143            OR REP CAN AUTO-APPLY THROUGH DARE           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'02'      OK TO APPLY FOR EDI ONE-SHOT                 
         BO    CHKMG143                                                         
*                                                                               
* IN WHICH CASE THE GROUP IS MARKED AS APPROVED                                 
*                                                                               
         OC    RMKGSFG1,RMKGSFG1   NEVER SENT TO AGENCY                         
         BZ    CHKMG143            OK TO APPLY                                  
*                                                                               
*        TM    RCONDRFG,X'80'      IF LINKED TO DARE ORDER                      
*        BZ    CHKMG142            MUST APPLY THRU DARE                         
         OC    RCONDRLK,RCONDRLK                                                
         BZ    CHKMG142                                                         
*                                                                               
         TM    TWADARE,X'02'       APPLY FROM DARE?                             
         BO    CHKMG142            YES                                          
         LA    R3,616                                                           
         B     CHKMGNO                                                          
         DROP  R6                                                               
*                                                                               
* IN ADDITION TO APPROVAL RECEIVED, SELF APPLY IF SENT OR RESENT                
*                                                                               
CHKMG142 DS    0H                                                               
         LA    R3,520                                                           
         TM    RMKGSFG1,RMGF1MAR+RMGF1MCF+RMGF1MSN+RMGF1MCR                     
         BZ    CHKMGNO                                                          
*                                                                               
CHKMG143 DS    0H                                                               
         TM    RCONMODR+1,X'40'    AND GRAPHNET, REP CAN APPLY AT               
         BO    CHKMG145            ANYTIME                                      
*                                                                               
         LA    R3,512              WAS ORDER REJECTED/RECALLED?                 
         TM    RMKGSCST,RMKGSRJQ+RMKGSRCQ                                       
         BNZ   CHKMGNO             MUST REVISE BEFORE APPLY                     
*                                                                               
         LA    R3,513              IS STATION IN WIP?                           
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    ERROR                                                            
*                                                                               
CHKMG145 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC                                                  
         GOTO1 VHIGH                                                            
         B     CHKMG160                                                         
*                                                                               
CHKMG150 DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
CHKMG160 DS    0H                                                               
         LA    R3,466              UNSELECTED CHOICE OFFER DETECTED             
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   CHKMGYES            ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BZ    CHKMG150                                                         
         TM    RMKGKRTY,X'10'      FIND 'CHOICE' RECORDS                        
         BZ    CHKMG150                                                         
         MVC   MGOFFNUM,RMKGKRTY   SAVE OFFER NUMBER                            
         MVI   MGORSELD,C'N'       DEFAULT NO RECORD SELECTED IN OFFER          
         DROP  R6                                                               
                                                                                
CHKMG170 DS    0H                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'        GET STATUS ELEMENT                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    CHKMG180                                                         
         MVI   MGORSELD,C'Y'       YES, AT LEAST ONE HAS BEEN SELECTED          
         DROP  R6                                                               
*                                                                               
CHKMG180 DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BE    CHKMG190            ALL DONE                                     
         CLI   MGORSELD,C'N'       ANYTHING SELECTED FROM PREV OFFER?           
         BE    CHKMGNO                                                          
         B     CHKMGYES                                                         
                                                                                
CHKMG190 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         MVC   WORK(1),RMKGKRTY                                                 
         NC    WORK(1),MGOFFNUM                                                 
         DROP  R6                                                               
                                                                                
         TM    WORK,X'0F'          CHECK IF SAME OFFER                          
         BO    CHKMG200            YES, SAME OFFER                              
         CLI   MGORSELD,C'Y'       IF AT LEAST ONE REC WAS SELECTED             
         BE    CHKMG180            DO SEQ UNTIL NEXT OFFER                      
         B     CHKMG170            ELSE CHECK IF REC SELECTED                   
                                                                                
CHKMG200 DS    0H                  DIFFERENT OFFER                              
         CLI   MGORSELD,C'N'       ANY RECORDS SELECTED FROM PREV OFF?          
         BE    CHKMGNO                                                          
         B     CHKMG160            YES, RESET AND START AGAIN                   
*                                                                               
CHKMGYES SR    RC,RC                                                            
CHKMGNO  LTR   RC,RC                                                            
         XIT1  REGS=(R3)           SAVE POINTER TO ERROR MESSAGE                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET LINE NUMBER FOR NEW BUY                                                   
***********************************************************************         
FINDHI#  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(27),MGDTREC                                                  
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   FINDHX              NOT THERE?                                   
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         TM    RMKGRTS,X'10'       PREEMPT DOESN'T CREATE NEW LINES             
         BO    FINDHX              SO DON'T NEED TO CHECK THIS                  
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'      BUY KEY TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  R6                                                               
*                                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
*              FIND NEXT LINE NUMBER                                            
         XC    HALF,HALF           LINE NUMBER                                  
         GOTO1 VHIGH                                                            
*                                                                               
FINDHI10 CLC   KEY(22),KEYSAVE                                                  
         BNE   FINDHI30                                                         
         CLI   KEY+26,255          PLANREC?                                     
         BE    FINDHI20                                                         
         CLC   HALF+1(1),KEY+26                                                 
         BNL   *+10                                                             
         MVC   HALF+1(1),KEY+26    HIGHEST LINE NUMBER SO FAR                   
         SPACE 1                                                                
FINDHI20 OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     FINDHI10                                                         
*                                                                               
FINDHI30 LH    RE,HALF             LAST LINE NUMBER                             
         LA    RE,1(RE)                                                         
         CH    RE,=H'254'                                                       
         BNH   *+16                                                             
         LA    R2,CONCACTH                                                      
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
*                                                                               
         STC   RE,HIGHBUY#         BUY LINE NUMBER                              
*                                                                               
FINDHX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE MAKEGOOD HEADER RECORD                                                 
***********************************************************************         
UPDTHDR  NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),TWAMKGDH  GET MKG HEADER REC                           
         DROP  R4                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
IOD      USING RMKGSDEM,R6                                                      
         OI    IOD.RMKGSCST,RMKGSAPQ MARK APPIED, CLEAR ALL ELSE                
         NI    IOD.RMKGSCST,X'FF'-RMKGSBOQ-RMKGSRCQ-RMKGSRJQ-RMKGSRVQ           
*                                                                               
* IF LAST DARE STATUS IS SENT OR RESENT, THIS IS A SELF APPLY                   
*                                                                               
         TM    IOD.RMKGSFG1,RMGF1MSN+RMGF1MCR                                   
         BZ    *+8                                                              
         OI    IOD.RMKGSFG3,RMGF3SAQ   MARK SELF APPLY                          
*                                                                               
         MVC   IOD.RMKGAPMN,RCONMOD RECORD CURRENT MOD NUM                      
*                                                                               
* GET LAST ACTIVITY DATE/TIME                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,IOD.RMKGSLAD)                               
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         LA    R4,IOD.RMKGSLAT                                                  
         STCM  R0,4,1(R4)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R4)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R4),2,0                                      
         DROP  IOD                                                              
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* IF DARE, SEND CONFIRMATION TO AGENCY                                          
***********************************************************************         
DARECNF  NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         MVC   KEY+28(4),TWAMKGDH  SET TO GET HEADER RECORD                     
         DROP  R4                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         OC    RMKGSFG1,RMKGSFG1   NEVER SENT TO AGENCY                         
         BZ    DCNFX               DON'T NEED TO SEND ROK MESSAGE               
         TM    RMKGSFG3,RMGF3SAQ   SELF APPLY?                                  
         BO    DCNF10              DON'T SEND MKGROK TO AGENCY                  
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC          FOR A DARE ORDER                             
         MVI   ELCODE,X'1D'        REP CAN APPLY APPROVAL RECEIVED FROM         
         BRAS  RE,GETEL            AGENCY                                       
         BNE   DCNFX               OR REP CAN AUTO-APPLY THROUGH DARE           
*                                                                               
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'80'+X'01'   MUST BE LINKED TO DARE ORDER              
*        BZ    DCNFX               OR TAKEOVER DARE CONTRACT                    
         OC    RCONDRLK,RCONDRLK                                                
         BZ    DCNFX                                                            
         DROP  R6                                                               
*                                                                               
         L     RE,ASPULAR                                                       
         XCEF  (RE),6500                                                        
         BAS   RE,INITIAL                                                       
         BAS   RE,OPENPQ                                                        
         BAS   RE,EDICT                                                         
         BAS   RE,MKGROK                                                        
*                                                                               
DCNF10   DS    0H                                                               
         BAS   RE,DAREHIST         UPDATE DARE MAKEGOOD HISTORY                 
*                                                                               
DCNFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
*              INITIALIZATION                                                   
***********************************************************************         
INITIAL  NTR1                                                                   
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
         SPACE 2                                                                
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
OPENPQ   NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
         MVC   PLSUBID,=C'DMG'                                                  
         MVC   PLDESC,=CL11'MAKEGOOD'                                           
         MVI   PLCLASS,C'G'                                                     
         DROP  R3                                                               
                                                                                
VPQ20    DS    0H                                                               
*                                                                               
         LA    RE,TWASPKEY                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'XX'                                                     
         DROP  RE,R4                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
***********************************************************************         
EDICT    NTR1                                                                   
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,=C'MKG'     TYPE=MAKEGOOD                                
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         DROP  R3                                                               
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE DARE MAKEGOOD HISTORY                                                  
***********************************************************************         
DAREHIST NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),TWAMKGDH                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
IOD      USING RMKGSDEM,R6                                                      
*                                                                               
         MVI   IOD.RMKGSFG1,RMGF1MCF MARK APPIED, CLEAR ALL ELSE                
*                                                                               
         XC    WORK,WORK                                                        
                                                                                
         LA    R5,WORK                                                          
         USING RMKGATEM,R5                                                      
         MVI   RMKGATCD,X'02'      ELEMENT CODE                                 
         MVI   RMKGATLN,RMKGATLQ   LENGTH                                       
         MVC   RMKGATVR,IOD.RMKGSCVR SAVE OFF VERSION#/ACTION                   
         MVI   RMKGATAT,X'08'      SAVE OFF VERSION#/ACTION                     
* TODAY                                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGATDT)                                   
                                                                                
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,RMKGATTM                                                    
*                                                                               
         TM    IOD.RMKGSFG3,RMGF3SAQ                                            
         BZ    DHIS10              SELF APPLY?                                  
         MVI   RMKGATLN,RMKGAL3Q   LENGTH                                       
         OI    RMKGATFG,X'80'                                                   
         MVC   RMKGABNM,TWAMGBYR   BUYER NAME                                   
         DROP  R4,R5,IOD                                                        
                                                                                
DHIS10   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,IOAREA,WORK                                        
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* SEND MAKEGOOD OK NOTICE -- IGNORE AGENCY OFFICE                               
***********************************************************************         
MKGROK   NTR1                                                                   
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         NI    MSFLAG,X'FF'-MSFACTIV                                            
*                                                                               
MKGROK05 DS    0H                                                               
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RDARKTYP,X'51'                                                   
         TM    MSFLAG,MSFACTIV                                                  
         BZ    *+8                                                              
         MVI   RDARKTYP,X'41'                                                   
*                                                                               
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,=20C' '                                                 
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    MKGROK25                                                         
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     MKGROK10                                                         
*                                                                               
** PREP KEY FOR SKIP READING: SKIP TO NEXT AGENCY OFFICE IF AGENCY              
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
MKGROK08 CLC   RDARKAOF,PRVKEY.RDARKAOF DID AGENCY OFFICE CHANGE?               
         DROP  PRVKEY                                                           
         BNE   MKGROK09              YES -- DON'T INCREMENT                     
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
MKGROK09 XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
MKGROK10 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   MKGROK11                                                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   MKGROK11              NO -- NEXT AGENCY RECORD                   
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   MKGROK08              NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    MKGROK20              YES -- DARE RECORD BUILT...                
         B     MKGROK25                                                         
         DROP  R6                                                               
*                                                                               
MKGROK11 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
MKGROK12 LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    MKGROK15              YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,MKGROK12           CHECK NEXT EQUIVALENCY CODE                
         B     MKGROK15                                                         
*                                                                               
         MVC   RDARKAGY,0(R4)        EQUIVALENCY CODE                           
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,MKGROK10                                                      
*                                                                               
* THE CONFIRMED ORDER WAS NOT FOUND, BEFORE CHECKING THE TAKEOVER               
* ELEMENT, CHECK IF AGENCY HAS SENT OVER THE ORDER BY LOOKING FOR               
* X'41' DARE ORDER RECORD                                                       
*                                                                               
MKGROK15 DS    0H                                                               
         TM    MSFLAG,MSFACTIV       CHECK ACTIVE X'41' RECORD                  
         BO    MKGROK25                                                         
         OI    MSFLAG,MSFACTIV                                                  
         B     MKGROK05                                                         
*                                                                               
MKGROK20 DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
         L     R5,AIO4                                                          
         B     MKGROK28            USE USER SIGNON AS RECEIVER ID               
*                                                                               
*                                                                               
* CHECK IF TAKEOVER CONTRACT/DARE                                               
*                                                                               
MKGROK25 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE! DARE CONFIRM HEADER NOT FOUND           
         USING RCONTKEL,R6                                                      
*                                                                               
         L     R5,AIO4                                                          
         USING RDARREC,R5                                                       
*                                                                               
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         MVC   RDARKAGY,RCONTKAR   AGENCY ROUTING (FOR JDS)                     
         MVC   RDARSNDR,RCONTKRC   SENDER ID                                    
         MVC   RDARRTS,RCONTKRT    AGENCY RETURN TO SENDER INFO                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         MVC   RDARKORD,RCONDRLK                                                
         DROP  R6                                                               
*                                                                               
* GET RECEIVER ID                                                               
*                                                                               
MKGROK28 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,28(R6)                                                        
MKGROK30 CLI   0(R6),X'02'         GET REP SIGN-ON ID                           
         BE    MKGROK40                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   MKGROK30                                                         
         DC    H'0'                                                             
*                                                                               
MKGROK40 DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDARRCVR(0),2(R6)                                                
         OC    RDARRCVR,=20C' '                                                 
         DROP  R5                                                               
*                                                                               
MKGROK50 DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCFMD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCFTID,=C'MKGROK'                                               
                                                                                
* ORDER NUMBER                                                                  
         OC    RDARKORD,RDARKORD                                                
         BNZ   *+6                                                              
         DC    H'0'                DID NOT FIND HEADER REC, DIE!!               
*                                                                               
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,MOCFORDR),FILL=0                                   
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCFFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCFTOID,RDARSNDR                                                
                                                                                
* ROUTING CODE                                                                  
         MVC   MOCFROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCFDATE)                               
                                                                                
* CURRENT TIME                                                                  
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,MOCFTIME,2,0                                    
*                                                                               
* STATION                                                                       
         MVC   MOCFQSTA,RDARKSTA                                                
         CLI   MOCFQSTA+4,C'L'                                                  
         BE    MKGROK60                                                         
         MVI   MOCFQSTA+5,C'V'     TV OR RADIO?                                 
         CLI   MOCFQSTA+4,C'T'                                                  
         BE    *+8                                                              
         MVI   MOCFQSTA+5,C'M'                                                  
                                                                                
* CONTRACT NUMBER                                                               
MKGROK60 DS    0H                                                               
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RCONKCON                                                
         EDIT  (P5,WORK2),(8,MOCFRPCN),FILL=0                                   
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCFRTNS,RDARRTS                                                 
         DROP  R6                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGDH  SET TO GET HEADER RECORD                     
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOCFOFRI(2),RMKGKGR1                                             
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOCFSEQN),FILL=0                                     
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ AND EXIT                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*---------------------------------------------------------------------*         
TRUDATE  NTR1                                                                   
*                                                                               
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     EXXMOD                                                           
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPDARMKGDD                                                     
       ++INCLUDE REGENDAR                                                       
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
* LOCAL VARIABLES                                                               
MYWORKD  DSECT                                                                  
RELO     DS    A                                                                
PERVAL   DS    V                                                                
VTSAR    DS    V                                                                
MKGKEY   DS    CL27                                                             
HIGHBUY# DS    X                                                                
MGOFFNUM DS    X                                                                
MGORSELD DS    C                   'OR' MAKEGOOD OFFER AT LEAST 1 SELTD         
TARGET#  DS    X                                                                
MISSDATE DS    XL3                                                              
*                                                                               
MSSTDT   DS    CL8                 BUY GRID START DATE                          
         DS    CL1                 SEPARATOR                                    
MSSTDT2  DS    CL8                 USER INPUT START DATE                        
         DS    CL1                 SEPARATOR                                    
MSSTDT3  DS    CL8                 USER INPUT END DATE                          
MSNUMWK  DS    X                   NUMBER OF WEEKS FOR THIS CREDIT              
MSFLAG   DS    X                                                                
MSFALTQ  EQU   X'80'               ALTERNATING WEEKS                            
MSFACTIV EQU   X'40'               CHECK X'41' ACTIVE KEY                       
MSF1PASS EQU   X'20'               ADD ZERO X'56' ONLY ON FIRST PASS            
MSBUYEL  DS    XL11                BUY ELEMENT BUILD AREA                       
MSENDDAY DS    X                                                                
*                                                                               
MGDEMCAT DS    XL3                 MAKEGOOD OFFER DEMO CATEGORY                 
CDEMCAT  DS    XL3                 CONTRACT DEMO CATEGORY                       
*                                                                               
TSARBLK  DS    XL256                                                            
ELEM     DS    XL256                                                            
SIBELEM  DS    XL256               MAKEGOOD SIBLING ELEMENT                     
MGDTREC  DS    XL1000              FIRST MAKEGOOD DETAIL RECORD                 
TSARREC  DS    XL1002                                                           
MKGHDR   DS    XL1002              MAKEGOOD HEADER RECORD                       
MYWORKX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T80237   CSECT                                                                  
*                                                                               
MGOCOUNT NTR1  BASE=*,LABEL=*      UPDATE # OF MAKEGOOD OFFERS COUNT            
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         LA    R6,RCONREC+FRSTELEM                                              
         USING RCONRFEL,R6                                                      
         MVI   ELCODE,X'1E'        LOOKING FOR RANDOM FLAGS ELEMENT             
         CLC   ELCODE,0(R6)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   MGOCNT20                                                         
*                                                                               
         L     R1,AFACILS                                                       
         L     RE,0(R1)                                                         
         USING TIOBD,RE                                                         
         LLC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         JNH   *+8                                                              
         AHI   R0,-12              PFKEY                                        
         DROP  RE                                                               
*                                                                               
         CHI   R0,0                APPLIED USING PF KEY?                        
         JNE   MGOCNT20                                                         
*                                                                               
         LLC   RE,RCONR#MO                                                      
         SHI   RE,1                BUMP DOWN COUNT BY 1                         
         STC   RE,RCONR#MO                                                      
         CHI   RE,0                                                             
         JH    *+8                                                              
         MVI   RCONR#MO,0          MAKE SURE COUNT IS NOT NEGATIVE              
         DROP  R6                                                               
*                                                                               
MGOCNT20 LA    R6,RCONREC+FRSTELEM                                              
         USING RCONMGEL,R6                                                      
         MVI   ELCODE,X'21'        LOOKING FOR MAKEGOOD OFFERS SEND ELM         
         CLC   ELCODE,0(R6)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   MGOCNT30                                                         
         OI    RCONMGFG,X'80'      MAKEGOOD OFFER APPLIED, SET TO WIP           
         DROP  R6,R4                                                            
*                                                                               
MGOCNT30 DS    0H                                                               
*                                                                               
MGOCNT_X XIT1                                                                   
*                                                                               
FRSTELEM EQU   RREPELEM-RREPREC    DISPLACEMENT TO FIRST REP REC ELEM           
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R6),0                                                          
         JNE   NXTEL                                                            
         LTR   R6,R6               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* INITIALIZE TSAR BUFFER                                                        
*                                                                               
INITTSAR NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACOMFACS         GET ADDRESS OF PERVAL                        
         USING COMFACSD,RE                                                      
         MVC   PERVAL,CPERVAL                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5D',0                                      
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         XC    TSARBLK,TSARBLK                                                  
         MVC   TD.TSACOM,ACOMFACS  A(COMFACS)                                   
         MVI   TD.TSPAGL,2         USE TEMPSTR PAGE 2                           
         MVI   TD.TSPAGN,3         USE 3 PAGES                                  
         MVI   TD.TSKEYL,27        KEY LENGTH                                   
         MVI   TD.TSRECI,TSRVAR    SET VARIABLE LENGTH RECORDS                  
         MVC   TD.TSRECL,=Y(1000)  MAX RECORD LENGTH                            
         OI    TD.TSINDS,TSIXTTWA  14K RECORDS                                  
         MVI   TD.TSACTN,TSAINI    SET INITIALIZE TSAR                          
         LA    RF,TSARREC                                                       
         ST    RF,TD.TSAREC                                                     
*                                                                               
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEINIF                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ALSO INITIALIZE ALTERNATE BUCKET FLAGS                                        
*                                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
         TM    TWAFLAGS,X'08'      REP USING DAILY PACING?                      
         BNO   ISAR0020            YES                                          
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
ISAR0020 DS    0H                                                               
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FILL TSAR BUFFER WITH TARGET BUY RECORDS:                                     
* REMOVE X'66' MAKEGOOD OFFER REFERENCE ELEMENTS                                
*                                                                               
GETRECS  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    MGDTREC(27),MGDTREC                                              
         MVC   TWAMKGDH,TWAMKGD2   HAS HEADER REC D/A                           
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         MVC   KEY(27),RMKGREC                                                  
         GOTO1 VHIGH                                                            
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRECS05  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         CLI   MGDTREC,0           FIRST DETAIL RECORD SAVED ALREADY?           
         BNE   GRECS08                                                          
         GOTO1 VMOVEREC,DMCB,IOAREA,MGDTREC                                     
*                                                                               
GRECS08  DS    0H                                                               
         MVC   MKGKEY,IOAREA       TO REESTABLISH SEQ LATER                     
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'20'       BONUS OFFER?                                 
         BNZ   GRECSX                                                           
         DROP  R6                                                               
*                                                                               
* READ ORIGINAL TARGET BUYS TO TSAR BUFFER                                      
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
         MVC   TARGET#,RMKGMGLI    FIRST TARGET BUY NUMBER FOR ORDERING         
*                                                                               
GRECS10  DS    0H                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
GRECS20  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BUYD.RBUYKLIN,RMKGMGLI                                           
         BE    GRECS30                                                          
         GOTO1 VSEQ                                                             
         B     GRECS20                                                          
         DROP  BUYD                                                             
*                                                                               
GRECS30  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         BAS   RE,DELEL66          REMOVE X'66' ELEM FOR THIS OFFER             
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TD.TSERRS,TSEDUP                                                 
         BO    GRECS40             DO THE FOLLOWING ONLY ONCE PER BUY!          
*                                                                               
* REMOVE ORIGINAL BUY BUCKETS FROM THE CONTRACT RECORD                          
*                                                                               
         GOTO1 =A(BUCKUP),DMCB,(X'FF',RBUYREC),RR=RELO                          
*                                                                               
GRECS40  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   GRECS50                                                          
         CLC   RBUYKLIN,RMKGMGLI                                                
         BE    GRECS40             SKIP IF SAME TARGET                          
         B     GRECS10                                                          
         DROP  R6                                                               
*                                                                               
* IF PREEMPT, OR LATE RUN OFFER,                                                
* READ ALL OFFERS RECORDS SINCE EACH OFFER RECORD HAS                           
* ITS OWN TARGET LINE                                                           
*                                                                               
GRECS50  DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'+X'08'+X'04'                                        
         BZ    GRECSX                                                           
         TM    RMKGRTS,X'20'       SKIP LATE RUN BONUS                          
         BO    GRECSX                                                           
         MVC   KEY(27),MKGKEY                                                   
         GOTO1 VHIGH                                                            
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),MGDTREC                                    
         BE    GRECS05                                                          
*                                                                               
GRECSX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE APPROPRIATE X'66' ELEMENTS FROM BUY RECORD                             
* X'66' ELEMENT MUST MATCH CURRENT GROUP AND OFFER                              
***********************************************************************         
DELEL66  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   R4,ELCODE           SAVE TO PRESERVE CALLER'S NEXTEL             
*                                                                               
D6610    DS    0H                                                               
MGD      USING RMKGREC,MGDTREC                                                  
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   D66X                                                             
*                                                                               
D6620    DS    0H                                                               
         USING RBMGMSEL,R6                                                      
         CLC   MGD.RMKGKGRP,RBMGMSGD   CURRENT GROUP                            
         BNE   D6630                                                            
*        CLC   MGD.RMKGKLIN,RBMGMSR#+1 CURRENT OFFER                            
*        BNE   D6630                                                            
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),(R6),0                              
         B     D6610                                                            
*                                                                               
D6630    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    D6620                                                            
*                                                                               
D66X     DS    0H                                                               
         STC   R4,ELCODE           RESTORE                                      
         B     EXXMOD                                                           
         DROP  R6,MGD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF DEMO CATEGORY WAS CHANGED                                            
***********************************************************************         
CHKDEMO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         MVC   KEY(27),RMKGREC                                                  
         GOTO1 VHIGH                                                            
         DROP  R4                                                               
*                                                                               
CDEM10   DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   CDEMX                                                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CDEM10                                                           
         USING RMKGDMEL,R6                                                      
         LR    R5,R6               R5->REP DEMO ELEMENT IN BUY RECORD           
         MVC   MGDEMCAT,RMKGDMCT                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CDEM20                                                           
         USING RCONDDEL,R6                                                      
*                                                                               
         CLI   1(R6),18            OLD DEMO TYPE                                
         BE    CDEM20              SKIP                                         
         CLC   RCONDDCT,MYSPACES                                                
         BE    CDEM20                                                           
         NI    MGDEMCAT,X'FF'-X'40'                                             
         NI    RCONDDCT,X'FF'-X'40'                                             
         CLC   MGDEMCAT,RCONDDCT                                                
         BE    CDEM10                                                           
*                                                                               
* DELETE THE REP DEMO THAT IS DIFFERENT THAN THE CONTRACT DEMO                  
*                                                                               
         B     CDEM60                                                           
*                                                                               
CDEM20   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CDEMX                                                            
         USING RSARXEL,R6                                                       
*                                                                               
         MVC   CDEMCAT,RSARXDEM    DEFAULT                                      
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
         DROP  R6                                                               
*                                                                               
CDEM30   DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    CDEM40                                                           
         AHI   R3,3                                                             
         BCT   R4,CDEM30                                                        
         B     CDEM50                                                           
*                                                                               
CDEM40   DS    0H                                                               
         MVC   CDEMCAT,0(R3)       PRIMARY FOUND                                
*                                                                               
CDEM50   DS    0H                  TURN OFF PRIMARY FLAG ON BOTH                
         NI    CDEMCAT,X'FF'-X'40'                                              
         NI    MGDEMCAT,X'FF'-X'40'                                             
*                                                                               
         CLC   CDEMCAT,MGDEMCAT                                                 
         BE    CDEM10                                                           
*                                                                               
* DELETE THE REP DEMO THAT IS DIFFERENT THAN THE CONTRACT DEMO                  
*                                                                               
CDEM60   GOTO1 VRECUP,DMCB,(2,IOAREA),(R5),(R5),0                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         B     CDEM10                                                           
*                                                                               
CDEMX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DIRECT CREDIT PROCESSING                                                      
***********************************************************************         
CREDIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),MGDTREC                                                  
         GOTO1 VHIGH                                                            
         B     CRDT20                                                           
                                                                                
CRDT10   DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
CRDT20   DS    0H                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   CREDITX             ALL DONE                                     
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         TM    RMKGKRTY,X'F0'      ALL LINES?                                   
         BZ    CRDT30                                                           
         MVI   ELCODE,X'20'        IF CHOICE, GET STATUS ELEMENT                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    CRDT10                                                           
         DROP  R6                                                               
*                                                                               
* FROM MAKEGOOD'S X'05' MAKEGOOD REFERENCE ELEMENT, RETRIEVE                    
* TARGET BUYS IN TSAR                                                           
*                                                                               
CRDT30   DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
CRDT75   DS    0H                  RETRIEVE BUY TO BE CREDITED                  
         XCEFL TSARREC,1002                                                     
         MVI   TSARREC+2,X'0B'                                                  
         MVI   TD.TSACTN,TSARDH                                                 
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CRDT78   DS    0H                                                               
MISSBUYD USING RBUYKEY,TSARREC+2                                                
         CLC   MISSBUYD.RBUYKLIN,RMKGMGLI                                       
         BE    CRDT80                                                           
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    CRDT78                                                           
         DC    H'0'                                                             
*                                                                               
CRDT80   DS    0H                  BUILD FAKE SPLITOUT ELEMENT                  
         XC    ELEM,ELEM           TO PASS TO SPLIT03S                          
MGSD     USING RBYMGSEL,ELEM                                                    
         MVI   MGSD.RBYMGSCD,X'56'                                              
         MVI   MGSD.RBYMGSLN,7                                                  
         MVC   MGSD.RBYMGSDT,RMKGMGD1                                           
         MVC   MGSD.RBYMGSLI,RMKGKLIN                                           
         MVC   MGSD.RBYMGSSP,RMKGMGSP                                           
*                                                                               
* REMOVE MISSED SPOT FROM X'03' ELEMENTS                                        
*                                                                               
         LA    R3,1                                                             
*                                                                               
CRDT83   DS    0H                                                               
         GOTO1 =A(SPLIT03S),RR=RELO                                             
*                                                                               
* USER SPECIFIED RANGE OF DATES. LOOP AROUND UNTIL DONE                         
*                                                                               
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    CRDT85                                                           
*                                                                               
         LA    R4,7                                                             
         GOTO1 DATCON,DMCB,(3,MGSD.RBYMGSDT),(0,WORK)                           
         GOTO1 ADDAY,DMCB,WORK,WORK,(R4)                                        
         GOTO1 DATCON,DMCB,(0,WORK),(3,MGSD.RBYMGSDT)                           
         CLC   MGSD.RBYMGSDT,RMKGMGD2                                           
         BH    CRDT85                                                           
         AHI   R3,1                                                             
         B     CRDT83                                                           
         DROP  MGSD                                                             
*                                                                               
CRDT85   DS    0H                                                               
         XC    ELEM,ELEM           ADD CREDIT AUDIT TRAIL ELEMENT               
CRD      USING RBUYCAEL,ELEM                                                    
         MVI   CRD.RBUYCACD,RBUYCACQ                                            
         MVI   CRD.RBUYCALN,RBUYCALQ                                            
         MVC   CRD.RBUYCASD,RMKGMGD1                                            
         MVC   CRD.RBUYCAED,RMKGMGD2                                            
         STC   R3,CRD.RBUYCANW                                                  
         MVC   CRD.RBUYCASP,RMKGMGSP                                            
         GOTO1 DATCON,DMCB,(5,0),(2,CRD.RBUYCACT)                               
         TM    RMKGRTS,X'02'       FLAG IF CREDIT FOR NA SPOTS                  
         BZ    *+8                                                              
         OI    CRD.RBUYCAFL,X'40'                                               
         DROP  CRD                                                              
*                                                                               
         GOTO1 VADDELEM,DMCB,TSARREC+2,ELEM                                     
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CRDT90                                                           
         CLC   MISSBUYD.RBUYKLIN,RMKGMGLI                                       
         BE    CRDT80                                                           
         DC    H'0'                MUST BE FOR THE SAME TARGET!                 
         DROP  MISSBUYD,R6                                                      
*                                                                               
* UPDATE MOD CODE AND VERSION NUMBER                                            
*                                                                               
CRDT90   DS    0H                                                               
         GOTO1 =A(MGADDCDE),RR=RELO                                             
*                                                                               
* ADD REFERENCE TO OFFER WHICH THIS BUY WAS CREATED FROM                        
*                                                                               
*        GOTO1 =A(ADDMGREF),DMCB,TSARREC+2,RR=RELO                              
*                                                                               
* ALL DONE, WRITE RECORD BACK TO TSAR                                           
*                                                                               
BUYD     USING RBUYREC,TSARREC+2                                                
         SR    R1,R1                                                            
         ICM   R1,3,BUYD.RBUYLEN   ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD BACK                            
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    CRDT10                                                           
         DC    H'0'                                                             
         DROP  BUYD                                                             
*                                                                               
CREDITX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPLIT CURRENT X'03' ELEMENTS IN TARGET MISS BUY AS REFERENCED BY              
* THE X'56' MISSED ELEMENT (X'56' IS IN ELEM)                                   
***********************************************************************         
SPLIT03S NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* BUILD SPOT GRID FROM CURRENT X'03' ELEMENTS                                   
*                                                                               
         XC    WORK2,WORK2                                                      
*                                                                               
         BAS   RE,BLDGRID                                                       
*                                                                               
* REMOVE SPOTS FROM GRID                                                        
*                                                                               
         BAS   RE,REMGRID                                                       
*                                                                               
* REBUILD X'03' ELEMENT FROM GRID                                               
*                                                                               
         BAS   RE,BLDBUY                                                        
*                                                                               
SPLIT3X  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTRUCT A GRID BASE ON X'03' ELEMENTS IN THE BUY RECORD                     
* EACH WEEK CONSIST OF 2 1-BYTE ENTRIES WITH THE FIRST ENTRY THE NUMBER         
* OF SPOTS FOR THAT WEEK, AND THE SECOND BYTE FOR VARIOUS FLAGS                 
* (ZERO OVERRIDE, ALTERNATE, ETC)                                               
* THE START OF THE GRID IS THE FIRST START DATE OF THE BUY                      
***********************************************************************         
BLDGRID  NTR1                                                                   
*                                                                               
         LA    R3,818              NO TARGET DATE                               
         LA    R6,TSARREC+2                                                     
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RBUYDTEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,MSSTDT)                              
         LA    R3,WORK2                                                         
*                                                                               
BGRID05  DS    0H                                                               
         ZIC   R1,RBUYDTWK                                                      
         LTR   R1,R1               MIGHT BE OVERRIDED WITH ZERO                 
         BNZ   BGRID10                                                          
         LA    R1,1                AT LEAST ONE WEEK                            
*                                                                               
BGRID10  DS    0H                                                               
         MVC   0(1,R3),RBUYDTNW                                                 
         TM    RBUYDTIN,X'01'      NPW OVERRIDE??                               
         BZ    *+8                                                              
         OI    1(R3),X'01'                                                      
         LA    R3,2(R3)            BUMP TO NEXT WEEK                            
         TM    RBUYDTIN,X'40'      ALTERNATE WEEKS??                            
         BZ    *+8                                                              
         LA    R3,2(R3)            YES, BUMP ONE MORE                           
         BCT   R1,BGRID10          PROCESS FOR SPECIFIED NUMBER OF WKS          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   BGRIDX                                                           
*                                                                               
         MVI   MSSTDT+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,MSSTDT2)                             
*                                                                               
         GOTO1 PERVAL,DMCB,(17,MSSTDT),WORK2+120                                
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2+120                                                
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R3,WORK2                                                         
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R3,R1               BUMP TO START OF NEXT X'03' ELEMENT          
         B     BGRID05             IN THE GRID                                  
*                                                                               
BGRIDX   DS    0H                                                               
         B     SPLIT3X                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* REMOVE SPOTS FROM THE BUY GRID BUILT FROM ABOVE.                              
***********************************************************************         
REMGRID  NTR1                                                                   
*                                                                               
MGSD     USING RBYMGSEL,ELEM                                                    
         GOTO1 DATCON,DMCB,(3,MGSD.RBYMGSDT),(5,MSSTDT2)                        
*                                                                               
         MVI   MSSTDT+8,C'-'                                                    
         GOTO1 PERVAL,DMCB,(17,MSSTDT),WORK2+120                                
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FALL WITHIN EXISTING DATES              
*                                                                               
WKD      USING PERVALD,WORK2+120                                                
RGRID220 DS    0H                                                               
         ZICM  R4,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R4,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         MVI   MSNUMWK,1           DEFAULT ONE FOR NOW                          
*                                                                               
         LA    R2,WORK2                                                         
         SLL   R4,1                MULTIPLY BY 2                                
         AR    R2,R4               BUMP TO STARTING CELL FOR THIS DATE          
         ZIC   R4,MSNUMWK     NUMBER OF WEEKS TO PROCESS                        
*                                                                               
RGRID310 DS    0H                                                               
         LA    R3,866                                                           
         CLC   MGSD.RBYMGSSP,0(R2)                                              
         BH    ERROR                                                            
         ZIC   RE,0(R2)                                                         
         ZIC   RF,MGSD.RBYMGSSP                                                 
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         OI    1(R2),X'01'         IF ALL MISSEDD OUT, FLAG OVERRIDE            
*                                  SO WE'LL REBUILD THIS DATE WITH (0)          
         AHI   R2,2                                                             
         TM    MSFLAG,MSFALTQ                                                   
         BZ    *+8                 ALTERNATING WEEKS BUMPS ONE MORE             
         AHI   R2,2                                                             
         BCT   R4,RGRID310                                                      
*                                                                               
         B     SPLIT3X                                                          
         DROP  MGSD                                                             
         EJECT                                                                  
***********************************************************************         
* REBUILD X'03' ELEMENT FROM BUY GRID                                           
***********************************************************************         
BLDBUY   NTR1                                                                   
         LA    R6,TSARREC+2                                                     
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BBUY03   DS    0H                  POINT TO LAST X'02' ELEMENT TO GET           
         LR    R2,R6               END DAY                                      
         BRAS  RE,NEXTEL                                                        
         BE    BBUY03                                                           
         LR    R6,R2                                                            
         USING RBUYDYEL,R6                                                      
         MVI   MSENDDAY,0     GET END DAY                                       
         MVN   MSENDDAY,RBUYDYIN                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,TSARREC+2                                                     
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6         SAVE OFF BUY START DATE                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,MSSTDT)                              
         DROP  R6                                                               
*                                                                               
* DELETE OLD X'03'S                                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'03',TSARREC+2)                                  
*                                                                               
         LA    R6,WORK2                                                         
*                                                                               
         SR    R3,R3                                                            
*                                                                               
TBUYD    USING RBUYREC,TSARREC+2                                                
*                                                                               
BBUY05   DS    0H                                                               
         LA    R4,1                                                             
         XC    MSBUYEL,MSBUYEL                                                  
WKD      USING RBUYDTEL,MSBUYEL                                                 
*                                                                               
         MVI   WKD.RBUYDTCD,X'03'                                               
         MVI   WKD.RBUYDTLN,11                                                  
*                                                                               
BBUY08   DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   BBUY10                                                           
         TM    1(R6),X'01'         OVERRIDE??                                   
         BZ    BBUY40                                                           
*                                                                               
BBUY10   DS    0H                                                               
         OC    WKD.RBUYDTST,WKD.RBUYDTST                                        
         BNZ   BBUY20              NEED TO FIND START DATE                      
         LTR   R3,R3                                                            
         BNZ   BBUY15                                                           
         MVC   WKD.RBUYDTST,MSSTDT                                              
         MVC   MSSTDT2,MSSTDT                                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT),(3,WKD.RBUYDTST)                          
         B     BBUY20                                                           
*                                                                               
BBUY15   DS    0H                  SUBSEQUENT X'03'S NEED TO COMPUTE            
         LR    R2,R3               START DATES RELATIVE TO INITIAL              
         MHI   R2,7                FLIGHT START FOR THIS BUY                    
         GOTO1 ADDAY,DMCB,MSSTDT,MSSTDT2,(R2)                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTST)                         
*                                                                               
BBUY20   DS    0H                                                               
         CLC   0(1,R6),2(R6)                                                    
         BNE   BBUY23                                                           
         CLI   0(R6),0             INCASE ALL SPOTS CREDITED OUT FOR            
         BNE   BBUY21              THIS WEEK, CHECK IF ZERO SPOT                
         TM    3(R6),X'01'         OVERRIDE FLAG IS SET                         
         BZ    BBUY23                                                           
*                                                                               
BBUY21   DS    0H                                                               
         LA    R6,2(R6)                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         CH    R3,=H'53'                                                        
         BL    BBUY08              BUMP TO NEXT CELL                            
*                                                                               
BBUY23   DS    0H                                                               
         MVC   WKD.RBUYDTNW,0(R6)                                               
         STC   R4,WKD.RBUYDTWK                                                  
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         OI    WKD.RBUYDTIN,X'80'  DEFAULT IS WEEKLY                            
         TM    1(R6),X'01'         OVERRIDE??                                   
         BZ    *+8                                                              
         OI    WKD.RBUYDTIN,X'01'                                               
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         CLC   TBUYD.RBUYNW,0(R6)                                               
         BE    BBUY25                                                           
         OI    WKD.RBUYDTIN,X'01'                                               
         DROP  TBUYD                                                            
***********                                                                     
***********                                                                     
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
BBUY25   DS    0H                                                               
         ZIC   R2,WKD.RBUYDTWK                                                  
         MHI   R2,7                                                             
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
*                                                                               
         GOTO1 GETDAY,DMCB,MSSTDT2,FULL                                         
         ZIC   RE,MSENDDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    BBUY35                                                           
         BP    BBUY30                                                           
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     BBUY38                                                           
*                                                                               
BBUY30   DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     BBUY38                                                           
*                                                                               
BBUY35   DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
BBUY38   DS    0H                                                               
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTED)                         
*                                                                               
***********                                                                     
***********                                                                     
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,TSARREC+2,WKD.RBUYDTEL                             
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
BBUY40   DS    0H                                                               
         LA    R6,2(R6)                                                         
         LA    R3,1(R3)                                                         
         CH    R3,=H'53'                                                        
         BL    BBUY05              BUMP TO NEXT CELL                            
*                                                                               
BBUYX    DS    0H                                                               
         B     SPLIT3X                                                          
         DROP  WKD                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ADD X'69' ELEMENT SO WE KNOW WHICH OFFER THIS BUY CAME FROM                   
***********************************************************************         
ADDMGREF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,0(R1)                                                         
         LR    R3,R6                                                            
         MVI   ELCODE,X'69'        REFERENCE ELEMENT?                           
         BAS   RE,GETEL                                                         
         BNE   ADDRF20                                                          
ADDRF10  CLC   MKGKEY+RMKGKGR1-RMKGKEY(2),2(R6)                                 
         BE    ADDRFX              DON'T ADD DUPLICATES                         
         BAS   RE,NEXTEL                                                        
         BE    ADDRF10                                                          
*                                                                               
ADDRF20  DS    0H                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'69'                                                       
         MVI   WORK+1,4                                                         
         MVC   WORK+2(2),MKGKEY+RMKGKGR1-RMKGKEY                                
         GOTO1 VADDELEM,DMCB,(R3),WORK                                          
*                                                                               
ADDRFX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
* DUMP RECORDS FROM TSAR TO FILE                                                
*                                                                               
DUMPTSAR NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY TO WRITE BACK             
         LA    RF,TSARREC                                                       
         ST    RF,TD.TSAREC                                                     
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'     START WITH BUY                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ADD CHANGED/NEW BUY BUCKETS TO THE CONTRACT RECORD                            
*                                                                               
DUMP10   DS    0H                                                               
         GOTO1 =A(BUCKUP),DMCB,TSARREC+2,RR=RELO                                
*                                                                               
* UPDATE/ADD TARGET BUY RECORD                                                  
*                                                                               
         MVC   KEY(27),TSARREC+2                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DUMP30                                                           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         B     DUMP50                                                           
*                                                                               
* NEW BUY, ADD                                                                  
*                                                                               
DUMP30   DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         BAS   RE,ADDSIBEL         ADD SIBLING ELEMENT                          
         GOTO1 VADDREC,DMCB,RBUYREC                                             
*                                                                               
DUMP50   DS    0H                                                               
         BAS   RE,XFERKEY          CHECK IF WE NEED TO BUILD X'9B'              
*                                  FOR REP TO SPOT TRANSFER                     
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 VTSAR,TSARBLK                                                    
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    DUMP10                                                           
*                                                                               
DUMPX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* CHECK IF MAKEGOOD BUY IS A MANY-FOR-MANY MAKEGOOD. IF YES, ADD X'26'          
* SIBLING ELEMENT.                                                              
**********************************************************************          
ADDSIBEL NTR1                                                                   
         TM    RBUYRTS,X'20'+X'10'+X'08'+X'04'                                  
         BNZ   ADDSIBX             MUST BE A 'REGULAR' MAKEGOOD                 
         CLI   SIBELEM+1,3                                                      
         BL    ADDSIBX                                                          
         MVI   SIBELEM,X'26'                                                    
         ZIC   R1,SIBELEM+1                                                     
         SHI   R1,2                OVERHEAD                                     
         LA    R3,SIBELEM+2                                                     
*                                                                               
ADDSIB10 DS    0H                                                               
         CLC   RBUYKLIN,0(R3)                                                   
         BE    ADDSIB20                                                         
         AHI   R3,1                                                             
         BCT   R1,ADDSIB10                                                      
         B     ADDSIBX                                                          
*                                                                               
ADDSIB20 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,SIBELEM                                    
*                                                                               
ADDSIBX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* CHECK IF WE NEED GENERATE THE X'9B' KEY FOR REP TO SPOT TRANSFER              
**********************************************************************          
XFERKEY  NTR1                                                                   
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXXMOD              NO SPOTPAK TRANSFER ELEM                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'9B'                                                        
         MVC   KEY+9(2),RCONKREP                                                
         MVC   KEY+11(4),RCONKADV                                               
         MVC   KEY+15(3),RCONPRD                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    KEYFOUND                                                         
         MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
         B     EXXMOD                                                           
*                                                                               
KEYFOUND DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         MVC   KEY,KEYSAVE         HAS CORRECT D/A & CTL BYTE                   
         GOTO1 VWRITE              UPDATE KEY                                   
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE UPDATES THE MOD CODE IN THE TARGET MISSED LINE FOR A             
* MAKEGOOD. THE TARGET MISSED BUY IS IN 'TSARREC+2'. ROUTINE IS LIFTED          
* FROM RECNT15                                                                  
**********************************************************************          
MGADDCDE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
BUYD     USING RBUYREC,TSARREC+2                                                
         MVC   BUYD.RBUYKMOD,RCONMOD K MOD NUM                                  
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    MGADDC20                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* TAKE THE LATEST/HIGHEST VERSION NUMBER                                        
         TM    RCONSENF,X'20'                                                   
         BZ    MGADDC10                                                         
*                                                                               
         ZIC   R1,RCONSRV          NEXT REP VERSION NUMBER                      
         LA    R1,2(R1)            SHOULD BE THE GREATER OF                     
         STC   R1,BUYD.RBUYVER     REP VERSION NUMBER + 2                       
         CLC   BUYD.RBUYVER,RCONSSV                                             
         BH    MGADDC30            OR                                           
         ZIC   R1,RCONSSV                                                       
         LA    R1,1(R1)            STA VERSION NUMBER + 1                       
         STC   R1,BUYD.RBUYVER                                                  
         B     MGADDC30                                                         
                                                                                
MGADDC10 DS    0H                  REP VERSION ADVANCED                         
         CLC   RCONSRV,BUYD.RBUYVER                                             
         BE    MGADDC40                                                         
         MVC   BUYD.RBUYVER,RCONSRV                                             
         B     MGADDC30                                                         
         DROP  R6                                                               
*                                                                               
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
*                                                                               
MGADDC20 DS    0H                                                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BE    MGADDC30                                                         
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BE    MGADDC30                                                         
* UPDATE MODIFICATION NUMBER IN TARGET MISSED LINE FOR NON-ACE/GRAPH            
* CONTRACT MOD NUMBER WILL BE UPDATED LATER BY ADDCODE                          
         ZIC   R1,BUYD.RBUYKMOD                                                 
         LA    R1,1(R1)                                                         
         STC   R1,BUYD.RBUYKMOD                                                 
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   BUYD.RBUYKMOD,0                                                  
         CLC   TODAY,BUYD.RBUYCHGD                                              
         BE    MGADDC40                                                         
                                                                                
MGADDC30 MVC   BUYD.RBUYCHGD,TODAY                                              
         MVC   BUYD.RBUYCHGI,=C'E ' MAKEGOOD CHANGE CODE                        
         B     MGADDCX                                                          
*                                                                               
* BUY ALREADY CHANGED TODAY                                                     
MGADDC40 CLI   BUYD.RBUYCHGI,C' '                                               
         BNE   MGADDC50                                                         
         MVI   BUYD.RBUYCHGI,C'E'                                               
         B     MGADDCX                                                          
                                                                                
MGADDC50 DS    0H                                                               
         CLI   BUYD.RBUYCHGI,C'*'                                               
         BE    MGADDCX                                                          
         CLI   BUYD.RBUYCHGI,C'E'                                               
         BE    MGADDCX                                                          
         CLI   BUYD.RBUYCHGI+1,C' '                                             
         BNE   MGADDC60                                                         
         MVI   BUYD.RBUYCHGI+1,C'E'                                             
         B     MGADDCX                                                          
                                                                                
MGADDC60 DS    0H                                                               
         CLI   BUYD.RBUYCHGI+1,C'E'                                             
         BE    MGADDCX                                                          
         MVC   BUYD.RBUYCHGI,=C'* ' MORE THAN 2                                 
*                                                                               
MGADDCX  XIT1                                                                   
         DROP  BUYD                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
*---------------------------------------------------------------------*         
BUCKUP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(BUYREC)                                    
         L     R0,VRECUP                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R5,TWABYTOT         CALCULATE A(WORKSPACE)                       
         AH    R5,=Y(TWABYTRD-TWABYTOT)                                         
         DROP  RF                                                               
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(R2),(BUCKFLGS,RCONREC),       +        
               ACOMFACS,VGTBROAD,(R0),(R5)                                      
         BE    EXXMOD                                                           
*                                                                               
         LA    R2,CONBACTH                                                      
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   MKGSAV:  SAVE DATA FROM ORIGINAL MAKEGOOD RECORD FOR UPDATING               
*        PASSIVE POINTERS                                                       
*                                                                               
MKGSAV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK           INIT KEYSAVE AREA                            
*                                                                               
         L     R2,0(R1)            POINT TO MAKEGOOD RECORD                     
         USING RMKGREC,R2          ESTABLISH MAKEGOOD RECORD KEY                
*                                                                               
*        READ MAKEGOOD HEADER                                                   
*                                                                               
         MVC   WORK(32),KEY        SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC     INSERT KEY OF SELECTED REC                   
*                                                                               
         OC    KEY+21(6),KEY+21    IF NULLS WE HAVE A MKG HEADER                
         BZ    OLD0A10                AND WE CAN USE IT                         
*                                                                               
         XC    KEY+21(6),KEY+21    CLEAR LOW ORDER OF KEY                       
*                                                                               
         GOTO1 VHIGH               READ KEY                                     
*                                                                               
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD FOUND?                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,MKGHDR   READ IN MAKEGOOD HEADER                    
*                                                                               
         LA    R2,MKGHDR           POINT TO MAKEGOOD HEADER                     
*                                                                               
OLD0A10  DS    0H                                                               
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         XC    SAVDAT,SAVDAT       FIRST OFFERED DATE                           
         XC    SAVWIP,SAVWIP       WIP STATUS                                   
         XC    SAVSTT,SAVSTT       OFFER STATUS                                 
         XC    SAVDST,SAVDST       DARE  STATUS                                 
         XC    SAVSAL,SAVSAL       SALESPERSON                                  
         XC    SAVTEM,SAVTEM       TEAM                                         
         XC    SAVADV,SAVADV       ADVERTISER                                   
         XC    SAVDSL,SAVDSL       DEVELOPMENTAL SALESPERSON                    
*                                                                               
         LA    R1,RMKGELEM         POINT TO FIRST ELM IN RECORD                 
*                                                                               
*        SAVE FIELDS FOR DELETING PASSIVES                                      
*                                                                               
         USING RMKGSDEM,R1         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   SAVDAT,RMKGFOFD     SAVE FIRST OFFERED DATE                      
         MVC   SAVWIP,RMKGSFG2     WIP STATUS                                   
         MVC   SAVSTT,RMKGSCST     OFFER STATUS                                 
         NI    SAVSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                            
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    SAVSTT,RMKGSLFQ        SET INDICATOR                             
*                                                                               
         MVC   SAVDST,RMKGSFG1     DARE  STATUS                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0020 EQU   *                                                                
*                                                                               
         CLI   0(R1),0             END OF RECORD?                               
         BE    OLDA0100            YES - FINISHED - NO ELT                      
*                                                                               
         CLI   0(R1),X'0A'         0A ELT?                                      
         BE    OLDA0040            YES - PROCESS IT                             
*                                                                               
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     OLDA0020            GO BACK FOR NEXT                             
*                                                                               
OLDA0040 EQU   *                                                                
*                                                                               
         USING RMKGXEL,R1                                                       
*                                                                               
*        SAVE FIELDS FOR PASSIVES                                               
*                                                                               
         MVC   SAVSAL,RMKGXSAL     SALESPERSON                                  
         MVC   SAVTEM,RMKGXTEM     TEAM                                         
         MVC   SAVADV,RMKGXADV     ADVERTISER                                   
         MVC   SAVDSL,RMKGXDSP     DEVELOPMENTAL SALESPERSON                    
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0100 EQU   *                                                                
*                                                                               
         MVC   KEY,WORK            RESTORE INCOMING KEY                         
*                                                                               
         GOTO1 VHIGH               RESTORE FILE POINTERS                        
*                                                                               
MKGSAVX  DS    0H                                                               
         XIT1                                                                   
*--->>                                                                          
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PASSUPD :  UPDATE PASSIVE POINTERS                                          
*                                                                               
PASSUPD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   DELETE A0 PASSIVE POINTERS                                                  
*                                                                               
         L     R2,0(R1)            POINT TO MAKEGOOD RECORD                     
         USING RMKGREC,R2          ESTABLISH RECORD                             
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE                     
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         MVC   27(1,R5),29(R2)     STATUS                                       
*                                                                               
         MVC   RMGSPKEY.RMGSPSAL,SAVSAL   INSERT S/P INTO KEY                   
         MVC   RMGSPKEY.RMGSPADV,SAVADV   ADVERTISER CODE                       
         MVC   RMGSPKEY.RMGSPDAT,SAVDAT   FIRST OFFERED DATE                    
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   SET WIP STATUS                        
         MVC   RMGSPKEY.RMGSPSTT,SAVSTT   OFFER STATUS                          
         MVC   RMGSPKEY.RMGSPDST,SAVDST   DARE  STATUS                          
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0520            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0540            PROCESS NEXT KEY                             
*                                                                               
PAWR0520 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
*                                                                               
PAWR0540 EQU   *                                                                
*                                                                               
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SALESPER                      
         BZ    PAWR0600                                                         
*                                                                               
         MVI   1(R5),X'02'         SET NEXT MG PASSIVE                          
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL INSERT DEV SALESPER                     
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0560            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0580            PROCESS NEXT KEY                             
*                                                                               
PAWR0560 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
*                                                                               
PAWR0580 EQU   *                                                                
*                                                                               
PAWR0600 DS    0H                                                               
*                                                                               
*        ADD NEW A0 PASSIVE POINTERS                                            
*                                                                               
         XC    KEY,KEY             CLEAR NEW KEY                                
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE                     
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)  COMPLEMENT                   
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         LA    RF,RMKGELEM                                                      
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
         USING RMKGSDEM,RF         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   RMGSPKEY.RMGSPDAT,RMKGFOFD RMGSPKEY.RMGSPE 1ST OFFERED           
         MVC   RMGSPKEY.RMGSPWIP,RMKGSFG2 WIP STATUS                            
         MVC   RMGSPKEY.RMGSPSTT,RMKGSCST OFFER STATUS                          
*                                                                               
         NI    RMGSPKEY.RMGSPSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                 
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    RMGSPKEY.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   RMGSPKEY.RMGSPDST,RMKGSFG1 DARE STATUS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASSUPDX            YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0240            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0220            GO BACK FOR NEXT                             
PASA0240 EQU   *                                                                
         USING RMKGXEL,RF                                                       
         MVC   RMGSPKEY.RMGSPSAL,RMKGXSAL SALESPERSON                           
         MVC   RMGSPKEY.RMGSPADV,RMKGXADV ADVERTISER                            
         MVC   SAVDSL,RMKGXDSP     SAVE DEVELOPMENTAL SALESPERSON               
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0260 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         MVC   KEY+28(4),TWAMKGDH  GET MKG HEADER REC                           
         DROP  R4                                                               
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0280            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0300            GO PROCESS NEXT KEY (IF ANY)                 
PASA0280 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VADD                ADD NEW KEY                                  
PASA0300 EQU   *                                                                
         CLI   KEY+1,X'02'         SECOND KEY PROCESSED?                        
         BE    PASA0320            YES - BOTH KEYS DONE                         
         MVI   KEY+1,X'02'         NO  - SET SECOND KEY TYPE                    
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL   USE DEV SALESPERSON                   
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SAL                           
         BZ    PASA0320                                                         
         B     PASA0260            GO BACK AND PROCESS                          
PASA0320 EQU   *                                                                
PASSUPDX DS    0H                                                               
         XIT1                                                                   
CHECKPAS EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110RECNT37   11/06/13'                                      
         END                                                                    
