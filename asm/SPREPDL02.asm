*          DATA SET SPREPDL02  AT LEVEL 056 AS OF 03/14/07                      
*PHASE SPDL02A                                                                  
*INCLUDE PRTREC                                                                 
*===========================================================*                   
* THIS VERSION SUPPORTS CONVERTED DEMOV RECORDS             *                   
* (PACKED STATION IN DOVKNET)                               *                   
*                                                           *                   
* 19JUL05 MHER EXTRACT NETWORK IMPRESSIONS                  *                   
*                                                           *                   
* 02MAY05 MHER FIX FOR ZERO VS. LOOKUP IN DEMOV RECORD      *                   
*                                                           *                   
* 11JUL02 MHER ALLOW REQUESTS ACROSS ESTIMATES BY READING   *                   
*              PROPER ESTIMATE HEADER BEFORE DEMO LOOKUP    *                   
*                                                           *                   
* 17DEC01 MHER SUPPORT FOR CABLE NEEDS TO SAVE 3 BYTES OF   *                   
*              STATION INSTEAD OF 2                         *                   
*===========================================================*                   
         TITLE 'SPDL02 - CANADIAN NETWORK DEMO UPDATE'                          
SPDL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDL02                                                         
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPDL02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    DB20                                                             
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    DB10                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=======================================================*                       
* REQUEST FIRST - FORCE HEADING                         *                       
*=======================================================*                       
         SPACE 1                                                                
DB10     DS    0H                                                               
         MVI   FORCEHED,C'Y'       FORCE HEAD LINES                             
         B     EXIT                                                             
         EJECT                                                                  
*=======================================================*                       
* ESTFIRST - READ ALL ESTIMATES FOR THIS PRODUCT        *                       
*=======================================================*                       
         SPACE 1                                                                
ESTF     DS    0H                                                               
         L     R0,=A(SVDEMOS)                                                   
         L     R1,=A(SVDEMOX-SVDEMOS)                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ADEST                                                         
         MVC   KEY(13),0(R6)                                                    
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         B     ESTF4                                                            
*                                                                               
ESTF2    GOTO1 SEQ                                                              
*                                                                               
ESTF4    CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   EXIT                                                             
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   ESTF2                                                            
*                                                                               
         GOTO1 GETBUY              READ ESTIMATE INTO ADBUY                     
*                                                                               
         L     R6,ADBUY                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,KEY+7            ESTIMATE NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,128              TIMES SAVE AREA LEN                          
         A     RE,=A(SVDEMOS)                                                   
         MVC   0(128,RE),EDEMOS                                                 
         B     ESTF2                                                            
         EJECT                                                                  
*=======================================================*                       
* PROCBUY                                               *                       
*=======================================================*                       
         SPACE 1                                                                
DB20     DS    0H                                                               
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
*                                                                               
         OC    BUYMSTA(2),BUYMSTA  NETWORK BUY?                                 
         BNZ   EXIT                NO - EXIT                                    
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,MYMKT,MYNET   NETWORK                        
         CLC   =C'ALL',QSTA                                                     
         BE    DB25                                                             
         CLC   MYNET,QSTA          REQUESTED NETWORK                            
         BNE   EXIT                                                             
*                                                                               
DB25     CLC   QBOOK1,SPACES       REQUESTED SHOW                               
         BE    DB30                NO - DO ALL SHOWS                            
*                                                                               
         CLC   QBOOK1,BDPROGRM     TEST SAME PGM AS REQUESTED                   
         BNE   EXIT                NO,SKIP BUY                                  
*                                                                               
DB30     DS    0H                                                               
* MOVE DEMOS FOR THIS ESTIMATE TO ADEST                                         
         SR    RE,RE                                                            
         IC    RE,KEY+9            BUY EST                                      
         BCTR  RE,0                                                             
         MHI   RE,128                                                           
         A     RE,=A(SVDEMOS)                                                   
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         MVC   EDEMOS(128),0(RE)                                                
         DROP  R6                                                               
                                                                                
         MVC   SVPGM,BDPROGRM      SAVE SHOW NAME                               
         MVC   SVKEY,KEY           SAVE ORIGINAL BUY KEY                        
         DROP  R5                                                               
*                                                                               
         L     RE,=A(REC2)                                                      
         LHI   RF,REC2X-REC2                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,=A(REC3)                                                      
         LHI   RF,REC3X-REC3                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         EJECT                                                                  
*=======================================================*                       
* FIND DEMO DEF REC TO MATCH BUY REC                    *                       
*=======================================================*                       
         SPACE 1                                                                
DB35     DS    0H                                                               
         MVC   SVNETBUY,KEY        SAVE NETWORK BUY KEY                         
         MVC   MYPROG,SVPGM        SHOW                                         
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R2,KEY                                                           
         USING DOVRECD,R2                                                       
*                                                                               
         MVC   DOVKEY(2),=X'0D17'                                               
         MVC   DOVKAGMD,CKEYAM     AGY/MED                                      
         MVC   DOVKNET,SVKEY+6     PACKED NETWORK                               
         MVC   DOVKCLT,SVKEY+1     CLIENT                                       
         MVC   DOVKPGM,SVPGM       SHOW                                         
         MVC   DOVKRTS,CPROF+3     RATING SERVICE CPROF + 3                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     DOES AN OVERRRIDE/CLT REC EXIST              
         BE    DB37                                                             
         MVC   KEY,KEYSAVE         NO - RE-SET KEY                              
         XC    DOVKCLT,DOVKCLT     AND CLEAR CLIENT                             
*                                                                               
DB36     GOTO1 HIGH                                                             
*                                                                               
DB37     CLC   KEY(12),KEYSAVE     DOES OVERRIDE REC EXIST                      
         BNE   EXIT                                                             
         L     R6,=A(REC2)                                                      
         ST    R6,AREC             ADDRESS OF DEMOD REC                         
         GOTO1 GET                 GET THE RECORD                               
         LA    R6,24(R6)           1ST ELEM                                     
*                                                                               
DB38     MVI   ELCODE,99           TEST SHOW CODE OVERRIDE IN REC               
         BRAS  RE,NEXTEL                                                        
         BNE   DB40                                                             
*                                                                               
* FOUND DEMO OVERRIDE                                                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY+7(4),2(R6)      SET PROGRAM IN KEY AND REREAD                
         MVC   MYPROG,2(R6)                                                     
         B     DB36                                                             
         EJECT                                                                  
*=======================================================*                       
* CHECK FOR 2ND DEMO REC                                *                       
*=======================================================*                       
         SPACE 1                                                                
DB40     L     R5,AREC             CORRECT USER DEMO NUMBERS                    
*                                                                               
         GOTO1 SEQ                 CHECK IF 2ND DEMOD EXISTS                    
         CLC   KEY(12),KEYSAVE                                                  
         BNE   DB45                                                             
*                                                                               
         L     RE,=A(REC3)                                                      
         ST    RE,AREC             ADDRESS OF DEMOD REC                         
         GOTO1 GET                 GET THE RECORD                               
*                                                                               
         L     R5,AREC             CORRECT USER DEMO NUMBERS                    
         EJECT                                                                  
DB45     DS    0H                  BUILD MKT/STA TAB FROM DEMO OVRD REC         
         BAS   RE,GETMKT                                                        
                                                                                
*=======================================================*                       
* GET REGION PERCENTAGE OUT OF 02 DEMO EL IN NETWORK BUY*                       
*=======================================================*                       
                                                                                
DB50     L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R0,15,20(R6)        NEW BUYS HAVE RGN PCT HERE                   
         BNZ   *+8                                                              
         L     R0,=F'100000'                                                    
         ST    R0,RGNPCTG                                                       
         SPACE 1                                                                
*=======================================================*                       
* FIND X'68' EXPLODED BUY ELEMENTS                      *                       
*=======================================================*                       
                                                                                
         MVC   KEY,SVNETBUY        RESTORE NETWORK BUY KEY                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY              REREAD NETWORK BUY                           
* SET UP TO GET NETWORK IMPRESSIONS                                             
         MVC   AREC,ADBUY          BLDDEM LOOKS IN AREC                         
         BRAS  RE,BLDDEM           REBUILD DEMO ELEMENTS                        
         SR    R7,R7               CLEAR 68 ELEM POINTER                        
*                                                                               
         XC    DEMOTAB,DEMOTAB                                                  
         BAS   RE,GETIMPS                                                       
         BAS   RE,SETDEM                                                        
*                                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
         L     R6,ADBUY            ORIGINAL BUY RECORD                          
         LA    R6,24(R6)                                                        
         LR    R7,R6               PRESET FOR RESTORE                           
*                                                                               
DB52     LR    R6,R7               RESTORE POINTER                              
*                                                                               
DB54     MVI   ELCODE,X'68'        RESET ELEM CODE                              
         BRAS  RE,NEXTEL           GET NEXT X'68' ELEM                          
         BNE   EXIT                                                             
*        DC    H'0'                                                             
*                                                                               
         LR    R7,R6               SAVE POINTER TO X'68' ELEM                   
*                                                                               
DB60     DS    0H                  NOW READ EXPLODED BUYREC                     
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       ORIGINAL BUY KEY                             
         MVC   KEY+4(5),2(R7)      MKT STA FROM X'68' ELEM                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    DB65                REC SHOULD BE THERE                          
*                                                                               
         MVC   P(29),=C'** ERROR ** MISSING LOCAL BUY'                          
         LA    R0,13                                                            
         GOTO1 HEXOUT,TRDMCB,KEYSAVE,P+32,(R0),=C'TOG'                          
         GOTO1 REPORT                                                           
         B     DB52                THEN SKIP THIS STATION                       
*                                                                               
DB65     MVI   CHANGESW,C'N'       RESET ACTIVITY SWITCH                        
*                                                                               
         L     R6,=A(REC4)                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                 GET EXPLODED BUY                             
*                                                                               
         BRAS  RE,BLDDEM           REBUILD DEMO ELEMENTS                        
* COPY EXISTING REC INTO SAVEREC                                                
         LR    R0,R6               EXISTING RECORD                              
         SR    R1,R1                                                            
         ICM   R1,3,13(R6)         LENGTH OF EXISTING RECORD                    
         L     RE,=A(SAVEREC)                                                   
         LA    RF,2(R1)            'TO' LEN = 'FROM' LEN+2                      
         MVCL  RE,R0                                                            
         EJECT                                                                  
*=================================================================*             
* PROCESS DEMOS FOR EXPLODED BUY                                  *             
* R7 POINTS TO 68 ELEM IN NETWORK BUY                             *             
*=================================================================*             
         SPACE 1                                                                
DB70     DS    0H                                                               
         MVC   MKTBUY,2(R7)        SAVE MKT FROM BUY                            
         MVC   STABUY,4(R7)        SAVE STA FROM BUY                            
*                                                                               
         BAS   RE,GETSTA           BUILD STA/MKT TABLE                          
         OC    STATBL(4),STATBL    TEST ANY OVERRIDES                           
         BNZ   DB75                                                             
         CLI   QOPT3,C'Y'                                                       
         BE    *+8                                                              
         B     DB52                                                             
         MVC   SVSTBUY,STABUY      IF NOTHING, TRY TO MATCH ON MKT              
         BAS   RE,MATCHMS                                                       
         OC    STABUY,STABUY       IF NO MATCH ON MKT, IGNORE                   
         BZ    DB52                                                             
         MVI   MKTFL,C'Y'          WILL PRINT * NXT TO STATION IN REP           
         BAS   RE,GETSTA                                                        
         OC    STATBL(4),STATBL    TEST ANY OVERRIDES                           
         BNZ   *+6                                                              
         DC    H'0'                IF WAS MATCH ON MKT SHOULD BE OVERRD         
         MVC   STABUY,SVSTBUY                                                   
*                                                                               
DB75     L     R6,=A(REC4)         POINT TO EXPLODED BUY                        
         USING BUYREC,R6                                                        
*                                                                               
         MVI   ELCODE,2            SET TO FIND 02 DEMEL                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,PULLDEM          GET INPUT VALUES INTO DEMOTAB                
         BAS   RE,GETIMPS             AND GLOBAL IMPS                           
         BAS   RE,SETDEM           PUT VALUES IN ELEMENT                        
*                                                                               
DB80     MVI   ELCODE,3            SEARCH FOR 03 DEMO ELS                       
         BRAS  RE,NEXTEL                                                        
         BNE   DB90                                                             
*                                                                               
         BAS   RE,PULLDEM                                                       
         BAS   RE,SETDEM                                                        
         B     DB80                                                             
         SPACE 1                                                                
*=====================================================*                         
* ALL SPILL ELEMENTS PROCESSED - TEST TO WRITE RECORD *                         
*=====================================================*                         
         SPACE 1                                                                
DB90     CLI   CHANGESW,C'Y'       TEST ACTIVITY                                
         BNE   DB92                                                             
*                                                                               
         L     RE,=A(SAVEREC)     EXISTING RECORD                               
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)        LENGTH OF EXISTING RECORD                     
         L     R4,=A(REC4)        NEW RECORD                                    
         SR    R5,R5                                                            
         ICM   R5,3,13(R4)        LENGTH OF NEW RECORD                          
         CR    R5,RF              COMPARE LENGTHS                               
         BNE   DB91               NOT EQUAL - MUST HAVE CHANGED                 
         CLCL  R4,RE              EQUAL - ANY CHANGE TO DATA?                   
         BE    DB92               NO CHANGE- PROCESS NEXT                       
*                                                                               
DB91     MVC   AREC,=A(REC4)                                                    
         GOTO1 PUT                 WRITE CHANGED RECORD                         
         BAS   RE,DLREP                                                         
         XC    MKTFL,MKTFL                                                      
*                                                                               
         CLI   QOPT5,C'Y'          TRACE ON                                     
         BNE   *+8                                                              
         BAS   RE,DBTRACE                                                       
*                                                                               
DB92     B     DB52                PROCESS NEXT 68 ELEMENT                      
         EJECT                                                                  
*========================================================*                      
* GETSTA - READS THROUGH X'05' ELEMS IN BOTH DEMO RECS   *                      
*          BUILDS TABLE OF 12 BYTE ENTRIES               *                      
*       +0          +4             +8                    *                      
*     STA  OR       DSPL IN        DSPL IN               *                      
*     X'00' MKT     DEMO REC 1     DEMO REC 2            *                      
*========================================================*                      
         SPACE 1                                                                
GETSTA   NTR1                                                                   
         XC    STATBL,STATBL                                                    
         LA    R4,STATBL                                                        
*                                                                               
         L     R6,=A(REC2)        PT TO FIRST DEMO OVRD REC                     
         LA    R6,24(R6)          1ST ELEM                                      
         MVI   ELCODE,5                                                         
*                                                                               
GETSTA5  BRAS  RE,NEXTEL                                                        
         BNE   GETSTA20                                                         
*                                                                               
GETSTA7  SR    RE,RE                                                            
         ICM   RE,7,STABUY         ORIGINAL STATION                             
         CLI   STABUY+2,X'B0'      TEST CABLE                                   
         BNL   *+8                 YES - USE 3 BYTES                            
         SRL   RE,8                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,2(R6)                                                       
         CLI   4(R6),X'B0'         TEST CABLE                                   
         BNL   *+8                 YES - USE 3 BYTES                            
         SRL   RF,8                                                             
*                                                                               
         CR    RE,RF               RIGHT STATION?                               
         BNE   GETSTA5             NO - TRY NEXT                                
*                                                                               
GETSTA10 DS    0H                                                               
         MVC   0(3,R4),2(R6)       SAVE STATION                                 
         ST    R6,4(R4)            SAVE ADDRESS OF ELEM                         
         LA    R4,12(R4)           BUMP POINTER                                 
         SPACE 1                                                                
*===============================================================*               
* NOW ADD ENTRIES FOR SPILL MKTS (WHICH FOLLOW STATION ELEMENT) *               
*===============================================================*               
         SPACE 1                                                                
GETSTA15 SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    GETSTA20                                                         
         CLI   0(R6),5                                                          
         BNE   GETSTA20                                                         
*                                                                               
         CLI   2(R6),0             FIRST BYTE IS X'00' IF SPILL OVRD            
         BNE   GETSTA20            SO IF NOT 0, NO MORE SPILL MKTS              
         MVC   0(3,R4),2(R6)       MOVE MARKET NUMBER                           
         ST    R6,4(R4)            SET ELEMENT ADDRESS                          
         LA    R4,12(R4)                                                        
         B     GETSTA15                                                         
         EJECT                                                                  
*=========================================================*                     
* FIRST RECORD COMPLETE - NOW SCAN SECOND RECORD          *                     
*=========================================================*                     
         SPACE 1                                                                
GETSTA20 L     R6,=A(REC3)        PT TO SECOND DEMO OVRD REC                    
         CLI   0(R6),0            TEST THERE ARE 2 RECORDS                      
         BE    GETSTAX            NO - DONE                                     
         LA    R6,24(R6)          1ST ELEM                                      
*                                                                               
GETSTA25 BRAS  RE,NEXTEL                                                        
         BNE   GETSTA30                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,STABUY         ORIGINAL STATION                             
         CLI   STABUY+2,X'B0'      TEST CABLE                                   
         BNL   *+8                 YES - USE 3 BYTES                            
         SRL   RE,8                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,2(R6)                                                       
         CLI   4(R6),X'B0'         TEST CABLE                                   
         BNL   *+8                 YES - USE 3 BYTES                            
         SRL   RF,8                                                             
*                                                                               
         CR    RE,RF               RIGHT STATION?                               
         BNE   GETSTA25            NO - TRY NEXT                                
*                                                                               
         LA    R4,STATBL           STA IS ALWAYS FIRST TABLE ENTRY              
         ST    R6,8(R4)            SAVE ADDRESS OF ELEM                         
         SPACE 1                                                                
*=======================================================*                       
* NOW PROCESS SPILL MARKETS                             *                       
*=======================================================*                       
         SPACE 1                                                                
GETSTA30 CLI   0(R6),0             TEST NO MORE ELEMENTS                        
         BE    GETSTAX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    GETSTAX                                                          
         CLI   0(R6),5                                                          
         BNE   GETSTAX                                                          
*                                                                               
         CLI   2(R6),0             FIRST BYTE IS X'00' IF SPILL OVRD            
         BNE   GETSTAX             SO IF NOT 0, NO MORE SPILL MKTS              
*                                                                               
         BAS   RE,CHKTBL           FIND PREVIOUS ENTRY                          
         MVC   0(3,R4),2(R6)       MOVE MARKET NUMBER                           
         ST    R6,8(R4)            SET ELEMENT ADDRESS                          
         B     GETSTA30                                                         
*                                                                               
GETSTAX  XIT1                                                                   
         EJECT                                                                  
*=========================================================*                     
* SEARCH STATBL FOR ENTRY MATCHING STA/MKT AT 2(R6)       *                     
* ON EXIT R4 POINTS TO MATCH OR END OF TABLE              *                     
* =====>  NOTE ROUTINE IS NOT NTR'D               <=====  *                     
*=========================================================*                     
         SPACE 1                                                                
CHKTBL   LA    R4,STATBL                                                        
*                                                                               
CHKTBL2  CLC   0(3,R4),2(R6)       MATCH ENTRY                                  
         BER   RE                                                               
         LA    R4,12(R4)                                                        
         OC    0(3,R4),0(R4)                                                    
         BNZ   CHKTBL2                                                          
         BR    RE                  EXIT                                         
         EJECT                                                                  
*=============================================================*                 
* PULLDEM - FINDS ENTRY IN STATBL AND PULLS DEMO CODES        *                 
*           FROM 01 ELEMENT AND DEMO VALUES FROM 05 ELEMENTS  *                 
*           AND SAVES THEM IN DEMOTAB IN THE FORMAT           *                 
*           CODE(3) VAL(2) CODE(3) ...                        *                 
*=============================================================*                 
         SPACE                                                                  
PULLDEM  NTR1                                                                   
*                                                                               
         XC    DEMOTAB,DEMOTAB     CLEAR PREVIOUS VALUES                        
         LA    R8,DEMOTAB                                                       
*                                                                               
         LA    R4,STATBL          POINT TO BEGINNING OF STATION TABLE           
         MVI   PASS,1             RESET FLAG                                    
*                                                                               
         CLI   0(R6),X'02'        USE ORIGINATING STATION?                      
         BE    PULLDEM4                                                         
*                                                                               
* FOR 03 NEED TO SEARCH TABLE FOR CORRECT MARKET                                
*                                                                               
PULLDEM2 CLC   4(2,R6),1(R4)      MATCH MKT IN ELEM TO MKT IN TABLE             
         BE    PULLDEM4                                                         
*                                                                               
         LA    R4,12(R4)          NEXT STATION TABLE ENTRY                      
         OC    0(4,R4),0(R4)      TEST MORE ENTRIES                             
         BNZ   PULLDEM2                                                         
         B     PULLDEMX           MARKET NOT FOUND                              
*                                                                               
PULLDEM4 ICM   R6,15,4(R4)        POINT TO 05 ELEM IN DEMOREC 1                 
         LA    R6,5(R6)           POINT TO FIRST DEMO                           
         BZ    PULLDEM8                                                         
         L     R1,=A(REC2)                                                      
         BAS   RE,PULLSET         POINT R1 TO DEMO LIST/ R0 FOR BCT             
         LTR   R0,R0                                                            
         BZ    PULLDEM8                                                         
*                                                                               
PULLDEM6 OC    0(3,R1),0(R1)       TEST NO DEMO                                 
         BZ    PULLDEM7                                                         
*                                                                               
         MVC   0(3,R8),0(R1)       MOVE DEMO NUM TO WORK AREA                   
         NI    0(R8),X'7F'         TURN OFF 'INPUT' FLAG                        
         MVC   3(2,R8),0(R6)       MOVE DEMO OVRD VALUE (X'8000'=ZERO)          
         LA    R8,5(R8)            NEXT DEMO TABLE ENTRY                        
*                                                                               
PULLDEM7 LA    R6,2(R6)            NEXT DEMO OVERRIDE VALUE                     
         LA    R1,3(R1)            NEXT DEMO NUMBER                             
         BCT   R0,PULLDEM6                                                      
         EJECT                                                                  
PULLDEM8 CLI   PASS,1              TEST RECORD 1 PASS                           
         BNE   PULLDEMX            NO - DONE                                    
         MVI   PASS,2              SET FOR SECOND PASS                          
*                                                                               
         ICM   R6,15,8(R4)         POINT TO 05 ELEM IN DEMOREC 2                
         BZ    PULLDEMX                                                         
         LA    R6,5(R6)            POINT TO FIRST DEMO                          
*                                                                               
         L     R1,=A(REC3)                                                      
         CLI   0(R1),0                                                          
         BE    PULLDEMX                                                         
         BAS   RE,PULLSET                                                       
         LTR   R0,R0                                                            
         BNZ   PULLDEM6                                                         
*                                                                               
PULLDEMX B     EXIT                                                             
         EJECT                                                                  
*=====================================================*                         
* ON ENTRY R1 POINTS TO A DEMO OVERRIDE RECORD        *                         
* ON EXIT  R1 POINTS TO THE DEMO OVERRIDE LIST        *                         
*             IN THE 01 ELEMENT                       *                         
*          R0 IS SET FOR BCT ON THE NUMBER OF DEMOS   *                         
*=====================================================*                         
         SPACE 1                                                                
PULLSET  NTR1                                                                   
         LA    R1,24(R1)          POINT TO O1 ELEMENT IN DEMOREC                
         CLI   0(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AHI   RF,-12                                                           
         BNP   PULLSETX                                                         
         SR    RE,RE                                                            
         D     RE,=F'3'            RF HAS COUNT                                 
         LR    R0,RF               SO PUT IT IN R0                              
         LA    R1,12(R1)           POINT TO LIST OF OVERRIDE DEMOS              
*                                                                               
PULLSETX DS    0H                                                               
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
*========================================================*                      
* GETIMPS - GETS GLOBAL IMPRESSIONS FROM OVERRIDE        *                      
*           DEMO RECORDS AND SAVES IN DEMOTAB            *                      
*           R7 POINTS TO X'68' ELEMENT IN EXPLODE BUY    *                      
*========================================================*                      
                                                                                
GETIMPS  NTR1                                                                   
         LA    R8,DEMOTAB          FIND LAST ENTRY IN DEMOTAB                   
*                                                                               
GETIMP2  OC    0(5,R8),0(R8)                                                    
         BZ    GETIMP4                                                          
         LA    R8,5(R8)                                                         
         B     GETIMP2                                                          
*                                                                               
GETIMP4  MVI   PASS,1                                                           
         L     R6,=A(REC2)         POINT TO FIRST RECORD                        
*                                                                               
GETIMP5  LA    R6,24(R6)           THEN POINT TO ELEMENTS                       
         MVI   ELCODE,2            FIND IMP OVERRIDE ELEM                       
         BRAS  RE,NEXTEL                                                        
         BNE   GETIMP20                                                         
*                                                                               
GETIMP6  DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)          ELEM LEN                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'5'            REMAINDER DROPPED                            
         LTR   R0,R1               SET FOR BCT                                  
         BNP   GETIMP20                                                         
*                                                                               
GETIMP8  LA    R1,2(R6)            POINT TO 1ST IMP IN 02 ELEM                  
         EJECT                                                                  
GETIMP10 SR    RF,RF                                                            
         ICM   RF,3,3(R1)          GET VALUE                                    
         STH   RF,HALF             SAVE                                         
         LTR   R7,R7               TEST DOING MKT 0 BUY                         
         BZ    GETIMP12                                                         
         AR    RF,RF               X 2                                          
         M     RE,RGNPCTG          X RGN SHARE OF NTWK                          
         D     RE,=F'100000'                                                    
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,15,7(R7)         STATION SHARE OF BUY                         
         AR    RE,RE               X 2                                          
         MR    RE,RE                                                            
         D     RE,=F'100000'                                                    
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         STH   RF,HALF                                                          
*                                                                               
GETIMP12 MVC   0(3,R8),0(R1)       MOVE DEMO CODE                               
         NI    0(R8),X'7F'         CLEAR 'INPUT' FLAG                           
         MVC   3(2,R8),HALF                                                     
         OC    3(2,R8),3(R8)       TEST ZERO                                    
         BNZ   *+8                                                              
         OI    3(R8),X'80'         SET OVERRIDE FLAG                            
         LA    R8,5(R8)            NEXT SLOT                                    
         LA    R1,5(R1)            NEXT DEMO VALUE                              
         BCT   R0,GETIMP10                                                      
*                                                                               
GETIMP20 CLI   PASS,1                                                           
         BNE   GETIMPX                                                          
         MVI   PASS,2                                                           
*                                                                               
         L     R6,=A(REC3)         POINT TO SECOND RECORD                       
         CLI   0(R6),0             TEST IT IS THERE                             
         BNE   GETIMP5                                                          
*                                                                               
GETIMPX  XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* SETDEM - SET NEW DEMO VALUES IN DEMO ELEMENT                  *               
* ON ENTRY R6 POINTS TO 02/03 DEMO ELEMENT IN BUY RECORD        *               
* IF POSTBUY OPTION IS NOT ON, REPLACE VALUES IN BUY ELEMENT    *               
*                   IS ON, DELETE FOLLOWING 22/23 IF THERE      *               
*                   AND INSERT NEW 22/23 FOLLOWING THIS ELEMENT *               
*===============================================================*               
         SPACE 1                                                                
SETDEM   NTR1                                                                   
         CLI   QOPT1,C'P'          TEST POST-BUY OPTION ON                      
         BE    SETDEM10                                                         
         CLI   QOPT1,C'Y'          TEST POST-BUY OPTION ON                      
         BE    SETDEM10                                                         
* REPLACE DEMO VALUES IN 02/03                                                  
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   SETDEMX             EXIT IF NO DEMOS IN ELEMENT                  
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
SETDEM2  BAS   RE,GETVAL           POINT R8 TO DEMOTAB ENTRY                    
         BNE   SETDEM4                                                          
*                                                                               
         OC    3(2,R8),3(R8)       TEST LOOK-UP                                 
         BZ    SETDEM4             YES - LEAVE IT ALONE                         
*                                                                               
         XC    3(5,R1),3(R1)       CLEAR OUT OLD VALUES                         
         MVI   3(R1),100           SET SVI VALUE                                
         OI    4(R1),X'80'         SET MANUAL OVRD                              
         MVC   6(2,R1),3(R8)       MOVE VALUE TO ELEMENT                        
         CLC   6(2,R1),=X'8000'    TEST ZERO                                    
         BNE   *+10                                                             
         XC    6(2,R1),6(R1)                                                    
*                                                                               
SETDEM4  LA    R1,8(R1)            NEXT DEMO                                    
         BCT   R0,SETDEM2                                                       
         B     SETDEMX                                                          
         EJECT                                                                  
*=======================================================*                       
* POST BUY OPTION ON WITH OPTION TO ONLY REPLACE IMPS   *                       
*=======================================================*                       
         SPACE 1                                                                
SETDEM10 CLI   QOPT2,C'Y'          ONLY REPLACE IMPRESSIONS                     
         BNE   SETDEM20                                                         
         CLI   0(R6),X'02'                                                      
         BE    SETDEM11                                                         
         CLI   0(R6),X'03'                                                      
         BNE   SETDEM20                                                         
*                                                                               
SETDEM11 LR    R7,R6               R6 POINTS TO X'02'/03 ELEM                   
         ZIC   R1,1(R6)                                                         
         AR    R7,R1                                                            
         CLI   0(R7),X'22'         R7 POINTS TO X'22' ELEM                      
         BE    SETDEM12                                                         
         CLI   0(R7),X'23'         OR TO X'23' ELEM                             
         BNE   SETDEM20                                                         
*                                                                               
SETDEM12 ZIC   R0,1(R6)            CAL NUM OF DEMOS FROM 02 ELEM                
         AHI   R0,-24                                                           
         SRL   R0,3                                                             
         LA    R1,24(R6)           R1 POINTS TO DEMO CODE IN 02/03 EL           
         LA    R2,2(R7)            R2 POINTS TO VALUE IN 22 EL                  
         CLI   0(R7),X'23'         OR IN X'23' ELEM                             
         BNE   *+8                                                              
         LA    R2,6(R7)            R2 POINTS TO VALUE IN 23 EL                  
*                                                                               
SETDEM14 BAS   RE,GETVAL           MATCHES ON CODE AT R1                        
         BNE   SETDEM16                                                         
         CLI   1(R8),C'E'          FOR RATINGS- SAVE OLD VALUES                 
         BE    SETDEM15                                                         
         CLI   1(R8),C'R'                                                       
         BNE   SETDEM16                                                         
SETDEM15 MVC   3(2,R8),1(R2)       SAVE 22 ELEM VALUE IN DEMOTAB                
SETDEM16 LA    R1,8(R1)            BUMP IN 02 ELEM                              
         LA    R2,3(R2)            BUMP IN 22 ELEM                              
         BCT   R0,SETDEM14                                                      
         SPACE 1                                                                
*=======================================================*                       
* POST BUY OPTION ON                                    *                       
*=======================================================*                       
         SPACE 1                                                                
SETDEM20 DS    0H                                                               
         LR    R7,R6               SAVE ELEM POINTER                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R6),X'22'                                                      
         BE    SETDEM22                                                         
         CLI   0(R6),X'23'                                                      
         BNE   SETDEM24                                                         
*                                                                               
SETDEM22 L     R8,=A(REC4)         DELETE THE 22/23 ELEMENT                     
         OC    KEY+4(2),KEY+4      TEST NETWORK BUY                             
         BNZ   *+8                                                              
         L     R8,ADBUY                                                         
         GOTO1 RECUP,DMCB,(R8),(R6),0                                           
*                                                                               
SETDEM24 LR    R6,R7               RESTORE ELEMENT POINTER                      
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   SETDEMX                                                          
         SRL   R0,3                                                             
         LA    R1,24(R6)                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(1),0(R6)                                                    
         OI    ELEM,X'20'          SET NEW ELEMENT CODE                         
*                                                                               
         LA    R7,ELEM+2                                                        
         CLI   ELEM,X'22'                                                       
         BE    SETDEM26                                                         
         MVC   ELEM+2(4),4(R6)    MOVE AGY/RTG SVC MARKET NUMBERS               
         LA    R7,ELEM+6                                                        
*                                                                               
SETDEM26 BAS   RE,GETVAL                                                        
         BNE   SETDEM28                                                         
         MVC   1(2,R7),3(R8)       MOVE DEMO VALUE                              
         OC    1(2,R7),1(R7)       NO VALUE MEANS LOOKUP                        
         BZ    SETDEM28            SO NOT AN OVERRIDE                           
         CLC   1(2,R7),=X'8000'    8000 MEANS ZERO                              
         BNE   *+10                                                             
         XC    1(2,R7),1(R7)       SO CLEAR IT AND                              
         OI    0(R7),X'80'         SET OVERRIDE FLAG                            
*                                                                               
SETDEM28 LA    R7,3(R7)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,SETDEM26                                                      
*                                                                               
         LA    R0,ELEM                                                          
         SR    R7,R0                                                            
         STC   R7,ELEM+1           SET ELEMENT LENGTH                           
*                                                                               
         LA    R0,2                                                             
         CLI   ELEM,X'22'                                                       
         BE    *+8                                                              
         LA    R0,6                                                             
         CLM   R0,1,ELEM+1         TEST ADDED ANY DEMOS                         
         BNL   SETDEMX             NO                                           
         CLI   CHANGESW,C'Y'       TEST ANY VALUES FOUND                        
         BNE   SETDEMX                                                          
         EJECT                                                                  
*=======================================================*                       
* INSERT NEW ELEMENT IN RECORD IF ROOM                  *                       
*=======================================================*                       
         SPACE 1                                                                
         L     R8,=A(REC4)                                                      
         OC    KEY+4(2),KEY+4      TEST NETWORK BUY                             
         BNZ   *+8                                                              
         L     R8,ADBUY                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R8)         GET RECORD LENGTH                            
         SR    RE,RE                                                            
         ICM   RE,1,ELEM+1         GET LEN OF NEW ELEMENT                       
         AR    R0,RE                                                            
         CHI   R0,3972                                                          
         BNH   SETDEM30                                                         
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(39),=C'*** ERROR - RECORD LENGTH TOO LARGE ***'                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
SETDEM30 DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               POINT PAST CURRENT ELEMENT                   
         GOTO1 RECUP,DMCB,(R8),ELEM,(R6)                                        
*                                                                               
SETDEMX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* GETVAL - POINT R8 TO AN OVERRIDE VALUE IN DEMOTAB           *                 
* ON ENTRY R1 POINTS TO 3 BYTE DEMO CODE                      *                 
* EXIT WITH CC EQ IF FIND VALUE, ELSE NEQ                     *                 
* =====>  NOTE NOT NTR'D  <=====                                                
*=============================================================*                 
         SPACE 1                                                                
GETVAL   DS    0H                                                               
         LA    R8,DEMOTAB                                                       
*                                                                               
GETVAL2  CLC   0(3,R1),0(R8)                                                    
         BE    GETVAL4                                                          
         LA    R8,5(R8)                                                         
         OC    0(3,R8),0(R8)                                                    
         BNZ   GETVAL2                                                          
         LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
*                                                                               
GETVAL4  MVI   CHANGESW,C'Y'       SET ACTIVITY FLAG                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=======================================================*                       
* PRINT OUT REPORT OF KEYS WHOSE RECORDS HAVE CHANGED   *                       
*=======================================================*                       
         SPACE 1                                                                
DLREP    NTR1                                                                   
         L     R6,=A(REC4)         ADDRESS EXPLODED BUYS                        
         USING BUYREC,R6                                                        
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',BUYMSTA),RPMKT,RPSTA   NETWORK                
*                                                                               
         XC    DUB,DUB                                                          
         MVC   P+11(4),MYNET                                                    
         SR    R0,R0                                                            
         ICM   R0,1,BUYKEST        ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+22(3),DUB                                                      
*                                                                               
         MVI   P+25,C'-'                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BUYKBUY        BUY LINE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+26(3),DUB                                                      
         CLI   MKTFL,C'Y'                                                       
         BNE   *+8                                                              
         MVI   P+35,C'*'                                                        
         MVC   P+36(4),RPSTA                                                    
         CLI   BUYMSTA+4,X'B0'     TEST CABLE                                   
         BL    *+10                                                             
         MVC   P+36(7),RPSTA                                                    
         MVC   P+47(18),SVPGM      SHOW NAME                                    
*                                                                               
* PRINT HEX KEY IF REQUEST0R = LISALISALISA                                     
*                                                                               
         CLC   QUESTOR,=C'LISALISALISA'                                         
         BNE   DLREP10                                                          
         LA    R0,13                                                            
         GOTO1 HEXOUT,TRDMCB,(R6),P+69,(R0),=C'TOG'                             
*                                                                               
DLREP10  GOTO1 REPORT                                                           
*                                                                               
DLREPX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*========================================================*                      
* BUILD MKT/STA TABLE USING STAT'S IN DEMO OVER REC      *                      
*       5 BYTE ENTRIES: 3 STA 2 MKT                                             
*========================================================*                      
         SPACE 1                                                                
GETMKT   NTR1                                                                   
         XC    MKTTBL,MKTTBL                                                    
         LA    R4,MKTTBL                                                        
         L     R6,=A(REC2)                                                      
         LA    R6,24(R6)                                                        
         MVI   ELCODE,5                                                         
*                                                                               
GETMKT5  BRAS  RE,NEXTEL                                                        
         BNE   GETMKT20                                                         
*                                                                               
         CLI   2(R6),0             IGNORE SPILL MKTS                            
         BE    GETMKT5                                                          
         MVC   2(3,R4),2(R6)       GET STAT IN MKTTBL                           
         XC    MKTSTA,MKTSTA       PREPARE STAT FOR UNPACKING                   
         MVC   MSSTA,2(R6)                                                      
         GOTO1 MSUNPK,DMCB,MKTSTA,WORK,WORK+5                                   
*                                                                               
         XC    KEY,KEY             PREP KEY FOR STATION RECORD CALL             
         LA    R5,KEY                                                           
         USING STAKEY,R5                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK+5                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   DMCB+12(4),=A(MYIO)                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,=A(MYIO)                                                      
         CLC   KEY(9),STAKEY         CHECK THE RECORD                           
         BNE   GETMKTX                                                          
         PACK  DUB,SMKT              PACK MKT                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,0(R4)            PUT IN MKTTBL                              
         LA    R4,5(R4)                                                         
         B     GETMKT5                                                          
         DROP  R5                                                               
*                                                                               
GETMKT20 CLI   QOPT5,C'Y'          FOR TRACING                                  
         BNE   GETMKTX                                                          
         GOTO1 HEXOUT,TRDMCB,MKTTBL,P,50,=C'TOG'                                
         GOTO1 REPORT                                                           
         L     R5,=A(REC2)                                                      
         GOTO1 HEXOUT,TRDMCB,(R5),P,50,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
GETMKTX  XIT1                                                                   
         EJECT                                                                  
*========================================================*                      
* MATCHMS FIND STATION IN MKTTBL WITH THE SAME MKT AS    *                      
* BUYSTA HAS IF THERE WAS NO MATCH ON STATION IN GETSTA  *                      
*========================================================*                      
*                                                                               
MATCHMS  NTR1                                                                   
         LA    R2,MKTTBL                                                        
         USING MKTSTA,R2                                                        
         XC    STABUY,STABUY                                                    
*                                                                               
MATCH10  CLC   MSMKT,MKTBUY        FIND MKT FROM MKTBUY IN MKTTBL               
         BE    MATCH30                                                          
         LA    R2,5(R2)                                                         
         OC    0(5,R2),0(R2)       EOT ?                                        
         BE    MATCHX                                                           
         B     MATCH10                                                          
*                                                                               
MATCH30  MVC   STABUY,MSSTA        SET STATION  N                               
*                                                                               
MATCHX   XIT1                                                                   
         DROP  R2                                                               
*========================================================*                      
* TRACE ROUTINE FOR TESTING                              *                      
*========================================================*                      
         SPACE 1                                                                
DBTRACE  NTR1                                                                   
         GOTO1 =V(PRTREC),TRDMCB,(C'E',AREC),(24,13),PRINT,HEXOUT               
         B     EXIT                                                             
*                                                                               
TRDMCB   DS    6F                                                               
OVER     DC    C'N'                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         DS    0D                                                               
MYMKT    DS    CL4                                                              
MYNET    DS    CL5                                                              
*                                                                               
RPMKT    DS    CL4                                                              
RPSTA    DS    CL8                                                              
*                                                                               
ELCODE   DS    X                                                                
PASS     DS    X                                                                
RGNPCTG  DS    F                                                                
*                                                                               
MYNSEQ   DS    CL1                                                              
MYPROG   DS    CL4                                                              
*                                                                               
SVPTR    DS    F                                                                
CHANGESW DC    C'N'                                                             
*                                                                               
SVKEY    DS    XL13                                                             
SVPGM    DS    CL18                                                             
STABUY   DS    XL3                 STATION FROM X'68' ELEMENT                   
SVSTBUY  DS    XL3                 STATION FROM X'68' ELEMENT, SAVED            
MKTBUY   DS    XL2                 MARKET FROM X'68' ELEMENT                    
*                                                                               
MKTSTA   DS    0CL5                                                             
MSMKT    DS    CL2                                                              
MSSTA    DS    CL3                                                              
MKTFL    DC    X'00'                                                            
         EJECT                                                                  
*=============================================================*                 
* THIS ROUTINE CREATES NEW DEMO ELEM FROM ESTHDR DEMOS        *                 
* AND INSERTS OLD DEMO VALUES FROM EXISTING DEMO ELEM         *                 
* ALSO CREATES NEW POST BUY DEMO ELEM IF ONE EXISTED          *                 
*=============================================================*                 
         SPACE 1                                                                
BLDDEM   NTR1  BASE=*,LABEL=*       ** USER 11 **                               
*                                                                               
         L     R6,AREC             FIND 02 DEMEL FIRST                          
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDDEM1  ST    R6,FULL             SAVE CURRENT DEMO ELEM ADDR                  
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM+24                                                       
         L     R4,ADEST                                                         
         LA    R4,EDEMLST-ESTHDRD(R4)                                           
         LA    R5,EDEMLSTN                                                      
*                                                                               
BLDDEM3  CLI   1(R4),0                                                          
         BE    BLDDEM4                                                          
*                                                                               
         CLI   0(R6),3             TEST SPILL                                   
         BNE   BLDDEM3B                                                         
         CLI   1(R4),C'R'          TEST RATING                                  
         BE    BLDDEM3B                                                         
         CLI   1(R4),C'E'          TEST EXTENDED RATING                         
         BE    BLDDEM3B                                                         
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BNE   BLDDEM3C            NO                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,2(R4)            GET DEMO NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         L     RF,ADEST                                                         
         LA    RF,EUSRNMS-ESTHDRD(RF)                                           
         AR    RE,RF               POINT TO USER DEMO NAMES                     
         CLI   0(RE),C'R'                                                       
         BE    BLDDEM3B                                                         
         CLI   0(RE),C'E'                                                       
         BNE   BLDDEM3C                                                         
*                                                                               
BLDDEM3B MVC   0(3,R1),0(R4)                                                    
         LA    R1,8(R1)                                                         
*                                                                               
BLDDEM3C LA    R4,3(R4)                                                         
         BCT   R5,BLDDEM3                                                       
*                                                                               
BLDDEM4  MVC   ELEM(24),0(R6)      MOVE ELCD/PRGM DESC/BOOK/ETC                 
         LA    R0,ELEM                                                          
         SR    R1,R0                                                            
         STC   R1,ELEM+1           SET LEN                                      
*                                                                               
         SR    R8,R8               CLEAR IN CASE NO DEMOS                       
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AHI   R7,-24                                                           
         BNP   BLDDEM35                                                         
         LA    R6,24(R6)                                                        
*                                                                               
BLDDEM20 SRL   R7,3                R7=N'OLD DEMOS                               
*                                                                               
         SR    R5,R5                                                            
         IC    R5,ELEM+1                                                        
         AHI   R5,-24                                                           
         BNP   BLDDEM35                                                         
         SRL   R5,3                R5=N'NEW DEMOS                               
*                                                                               
         L     R6,FULL             CHECK FOR POSTBUY ELEM FOLLOWING             
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'22'                                                      
         BE    BLDDEM22                                                         
         CLI   0(R6),X'23'                                                      
         BE    BLDDEM22                                                         
         B     BLDDEM26                                                         
*                                                                               
BLDDEM22 LR    R8,R6               R8=A(POST BUY DEMO ELE)                      
*                                                                               
         XC    WORK,WORK           BUILD NEW POST BUY ELEM IN WORK              
         LA    RE,WORK                                                          
         LR    R1,R5                                                            
         MHI   R1,3                                                             
         CLI   ELEM,2                                                           
         BNE   BLDDEM24                                                         
         MVC   0(PDEMO-PDELEM,RE),0(R8)                                         
         LA    R1,PDEMO-PDELEM(R1)                                              
         STC   R1,1(RE)                                                         
         LA    R8,PDEMO-PDELEM(R8)                                              
         LA    RE,PDEMO-PDELEM(RE)                                              
         ST    RE,DUB+4                                                         
         B     BLDDEM26                                                         
*                                                                               
BLDDEM24 MVC   0(SDEMO-SDELEM,RE),0(R8)                                         
         LA    R1,SDEMO-SDELEM(R1)                                              
         STC   R1,1(RE)                                                         
         LA    R8,SDEMO-SDELEM(R8)                                              
         LA    RE,SDEMO-SDELEM(RE)                                              
         ST    RE,DUB+4                                                         
*                                                                               
BLDDEM26 L     R6,FULL             MOVE OLD DEMO VALS TO NEW ELEM               
         LA    R6,24(R6)                                                        
*                                                                               
BLDDEM28 LR    R0,R5               R0=N'NEW DEMOS                               
         LA    R1,ELEM+24                                                       
         LTR   R8,R8               R8 NE 0 - PROCESS POST BUY ELEMENT           
         BZ    BLDDEM30                      ALSO                               
         L     RE,DUB+4                                                         
*                                                                               
BLDDEM30 CLC   0(3,R6),0(R1)       FIND DEMO IN NEW DEMO ELEMENT                
         BE    BLDDEM32                                                         
         LA    R1,8(R1)                                                         
         LTR   R8,R8                                                            
         BZ    *+8                                                              
         LA    RE,3(RE)                                                         
         BCT   R0,BLDDEM30                                                      
         B     BLDDEM34            NOT FOUND                                    
*                                                                               
BLDDEM32 MVC   0(8,R1),0(R6)       FOUND-MOVE IN THE VALUE(S)                   
         LTR   R8,R8                                                            
         BZ    BLDDEM34                                                         
         MVC   0(3,RE),0(R8)                                                    
*                                                                               
BLDDEM34 LA    R6,8(R6)            NEXT DEMO                                    
         LTR   R8,R8                                                            
         BZ    *+8                                                              
         LA    R8,3(R8)                                                         
         BCT   R7,BLDDEM28                                                      
*                                                                               
         SPACE 1                                                                
BLDDEM35 L     R6,FULL                 RESTORE DEMEL ADDRESS                    
         BAS   RE,BLDDEL                                                        
         BAS   RE,BLDADD                                                        
*                                                                               
         LTR   R8,R8               TEST POST BUY DEMO ELEMENT                   
         BZ    BLDDEM40                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO POST BUY ELEMENT                    
         BAS   RE,BLDDEL                                                        
         MVC   ELEM,WORK           MOVE ELEMENT FOR ADDEL                       
         BAS   RE,BLDADD                                                        
*                                                                               
BLDDEM40 L     R6,FULL             GET CURRENT 02/03 ELEM ADDRESS               
                                                                                
BLDDEM42 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDDEMX                                                          
         CLI   0(R6),3             TEST SPILL                                   
         BE    BLDDEM1             YES - GO PROCESS                             
         B     BLDDEM42                                                         
*                                                                               
BLDDEMX  XIT1                                                                   
         EJECT                                                                  
BLDDEL   LR    R0,RE                                                            
         GOTO1 RECUP,DMCB,AREC,(R6)                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
BLDADD   LR    R0,RE                                                            
         GOTO1 RECUP,DMCB,AREC,ELEM,(R6)                                        
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*NETTAB*'                                                    
CNNETTAB DS    127XL4                                                           
CNNETTBX EQU   *                                                                
         DS    XL4                                                              
         DS    0D                                                               
         DC    CL8'*STATBL*'                                                    
STATBL   DS    XL256               STATION TABLE --- 3 BYTE STA,                
         DC    CL8'**ELEM**'                                                    
ELEM     DS    XL256                                                            
         DC    CL8'DEMOTAB'                                                     
DEMOTAB  DS    XL256                                                            
         DC    CL8'*MKTTBL*'                                                    
MKTTBL   DS    XL256               MKT TABLE --- 3 BYTE STA, 2 MKT              
*                                                                               
         LTORG                                                                  
*                                                                               
SVNETBUY DS    XL16                                                             
         DS    0D                                                               
         DC    CL8'**REC2**'                                                    
REC2     DS    6000C                                                            
REC2X    EQU   *                                                                
         DS    0D                                                               
         DC    CL8'**REC3**'                                                    
REC3     DS    6000C                                                            
REC3X    EQU   *                                                                
         DS    0D                                                               
         DC    CL8'**REC4**'                                                    
REC4     DS    6000C                                                            
REC4X    EQU   *                                                                
         DS    0D                                                               
         DC    CL8'**SREC**'                                                    
SAVEREC  DS    6000C                                                            
SAVERECX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    1000C                                                            
MYIOX    EQU   *                                                                
         DS    0D                                                               
         DC    CL8'*SVDEMOS'                                                    
SVDEMOS  DS    256XL128                                                         
SVDEMOX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPREPDL02 03/14/07'                                      
         END                                                                    
