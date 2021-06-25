*          DATA SET REDAR21    AT LEVEL 245 AS OF 04/12/04                      
*PHASE T80F21B                                                                  
         TITLE 'T80F21 - REDAR21 - DARE REVISION OPEN/REJECT'                   
***********************************************************************         
*                                                                     *         
*  REDAR21 (T80F21) --- DARE REVISION OPEN/REJECT                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 12APR04 HQ  SKIP OLD FORMAT DEMO WHEN SAVE PREVIOUS DEMO VALUE      *         
* 24MAR04 HQ  SKIP DEMO VALUE THAT HAS NULL CATEGORY                  *         
* 03OCT03 HQ  POPULATE COST OVERRIDE FLAG                             *         
* 04JUN03 SKU ZERO SPOT FIX                                           *         
* 10APR03 SKU DEMO RATING SUPPORT                                     *         
* 14MAR03 HQ  HANDLE NULL PROGRAM NAME ON REVISION OPEN               *         
* 04JAN02 SKU CHANGE APPROVE TO OPEN                                  *         
* 03DEC01 SKU MAKE DARE BUYS TRADE AWARE                              *         
* 17OCT01 SKU ADD AUDIT TRAIL                                         *         
* 05OCT01 SKU FIX REVISION MAKEGOOD APPROVAL PROBLEM                  *         
* 13AUG01 SKU FLAGGED AND SKIP CANCELLED REP BUYS FOR MANUAL APPROVAL *         
* 04JUN01 BU  DAILY PACING                                            *         
* 31JAN01 SKU SUPPORT NEW PROGRAM NAME ELEMENT                        *         
*         SKU FIX BUY NPW OVERRIDE BIT BUG                            *         
*         SKU CC OVERRIDE FLAG AWARE                                  *         
* 19JAN01 SKU FIX REVISION BUG                                        *         
* 01JAN01 SKU COMPRESS TIME IF START AND END TIME ARE SAME            *         
* 27DEC00 SKU CHECK FOR NON-BLANK OR NON-NULL IN BUY ROTATION FIELD   *         
* 12OCT00 SKU DAILY ADD WEIGHT BUG FIX                                *         
* 24AUG00 SKU REDIRECT H7 TO MS SENDER ID                             *         
* 28JUN00 BU  REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG           *         
* 03FEB99 SKU SPOT ZEROED OUT BIT CHANGED FROM X'20' TO X'01'         *         
* 15NOV99 SKU FIX TAKEOVER 8D/8E PASSIVE KEY BUG                      *         
* 18OCT99 SKU APPROVE BUG FIX                                         *         
* 08JUL99 SKU MATCH BONUS LINES                                       *         
* 19MAR99 SKU VERSION HISTORY BUG FIX                                 *         
* 21JAN99 SKU SUPPORT NEW CANCELLED BUY BUCKETING                     *         
* 08JUN98 SKU SAVE DARE DEMO VALUES. FIX BUY COST TOTAL BUG           *         
* 10MAR98 SKU FIX PARTIAL CANCEL BUG                                  *         
* 05MAR98 SKU DELETE CFC COMMENT IF VERSION IS BUMPED                 *         
* 04MAR98 SKU ALTERNATE CALENDAR UPGRADE                              *         
* 19AUG97 SKU DATE OF BIRTH                                           *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
T80F21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F21*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         MVC   MYLABEL,=C'*MYAREA*'                                             
         ST    R3,RELO                                                          
*                                                                               
* INCAES OF RETURN FROM GLOBBER, NEEDS TO RETRANSMIT ENTIRE SCREEN              
*                                                                               
         MVC   AORLAST+1(2),=X'0101'                                            
*                                                                               
         MVI   MYSCRNUM,X'F8'      SET SCREEN NUMBER                            
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                  YES                                          
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VALRECRD            YES                                          
*                                  NO OTHER ACTIONS RECOGNIZED                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VKX                                                              
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VALRECRD DS    0H                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   VREPFACS,0(R1)                                                   
*                                  EXTERNAL ROUTINE, MISC SUBRTNS               
         CLI   PFKEY,2             USER WANTS TO SWITCH TO CONTRACT             
         BNE   VREC0030                                                         
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   FULL,RDARREP#                                                    
         OC    RDARREP#,RDARREP#                                                
         BNZ   VREC0020                                                         
         MVC   FULL,CCONKNUM                                                    
         DROP  R6                                                               
*                                                                               
VREC0020 DS    0H                                                               
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),FULL                                                  
         EDIT  (P5,WORK+20),(8,WORK+30),ALIGN=LEFT                              
*                                                                               
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER?                         
         GOTO1 CGLOBBER,DMCB,=C'PUTD',FULL,L'FULL,GLRCONNO                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',=CL1'D',1,GLRPFKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
VREC0030 DS    0H                                                               
         XC    ORDTOT$$,ORDTOT$$   CLEAR ORDER $ TOTALS                         
         XC    ORDTOTSP,ORDTOTSP   CLEAR ORDER SPOT TOTALS                      
*                                                                               
         CLI   NEEDSCRN,C'Y'                                                    
         BNE   VREC0040                                                         
         MVI   NEEDSCRN,C'Y'       SET 'NEED SCREEN' FLAG                       
                                                                                
VREC0040 DS    0H                                                               
         LA    R2,CONACTH          DETERMINE ACTION                             
         MVI   BUYLINE#,0          CLEAR BUYLINE NUMBER                         
         XC    IFELCODE(IFELLEN),IFELCODE                                       
*                                  CLEAR SPOTPAK XFER ELEMENT STORE             
         MVI   ACTCODE,C'A'                                                     
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                SET FOR EX STATEMENT                         
         EX    RF,VREC1200         COMPARE TO 'OPEN'                            
         BE    VREC0080            FOUND - PROCEED                              
         MVI   ACTCODE,C'R'                                                     
         EX    RF,VREC1240         COMPARE TO 'REJECT'                          
         BE    VREC0080            FOUND - PROCEED                              
         LA    R3,INVRCACT         ACTION NOT RECOGNIZED                        
         DC    H'0'                DUMP FOR NOW                                 
         B     VREC0880            TRANSFER TO WHERE???                         
VREC0080 EQU   *                                                                
         MVC   AORRMSG(21),=C'REASON FOR REJECTION:'                            
         CLI   ACTCODE,C'R'        REJECT REQUEST?                              
         BE    VREC0120            YES                                          
         XC    AORRMSG,AORRMSG     NO  - APPROVAL - CLEAR IT                    
VREC0120 EQU   *                                                                
         LA    R2,AORRMSGH         SET TRANSMIT BIT                             
         OI    6(R2),X'80'            FOR WHEN SCREEN ALREADY UP                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN!                         
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         NI    MISCFLG2,X'FF'-MF2TRADE                                          
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2TRADE                                                
*                                                                               
         MVI   KATZEDI,C'N'        SET KATZ EDI ORDER TO 'NO'                   
         CLC   =C'$EDI$',RDARAGAD  SCAN FOR KATZ EDI SPECIAL                    
         BNE   *+8                                                              
         MVI   KATZEDI,C'Y'                                                     
*                                                                               
         MVI   RESENT,C'N'         SET RESENT FLAG TO 'NO'                      
         TM    RDARMISC,X'80'      ORDER RESENT?                                
         BNO   VREC0160            NO                                           
         MVI   RESENT,C'Y'         YES - NEED VERSION BUMP                      
VREC0160 EQU   *                                                                
         TM    RDARMISC,X'20'      IS ORDER 'NOTDARE'?                          
         BNO   VREC0180            NO                                           
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         MVC   RERROR,=AL2(451)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
VREC0180 EQU   *                                                                
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER?                         
         CLI   5(R2),0                                                          
         BNE   VREC0320            YES - CONTINUE                               
         OC    RDARREP#,RDARREP#   IS ORDER LINKED?                             
         BNZ   VREC0200            YES                                          
         CLI   ACTCODE,C'R'        'REJECT' REQUEST?                            
         BE    VREC0320            YES - SKIP CONTRACT # OUTPUT                 
         MVC   RERROR,=AL2(435)    NO  - UNLINKED: ILLEGAL APPROVAL             
         B     VREC1000                                                         
VREC0200 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RDARREP#,WORK,4,=C'TOG'                              
*                                  GET REP CONTRACT NUMBER                      
         LA    R3,WORK                                                          
         LA    RF,8                LOOP CONTROL                                 
VREC0240 EQU   *                                                                
         CLI   0(R3),C'0'          LEADING ZERO?                                
         BNE   VREC0280            NO  - MOVE IT                                
         LA    R3,1(R3)            YES - BUMP TO NEXT POSITION                  
         BCT   RF,VREC0240         GO BACK FOR NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN                             
VREC0280 EQU   *                                                                
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VREC1280         MOVE BY LENGTH TO SCREEN                     
VREC0320 EQU   *                                                                
         TM    CCONFLAG,CCONFSWP   CANNOT OPEN IF CONTRACT IS IN                
         BZ    VREC0325            STATION'S WORK-IN-PROGRESS                   
         MVC   RERROR,=AL2(656)    SET ERROR TYPE = STATION IN WIP              
         B     VREC1000                                                         
*                                                                               
*    ACTIVITY DECISION TABLE:                                                   
*        AGENCY ORDER        TYPE OF        PERMITTED/                          
*        PRIOR STATUS        REQUEST        PROHIBITED                          
*        ------------        -------        ----------                          
*        OPEN                OPEN           PERMIT                              
*        OPEN                REJECT         PERMIT (SHOULDN'T OCCUR)            
*        OPENED              OPEN           PROHIBIT                            
*        OPENED              REJECT         PERMIT                              
*        REJECTED            OPEN           PROHIBIT                            
*        REJECTED            REJECT         PROHIBIT                            
*        NOTDARED            OPEN           PROHIBIT                            
*        NOTDARED            REJECT         PROHIBIT                            
*                                                                               
VREC0325 EQU   *                                                                
         TM    RDARMISC,X'20'      ORDER NOTDARED FROM AGENCY SIDE?             
         BZ    VREC0330                                                         
         CLI   ACTCODE,C'R'        YES - THIS REQUEST = REJECT?                 
         BE    VREC0440            YES - PERMIT IT                              
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(450)    SET ERROR TYPE = ONLY ACTION REJECT          
*                                     IS ALLOWED FOR NOTDARE ORDER              
         B     VREC1000                                                         
                                                                                
VREC0330 EQU   *                                                                
*                                                                               
         CLI   RDARBSTS,X'00'      AGY ORDER PRIOR STATUS?                      
         BE    VREC0440            NO  - DO OPEN OR REJECT                      
         CLI   RDARBSTS,C'A'       PRIOR STATUS = OPENED?                       
         BNE   VREC0360            NO  - REJECTED - NOT ALLOWED                 
         CLI   ACTCODE,C'R'        YES - THIS REQUEST = REJECT?                 
         BE    VREC0440            YES - PERMIT IT                              
         B     VREC0400            NO  - TRYING TO OPEN TWICE                   
VREC0360 EQU   *                                                                
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(425)    SET ERROR TYPE = REJECTED:                   
*                                     THIS ACTION IGNORED                       
         B     VREC1000                                                         
VREC0400 EQU   *                                                                
*        MVI   RMSGTYPE,C'E'                                                    
*        LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
*        MVC   RERROR,=AL2(424)    SET ERROR TYPE = OPENED:                     
*                                     CAN'T OPEN TWICE                          
         GOTO1 =A(DISPLNKS),RR=RELO                                             
         B     EXIT                                                             
*        B     VREC1000                                                         
VREC0440 EQU   *                                                                
         CLI   ACTCODE,C'R'        REJECT REQUEST?                              
         BNE   VREC0480            NO                                           
         LA    R6,AORREASH         YES - MUST HAVE AT LEAST 1 LINE              
         CLI   5(R6),0             ANYTHING ON FIRST LINE?                      
         BNE   VREC0480            YES                                          
         MVI   RMSGTYPE,C'E'                                                    
         LR    R2,R6               SET A(CURSOR)                                
         MVC   RERROR,=AL2(433)    SET ERROR TYPE = REJECTED:                   
*                                     REQUIRES SOME INPUT                       
         GOTO1 MYERROR                                                          
VREC0480 EQU   *                                                                
*                                                                               
*   SAVE EASI CODES BEFORE ANYTHING ELSE.  AGENCY HEADER RECORD                 
*     CONTAINS X'01' AND X'02' ELEMENT, SO ALL LABELS APPLY AT                  
*     THIS TIME.                                                                
*                                                                               
         MVC   EASIADV(4),RDARCLI  INSERT CLIENT/ADV CODE                       
*                                     CHOPPED TO 4 CHARS                        
         MVC   EASIPROD,RDARPRD1   INSERT PRODUCT CODE                          
         MVC   EASIPRD2,RDARPRD2   INSERT PRODUCT CODE 2                        
*                                                                               
         MVC   SAVEPROD,RDARPRN1   SAVE PRODUCT NAME(S)                         
         CLC   =C'$EDI$',RDARAGAD  KATZ EDI ONLY HAS ONE PRODUCT                
         BE    VREC0490                                                         
         CLC   RDARPRN2,SPACES                                                  
         BE    VREC0490                                                         
         MVI   SAVEPROD+10,C'/'                                                 
         MVC   SAVEPROD+11(9),RDARPRN2                                          
                                                                                
VREC0490 EQU   *                                                                
         EDIT  RDAREST#,(4,EASIEST#),FILL=0,ZERO=NOBLANK                        
*                                  INSERT ESTIMATE NUMBER, 3 CHARS              
         MVC   FLTDATES,RDARESST   SAVE FLIGHT START/END                        
         MVC   SAVEBUYR,RDARBUYR   SAVE BUYER NAME                              
*                                                                               
* SAVE OFF DEMO CATEGORIES TO LATER STORE IN CONTRACT RECORD                    
*                                                                               
         LR    RE,RA               CHECK TO SEE IF  LOCAL SIGN ON               
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    VREC0493                                                         
         DROP  RE                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    VREC0495            YES, DON'T CONSTRUCT DEMO ELE                
*                                                                               
VREC0493 EQU   *                                                                
         XC    WORK,WORK                                                        
         NI    MISCFLG2,X'FF'-MF2NODEM-MF2DEMCH-MF2NOADM-MF2NORDM               
*                                                                               
         GOTOR GETCOND                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   VREC0494            NO DEMO                                      
*                                                                               
         CLI   2(R6),C'('          TARGET DEMO IS A USER DEFINED DEMO?          
         BNE   *+12                                                             
         OI    MISCFLG2,MF2USRDM   YES,AND IF THERE IS NO REPDEMO               
         B     VR049405            THIS WILL SKIP ALL DEMO PROCESSING           
*                                  OTHERWISE, WE NEED TO BLOW ALL               
*                                  AGENCY DEMO AWAY                             
         ZIC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),SPACES      OR DEMO = SPACES                             
         BE    VREC0494                                                         
*                                                                               
         USING RDARDMEL,R6                                                      
         ZIC   R1,RDARDMLN                                                      
         SHI   R1,3                SUBTRACT OVERHEAD, AND 1 FOR EX              
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
*                                                                               
         LA    R1,L'RDARDEM1       MOVE AT LEAST ONE CATEGORY                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RDARDEM1                                                 
         OC    WORK,SPACES                                                      
*                                                                               
         GOTOR CONVDCAT            THERE IS AGENCY DEMO BUT                     
*                                                                               
         TM    MISCFLG2,MF2NORDM   THERE IS NO REP DEMO                         
         BZ    VR0493A                                                          
         CLC   ELTBUILD(L'RCONDDCT),SVDEMCAT+2                                  
         BE    VREC0495                                                         
*                                                                               
         OI    MISCFLG2,MF2DEMCH   TREAT IT LIKE A DEMO CAT CHANGE              
         B     VREC0495                                                         
*                                                                               
VR0493A  DS    0H                                                               
         CLC   ELTBILD2+2(L'RCONDDCT),SVDEMCAT+2                                
         BE    VREC0495            FIRST CAT CHANGED                            
         OI    MISCFLG2,MF2DEMCH   DEMO CAT IS CHANGED                          
         B     VREC0495                                                         
         DROP  R6                                                               
*                                                                               
VREC0494 DS    0H                  NO AGENCY DEMO                               
         OI    MISCFLG2,MF2NOADM                                                
*                                                                               
VR049405 DS    0H                                                               
         TM    MISCFLG2,MF2NORDM   NO REP DEMO                                  
         BZ    VR049410                                                         
         OI    MISCFLG2,MF2NODEM   NO REP+AGENCY DEMO                           
         B     VREC0495                                                         
*                                                                               
VR049410 DS    0H                                                               
         MVC   SVDEMCAT,ELTBILD2                                                
*                                                                               
VREC0495 EQU   *                                                                
*                                                                               
         L     R6,AIO                                                           
         USING RDARELEM,R6                                                      
*                                                                               
*   INSERT INFORMATION FROM KEY SECTION INTO RETURN MESSAGE                     
*      SAVE AREA                                                                
*                                                                               
         GOTO1 HEXOUT,DMCB,RDARKORD,RETORD#,4,=C'TOG'                           
*                                  INSERT ORDER #                               
         MVC   RETSTAT,RDARKSTA    INSERT STATION                               
         CLI   RETSTAT+4,C'L'      IS IT A TV STATION?                          
         BE    VREC0500            YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'V'      INSERT LAST CHAR OF MEDIA                    
         CLI   RETSTAT+4,C'T'      IS IT A TV STATION?                          
         BE    VREC0500            YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'M'      NO  - INSERT RADIO MEDIA                     
*                                                                               
VREC0500 EQU   *                                                                
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
*   SAVE HEADER INFORMATION FOR RETURN MESSAGE                                  
*                                                                               
         MVC   RETFROM(20),RDARSNDR                                             
*                                                                               
         CLC   =C'H7',RDARSNDR                                                  
         BNE   *+10                                                             
         MVC   RETFROM(2),=C'MS'                                                
*                                                                               
*                                  INSERT SENDER/RECEIVER                       
         GOTO1 HEXOUT,DMCB,RDARREP#,RETCON#,4,=C'TOG'                           
*                                  INSERT CONTRACT # (REP)                      
*                                                                               
         MVC   AORHDLN,RETCON#     PUT CONTRACT NUMBER ON SCREEN                
         LA    R2,AORHDLNH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         MVC   AORAORD,RETORD#     PUT AGY ORDER NUMBER ON SCREEN               
         LA    R2,AORAORDH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         MVC   RETSENDR,RDARRTS    INSERT 'RETURN TO SENDER' INFO               
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    VREC0520                                                         
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RDARREP#                                                 
         EDIT  (P5,WORK),(8,AORHDLN),ALIGN=LEFT                                 
         STC   R0,AORHDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   AORHDLNH+4,X'08'    SET VALID NUMERIC                            
         CLI   NEEDSCRN,C'Y'       IS SCREEN NEEDED?                            
         BNE   VREC0520            NO                                           
         CLI   ACTCODE,C'R'        YES - IS IT 'REJECT' ACTION?                 
         BNE   VREC0520            NO  - PROCEED                                
         LA    R2,AORREASH         SET CURSOR                                   
         MVC   RERROR,=AL2(112)    ERROR MESSAGE                                
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             EXIT VIA MYERROR                             
****     B     EXIT                                                             
                                                                                
VREC0520 DS    0H                                                               
         LA    R2,AORHDLNH         CONTRACT# ON SCREEN?                         
         CLI   5(R2),0                                                          
         BNE   VREC0560            YES                                          
         CLI   ACTCODE,C'R'        NO  - IS IT AN UNLINKED REJECT?              
         BE    VREC0600            YES                                          
         B     VREC0760            NO                                           
VREC0560 EQU   *                                                                
*                                                                               
* READ STATION RECORD FOR REVISION AUTO METHOD OVERRIDE, IF ANY                 
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+8(4),RDARKSTA                                           
         MVI   ELTBUILD+5,4                                                     
         CLI   RDARKSTA+3,C' '                                                  
         BNE   *+8                                                              
         MVI   ELTBUILD+5,3        IF 3 LETTER CALL LETTERS                     
         MVI   ELTBUILD+2,X'40'    SET ALPHA                                    
         CLI   RDARKSTA+4,C'T'      NO NEED FOR TV BAND                         
         BE    VREC0570                                                         
         MVI   ELTBUILD+12,C'-'                                                 
         MVC   ELTBUILD+13(1),RDARKSTA+4                                        
         MVI   ELTBUILD+5,6                                                     
         CLI   RDARKSTA+3,C' '                                                  
         BNE   VREC0570                                                         
         MVC   ELTBUILD+11(2),ELTBUILD+12                                       
         MVI   ELTBUILD+13,0       IF 3 LETTER CALL LETTERS                     
         MVI   ELTBUILD+5,5                                                     
         DROP  R6                                                               
                                                                                
VREC0570 EQU   *                                                                
         LA    R2,ELTBUILD                                                      
         GOTO1 VALISTA                                                          
                                                                                
         LA    R2,AORHDLNH                                                      
         GOTO1 VALICON,DMCB,(R2)   GET CONTRACT INFO AND STORE IT               
         GOTO1 =A(GETCON),DMCB,(RC),RR=RELO RETRIEVE CONTRACT RECORD            
         BZ    VREC0600            EXIT:  NO CONTRACT NUMBER                    
*                                  ERROR IN GETCON:  MESSAGE CODE               
*                                     ALREADY SET IN RERROR                     
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         GOTO1 MYERROR             EXIT WITH ERROR                              
*                                                                               
*   SET REPORT ID/CLASS FOR SPOOLING                                            
*                                                                               
VREC0600 EQU   *                                                                
         CLI   KATZEDI,C'Y'        SKIP SENDING APPROVAL NOTICE BACK TO         
         BE    VREC0690            THE AGENCY FOR KATZ EDI ORDERS               
*                                                                               
         MVC   REMUSER,=C'DAR'                                                  
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BNE   VREC0640            NO                                           
         MVC   PLDESC,=CL11'DARE APPRVL'                                        
         CLC   =C'PV',AGENCY       POETIC PETRY                                 
         BNE   VREC0630                                                         
         MVC   PLDESC(4),=C'POEM'                                               
VREC0630 EQU   *                                                                
         MVC   EDICTACT,=C'APP'    SET EDICT 'ACTION'                           
         B     VREC0680                                                         
VREC0640 EQU   *                                                                
         MVC   PLDESC,=CL11'DARE REJECT'                                        
         CLC   =C'PV',AGENCY       POETIC PETRY                                 
         BNE   VREC0650                                                         
         MVC   PLDESC(4),=C'POEM'                                               
VREC0650 EQU   *                                                                
         MVC   EDICTACT,=C'REJ'    SET EDICT 'ACTION'                           
VREC0680 EQU   *                                                                
         OI    GENSTAT3,NOCLRSPK                                                
         MVI   PLCLASS,C'G'        CLASS G                                      
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    RE,SPLKEYAD         SET EXTENDED KEY ADDRESS                     
*                                                                               
*                                                                               
         ST    RE,SPOOLQLK         SAVE EXTENDED KEY ADDRESS                    
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         DROP  RE                                                               
*                                                                               
*        GOTO1 OPENPQ                                                           
*        GOTO1 =A(EDICT),DMCB,(R8),(R5),RR=RELO                                 
*                                  PUT OUT EDICT HEADER                         
VREC0690 EQU   *                                                                
         GOTO1 =A(DATETIME),DMCB,(R5),RR=RELO                                   
*                                  SET UP DATE/TIMES FOR STAMP                  
         LA    R2,CONACTH          DETERMINE ACTION                             
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                SET FOR EX STATEMENT                         
         EX    RF,VREC1200         COMPARE TO 'OPEN'                            
         BE    VREC0720            OPEN:     PROCESS ORDER                      
         B     VREC0840            REJECT:   CYCLE SCREEN ENTRIES               
*                                                                               
VREC0720 EQU   *                   PROCCESS REVISION ORDERS                     
         BAS   RE,SORTBUYS                                                      
*                                                                               
         BAS   RE,AGYPROC                                                       
         GOTOR DEMOPROC            PROCESS DEMO                                 
*                                                                               
         TM    MISCFLAG,MFMANUAL                                                
         BNZ   VREC0740                                                         
*                                                                               
* SPECIAL TRAP FOR AUTO-APPROVAL TOTALS NOT MATCHING                            
*                                                                               
         GOTO1 =A(TRAP),RR=RELO                                                 
*                                                                               
*  FOR AUTO METHOD ONLY, DELETE CFC COMMENT, IF ANY                             
*                                                                               
         GOTO1 =A(DELCFC),RR=RELO                                               
*                                                                               
*                                                                               
VREC0740 EQU   *                                                                
         EDIT  ORDTOT$$,(10,AORCTOT),2                                          
*                                  INSERT TOTAL $$ ON SCREEN                    
         LA    R2,AORCTOTH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         EDIT  ORDTOTSP,(6,AORTSPT),ZERO=NOBLANK                                
*                                  INSERT TOTAL SPOTS ON SCREEN                 
         LA    R2,AORTSPTH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     VREC0800            EXIT:  BUYS GENERATED                        
                                                                                
                                                                                
VREC0760 DS    0H                                                               
*                                  SEND MESSAGE: NO CONTRACT                    
         B     VREC0920                                                         
VREC0800 DS    0H                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    VREC0880                                                         
*                                  SEND MESSAGE: ORDER OPENED                   
         GOTO1 OPENPQ                                                           
         GOTO1 =A(EDICT),DMCB,(R8),(R5),RR=RELO                                 
*                                  PUT OUT EDICT HEADER                         
         GOTO1 =A(APPRREJC),DMCB,(R8),(R5),RR=RELO                              
         B     VREC0880            FINISHED - EXIT                              
*                                                                               
VREC0840 EQU   *                                                                
*                                  SEND MESSAGE: ORDER REJECTED                 
         GOTO1 OPENPQ                                                           
         GOTO1 =A(EDICT),DMCB,(R8),(R5),RR=RELO                                 
         GOTO1 =A(APPRREJC),DMCB,(R8),(R5),RR=RELO                              
         GOTO1 =A(REJCMSGS),DMCB,(R8),(R5),RR=RELO                              
*                                  REJECT ACTIVITY                              
VREC0880 DS    0H                                                               
*                                                                               
*                                  RETRIEVE HEADER OF AGY ORD                   
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         MVC   RDARBSTS,ACTCODE    SET STATUS TO ACTCODE                        
         CLI   ACTCODE,C'A'        ORDER OPENED?                                
         BNE   VREC0900            NO  -                                        
         OI    RDARMISC,X'40'      YES - SET 'OPENED AT LEAST ONCE'             
*                                                                               
VREC0900 EQU   *                                                                
         CLI   ACTCODE,C'R'        ORDER REJECTED?                              
         BNE   VREC0910            NO  -                                        
*                                                                               
         GOTO1 =A(ADDREJS),DMCB,(R5),(R4),RR=RELO                               
VREC0910 EQU   *                                                                
         GOTO1 PUTREC              REWRITE RECORD WITH NEW STATUS               
*                                                                               
         GOTO1 =A(DOAUDIT),DMCB,(RC),(R5),(R4),RR=RELO                          
*                                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    VREC0960                                                         
*                                  SEND MESSAGE: ORDER OPENED                   
*                       1.3.5.7.9.1.3.5.7.9.1.3.5.                              
*        MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
*                                  SEND SPECIAL PRINT LINE                      
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        MVI   LINE,1              FORCE TO SINGLE PAGE                         
         DROP  R4                                                               
*                                                                               
VREC0920 DS    0H                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
* FOR REJECT, AUTOMATICALLY GENERATE WORKSHEET                                  
*                                                                               
         CLI   ACTCODE,C'R'        REJECT?                                      
         BNE   VREC0960                                                         
         GOTO1 VLOAD,DMCB,(X'30',0),('QPRONE',0)                                
*        GOTO1 VREDAR30,DMCB,(RC),('QPRONE',0)                                  
         B     EXIT                WANT TO SHOW REPORT ID ON CONMSG             
*                                  INSTEAD OF REJECT MESSAGE                    
VREC0960 DS    0H                                                               
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS OPEN, ADD A PASSIVE KEY             
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
         CLI   ACTCODE,C'A'        FOR ACTION OPEN                              
         BNE   VREC0970                                                         
         CLI   KATZEDI,C'Y'        AND KATZ EDI ORDERS                          
         BNE   VREC0970                                                         
         GOTO1 =A(GENEDIKY),RR=RELO                                             
*                                                                               
VREC0970 DS    0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(113)    SET ERROR TYPE = OPENED                      
         CLI   ACTCODE,C'A'        OPENED?                                      
         BE    VREC1000            YES                                          
         MVC   RERROR,=AL2(114)    SET ERROR TYPE = REJECTED                    
*                                                                               
VREC1000 EQU   *                                                                
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
*                                                                               
VREC1200 CLC   8(0,R2),=C'OPEN'                                                 
VREC1240 CLC   8(0,R2),=C'REJECT'                                               
VREC1280 MVC   8(0,R2),0(R3)                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*   GETCON:  RETRIEVE THE CONTRACT RECORD FOR PROCESSING.                       
*        STORE IT IN ALTERNATE IO AREA 3 (AIO3).                                
*                                                                               
NOCONREC EQU   82                                                               
*                                                                               
GETCON   NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE) IF NMOD NEEDED            
         LA    R2,AORHDLNH         CONVERT HEADLINE #                           
         LA    R3,INVALID          INVALID INPUT ERROR MSG                      
         MVC   AIO,AIO3            SET ALT IO AREA 3                            
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         GOTO1 SCANNER,DMCB,(R2),(1,RCONREC),0                                  
         CLI   4(R1),0                                                          
         BE    ERREXIT                                                          
         LA    R4,RCONREC          POINT TO SCANNER BLOCK                       
         CLI   0(R4),0                                                          
         BE    ERREXIT                                                          
         CLI   0(R4),8             K NUMBER UP TO 8 DIGITS                      
         BH    ERREXIT                                                          
         TM    2(R4),X'80'         TEST FOR NUMERIC INPUT                       
         BZ    ERREXIT                                                          
         ICM   R0,15,4(R4)                                                      
         CVD   R0,DUB                                                           
         SPACE                                                                  
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
*                                  CALCULATE 9'S COMP                           
         MVO   WORK(5),WORK+10(5)                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,TWAAGY     INSERT POWER CODE                            
         MVC   RCONPCON,WORK       CHECK IF NUMBER IS ON FILE                   
         MVC   COMPCON,WORK        REVERSE THE COMPLEMENT                       
         PACK  COMPCON+0(1),WORK+3(1)                                           
         PACK  COMPCON+1(1),WORK+2(1)                                           
         PACK  COMPCON+2(1),WORK+1(1)                                           
         PACK  COMPCON+3(1),WORK+0(1)                                           
         MVC   KEY,RCONREC                                                      
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETC0040            RECORD FOUND                                 
         MVC   RERROR,=AL2(031)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
***>>>   LA    R3,NOCONREC         NO CONTRACT RECORD FOUND!!                   
***>>>   B     GETC0370            OK EXIT ANYWAY, IN CASE WE DO UNDARE         
*                                  FOR CONTRACTS THAT HAVE BEEN PURGED          
GETC0040 EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
*                                                                               
***>>>                                                                          
*                                                                               
*                                  DELETE  ALT CAL CTL  ELT                     
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),TWAAGY                                                 
*                                                                               
         GOTOX (RFVALTCL,VREPFACS),DMCB,(X'FF',RCONREC),GETBROAD,0,WORK         
         BE    GETC0140            ALT CALENDAR(S) TEST PASSED                  
         MVC   RERROR,=AL2(780)    SET ERROR MESSAGE                            
         B     ERREXIT             EXIT CC NOT ZERO                             
*                                                                               
GETC0140 EQU   *                                                                
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
*                                  SET ALT CAL FLAGS FOR STATION                
***>>>                                                                          
         CLI   ACTCODE,C'R'        'REJECT' ACTION?                             
         BE    GETC0360            YES - DON'T DUMP BUYS                        
         DROP  R4                                                               
*                                                                               
         NI    MISCFLAG,X'FF'-MFMANUAL                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BAS   RE,GETEL                                                         
         BNE   GETC0050                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GETC0050                                                         
         TM    RCONDRF2,X'04'                                                   
         BZ    GETC0050                                                         
         OI    MISCFLAG,MFMANUAL   SET MANUAL CHANGES STARTED                   
         DROP  R6                                                               
*                                                                               
GETC0050 EQU   *                                                                
         BAS   RE,GETVERSN         RETRIEVE VERSION NUMBER                      
*                                                                               
* CALL TO REGENVER MIGHT HAVE FLAGGED CONTRACT UPDATED MANUALLY                 
* IF SO, RESET MANUAL FLAG BACK IF CONTRACT WAS NOT PREVIOUSLY                  
* UPDATE MANUALLY                                                               
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BAS   RE,GETEL                                                         
         BNE   GETC0060                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GETC0060                                                         
         TM    MISCFLAG,MFMANUAL                                                
         BNZ   GETC0060                                                         
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R6                                                               
*                                                                               
GETC0060 EQU   *                                                                
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
*                                                                               
*   RETRIEVE REP RECORD IN ALL CASES TO GET DAILY PACING PROFILE                
*                                                                               
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
*                                     FOR SPOT CODES                            
         L     R2,AIO                                                           
         USING RREPRECD,R2                                                      
*        XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,1           INSERT RECORD CODE                           
         MVC   RREPKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RREPKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         MVC   DAILYFLG,RREPPROF+27                                             
*                                  SAVE DAILY PACING FLAG                       
*                                                                               
         CLI   RCONTYPE,C'N'       REP-TO-SPOT TRANSFER NEEDED?                 
         BE    GETC0160            YES                                          
         CLI   RCONTYPE,C'X'       REP-TO-SPOT TRANSFER NEEDED?                 
         BNE   GETC0360            NO                                           
GETC0160 EQU   *                                                                
         MVC   IFELCODE,=X'0830'   INSERT CODE/LENGTH                           
*                                                                               
         LA    R3,RREPELEM         FIND X'05' ELEMENT                           
GETC0200 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                NO SPOT INTERFACE CODES!!                    
         CLI   0(R3),X'05'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0240            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,R4                                                            
         B     GETC0200            GO BACK FOR NEXT                             
GETC0240 EQU   *                                                                
         MVC   IFSPAG,RREPSPPC-RREPSPOT(R3)                                     
*                                  INSERT SPOT AGENCY CODE                      
         MVC   IFSPMD,RREPMED-RREPSPOT(R3)                                      
*                                  INSERT SPOT MEDIA                            
         DROP R2                                                                
         USING RPRDREC,R2                                                       
*        XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           INSERT RECORD CODE                           
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RPRDKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         LA    R3,RPRDELEM         FIND X'03' ELEMENT                           
GETC0280 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                NO SPOT INTERFACE CODES!!                    
         CLI   0(R3),X'03'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0320            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,R4                                                            
         B     GETC0280            GO BACK FOR NEXT                             
GETC0320 EQU   *                                                                
         MVC   IFSPCL,RPRDSPCL-RPRDSPOT(R3)                                     
*                                  INSERT SPOT CLIENT CODE                      
         MVC   IFSPPRD,RPRDSPP1-RPRDSPOT(R3)                                    
*                                  INSERT SPOT PRODUCT CODE                     
         MVC   IFSPES,RPRDSPES-RPRDSPOT(R3)                                     
*                                  INSERT SPOT ESTIMATE NUMBER                  
         MVC   IFSPPP,RPRDSPP2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY                    
         MVC   IFSPP1,RPRDSPS1-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 1            
         MVC   IFSPP2,RPRDSPS2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 2            
         MVC   IFSPST,RCONKSTA     INSERT STATION CALL LETTERS                  
         MVC   IFSPADV,RCONKADV    INSERT REP ADVERTISER                        
         MVC   IFSPRD,RCONPRD      INSERT REP PRODUCT CODE                      
GETC0360 EQU   *                                                                
         MVC   CONMOD#,RCONMOD     SAVE MOD #                                   
*                                                                               
GETC0370 EQU   *                                                                
         SR    RC,RC               SET CC ZERO - CLEAN END                      
ERREXIT  LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*&&DO                                                                           
*   DUMPBUYS: AS A WHOLE ORDER IS BEING PROCESSED AT ONE TIME, THE              
*        BUYS FOR IT HAVE TO BE DELETED.  ALSO, THE DOLLARS FOR                 
*        THOSE BUYS HAVE TO BE BACKED OUT OF THE CONTRACT, FOR                  
*        PACING PURPOSES.  AT A LATER DATE, WHEN ''CHANGES'' ARE                
*        PROCESSED, THIS ROUTINE WILL HAVE TO BE MODIFIED, OR                   
*        DROPPED COMPLETELY.                                                    
*                                                                               
DUMPBUYS NTR1                                                                   
         MVC   AIO,AIO2            SET IO AREA FOR READING                      
         L     R4,AIO2             SET WORK AREA FOR BUYS                       
         USING RBUYREC,R4                                                       
*                                                                               
         NI    DMINBTS,X'FF'-X'08' TURN OFF 'RETURN DELETES'                    
         XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         MVC   KEY(27),RBUYREC     RETRIEVE KEY: ACTIVE ONLY                    
         GOTO1 HIGH                                                             
         B     DBUY0040                                                         
DBUY0020 EQU   *                                                                
         GOTO1 SEQ                 RETRIEVE NEXT BUY                            
DBUY0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME ORDER?                                  
         BNE   DBUY0400            NO  - FINISHED                               
         GOTO1 GETREC              RETRIEVE RECORD                              
         OI    RBUYCNTL,X'80'      MARK RECORD DELETED                          
*                                                                               
*   MUST MARK BUY AS DELETED, OR, IF SENT TO STATION, AS CANCELLED.             
*     TEST OF VERSION NUMBERS DETERMINES WHETHER BUY SENT OR NOT.               
*                                                                               
         MVI   RBUYCHGI,C'X'       SET CHANGE TO 'DELETED'                      
         CLC   RBUYVER,VERDFLT     BUY VERSION VS CONTRACT VERSION              
         BNL   DBUY0060            BUY VER LESS THAN CONTRACT VERSION           
         MVI   RBUYCHGI,C'C'       SET CHANGE TO 'CANCELLED'                    
DBUY0060 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RBUYCHGD)  UPDATE LAST CHANGE DATE          
                                                                                
         L     RF,AIO3             GET CONTRACT RECORD IOAREA                   
         USING RCONREC,RF                                                       
         TM    RCONMODR+1,X'C0'    ACE OR GRAPHNET                              
         BZ    DBUY0070                                                         
         DROP  RF                                                               
                                                                                
         MVC   RBUYVER,VERDFLT     UPDATE VERSION NUMBER                        
                                                                                
DBUY0070 EQU   *                                                                
         GOTO1 PUTREC              REWRITE BUY AS DELETED                       
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  BACK OUT BUCKETS                             
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 WRITE               REWRITE DELETED KEY                          
         B     DBUY0020            GO BACK FOR NEXT                             
DBUY0400 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
*                                                                               
*    GETVERSN:  RETRIEVE X'20' ELEMENT FROM CONTRACT, SAVE THE                  
*        REP VERSION NUMBER.  BUMP THE NUMBER IF CONTRACT IS                    
*       'RESENT'.                                                               
*        TO SUPPORT WIP IN CONTRACT, THE NEXT REP VERSION NUMBER MUST           
*        BE THE HIGHER OF CURRENT REP NUMBER PLUS 2 *OR* CURRENT STA            
*        NUMBER PLUS 1.                                                         
*                                                                               
GETVERSN NTR1                                                                   
         MVI   VERDFLT,1           SET REP VERSION DEFAULT                      
*                                                                               
         L     R6,AIO3             SET WORK AREA FOR BUYS                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETVX                                                            
*                                  SAVE THE REP VERSION NUMBER                  
         CLI   RESENT,C'Y'         CONTRACT IS RESENT?                          
         BE    GETV0010            YES - NEED VERSION BUMP                      
*                                                                               
         L     R4,AIO1                                                          
         USING RDARREC,R4                                                       
         TM    RDARMISC,X'10'      IF VARIOUS, THIS MIGHT BE A                  
         BZ    GETV0100            POOL RESENT AFTER ALL                        
         DROP  R4                                                               
                                                                                
* BUMP REP VERSION IF REP VERSION WAS NOT ADVANCED, ELSE EXIT                   
GETV0010 EQU   *                                                                
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'20'                                                   
         BZ    GETV0100                                                         
         DROP  R6                                                               
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
*                                                                               
GETV0100 EQU   *                                                                
         L     R6,AIO3             SET WORK AREA FOR BUYS                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETVX                                                            
         USING RCONSEND,R6                                                      
         MVC   VERDFLT,RCONSRV                                                  
         DROP  R6                                                               
*                                                                               
GETVX    EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SORT CONTRACT BUYS BY AGENCY BUY NUMBER AND CONTRACT BUY NUMBER               
* USES IO2 AS TEMPORARY IO AREA                                                 
* NOTE: FOR MAKEGOODS/CREDIT, EACH K BUY ENTRY USES THE AGENCY LINK             
* FROM THE BUY BEFORE IT. THIS WILL ONLY WORK FOR ONE LEVEL MAKEGOOD,           
* SINCE THE BUYS ARE SEQUENCED ONE AFTER THE OTHER. THIS WILL NOT WORK          
* FOR MULTI-LEVEL MAKEGOOD BUYS                                                 
***********************************************************************         
SORTBUYS NTR1                                                                   
         SR    R3,R3                                                            
         LA    R4,SORTAREA                                                      
         XCEFL SORTAREA,2300                                                    
         USING SORTD,R4                                                         
                                                                                
         XC    KEY,KEY             CONSTRUCT CONTRACT BUY KEY                   
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         MVC   RBUYKCON,WORK                                                    
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'  DON'T PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BNE   SORTBX                                                           
                                                                                
         MVC   AIO,AIO2                                                         
*                                                                               
SORTB10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
*                                                                               
* FLAG IF CONTRACT BUY HAS ZERO TOTAL SPOTS                                     
*                                                                               
         OC    RBUYTSPT,RBUYTSPT                                                
         BNZ   *+8                                                              
         OI    SRTFLG2,SF20SPTS                                                 
*                                                                               
* FLAG IF CONTRACT BUY HAS ZERO TOTAL DOLLARS                                   
*                                                                               
         OC    RBUYTCOS,RBUYTCOS                                                
         BNZ   *+8                                                              
         OI    SRTFLG2,SF20CSTS                                                 
*                                                                               
* FLAG IF CONTRACT BUY HAS BEEN MANUALL CANCELLED BY THE REP                    
*                                                                               
         CLI   RBUYCHGI,C'C'                                                    
         BNE   *+8                                                              
         OI    SRTFLG2,SF2MCAN                                                  
*                                                                               
         MVC   SRTAGYBY,RBUYAGBL                                                
         OC    SRTAGYBY,SRTAGYBY                                                
         BNZ   *+8                                                              
         MVI   SRTAGYBY,X'FF'      PUT MANUALLY ADDED BUYS IN THE END           
         MVC   SRTCONBY,RBUYKLIN   BUY LINE                                     
         MVC   SRTTKBUY,RBUYKMLN   TARGET BUY (IF MAKEGOOD/CREDIT)              
         MVC   SRTBUYDA,KEY+28                                                  
*                                                                               
         MVI   ELCODE,X'04'        CHECK IF MAKEGOOD/CREDIT                     
         BAS   RE,GETEL                                                         
         BNE   SORTB40                                                          
*                                                                               
         USING RBUYCMEL,R6                                                      
         CLC   =C'MG=',RBUYCMNT                                                 
         BNE   SORTB20                                                          
         OI    SRTFLAGS,SFMKGOOD   FLAG AS MAKEGOOD                             
         MVI   SRTAGYBY,X'FF'      NO LINK FOR MAKEGOODS, REMATCH               
*                                  EVERYTIME                                    
         B     SORTB40                                                          
*                                                                               
SORTB20  DS    0H                                                               
         CLC   =C'MG>',RBUYCMNT    INCASE OF TAKEOVER CHOPPING EARLIER          
         BNE   SORTB30             TARGET DATE                                  
         OI    SRTFLAGS,SFTKMKGD   FLAG AS TAKEOVER MAKEGOOD                    
         MVI   SRTAGYBY,X'FF'      NO LINK FOR MAKEGOODS, REMATCH               
*                                  EVERYTIME                                    
         B     SORTB40                                                          
*                                                                               
SORTB30  DS    0H                                                               
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   SORTB40                                                          
         OI    SRTFLAGS,SFCREDIT   FLAG AS CREDIT                               
*                                                                               
SORTB40  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   SORTB50                                                          
         OI    SRTFLAGS,SFTCRDT    BUY IS A TARGET CREDIT LINE                  
*                                                                               
SORTB50  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'56'                                                     
         BAS   RE,GETEL                                                         
         BNE   SORTB100                                                         
         OI    SRTFLAGS,SFTMKGD    BUY IS A TARGET MAKEGOOD LINE                
*                                                                               
SORTB100 DS    0H                  FIND LINK FOR MAKEGOODS                      
         TM    SRTFLAGS,SFMKGOOD+SFTKMKGD                                       
         BZ    SORTB200                                                         
*                                                                               
         LA    R2,SORTAREA         FIND TARGET AGENCY BUY                       
*                                                                               
SORTB110 DS    0H                                                               
         CLI   0(R2),0             SKIP FOR FIRST ENTRY                         
         BE    SORTB200                                                         
*                                                                               
SORTB130 DS    0H                                                               
         CLC   SRTTKBUY,1(R2)      FIND TARGET LINE                             
         BNE   SORTB150                                                         
         CLC   SRTTKBUY,2(R2)      FIND TARGET LINE                             
         BNE   SORTB150                                                         
         MVC   SRTAGYBY,0(R2)      COPY AGENCY LINK                             
         B     SORTB200                                                         
*                                                                               
* MAKEGOOD FOR MAKEGOODS WILL GET LINKED LATER                                  
*                                                                               
SORTB150 DS    0H                                                               
         LA    R2,SORTDLQ(R2)                                                   
         B     SORTB110                                                         
*                                                                               
SORTB200 DS    0H                                                               
         LA    R4,SORTDLQ(R4)                                                   
         LA    R3,1(R3)            NUMBER OF SORT RECORDS                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(22),KEYSAVE                                                  
         BE    SORTB10                                                          
         DROP  R6                                                               
                                                                                
         STC   R3,SORTRECS                                                      
         GOTO1 XSORT,DMCB,SORTAREA,(R3),9,3,0                                   
                                                                                
SORTBX   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   AGYPROC:  PROCESS THE AGENCY ORDER.  FIRST RECORD HAS BEEN                  
*        DELIVERED INTO THE PRIMARY IOAREA (AIO1).                              
*        CONSTRUCT BUYLINES IN ALTERNATE IO AREA 2 (AIO2).                      
*        UPDATE CONTRACT HEADER BUCKETS.  CONTRACT RECORD IS IN                 
*            ALTERNATE IO AREA 3 (AIO3).                                        
*                                                                               
***********************************************************************         
AGYPROC  NTR1                                                                   
*                                                                               
*        MVI   APPRBUY#,0          INITIALIZE                                   
         NI    MISCFLAG,X'FF'-MFCONOK INIT CONTRACT UPDATED FLAG                
*                                                                               
         L     R4,AIO2             SET WORK AREA FOR BUYS                       
         USING RBUYREC,R4                                                       
*                                                                               
         MVC   AIO,AIO1            SET IO AREA FOR X'41' RECS                   
         L     R3,AIO                                                           
         USING RDARREC,R3                                                       
*                                                                               
         GOTO1 =A(SETUPHIA),RR=RELO                                             
*                                  RESET HIATUS DATA, IF ANY                    
         MVI   RDARKRT,X'40'       SET REC TYPE TO 'BUY'                        
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SEQ # AND SUB TYPE                     
         MVC   KEY,RDARKEY         RETRIEVE FIRST BUY HEADER                    
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH REC TYPE                   
         BE    APRO0020            YES                                          
*                                                                               
* IF NO BUYS AT ALL, AGENCY MUST HAVE SENT AN 'EMPTY ORDER'. IF IN              
* AUTOMATIC MODE, WE NEED TO CANCEL ALL BUYS. IF MANUAL, CHECK IF               
* ANY BUYS ARE LEFT IN THE CONTRACT                                             
*                                                                               
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BNZ   APRO0235                                                         
*                                                                               
APRO0010 EQU   *                                                                
*        GOTO1 =A(CANKBUY),RR=RELO CANCEL UNMATCHED CONTRACT BUYS               
         B     APRO0230            FINISHED WITH GENERATION                     
*                                                                               
APRO0020 EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         MVI   BUYUPDAT,C'Y'       SET 'WRITE OUTPUT RECORD'                    
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(R3),(R5),RR=RELO                              
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
APRO0040 EQU   *                                                                
         MVI   ORBSTDAY,0          CLEAR 'ORBIT START DAY'                      
         MVI   ORBFLAG,C'N'        SET   'ORBIT NOT PRESENT'                    
         MVI   DETLFLAG,C'N'       CLEAR COUNT OF DETAILS                       
         XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         MVC   RBUYKPLN,=X'FFFFFF' INSERT PLAN CODE                             
         ZIC   RF,BUYLINE#         GET NEXT BUYLINE NUMBER                      
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
         STC   RF,RBUYKMLN         INSERT MASTER LINE NUMBER                    
         STC   RF,RBUYKLIN         INSERT LINE NUMBER                           
         MVC   RBUYLEN,=X'004D'    INSERT RECORD LENGTH:                        
*                                     34+43 = 77 = X'4D'                        
         MVC   RBUYELEM(2),=X'012B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         LA    R6,RDARELEM                                                      
         USING RDARBYEL,R6                                                      
         MVC   PROGNAME,RDARBYPN   SAVE BUY HDR PROGRAM NAME                    
         MVC   RBUYCOS,RDARBYCO    INSERT COST                                  
         MVC   BUYCOST,RDARBYCO    SAVE COST FOR CALCULATIONS                   
         MVC   RBUYCLS(6),SPACES   SET CLASS/SECTION TO SPACES                  
         GOTO1 DATCON,DMCB,(5,WORK),(3,RBUYCREA)                                
*                                  INSERT CREATION DATE                         
         MVC   RBUYKMOD,CONMOD#    INSERT CONTRACT MOD #                        
         MVI   RBUYCHGI,C'A'       INSERT CHANGE INDICATOR                      
         MVC   RBUYAGBL,RDARKSEQ   INSERT AGENCY BUY LINE NUMBER                
*                                                                               
         TM    MISCFLG2,MF2TRADE   SET TRADE FLAG                               
         BZ    *+8                                                              
         OI    RBUYFLG2,X'02'                                                   
*                                                                               
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS OF WEEK                 
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,RBUYSTED,(R5),RR=RELO                 
*                                  CONVERT ONE-BYTE START/END DATE              
         GOTO1 EFFDATEL,DMCB,(RC),RDARREC                                       
*                                                                               
         MVC   RBUYDUR,RDARBYSL    INSERT TOTAL SPOT LENGTH                     
         CLI   RDARBYSL,C'M'       LENGTH IN MINUTES?                           
         BNE   APRO0060            NO                                           
         OI    RBUYDUR,X'80'       YES - SET 'MINUTES' INDICATOR                
APRO0060 EQU   *                                                                
*                                                                               
         MVC   RBUYVER,VERDFLT     INSERT VERSION NUMBER                        
*                                     FROM REP VERSION NUMBER                   
         OC    IFELCODE(IFELLEN),IFELCODE SPOTPAK XFER ELEMENT?                 
         BZ    APRO0070            NO                                           
         GOTO1 LOADELT,DMCB,(R4),IFELCODE,=C'ADD=CODE'                          
         DROP  R6                                                               
*                                                                               
APRO0070 EQU   *                   SAVE OFF DEMO VALUES, IF ANY                 
*                                                                               
         L     R6,AIO              IF AGENCY BUY IS A MAKEGOOD                  
         MVI   AGBUYMTR,0                                                       
         MVI   ELCODE,X'10'        NEED TO SAVE OFF TARGET/MASTER               
         BAS   RE,GETEL            AGENCY BUY FOR BETTER MATCHING               
         BNE   APRO0080                                                         
         USING RDARRVEL,R6                                                      
         MVC   AGBUYMTR,RDARRVBY                                                
         DROP  R6                                                               
*                                                                               
APRO0080 EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 SEQ                 ACCESS NEXT X'41' RECORD                     
         MVC   SAVEKEY,KEY                                                      
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH RECORD TYPE?               
         BE    APRO0120            YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   APRO0230            NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    APRO0230            YES - ALREADY WRITTEN                        
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
* GO AND MATCH THE MANUALLY UPDATED/AUTOMATIC CONTRACT BUYS                     
*                                                                               
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BZ    APRO0090                                                         
         GOTO1 =A(MANMATCH),RR=RELO                                             
*                                  CHECK IF ALL CONTRACT BUYS WERE              
         B     APRO0235            MATCHED UP WITH ALL AGENCY BUYS              
*                                                                               
APRO0090 EQU   *                                                                
         GOTOR AUTOGEN                                                          
*                                  NO  - OUTPUT PREVIOUS RECORD                 
*                                                                               
*        GOTO1 =A(CANKBUY),RR=RELO CANCEL UNMATCHED CONTRACT BUYS               
         B     APRO0230            FINISHED WITH GENERATION                     
*                                                                               
APRO0120 EQU   *                                                                
         CLC   KEY+25(1),KEYSAVE+25                                             
*                                  SAME BUYLINE #?                              
         BE    APRO0140            YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   APRO0140            NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   APRO0130            NO  - GO PUT IT OUT                          
*                                                                               
*   NOTE:  'DAILY'S ARE GENERATED FROM WITHIN THE BUYDETL ROUTINE.              
*      AS SUCH, THE BUYLINE# IS BUMPED AFTER THE RECORD IS WRITTEN.             
*      WHEN THE AGENCY BUY IS COMPLETED, THE BUYLINE # IS SET TO                
*      THE NEXT EXPECTED BUYLINE.  TO ENSURE THAT IT IS NOT DOUBLE-             
*      INCREMENTED, IT IS BACKED OFF HERE.                                      
*                                                                               
         ZIC   RF,BUYLINE#         YES - DROP BUYLINE # BY 1                    
         BCTR  RF,0                                                             
         STC   RF,BUYLINE#                                                      
         B     APRO0140            DON'T PUT IT OUT AGAIN                       
APRO0130 EQU   *                                                                
*                                                                               
* GO AND MATCH THE MANUALLY UPDATED CONTRACT BUYS                               
*                                                                               
*        MVC   SAVEKEY,KEY                                                      
*                                                                               
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BZ    APRO0135                                                         
         GOTO1 =A(MANMATCH),RR=RELO                                             
         B     APRO0138                                                         
*                                                                               
APRO0135 EQU   *                                                                
         GOTO1 =A(AUTOGEN),RR=RELO                                              
*                                                                               
APRO0138 EQU   *                                                                
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 =A(GENREC),RR=RELO                                               
*                                  NO  - OUTPUT PREVIOUS RECORD                 
APRO0140 EQU   *                                                                
         MVC   AIO,AIO1            SET A(IO AREA 1)                             
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(R3),(R5),RR=RELO                              
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
*                                                                               
         CLI   KEY+26,X'00'        BUY HEADER?                                  
         BE    APRO0040            YES - START NEXT HEADER                      
         CLI   KEY+26,X'10'        BUY ORBIT?                                   
         BNE   APRO0160            NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         GOTO1 BUYORBIT,DMCB,(RC),(R3)                                          
*                                  PROCESS BUY ORBIT RECORD                     
         B     APRO0080            GO BACK FOR NEXT RECORD                      
APRO0160 EQU   *                                                                
         CLI   KEY+26,X'20'        BUY COMMENT?                                 
         BNE   APRO0200            NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         GOTO1 BUYCOMMT,DMCB,(RC),(R3)                                          
*                                  PROCESS BUY COMMENT RECORD                   
         B     APRO0080            GO BACK FOR NEXT RECORD                      
APRO0200 EQU   *                                                                
         CLI   KEY+26,X'30'        BUY DETAIL?                                  
         BE    *+6                 NO  - ONLY TRAILER LEFT                      
         DC    H'0'                UNKNOWN RECORD SUBTYPE                       
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    APRO0220            NO  - DON'T NEED COMMENT                     
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   APRO0210            WITH THE LAST 2 FOR DAYPART                  
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    APRO0220            NO  - DON'T NEED COMMENT                     
APRO0210 DS    0H                                                               
         GOTO1 =A(GENCOMMT),DMCB,RBUYREC,(R5),RR=RELO                           
APRO0220 EQU   *                                                                
         MVC   SAVEKEY,KEY                                                      
         GOTO1 =A(BUYDETL),DMCB,(R3),RR=RELO                                    
         BZ    APRO0225                                                         
         MVI   BUYUPDAT,C'N'                                                    
         B     APRO0080                                                         
*                                                                               
APRO0225 DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     APRO0080            GO BACK FOR NEXT RECORD                      
         DROP  R2,R3,R4                                                         
*                                                                               
* DATE/TIME STAMP CONTRACT AND WRITE IT OUT                                     
*                                                                               
APRO0230 EQU   *                                                                
         TM    MISCFLAG,MFMANUAL   FOR AUTOMATIC??                              
         BNZ   APRO0235                                                         
         GOTO1 =A(CANKBUY),RR=RELO CANCEL UNMATCHED CONTRACT BUYS               
         B     APRO0238                                                         
*                                                                               
APRO0235 EQU   *                   MANUAL ONLY, CHECK ANY K BUYS LEFT           
         BAS   RE,CHECKMTC                                                      
*                                                                               
APRO0238 EQU   *                                                                
         MVI   FLTKEYFG,C'N'       DEFAULT DON'T REWRITE 8D/8E POINTERS         
*                                  OVERRIDE REP CON'S FLIGHT DATES              
*                                     WITH AGENCY ORDER'S DATES                 
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         GOTO1 DATCON,(R1),(2,FLTDATES+2),(3,WORK+3)                            
*                                                                               
         L     R6,AIO3                                                          
         USING RCONREC,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,CONFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,CONFLTDT+2)                        
*                                  SAVE ORIGINAL CONTRACT DATES                 
         CLC   RCONDATE,WORK       IF DIFFERENT, FLAG TO UPDATE                 
         BE    APRO0240            8E/8D KEYS TO CONTRACT                       
*                                                                               
         MVC   RCONDATE,WORK                                                    
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
APRO0240 EQU   *                                                                
         CLC   RTKODATE,RCONDATE   DATE CHOPPING??                              
         BNH   APRO0250                                                         
*                                                                               
         MVC   RCONDATE(3),RTKODATE                                             
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
APRO0250 EQU   *                   OVERRIDE BUYER NAME WITH A/O NAME            
         MVC   RCONBUYR(20),SAVEBUYR                                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         OI    RCONDRFG,X'40'      SET OPENED AND NOT REJECT/RECALL             
         NI    RCONDRFG,X'FF'-X'20'-X'10'                                       
         MVC   RCONDRDA,ACTDATE                                                 
         MVC   RCONDRTA,ACTTIME                                                 
         DROP  R6                                                               
*                                                                               
         TM    MISCFLG2,MF2NOADM                                                
         BO    APRO0260                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'DD',AIO3),0,0                   
         TM    MISCFLG2,MF2USRDM   TARGET=USER DEFINED DEMO?                    
         BO    APRO0260            YES, DON'T ADD TO CONTRACT                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,SVDEMCAT,=C'ADD=CODE'         
*                                                                               
APRO0260 EQU   *                   OVERRIDE BUYER NAME WITH A/O NAME            
         TM    MISCFLAG,MFCONOK    IF NOT ALREADY                               
         BO    APROX                                                            
         L     RF,AIO3                                                          
         MVC   KEY(L'RCONKEY),0(RF)                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA         WRITE OUT CONTRACT RECORD                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 PUTREC                                                           
*                                                                               
         OI    MISCFLAG,MFCONOK                                                 
*                                                                               
         CLI   FLTKEYFG,C'Y'       NEED TO REFRESH 8D/E POINTER?                
         BNE   APROX                                                            
*                                  YES! CONFLTDT HAS OLD DATES                  
         GOTO1 =A(GEN8DEKY),RR=RELO                                             
*                                                                               
*                                                                               
APROX    DS    0H                                                               
         GOTO1 =A(DISPLNKS),RR=RELO                                             
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* AFTER COMPLETION OF MANUAL MATCHING, WE MUST CHECK IF ANY CONTRACT            
* BUYS WERE NOT MATCHED AGAINST ANY AGENCY BUYS. IF UNMATCHED                   
* CONTRACT BUYS ARE FOUND. MUST TELL USER THE ERROR                             
*********************************************************************           
CHECKMTC NTR1                                                                   
         OC    SORTAREA(SORTDLQ),SORTAREA                                       
         BZ    CKBUYX              CONTRACT BUYS ALL CANCELLED, EXIT            
         LA    R3,SORTAREA         CHECK IF ALL CONTRACT BUYS WERE              
         USING SORTD,R3            MATCHED UP WITH ALL AGENCY BUYS              
*                                                                               
CKBUY10  TM    SRTFLAGS,SFMATCHD                                                
         BO    CKBUY20                                                          
*                                                                               
* UNMATCHED CONTRACT BUY IS OKAY IF BUY HAS ZERO TOTAL SPOTS                    
* OR IF BUY WAS MANUALLY CANCELLED                                              
*                                                                               
         TM    SRTFLG2,SF20SPTS+SF2MCAN                                         
         BZ    BUYNOMTC                                                         
*                                                                               
         MVC   KEY+28(4),SRTBUYDA  NEED TO FLAG THIS ZERO SPOT BUY              
         L     R6,AIOAREA          SO WE WON'T SEND IT BACK TO THE AGY          
         ST    R6,AIO              WHEN USER CONFIRMS LATER                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING RBUYREC,R6                                                       
         OI    RBUYRTS,X'01'       DON'T CONFIRM THIS 0-SPOT BUY TO AGY         
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
CKBUY20  DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   CKBUY10                                                          
*                                                                               
CKBUYX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*   BUYORBIT:  BUILDS DAY-TIME ELEMENT FROM THE ORBIT ELEMENTS                  
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
*                                                                               
BUYORBIT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST ORBIT ELEMENT                    
         MVI   ORBFLAG,C'Y'        SET 'ORBIT PRESENT' INDICATOR                
*                                     ORBIT TAKES PRIORITY OVER                 
*                                     BUY DAY/TIME ELTS                         
BORB0040 EQU   *                                                                
*                                  CLEAR START/END DAYS                         
         CLI   0(R8),0             END OF RECORD?                               
         BE    BORB0400            YES - FINISHED                               
*                                                                               
         USING RDAROEEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS                         
         MVC   ELTBUILD(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,RDAROERO,ELTBUILD+2,(R5),RR=RELO               
*                                  INSERT START/END DAY                         
         CLI   ORBSTDAY,0          ANY ENTRY IN ORBIT START DAY?                
         BNZ   BORB0060            YES - DON'T REPLACE IT                       
         MVC   ORBSTDAY,STARTDAY   NO  - SAVE FIRST ORBIT START DAY             
BORB0060 EQU   *                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDAROERO         SET A(ROTATION ARRAY)                        
BORB0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    BORB0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    BORB0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
BORB0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,BORB0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT BACK 1 'DAY'                           
         STC   RE,ELTBUILD+3       INSERT DAYS INTO ELEMENT                     
         MVC   ELTBUILD+4(4),RDAROEST                                           
*                                  INSERT START/END TIMES INTO ELEMENT          
         MVI   ELTBUILD+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
*   NOTE:  DAY/TIME ELEMENTS ENTERED VIA ORBITS WILL BE DELETED IF              
*      BUY HAS BEEN ENTERED AS A 'DAILY' BUY.  THIS SHOULD NOT HAPPEN,          
*      BUT HAS BEEN HANDLED IF IT DOES.                                         
*                                                                               
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BORB0040            GO BACK FOR NEXT ELEMENT                     
BORB0400 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
*                                                                               
*   BUYCOMMT:  BUILDS BUY COMMENT ELEMENT FOR THE FIRST TWO COMMENTS            
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
*                                                                               
BUYCOMMT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         LA    R4,2                LOOP CONTROL:                                
*                                     FIRST TWO ARE BUY COMMENTS                
*                                     NEXT  TWO ARE BUY ORDER COMMENTS          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - PROCEED                                
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEMENT,ELEMENT     FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEMENT          ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEMENT,0                     
         DROP  R6                                                               
*                                                                               
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - DON'T NEED COMMENT                     
         DROP  R8                                                               
*                                                                               
BCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
BCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   BCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,BCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
BCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORD                               
         EX    RF,BCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
         BCTR  R4,0                SUBTRACT 1 FROM COMMENT COUNT                
BCOM0020 EQU   *                                                                
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST COMMT ELEMENT                    
BCOM0040 EQU   *                                                                
****                                                                            
**** SKIP FOR REVISION                                                          
****                                                                            
         B     BCOM0400            SKIP REST OF COMMENTS                        
****                                                                            
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
*                                                                               
         USING RDARCTEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
         LA    RE,59               MAX SIZE FOR REP COMMENTS = 60               
         CR    R1,RE               NEW INPUT VS REP MAX                         
         BNH   BCOM0060            ACCEPTABLE                                   
         LR    R1,RE               NOT ACCEPTABLE                               
BCOM0060 EQU   *                                                                
         EX    R1,BCOM0500         MOVE ELEMENT TO BUILT AREA                   
         LA    R1,3(R1)            SET ELEMENT LENGTH                           
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0040         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
         LA    R4,2                IF MORE COMMENTS, PUT TO                     
*                                     BUY ORDER COMMENT RECORDS                 
BCOM0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'84'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
*                                                                               
         CH    R1,=H'60'           TRUNCATE IF MORE THAN 60 CHARS.              
         BL    BCOM0090                                                         
         LA    R1,59               MAX - 1 FOR EX                               
*                                                                               
BCOM0090 EQU   *                                                                
         EX    R1,BCOM0510         MOVE ELEMENT TO BUILD AREA                   
         LA    R1,4(R1)            RESET ELEMENT LENGTH                         
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         MVI   ELTBUILD+2,X'80'    TURN ON 'SENT BY REP'                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0080         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
BCOM0400 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
BCOM0500 MVC   ELTBUILD+2(0),2(R8) SET UP BUY COMMENT                           
*                                                                               
BCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                  SET UP PROGRAM NAME COMMENT                  
BCOM0510 MVC   ELTBUILD+3(0),2(R8) SET UP BUY ORDER COMMENT                     
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
*                                                                               
*   EFFDATEL:  BUILD AN ALTERNATE X'02' ELEMENT IN EVENT THERE                  
*        ARE NO ORBIT RECORDS.                                                  
*                                                                               
EFFDATEL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         USING RDARBYEL,R8                                                      
*                                                                               
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAY WORK AREA                
         XC    ELTBILD2,ELTBILD2   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBILD2(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,ELTBILD2+2,(R5),RR=RELO               
*                                  INSERT START/END DAY                         
                                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDARBYRO         SET A(ROTATION ARRAY)                        
EFFD0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    EFFD0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    EFFD0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
EFFD0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,EFFD0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT DOWN 1 'DAY' FOR                       
*                                     PROPER ALIGNMENT                          
         STC   RE,ELTBILD2+3       INSERT DAYS INTO ELEMENT                     
         ZICM  RF,RDARBYST,2       CHECK START TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0160            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0160 EQU   *                                                                
         STCM  RF,3,ELTBILD2+4     SAVE START TIME                              
*                                                                               
         CLC   RDARBYST,RDARBYET   IF SAME, SKIP END TIME                       
         BE    EFFD0210                                                         
*                                                                               
         ZICM  RF,RDARBYET,2       CHECK END   TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0200            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0200 EQU   *                                                                
         STCM  RF,3,ELTBILD2+6     SAVE END   TIME                              
*                                  INSERT START/END TIMES INTO ELEMENT          
EFFD0210 EQU   *                                                                
         MVI   ELTBILD2+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         B     EXIT                                                             
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD A BUY GRID                                                              
* FIRST CELL IS THE WEEK OF AGENCY ORDER FLIGHT START                           
* P1 = A(START DATE) AND BUYGRID                                                
* P2 = A(BUY RECORD) TO BE GRIDED                                               
***********************************************************************         
BLDGRID  NTR1                                                                   
         L     R2,0(R1)            A(START DATE) AND BUYGRID                    
         LR    R3,R2               SAVE OFF GRID STARTING POINT                 
         L     R6,4(R1)            A(BUY RECORD) TO BE GRIDED                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,FLTSTART)                            
         GOTO1 DATCON,(R1),(2,FLTDATES+2),(3,FLTEND)                            
*                                                                               
         L     R4,AIO3             USE THE EARLIER START DATE AND THE           
         USING RCONREC,R4          LATER END DATE BETWEEN                       
         CLC   FLTSTART,RCONDATE   CONTRACT AND DARE ORDER                      
         BNH   *+10                                                             
         MVC   FLTSTART,RCONDATE                                                
*                                                                               
         CLC   FLTEND,RCONDATE+3                                                
         BNL   *+10                                                             
         MVC   FLTEND,RCONDATE+3                                                
         DROP  R4                                                               
*                                                                               
         LR    R4,R6               REFERENCE POINTER TO BUY RECORD              
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         MVC   0(3,R2),RBUYDTST    GET BUY START DATE                           
         XC    3(L'RBUYGRID,R2),3(R2)                                           
         LA    R2,3(R2)            CLEAR AND POINT TO START OF GRID             
*                                                                               
BGRID05  DS    0H                                                               
         CLI   RBUYDTWK,0          SKIP THIS ROTATION IF ZERO WEEKS             
         BE    BGRID20                                                          
         CLI   RBUYDTNW,0          OR ZERO SPOTS                                
         BE    BGRID20                                                          
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,ELTBUILD)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELTBUILD+9)                          
         MVI   ELTBUILD+8,C'-'                                                  
         GOTO1 PERVAL,DMCB,(17,ELTBUILD),WORK2                                  
         CLI   DMCB+4,0                                                         
         BE    BGRID08                                                          
*        DC    H'0'                                                             
R4BUY    USING RBUYREC,R4                                                       
         OC    R4BUY.RBUYTSPT,R4BUY.RBUYTSPT   ZERO SPOT BUY??                  
         BZ    BGRIDX                                                           
         DC    H'0'                                                             
         DROP  R4BUY                                                            
*                                                                               
BGRID08  DS    0H                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         AR    R2,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
BGRID10  DS    0H                                                               
         ZIC   R1,RBUYDTWK         NUMBER OF WEEKS                              
         LTR   R1,R1                                                            
         BZ    BGRID20             FILL EACH CELL WITH THE AGENCY BUY'S         
*                                                                               
BGRID15  DS    0H                                                               
         MVC   0(1,R2),RBUYDTNW                                                 
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      WEEKLY OR EVERY OTHER??                      
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         BCT   R1,BGRID15                                                       
*                                                                               
BGRID20  DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         BAS   RE,NEXTEL                                                        
         BNE   BGRIDX                                                           
         LA    R2,3(R3)            RESET TO START OF GRID                       
         B     BGRID05                                                          
         DROP  R6                                                               
*                                                                               
BGRIDX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION OPEN/REJECT                                                        
***************                                                                 
         CLI   ACTNUM,ACTAPP       ACTION OPEN?                                 
         BE    STPFK10                                                          
         CLI   ACTNUM,ACTREJ       ACTION REJECT?                               
         BNE   STPFINIT                                                         
                                                                                
STPFK10  LA    R2,SPFTABLE         YES, USE LIST PFKEY TABLE                    
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN/REJECT PFKEY TABLE DEFINITIONS                                           
***********************************************************************         
SPFTABLE  DS    0C                                                              
*                                                                               
* JUMP TO THE CONTRACT PROGRAM                                                  
         DC    AL1(SPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF02X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*   LOADELT :  LOADS ELEMENT IN WORKSPACE (ELTBUILD) TO RECORD.                 
*                                                                               
LOADELT  NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         L     R3,4(R1)            RESET A(ELEMENT BUILD AREA)                  
         L     R4,8(R1)            RESET A(COMMAND EXTENSION)                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),(R3),(R4)                     
*                                  ADD ELT TO RECORD                            
         B     EXIT                                                             
*                                                                               
*   DELELT :  DROPS X'03' ELEMENT FROM BUYRECS                                  
*                                                                               
DELELT   NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(3,(R2)),0,0                       
*                                  DROP X'03' ELTS FROM BUYRECORD               
         B      EXIT                                                            
*                                                                               
*   DELELT02 :  DROPS X'02' ELEMENT(S) (DAY/TIME) FROM BUYRECS                  
*        WHEN THEY WERE GENERATED BY AN ORBIT RECORD, AND THE BUYS              
*        ARE 'DAILY'                                                            
*                                                                               
DELELT02 NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(2,(R2)),0,0                       
*                                  DROP X'02' ELTS FROM BUYRECORD               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
MAXBUYER MVC   RERROR,=AL2(93)                                                  
         B     ERREND                                                           
*                                                                               
INVMETH  MVC   RERROR,=AL2(728)                                                 
         B     ERREND                                                           
*                                                                               
MUSTMAN  MVC   RERROR,=AL2(737)                                                 
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
BUYNOMTC DS    0H                                                               
         USING SORTD,R3                                                         
         XC    BLOCK,BLOCK                                                      
         EDIT  SRTCONBY,(3,BLOCK+1),ALIGN=LEFT                                  
         AH    R0,=H'1'                                                         
         STC   R0,BLOCK                                                         
         MVC   RERROR,=AL2(170)                                                 
         GOTO1 =A(DISPLNKS),RR=RELO                                             
         B     INFRTEXT                                                         
         DROP  R3                                                               
*                                                                               
INCMATCH DS    0H                                                               
         L     R4,AIO2                                                          
         USING RBUYREC,R4                                                       
         XC    BLOCK,BLOCK                                                      
         EDIT  RBUYAGBL,(3,BLOCK+1),ALIGN=LEFT                                  
         AH    R0,=H'1'                                                         
         STC   R0,BLOCK                                                         
*        MVC   RERROR,=AL2(724)                                                 
         MVC   RERROR,=AL2(169)                                                 
         GOTO1 =A(DISPLNKS),RR=RELO                                             
         B     INFRTEXT                                                         
         DROP  R4                                                               
*                                                                               
RECFULL  DS    0H                  FORCE ABEND AND REVERSE TRANSACTIONS         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(RECFULLQ),RECFULLM                                       
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
INFRTEXT DS    0H                  SUBSTITUTION TEXT                            
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND   MVI   RMSGTYPE,C'I'                                                    
         LA    R2,CONRECH          SET CURSOR HERE FOR ALL ERROR/INFO           
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
         GETELN R4,DATADISP,ELCODE,2                                            
*                                                                               
RECFULLM DC    C'RECORD FULL - ACTION NOT PROCESSED - CALL DDS'                 
RECFULLQ EQU   *-RECFULLM                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILDS DEMO ELEMENT                                                           
* WORK2 HAS A COPY OF 0D ELEMENT                                                
**********************************************************************          
SAVEDEMO NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    MISCFLG2,MF2NODEM                                                
         BO    SVDEMX                                                           
*                                                                               
         LR    RE,RA               CHECK TO SEE IF LOCAL SIGN ON                
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    SVDEM05                                                          
         DROP  RE                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'     YES, DO NOT PROCESS DEMO                     
         BE    SVDEMX                                                           
*                                                                               
SVDEM05  DS    0H                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            IS THERE A AGY DEMO?                         
         BE    SVDEM07             YES                                          
*                                                                               
         CLI   WORK2,X'0D'         DO WE HAVE A REP DEMO?                       
         BNE   SVDEMX              IF NO, EXIT                                  
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK             FAKE A 20 ELEMENT WITH NULLS                 
         MVI   WORK+1,RDARBMLQ                                                  
*                                                                               
SVDEM07  DS    0H                                                               
         USING RDARBMEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,RDARBMLN                                                      
         SHI   R3,2                SUBTRACT OVERHEAD                            
         LA    RE,L'RDARBDM1                                                    
         DR    R2,RE               GET NUMBER OF DEMOS TO PROCESS               
*                                                                               
         LA    R6,RDARBDM1                                                      
         LA    R1,WORK2+2                                                       
SV0D     USING RBUYDMCV,R1                                                      
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
ELEMD    USING RBUYDMEL,ELEM                                                    
         MVI   ELEMD.RBUYDMCD,RBUYDMCQ     X'0D'                                
         MVI   ELEMD.RBUYDMLN,2                                                 
         LA    R4,ELEMD.RBUYDMCV                                                
         LA    R2,SVDEMCAT+2                                                    
*                                                                               
SVDEM10  DS    0H                                                               
         MVC   0(L'RBUYDMCT,R4),0(R2)                                           
         AHI   R4,L'RBUYDMCT                                                    
         MVC   0(8,R4),=8X'FF'     -1 MEANS VALUE NOT PROVIDED                  
*                                                                               
         OC    0(L'RBUYDMCT,R2),0(R2)  NO CATEGORY - SKIP                       
         BZ    SVDEM20                                                          
*                                                                               
         CLC   0(L'RDARBDM1,R6),SPACES                                          
         BE    SVDEM20                                                          
*                                                                               
         PACK  DUB(8),0(7,R6)                                                   
         L     RF,DUB+4                                                         
         SRL   RF,4                DUMP THE SIGN                                
         STCM  RF,15,0(R4)                                                      
         TM    MISCFLG2,MF2DEMCH   IF THIS IS A CAT CHANGE                      
         BO    SVDEM20             DO NO SAVE PREVIOUS VALUE                    
*                                                                               
         CLI   WORK2+1,34          OLD FORMAT DEMO - SKIP                       
         BE    SVDEM20                                                          
         CLI   WORK2+1,18                                                       
         BE    SVDEM20                                                          
*                                                                               
         MVC   L'RBUYDMDM(L'RBUYDM2M,R4),SV0D.RBUYDMDM                          
*                                                                               
SVDEM20  DS    0H                                                               
         AHI   R6,L'RDARBDM1                                                    
         AHI   R4,L'RBUYDMDM+L'RBUYDM2M                                         
         ZIC   RE,ELEMD.RBUYDMLN                                                
         AHI   RE,L'RBUYDMCT+L'RBUYDMDM+L'RBUYDM2M                              
         STC   RE,ELEMD.RBUYDMLN                                                
*                                                                               
         AHI   R2,L'RCONDDCT                                                    
         AHI   R1,L'RBUYDMCV                                                    
*                                                                               
         BCT   R3,SVDEM10                                                       
*                                                                               
*                                                                               
SVDEMX   DS    0H                                                               
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COPY0D: COPY 0D ELEMENT FROM OLD BUY TO NEW BUY                               
**********************************************************************          
COPY0D   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL                                                         
         BNE   COPY0DX                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2)                       
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,(R6),=C'ADD=CODE'             
*                                                                               
COPY0DX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CONVERT AND BUILD CONTRACT DEMO CATEGORIES ELEMENT IN SVDEMCAT                
**********************************************************************          
CONVDCAT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         XC    SVDEMCAT,SVDEMCAT                                                
ELEMD    USING RCONDDEL,SVDEMCAT                                                
         MVI   ELEMD.RCONDDCD,X'DD'                                             
         MVI   ELEMD.RCONDDLN,2                                                 
*                                                                               
         LA    R3,10                                                            
         LA    R6,WORK                                                          
         LA    R4,ELEMD.RCONDDCT                                                
*                                                                               
CVCAT10  DS    0H                                                               
         CLC   0(L'RDARDEM1,R6),SPACES                                          
         BE    CVCATX                                                           
         CLI   0(R6),C'('          USER DEFINED DEMO?                           
         BNE   *+14                                                             
         MVC   0(3,R4),0(R6)                                                    
         B     CVCAT20                                                          
*                                                                               
         MVC   1(1,R4),0(R6)                                                    
         PACK  DUB(8),1(3,R6)                                                   
         CVB   RF,DUB                                                           
         STC   RF,2(R4)                                                         
*                                                                               
CVCAT20  DS    0H                                                               
         ZIC   RE,ELEMD.RCONDDLN                                                
         AHI   RE,L'RCONDDCT                                                    
         STC   RE,ELEMD.RCONDDLN                                                
*                                                                               
         AHI   R4,L'RCONDDCT                                                    
         AHI   R6,L'RDARDEM1                                                    
         BCT   R3,CVCAT10                                                       
         DROP  ELEMD                                                            
*                                                                               
CVCATX   DS    0H                                                               
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETCOND GET CONTRACT DEMO                                                     
*                                                                               
***********************************************************************         
GETCOND  NTR1  BASE=*,WORK=(R2,IMWORKQ),LABEL=*                                 
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         USING IMWORKD,R2                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVIO,AIO                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK                                                    
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   GCNDEMX                                                          
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              IF THERE IS NO DEMO ON AGENCY SEND           
         MVI   ELCODE,X'12'        AND NO DEMO ON THE CONTRACT                  
         BRAS  RE,GETEL                                                         
         BNE   *+10                SAVE PENDING DEMO                            
         USING RSARCO,R6                                                        
         MVC   ELTBUILD(L'RCONDDCT),RSARDEM                                     
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              IF THERE IS NO DEMO ON AGENCY SEND           
         MVI   ELCODE,X'DD'        AND NO DEMO ON THE CONTRACT                  
         BRAS  RE,GETEL                                                         
         BNE   MRKFLG              SET NODEMO FLAG AND SKIP DEMO                
         ZIC   R1,1(R6)            PROCESSING                                   
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),SPACES                                                   
         BE    MRKFLG                                                           
*                                                                               
         CLI   1(R6),18            OLD FORMAT DEMO SKIP                         
         BE    MRKFLG                                                           
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELTBILD2(0),0(R6)   COPY ORIGINAL DEMO                           
         B     GCNDYES                                                          
*                                                                               
MRKFLG   DS    0H                                                               
         OI    MISCFLG2,MF2NORDM                                                
*                                                                               
GCNDYES  SR    R5,R5                                                            
GCNDNO   LTR   R5,R5                                                            
         MVC   KEY,IMSVKEY                                                      
         MVC   AIO,IMSVIO                                                       
GCNDEMX  XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CONTRACT VERSION BUMPED. DELETE CONTRACT CONFIRM COMMENT                      
*                                                                               
DELCFC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYSVAIO,AIO                                                      
         XC    KEY,KEY             READ CFC REC                                 
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,AGENCY                                                  
         MVC   RCFCKCON,CCONKNUM                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCFCKEY),KEYSAVE   HAVE CFC REC?                           
         BNE   DELCFCX                  NO - GET OUT                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RCFCREC,R6                                                       
         OI    RCFCCNTL,X'80'          DELETE RECORD                            
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         OI    KEY+27,X'80'            DELETE KEY                               
         GOTO1 WRITE                                                            
*                                                                               
DELCFCX  EQU   *                                                                
         MVC   AIO,MYSVAIO         RESTORE AIO BEFORE EXIT                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SPECIAL TRAP FOR AUTO-APPROVAL TOTALS NOT MATCHING                            
*                                                                               
TRAP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XR    R1,R1                                                            
         L     R6,AIO3                                                          
         MVI   ELCODE,X'03'                                                     
         TM    MISCFLG2,MF2TRADE   TRADE ORDER?                                 
         BZ    *+8                                                              
         MVI   ELCODE,X'63'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
TRAP10   BRAS  RE,NEXTEL                                                        
         BNE   TRAP20                                                           
*                                                                               
         ZICM  R0,6(R6),4                                                       
         AR    R1,R0                                                            
         B     TRAP10                                                           
*                                                                               
TRAP20   DS    0H                                                               
         ZICM  R0,GTOTAL$,4                                                     
         CR    R0,R1                                                            
         BE    TRAPX                                                            
*                                                                               
* NOTIFY USER TO CONTRACT DDS                                                   
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(ERRMTCHQ),ERRMTCH                                        
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
TRAPX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ERRMTCH  DC    C'Error: Contract and order total mismatch. Call DDS.'           
ERRMTCHQ EQU   *-ERRMTCH                                                        
*                                                                               
*                                                                               
*                                                                               
*   GEN8DEKY:  DELETE OLD 8D/8E KEY AND ADD NEW ONES                            
*                                                                               
* CONFLTDT = OLD/ORIGINAL CONTRACT FLIGHT DATES                                 
* FLTDATES = NEW/DARE ORDER FLIGHT DATES                                        
*                                                                               
         DS    0D                                                               
GEN8DEKY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO              SAVE OFF STATION AND CON# BEFORE             
         USING RCONREC,R6          AIO GETS USED BY DATAMGR                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,NEWFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,NEWFLTDT+2)                        
         CLC   CONFLTDT,NEWFLTDT   NO CHANGES, SHOULDN'T BE IN HERE             
         BE    GENKEYX                                                          
*                                                                               
         MVC   WORK(5),RCONKSTA                                                 
         MVC   WORK+5(4),RCONKCON                                               
         DROP  R6                                                               
*                                                                               
* DELETE OLD SET OF 8D/8E RIS KEYS FIRST                                        
*                                                                               
         XC    KEY,KEY                                                          
KYD      USING RCONKEY,KEY                                                      
         MVI   KYD.RCON8TYP,X'8D'  INSERT KEY ID                                
*                                                                               
GENKEY10 DS    0H                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   KYD.RCON8REP,AGENCY                                              
         MVC   KYD.RCON8FST(4),CONFLTDT                                         
         MVC   KYD.RCON8CON,WORK+5                                              
         LA    R3,1                                                             
         STC   R3,KYD.RCON8RID                                                  
         NI    DMINBTS,X'FF'-X'08' TURN OFF 'RETURN DELETES'                    
         GOTO1 HIGH                READ HIGH AND GET X'01' KEY                  
*                                  READ ACTIVE KEYS ONLY!                       
GENKEY20 DS    0H                                                               
         MVC   0(32,R4),KEY        SAVE OFF KEY                                 
*                                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                                                               
         ZIC   R1,KYD.RCON8RID     SEQUENCE MUST MATCH                          
         CR    R3,R1                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 SEQ                                                              
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY20                                                         
*                                                                               
* ADD NEW SET OF 8D/8E RIS KEYS WITH NEW FLIGHT DATES                           
*                                                                               
GENKEY30 DS    0H                                                               
*                                                                               
         LA    R3,1                                                             
         LA    R4,BLOCK                                                         
*                                                                               
GENKEY40 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R4)                                                    
*                                  ADD NEW KEYS WITH NEW FLIGHT DATES           
         MVC   KYD.RCON8FST(4),NEWFLTDT                                         
         STC   R3,KYD.RCON8RID                                                  
         OI    DMINBTS,X'08'       RETURN DELETED KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CHECK IF NEW KEY EXISTS                      
         BNE   GENKEY50                                                         
         MVI   KEY+27,0            YES, RESTORE IT                              
         GOTO1 WRITE                                                            
         B     GENKEY60                                                         
*                                                                               
GENKEY50 DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+27,0                                                         
         GOTO1 ADD                                                              
*                                                                               
GENKEY60 DS    0H                                                               
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY40                                                         
*                                                                               
         CLI   KEYSAVE,X'8E'       HAVE DONE PROCESSING 8D AND 8E KEYS?         
         BE    GENKEYX             YES, EXIT                                    
         XC    KEY,KEY             NO, SET TO PROCESS 8E KEYS                   
         MVI   KYD.RCON8TYP,X'8E'  INSERT KEY ID                                
         MVC   KYD.RCON8EST,WORK                                                
         B     GENKEY10                                                         
*                                                                               
GENKEYX  EQU   *                                                                
         XIT1                                                                   
         DROP  KYD                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE RIPPED FROM REDAR20                                                   
* ALL THIS ROUTINE DOES AT THIS POINT IS AGGREGATE TOTAL SPOTS AND              
* DOLLARS FOR A BUY AS WELL AS THE ENTIRE ORDER.                                
***********************************************************************         
GENREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2            SET IO AREA = IOAREA2                        
         L     R2,AIO                                                           
         USING RBUYREC,R2                                                       
*                                                                               
*   INSERT TOTAL SPOTS, TOTAL COST, AND TOTAL WEEKS FIGURES                     
*                                                                               
         MVC   RBUYTSPT,SPOTCTR+2  LAST TWO POSITIONS ONLY                      
         L     RF,SPOTCTR          ACCUMULATE TOTAL SPOTS                       
         A     RF,ORDTOTSP                                                      
         ST    RF,ORDTOTSP         SAVE ORDER TOTAL SPOTS                       
         MVC   RBUYTWKS,WEEKCTR+3  LAST POSITION ONLY                           
         ZICM  RF,RBUYTSPT,2       LOAD TOTAL SPOTS                             
         SR    R0,R0                                                            
         L     R1,BUYCOST          LOAD COST PER SPOT                           
         MR    R0,RF               # SPOTS X COST/SPOT                          
         STCM  R1,15,RBUYTCOS      INSERT TOTAL COST                            
         A     R1,ORDTOT$$         ACCUMULATE ORDER TOTAL DOLLARS               
         ST    R1,ORDTOT$$         SAVE ORDER TOTAL DOLLARS                     
         XC    SPOTCTR,SPOTCTR     CLEAR ACCUMULATORS                           
         XC    WEEKCTR,WEEKCTR                                                  
         XC    BUYCOST,BUYCOST                                                  
*                                                                               
*        TM    MISCFLAG,MFMANUAL                                                
*        BZ    GENR0010                                                         
         MVC   AIO,AIO1            RESET A(X'41' RECORD AREA)                   
         B     EXIT                                                             
*                                                                               
GENR0010 EQU   *                                                                
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY FOR RESTART                 
         MVC   KEY,RBUYKEY         GET KEY OF BUY                               
*                                  LOOK FOR OLD RECORD                          
*                                     GENCON DOESN'T LIKE 'ADDREC'              
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BE    GENR0040            YES - RECORD/KEY MUST BE REPLACED            
*                                                                               
         MVC   KEY(27),KEYSAVE     NO  - RESET NEW KEY                          
         CLI   RBUYVER,1           VERSION = 1?                                 
         BE    GENR0020            YES - ORIGINAL CREATION                      
         MVI   RBUYCHGI,C'A'       NO  - ADDED ON THIS PASS                     
         MVI   RBUYCHGI+1,0        CLEAR SECOND CHG BYTE                        
GENR0020 EQU   *                                                                
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
*                                     GENCON TRAPS ERRORS                       
         B     GENR0080            RECORD ADDED SUCCESSFULLY                    
GENR0040 EQU   *                                                                
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         GOTO1 GETREC              READ RECD INTO AIOAREA                       
         MVC   AIO,AIO2            RESET IOAREA TO NEW BUY REC                  
*                                  COMPARE OLD/NEW FOR CHANGE CODE              
*                                                                               
*                                     OLD RECORD MAY HAVE BEEN DELETED:         
*                                     STILL MUST ACCESS KEY/RECORD              
*                                        FOR UPDATE                             
*                                                                               
*   GENERAL NOTE:  AT PRESENT, THE BUYS ARE ALL BLOWN AWAY BY A                 
*      GENERAL SUBROUTINE AT THE START OF THE JOB.  THERE IS NO                 
*      WAY TO DETERMINE WHICH BUYS ARE TO REMAIN, WHICH TO BE                   
*      DELETED ANY OTHER WAY.  THE FOLLOWING ITEMS OF CODE ARE                  
*      INCLUDED FOR THAT TIME WHEN ''CHANGES'' ARE PROCESSED.                   
*      AT THAT TIME, BUYS WILL BE PROCESSED SINGLY.                             
*                                                                               
         MVI   DELBUKTS,0          CLEAR 'DELETE BUCKET' FLAG                   
         TM    KEY+27,X'80'        KEY DELETED?                                 
         BO    GENR0050            YES - NOTHING TO CLEAR                       
         MVI   DELBUKTS,1          SET 'DELETE BUCKET' FLAG                     
GENR0050 EQU   *                                                                
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 GETREC              RETRIEVE ORIGINAL RECORD                     
         CLI   DELBUKTS,0          DELETED RECORD IN PROCESS?                   
         BE    GENR0060            YES - DON'T BACK BUCKETS OUT                 
         MVI   BUCKFLAG,X'FF'      NO  - SET TO BACK OUT FIGURES                
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  BACK OUT BUCKETS                             
GENR0060 EQU   *                                                                
         MVC   AIO,AIO2            SET IO AREA FOR NEW BUY                      
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
GENR0080 EQU   *                                                                
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  ADD NEW BUCKETS                              
GENR0100 EQU   *                                                                
         MVC   AIO,AIO1            RESET A(X'41' RECORD AREA)                   
         MVC   KEY,SAVEKEY         RESET KEY FOR X'41' RECS                     
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SHOW LINKS ON THE OPEN SCREEN                                                 
***********************************************************************         
DISPLNKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TWAXC AORREASH,AORPFLNH                                                
*                                                                               
         ZIC   R3,SORTRECS                                                      
         GOTO1 XSORT,DMCB,SORTAREA,(R3),9,3,0                                   
*                                                                               
         LA    R4,SORTAREA                                                      
         USING SORTD,R4                                                         
         CLI   0(R4),0                                                          
         BE    DLNKX                                                            
         CLI   APPRBUY#,0                                                       
         BE    DLNK03                                                           
         ZIC   R2,APPRBUY#                                                      
         LA    R3,SORTDLQ                                                       
         STCM  R3,3,HALF                                                        
         MH    R2,HALF                                                          
         AR    R4,R2                                                            
         CLI   0(R4),0                                                          
         BNE   DLNK03                                                           
         MVI   APPRBUY#,0                                                       
         B     DLNKX                                                            
*                                                                               
DLNK03   DS    0H                                                               
         LA    R2,AORREA2H                                                      
*                                                                               
         LA    R3,AORREAS                                                       
         USING SLINKD,R3                                                        
DLNK05   MVC   SLAGYBUY,=C'Agy'                                                 
         MVC   SLKBUY,=C'Con'                                                   
         MVC   SLTKBUY,=C'Tar'                                                  
         DROP  R3                                                               
*                                                                               
DLNK10   DS    0H                                                               
         OI    6(R2),X'80'         XMIT FIELD                                   
         LA    R3,8(R2)                                                         
         OC    AORREAS+40(12),AORREAS+40                                        
         BZ    *+8                 NEXT COLUMN??                                
         LA    R3,40(R3)                                                        
         USING SLINKD,R3                                                        
         CLI   SRTAGYBY,X'FF'                                                   
         BE    DLNK15                                                           
*                                                                               
         EDIT  SRTAGYBY,(3,SLAGYBUY)                                            
*                                                                               
DLNK15   DS    0H                                                               
         EDIT  SRTCONBY,(3,SLKBUY)                                              
*                                                                               
         LA    R6,SLFLAGS                                                       
*                                                                               
         TM    SRTFLAGS,SFMATCHD                                                
         BZ    DLNK20                                                           
         TM    SRTFLAGS,SFADDED                                                 
         BZ    DLNK18                                                           
         MVC   0(9,R6),=C'Buy Added'                                            
         LA    R6,10(R6)                                                        
         B     DLNK90                                                           
*                                                                               
DLNK18   DS    0H                                                               
         TM    SRTFLAGS,SFTMKGD+SFTCRDT                                         
         BNZ   DLNK19              MKGD AND CREDIT TARGET LINES DON'T           
         TM    SRTFLG2,SF2CAN      GET CANCELLED                                
         BO    DLNK48                                                           
*                                                                               
DLNK19   DS    0H                                                               
         MVC   0(7,R6),=C'Matched'                                              
         LA    R6,8(R6)                                                         
*                                                                               
DLNK20   DS    0H                                                               
         TM    SRTFLG2,SF2METH1                                                 
         BZ    DLNK30                                                           
*        MVC   0(7,R6),=C'Can/Sup'                                              
*        LA    R6,8(R6)                                                         
         B     DLNK50                                                           
*                                                                               
DLNK30   DS    0H                                                               
         TM    SRTFLG2,SF2METH2                                                 
         BZ    DLNK40                                                           
*        MVC   0(7,R6),=C'+Weight'                                              
*        LA    R6,8(R6)                                                         
         B     DLNK50                                                           
*                                                                               
DLNK40   DS    0H                                                               
         TM    SRTFLG2,SF2METH3                                                 
         BZ    DLNK45                                                           
*        MVC   0(6,R6),=C'Revise'                                               
*        LA    R6,7(R6)                                                         
         B     DLNK50                                                           
*                                                                               
DLNK45   DS    0H                                                               
         TM    SRTFLAGS,SFMATCHD                                                
         BO    DLNK50                                                           
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BZ    DLNK48                                                           
*                                                                               
*        MVC   0(9,R6),=C'UNMATCHED'                                            
*        LA    R6,10(R6)                                                        
*        TM    SRTFLG2,SF20SPTS                                                 
*        BZ    DLNK50                                                           
*        MVC   0(6,R6),=C'0 SPTS'                                               
*        LA    R6,7(R6)                                                         
         B     DLNK50                                                           
*                                                                               
DLNK48   DS    0H                                                               
         MVC   0(6,R6),=C'Cancel'                                               
         LA    R6,7(R6)                                                         
*                                                                               
DLNK50   DS    0H                                                               
*        TM    SRTFLAGS,SFMATING                                                
*        BZ    *+10                                                             
*        MVC   0(2,R6),=C'IP'                                                   
*        LA    R6,3(R6)                                                         
*                                                                               
DLNK60   DS    0H                                                               
         TM    SRTFLAGS,SFMKGOOD                                                
         BZ    DLNK70                                                           
         EDIT  SRTTKBUY,(3,SLTKBUY)                                             
         MVC   0(2,R6),=C'Mg'                                                   
         LA    R6,3(R6)                                                         
*                                                                               
DLNK70   DS    0H                                                               
         TM    SRTFLAGS,SFCREDIT                                                
         BZ    DLNK80                                                           
         MVC   0(6,R6),=C'Credit'                                               
         LA    R6,7(R6)                                                         
*                                                                               
DLNK80   DS    0H                                                               
*        TM    SRTFLAGS,SFTMKGD                                                 
*        BZ    *+10                                                             
*        MVC   0(2,R6),=C'TM'                                                   
*        LA    R6,3(R6)                                                         
*                                                                               
*        TM    SRTFLAGS,SFTCRDT                                                 
*        BZ    *+10                                                             
*        MVC   0(2,R6),=C'TC'                                                   
*                                                                               
DLNK90   DS    0H                                                               
         ZIC   RF,APPRBUY#                                                      
         LA    RF,1(RF)                                                         
         STC   RF,APPRBUY#                                                      
*                                                                               
         LA    R4,SORTDLQ(R4)                                                   
         CLI   0(R4),0                                                          
         BNE   DLNK95                                                           
         MVI   APPRBUY#,0                                                       
         XC    AORMORE,AORMORE                                                  
         B     DLNKX                                                            
*                                                                               
DLNK95   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    RF,AORENDH                                                       
         CR    R2,RF                                                            
         BNH   DLNK10                                                           
*                                                                               
         OC    AORREAS+40(12),AORREAS+40                                        
         BZ    DLNK100                                                          
         MVC   AORMORE,=C'*More*'                                               
         B     DLNKX               PAGE FULL??                                  
*                                                                               
DLNK100  DS    0H                                                               
         LA    R2,AORREA2H                                                      
         LA    R3,AORREAS+40                                                    
         B     DLNK05                                                           
*                                                                               
DLNKX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS MANUAL DARE CHANGES                                        
*                                                                               
* INPUT: PARM 1, BYTE 1 = X'80', RETURN NON ZERO CONDITION IF NO MATCH          
*                                INSTEAD OF EXITING TO ERROR                    
*                                                                               
* 1 - SORT CONTRACT BUYS                                                        
* 2 - VALIDATE EACH AGENCY BUY TO ITS CORRESPONDING CONTRACT BUY(S)             
*     THE BUYS MUST MATCH EACH OTHER                                            
*                                                                               
* CASES:                                                                        
*                                                                               
*   1: ONE AGENCY BUY TO ONE CONTRACT BUY (MATCH)                               
*   2: NEW AGENCY BUYS (MATCH TO MANUALLY ADDED CONTRACT BUY(S))                
*   3: ERROR IF UNMATCHED CONTRACT BUYS REMAIN                                  
*                                                                               
*                                                                               
* THE AGENCY BUY IS DEFINED BY A GRID WITH EACH CELL REPRESENTING ONE           
* WEEK OF THE BUY. EACH CELL HAS THE NUMBER OF SPOTS FOR THAT WEEK.             
* THIS ROUTINE WILL THEN READ ALL MANUALLY ADDED CONTRACT BUYS WHICH            
* HAVE THE SAME DAY, TIME , RATE AND LENGTH AS THE AGENCY BUY.                  
* THE ROUTINE WILL THEN REMOVE SPOTS FOUND IN THE GRID FROM THESE               
* THESE CONTRACT BUYS. WHEN ALL CONTRACT BUYS ARE PROCESSED AND                 
* ALL CELLS IN THE GRID BECOME ZERO, THEN WE HAVE A MATCH BETWEEN               
* THE AGENCY BUY AND THE CONTRACT BUY(S).                                       
*                                                                               
* THIS ROUTINE WILL LOOK FOR CONTRACT BUYS WITH SAME # SPOT/WK BUYS             
* FIRST. IF NOT ALL SPOTS ARE ACCOUNTED FOR, IT WILL LOOP AGAIN TO              
* LOOK FOR BUYS WITH DIFFERENT SPOT/WK                                          
*                                                                               
* FOR EXAMPLE, (FOR THE SECOND ITERATION)                                       
* AGENCY BUY: M,W,F FOR 3 SPOTS, JAN6/97-6W                                     
* THE GRID LOOKS LIKE THIS: (GRID HAS ROOM FOR 53 WEEKS)                        
*                                                                               
* 33333300000000000000000000000000000000000000000000000                         
*                                                                               
* SO IF WE FIND A CONTRACT BUY WITH: M,W,F FOR 2 SPOTS, JAN13-3W                
* THE GRID BECOMES:                                                             
*                                                                               
* 31113300000000000000000000000000000000000000000000000                         
*                                                                               
* AND SO ON AND SO FORTH...                                                     
*                                                                               
***********************************************************************         
MANMATCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    0(R1),X'80'                                                      
         BZ    MMAT05                                                           
         OI    MISCFLG2,MF2NOXIT                                                
*                                                                               
MMAT05   DS    0H                                                               
         OI    MISCFLAG,MFEXACT                                                 
         NI    MISCFLAG,X'FF'-MFMGLOOP                                          
*                                                                               
MMAT10   DS    0H                  BUILD AGY BUY GRID                           
         GOTO1 BLDGRID,DMCB,ASTRDATE,AIO2                                       
         MVC   ABUYGRD2,ABUYGRID                                                
*                                                                               
         LA    R3,SORTAREA                                                      
         USING SORTD,R3                                                         
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
MMAT50   DS    0H                                                               
         CLI   SRTAGYBY,0                                                       
         BNE   MMAT60              ALL DONE, CHECK GRID                         
*                                                                               
*                                                                               
* THE AGENCY(SPOTPAK) SIDE DOES NOT SEND A RATE IN BUYDTL IF THE                
* BUY HAS ALL ITS SPOTS REMOVED BY PREEMPTS OR BY MAKEGOODS. THEREFORE,         
* TARGET MAKEGOOD BUYS WITH ALL SPOTS TAKEN OUT THAT HAVE NO AGENCY/REP         
* LINK WILL NEED TO BE PROCESSED AGAIN. THIS WILL PREVENT ACCIDENTAL            
* CANCELLATION OF TARGET MAKEGOOD BUYS AND CORRESPONDING MAKEGOODS ON           
* THE REP SIDE.                                                                 
*                                                                               
* THIS WILL CHECK FOR ZERO SPOT BUYS ON THE REP SIDE WITH THE SAME              
* DAY, TIME AND LENGTH TO MATCH                                                 
*                                                                               
         OC    AGYBUY.RBUYTSPT(6),AGYBUY.RBUYTSPT                               
         BZ    MMAT52                                                           
*                                                                               
         OC    ABUYGRID,ABUYGRID   ALL SPOTS ACCOUNTED FOR?                     
         BZ    MMAT100                                                          
*                                                                               
MMAT52   DS    0H                                                               
         TM    MISCFLAG,MFEXACT    NO, WERE WE LOOKING FOR EXACT NPW?           
         BO    MMAT55              NO, BOTH LOOPS FAILED                        
         TM    MISCFLAG,MFMGLOOP   DO EXTRA LOOP FOR MAKEGOOD                   
         BZ    MMAT54                                                           
*                                                                               
         TM    MISCFLG2,MF2NOXIT   RETURN ON ERROR?                             
         BO    MMATNO              YES                                          
         B     INCMATCH            NO, JUST EXIT TO ERROR                       
*                                                                               
MMAT54   DS    0H                                                               
         OI    MISCFLAG,MFEXACT+MFMGLOOP                                        
         B     MMAT10                                                           
*                                                                               
MMAT55   DS    0H                                                               
         NI    MISCFLAG,X'FF'-MFEXACT                                           
         B     MMAT10              YES, NOW DO INEXACT MATCH LOOP               
*                                                                               
* THIS CHECK IS TO REFINE THE MATCHING AMONG MAKEGOOD BUYS                      
* COMMENT OUT THIS CHECK IF JDS EVER GETS AN AGENCY SINCE JDS DO NOT            
* SENT US THE BACKWARD LINK FOR THE MAKEGOOD                                    
*                                                                               
MMAT60   DS    0H                                                               
         TM    SRTFLAGS,SFCREDIT   IS K BUY A CREDIT??                          
         BO    MMAT90              DON'T PROCESS CREDIT BUYS                    
*                                  CREDIT BUYS WILL BE PROCESSED                
*                                  ALONG WITH TARGET CREDIT LATER               
         TM    SRTFLAGS,SFMKGOOD   IS K BUY A MAKEGOOD??                        
         BZ    MMAT65                                                           
         TM    MISCFLAG,MFMGLOOP   EXTRA MAKEGOOD LOOP                          
         BO    MMAT70              DISREGARD MAKEGOOD LINK                      
         CLC   AGBUYMTR,SRTAGYBY   ORIGINAL LINKS MUST MATCH                    
         BE    MMAT70                                                           
         CLI   AGBUYMTR,0          IS AGENCY A MAKEGOOD LINE??                  
         BE    MMAT90                                                           
         B     MMAT70                                                           
*                                                                               
MMAT65   DS    0H                                                               
         CLC   AGYBUY.RBUYAGBL,SRTAGYBY                                         
         BE    MMAT70                                                           
         CLI   SRTAGYBY,X'FF'                                                   
         BNE   MMAT90              MANUALLY ADDED K BUY??                       
MMAT70   TM    SRTFLAGS,SFMATCHD   MATCHED ALREADY??                            
         BO    MMAT90                                                           
*                                                                               
* FLAG IF THERE ARE CREDIT K BUYS FOR THE CURRENT K BUY                         
*                                                                               
         NI    MISCFLAG,X'FF'-MFCREDIT                                          
         LR    RF,R3                                                            
MMAT73   DS    0H                                                               
         LA    RF,SORTDLQ(RF)                                                   
         CLI   0(RF),0                                                          
         BE    MMAT75                                                           
NEXTBUYD USING SORTD,RF                                                         
         CLC   SRTTKBUY,NEXTBUYD.SRTTKBUY                                       
         BNE   MMAT73                                                           
         TM    NEXTBUYD.SRTFLAGS,SFCREDIT                                       
         BZ    MMAT73                                                           
         OI    MISCFLAG,MFCREDIT                                                
         DROP  NEXTBUYD                                                         
*                                                                               
MMAT75   DS    0H                                                               
         BAS   RE,UPDTGRID                                                      
         BNZ   MMAT85                                                           
*                                                                               
* IF AGENCY BUY HAS NO TOTAL SPOT/COST, SEE IF WE FOUND A MATCH ANYWAY          
*                                                                               
         OC    AGYBUY.RBUYTSPT(6),AGYBUY.RBUYTSPT                               
         BNZ   MMAT80                                                           
         OC    SPOTCTR+2(2),SPOTCTR+2                                           
         BNZ   MMAT80              CHECK NUMBER OF SPOTS AGAIN                  
         TM    SRTFLAGS,SFMATING   INCASE WE NEVER WENT TO GENREC               
         BO    MMAT100                                                          
         B     MMAT85                                                           
*                                                                               
MMAT80   DS    0H                  CHECK IF ALL CELLS ARE ZERO SPOTS            
         OC    ABUYGRID,ABUYGRID   ALL SPOTS ACCOUNTED FOR                      
         BZ    MMAT100                                                          
MMAT85   TM    MISCFLAG,MFEXACT    NEED TO RESTORE AGENCY BUY GRID              
         BZ    MMAT90              IF WE ARE IN THE EXACT MATCH LOOP            
         MVC   ABUYGRID,ABUYGRD2                                                
         NI    SRTFLAGS,X'FF'-SFMATING                                          
*                                  RESET UNMATCHED BUY FOR NEXT ROUND           
MMAT90   LA    R3,SORTDLQ(R3)                                                   
         B     MMAT50                                                           
*                                                                               
MMAT100  DS    0H                  MARK CELL(S) MATCHED, UPDATE K BUYS          
         BAS   RE,UPDTKBUY                                                      
*                                                                               
*                                                                               
MMATYES  SR    RC,RC               MATCHED OK                                   
MMATNO   LTR   RC,RC               AT LEAST ONE DID NOT MATCH                   
         B     EXIT                                                             
         DROP  AGYBUY,R3                                                        
         EJECT                                                                  
***********************************************************************         
* MARK CELL IN PROCESS                                                          
* R3 IS POINTING TO SORT ENTRY IN PROGRESS                                      
***********************************************************************         
UPDTGRID NTR1                                                                   
UPDTGD00 DS    0H                                                               
         USING SORTD,R3                                                         
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
         TM    SRTFLAGS,SFTCRDT                                                 
         BO    UPGRD20                                                          
         TM    SRTFLAGS,SFCREDIT                                                
         BO    UPGRD45                                                          
*                                                                               
* IF AGENCY SENDS A BUY WITH ZERO TOTAL SPOT/COST, JUST MATCH WITH A            
* REP BUY WITH ZERO TOTAL SPOT/COST. THIS ALLEVIATES THE REP HAVING TO          
* CHANGE SPOTS PER WEEK AND RATE TO ZERO JUST FOR MATCHING                      
*                                                                               
         OC    AGYBUY.RBUYTSPT(6),AGYBUY.RBUYTSPT                               
         BNZ   UPDTGD05                                                         
         OC    RBUYTSPT(6),RBUYTSPT                                             
         BNZ   UPDTGD05                                                         
*                                                                               
* IF AGENCY DID SEND THE RATE, THEN COMPARE IT AGAINST THE REP'S                
*                                                                               
         OC    AGYBUY.RBUYCOS,AGYBUY.RBUYCOS                                    
         BZ    UPGRD02                                                          
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
         BNE   UPGRDNO                                                          
*                                                                               
*                                                                               
* LENGTH, DAY/TIME MUST STILL MATCH FOR ZERO SPOT/COST BUY COMPARES             
*                                                                               
* COMPARE LENGTH                                                                
UPGRD02  DS    0H                                                               
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
         BNE   UPGRDNO                                                          
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDYEL,R4                                                      
*                                                                               
* COMPARE DAY                                                                   
UPGRD03  DS    0H                                                               
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   UPGRDNO                                                          
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BNE   UPGRDNO                                                          
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BNE   UPGRDNO                                                          
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   UPGRD04                                                          
         BAS   RE,NEXTEL                                                        
         BNE   UPGRDNO                                                          
         B     UPGRD04                                                          
*                                                                               
UPGRD04  DS    0H                                                               
         BAS   RE,NEXTEL           DAY/TIME ORBITS MUST MATCH EXACTLY           
         BE    UPGRDNO                                                          
*                                                                               
         OI    SRTFLAGS,SFMATING   SET MATCHING IN PROGRESS                     
         B     UPGRDYES                                                         
*                                                                               
* COMPARE RATE                                                                  
UPDTGD05 DS    0H                                                               
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
         BNE   UPGRDNO                                                          
*                                                                               
* COMPARE LENGTH                                                                
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
         BNE   UPGRDNO                                                          
*                                                                               
* CHECK IF BUY COMMENT (PROGRAM NAME) CHANGED                                   
*                                                                               
*        TM    SRTFLAGS,SFMKGOOD                                                
*        BO    UPGRD20             SKIP PROGRAM NAME CHECK FOR MAKEGOOD         
*        TM    MISCFLAG,MFMGLOOP                                                
*        BO    UPGRD20                                                          
*                                                                               
*        L     R6,AIOAREA                                                       
*        MVI   ELCODE,X'04'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   UPGRDNO                                                          
*        USING RBUYCMEL,R6                                                      
*        CLC   =C'MG=',RBUYCMNT    IF MAKEGOOD, SKIP CHECK                      
*        BE    UPGRD20                                                          
*                                                                               
UPGRD05  DS    0H                                                               
*        L     R4,AIO2                                                          
*        MVI   ELCODE,X'04'                                                     
*        BAS   RE,GETEL2                                                        
*        BNE   UPGRDNO                                                          
AGYBUY   USING RBUYCMEL,R4                                                      
*                                                                               
*        CLC   AGYBUY.RBUYCMLN,RBUYCMLN                                         
*        BNE   UPGRDNO                                                          
*        ZIC   R1,AGYBUY.RBUYCMLN                                               
*        BCTR  R1,0                                                             
*        EX    R1,UPGRD10                                                       
*        BNE   UPGRDNO                                                          
*        B     UPGRD20                                                          
*                                                                               
UPGRD10  CLC   AGYBUY.RBUYCMEL(0),RBUYCMEL                                      
*        DROP  AGYBUY,R6                                                        
*                                                                               
UPGRD20  DS    0H                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDYEL,R4                                                      
*                                                                               
* COMPARE DAY                                                                   
UPGRD30  DS    0H                                                               
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   UPGRDNO                                                          
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BNE   UPGRDNO                                                          
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BNE   UPGRDNO                                                          
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   UPGRD40                                                          
         BAS   RE,NEXTEL                                                        
         BNE   UPGRDNO                                                          
         B     UPGRD30                                                          
*                                                                               
UPGRD40  DS    0H                                                               
         BAS   RE,NEXTEL           DAY/TIME ORBITS MUST MATCH EXACTLY           
         BE    UPGRDNO                                                          
*                                                                               
UPGRD45  DS    0H                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
UPGRD50  DS    0H                                                               
         XC    ELTBUILD,ELTBUILD                                                
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,ELTBUILD)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELTBUILD+9)                          
         MVI   ELTBUILD+8,C'-'                                                  
         GOTO1 PERVAL,DMCB,(17,ELTBUILD),WORK2                                  
         CLI   DMCB+4,0                                                         
         BNE   UPGRDNO                                                          
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,ABUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
         LTR   R1,R1               IF 0 WEEK, GET NEXT EFF DATES                
         BZ    UPGRD130                                                         
*                                                                               
UPGRD60  DS    0H                                                               
         TM    MISCFLAG,MFCREDIT   SKIP CHECK IF BUY HAS CREDIT                 
         BO    UPGRD80             OR BUY IS A CREDIT                           
         TM    SRTFLAGS,SFCREDIT                                                
         BO    UPGRD80             FIRST, CHECK IF                              
         CLC   RBUYDTNW,0(R2)      CONTRACT BUY NPW TOO LARGE.                  
         BH    UPGRDNO             CANNOT HAVE NEGATIVE SPOTS IN CELL           
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    UPGRD70                                                          
         LA    R2,1(R2)                                                         
UPGRD70  BCT   R1,UPGRD60                                                       
*                                                                               
* UPDATE GRID BY SUBTRACTING CELL NPW BY CONTRACT BUY NPW                       
* IF CONTRACT BUY IS A CREDIT, ADD THE NPW BACK IN THE GRID                     
*                                                                               
WKD      USING PERVALD,WORK2                                                    
UPGRD80  ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,ABUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
UPGRD90  DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         ZIC   RF,RBUYDTNW                                                      
         TM    SRTFLAGS,SFCREDIT                                                
         BZ    UPGRD100                                                         
         AR    RE,RF                                                            
         B     UPGRD110                                                         
*                                                                               
UPGRD100 SR    RE,RF                                                            
*                                                                               
UPGRD110 STC   RE,0(R2)                                                         
*                                                                               
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    UPGRD120                                                         
         LA    R2,1(R2)                                                         
UPGRD120 BCT   R1,UPGRD90                                                       
*                                                                               
UPGRD130 DS    0H                                                               
         BAS   RE,NEXTEL           INCASE OF MORE EFF DATES                     
         BE    UPGRD50                                                          
*                                                                               
*************                                                                   
*        L     RE,AIOAREA                                                       
*        USING RBUYREC,RE                                                       
*        CLI   RBUYKMLN,3                                                       
*        BNE   TEST10                                                           
*        TM    MISCFLAG,MFEXACT                                                 
*        BO    TEST10                                                           
*        DC    H'0'                                                             
*        DROP  RE                                                               
TEST10   DS    0H                                                               
*************                                                                   
         OI    SRTFLAGS,SFMATING   SET MATCHING IN PROGRESS                     
*                                                                               
         TM    SRTFLAGS,SFTCRDT+SFCREDIT IS THIS A CREDIT OR TARGET??           
         BZ    UPGRDYES                                                         
         LR    RF,R3                                                            
*                                                                               
UPGRD140 DS    0H                                                               
         LA    RF,SORTDLQ(RF)      GET CREDIT BUY AND                           
NEXTBUYD USING SORTD,RF                                                         
         CLI   NEXTBUYD.SRTAGYBY,0          COLLAPSE TO GRID                    
         BE    UPGRDYES                                                         
         CLC   SRTTKBUY,NEXTBUYD.SRTTKBUY                                       
         BNE   UPGRD140                                                         
         TM    NEXTBUYD.SRTFLAGS,SFCREDIT                                       
         BZ    UPGRD140                                                         
         LR    R3,RF                                                            
         B     UPDTGD00                                                         
         DROP  NEXTBUYD                                                         
*                                                                               
UPGRDYES SR    RC,RC               CHANGES FOUND                                
UPGRDNO  LTR   RC,RC               NO CHANGES FOUND                             
UPGRDX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* MARK CELL MATCHED AND UPDATE K BUYS POINTING TO THIS AGENCY BUY FOR           
* MANUAL ADDED BUYS                                                             
***********************************************************************         
UPDTKBUY NTR1                                                                   
         LA    R3,SORTAREA                                                      
         USING SORTD,R3                                                         
UPKBY05  DS    0H                                                               
         TM    SRTFLAGS,SFMATCHD                                                
         BO    UPKBY40                                                          
         TM    SRTFLAGS,SFMATING                                                
         BZ    UPKBY40                                                          
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         CLC   RBUYAGBL,AGYBUY.RBUYAGBL                                         
         BE    UPKBY30                                                          
*                                                                               
* LINK CHANGED, NEED TO UPDATE CONTRACT BUY RECORD                              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RBUYAGBL,AGYBUY.RBUYAGBL                                         
*                                                                               
UPKBY20  DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
UPKBY30  DS    0H                                                               
         MVC   SRTAGYBY,AGYBUY.RBUYAGBL  UPDATE SORT TABLE                      
         OI    SRTFLAGS,SFMATCHD                                                
         DROP  R6,AGYBUY                                                        
*                                                                               
UPKBY40  DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   UPKBY05             ALL DONE                                     
*                                                                               
UPKBYX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AUTOMATIC REVISION HANDLING                                                   
* THE DIFFERENT METHODS OF PROCESSING AGENCY REVISION ARE SET BY A              
* REP PROFILE                                                                   
***********************************************************************         
AUTOGEN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =A(CHKCRED),RR=RELO CHECK IF ANY CREDITS                         
*                                  MUST DO MANUAL FOR NOW                       
*                                                                               
         NI    MISCFLAG,X'FF'-MFMGLOOP                                          
         NI    MISCFLG2,X'FF'-MF20SBUY                                          
         NI    MISCFLG2,X'FF'-MF2NOXIT                                          
         OC    SPOTCTR,SPOTCTR     CALCULATE TOTALS IF NOT ALREADY              
         BNZ   AUTO10                                                           
         OC    WEEKCTR,WEEKCTR                                                  
         BNZ   AUTO10                                                           
         OC    BUYCOST,BUYCOST                                                  
         BZ    AUTO20                                                           
AUTO10   DS    0H                                                               
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
AUTO20   DS    0H                                                               
         L     R4,AIO2             SKIP IF REMOVED FROM SCHEDULE                
AGYBUY   USING RBUYREC,R4          DONT' MATCH SO K BUY WILL BE                 
         OC    AGYBUY.RBUYNW,AGYBUY.RBUYNW                                      
         BZ    AUTO45              CHECK PROFILE IN METHOD 3 IF WE'RE           
         DROP  AGYBUY              CANCELLING THE BUYLINES                      
*                                                                               
         CLI   STAMETH,0           STATION AUTO METHOD OVERRIDE??               
         BE    AUTO50                                                           
*                                                                               
         CLI   STAMETH,1                                                        
         BNE   AUTO30                                                           
         BAS   RE,METHOD1                                                       
         B     AUTO80                                                           
*                                                                               
AUTO30   DS    0H                                                               
         CLI   STAMETH,2                                                        
         BNE   AUTO40                                                           
         BAS   RE,METHOD2                                                       
         B     AUTO80                                                           
*                                                                               
AUTO40   DS    0H                                                               
         CLI   STAMETH,3                                                        
         BNE   AUTO50                                                           
*                                                                               
AUTO45   DS    0H                                                               
         BAS   RE,METHOD3                                                       
         B     AUTO80                                                           
*                                                                               
AUTO50   DS    0H                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AH    R4,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R4                                                       
*                                                                               
         TM    SVPGPBIT+CNTDRV1B,CNTDRV1A                                       
         BZ    AUTO60                                                           
         BAS   RE,METHOD1                                                       
         B     AUTO80                                                           
*                                                                               
AUTO60   DS    0H                                                               
         TM    SVPGPBIT+CNTDRV2B,CNTDRV2A                                       
         BZ    AUTO70                                                           
         BAS   RE,METHOD2                                                       
         B     AUTO80                                                           
*                                                                               
AUTO70   DS    0H                                                               
         TM    SVPGPBIT+CNTDRV3B,CNTDRV3A                                       
         BZ    INVMETH             MUST SPECIFY A METHOD                        
         BAS   RE,METHOD3                                                       
*                                                                               
AUTO80   DS    0H                                                               
*                                                                               
AUTOX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* METHOD 1 : CANCEL AND SUPERSEDE                                               
*            IF CHANGES FOUND,                                                  
*            CANCEL EXISTING BUYLINE AND GENERATES A NEW BUYLINE                
* NOTE: TARGET MAKEGOOD/CREDIT OR MAKEGOOD/CREDIT K BUYS WILL ALWAYS            
* DEFAULT TO METHOD 3. THE MAKEGOOD/CREDIT ELEMENTS WILL BE PRESERVED           
* IN ADDITION, MAKEGOOD/CREDIT K BUYS WILL ALSO DEFAULT TO METHOD 3             
***********************************************************************         
METHOD1  NTR1                                                                   
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         LA    R3,SORTAREA                                                      
         USING SORTD,R3                                                         
*                                                                               
MTH10010 DS    0H                                                               
         TM    SRTFLAGS,SFMATCHD   CHECK IF MATCHED ALREADY                     
         BO    MTH10020                                                         
         CLI   AGBUYMTR,0          IS AGENCY A MAKEGOOD LINE??                  
         BE    MTH10015                                                         
         TM    SRTFLAGS,SFMKGOOD+SFTKMKGD ONLY LOOK AT K MAKEGOOD BUYS          
         BZ    MTH10020                                                         
         CLC   AGBUYMTR,SRTAGYBY   ORIGINAL LINKS MUST MATCH                    
         BE    METHOD3A                                                         
         B     MTH10020                                                         
*                                                                               
MTH10015 DS    0H                  AGENCY BUY NOT A MKGD, REGULAR CHECK         
         CLC   AGYBUY.RBUYAGBL,SRTAGYBY                                         
         BNE   MTH10020                                                         
         TM    SRTFLAGS,SFMKGOOD+SFTKMKGD                                       
         BNZ   MTH10020            SKIP MAKEGOOD CONTRACT BUYS                  
*                                                                               
* FOUND MATCHING K BUY. IF K BUY IS A TARGET FOR MAKEGOOD OR CREDIT,            
* CAN ONLY BE UPDATED VIA METHOD 3 (REPLACE BUY)                                
*                                                                               
         TM    SRTFLAGS,SFTMKGD+SFTCRDT                                         
         BNZ   METHOD3A                                                         
         B     MTH10040                                                         
*                                                                               
MTH10020 DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   MTH10010            ALL DONE                                     
*                                                                               
MTH10030 DS    0H                  NO MATCH FOUND, ADD AS NEW LINE              
         GOTO1 =A(ADDAGBUY),RR=RELO                                             
         B     MTH1X                                                            
*                                                                               
MTH10040 DS    0H                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
* COMPARE RATE                                                                  
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
         BNE   MTH10090                                                         
*                                                                               
* COMPARE LENGTH                                                                
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
         BNE   MTH10090                                                         
         DROP  AGYBUY                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDYEL,R4                                                      
*                                                                               
* COMPARE DAY                                                                   
MTH10050 DS    0H                                                               
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   MTH10090                                                         
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BNE   MTH10090                                                         
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BNE   MTH10090                                                         
*                                                                               
         TM    RBUYDAYS,X'80'      CC OVERRIDE SET IN REPPAK?                   
         BZ    *+8                 RETAIN FLAG AFTER APPROVAL                   
         OI    AGYBUY.RBUYDAYS,X'80'                                            
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   MTH10060                                                         
         BAS   RE,NEXTEL                                                        
         BE    MTH10050                                                         
         B     MTH10090                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH10060 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH10090                                                         
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDTEL,R4                                                      
*                                                                               
* COMPARE NUMBER OF SPOTS                                                       
MTH10070 DS    0H                                                               
         CLI   RBUYDTNW,0          SKIP FOR PARTIAL CANCEL                      
         BE    MTH10075                                                         
         CLC   AGYBUY.RBUYDTNW,RBUYDTNW                                         
         BNE   MTH10090                                                         
*                                                                               
* COMPARE START AND END DATES                                                   
         CLC   AGYBUY.RBUYDTST(6),RBUYDTST                                      
         BNE   MTH10090                                                         
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF ORBITS                             
         BNE   MTH10080                                                         
*                                                                               
MTH10075 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH10070                                                         
         B     MTH10090                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH10080 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH10090                                                         
*                                                                               
* CHECK IF PROGRAM NAME CHANGED                                                 
* IF NO PROGRAM NAME ELEMENT FOUND, CHECK IF COMMENT STARTS WITH P=             
* P= IS OLD STYPE PROGRAM NAME                                                  
* CHECK IF BUY COMMENT (PROGRAM NAME) CHANGED                                   
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   MTH10085                                                         
         USING RBUYPGEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BE    MTH10082                                                         
*                                                                               
*        DC    H'0'                DON'T DIE                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'21034B'     FAKE A 21 ELEMENT WITH .                  
         LA    R4,WORK                                                          
*                                                                               
MTH10082 DS    0H                                                               
AGYBUY   USING RBUYPGEL,R4                                                      
*                                                                               
         CLC   AGYBUY.RBUYPGLN,RBUYPGLN                                         
         BNE   MTH10090                                                         
         ZIC   R1,AGYBUY.RBUYPGLN                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   AGYBUY.RBUYPGM(0),RBUYPGM                                        
         BNE   MTH10090                                                         
*                                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH1X               NO CHANGES, DO NOTHING                       
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH10085 DS    0H                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYCMEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYCMEL,R4                                                      
*                                                                               
         CLC   AGYBUY.RBUYCMLN,RBUYCMLN                                         
         BNE   MTH10090                                                         
         ZIC   R1,AGYBUY.RBUYCMLN                                               
         BCTR  R1,0                                                             
         EX    R1,MTH10087                                                      
         BNE   MTH10090                                                         
*                                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH1X               NO CHANGES, DO NOTHING                       
*                                                                               
MTH10087 CLC   AGYBUY.RBUYCMEL(0),RBUYCMEL                                      
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH10090 DS    0H                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         OI    SRTFLG2,SF2METH1                                                 
         MVC   KEY+28(4),SRTBUYDA                                               
         L     R6,AIOAREA                                                       
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING RBUYREC,R6                                                       
         MVI   RBUYCNTL,X'80'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RBUYCHGD)  UPDATE LAST CHANGE DATE          
         MVC   RBUYCHGI,=C'C '     CANCEL INDICATOR                             
*                                                                               
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         MVC   RBUYKMOD,RCONMOD                                                 
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    MTH10100                                                         
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    MTH10100                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
MTH10100 DS    0H                                                               
         MVC   RBUYVER,RCONSRV STORE REP VERSION NO. IN BUY                     
         MVC   VERDFLT,RBUYVER                                                  
         DROP  R4,R6                                                            
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R4                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    MTH10110                                                         
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
MTH10110 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R3,R4                                                            
*                                                                               
         MVC   AIO,AIOAREA         WRITE OUT CANCELLED CONTRACT BUY REC         
         GOTO1 PUTREC                                                           
         L     R6,AIOAREA                                                       
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MTH10120                                                         
         MVI   KEY+27,X'80'        DELETE KEY AS WELL                           
         GOTO1 WRITE                                                            
*                                                                               
MTH10120 DS    0H                                                               
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  BACK OUT BUCKETS                             
         NI    BUCKFLGS,X'FF'-X'10' RESET                                       
*                                                                               
         GOTO1 =A(PARTCAN),RR=RELO ADJUST AGENCY BUY FOR ANY                    
*                                  PARTIAL CANCELS                              
*        BAS   RE,ADDAGBUY         ADD SUPERSEDE AGENCY BUY                     
         GOTO1 =A(ADDAGBUY),RR=RELO                                             
*                                                                               
* ADD CANCELLED BUY REPLACEMENT REFERENCE COMMENT                               
*                                                                               
         L     R6,AIOAREA                                                       
         MVC   KEY(27),0(R6)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIOAREA                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYOCEL,ELTBUILD                                                
         MVI   ELM.RBUYOCCD,X'84'                                               
         MVI   ELM.RBUYOCID,X'80'                                               
         MVC   ELM.RBUYOCNT(23),=C'CANCELLED REFER TO BUY '                     
         L     R6,AIO2                                                          
AGYBUY   USING RBUYREC,R6                                                       
         EDIT  AGYBUY.RBUYKMLN,(3,ELM.RBUYOCNT+23),ALIGN=LEFT                   
         LA    R1,26                                                            
         AR    R1,R0                                                            
         STC   R1,ELM.RBUYOCLN                                                  
         DROP  ELM,AGYBUY                                                       
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
         MVC   RBUYVER,VERDFLT     UPDATE VERSION NUMBER                        
         DROP  R6                                                               
*                                                                               
         GOTOR COPY0D                                                           
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIOAREA),0,0                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIOAREA,ELTBUILD,0                 
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
MTH1X    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* METHOD 2 : ADD A NEW LINE                                                     
*            ADD A NEW LINE FOR ADDITIONAL WEIGHT                               
* NOTE: TARGET MAKEGOOD/CREDIT OR MAKEGOOD/CREDIT K BUYS WILL ALWAYS            
* DEFAULT TO METHOD 3. THE MAKEGOOD/CREDIT ELEMENTS WILL BE PRESERVED           
***********************************************************************         
METHOD2  NTR1                                                                   
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         LA    R3,SORTAREA                                                      
         USING SORTD,R3                                                         
*                                                                               
MTH20010 DS    0H                                                               
         TM    SRTFLAGS,SFMATCHD   CHECK IF MATCHED ALREADY                     
         BO    MTH20030                                                         
         CLI   AGBUYMTR,0          IS AGENCY A MAKEGOOD LINE??                  
         BE    MTH20020                                                         
         TM    SRTFLAGS,SFMKGOOD+SFTKMKGD ONLY LOOK AT K MAKEGOOD BUYS          
         BZ    MTH20030                                                         
         CLC   AGBUYMTR,SRTAGYBY   ORIGINAL LINKS MUST MATCH                    
         BE    METHOD3A                                                         
         B     MTH20030                                                         
*                                                                               
MTH20020 DS    0H                  AGENCY BUY NOT A MKGD, REGULAR CHECK         
         CLC   AGYBUY.RBUYAGBL,SRTAGYBY                                         
         BNE   MTH20030                                                         
         TM    SRTFLAGS,SFMKGOOD+SFTKMKGD                                       
         BNZ   MTH20030            SKIP MAKEGOOD CONTRACT BUYS                  
*                                                                               
* FOUND MATCHING K BUY. IF K BUY IS A TARGET FOR MAKEGOOD OR CREDIT,            
* CAN ONLY BE UPDATED VIA METHOD 3 (REPLACE BUY)                                
*                                                                               
         TM    SRTFLAGS,SFTMKGD+SFTCRDT                                         
         BNZ   METHOD3A                                                         
         B     MTH20050                                                         
*                                                                               
MTH20030 DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   MTH20010            ALL DONE                                     
*                                                                               
MTH20040 DS    0H                  CHECK IF BONUS LINES ADDED                   
         GOTO1 =A(MANMATCH),RR=RELO                                             
         BZ    MTH2X                                                            
*                                  NO MATCH FOUND, ADD AS NEW LINE              
         GOTO1 =A(ADDAGBUY),RR=RELO                                             
         B     MTH2X                                                            
*                                                                               
MTH20050 DS    0H                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
* IF AGENCY SENT OVER A BUY WITH NO SPOTS, JUST OVERWRITE                       
*                                                                               
         OC    AGYBUY.RBUYTSPT,AGYBUY.RBUYTSPT                                  
         BZ    METHOD3A                                                         
*                                                                               
* COMPARE RATE                                                                  
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
         BNE   METHOD3A                                                         
*                                                                               
* COMPARE LENGTH                                                                
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
         BNE   METHOD3A                                                         
         DROP  AGYBUY                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDYEL,R4                                                      
*                                                                               
* COMPARE DAY                                                                   
MTH20060 DS    0H                                                               
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   METHOD3A                                                         
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BNE   METHOD3A                                                         
*                                                                               
* COMPARE TIME                                                                  
MTH20065 DS    0H                                                               
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BNE   METHOD3A                                                         
*                                                                               
         TM    RBUYDAYS,X'80'      CC OVERRIDE SET IN REPPAK?                   
         BZ    *+8                 RETAIN FLAG AFTER APPROVAL                   
         OI    AGYBUY.RBUYDAYS,X'80'                                            
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   MTH20070                                                         
         BAS   RE,NEXTEL                                                        
         BE    MTH20060                                                         
         B     METHOD3A                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH20070 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    METHOD3A                                                         
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDTEL,R4                                                      
*                                                                               
* COMPARE START AND END DATES                                                   
MTH20080 DS    0H                                                               
         CLI   RBUYDTNW,0          SKIP FOR PARTIAL CANCEL                      
         BE    MTH20090                                                         
         CLC   AGYBUY.RBUYDTST,RBUYDTST                                         
         BNE   MTH20083                                                         
         CLC   AGYBUY.RBUYDTED,RBUYDTED                                         
         BE    MTH20085            MATCHING DATES??                             
*                                                                               
MTH20083 DS    0H                                                               
         CLC   AGYBUY.RBUYDTST,RBUYDTST                                         
         BH    METHOD3A                                                         
         CLC   AGYBUY.RBUYDTED,RBUYDTED                                         
         BNL   MTH20120            ADDITIONAL WEIGHT??                          
         B     METHOD3A            NO, REVISE BUY                               
*                                                                               
* COMPARE NUMBER OF SPOTS                                                       
*                                                                               
MTH20085 DS    0H                                                               
         CLC   RBUYDTNW,AGYBUY.RBUYDTNW                                         
         BH    METHOD3A            NEW BUY HAS LESS SPOTS, OVERWRITE            
         BNE   MTH20120            EXISTING REP BUY INSTEAD                     
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MORE EFFECTIVE DATES               
         BNE   MTH20100                                                         
*                                                                               
MTH20090 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH20080                                                         
         B     MTH20120                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH20100 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    METHOD3A            PARTIAL CANCEL, REPLACE BUY                  
*                                                                               
* CHECK IF PROGRAM NAME CHANGED                                                 
* IF NO PROGRAM NAME ELEMENT FOUND, CHECK IF COMMENT STARTS WITH P=             
* P= IS OLD STYPE PROGRAM NAME                                                  
* CHECK IF BUY COMMENT (PROGRAM NAME) CHANGED                                   
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   MTH20115                                                         
         USING RBUYPGEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BE    MTH20111                                                         
*                                                                               
*        DC    H'0'                DON'T DIE                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'21034B'     FAKE A 21 ELEMENT WITH .                  
         LA    R4,WORK                                                          
MTH20111 DS    0H                                                               
AGYBUY   USING RBUYPGEL,R4                                                      
*                                                                               
         CLC   AGYBUY.RBUYPGLN,RBUYPGLN                                         
         BNE   MTH20120                                                         
         ZIC   R1,AGYBUY.RBUYPGLN                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   AGYBUY.RBUYPGM(0),RBUYPGM                                        
         BNE   MTH20120                                                         
*                                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH2X               NO CHANGES, DO NOTHING                       
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH20115 DS    0H                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   METHOD3A                                                         
         USING RBUYCMEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BNE   MTH20118                                                         
AGYBUY   USING RBUYCMEL,R4                                                      
*                                                                               
         CLC   AGYBUY.RBUYCMLN,RBUYCMLN                                         
         BNE   MTH20120                                                         
         ZIC   R1,AGYBUY.RBUYCMLN                                               
         BCTR  R1,0                                                             
         EX    R1,MTH20110                                                      
         BNE   MTH20120                                                         
*                                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH2X               NO CHANGES, DO NOTHING                       
*                                                                               
MTH20110 CLC   AGYBUY.RBUYCMEL(0),RBUYCMEL                                      
*                                                                               
MTH20118 DS    0H                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BE    MTH20119                                                         
*        DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'21034B'     FAKE A 21 ELEMENT WITH .                  
         LA    R4,WORK                                                          
MTH20119 DS    0H                                                               
AGYBUY   USING RBUYPGEL,R4                                                      
*                                                                               
         ZIC   R1,AGYBUY.RBUYPGLN                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   AGYBUY.RBUYPGM(0),RBUYCMNT+2                                     
         BNE   MTH20120                                                         
*                                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH2X               NO CHANGES, DO NOTHING                       
         DROP  AGYBUY,R6                                                        
*                                                                               
* ADDITIONAL WEIGHT FOUND (EXTEND FLIGHT END DATE OR INCREASE IN THE            
* NUMBER OF SPOTS)                                                              
*                                                                               
MTH20120 DS    0H                                                               
         OI    SRTFLG2,SF2METH2                                                 
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    MTH20130                                                         
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    MTH20130                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R4                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R4                                                               
*                                                                               
MTH20130 DS    0H                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    MTH20140                                                         
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
MTH20140 OI    RCONCONF,X'80'      NOT CONFIRMED                                
*                                                                               
* IF AGENCY PARTICAL CANCELS A BUY, THE BUY WILL GET REVISED (METHOD 3)         
* IF USERS WANT TO DO COMBINATION OF PARTICAL CANCEL AND ADD WEIGHT             
* PUT THIS ROUTINE BACK IN AND SELECTIVELY SKIPPED DELETION OF THESE            
* ZERO SPOT X'03' ELEMENTS IN THE ADWEIGHT ROUTINE.                             
*                                                                               
*>>>>>>  GOTO1 =A(PARTCAN),RR=RELO ADJUST AGENCY BUY FOR ANY                    
*                                                                               
         GOTO1 =A(ADWEIGHT),RR=RELO BUILD NEW BUY WITH ADDED WEIGHT             
         BNZ   METHOD3A            CANNOT GENERATE ADDITIONAL WEIGHT            
*                                  REPLACE EXISTING BUY INSTEAD                 
         GOTO1 =A(BLDWCMT),RR=RELO BUILD WEIGHT ADDED COMMENT                   
*                                  PARTIAL CANCELS                              
         GOTO1 =A(ADDAGBUY),RR=RELO ADD NEW LINE FOR ADDITIONAL WEIGHT          
*                                                                               
         OI    SRTFLAGS,SFMATCHD   MARK BUY MATCHED                             
*                                                                               
MTH2X    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* METHOD 3 : CHANGE AN EXISTING LINE                                            
* NOTE: TARGET MAKEGOOD/CREDIT OR MAKEGOOD/CREDIT K BUYS WILL ALWAYS            
* DEFAULT TO METHOD 3. THE MAKEGOOD/CREDIT ELEMENTS WILL BE PRESERVED           
* IN ADDITION, MAKEGOOD/CREDIT K BUYS WILL ALSO DEFAULT TO METHOD 3             
***********************************************************************         
METHOD3  NTR1                                                                   
METHOD3A DS    0H                  METHOD 2 WITH CHANGES NOT ADDITIONS          
*                                  IN WEIGHT                                    
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         LA    R3,SORTAREA                                                      
         USING SORTD,R3                                                         
MTH30010 DS    0H                                                               
         TM    SRTFLAGS,SFMATCHD   CHECK IF MATCHED ALREADY                     
         BO    MTH30030                                                         
         CLI   AGBUYMTR,0          IS AGENCY A MAKEGOOD LINE??                  
         BE    MTH30020                                                         
         TM    SRTFLAGS,SFMKGOOD   ONLY LOOK AT K MAKEGOOD BUYS                 
         BZ    MTH30030                                                         
         CLC   AGBUYMTR,SRTAGYBY   ORIGINAL TARGETS MUST MATCH                  
         BE    MTH30070                                                         
         B     MTH30030                                                         
*                                                                               
MTH30020 DS    0H                  AGENCY BUY NOT A MKGD, REGULAR CHECK         
         CLC   AGYBUY.RBUYAGBL,SRTAGYBY                                         
         BNE   MTH30030                                                         
*                                                                               
* CONTRACT PROFILE 36 TO CANCEL CONTRACT BUY IF AGENCY HAS SENT A BUY           
* WITH NO SPOTS. ONLY WORKS IF CONTRACT BUY IS NOT A MAKEGOOD TARGET,           
* CREDIT TARGET OR HAS OUTSTANDING MAKEGOOD OFFERS                              
*                                                                               
         OC    AGYBUY.RBUYTSPT(6),AGYBUY.RBUYTSPT                               
         BNZ   MTH30070                                                         
*                                                                               
         LR    R6,RA               USE R2 TO COVER THE ENTRY                    
         AH    R6,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R6                                                       
         TM    SVPGPBIT+CNTDRCNB,CNTDRCNA                                       
         BO    MTH30070            CHECK PROFILE TO CANCEL 0 SPOT BUY           
         DROP  R6                                                               
*                                                                               
         TM    SRTFLAGS,SFTMKGD+SFTCRDT                                         
         BNZ   MTH30070            CANNOT BE A MKGD/CREDIT TARGET               
         TM    SRTFLG2,SF2MGOF                                                  
         BNZ   MTH30070            CANNOT HAVE PENDING MKGD OFFERS              
         B     MTH3X                                                            
*                                                                               
MTH30030 DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   MTH30010            ALL DONE                                     
*                                                                               
MTH30060 DS    0H                  CHECK REMAINING BUYS WITH NO LINK            
         GOTO1 =A(MANMATCH),RR=RELO                                             
         BZ    MTH2X                                                            
*                                  NO MATCH FOUND, ADD AS NEW LINE              
         GOTO1 =A(ADDAGBUY),RR=RELO                                             
         B     MTH3X                                                            
*                                                                               
MTH30070 DS    0H                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
* IF CONTRACT BUY IS A CREDIT TARGET, WE NEED TO COMBINE THE CREDITS            
* AGAINST THIS CONTRACT BUY BEFORE COMPARING TO THE AGENCY'S. IF THERE          
* ARE ANY CHANGES, THE CREDIT MUST BE REMOVED FROM THE TARGET BUY               
* BEFORE WRITING IT BACK TO THE FILE                                            
*                                                                               
         TM    SRTFLAGS,SFTCRDT                                                 
         BZ    MTH30080                                                         
         LA    R1,SORTDLQ(R3)                                                   
         TM    SRTFLAGS-SORTD(R1),SFCREDIT                                      
         BZ    MTH30080            NEXT BUY IN SRT TABLE MUST BE CREDIT         
*                                  PROCESS CREDIT BUY                           
         GOTO1 =A(PROCREDT),RR=RELO                                             
         BNZ   MTH3X               NO CHANGE                                    
         B     MTH30180            CHANGE FOUND                                 
*                                                                               
MTH30080 DS    0H                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
* IF AGENCY SENT OVER A BUY WITH NO SPT/WK, RETAIN CURRENT REP                  
* BUY, BUT OVERRIDE ALL DATES WILL 0 SPOT.                                      
*                                                                               
         NI    MISCFLG2,X'FF'-MF20SBUY                                          
         OC    AGYBUY.RBUYTSPT(6),AGYBUY.RBUYTSPT                               
         BNZ   MTH30085                                                         
         OI    MISCFLG2,MF20SBUY                                                
         B     MTH30180                                                         
*                                                                               
* COMPARE RATE                                                                  
MTH30085 DS    0H                                                               
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
         BNE   MTH30170                                                         
*                                                                               
* COMPARE LENGTH                                                                
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
         BNE   MTH30170                                                         
         DROP  AGYBUY                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDYEL,R4                                                      
*                                                                               
* COMPARE DAY                                                                   
MTH30090 DS    0H                                                               
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   MTH30170                                                         
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BNE   MTH30170                                                         
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BNE   MTH30170                                                         
*                                                                               
         TM    RBUYDAYS,X'80'      CC OVERRIDE SET IN REPPAK?                   
         BZ    *+8                 RETAIN FLAG AFTER APPROVAL                   
         OI    AGYBUY.RBUYDAYS,X'80'                                            
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   MTH30100                                                         
         BAS   RE,NEXTEL                                                        
         BE    MTH30090                                                         
         B     MTH30170                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH30100 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH30170                                                         
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDTEL,R4                                                      
*                                                                               
MTH30110 DS    0H                                                               
         CLI   RBUYDTNW,0          SKIP FOR PARTIAL CANCEL                      
         BE    MTH30120                                                         
*                                                                               
* COMPARE NUMBER OF SPOTS                                                       
         CLC   RBUYDTNW,AGYBUY.RBUYDTNW                                         
         BNE   MTH30170                                                         
*                                                                               
* COMPARE START AND END DATES                                                   
         CLC   RBUYDTST(6),AGYBUY.RBUYDTST                                      
         BNE   MTH30170                                                         
*                                                                               
         BAS   RE,NEXTEL2          INCASE OF ORBITS                             
         BNE   MTH30130                                                         
*                                                                               
MTH30120 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH30110                                                         
         B     MTH30170                                                         
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH30130 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    MTH30170                                                         
*                                                                               
* CHECK IF BUY COMMENT (PROGRAM NAME) CHANGED                                   
*                                                                               
MTH30140 DS    0H                                                               
         TM    SRTFLAGS,SFTKMKGD   SKIP PROGRAM NAME CHECK                      
         BO    MTH30155            FOR DARE TAKEOVER MAKEGOOD                   
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   MTH30170                                                         
         USING RBUYCMEL,R6                                                      
         TM    SRTFLAGS,SFMKGOOD+SFCREDIT                                       
         BZ    MTH30150                                                         
         BAS   RE,NEXTEL           IF CREDIT/MAKEGOOD, FIRST LINE               
         BNE   MTH30170            IS RESERVED FOR MG=/CR= REFERENCE            
*                                                                               
MTH30150 DS    0H                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BNE   MTH30170                                                         
*                                                                               
AGYBUY   USING RBUYCMEL,R4                                                      
*                                                                               
         CLC   AGYBUY.RBUYCMLN,RBUYCMLN                                         
         BNE   MTH30170                                                         
         ZIC   R1,AGYBUY.RBUYCMLN                                               
         BCTR  R1,0                                                             
         EX    R1,MTH30160                                                      
         BNE   MTH30170                                                         
*                                                                               
MTH30155 DS    0H                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         B     MTH3X               NO CHANGES, DO NOTHING                       
*                                                                               
MTH30160 CLC   AGYBUY.RBUYCMEL(0),RBUYCMEL                                      
         DROP  AGYBUY,R6                                                        
*                                                                               
* CHANGE EXISTING LINE                                                          
*                                                                               
MTH30170 DS    0H                                                               
         GOTO1 =A(PARTCAN),RR=RELO ADJUST AGENCY BUY FOR ANY                    
*                                  PARTIAL CANCELS                              
MTH30180 DS    0H                                                               
         OI    SRTFLAGS,SFMATCHD                                                
         OI    SRTFLG2,SF2METH3                                                 
         MVC   KEY+28(4),SRTBUYDA                                               
         L     R6,AIOAREA                                                       
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING RBUYREC,R6                                                       
*                                                                               
MTH30183 DS    0H                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
         MVC   AGYBUY.RBUYKEY,RBUYKEY                                           
         DROP  AGYBUY,R6                                                        
*                                                                               
MTH30185 DS    0H                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    MTH30190                                                         
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    MTH30190                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
*                                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
MTH30190 DS    0H                                                               
         L     R6,AIO2                                                          
*                                                                               
         TM    MISCFLG2,MF20SBUY                                                
         BZ    MTH30193            UPDATE REP BUY INSTEAD IF                    
         TM    SRTFLG2,SF20SPTS    ORIGINAL K BUY HAS ZERO SPOTS TO             
         BO    MTH30195            BEGIN, NO #SPTS WAS CHANGED                  
         L     R6,AIOAREA          AGENCY SENT 0 SPT BUY                        
*                                                                               
MTH30193 DS    0H                                                               
         USING RBUYREC,R6                                                       
         MVC   RBUYVER,RCONSRV     STORE REP VERSION NO. IN BUY                 
         DROP  R4,R6                                                            
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
MTH30195 DS    0H                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R4                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    MTH30200                                                         
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
MTH30200 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIOAREA         FOR BUY TO BE REPLACED                       
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  BACK OUT BUCKETS                             
         NI    BUCKFLGS,X'FF'-X'10' RESET                                       
*                                                                               
* IF AGENCY SENT ZERO BUY, OVERRIDE REP BUYS WITH 0 SPOTS, INSERT               
* COMMENT AND REWRITE REP BUY INSTEAD OF AGENCY                                 
*                                                                               
         TM    MISCFLG2,MF20SBUY                                                
         BZ    MTH30220                                                         
*                                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         TM    SRTFLG2,SF20SPTS    ORIGINAL K BUY HAS ZERO SPOTS TO             
         BO    *+10                BEGIN, NO #SPTS WAS CHANGED                  
         MVC   RBUYCHGI,=C'S '     SPOTS PER WEEK CHANGED                       
         XC    RBUYTSPT,RBUYTSPT   0 TOTAL SPOTS                                
         XC    RBUYTCOS,RBUYTCOS   0 TOTAL COSTS                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
MTH30210 DS    0H                                                               
         USING RBUYDTEL,R6                                                      
         OI    RBUYDTIN,X'01'      NPW OVERRIDE                                 
         MVI   RBUYDTNW,0          0 OUT DATES                                  
         BAS   RE,NEXTEL                                                        
         BE    MTH30210                                                         
         DROP  R6                                                               
*                                                                               
         OI    SRTFLG2,SF2CAN      MARKED AS CANCEL FOR DISPLAY LATER           
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIOAREA),0,0                
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYOCEL,ELTBUILD                                                
         MVI   ELM.RBUYOCCD,X'84'                                               
         MVI   ELM.RBUYOCLN,20                                                  
         MVI   ELM.RBUYOCID,X'80'                                               
         MVC   ELM.RBUYOCNT(17),=C'BUYLINE CANCELLED'                           
*                                                                               
         TM    SRTFLAGS,SFTMKGD+SFTCRDT                                         
         BZ    MTH30215                                                         
         TM    SRTFLG2,SF20SPTS    ORIGINAL K BUY HAS ZERO SPOTS TO             
         BO    MTH30218            BEGIN, NO #SPTS WAS CHANGED                  
         MVC   ELM.RBUYOCNT(17),=C'# OF SPTS CHANGED'                           
         DROP  ELM                                                              
*                                                                               
MTH30215 DS    0H                                                               
         GOTOR COPY0D                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIOAREA,ELTBUILD,0                 
*                                                                               
MTH30218 DS    0H                                                               
         GOTO1 PUTREC                                                           
         B     MTH3X                                                            
         EJECT                                                                  
MTH30220 DS    0H                  GET MODIFICATION CODES                       
         GOTO1 =A(GETMDCD),RR=RELO SET CHANGE CODES                             
*                                                                               
         GOTO1 =A(MKGDCRDT),RR=RELO                                             
*                                  PRESERVE MAKEGOOD/CREDIT ELEMENTS            
         MVC   AIO,AIO2            WRITE OUT REVISED AGENCY BUYS                
         GOTO1 PUTREC                                                           
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  ADD  NEW BUCKETS                             
*                                                                               
MTH3X    DS    0H                  UPDATE LINK                                  
         TM    SRTFLAGS,SFMKGOOD                                                
         BZ    MTH3XX                                                           
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
         CLI   RBUYAGBL,0                                                       
         BE    MTH3XX                                                           
         MVC   SRTAGYBY,RBUYAGBL                                                
         DROP  R6                                                               
*                                                                               
         BAS   RE,UPDTLINK                                                      
*                                                                               
MTH3XX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE SORTAREA WITH THE LATEST LINK ENTRIES                                  
* MAINLY FOR MAKEGOOD FOR MAKEGOODS                                             
* R3 HAS CURRENT ENTRY                                                          
***********************************************************************         
UPDTLINK NTR1                                                                   
*                                                                               
         LA    R2,SORTAREA                                                      
ULD      USING SORTD,R2                                                         
*                                                                               
UPLK10   DS    0H                                                               
         CLI   ULD.SRTAGYBY,0                                                   
         BE    UPLKX                                                            
         TM    ULD.SRTFLAGS,SFMATCHD   UNMATCHED                                
         BO    UPLK20                                                           
         TM    ULD.SRTFLAGS,SFMKGOOD   MAKEGOODS                                
         BZ    UPLK20                                                           
         CLI   ULD.SRTAGYBY,X'FF'  WITH NO LINK ONLY                            
         BNE   UPLK20                                                           
         CLC   SRTCONBY,ULD.SRTTKBUY                                            
         BNE   UPLK20              SAVE LATEST LINK                             
         MVC   ULD.SRTAGYBY,SRTAGYBY                                            
*                                                                               
UPLK20   DS    0H                                                               
         LA    R2,SORTDLQ(R2)                                                   
         B     UPLK10                                                           
*                                                                               
UPLKX    DS    0H                                                               
         B     EXIT                                                             
         DROP  ULD                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* PROPAGATE AGENCY DEMO CHANGES                                                 
* USING THE REP BUY'S LINK TO ITS AGENCY COUNTERPART. WE'LL COMPARE THE         
* TWO TO SEE IF WE NEED TO UPDATE ANY DEMO CATEGORY/VALUE CHANGES               
* IF CATEGORY IS CHANGED, REP OVERRIDE 0E ELE IS DELETED, AND AGY               
* 0D ELEMENT VALUES ARE RESET TO 0                                              
* NOTE: WORK2 IS USED TO SAVE THE OLD 0D ELEMENT, DO NOT USE                    
*       FOR OTHER PURPOSES THROUGH OUT DEMOPROC                                 
***********************************************************************         
DEMOPROC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    MISCFLG2,MF2NODEM   SKIP DEMO PROCESSING                         
         BO    DEMOPX                                                           
*                                                                               
         LR    RE,RA               CHECK TO SEE IF LOCAL SIGN ON                
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    DEMOP05                                                          
         DROP  RE                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    DEMOPX                                                           
*                                                                               
DEMOP05  DS    0H                                                               
*                                                                               
K        USING RBUYREC,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   K.RBUYKTYP,X'0B'      INSERT RECORD TYPE                         
         MVC   K.RBUYKREP,TWAAGY     INSERT REP CODE                            
         MVC   K.RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
DEMOP10  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DEMOP100                                                         
*                                                                               
         MVC   SAVEKEY,KEY         SAVE FOR RESTARTS                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
         MVI   WORK2,X'FF'                                                      
         MVC   WORK2+1(255),WORK2                                               
         L     R6,AIO2             AIO2->REP BUY                                
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMOP14                                                          
*                                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
*                                                                               
DEMOP12  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0D',AIO2),0,0                   
DEMOP14  DS    0H                                                               
         TM    MISCFLG2,MF2DEMCH+MF2USRDM                                       
         BNO   DEMOP15                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
*                                                                               
DEMOP15  DS    0H                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         CLC   RBUYKMLN,RBUYKLIN   IF RBUYKMLN <> RBUYKLIN                      
         BNE   DEMOP18             THEN THIS MUST BE A MAKE GOOD                
*                                                                               
         CLI   RBUYAGBL,0          ELSE CHECK TO SEE IF HAVE LINK?              
         BNE   DEMOP20             YES=NORMAL BUY, NO=MAKEGOOD                  
*                                                                               
DEMOP18  DS    0H                                                               
         TM    MISCFLG2,MF2USRDM   TARGE IS USER DEFINED DEMO                   
         BO    DEMOP77             DON'T COPY AGY DEMO OVER                     
         GOTOR MKGDMPR             PROCESS MAKEGOOD DEMO                        
         BE    DEMOP75             DEMO UPDATED                                 
         B     DEMOP80             DEMO NOT UPDATED, NOTHING CHANGED            
*                                                                               
DEMOP20  DS    0H                  AIO2->REP BUY, AIO1->AGY BUY                 
*                                                                               
* FIND MATCHING AGENCY SHADOW BUY                                               
*                                                                               
         TM    MISCFLG2,MF2USRDM   USER DEFINED DEMO?                           
         BO    DEMOP77             DON'T COPY AGY DEMO OVER                     
*                                                                               
         XC    KEY,KEY                                                          
K        USING RDARREC,KEY                                                      
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   K.RDARKRT,X'40'     GET THE CORRESPONDING AGY BUY                
         MVC   K.RDARKSEQ,RBUYAGBL                                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DEMOP80                                                          
*                                                                               
         MVC   AIO,AIO1            AIO1->AGY BUY                                
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
DEMOP70  DS    0H                                                               
         GOTOR SAVEDEMO            AGYDEMO =>> REP DEMO                         
*                                                                               
DEMOP75  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELEM,=C'ADD=CODE'             
*                                                                               
DEMOP77  DS    0H                                                               
         MVC   KEY(27),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
*                                                                               
DEMOP80  DS    0H                                                               
         MVC   KEY(27),SAVEKEY     RESTART                                      
         GOTO1 HIGH                                                             
*                                                                               
DEMOP90  DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     DEMOP10                                                          
*                                                                               
* PROCESS MAKEGOOD OFFERS                                                       
*                                                                               
DEMOP100 DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
MGK      USING RMKGREC,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   MGK.RMKGKTYP,X'11'                                               
         MVC   MGK.RMKGKREP,TWAAGY     INSERT REP CODE                          
         MVC   MGK.RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                       
         MVC   MGK.RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS              
         MVC   MGK.RMKGKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV            
         DROP  R2,MGK                                                           
*                                                                               
DEMOP105 DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DEMOP110 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DEMOPX                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN,RMKGKPLN   HEADER RECORD?                               
         BNZ   DEMOP130            YES, CHECK IF APPLIED OR CANCELLED           
         TM    RMKGSCST,RMKGSAPQ+RMKGSCNQ                                       
         BNZ   DEMOP115            IF SO, SKIP TO NEXT GROUP                    
         GOTO1 SEQ                                                              
         B     DEMOP110                                                         
         DROP  R6                                                               
*                                                                               
DEMOP115 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGREC,R6                                                       
         CLI   RMKGKGR2,C'Z'                                                    
         BL    DEMOP120                                                         
         MVI   RMKGKGR2,C'A'                                                    
         ZIC   RF,RMKGKGR1                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR1                                                      
         B     DEMOP105                                                         
*                                                                               
DEMOP120 DS    0H                                                               
         ZIC   RF,RMKGKGR2                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR2                                                      
         B     DEMOP105                                                         
*                                                                               
DEMOP130 DS    0H                                                               
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DEMOP140                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         GOTO1 PUTREC                                                           
*                                                                               
DEMOP140 DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     DEMOP110                                                         
*                                                                               
DEMOPX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS LINES THAT DO NOT HAVE AGENCY LINK NUMBER IN THE BUY RECORD           
* WHICH IS EITHER A MAKEGOOD, OR A BUY CREATED AFTER DARE ORDER IS              
* CONFIRMED.                                                                    
***********************************************************************         
MKGDMPR NTR1   BASE=*,LABEL=*                                                   
*                                  THIS IS EITHER A MKGOOD                      
*                                  OR A BUY CREATED BY REPS AFTER CF            
         TM    MISCFLG2,MF2DEMCH   IF DEMO CATEGORY CHANGE                      
         BZ    MKGDMNO                                                          
*                                                                               
         XC    ELEM,ELEM            RESET DEMO VALUE TO 0                       
D        USING RBUYDMEL,ELEM                                                    
         MVI   D.RBUYDMCD,RBUYDMCQ                                              
         LA    R2,2                                                             
         LA    R3,SVDEMCAT+2                                                    
         LA    R4,ELEM+2                                                        
         USING RBUYDMCV,R4                                                      
*                                                                               
MKGDM13  DS    0H                                                               
         OC    0(3,R3),0(R3)                                                    
         BZ    MKGDM15                                                          
*                                                                               
         MVC   RBUYDMCT,0(R3)                                                   
         XC    RBUYDMDM,RBUYDMDM                                                
         MVC   RBUYDM2M,=F'-1'                                                  
*                                                                               
         LA    R3,L'RBUYDMCT(R3)                                                
         LA    R4,L'RBUYDMCV(R4)                                                
         LA    R2,L'RBUYDMCV(R2)                                                
         B     MKGDM13                                                          
*                                                                               
MKGDM15  DS    0H                                                               
         CHI   R2,2                                                             
         BE    MKGDMNO                                                          
         STC   R2,D.RBUYDMLN                                                    
*                                                                               
MKGDMYES SR    RC,RC                                                            
MKGDMNO  LTR   RC,RC                                                            
MKGDMX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS CREDITS                                                               
***********************************************************************         
PROCREDT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 BLDGRID,DMCB,ASTRDATE,AIO2                                       
         GOTO1 BLDGRID,DMCB,RSTRDATE,AIOAREA                                    
         MVC   TARGETBY,SRTTKBUY                                                
*                                                                               
         LR    R4,R3                                                            
*                                                                               
PRCD50   DS    0H                                                               
         LA    R3,SORTDLQ(R3)      ONLY PROCESS CREDITS AGAINST                 
         USING SORTD,R3            DESIGNATED TARGET CREDIT BUY                 
         CLC   TARGETBY,SRTTKBUY                                                
         BNE   PRCD130                                                          
         TM    SRTFLAGS,SFCREDIT                                                
         BZ    PRCD130                                                          
*                                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
PRCD60   DS    0H                                                               
         XC    ELTBUILD,ELTBUILD                                                
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,ELTBUILD)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELTBUILD+9)                          
         MVI   ELTBUILD+8,C'-'                                                  
         GOTO1 PERVAL,DMCB,(17,ELTBUILD),WORK2                                  
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                INVALID FIELD 1 OR 2                         
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,RBUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
         LTR   R1,R1               IF 0 WEEK, JUST JUMP TO NEXT ORBIT           
         BZ    PRCD120                                                          
*                                                                               
PRCD70   DS    0H                  FIRST, CHECK IF                              
         CLC   RBUYDTNW,0(R2)      CONTRACT BUY NPW TOO LARGE.                  
         BNH   *+6                 CANNOT HAVE NEGATIVE SPOTS IN CELL           
         DC    H'0'                SHOULD NEVER HAPPEN!!                        
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    PRCD80                                                           
         LA    R2,1(R2)                                                         
PRCD80   BCT   R1,PRCD70                                                        
*                                                                               
* UPDATE GRID BY SUBTRACTING CELL NPW BY CONTRACT BUY NPW                       
*                                                                               
WKD      USING PERVALD,WORK2                                                    
PRCD90   ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,RBUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
PRCD100  DS    0H                  FIRST, CHECK IF                              
         ZIC   RE,0(R2)                                                         
         ZIC   RF,RBUYDTNW                                                      
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
*                                                                               
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    PRCD110                                                          
         LA    R2,1(R2)                                                         
PRCD110  BCT   R1,PRCD100                                                       
*                                                                               
PRCD120  DS    0H                                                               
         BAS   RE,NEXTEL           INCASE OF ORBITS                             
         BE    PRCD60                                                           
         DROP  R6                                                               
*                                                                               
PRCD130  DS    0H                                                               
         CLC   ABUYGRID,RBUYGRID                                                
         BE    PRCDNO                                                           
*                                                                               
* CHANGES FOUND. MAKE CHANGES TO TARGET CONTRACT CREDIT LINE                    
* REMOVE CREDITS (ADD SPOTS BACK) TO AGENCY BUY AND USE IT TO                   
* OVERWRITE THE EXISTING REP BUYLINE                                            
*                                                                               
PRCD140  DS    0H                                                               
         LR    R3,R4                                                            
         LA    R3,SORTDLQ(R3)      ONLY PROCESS CREDITS AGAINST                 
         USING SORTD,R3            DESIGNATED TARGET CREDIT BUY                 
         CLC   TARGETBY,SRTTKBUY                                                
         BNE   PRCDYES                                                          
         TM    SRTFLAGS,SFCREDIT                                                
         BZ    PRCDYES                                                          
*                                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
PRCD160  DS    0H                                                               
         XC    ELTBUILD,ELTBUILD                                                
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,ELTBUILD)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELTBUILD+9)                          
         MVI   ELTBUILD+8,C'-'                                                  
         GOTO1 PERVAL,DMCB,(17,ELTBUILD),WORK2                                  
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                INVALID FIELD 1 OR 2                         
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,RBUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
         LTR   R1,R1               IF 0 WEEK, JUST JUMP TO NEXT ORBIT           
         BZ    PRCD220                                                          
*                                                                               
PRCD200  DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         ZIC   RF,RBUYDTNW                                                      
         AR    RE,RF               REMOVE CREDIT BY ADDING SPOTS BACK           
         STC   RE,0(R2)                                                         
*                                                                               
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    PRCD210                                                          
         LA    R2,1(R2)                                                         
PRCD210  BCT   R1,PRCD200                                                       
*                                                                               
PRCD220  DS    0H                                                               
         BAS   RE,NEXTEL           INCASE OF ORBITS                             
         BE    PRCD160                                                          
         DROP  R6                                                               
*                                                                               
PRCDYES  SR    RC,RC               CHANGES FOUND                                
PRCDNO   LTR   RC,RC               NO CHANGES FOUND                             
PRCDX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADJUST AGENCY BUY FOR ANY PARTIAL CANCELS                                     
***********************************************************************         
PARTCAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 BLDGRID,DMCB,ASTRDATE,AIO2                                       
         GOTO1 BLDGRID,DMCB,RSTRDATE,AIOAREA                                    
*                                                                               
         LA    R3,53               MAX WEEKS                                    
         LA    R4,ABUYGRID                                                      
         LA    R6,RBUYGRID                                                      
*                                                                               
PCAN05   DS    0H                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYDTEL,ELTBUILD                                                
         MVI   ELM.RBUYDTCD,X'03'                                               
         MVI   ELM.RBUYDTLN,11                                                  
         MVI   ELM.RBUYDTIN,X'81'                                               
*                                                                               
PCAN10   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PCAN20                                                           
         CLI   0(R4),0                                                          
         BE    PCAN40                                                           
*                                                                               
PCAN20   DS    0H                                                               
         LA    R6,1(R6)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,PCAN10                                                        
         B     PCAN200                                                          
*                                                                               
PCAN40   DS    0H                  PARTIAL CANCEL FOUND                         
*                                  GET START DATE FOR PARTIAL CANCEL            
         GOTO1 DATCON,DMCB,(3,FLTSTART),(0,WORK)                                
         LA    RE,RBUYGRID                                                      
         LR    RF,R6                                                            
         SR    RF,RE                                                            
         MH    RF,=H'7'            GET NUMBER OF DAYS                           
         ST    RF,DMCB+8           INSERT DAY ADJUSTMENT TO P3                  
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,ELM.RBUYDTST)                          
*                                                                               
* CALCULATE CORRECT START DATE                                                  
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK                                          
         ZIC   RE,STARTDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    PCAN45                                                           
         BP    PCAN43                                                           
         A     RE,=F'7'                                                         
*                                                                               
PCAN43   DS    0H                                                               
         ST    RE,DMCB+8           INSERT DAY ADJUSTMENT TO P3                  
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELM.RBUYDTST)                         
*                                                                               
PCAN45   DS    0H                                                               
         ZIC   RF,ELM.RBUYDTWK                                                  
         LA    RF,1(RF)                                                         
         STC   RF,ELM.RBUYDTWK     FIND NUMBER OF WEEKS                         
         LA    R6,1(R6)            FOR THIS PARTIAL CANCEL                      
         LA    R4,1(R4)                                                         
         BCTR  R3,0                                                             
         CLI   0(R6),0                                                          
         BE    PCAN50                                                           
         CLI   0(R4),0                                                          
         BNE   PCAN50                                                           
         LTR   R3,R3                                                            
         BNZ   PCAN45                                                           
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
PCAN50   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,ELM.RBUYDTST),(0,WORK)                            
         ZIC   R2,ELM.RBUYDTWK                                                  
         MH    R2,=H'7'                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
*                                                                               
         SR    R2,R2                                                            
         GOTO1 GETDAY,DMCB,WORK+6,WORK                                          
         ZIC   RE,ENDDAY                                                        
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    PCAN70                                                           
         BP    PCAN60                                                           
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     PCAN80                                                           
*                                                                               
PCAN60   DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     PCAN80                                                           
*                                                                               
PCAN70   DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
PCAN80   DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(R2)                                   
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELM.RBUYDTED)                         
*                                                                               
* DON'T ADD PARTIAL CANCEL ELEMENT IF AGENCY CHOPPED THE START DATE             
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         CLC   ELM.RBUYDTST,WORK                                                
         BL    PCAN05                                                           
*                                                                               
PCAN100  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
         LTR   R3,R3                                                            
         BNZ   PCAN05                                                           
*                                                                               
* PRESERVE ANY PARTIAL CANCEL WEEKS IN THE CURRENT REP BUY                      
*                                                                               
PCAN200  DS    0H                                                               
         L     R6,AIOAREA          POINT TO REP BUY                             
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
PCAN210  DS    0H                                                               
         CLI   RBUYDTNW,0          FIND ANY PARTIAL CANCELLED WEEKS             
         BE    PCAN220                                                          
         BAS   RE,NEXTEL                                                        
         BE    PCAN210                                                          
         B     PCANX                                                            
*                                                                               
PCAN220  DS    0H                                                               
         L     R4,AIO2             NOW POINT TO THE AGENCY'S CONVERTED          
         MVI   ELCODE,X'03'        BUY                                          
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
AGYBUY   USING RBUYDTEL,R4                                                      
*                                                                               
PCAN230  DS    0H                  CHECK IF ANY SPOT FOR THIS WEEK              
         CLC   RBUYDTED,AGYBUY.RBUYDTST                                         
         BL    PCAN240                                                          
         CLC   RBUYDTST,AGYBUY.RBUYDTED                                         
         BNH   PCAN250                                                          
*                                                                               
         BAS   RE,NEXTEL2                                                       
         BE    PCAN230                                                          
*                                                                               
* ADD PARTIAL CANCELLED WEEK TO THE AGENCY CONVERTED BUY                        
*                                                                               
PCAN240  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,0(R6),0                       
*                                                                               
PCAN250  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    PCAN210                                                          
*                                                                               
PCANX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6,AGYBUY                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR NOW, IF CONTRACT HAS ANY CREDITS, MUST PROCESS MANUALLY                   
***********************************************************************         
CHKCRED  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         L     RE,AIO2                                                          
         MVC   RBUYKEY(22),0(RE)                                                
         GOTO1 HIGH                                                             
*                                                                               
CKCR10   CLC   KEY(22),KEYSAVE                                                  
         BNE   CKCRX                                                            
         CLC   RBUYKMLN,RBUYKLIN                                                
         BNE   CKCR30                                                           
CKCR20   GOTO1 SEQ                                                              
         B     CKCR10                                                           
         DROP  R4                                                               
*                                                                               
CKCR30   DS    0H                                                               
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKCR20                                                           
         USING RBUYCMEL,R6                                                      
         CLC   =C'CR=',RBUYCMNT                                                 
         BE    MUSTMAN                                                          
         B     CKCR20                                                           
         DROP  R6                                                               
*                                                                               
CKCRX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIND THE ADDITIONAL WEIGHT THAT THE AGENCY HAS PUT ON A BUY, AS               
* COMPARED TO THE CONTRACT BUY                                                  
* SEE ROUTINE UPDTGRID AS TO HOW THE RBUYGRID IS CONSTRUCTED                    
***********************************************************************         
ADWEIGHT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 BLDGRID,DMCB,ASTRDATE,AIO2                                       
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
ADW60    DS    0H                                                               
         CLI   RBUYDTWK,0          SKIP TO NEXT ORBIT IF ZERO WEEKS             
         BE    ADW120              OR ZERO SPOTS                                
         CLI   RBUYDTNW,0                                                       
         BE    ADW120                                                           
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(5,ELTBUILD)                            
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELTBUILD+9)                          
         MVI   ELTBUILD+8,C'-'                                                  
         GOTO1 PERVAL,DMCB,(17,ELTBUILD),WORK2                                  
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,ABUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
         LTR   R1,R1               IF 0 WEEK, JUST JUMP TO NEXT ORBIT           
         BZ    ADW120                                                           
*                                                                               
ADW70    DS    0H                  FIRST, CHECK IF                              
         CLC   RBUYDTNW,0(R2)      CONTRACT BUY NPW TOO LARGE.                  
         BH    ADWNO               CANNOT HAVE NEGATIVE SPOTS IN CELL           
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    ADW80                                                            
         LA    R2,1(R2)                                                         
ADW80    BCT   R1,ADW70                                                         
*                                                                               
* UPDATE GRID BY SUBTRACTING CELL NPW BY CONTRACT BUY NPW                       
*                                                                               
WKD      USING PERVALD,WORK2                                                    
ADW90    ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,ABUYGRID                                                      
         AR    R2,R1                                                            
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
ADW100   DS    0H                  FIRST, CHECK IF                              
         ZIC   RE,0(R2)                                                         
         ZIC   RF,RBUYDTNW                                                      
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
*                                                                               
         LA    R2,1(R2)                                                         
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK??                           
         BZ    ADW110                                                           
         LA    R2,1(R2)                                                         
ADW110   BCT   R1,ADW100                                                        
*                                                                               
ADW120   DS    0H                                                               
         BAS   RE,NEXTEL           INCASE OF ORBITS                             
         BE    ADW60                                                            
         DROP  R6                                                               
*                                                                               
* FROM WHAT'S LEFT ON ABUYGRID, REBUILD THE X'03' ELEMENT TO GET THE            
* DIFFERENCE                                                                    
*                                                                               
         OC    ABUYGRID,ABUYGRID                                                
         BZ    ADWNO               NO DATES LEFT, MUST BE DAY/TIME CHG          
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',AIO2),0,0                   
*                                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
*                                                                               
         XC    RBUYTSPT,RBUYTSPT                                                
         XC    RBUYTCOS,RBUYTCOS                                                
         XC    RBUYTWKS,RBUYTWKS                                                
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYDTEL,ELTBUILD                                                
         MVI   ELM.RBUYDTCD,X'03'                                               
         MVI   ELM.RBUYDTLN,11                                                  
         MVI   ELM.RBUYDTIN,X'80'                                               
*                                                                               
         LA    R2,ABUYGRID                                                      
         SR    R3,R3               OVERALL # WEEKS LENGTH (MAX 53)              
*                                                                               
ADW125   DS    0H                                                               
         SR    R4,R4               # WEEKS FOR THIS ORBIT                       
*                                                                               
ADW130   DS    0H                                                               
         CLI   0(R2),0             BREAK??                                      
         BNE   ADW132                                                           
         LTR   R4,R4               ANY WEEKS FOR THIS ORBIT??                   
         BNZ   ADW145                                                           
         B     ADW135                                                           
*                                                                               
ADW132   DS    0H                                                               
         LA    R4,1(R4)                                                         
         OC    ELM.RBUYDTST,ELM.RBUYDTST                                        
         BNZ   ADW133                                                           
         GOTO1 DATCON,DMCB,(3,FLTSTART),(0,WORK)                                
         LR    RF,R3                                                            
         MH    RF,=H'7'            GET NUMBER OF DAYS                           
         ST    RF,DMCB+8           INSERT DAY ADJUSTMENT TO P3                  
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,ELM.RBUYDTST)                          
*                                                                               
* CALCULATE CORRECT START DATE                                                  
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK                                          
         ZIC   RE,STARTDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    ADW133                                                           
         BP    ADW132A                                                          
         A     RE,=F'7'                                                         
*                                                                               
ADW132A  DS    0H                                                               
         ST    RE,DMCB+8           INSERT DAY ADJUSTMENT TO P3                  
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELM.RBUYDTST)                         
*                                                                               
ADW133   DS    0H                                                               
         OC    ELM.RBUYDTNW,ELM.RBUYDTNW                                        
         BNZ   *+10                                                             
         MVC   ELM.RBUYDTNW,0(R2)                                               
         CLC   ELM.RBUYDTNW,0(R2)                                               
         BNE   ADW140                                                           
         TM    RBUYFLG2,X'80'      DAILY?                                       
         BO    ADW140              YES, BREAK                                   
*                                                                               
ADW135   DS    0H                                                               
         LA    R3,1(R3)                                                         
         CH    R3,=H'53'           MAX 53 WEEKS TO CHECK                        
         BH    ADW140                                                           
         LA    R2,1(R2)                                                         
         B     ADW130                                                           
*                                                                               
ADW140   DS    0H                  BREAK IN TO ORBIT                            
         LTR   R4,R4                                                            
         BZ    ADWYES                                                           
         BCTR  R4,0                                                             
*                                                                               
ADW145   DS    0H                                                               
         STC   R4,ELM.RBUYDTWK                                                  
         CLC   RBUYNW,ELM.RBUYDTNW                                              
         BE    *+8                                                              
         OI    ELM.RBUYDTIN,X'01'  NPW OVERRIDE                                 
*                                                                               
         TM    RBUYFLG2,X'80'      DAILY?                                       
         BZ    ADW150                                                           
         MVC   ELM.RBUYDTED,ELM.RBUYDTST                                        
         MVI   ELM.RBUYDTWK,1                                                   
         B     ADW200              YES, END DATE = START DATE                   
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
ADW150   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,ELM.RBUYDTST),(0,WORK)                            
         ZIC   R4,ELM.RBUYDTWK                                                  
         MH    R4,=H'7'                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R4)                                      
*                                                                               
         SR    R4,R4                                                            
         GOTO1 GETDAY,DMCB,WORK+6,WORK                                          
         ZIC   RE,ENDDAY                                                        
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    ADW160                                                           
         BP    ADW155                                                           
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R4,RE                                                            
         B     ADW170                                                           
*                                                                               
ADW155   DS    0H                                                               
         LA    R4,7                                                             
         LNR   R4,R4                                                            
         AR    R4,RE                                                            
         B     ADW170                                                           
*                                                                               
ADW160   DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R4,7                                                             
         LNR   R4,R4                                                            
*                                                                               
ADW170   DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(R4)                                   
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELM.RBUYDTED)                         
*                                                                               
ADW200   DS    0H                  ADD ELEMENT TO BUY                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
* CALCULATE TOTAL SPOTS                                                         
*                                                                               
         ZIC   RE,ELM.RBUYDTNW                                                  
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),ELM.RBUYDTWK                                           
         MH    RE,HALF                                                          
         LR    R1,RE               SAVE OFF TOTAL SPOTS                         
         ZICM  RF,RBUYTSPT,2                                                    
         AR    RE,RF                                                            
         STCM  RE,3,RBUYTSPT       ACCUMULATE TOTAL SPOTS                       
*                                                                               
* CALCULATE TOTAL COST                                                          
*                                                                               
         MVC   FULL,RBUYCOS                                                     
         SR    R0,R0                                                            
         M     R0,FULL                                                          
         ICM   RF,15,RBUYTCOS                                                   
         AR    R1,RF                                                            
         STCM  R1,15,RBUYTCOS      ACCUMULATE TOTAL COST                        
*                                                                               
* CALCULATE TOTAL WEEKS                                                         
*                                                                               
         ZIC   RE,ELM.RBUYDTWK                                                  
         ZIC   RF,RBUYTWKS                                                      
         AR    RE,RF                                                            
         STC   RE,RBUYTWKS         ACCUMULATE TOTAL WEEKS                       
*                                                                               
         TM    RBUYFLG2,X'80'      DAILY? WE ARE DONE                           
         BO    ADWYES                                                           
*                                                                               
         CH    R3,=H'53'           MAX 53 WEEKS TO CHECK                        
         BH    ADWYES                                                           
*                                                                               
         XC    ELTBUILD,ELTBUILD   INIT FOR NEXT DATES, IF ANY                  
         MVI   ELM.RBUYDTCD,X'03'                                               
         MVI   ELM.RBUYDTLN,11                                                  
         MVI   ELM.RBUYDTIN,X'80'                                               
         B     ADW125                                                           
*                                                                               
ADWYES   SR    RC,RC                                                            
ADWNO    LTR   RC,RC                                                            
ADWX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6,ELM                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO2 HAS TRANSLATED AGENCY BUY TO ADD                                          
***********************************************************************         
ADDAGBUY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BUYNUM,0                                                         
         MVC   KEY(RBUYKPLN-RBUYKEY),AGYBUY.RBUYKEY                             
         OI    DMINBTS,X'08'       RETURN DELETED KEYS                          
         GOTO1 HIGH                                                             
ADDB10   CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   ADDB20                                                           
         CLI   KEY+26,255          PLANREC?                                     
         BE    ADDB15                                                           
         CLC   BUYNUM,KEY+26                                                    
         BNL   *+10                                                             
         MVC   BUYNUM,KEY+26       HIGHEST LINE NUMBER SO FAR                   
ADDB15   OI    DMINBTS,X'08'       RETURN DELETED KEYS                          
         GOTO1 SEQ                                                              
         B     ADDB10                                                           
*                                                                               
ADDB20   DS    0H                  ASSIGN A NEW BUYLINE #                       
         CLI   BUYNUM,X'FF'                                                     
         BE    MAXBUYER                                                         
         ZIC   RF,BUYNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,AGYBUY.RBUYKMLN                                               
         STC   RF,AGYBUY.RBUYKLIN                                               
*                                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    ADDB30                                                           
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    ADDB30                                                           
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDB30   DS    0H                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
         MVC   RBUYVER,RCONSRV     STORE REP VERSION NO. IN BUY                 
         DROP  R4,R6                                                            
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R4                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    ADDB40                                                           
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
ADDB40   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R4                                                               
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYOCEL,ELTBUILD                                                
         MVI   ELM.RBUYOCCD,X'84'                                               
         MVI   ELM.RBUYOCLN,23                                                  
         MVI   ELM.RBUYOCID,X'80'                                               
         MVC   ELM.RBUYOCNT(20),=C'ADDITION TO SCHEDULE'                        
         DROP  ELM                                                              
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIO2),0,0                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 ADDREC                                                           
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  ADD  NEW BUCKETS                             
*                                                                               
* SAVE ADDED BUY INFO SO WE CAN SHOW IT TO USER LATER                           
*                                                                               
         LA    R3,SORTAREA         ADD BUY ADDED TO A LIST                      
         USING SORTD,R3                                                         
ADDB50   CLI   SRTAGYBY,0                                                       
         BE    ADDB60                                                           
         LA    R3,SORTDLQ(R3)                                                   
         B     ADDB50                                                           
*                                                                               
ADDB60   DS    0H                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
         MVC   SRTAGYBY,RBUYAGBL                                                
         MVC   SRTCONBY,RBUYKLIN                                                
         OI    SRTFLAGS,SFADDED+SFMATCHD                                        
         DROP  R3,R6                                                            
*                                                                               
ADDBX    DS    0H                                                               
         B     EXIT                                                             
         DROP  AGYBUY                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRESERVE MAKEGOOD/CREDIT ELEMENTS: X'05', X'07', X'56', X'66', X'76'          
* ALSO PRESERVE CREDIT HISTORY: X'16'                                           
* COPY THE ELEMENTS TO THE AGENCY TRANSLATED BUY THAT WILL REPLACE              
* THE CURRENT CONTRACT BUY                                                      
* R3 POINTS TO CURRENT MAKEGOOD BUY RECORD                                      
***********************************************************************         
MKGDCRDT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SORTD,R3                                                         
         LA    R4,ELEMLIST                                                      
*                                                                               
MC10     DS    0H                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
         MVC   ELCODE,0(R4)                                                     
         BAS   RE,GETEL                                                         
         BNE   MC30                                                             
*                                                                               
MC20     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,(R6),0                        
         BAS   RE,NEXTEL                                                        
         BE    MC20                                                             
*                                                                               
MC30     DS    0H                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   MC10                                                             
*                                                                               
* ONLY FOR MAKEGOOD/CREDIT BUY, PRESERVE X'04' BUY COMMENT MG= OR CR=           
* LINK. MOVE PROGRAM NAME TO SECOND LINE                                        
*                                                                               
MC40     DS    0H                                                               
         TM    SRTFLAGS,SFMKGOOD+SFCREDIT                                       
         BZ    MCX                                                              
*                                                                               
         L     R6,AIOAREA                                                       
         USING RBUYREC,R6                                                       
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   MCX                                                              
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BE    MC50                                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,(R6),0                        
         B     MCX                                                              
*                                                                               
MC50     DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,AIO2),(R6),(R4),0                                 
*                                                                               
MCX      DS    0H                                                               
         B     EXIT                                                             
         DROP  AGYBUY,R3,R6                                                     
*                                                                               
ELEMLIST DS    0XL5                                                             
         DC    X'05'               BUY MAKE-GOOD REFERENCE ELEMENT              
         DC    X'07'               CREDIT ELEMENT FOR MISSED SPOT               
         DC    X'0D'               CREDIT ELEMENT FOR MISSED SPOT               
         DC    X'0E'               CREDIT ELEMENT FOR MISSED SPOT               
         DC    X'16'               CREDIT ELEMENT HISTORY                       
         DC    X'56'               MG SPLITOUT BUY MISSED ELEMENT               
         DC    X'66'               MAKEGOOD BUY MISSED ELEMENT(OFFER)           
         DC    X'76'               MAKEGOOD BUY MISSED ELEMENT(APPLIED)         
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* AFTER COMPLETION OF AUTOMATIC CHANGES, ANY CONTRACT BUYS THAT WERE            
* NOT MATCHED/PROCESSED WILL BE MARKED CANCELLED FOR THIS APPROVAL              
* NOTE:                                                                         
* IF REP BUY TO BE CANCELLED IS A TARGET FOR MAKEGOOD OR CREDIT, THE            
* ATTACHED MAKEGOOD AND CREDIT BUYS WILL BE CANCELLED AS WELL!!                 
*                                                                               
* IF CONTRACT PROFILE 36 = Y, THE CONTRACT BUY WILL BE ZEROED OUT               
* INSTEAD OF CANCELLED. ANY MAKEGOODS/CREDITS FOR THIS LINE WILL STILL          
* BE CANCELLED.                                                                 
*********************************************************************           
CANKBUY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SORTAREA(SORTDLQ),SORTAREA                                       
         BZ    CANKBX              CONTRACT BUYS ALL CANCELLED, EXIT            
*                                                                               
         LA    R3,SORTAREA         CHECK IF ALL CONTRACT BUYS WERE              
         USING SORTD,R3            MATCHED UP WITH ALL AGENCY BUYS              
CANKB10  TM    SRTFLAGS,SFMATCHD                                                
         BZ    CANKB15             CONTRACT BUY NOT MATCHED                     
*                                                                               
* NOTE: THE FOLLOWING SHOULD NEVER HAPPEN. WE'LL FORCE A DUMP FOR NOW           
* TO SEE WHAT THE AGENCY HAS SENT TO CREAT THIS SCENARIO                        
* (SKUI 1/19/2001)                                                              
*                                                                               
* THE SITUATION IS FOR MAKEGOOD OR CREDIT BUYS. IF THE TARGET BUY IS            
* NOT MATCHED WHILE THE CREDIT/MAKEGOOD BUY DID, WE HAVE A PROBLEM.             
*                                                                               
         TM    SRTFLAGS,SFMKGOOD+SFCREDIT                                       
         BZ    CANKB100                                                         
*                                                                               
         LA    R2,SORTAREA                                                      
CAND     USING SORTD,R2                                                         
CANKB12  CLC   CAND.SRTCONBY,SRTTKBUY                                           
         BE    CANKB13             FIND TARGET BUY                              
         LA    R2,SORTDLQ(R2)                                                   
         CLI   CAND.SRTAGYBY,0                                                  
         BNE   CANKB12                                                          
         B     CANKB100            SHOULDN'T HAPPEN, BUT SKIP FOR NOW           
*                                                                               
CANKB13  DS    0H                                                               
         TM    CAND.SRTFLAGS,SFMATCHD                                           
         BO    CANKB100            SKIP IF TARGET BUY WAS MATCHED,              
         TM    CAND.SRTFLG2,SF20SPTS                                            
         BO    CANKB100            SKIP IF TARGET BUY HAS NO SPOTS              
*                                                                               
***                                                                             
* IF TARGET MAKEGOOD HAS NO SPOTS, THEN LEAVE IT IN THE CONTRACT EVEN           
* THOUGH IT WAS NOT MATCHED. OTHERWISE, CONTINUE TO NOTIFY USER TO              
* CONTACT DDS                                                                   
***                                                                             
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(ERRAPRVQ),ERRAPRV                                        
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
         NI    SRTFLAGS,X'FF'-SFMATCHD                                          
*                                  TURN OFF MATCHED FLAG, INCASE OF             
*                                  MAKEGOOD FOR MAKEGOOD                        
CANKB15  DS    0H                                                               
         MVC   KEY+28(4),SRTBUYDA                                               
         L     R6,AIOAREA                                                       
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYOCEL,ELTBUILD                                                
         MVI   ELM.RBUYOCCD,X'84'                                               
         MVI   ELM.RBUYOCLN,20                                                  
         MVI   ELM.RBUYOCID,X'80'                                               
         MVC   ELM.RBUYOCNT(17),=C'BUYLINE CANCELLED'                           
         DROP  ELM                                                              
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIOAREA),0,0                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIOAREA,ELTBUILD,0                 
*                                                                               
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDRCNB,CNTDRCNA                                       
         BO    CANKB18             ON = ZERO OUT BUY                            
         DROP  R1                  OFF = CANCEL BUY                             
*                                                                               
         USING RBUYREC,R6                                                       
*                                                                               
         MVI   RBUYCNTL,X'80'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RBUYCHGD)  UPDATE LAST CHANGE DATE          
         MVC   RBUYCHGI,=C'C '     CANCEL INDICATOR                             
*******                                                                         
*        MVI   ELCODE,X'56'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   *+6                 DIE IF MAKEGOOD ATTACHED                     
*        DC    H'0'                                                             
*******                                                                         
CANKB18  DS    0H                                                               
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         MVC   RBUYKMOD,RCONMOD                                                 
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    CANKB20                                                          
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    CANKB20                                                          
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
         L     R4,AIO3                                                          
         USING RCONSEND,R4                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CANKB20  DS    0H                                                               
         MVC   RBUYVER,RCONSRV     STORE REP VERSION NO. IN BUY                 
         DROP  R4,R6                                                            
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R4                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R4                                                               
*                                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    CANKB30                                                          
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
CANKB30  OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIOAREA         WRITE OUT CANCELLED CONTRACT BUY REC         
*                                                                               
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),RR=RELO                              
*                                  BACK OUT BUCKETS                             
         NI    BUCKFLGS,X'FF'-X'10' RESET                                       
*                                                                               
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDRCNB,CNTDRCNA                                       
         BO    CANKB35             ON = ZERO OUT BUY                            
         DROP  R1                  OFF = CANCEL BUY                             
*                                  OR ZERO OUT BUY INSTEAD                      
         GOTO1 PUTREC                                                           
         L     R6,AIOAREA                                                       
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CANKB100                                                         
         MVI   KEY+27,X'80'        DELETE KEY AS WELL                           
         GOTO1 WRITE                                                            
         B     CANKB100                                                         
*                                                                               
CANKB35  DS    0H                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         OI    RBUYRTS,X'01'       FLAG BUY WAS ZERO OUT VIA PROFILE            
         MVC   RBUYCHGI,=C'S '     SPOTS PER WEEK CHANGED                       
         XC    RBUYTSPT,RBUYTSPT   0 TOTAL SPOTS                                
         XC    RBUYTCOS,RBUYTCOS   0 TOTAL COSTS                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
CANKB40  DS    0H                                                               
         USING RBUYDTEL,R6                                                      
         OI    RBUYDTIN,X'01'      NPW OVERRIDE                                 
         MVI   RBUYDTNW,0          0 OUT DATES                                  
         BAS   RE,NEXTEL                                                        
         BE    CANKB40                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
CANKB100 DS    0H                                                               
         LA    R3,SORTDLQ(R3)                                                   
         CLI   SRTAGYBY,0                                                       
         BNE   CANKB10                                                          
*                                                                               
CANKBX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ERRAPRV  DC    C'Error: Makegood missed buy was not matched. Call DDS.'         
ERRAPRVQ EQU   *-ERRAPRV                                                        
***********************************************************************         
*   BUYDETL:  BUILDS BUY EFFECTIVE DATE ELEMENTS, THEN INSERTS                  
*        THEM INTO THE NEW BUY RECORD.                                          
***********************************************************************         
         DS    0D                                                               
BUYDETL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         CLC   RTKODATE,WORK                                                    
         BNH   BDET0010                                                         
*** TESTING                                                                     
*        GOTO1 DATCON,DMCB,(0,=C'970428'),(3,RTKODATE)                          
*                                                                               
         ST    R3,DMCB             SETUP CHOPPING CALL                          
         MVC   DMCB+4(3),RTKODATE                                               
         MVC   DMCB+8(1),STARTDAY                                               
         MVC   DMCB+9(1),ENDDAY                                                 
         MVC   DMCB+10(1),ROTATDAY                                              
         MVC   DMCB+11(1),ORBSTDAY                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),DATCON                                                   
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),PERVERT                                               
         LA    RE,WORK                                                          
         ST    RE,DMCB+12                                                       
*                                                                               
         GOTO1 VREDARTK,DMCB                                                    
         BNE   BDETNO              BUY DELETED, EXIT                            
*                                                                               
BDET0010 EQU   *                                                                
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         DROP  R3                                                               
         USING RDARBDEL,R8         BUY DETAIL DESCRIP ELEMENT                   
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         USING RBUYREC,R2                                                       
*                                                                               
         NI    MISCFLAG,X'7F'      TURN OFF 'DAILY' BUY FLAG                    
         TM    RDARBDFL,X'80'      'DAILY' BUY?                                 
         BNO   BDET0020            NO                                           
         OI    MISCFLAG,X'80'      SET FLAG FOR 'DAILY' BUY                     
BDET0020 EQU   *                                                                
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0030            NO                                           
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BNE   BDET0040            NO                                           
         GOTO1 DELELT02,DMCB,RBUYREC                                            
*                                  CLEAR ORBIT X'02' DAY/TIME ELTS              
         MVI   ORBFLAG,C'N'        SET ORBIT FLAG TO NO                         
         OI    MISCFLAG,X'40'      SET 'ORBIT DROPPED' FLAG                     
         B     BDET0040                                                         
BDET0030 EQU   *                                                                
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BE    BDET0060            YES - DON'T NEED SECONDARY -                 
*                                  ORBIT HAS ALREADY ADDED THE X'02'            
*                                     ELT.  SHOULD NEVER ABE ORB W/             
*                                        'DAILY' RECORDS                        
BDET0040 EQU   *                                                                
         GOTO1 LOADELT,DMCB,(R2),ELTBILD2,=C'ADD=CODE'                          
         XC    ELTBILD2,ELTBILD2                                                
BDET0060 EQU   *                                                                
*                                  MOVE FROM DESCRIP ELT TO DETAIL              
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST DETAIL ELEMENT                   
BDET0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BDETYES             YES - FINISHED                               
*                                                                               
         USING RDARBUEL,R8                                                      
*                                                                               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    BDET0100            YES - SKIP CHECK FOR COST BREAK ->           
*                                     EACH ELEMENT BECOMES A BUY REC            
         GOTO1 DETLBRAK,DMCB,(RC),(R8),(R2)                                     
*                                  CHECK FOR COST BREAK                         
BDET0100 EQU   *                                                                
         MVI   DETLFLAG,C'Y'       SET DETAILS TO YES                           
         OC    RBUYNW,RBUYNW       NUMBER/WEEK FILLED IN?                       
         BNZ   BDET0120            YES                                          
*                                                                               
         MVC   RBUYNW,RDARBUSW     NO  - INSERT NUMBER PER WEEK                 
BDET0120 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBUILD(2),=X'030B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0130            NO  -                                        
         MVC   WORK+12(6),WORK     YES - USE START AS IS, THEN                  
*                                     SET END TO START                          
         B     BDET0190                                                         
BDET0130 EQU   *                                                                
         ZIC   RF,RDARBUWK         CALCULATE NUMBER OF DAYS                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         BCTR  RF,0                MAKE WEEKS ZERO RELATIVE                     
         MH    RF,=H'7'            MULTIPLY WEEKS BY 7                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF START WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0140            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0135                                                         
         MVC   WORK(6),WORK+12                                                  
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                                                               
BDET0135 DS    0H                                                               
         MVC   WORK(6),WORK+12                                                  
*                                                                               
BDET0140 EQU   *                                                                
         CLI   ORBSTDAY,0          ANY ORBIT START DAY?                         
         BZ    BDET0160            NO  - USE HEADER START DAY                   
         ZIC   RF,ORBSTDAY         YES - USE IT                                 
         B     BDET0180                                                         
BDET0160 EQU   *                                                                
         ZIC   RF,STARTDAY         ADD START DAY                                
BDET0180 EQU   *                                                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BUMP TO START DAY IN WEEK                    
BDET0190 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+2)                           
*                                  INSERT START DATE INTO ELEMENT               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0200            NO                                           
         MVC   ELTBUILD+5(3),ELTBUILD+2                                         
*                                  YES - SET END DATE = START DATE              
         B     BDET0260                                                         
BDET0200 EQU   *                                                                
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
*                                  GET DAY OF END   WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0240            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         CLC   STARTDAY,ENDDAY     START/END ON SAME DAY?                       
*                                     (SINGLE-DAY BUY?)                         
         BE    BDET0220            YES - DON'T BUMP TO NEXT WEEK                
         BL    BDET0220            START < END DAY:  NOT AN                     
*                                     OOWR - DON'T BUMP                         
         LA    RF,7(RF)            BUMP IT INTO NEXT WEEK                       
BDET0220 EQU   *                                                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0230                                                         
         MVC   WORK+6(6),WORK+12                                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                                                               
BDET0230 DS    0H                                                               
         MVC   WORK+6(6),WORK+12                                                
*                                                                               
BDET0240 EQU   *                                                                
         ZIC   RF,ENDDAY           ADD END   DAY                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BUMP TO END   DAY IN WEEK                    
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+5)                           
*                                  INSERT END   DATE INTO ELEMENT               
*                                                                               
BDET0260 EQU   *                                                                
         MVI   ELTBUILD+8,X'80'    SET TO 'EVERY WEEK'                          
         CLC   RBUYNW,RDARBUSW     HEADER #/WK = DETAIL #/WK?                   
         BE    BDET0280            YES                                          
         OI    ELTBUILD+8,X'01'    NO  - PUT IN 'OVERRIDE' FLAG                 
         B     BDET0290                                                         
BDET0280 EQU   *                                                                
         CLI   RDARBUSW,0          SPOT PER WEEK ZERO??                         
         BNE   BDET0290            RBUYNW MIGHT GET SET TO SOMETHING            
         OI    ELTBUILD+8,X'01'    ELSE LATER, SET OVERRIDE ANYWAY              
*                                                                               
BDET0290 EQU   *                                                                
         MVC   ELTBUILD+9(1),RDARBUSW                                           
*                                  INSERT NUMBER PER WEEK                       
         MVC   ELTBUILD+10(1),RDARBUWK                                          
*                                  INSERT NUMBER OF WEEKS                       
         ZIC   RF,RDARBUWK         ACCUMULATE NUMBER OF WEEKS                   
         SR    R0,R0                  AND CALC # SPOTS                          
         ZIC   R1,RDARBUSW         NUMBER SPOTS PER WEEK                        
         MR    R0,RF               # SPOTS X # WEEKS                            
         L     RE,SPOTCTR                                                       
         AR    RE,R1               ACCUMULATE NUMBER OF SPOTS                   
         ST    RE,SPOTCTR          STORE IT BACK                                
         A     RF,WEEKCTR          ACCUMULATE NUMBER OF WEEKS                   
         ST    RF,WEEKCTR          STORE IT BACK                                
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0300            NO  - GO BACK FOR NEXT ELEMENT               
         BAS   RE,GENDAILY         YES - GENERATE A BUY RECORD                  
BDET0300 EQU   *                                                                
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BDET0080            GO BACK FOR NEXT ELEMENT                     
BDETYES  SR    RC,RC                                                            
BDETNO   LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   GENDAILY:  FOR 'DAILY' BUYS, EACH DETAIL IS TO BE A SEPARATE BUY            
*        RECORD.  THE DETAIL'S DETAILS ARE INSERTED INTO THE HEADER,            
*        AND THE RECORD WRITTEN TO THE FILE.                                    
*                                                                               
GENDAILY NTR1                                                                   
         OI    RBUYFLG2,X'80'      SET 'DAILY ORDER' FLAG                       
         MVC   RBUYNW,RDARBUSW     INSERT NUMBER SPOTS/WEEK                     
         MVC   RBUYTSPT,RDARBUSW   INSERT TOTAL SPOT: SAME AS SPTS/WK           
         MVC   RBUYCOS,RDARBU$$    INSERT SPOT COST                             
         MVC   BUYCOST,RDARBU$$    SAVE   SPOT COST FOR CALCULATION             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF WEEK OF START DAY                 
         ZIC   RF,DMCB             GET DAY OF WEEK:  START DAY                  
         SLL   RF,4                SHIFT TO LOW ORDER, HI NYBBLE                
         ZIC   RE,DMCB             GET DAY OF WEEK:  END   DAY                  
         AR    RF,RE               ADD END DAY TO START DAY                     
         STC   RF,RBUYSTED         INSERT START/END DAYS                        
         LA    RF,RBUYELEM         SET A(01 ELEMENT)                            
GDAI0020 EQU   *                                                                
         ZIC   RE,1(RF)            GET LENGTH                                   
         AR    RF,RE               BUMP TO NEXT                                 
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(RF),2             DAY/TIME ELEMENT?                            
         BNE   GDAI0020            NO  - GO BACK FOR NEXT                       
         MVC   RBUYDYIN-RBUYDYEL(1,RF),RBUYSTED                                 
*                                  YES - INSERT START/END FROM HDR              
         ZIC   RE,RBUYSTED         GET ST/END DAYS                              
         SRL   RE,4                DROP THE END DAY                             
         LA    R3,DAYTABLE                                                      
         AR    R3,RE               ADD DAY TO TABLE ADDR                        
         MVC   RBUYDAYS-RBUYDYEL(1,RF),0(R3)                                    
*                                  INSERT DAYS INTO ELEMENT -                   
*                                     ALWAYS A SINGLE DAY                       
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
* GO AND MATCH THE MANUALLY UPDATED CONTRACT BUYS                               
*                                                                               
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BZ    GDAI0030                                                         
         GOTO1 =A(MANMATCH),RR=RELO                                             
         B     GDAI0040                                                         
*                                                                               
GDAI0030 EQU   *                                                                
         GOTO1 =A(AUTOGEN),RR=RELO                                              
*                                  OUTPUT THE BUY RECORD                        
GDAI0040 EQU   *                                                                
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
         STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
         B     EXIT                                                             
*                                                                               
DAYTABLE DC    X'8040201008040201'                                              
*                                                                               
         DROP  R2,R8                                                            
         EJECT                                                                  
*                                                                               
*   DETLBRAK:  IF DETAIL COST IS DIFFERENT THAN BUYHDR COST, AND                
*      PREVIOUS DETAILS HAVE BEEN ENCOUNTERED WITHIN THE BUYHDR                 
*      GROUP, THE FOLLOWING STEPS MUST BE TAKEN:                                
*         1.  THE BUY MUST BE OUTPUT                                            
*         2.  THE NEXT BUYLINE NUMBER MUST BE CALCULATED                        
*         3.  THE RECORD MUST BE CLEARED OF DETAIL INFORMATION                  
*         4.  PROCESSING OF THE NEW DETAIL WILL THEN CONTINUE                   
*                                                                               
DETLBRAK NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(DETAIL ELEMENT)                      
         USING RDARBUEL,R3         BUY DETAIL ELEMENT                           
*                                                                               
         L     R4,8(R1)            RESET A(RBUYREC)                             
         USING RBUYREC,R4                                                       
*                                                                               
         CLI   DETLFLAG,C'N'       ANY DETAILS ENCOUNTERED?                     
         BNE   DBRA0040            YES - CHECK FOR $$ CHANGE                    
         MVC   RBUYCOS,RDARBU$$    NO  - INSERT BUY COST IN CASE                
*                                     DIFFERENT FROM HEADER COST                
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
*                                                                               
         B     DBRA0200            EXIT                                         
DBRA0040 EQU   *                                                                
         CLC   RBUYCOS,RDARBU$$    BUY COST = DETAIL COST?                      
         BE    DBRA0200            YES - FINISHED                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        MISC FLAG ELEMENT                            
         BRAS  RE,GETEL                                                         
         BE    DBRA0045                                                         
*                                                                               
WKD      USING RBUYXXEL,ELEM                                                    
         XC    ELEM,ELEM                                                        
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         OI    WKD.RBUYXXFG,X'40'  MARK COST OVERRIDE FLAG                      
         DROP  WKD                                                              
         GOTO1 LOADELT,DMCB,(R4),ELEM,=C'ADD=CODE'                              
         B     DBRA0048                                                         
*                                                                               
DBRA0045 EQU   *                                                                
         USING RBUYXXEL,R6                                                      
         OI    RBUYXXFG,X'40'      MARK COST OVERRIDE FLAG                      
         DROP  R6                                                               
*                                                                               
DBRA0048 EQU   *                                                                
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
         TM    MISCFLAG,MFMANUAL   MANUAL REVISION??                            
         BZ    DBRA0050                                                         
         GOTO1 =A(MANMATCH),RR=RELO                                             
         B     DBRA0060                                                         
*                                                                               
DBRA0050 EQU   *                                                                
         GOTO1 =A(AUTOGEN),RR=RELO                                              
*                                                                               
DBRA0060 EQU   *                                                                
         MVC   RBUYCOS,RDARBU$$    INSERT NEW COST                              
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
         STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
DBRA0200 EQU   *                                                                
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
         CSECT                                                                  
TRUDATE  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)                                                         
*                                                                               
         USING RCONREC,R8                                                       
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
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,TDATELT,          X        
               =C'ADD=CODE'                                                     
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
         DS    0F                                                               
EDICT    NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            RESET A(SPOOLWORK)                           
         USING SPOOLD,R8                                                        
*                                                                               
*                                                                               
*                                                                               
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
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
         MVC   EDIPROG,EDICTACT    INSERT EDICT 'ACTION'                        
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
                                                                                
*                                                                               
* INFORMATION CHUNK FOR ETI REPORTING                                           
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO              DARE RECORD                                  
         USING RDARREC,R6                                                       
         MVC   EDIRDRRP,RDARKREP   REP CODE                                     
         MVC   EDIRDRAG,RDARKAGY   AGENCY CODE                                  
         MVC   EDIRDRST,RDARKSTA   STATION CODE                                 
         MVC   EDIRDRMD,RDARMEDI   MEDIA CODE                                   
                                                                                
         EDIT  RDAREST#,(3,EDIRDRES),ALIGN=LEFT                                 
                                                                                
* AGENCY ORDER #                                                                
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,EDIRDRAN),ALIGN=LEFT                               
                                                                                
* CONTRACT #                                                                    
         OC    RDARREP#,RDARREP#                                                
         BZ    EDICT10                                                          
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARREP#                                                
         EDIT  (P5,WORK2),(8,EDIRDRCN),ALIGN=LEFT                               
                                                                                
         L     R4,AIO3             RESET A(CONTRACT RECORD)                     
         USING RCONREC,R4                                                       
         MVC   EDIRDRSP,RCONSAL    SALESMAN CODE                                
         DROP  R4                                                               
                                                                                
EDICT10  DS    0H                                                               
         MVC   EDIRDRBY,RDARBUYC   BUYER CODE                                   
                                                                                
         MVI   ELCODE,X'02'        DESCRIPTIVE ELEMENT #2                       
         BAS   RE,GETEL                                                         
         BNE   EDICT50                                                          
         USING RDARCLEM,R6                                                      
         MVC   EDIRDRCL,RDARCLI    CLIENT CODE                                  
         MVC   EDIRDRP1,RDARPRD1   PRODUCT CODE 1                               
         MVC   EDIRDRP2,RDARPRD2   PRODUCT CODE 2                               
         DROP  R3,R6                                                            
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   APPRREJC:  APPROVAL/REJECTION HEADER OUTPUT                                 
*                                                                               
         DS    0F                                                               
APPRREJC NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            RESET A(SPOOLD)                              
         USING SPOOLD,R8                                                        
*                                                                               
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         LA    R4,P                SET PRINT OUTPUT                             
         USING ORDAPREJ,R4                                                      
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BNE   APRJ0020            NO  -  REJECT                                
         MVC   ARTRANID,=C'ORDAPP'                                              
         B     APRJ0040                                                         
APRJ0020 EQU   *                                                                
         MVC   ARTRANID,=C'ORDREJ'                                              
APRJ0040 EQU   *                                                                
*                                  INSERT IDENTIFIER                            
         MVC   ARORDNUM,RETORD#    INSERT ORDER NUMBER                          
         MVC   ARFROM,RETTO        INSERT FROM CODE                             
         MVC   ARTO,RETFROM        INSERT TO CODE                               
*                                     NOTE:  CODES ARE REVERSED                 
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',ARDATE)                              
*                                  INSERT TODAY'S DATE                          
*&&DO                                                                           
         TIME  DEC                 RETRIEVE TIME:  RETURNED IN R0               
*                                     AS HH:MM:SS:TT                            
         LR    R2,R0               MOVE TO A 'REAL' REGISTER                    
         SRDL  R2,24               SHIFT MM:SS:TT INTO R1                       
         LA    R2,6(R2)            ADD 6 TO HOURS                               
         SLDL  R2,8                SHIFT MM BACK TO R0                          
         STCM  R2,3,WORK+20        MOVE HH:MM TO WORK                           
         STCM  R2,3,WORK+32        SAVE FOR TEST DISPLAY                        
*                                                                               
*   NOTE ON EC TIME DISPLAY:                                                    
*     TIME IS NOW A HEX REPRESENTATION OF HH:MM.  THEREFORE,                    
*     IT MUST BE CONVERTED TO A MEANINGFUL DISPLAY FORMAT.  FOR                 
*     EXAMPLE, 6:15 AM WAS ORIGINALLY TAKEN FROM THE SYSTEM AS                  
*     TWO BYTES OF 0015 (DDS TIME: 6AM=0000).  ABOVE CODE ADDED                 
*     A VALUE OF 6 TO THE HOURS BYTE, PRODUCING 0615.  AT 11:15 AM,             
*     SYSTEM TIME WAS 0515.  ADDING 6 PRODUCED 0B15, WHICH MUST BE              
*     PROPERLY INTERPRETED FOR HOURS.                                           
*                                                                               
*     FIRST, INTERPRET THE HOURS BYTE...                                        
*                                                                               
         ZIC   R1,WORK+20          EXTRACT HOURS BYTE                           
         LA    RF,36               HOUR ADJUSTMENT                              
         CLI   WORK+20,X'28'       4AM-7AM?                                     
         BNL   APRJ0120            YES - SUBTRACT 36                            
         LA    RF,30               HOUR ADJUSTMENT                              
         CLI   WORK+20,X'1F'       1AM-3AM?                                     
         BNL   APRJ0120            YES - SUBTRACT 20                            
         LA    RF,6                HOUR ADJUSTMENT                              
         CLI   WORK+20,X'18'       6PM-MID?                                     
         BNL   APRJ0120            YES - SUBTRACT 6                             
         LA    RF,0                NO ADJUSTMENT                                
APRJ0120 EQU   *                                                                
         SR    R1,RF               SUBTRACT ADJUSTMENT FROM HOUR                
         EDIT  (R1),(2,WORK+24),FILL=0,ZERO=NOBLANK                             
*                                                                               
*     NEXT, DISPLAY THE MINUTES BYTE                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+21,WORK+26,1,=C'TOG'                            
         MVC   ARTIME,WORK+24      INSERT INTO OUTPUT                           
*&&                                                                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,ARTIME,2,0                                      
*                                                                               
*                                                                               
         MVC   ARSTAT(6),RETSTAT   INSERT STATION                               
         MVC   ARCON#,RETCON#      INSERT CONTRACT #                            
         MVC   ARRETSND,RETSENDR   INSERT 'RET TO SENDER' INFO                  
****>>>> MVC   ARDDS,=C'DDS'       INSERT LINE DELIMITER                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND THE MESSAGE                             
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    APRJ0320            OPENED:    DON'T UPDATE CONTRACT             
*                                     STATUS FLAG HERE...                       
*                                                                               
*                                  SET 'ORDER REJECTED' FLAG                    
*                                     IN CONTRACT RECORD AND REWRITE            
         MVC   AIO,AIO3            SET A(IO AREA)                               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         OC    RCONREC(L'RCONKEY),RCONREC                                       
         BZ    APRJ0320            ORDER IS UNLINKED                            
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM CONTRACT RECORD                 
         MVC   KEYSAVE,KEY         DON'T RETURN DELETED RECORDS                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   APRJ0320                       NO  - EXIT                        
         GOTO1 GETREC              READ CONTRACT                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BAS   RE,GETEL                                                         
         BE    APRJ0160            FOUND: UPDATE IT                             
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         LA    RF,RCONDL2Q                                                      
         STC   RF,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   AORHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     APRJ0200                                                         
APRJ0160 EQU   *                                                                
         MVI   2(R6),X'A0'         SET 'REJECTED+LINKED' FLAGS, CLEAR           
*                                     ANY PREVIOUS VALUE                        
APRJ0200 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
*                                                                               
APRJ0320 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R2,R4,R5,R8                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   REJCMSGS:  REJECTION MESSAGES                                               
*                                                                               
         DS    0F                                                               
REJCMSGS NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            RESET A(SPOOLD)                              
         USING SPOOLD,R8                                                        
*                                                                               
         L     R6,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R6                                                       
*                                                                               
         LA    RF,REJMESS          SET A(REJECTION MESSAGES)                    
         ST    RF,AREJMESS                                                      
*                                                                               
*   LINES ON SCREEN ARE COUNTED, TO DETERMINE WHERE '*' CONTINUATION            
*     MARKS ARE TO BE PLACED.  BLANK LINES WITHIN THE BODY OF THE               
*     REJECTION COMMENTS ARE SKIPPED.  THESE ARE THEN SKIPPED OVER              
*     WHEN THE SCREEN IS FORMATTED INTO THE OUTPUT MESSAGES.                    
*                                                                               
         LA    R2,AORREASH         MESSAGE HEADER                               
         LA    R3,AORLAST          LAST                                         
         SR    R4,R4                                                            
REMS0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING ON LINE?                            
         BE    REMS0040            NO  - DON'T COUNT LINE                       
         LA    R4,1(R4)            YES - COUNT LINE                             
REMS0040 EQU   *                                                                
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         CR    R2,R3               END OF SCREEN REACHED?                       
         BL    REMS0020            NO  - GO BACK FOR NEXT                       
REMS0060 EQU   *                                                                
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R5,P                                                             
         USING ORDCOM,R5                                                        
*                                                                               
         LR    R3,R4               SET UP LOOP                                  
         LTR   R3,R3               ANY REJECT LINES?                            
         BZ    REMS0100            NO                                           
         LA    R2,AORREASH                                                      
REMS0080 EQU   *                                                                
         BAS   RE,MSGCHECK         OUTPUT PREVIOUS LINE?                        
         MVC   OCTRANID,=C'ORDCOM'                                              
         MVC   OCORDNUM,RETORD#                                                 
REMS0090 EQU   *                                                                
         ZIC   RF,5(R2)            GET LENGTH OF LINE                           
         LTR   RF,RF               ANYTHING ON LINE?                            
         BNZ   REMS0095            YES                                          
         ST    RF,DMCB+8           INSERT LENGTH (ZERO) INTO P3                 
         GOTO1 REJMSGS,DMCB,(RC),0                                              
*                                  NO  - BLANK LINE IN MESS AREA                
         LA    R4,1(R4)            COUNT BLANK LINE IN                          
*                                     TOTAL LINES                               
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         B     REMS0080            GO BACK FOR NEXT                             
*                                     WITHOUT CHANGING COUNTER                  
REMS0095 EQU   *                                                                
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,REMS0950         MOVE BY LENGTH                               
****>>>  MVC   OCDDS,=C'DDS'       INSERT LINE DELIMITER                        
         ST    RF,DMCB+8           INSERT LENGTH INTO P3                        
         GOTO1 REJMSGS,DMCB,(RC),(R2)                                           
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         BCT   R3,REMS0080         GO BACK FOR NEXT                             
*                                                                               
* SUPPRESS STATION ORDER COMMENTS PER PETRY AND ELLEN WEINSTEIN                 
*                                                                               
REMS0100 EQU   *                                                                
         B     REMS1000                                                         
*        MVC   AIO,AIO2            SET IO AREA FOR READING                      
*        L     R2,AIO2             SET WORK AREA FOR BUYS                       
*        USING RBUYREC,R2                                                       
*                                                                               
*        XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
*        MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
*        MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
*        MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
*        MVC   KEY(27),RBUYREC     RETRIEVE KEY: ACTIVE ONLY                    
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 HIGH                                                             
*        B     REMS0140                                                         
*REMS0120 EQU   *                                                               
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 SEQ                 RETRIEVE NEXT BUY                            
*REMS0140 EQU   *                                                               
*        CLC   KEY(22),KEYSAVE     SAME ORDER?                                  
*        BNE   REMS1000            NO  - FINISHED                               
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 GETREC              RETRIEVE RECORD                              
*        LA    R3,RBUYELEM                                                      
*REMS0160 EQU   *                                                               
*        CLI   0(R3),0             END OF RECORD?                               
*        BE    REMS0120            YES                                          
*        CLI   0(R3),X'84'         BY ORDER COMMENT ELEMENT?                    
*        BNE   REMS0200            NO  -                                        
*REMS0180 EQU   *                                                               
*        USING RBUYOCEL,R3                                                      
*        CLI   RBUYOCID,0          ZERO = STATION INPUT                         
*        BNE   REMS0120            COMMENT IS REP SIDE - IGNORE                 
*        BAS   RE,MSGCHECK                                                      
*        BZ    REMS0190            NO OUTPUT:  DON'T ADD TO LINECT              
*        LA    R4,1(R4)            OUTPUT:  BUMP COUNT                          
*REMS0190 EQU   *                                                               
*        MVC   OCTRANID,=C'ORDCOM' STATION SIDE:  SEND IT OUT                   
*        MVC   OCORDNUM,RETORD#                                                 
*        EDIT  RBUYAGBL,(4,OCBUYLIN),FILL=0,ZERO=NOBLANK                        
*                                                                               
*  INSERT AGENCY ORDER BUYLINE NUMBER INTO ORDER COMMENT                        
*                                                                               
*        ZIC   RF,RBUYOCLN         GET LENGTH OF LINE                           
*        LA    RE,4                DECREMENT FOR MOVE:  SKIP                    
*                                     CODE/LENGTH                               
*                                     COMMENT ENTRY INDICATOR                   
*                                     1 FOR EXEC STATEMENT                      
*        SR    RF,RE               SUBTRACT 3                                   
*        EX    RF,REMS0960         MOVE BY LENGTH                               
****>>>> MVC   OCDDS,=C'DDS'       INSERT LINE DELIMITER                        
*REMS0200 EQU   *                                                               
*        ZIC   RF,1(R3)                                                         
*        AR    R3,RF               BUMP TO NEXT ELEMENT                         
*        B     REMS0160            GO BACK FOR NEXT ELEMENT                     
*                                                                               
REMS0950 MVC   OCCOMMNT(0),8(R2)   MOVE BY LENGTH                               
*REMS0960 MVC   OCCOMMNT(0),RBUYOCNT                                            
*                                  MOVE BY LENGTH                               
*                                                                               
REMS1000 EQU   *                                                                
*                                                                               
*   DETERMINE IF LAST BUYLINE COMMENT ENTRY IS STILL WAITING TO                 
*      SPOOL.                                                                   
*                                                                               
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    REMS1100                                                         
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    REMS1100            NO  - NO OUTPUT                              
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
         OC    OCBUYLIN,OCBUYLIN   BUYLINE NUMBER IN LINE?                      
         BZ    REMS1100            NO                                           
         CLC   OCBUYLIN,SPACES     BUYLINE NUMBER IN LINE?                      
         BE    REMS1100                                                         
*                                                                               
*   ONLY BUMP THE LINECOUNT FOR COMMENTS FROM BUYLINE RECORDS.                  
*      THE SCREEN MESSAGE LINES ARE ALREADY COUNTED.                            
*                                                                               
*        LA    R4,1(R4)            BUMP THE LINECOUNT                           
*                                                                               
*        DROP  R2,R3,R5                                                         
         DROP  R5                                                               
*                                                                               
REMS1100 EQU   *                                                                
         USING ORDTRLR,R5                                                       
         MVC   OTTRANID,=C'ORDTLR'                                              
         MVC   OTORDNUM,RETORD#                                                 
         LA    R4,2(R4)            ADD 1 EACH FOR HDR, TRLR                     
         EDIT  (R4),(6,OTCOUNT),FILL=0,ZERO=NOBLANK                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
*   INSERT REJECT DATE/TIME STAMP                                               
*                                                                               
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER ON SCREEN?               
         CLI   5(R2),0                                                          
         BE    REMS1300            NO  - DON'T GET CONTRACT NUMBER              
         L     RF,AIOAREA          PREPARE TO REWRITE CONTRACT RECORD           
         ST    RF,AIO              SET IO AREA TO READ OLD RECORD               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM NEW RECORD                      
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEY ALSO                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   REMS1300            YES                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       RETURN DELETED RECORDS ALSO                  
         GOTO1 GETREC              READ INTO AIOAREA                            
         MVC   AIO,AIO3            RESET TO UPDATED CON RECORD                  
*                                                                               
*                                  UPDATE CONTRACT STATUS ELEMENT               
*                                                                               
*                                                                               
         LA    R2,RCONELEM         FIND X'1D' ELEMENT                           
REMS1120 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    REMS1140            YES - NOT FOUND - BUILD IT                   
         CLI   0(R2),X'1D'         DARE ELEMENT?                                
         BE    REMS1200            FOUND: UPDATE IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     REMS1120            GO BACK FOR NEXT                             
REMS1140 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         LA    RF,RCONDL2Q                                                      
         STC   RF,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   AORHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    ELTBUILD+2,X'08'    SET DAILY FLAG                               
                                                                                
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         MVC   ELTBUILD+RCONDRDR-RCONDREL(4),ACTDATE                            
*                                  YES - MOVE DATE+TIME TO REJECTED             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     REMS1250                                                         
REMS1200 EQU   *                                                                
                                                                                
         MVI   2(R2),X'A0'         SET 'REJECTED+LINKED' FLAGS, CLEAR           
*                                     ANY PREVIOUS VALUE                        
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    2(R2),X'08'         SET DAILY FLAG                               
                                                                                
         MVC   RCONDRDR-RCONDREL(4,R2),ACTDATE                                  
*                                  YES - MOVE DATE+TIME TO REJECTED             
REMS1250 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
REMS1300 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
*   MSGCHECK:  DETERMINE IF A LINE HAS TO BE SPOOLED.  THIS IS TO               
*      ENABLE THE '*' TO BE INSERTED FOR ANOTHER LINE FOLLOWING                 
*      INDICATOR.  THIS ROUTINE WILL ONLY BE CALLED WHEN A NEW LINE             
*      IS TO BE CONSTRUCTED.  IF OLD LINE IS WAITING TO SPOOL, IT               
*      MUST BE FLAGGED.                                                         
*   NOTE:  CONDITION CODE ON EXIT DETERMINES WHETHER LINE COUNT IS              
*      TO BE INCREMENTED.                                                       
*                                                                               
MSGCHECK NTR1                                                                   
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    MCHE0100            NO  - NO OUTPUT                              
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    MCHE0100            NO  - NO OUTPUT                              
         USING ORDCOM,R5                                                        
         MVI   OCCONTIN,C'*'       YES - INSERT INDICATOR                       
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
MCHE0100 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R5,R8                                                            
         EJECT                                                                  
*                                                                               
*   REJMSGS:  INSERTS MESSAGE INTO HOLD AREA, FOR LATER INCLUSION               
*        IN THE AGENCY ORDER HEADER, SO THAT MESSAGES CAN BE RE-                
*        CALLED DURING WORKSHEET PRINTING.                                      
*                                                                               
REJMSGS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(ERROR MESSAGE LINE)                  
         L     RF,8(R1)            RESET L(ERROR MESSAGE LINE)                  
         L     R3,AREJMESS         SET A(NEXT MESSAGE SLOT)                     
         LTR   RF,RF               ANY LENGTH IN LINE?                          
         BNZ   RMSG0040            YES - PROCESS                                
         MVC   0(2,R3),=X'1002'    NO  - PUT IN EMPTY ELEMENT                   
         LA    R3,2(R3)                                                         
         B     RMSG0200            EXIT                                         
RMSG0040 EQU   *                                                                
         LR    RE,RF               SET NEW ELEMENT LENGTH                       
         LA    RE,3(RE)            ADD FOR ELTID+CTRL+EX DEC                    
         MVI   0(R3),X'10'         INSERT ELEMENT CODE                          
         STC   RE,1(R3)            INSERT ELEMENT LENGTH                        
         EX    RF,RMSG0080         MOVE BY LENGTH                               
         AR    R3,RE               ADD LENGTH TO SLOT ADDR                      
         B     RMSG0200                                                         
*                                                                               
RMSG0080 MVC   2(0,R3),8(R2)       INSERT MESSAGE BY LENGTH                     
RMSG0200 EQU   *                                                                
         ST    R3,AREJMESS         REPLACE A(NEXT MESSAGE SLOT)                 
         XC    0(4,R3),0(R3)       CLEAR NEXT SLOT                              
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
*                                                                               
STARTEND NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,4(R1)            A(RECEIVING FIELD)                           
         L     R5,8(R1)            A(MYAREAD)                                   
         USING MYAREAD,R5                                                       
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R8,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R8               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R8,RF               GET A(1ST DAY+1)                             
         BCTR  R8,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R8               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STEX0040 EQU   *                                                                
         CR    R8,RE               END OF ROTATION FIELD REACHED?               
         BNE   STEX0080            NO                                           
         LA    R8,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STEX0080 EQU   *                                                                
         CLI   0(R8),C' '          ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   0(R8),0             ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STEX0120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STEX0100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STEX0100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STEX0120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STEX0160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STEX0160 EQU   *                                                                
         LR    RF,R8               YES - SAVE NEW ARRAY POSITION                
STEX0200 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STEX0040         GO BACK AND CHECK NEXT                       
         LA    R8,0(R2)            A(START OF ARRAY)                            
         BCTR  R8,0                BACK UP 1 POSITION                           
         SR    RF,R8               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R8,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STEX0240 EQU   *                                                                
         CLI   0(R8),C' '          DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         CLI   0(R8),0             DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STEX0280 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT POSITION                        
         BCT   R0,STEX0240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STEX0320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STEX0320 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDREJS :  REJECTION MESSAGES                                               
*                                                                               
         DS    0F                                                               
ADDREJS  NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R4,4(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         LA    R3,REJMESS          SET A(REJECT MSG AREA)                       
*                                                                               
ARJS0020 EQU   *                                                                
         CLI   0(R3),0             ANY ENTRY?                                   
         BE    ARJS0080            NO  - FINISHED                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),(R3),=C'ADD=CODE'             
*                                  YES - ADD ELEMENT TO AGY HDR REC             
         ZIC   RF,1(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     ARJS0020            GO BACK FOR NEXT                             
ARJS0080 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   INSERT DATE/TIME ELEMENT                     
         MVC   ELTBUILD(2),=X'2006'                                             
         MVC   ELTBUILD+2(4),ACTDATE                                            
*                                  MOVE DATE+TIME TO OPENED                     
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELTBUILD,            X        
               =C'ADD=CODE'                                                     
         B     EXIT                                                             
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DOAUDIT :  ADD AUDIT TRAIL                                                  
*                                                                               
         DS    0F                                                               
DOAUDIT  NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R4,8(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         MVC   REVNUM,RDARRNUM                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),RDARKEY                                     
         MVI   KEY+RDARKRT-RDARKEY,X'70'                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   DOAUDX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVI   DMCB+5,DHAPPROQ     ACTION OPEN                                  
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    *+8                                                              
         MVI   DMCB+5,DHREJECQ     ACTION REJECT                                
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         GOTO1 VREGENDH,DMCB,AIO,,WORK                                          
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
DOAUDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATETIME:  DEVELOPS THE DATE AND TIME FOR STAMPING                          
*                                                                               
         DS    0F                                                               
DATETIME NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,ACTDATE)                                 
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,ACTTIME                                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BLDWCMT: BUILD BUY COMMENT OF ADDITIONAL WEIGHT                             
***********************************************************************         
         DS    0F                                                               
BLDWCMT  NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R4,AIO2             SET A(NEW BUY RECORD)                        
         L     R6,AIOAREA          SET A(OLD BUY RECORD)                        
         USING RBUYREC,R6                                                       
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BNE   BLDWX                                                            
AGYBUY   USING RBUYDTEL,R4                                                      
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
*****                                                                           
***** GENERIC COMMENT FOR NOW                                                   
*****                                                                           
*        MVC   ELEMENT(24),=C'ADD TO SCHEDULE FOR BUY '                         
*        EDIT  RBUYKMLN,(3,ELEMENT+24),ALIGN=LEFT                               
         MVC   ELEMENT(20),=C'ADDITION TO SCHEDULE'                             
         B     BLDW15                                                           
*****                                                                           
         LA    R2,ELEMENT                                                       
         MVC   0(16,R2),=C'+WEIGHT FOR BUY '                                    
         LA    R2,16(R2)                                                        
         EDIT  RBUYKMLN,(3,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         DROP  R6                                                               
*                                                                               
BLDW10   DS    0H                                                               
         MVC   0(2,R2),=C',+'                                                   
         LA    R2,2(R2)                                                         
         EDIT  AGYBUY.RBUYDTNW,(3,0(R2)),ALIGN=LEFT                             
         AR    R2,R0                                                            
         MVC   0(6,R2),=C' SPTS/'                                               
         LA    R2,6(R2)                                                         
         EDIT  AGYBUY.RBUYDTWK,(3,0(R2)),ALIGN=LEFT                             
         AR    R2,R0                                                            
         MVC   0(5,R2),=C' WKS,'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         BAS   RE,NEXTEL2                                                       
         BE    BLDW10                                                           
*                                                                               
         L     R4,AIO2                                                          
         BCTR  R2,0                                                             
         MVC   0(2,R2),=C'(+'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
         L     R4,AIO2                                                          
AGYBUY   USING RBUYREC,R4                                                       
         EDIT  AGYBUY.RBUYTSPT,(3,0(R2)),ALIGN=LEFT                             
         AR    R2,R0                                                            
         MVC   0(8,R2),=C' SPTS,+$'                                             
         LA    R2,8(R2)                                                         
         EDIT  AGYBUY.RBUYTCOS,(12,0(R2)),2,ALIGN=LEFT                          
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
*                                                                               
* ADD CHANGE COMMENT(S)                                                         
*                                                                               
         OC    ELEMENT,ELEMENT           EXIT IF NO CHANGES                     
         BZ    GETCDX                                                           
*                                                                               
* ALWAYS REPLACE EXISTING BUY ORDER COMMENT                                     
*                                                                               
BLDW15   DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIO2),0,0                   
*                                                                               
         XC    ELTBUILD,ELTBUILD   BUILD AND ADD CHANGE BUY ORD CMTS            
         MVC   ELTBUILD+3(60),ELEMENT                                           
BLDW20   LA    R1,60                                                            
         LA    R2,ELTBUILD+63                                                   
BLDW30   CLI   0(R2),0                                                          
         BE    BLDW40                                                           
         CLI   0(R2),C' '                                                       
         BNE   BLDW50                                                           
BLDW40   BCTR  R2,0                                                             
         BCT   R1,BLDW30                                                        
         CLI   0(R2),C','                                                       
         BE    BLDWX               NO MORE COMMENT, ENDED WITH A COMMA          
         DC    H'0'                                                             
*                                                                               
BLDW50   DS    0H                                                               
         CLI   0(R2),C','          LINE SHOULDN'T END IN A COMMA                
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,4(R1)                                                         
         STC   R1,ELTBUILD+1                                                    
         MVI   ELTBUILD,X'84'                                                   
         MVI   ELTBUILD+2,X'80'    FLAG AS REP ORDER COMMENT                    
*                                  USE HELLO FOR FIRST LINE                     
         L     R4,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL2                                                        
         BE    BLDW60                                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
         OC    ELEMENT+60(60),SPACES                                            
         CLC   ELEMENT+60(60),SPACES                                            
         BE    BLDWX                                                            
         L     R4,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R4               SAVE OFF LOCATION FOR SECOND ELEMENT         
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+3(60),ELEMENT+60                                        
         B     BLDW20              GO ADD SECOND ELEMENT                        
*                                                                               
BLDW60   DS    0H                  SECOND LINE MUST USE RECUP FOR               
         ZIC   R1,1(R4)            PROPER ORDERING                              
         AR    R4,R1                                                            
         GOTO1 VRECUP,DMCB,(2,AIO2),ELTBUILD,(R4),0                             
*                                                                               
BLDWX    EQU   *                                                                
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GETMDCD: RETRIEVE MODIFICATION CODES FOR BUY. COMPARES THE AGENCY           
*            BUY AGAINST THE REP BUY                                            
         DS    0F                                                               
GETMDCD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R4,AIO2             SET A(NEW BUY RECORD)                        
AGYBUY   USING RBUYREC,R4                                                       
         L     R6,AIOAREA          SET A(OLD BUY RECORD)                        
         USING RBUYREC,R6                                                       
*                                                                               
         XC    ELEMENT,ELEMENT     USE TO STORE CHANGE COMMENTS                 
         LA    R2,ELEMENT                                                       
*                                                                               
*   CERTAIN FIELDS MUST BE RESET BEFORE OLD VS NEW COMPARE CAN BE               
*      DONE.  INCLUDED IN THESE FIELDS ARE CLASS AND SECTION, WHICH             
*      CAN ONLY BE ENTERED FROM THE REP SIDE, SO WE DON'T WANT TO               
*      LOSE THEM.                                                               
*                                                                               
         MVC   AGYBUY.RBUYCREA,RBUYCREA                                         
*                                  INSERT OLD CREATE DATE IN NEW                
         MVC   AGYBUY.RBUYCHGD,RBUYCHGD                                         
*                                  INSERT OLD CHANGE DATE IN NEW                
         MVC   AGYBUY.RBUYCHGI,RBUYCHGI                                         
*                                  INSERT OLD CODES IN NEW RECORD               
         MVC   NEWVER#,AGYBUY.RBUYVER                                           
*                                  SAVE NEW VERSION #                           
         MVC   AGYBUY.RBUYVER,RBUYVER                                           
*                                  MOVE OLD VERSION NUMBER TO                   
*                                     NEW RECORD                                
         MVC   AGYBUY.RBUYCLS(6),RBUYCLS                                        
*                                  INSERT OLD CLASS/SECTION IN NEW              
*                                                                               
*   COMPARE TOTAL RECORDS:  IF SAME, CODE IS OKAY                               
*                                                                               
         ZICM  RF,AGYBUY.RBUYLEN,2 RETRIEVE L(NEW RECORD)                       
         BCTR  RF,0                                                             
         EX    RF,GETCD10          COMPARE BY LENGTH                            
         BE    GETCDX              RECORDS EQUAL:  OLD CODES WHICH              
         B     GETCD20                WERE INSERTED ARE USED.                   
*                                                                               
GETCD10  CLC   AGYBUY.RBUYREC(0),RBUYREC                                        
*                                                                               
GETCD20  DS    0H                                                               
         MVC   AGYBUY.RBUYVER,NEWVER#                                           
*                                  REINSERT NEW VERSION NUMBER                  
         GOTO1 DATCON,DMCB,(5,FULL),(3,AGYBUY.RBUYCHGD)                         
*                                  USE TODAYS DATE IN NEW RECORD                
         XCEFL WORK2,300           CLEAR STORAGE AREA FOR CHG CODES             
         LA    R3,WORK2                                                         
         CLC   AGYBUY.RBUYFLT,RBUYFLT                                           
*                                  SAME FLIGHT?                                 
         BE    GETCD30             YES                                          
         MVI   0(R3),C'F'          NO  - INSERT 'FLIGHT CHANGE'                 
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
GETCD30  DS    0H                                                               
         CLC   AGYBUY.RBUYDUR,RBUYDUR                                           
*                                  SAME DURATION?                               
         BE    GETCD35             YES                                          
         MVI   0(R3),C'L'          NO  - INSERT 'LENGTH CHANGE'                 
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
         ZICM  RE,AGYBUY.RBUYDUR,2                                              
         ZICM  RF,RBUYDUR,2                                                     
*        MVI   0(R2),C'1'                                                       
         CR    RE,RF                                                            
         BL    GETCD31                                                          
         MVI   0(R2),C'+'                                                       
         LA    R2,1(R2)                                                         
GETCD31  DS    0H                                                               
         SR    RE,RF                                                            
         ST    RE,FULL                                                          
         EDIT  FULL,(4,0(R2)),FLOAT=-,ALIGN=LEFT                                
         AR    R2,R0                                                            
         MVC   0(5,R2),=C' SEC,'                                                
         TM    AGYBUY.RBUYDUR,X'80'                                             
         BZ    *+10                                                             
         MVC   1(3,R2),=C'MIN'                                                  
         LA    R2,5(R2)                                                         
*                                                                               
GETCD35  EQU   *                                                                
         CLC   AGYBUY.RBUYKPLN,RBUYKPLN                                         
*                                  SAME PLAN?                                   
         BE    GETCD40             YES                                          
         MVI   0(R3),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
GETCD40  EQU   *                                                                
*                                                                               
*   CLASS AND SECTION ARE ILLOGICAL TESTS HERE.  NEITHER FIELD CAN BE           
*      ENTERED VIA AN AGENCY ORDER.  THEY MAY BE ENTERED ONLY VIA THE           
*      REP CONTRACT BUY SCREEN.  AS SUCH, THEY WILL ALWAYS BE ON THE            
*      'OLD' RECORD AND NEVER ON THE 'NEW' RECORD.  A PREVIOUSLY-               
*      ENTERED CODE IS TO BE CARRIED OVER.  CODING DURING FULL RECORD           
*      COMPARISONS TAKES CARE OF THAT.                                          
*                                                                               
         CLC   AGYBUY.RBUYCLS,RBUYCLS                                           
*                                  SAME CLASS?                                  
         BE    GETCD50             YES                                          
         MVI   0(R3),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
GETCD50  EQU   *                                                                
         CLC   AGYBUY.RBUYSEC,RBUYSEC                                           
*                                  SAME SECTION?                                
         BE    GETCD60             YES                                          
         MVI   0(R3),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
GETCD60  EQU   *                                                                
         CLC   AGYBUY.RBUYCOS,RBUYCOS                                           
*                                  SAME COST?                                   
         BE    GETCD70             YES                                          
         MVI   0(R3),C'R'          NO  - INSERT 'RATE CHG'                      
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
         ZICM  RE,AGYBUY.RBUYCOS,4                                              
         ZICM  RF,RBUYCOS,4                                                     
         MVC   0(4,R2),=C'RATE'                                                 
         LA    R2,4(R2)                                                         
         MVI   0(R2),C'-'                                                       
         CR    RE,RF                                                            
         BL    GETCD65                                                          
         MVI   0(R2),C'+'                                                       
GETCD65  DS    0H                                                               
         MVI   1(R2),C'$'                                                       
         LA    R2,2(R2)                                                         
         SR    RE,RF                                                            
         ST    RE,FULL                                                          
         EDIT  FULL,(12,0(R2)),2,ALIGN=LEFT                                     
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         DROP  R6,AGYBUY                                                        
         EJECT                                                                  
***********************************************************************         
* CHECK BUY COMMENTS                                                            
***********************************************************************         
GETCD70  DS    0H                                                               
         L     R6,AIOAREA                                                       
         L     R4,AIO2                                                          
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETCD80                                                          
         USING RBUYCMEL,R6                                                      
         CLC   =C'P=',RBUYCMNT     SKIP PROGRAM NAME COMMENT                    
         BE    GETCD73                                                          
         CLC   =C'MG=',RBUYCMNT    IF CREDIT/MAKEGOOD, FIRST LINE               
         BE    GETCD73             IS RESERVED FOR MG=/CR= REFERENCE            
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   GETCD75                                                          
*                                                                               
GETCD73  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GETCD80                                                          
*                                                                               
GETCD75  DS    0H                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BNE   GETCD110                                                         
         B     GETCD90                                                          
*                                                                               
GETCD80  DS    0H                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                                                        
         BE    GETCD110                                                         
         B     GETCD130                                                         
*                                                                               
GETCD90  DS    0H                                                               
AGYBUY   USING RBUYCMEL,R4                                                      
         USING RBUYCMEL,R6                                                      
         CLC   AGYBUY.RBUYCMLN,RBUYCMLN                                         
         BNE   GETCD110                                                         
         ZIC   R1,AGYBUY.RBUYCMLN                                               
         BCTR  R1,0                                                             
         EX    R1,GETCD120                                                      
*        BNE   GETCD110                                                         
         BE    GETCD130                                                         
*                                                                               
*        BAS   RE,NEXTEL                                                        
*        BNE   GETCD100                                                         
*        BAS   RE,NEXTEL2                                                       
*        BE    GETCD90                                                          
*        B     GETCD110                                                         
*                                                                               
*ETCD100 DS    0H                                                               
*        BAS   RE,NEXTEL2                                                       
*        BNE   GETCD130                                                         
*                                                                               
GETCD110 DS    0H                                                               
         MVI   0(R3),C'Z'          NO  - INSERT 'BUY COMMENT CHG'               
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
         MVC   0(9,R2),=C'CMT CHGD,'                                            
         LA    R2,9(R2)                                                         
         B     GETCD130                                                         
*                                                                               
GETCD120 CLC   AGYBUY.RBUYCMEL(0),RBUYCMEL                                      
         DROP  R6,AGYBUY                                                        
         EJECT                                                                  
***********************************************************************         
* CHECK IF PROGRAM NAME CHANGED                                                 
***********************************************************************         
GETCD130 EQU   *                                                                
         L     R6,AIOAREA                                                       
         L     R4,AIO2                                                          
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETCD140                                                         
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BE    GETCD150                                                         
         B     GETCD170                                                         
*                                                                               
GETCD140 EQU   *                                                                
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BNE   GETCD200                                                         
*                                                                               
GETCD150 DS    0H                                                               
AGYBUY   USING RBUYPGEL,R4                                                      
         USING RBUYPGEL,R6                                                      
         CLC   AGYBUY.RBUYPGLN,RBUYPGLN                                         
         BNE   GETCD170                                                         
         ZIC   R1,AGYBUY.RBUYPGLN                                               
         BCTR  R1,0                                                             
         EX    R1,GETCD190                                                      
         BE    GETCD200                                                         
*                                                                               
GETCD170 DS    0H                                                               
         LR    RE,R3                                                            
         SHI   RE,1                                                             
         CLI   0(RE),C'Z'          CHECK PREVIOUS MOD CODE                      
         BE    GETCD180            CHANGE CODE Z ALREADY SET?                   
         MVI   0(R3),C'Z'          NO  - INSERT 'PGM CHGD'                      
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
GETCD180 DS    0H                                                               
         MVC   0(9,R2),=C'PGM CHGD,'                                            
         LA    R2,9(R2)                                                         
         B     GETCD200                                                         
*                                                                               
GETCD190 CLC   AGYBUY.RBUYPGEL(0),RBUYPGEL                                      
         DROP  R6,AGYBUY                                                        
         EJECT                                                                  
***********************************************************************         
* CHECK DAY/TIME ELEMENTS                                                       
***********************************************************************         
GETCD200 EQU   *                                                                
         L     R6,AIOAREA                                                       
         L     R4,AIO2                                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETCD210 DS    0H                  MAX 1 TIME AND 1 DAY CHANGES NOTED           
AGYBUY   USING RBUYDYEL,R4                                                      
         USING RBUYDYEL,R6                                                      
         CLC   AGYBUY.RBUYDYIN,RBUYDYIN                                         
         BNE   GETCD215                                                         
*                                                                               
         MVC   WORK(1),AGYBUY.RBUYDAYS                                          
         MVC   WORK+1(1),RBUYDAYS                                               
         NI    WORK,X'FF'-X'80'    REMOVE CC OVERRIDE                           
         NI    WORK+1,X'FF'-X'80'  FOR DAYS COMPARISON                          
         CLC   WORK(1),WORK+1                                                   
         BE    GETCD230                                                         
*                                                                               
GETCD215 DS    0H                                                               
         CLI   0(R3),C'T'                                                       
         BNE   GETCD220                                                         
         MVI   1(R3),C'D'                                                       
         LA    R3,2(R3)                                                         
         B     GETCD221                                                         
*                                                                               
GETCD220 DS    0H                                                               
         MVI   0(R3),C'D'                                                       
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
GETCD221 DS    0H                                                               
         MVC   0(8,R2),=C'DAY WAS '                                             
         LA    R2,8(R2)                                                         
         GOTO1 VOUTDAY,DMCB,RBUYDAYS,RBUYDYIN,0(R2)                             
GETCD223 CLI   0(R2),0                                                          
         BE    GETCD225                                                         
         CLI   0(R2),C' '                                                       
         BE    GETCD225                                                         
         LA    R2,1(R2)                                                         
         B     GETCD223                                                         
*                                                                               
GETCD225 DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R3),C'D'                                                       
         BNE   GETCD300                                                         
*                                                                               
GETCD230 DS    0H                                                               
         CLC   AGYBUY.RBUYDYT1(4),RBUYDYT1                                      
         BE    GETCD250                                                         
         CLI   0(R3),C'D'                                                       
         BNE   GETCD240                                                         
         MVI   1(R3),C'T'                                                       
         LA    R3,2(R3)                                                         
         B     GETCD241                                                         
*                                                                               
GETCD240 DS    0H                                                               
         MVI   0(R3),C'T'                                                       
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
GETCD241 DS    0H                                                               
         MVC   0(9,R2),=C'TIME WAS '                                            
         LA    R2,9(R2)                                                         
*                                                                               
         GOTO1 UNTIME,DMCB,RBUYDYT1,0(R2)                                       
*                                                                               
GETCD243 CLI   0(R2),0                                                          
         BE    GETCD245                                                         
         CLI   0(R2),C' '                                                       
         BE    GETCD245                                                         
         LA    R2,1(R2)                                                         
         B     GETCD243                                                         
*                                                                               
GETCD245 DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R3),C'T'                                                       
         BNE   GETCD300                                                         
*                                                                               
GETCD250 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GETCD260                                                         
         BAS   RE,NEXTEL2                                                       
         BE    GETCD210                                                         
         B     GETCD270                                                         
*                                                                               
GETCD260 DS    0H                                                               
         BAS   RE,NEXTEL2                                                       
         BNE   GETCD300                                                         
*                                                                               
GETCD270 DS    0H                  NEW ORBIT FOUND, DAY/TIME CHANGED            
         MVC   0(2,R3),=C'DT'                                                   
         LA    R3,2(R3)            BUMP TO NEXT POSITION                        
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
         MVC   0(13,R2),=C'NEW DAY/TIME,'                                       
         LA    R2,13(R2)                                                        
         DROP  R6,AGYBUY                                                        
         EJECT                                                                  
***********************************************************************         
* CHECK EFFECTIVE DATE ELEMENTS                                                 
***********************************************************************         
GETCD300 EQU   *                                                                
         L     R6,AIOAREA                                                       
         L     R4,AIO2                                                          
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETCD310 DS    0H                  MAX 1 DATE AND 1 SPOT CHANGES NOTED          
AGYBUY   USING RBUYDTEL,R4                                                      
         USING RBUYDTEL,R6                                                      
         CLC   AGYBUY.RBUYDTST(6),RBUYDTST                                      
         BE    GETCD330                                                         
         CLI   0(R3),C'S'                                                       
         BNE   GETCD320                                                         
         MVI   1(R3),C'E'                                                       
         LA    R3,2(R3)                                                         
         B     GETCD325                                                         
*                                                                               
GETCD320 DS    0H                                                               
         MVI   0(R3),C'E'                                                       
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
GETCD325 DS    0H                                                               
         MVC   0(9,R2),=C'DATE WAS '                                            
         LA    R2,9(R2)                                                         
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(4,0(R2))                               
         LA    R2,5(R2)                                                         
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         EDIT  RBUYDTWK,(3,0(R2)),ZERO=NOBLANK,ALIGN=LEFT                       
         AR    R2,R0                                                            
         MVC   0(3,R2),=C'WK,'                                                  
         CLI   RBUYDTWK,1                                                       
         BNH   GETCD327                                                         
         MVC   2(2,R2),=C'S,'                                                   
         LA    R2,1(R2)                                                         
GETCD327 LA    R2,3(R2)                                                         
GETCD328 CLI   0(R3),C'E'                                                       
         BNE   GETCD380                                                         
*                                                                               
* CHECK SPOTS PER WEEK                                                          
*                                                                               
GETCD330 DS    0H                                                               
         CLC   AGYBUY.RBUYDTNW,RBUYDTNW                                         
         BE    GETCD350                                                         
         CLI   0(R3),C'E'                                                       
         BNE   GETCD340                                                         
         MVI   1(R3),C'S'                                                       
         LA    R3,2(R3)                                                         
         B     GETCD345                                                         
*                                                                               
GETCD340 DS    0H                                                               
         MVI   0(R3),C'S'                                                       
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
GETCD345 DS    0H                                                               
         MVC   0(10,R2),=C'#SPTS WAS '                                          
         LA    R2,10(R2)                                                        
         EDIT  RBUYDTNW,(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         CLI   0(R3),C'S'                                                       
         BNE   GETCD380                                                         
*                                                                               
GETCD350 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GETCD360                                                         
         BAS   RE,NEXTEL2                                                       
         BE    GETCD310                                                         
         B     GETCD370                                                         
*                                                                               
GETCD360 DS    0H                                                               
         BAS   RE,NEXTEL2                                                       
         BNE   GETCD380                                                         
*                                                                               
GETCD370 DS    0H                  NEW ELM FOUND, DATE/SPOT CHANGED             
         CLI   AGYBUY.RBUYDTNW,0   UNLESS IT IS A PARTIAL CANCEL                
         BE    GETCD380                                                         
         MVC   0(2,R3),=C'ES'                                                   
*                                                                               
* EXPRESS CHANGE AS COMMENT                                                     
*                                                                               
         MVC   0(11,R2),=C'+DATE/#SPTS'                                         
         LA    R2,11(R2)                                                        
*                                                                               
GETCD380 EQU   *                                                                
         CLI   WORK2+2,0                                                        
         BE    GETCD390                                                         
         MVC   WORK2(2),=C'* '                                                  
         DROP  R6,AGYBUY                                                        
*                                                                               
GETCD390 EQU   *                                                                
         L     R4,AIO2             SET A(NEW BUY RECORD)                        
AGYBUY   USING RBUYREC,R4                                                       
         MVC   AGYBUY.RBUYCHGI,WORK2                                            
         DROP  AGYBUY                                                           
*                                  INSERT DEVELOPED CHANGE CODES                
*                                                                               
* ADD CHANGE COMMENT(S)                                                         
*                                                                               
         OC    ELEMENT,ELEMENT     EXIT IF NO CHANGES                           
         BZ    GETCDX                                                           
*                                                                               
* ALWAYS REPLACE EXISTING BUY ORDER COMMENT                                     
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIO2),0,0                   
*                                                                               
*        MVI   ELCODE,X'84'        ROOM ONLY FOR 2 COMMENT ELEMENTS             
*        BAS   RE,GETEL2                                                        
*        LR    R6,R4                                                            
*        BNE   GETCD395                                                         
*        BAS   RE,NEXTEL2                                                       
*        BE    GETCDX                                                           
*                                                                               
GETCD395 DS    0H                  BUILD AND ADD CHANGE BUY ORD CMTS            
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+3(60),ELEMENT                                           
GETCD398 LA    R1,60                                                            
         LA    R2,ELTBUILD+63                                                   
GETCD400 CLI   0(R2),0                                                          
         BE    GETCD405                                                         
         CLI   0(R2),C' '                                                       
         BNE   GETCD410                                                         
GETCD405 BCTR  R2,0                                                             
         BCT   R1,GETCD400                                                      
         B     GETCDX                                                           
*        CLI   0(R2),C','                                                       
*        BE    GETCDX              NO MORE COMMENT, ENDED WITH A COMMA          
*        DC    H'0'                                                             
*                                                                               
GETCD410 DS    0H                                                               
         CLI   0(R2),C','          LINE SHOULDN'T END IN A COMMA                
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,4(R1)                                                         
         STC   R1,ELTBUILD+1                                                    
         MVI   ELTBUILD,X'84'                                                   
         MVI   ELTBUILD+2,X'80'    FLAG AS REP ORDER COMMENT                    
*                                  USE HELLO FOR FIRST LINE                     
         L     R4,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL2                                                        
         BE    GETCD420                                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
         OC    ELEMENT+60(60),SPACES                                            
         CLC   ELEMENT+60(60),SPACES                                            
         BE    GETCDX                                                           
         L     R4,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R4               SAVE OFF LOCATION FOR SECOND ELEMENT         
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+3(60),ELEMENT+60                                        
         B     GETCD398            GO ADD SECOND ELEMENT                        
*                                                                               
GETCD420 DS    0H                  SECOND LINE MUST USE RECUP FOR               
         ZIC   R1,1(R4)            PROPER ORDERING                              
         AR    R4,R1                                                            
         GOTO1 VRECUP,DMCB,(2,AIO2),ELTBUILD,(R4),0                             
*                                                                               
GETCDX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GENCOMMT:  GENERATE A COMMENT RECORD IN THOSE INSTANCES WHEN                
*      A PROGRAM NAME IS PRESENT, AND NO DARE COMMENTS HAVE BEEN                
*      FOUND.  OTHERWISE, THE PROGRAM NAME AS A COMMENT WILL BE                 
*      SKIPPED.                                                                 
*                                                                               
GENCOMMT NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            RESET A(BUYREC)                              
         USING RBUYREC,R2                                                       
*                                                                               
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEMENT,ELEMENT     FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEMENT          ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEMENT,0                     
         DROP  R6                                                               
*                                                                               
GCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
GCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   GCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,GCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
GCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORK                               
         EX    RF,GCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
         B     EXIT                                                             
*                                                                               
GCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS OPEN, ADD A PASSIVE KEY             
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
GENEDIKY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REDIKEY,R6                                                       
         MVI   REDIKTYP,REDIKTYQ                                                
         MVC   REDIKREP,AGENCY                                                  
         MVI   REDIKACT,C'A'       ACTION IF OPEN                               
         MVC   REDIKCON,CCONKNUM   CONTRACT NUMBER IN PWOS                      
*                                  DATE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,REDIKDTE)                                
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,REDIKTIM       TIME IN HHMMSS                               
         DROP  R6                                                               
*                                                                               
         L     RE,AIOAREA                                                       
         LA    RF,LIOS                                                          
         XCEF                                                                   
         L     R6,AIOAREA                                                       
         MVC   0(27,R6),KEY                                                     
*                                                                               
         MVC   KEY+28(4),RECADDR   CONTRACT RECORD ADDRESS                      
*                                                                               
         GOTO1 ADD                 ADD THE KEY                                  
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIOAREA                                                       
         USING REDBKEY,R6                                                       
*                                                                               
         MVI   REDBKTYP,REDBKTYQ   ADD X'0E' RECORD AS A BACKUP                 
         MVI   REDBLEN+1,34+REDBELLQ                                            
         MVI   REDBCODE,1          TO THE X'E1' KEYS                            
         MVI   REDBELLN,REDBELLQ                                                
         DROP  R6                                                               
*                                                                               
         MVC   MYSVAIO,AIO                                                      
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   AIO,MYSVAIO         RESTORE AIO BEFORE EXIT                      
*                                                                               
GENEDIX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETUPHIA:  DELETE ANY HIATUS ELEMENT FROM THE CONTRACT RECORD.              
*        ACCESS THE TYPE 15 RECORD.  IF FOUND, ADD ITS X'02' ELEMENT            
*        TO THE CONTRACT RECORD AS AN X'25' ELEMENT.                            
*                                                                               
SETUPHIA NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'25',RCONREC),0,0                
*                                  DELETE HIATUS ELEMENT, IF ANY                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'26',RCONREC),0,0                
*                                  DELETE HIATUS COMMENT, IF ANY                
         MVC   AIO,AIO1            SET IO AREA FOR X'15' RECORD                 
         L     R3,AIO                                                           
         USING RDARREC,R3                                                       
         MVI   RDARKRT,X'15'       SET REC TYPE TO 'HIATUS'                     
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SEQ # AND SUB TYPE                     
         MVC   KEY,RDARKEY         RETRIEVE HIATUS RECORD, IF ANY               
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH REC TYPE                   
         BNE   SETH0100            NO  - NO HIATUS RECORD                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(R3),(R5),RR=RELO                              
         BNZ   SETH0100            SOFT/HARD DELETED:  SKIP IT                  
         LA    R3,RDARELEM         SET A(01 ELEMENT)                            
         ZIC   RF,1(R3)                                                         
         AR    R3,RF               BUMP TO X'02' ELEMENT                        
         USING RDARHIE2,R3         SET A(HIATUS COMMENT DATES)                  
         MVI   RDARHIC2,X'25'      RESET ELEMENT CODE TO X'25'                  
         PRINT GEN                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,RDARHIC2,         X        
               =C'ADD=CODE'                                                     
         PRINT NOGEN                                                            
*                                  ADD HIATUS ELEMENT TO CONTRACT               
SETH0100 EQU   *                                                                
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHKDAREC:  CHECKS FOR TYPE OF DARE RECORD.  IF SOFT DELETE                  
*        BIT OR HARD DELETE BIT IS SET, CC IS SET TO NON-ZERO,                  
*        INDICATING THAT RECORD IS TO BE SKIPPED/NOT PROCESSED.                 
*        SOFT DELETE BIT (X'80') WILL ALWAYS BE SET WHEN HARD BIT               
*        (X'40') IS SET.  THEREFORE, ONLY SOFT BIT IS TESTED.                   
*                                                                               
CHKDAREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            RESET A(DARE RECORD IN PROCESS)              
         USING RDARREC,R2                                                       
*                                                                               
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         MVI   SKIPRECS,C'N'       SET 'SKIP RECORD' TO NO                      
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
*                                                                               
         TM    RDARDELS,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         ZIC   RF,BUYLINE#         YES - BUMP LINE NUMBER                       
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
*                                                                               
         MVI   BUYUPDAT,C'Y'       SET WRITE FLAG TO YES                        
         ZIC   RF,RDARBYLN         CHECK LENGTH                                 
         LA    RE,RDARBYOL         SET OLD ELEMENT LENGTH                       
         CR    RF,RE               COMPARE OLD VS NEW                           
         BE    CDAR0900            EQUAL: OLD LENGTH FOUND -                    
*                                     NOT KEY-DELETED:  MUST NOT                
*                                     BE SOFT DELETED.                          
CDAR0050 EQU   *                                                                
         TM    RDARBYDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   BUYUPDAT,C'N'       YES - SET WRITE FLAG TO NO                   
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0080 EQU   *                                                                
         BH    CDAR0800            SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                        THESE AREN'T SOFT-DELETED.             
CDAR0120 EQU   *                                                                
*                                                                               
*                                  ONLY BUYS WILL HAVE RDARKSRT                 
*                                     SET - STANDARD AND ORDER                  
*                                        COMMENTS WILL NOT                      
         CLI   RDARKSRT,X'30'      BUY DETAIL?                                  
         BL    CDAR0280            NO  - ORB/COMMT                              
         LA    R3,RDARELEM         YES - CHECK FOR MULTI RATES                  
         USING RDARBDEL,R3                                                      
*                                                                               
         TM    RDARBDDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
*                                                                               
CDAR0280 EQU   *                                                                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, AND                    
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
*                                                                               
         TM    RDARELDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CDAR1000                                                         
CDAR0900 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CDAR1000 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********RINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE REDARFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF8D                                                       
         EJECT                                                                  
       ++INCLUDE REDARTWA                                                       
         EJECT                                                                  
       ++INCLUDE REDARWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REDARDSECT                                                     
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
REDIRECD DSECT                                                                  
       ++INCLUDE REGENEDI                                                       
         EJECT                                                                  
RECFCD   DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
ACTAPP   EQU   15                                                               
ACTREJ   EQU   16                                                               
MYLABEL  DS    CL8                 FOR IDENTIFIER                               
SAVEROTE DS    A                   SAVE A(ROTATION FIELD)                       
COMPCON  DS    CL4                 CONTRACT # COMP/REVERSED                     
ELTBUILD DS    CL96                ELEMENT BUILD AREA                           
ELTBILD2 DS    CL64                SECOND BUILD AREA                            
SVDEMCAT DS    XL42                SAVED DEMO CATEGORIES                        
PROGNAME DS    CL34                SAVE BUY PROGRAM NAME                        
REVNUM   DS    X                   REVISION NUMBER                              
DETLFLAG DS    XL2                 TOTAL SPOT LENGTH                            
BUYUPDAT DS    CL1                 BUY WRITE FLAG                               
SKIPRECS DS    CL1                 GENERAL RECORD SKIP FLAG                     
TOTSPTUN DS    XL1                 TOTAL SPOT UNITS                             
ACTCODE  DS    CL1                 ACTION CODE:                                 
*                                     A  =  OPEN                                
*                                     R  =  REJECT                              
MISCFLAG DS    XL1                 MESSAGE FLAGS                                
*                                     X'80'  =  DAILY ORDER                     
*                                     X'40'  =  ORBIT DROPPED                   
MFMANUAL EQU   X'20'                  X'20'  =  MANUAL CHANGES STARTED          
MFEXACT  EQU   X'10'                  X'10'  =  EXACT #SPOTS MATCH              
MFCREDIT EQU   X'08'                  X'08'  =  K BUY HAS CREDIT BUYS           
MFMGLOOP EQU   X'04'                  X'04'  =  MATCH MAKEGOOD ONLY             
MFCONOK  EQU   X'01'                  X'01'  =  CONTRACT UPDATED                
*                                                                               
MISCFLG2 DS    XL1                 MORE MESSAGE FLAGS                           
MF2USRDM EQU   X'80'                  X'80'  =  TARGET=USR DEFINED DEMO         
MF20SBUY EQU   X'40'                  X'40'  =  AGENCY SENT 0 SPT BUY           
MF2NOXIT EQU   X'20'                  X'20'  =  DON'T ERROR EXIT IN             
*                                               MANUAL MATCHING                 
MF2TRADE EQU   X'10'                  X'10'  =  TRADE ORDER                     
MF2DEMCH EQU   X'08'                  X'08'  =  DEMO CATEGORY CHANGED           
MF2NODEM EQU   X'04'                  X'04'  =  NO DEMO ON AGY&REP ORD          
MF2NOADM EQU   X'02'                  X'02'  =  NO AGENCY DEMO                  
MF2NORDM EQU   X'01'                  X'01'  =  NO REP DEMO                     
APPRBUY# DS    X                   LAST BUY DISPLAYED IN APPROVAL SCRN          
AGBUYMTR DS    X                   AGENCY MAKEGOOD BUY MASTER LINE              
ORBFLAG  DS    CL1                 ORBIT PRESENCE INDICATOR                     
*                                     Y  =  ORBIT PRESENT                       
BUCKFLGS DS    CL1                 INDICATOR FOR BUCKUP ROUTINE                 
EDICTACT DS    CL3                 EDICT 'ACTION'                               
RESENT   DS    CL1                 RESENT FLAG                                  
ACTDATE  DS    XL2                 DATE FOR STAMPING                            
ACTTIME  DS    XL2                 TIME FOR STAMPING                            
VERDFLT  DS    CL1                 VERSION NUMBER (REP)                         
CONMOD#  DS    CL1                 SAVE AREA FOR MOD NUMBER                     
BUYLINE# DS    X                   BUYLINE NUMBER BEING GENERATED               
NEWVER#  DS    CL1                 NEW RECORD VERSION NUMBER                    
NEEDSCRN DS    CL1                                                              
SAVEKEY  DS    CL27                KEY SAVE AREA FOR RESTARTS                   
SAVEPROD DS    CL20                PROD NAME(S) FROM AGENCY ORDER               
SAVEEASI DS    0CL16               SAVE AREA FOR EASI CODES                     
EASIADV  DS    CL4                 CLIENT CODE: 6 CHARS ON AGENCY HDR           
EASIPROD DS    CL4                                                              
EASIEST# DS    CL4                 EST#: 3 CHARS ON AGENCY HDR                  
EASIPRD2 DS    CL4                 PIGGY/PARTNER PRODUCT                        
FLTDATES DS    CL4                 AGENCY ORDER FLIGHT DATES                    
FLTSTART DS    XL3                                                              
FLTEND   DS    XL3                                                              
SAVEBUYR DS    CL24                BUYER NAME FROM AGENCY ORDER                 
STARTDAY DS    XL1                 START DAY FOR BUY                            
ENDDAY   DS    XL1                 END DAY FOR BUY                              
ORBSTDAY DS    XL1                 ORBIT START DAY                              
ROTATDAY DS    XL1                 ROTATION START DAY                           
DELBUKTS DS    CL1                 DELETE BUCKET FLAG                           
BUCKFLAG DS    CL1                 ADD/SUBTRACT FLAG FOR BUCKETS                
TRUFLAG  DS    CL1                 GENERATE EC/SAR KEY IF 'Y'                   
FLTKEYFG DS    CL1                 GENERATE 8E AND 8D KEY IF 'Y'                
CONFLTDT DS    CL4                 ORIGINAL CONTRACT FLIGHT DATES               
NEWFLTDT DS    CL4                 NEW CONTRACT FLIGHT DATES                    
RECADDR  DS    CL4                 DISK ADDRESS OF CONTRACT RECORD              
TKOVDATE DS    XL3                 TAKEOVER DATE                                
SAVEBU$$ DS    CL4                 BUY BREAK SAVE AREA                          
SENDID   DS    CL2                                                              
INTTYPE  DS    CL1                 INPUT TYPE                                   
SPLKEYAD DS    133C                EXTENDED SPOOL AREA                          
ORDRETRN EQU   *                   INFO FOR RETURN MESSAGES                     
RETORD#  DS    CL8                                                              
RETFROM  DS    CL10                                                             
RETTO    DS    CL10                                                             
RETSTAT  DS    CL6                                                              
RETCON#  DS    CL8                                                              
RETSENDR DS    CL16                                                             
*                                                                               
*   SPOTPAK TRANSFER ELEMENT                                                    
*                                                                               
IFELCODE DS    CL2                 CODE/LENGTH = X'0830'                        
IFSPAG   DS    CL2                 SPOT AGENCY POWER CODE                       
IFSPMD   DS    CL1                 SPOT MEDIA CODE                              
IFSPCL   DS    CL3                 SPOT CLIENT CODE                             
IFSPPRD  DS    CL3                 SPOT PRODUCT CODE                            
IFSPES   DS    CL1                 SPOT ESTIMATE NUMBER                         
IFSPPP   DS    CL3                 SPOT PRODUCT PIGGY                           
IFSPP1   DS    CL1                 SPOT PRODUCT 1 SPLIT                         
IFSPP2   DS    CL1                 SPOT PRODUCT 2 SPLIT                         
IFSPL#   DS    CL1                 SPOT BUYLINE NUMBER                          
IFSPST   DS    CL5                 STATION CALL LETTERS                         
IFSPADV  DS    CL4                 REP ADVERTISER CODE                          
IFSPRD   DS    CL3                 REP PRODUCT CODE                             
IFSPDT   DS    CL3                 SPOT TRANSFER DATE                           
IFSPTM   DS    CL4                 SPOT TRANSFER TIME                           
         DS    CL12                SPARE                                        
IFELLEN  EQU   *-IFELCODE          ELEMENT LENGTH                               
*                                                                               
WORK2    DS    300C                WORK SPACE FOR BUCKET BUILD                  
*                                     AND GENERAL COMPARISONS                   
         DS    0F                                                               
BUYCOST  DS    F                   COST OF SPOTS IN BUY                         
SPOTCTR  DS    F                   NUMBER SPOTS IN BUY                          
WEEKCTR  DS    F                   NUMBER WEEKS IN BUY                          
ORDTOT$$ DS    F                   ORDER TOTAL DOLLARS                          
ORDTOTSP DS    F                   ORDER TOTAL SPOTS                            
AREJMESS DS    A                   A(NEXT REJ MESS SPACE)                       
KATZEDI  DS    C                   Y/N KATZ EDI ORDER?                          
DAILYFLG DS    C                   DAILY PACING FLAG                            
TARGETBY DS    X                                                                
TKODATE  DS    XL3                 TAKEOVER DATE                                
MYSVAIO  DS    F                                                                
*                                                                               
BUYNUM   DS    X                                                                
*                                                                               
RSTRDATE DS    XL3                 (YMD) REP BUY START DATE                     
RBUYGRID DS    XL53                BUYGRID FOR THE REP BUYLINE                  
*                                                                               
ASTRDATE DS    XL3                 (YMD) AGENCY BUY START DATE                  
ABUYGRID DS    XL53                BUYGRID FOR THE AGY BUYLINE                  
ABUYGRD2 DS    XL53                A COPY OF ABUYGRID                           
*                                                                               
SORTRECS DS    X                   NUMBER OF SORT RECORDS IN SORTAREA           
CONBUY#  DS    X                                                                
*                                                                               
VREPFACS DS    V                   VLINK FOR REPFACS                            
*                                                                               
REJMESS  DS    600C                REJECTION MESSAGE BUILD AREA                 
SORTAREA DS    XL2300                                                           
*                                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTRHDLN DS    CL8                                                              
         DS    CL1                                                              
LSTRAORN DS    CL8                                                              
         DS    CL1                                                              
LSTRSTA  DS    CL6                                                              
         DS    CL1                                                              
LSTRAGY  DS    CL5                                                              
         EJECT                                                                  
ORDAPREJ DSECT                                                                  
ARTRANID DS    CL6                                                              
ARORDNUM DS    CL8                                                              
ARFROM   DS    CL10                                                             
ARTO     DS    CL10                                                             
ARDATE   DS    CL6                                                              
ARTIME   DS    CL4                                                              
ARSTAT   DS    CL6                                                              
ARCON#   DS    CL8                                                              
ARRETSND DS    CL16                                                             
ARDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDCOM   DSECT                                                                  
OCTRANID DS    CL6                                                              
OCORDNUM DS    CL8                                                              
OCCONTIN DS    CL1                                                              
OCBUYLIN DS    CL4                                                              
OCCOMMNT DS    CL70                                                             
OCDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDTRLR  DSECT                                                                  
OTTRANID DS    CL6                                                              
OTORDNUM DS    CL8                                                              
OTCOUNT  DS    CL6                                                              
         SPACE 4                                                                
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMSVKEY  DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
* IF ANY OF THE BELOW FIELDS CHANGE, MAKE SURE TO CHANGE THE PARAMETERS         
* IN THE XSORT CALLS ABOVE                                                      
*                                                                               
SORTD    DSECT                                                                  
SRTAGYBY DS    X                   AGENCY BUY THAT K BUY IS MAPPED TO           
*                                  X'FF' MEANS K BUY ADDED MANUALLY             
SRTTKBUY DS    X                   TARGET K BUY (FOR MAKEGOOD/CREDIT)           
SRTCONBY DS    X                   K BUY                                        
SRTBUYDA DS    XL4                 CONTRACT BUY DISK ADDRESS                    
SRTFLAGS DS    X                   MISC. FLAGS                                  
SFMATCHD EQU   X'80'               K BUY WAS MATCHED                            
SFMATING EQU   X'40'               K BUY IS IN PROCESS OF MATCHING              
SFMKGOOD EQU   X'20'               K BUY IS A MAKEGOOD                          
SFCREDIT EQU   X'10'               K BUY IS A CREDIT                            
SFTMKGD  EQU   X'08'               K BUY IS TARGET MAKEGOOD                     
SFTCRDT  EQU   X'04'               K BUY IS TARGET CREDIT                       
SFADDED  EQU   X'02'               K BUY WAS ADDED AT THIS APPROVAL             
SFTKMKGD EQU   X'01'               K BUY IS A TAKEOVER MAKEGOOD                 
SRTFLG2  DS    X                                                                
SF2METH1 EQU   X'80'               AUTO METHOD 1                                
SF2METH2 EQU   X'40'               AUTO METHOD 2                                
SF2METH3 EQU   X'20'               AUTO METHOD 3                                
SF20SPTS EQU   X'10'               CONTRACT BUY HAS ZERO TOTAL SPOTS            
SF20CSTS EQU   X'08'               CONTRACT BUY HAS ZERO TOTAL DOLLARS          
SF2MGOF  EQU   X'04'               CONTRACT BUY HAS PENDING MKGD OFFERS         
SF2CAN   EQU   X'02'               CANCELLED BY 0 SPOT OVERRIDE                 
SF2MCAN  EQU   X'01'               REP HAS MANUALLY CANCELLED THE BUY           
SORTDLQ  EQU   *-SORTD                                                          
*                                                                               
SLINKD   DSECT SHOW LINK ON SCREEN                                              
SLAGYBUY DS    CL3                                                              
         DS    C                                                                
SLKBUY   DS    CL3                                                              
         DS    C                                                                
SLTKBUY  DS    CL3                                                              
         DS    C                                                                
SLFLAGS  DS    CL23                                                             
         EJECT                                                                  
T80F21   CSECT                                                                  
*                                                                               
*   BUCKUPDT: EITHER DELETE OR ADD BUCKET DOLLARS TO CONTRACT RECORD,           
*        BASED ON BUCKFLAG VALUE.                                               
*                                                                               
BUCKUPDT NMOD1 BUCKWRKX-BUCKWORK,*BUCKUP*,CLEAR=Y                               
         LR    R2,RC               SET A(BUCKWORK AREA)                         
         L     RC,0(R1)            RESET A(WORKAREA)                            
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R8,AIO3             SET A(CONTRACT RECORD)                       
         USING RCONREC,R8                                                       
*                                  SET ADDRESSES FOR REGENBUC                   
         CLI   DAILYFLG,C'Y'       DAILY PACING?                                
         BNE   BUUP0040            NO                                           
         OI    BUCKFLGS,X'08'      YES - SET DAILY PACING CALL                  
BUUP0040 EQU   *                                                                
         MVC   DMCB,AIO            A(BUYREC)                                    
         MVC   DMCB(1),BUCKFLAG                                                 
         L     R0,VRECUP                                                        
         GOTOX (RFBUCKUP,VREPFACS),DMCB,,(BUCKFLGS,RCONREC),           +        
               ACOMFACS,GETBROAD,(R0),(R2)                                      
         BNE   BUUP0100                                                         
*        BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
         GOTO1 =A(TRUDATE),DMCB,(RC),(R8),RR=RELO                               
*                                                                               
         B     ABUCEXIT                                                         
*                                                                               
BUUP0100 EQU   *                                                                
         LA    R2,CONACTH                                                       
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
ABUCEXIT DS    0H                                                               
         B     EXIT                                                             
         DROP  R5,R8                                                            
         EJECT                                                                  
BUCKWORK DSECT                                                                  
BUCKSPAC DS    2000C                 WORKAREA FOR BUCKUP ROUTINE                
BUCKWRKX EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'245REDAR21   04/12/04'                                      
         END                                                                    
