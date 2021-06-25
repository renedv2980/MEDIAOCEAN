*          DATA SET RECNT55    AT LEVEL 023 AS OF 05/01/02                      
*          DATA SET RECNT55A   AT LEVEL 021 AS OF 03/03/98                      
*PHASE T80255A,+0                                                               
         TITLE 'T80255 - REP COMBO MON DISPLAY/EDIT'                            
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT55 (T80255) --- COMBO MON DISPLAY/EDIT              *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 14JAN93 (SKU) --- ORIGINAL ROLLOUT                              *             
*                                                                 *             
* 29MAR93 (BU ) --- CREATE/UPDATE TRUE ACTIVITY DATE ELEMENT      *             
*                                                                 *             
* 28OCT94 (SKU) --- ADD ROUTINE TO SAVE REP/STA VERSION DATES     *             
*                                                                 *             
* 18JAN95 (SKU) --- CALL REGENVER TO MARK CONTRACT UNCONFIRMED    *             
*                   AND BUMP VERSION NUMBERS                      *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 29MAR96 (JRD) --- MOVE PENDING COMMENTS TO SPL COMMENTS         *             
*                                                                 *             
* 24JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 03MAR98 (BU ) --- ALTERNATE CALENDAR PROCESSING                 *             
*                                                                 *             
* 13MAR98 (BU ) --- CONVERT MON BUCKETS IN TO INVOICE BUCKETS FOR *             
*                   BACK BILLING CONTRACTS, ACC-BB IN CONBUY      *             
*                                                                 *             
* 03NOV98 (SKU) --- Y2K COMPLIANT                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80255   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T80255,R9                                        
         LR    R8,RC                                                            
         USING MYWORKD,R8                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         CLI   TWACOMBO,0          MUST BE COMBO TO BE IN HERE!                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+28(4),TWAKADDR  RETRIEVE CONTRACT                            
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BAS   RE,ALTSTAS          CHECK STATIONS FOR ALT CALS                  
*                                                                               
MON      DS    0H                                                               
         XC    CFLAG,CFLAG                                                      
         MVC   KEY+28(4),TWAKADDR  RETRIEVE CONTRACT FOR UPDATE                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         TM    MO2STRTH+4,X'20'    PRE-VALID?                                   
         BZ    MON10               NO,                                          
         BAS   RE,VALCOMBO         VALIDATE COMBO                               
*                                  MAKE SURE ALL FIELDS ARE VALID!!             
MON10    DS    0H                  SO CONTRACTS WILL UPDATE IN SYNC             
         BAS   RE,PROCOMBO         PROCESS COMBO                                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,60     ADD/DISP MON DATA                            
         TM    CFLAG,CFSCNCHG      CHANGED?                                     
         BZ    MONX                                                             
*                                                                               
         GOTO1 VDISMSG,DMCB,61     MON DATA CHANGED                             
*                                                                               
MONX     DS    0H                                                               
         B     EXXMOD              GO TO HELL                                   
         EJECT                                                                  
**********************************************************************          
* ALTSTAS:  FIND X'17' ELEMENT IN CONTRACT RECORD                               
*           RETRIEVE EACH STATION IN CONTROL ELEMENT                            
*           INSERT STATION IF ALTERNATE CALENDAR USE                            
*                                                                               
**********************************************************************          
ALTSTAS NTR1                                                                    
         XC    ALCASTAS(20),ALCASTAS                                            
         LA    R3,ALCASTAS         SET A(STATIONS FOR ALT CALS)                 
         LA    R2,RCONELEM                                                      
ALST0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE AN X'17' ELT                         
         CLI   0(R2),X'17'         COMBO CONTROL ELEMENT                        
         BE    ALST0040            YES                                          
         ZIC   RF,1(R2)                                                         
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     ALST0020            GO BACK FOR NEXT                             
ALST0040 EQU   *                                                                
         ZIC   RF,1(R2)            CALCULATE NUMBER OF STATIONS                 
         SH    RF,=H'2'            SUBTRACT CONTROL                             
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY L(STATION/CONTRACT)                
         LR    R0,RF               SET LOOP CONTROL                             
         LA    R2,2(R2)            BUMP TO 1ST STATION                          
ALST0060 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+22(5),0(R2)     INSERT STATION FROM CONTROL ELT              
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE     STATION FOUND?                               
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE THERE                          
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         BZ    *+6                 STATION RECORD MUST BE THERE                 
         DC    H'0'                                                             
         LA    RF,RSTAELEM                                                      
ALST0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ALST0120            YES                                          
         CLI   0(RF),X'08'         EXTRA DESCRIP ELT?                           
         BE    ALST0100            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     ALST0080            GO BACK FOR NEXT                             
ALST0100 EQU   *                                                                
         TM    RSTAOPTA-RSTAXXEL(RF),X'20'                                      
*                                  ALTERNATE CALENDAR STATION?                  
         BNO   ALST0120            NO  -                                        
         MVC   0(5,R3),RSTAKSTA    YES - TABLE STATION CALLS                    
         LA    R3,5(R3)            BUMP ALT CAL TABLE                           
ALST0120 EQU   *                                                                
         LA    R2,9(R2)            BUMP CONTROL ELEMENT                         
         BCT   R0,ALST0060         GO BACK FOR NEXT STATION                     
         XIT1                                                                   
*                                                                               
ALCASTAS DS    4CL5                FOUR POSSIBLE STATIONS                       
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS COMBO ORDERS                                                          
* SET MON FLAG ON FOR ALL COMPONENTS                                            
**********************************************************************          
PROCOMBO NTR1                                                                   
         MVC   SVCKEY,RCONREC      SAVE FIRST COMBO K KEY                       
         MVC   SVKNUM,TWACNUM      SAVE OFF CURRENT TWACNUM                     
         LA    R6,ALCASTAS         SET A(ALT STATION FLAGS)                     
*                                                                               
PCMB0020 MVI   CMBNDX,1            INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R2,TWACMBC1                                                      
         B     PCMB0060                                                         
*                                                                               
PCMB0040 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         ZAP   MYWORK+10(5),=P'0'                                               
         MVO   MYWORK+10(5),0(4,R2) CHANGE TO PACK WITH SIGN                    
         ZAP   MYWORK+5(5),=P'99999999'                                         
         SP    MYWORK+5(5),MYWORK+10(5) GET 9'S COMPLEMENT                      
         MVO   MYWORK(5),MYWORK+5(5) CHANGE TO PWOS                             
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,MYWORK     NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PCMB0060 DS    0H                                                               
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         BAS   RE,VALBUCK          GO VALIDATE BUCKETS                          
         BAS   RE,DISBUCK          GO DISPLAY BUCKETS                           
*                                                                               
         TM    CFLAG,CFSCNCHG      CHANGED?                                     
         BZ    PCMB0140                                                         
*                                                                               
         BAS   RE,UPDVER           UPDATE VERSION NUMBER AND UNCONFIRM          
         OI    RCONMODR+1,X'20'    SET MONTHLY DONE FLAG ON CON                 
         BAS   RE,TRUDATE          UPDATE TRUE ACTIV DATE ELEMENT               
         BAS   RE,SARCOMS                                                       
         LA    RF,ALCASTAS         ALTERNATE CALENDAR STATION?                  
         LA    RE,4                SET LOOP CONTROL                             
PCMB0080 EQU   *                                                                
         OC    0(5,RF),0(RF)       EMPTY SLOT IN TABLE?                         
         BZ    PCMB0120            YES - NO ALTERNATE CALENDAR                  
         CLC   RCONKSTA,0(RF)      CONTRACT STATION IN TABLE?                   
         BE    PCMB0100            YES                                          
         LA    RF,5(RF)            NO  - BUMP TO NEXT ENTRY                     
         BCT   RE,PCMB0080         GO BACK FOR NEXT                             
PCMB0100 EQU   *                                                                
         BAS   RE,ALTCAL           PROCESS ALTERNATE CALENDARS                  
PCMB0120 EQU   *                                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
PCMB0140 DS    0H                                                               
         ZIC   RF,CMBNDX           BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,CMBNDX                                                        
*                                                                               
         CLC   CMBNDX,TWACOMBO     WE HAVE THIS MANY COMBO K'S TO DO            
         BH    PCMB0160                                                         
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
         LA    R6,1(R6)            BUMP TO NEXT ALT STATION FLAG                
         B     PCMB0040                                                         
*                                                                               
PCMB0160 DS    0H                                                               
         MVC   KEY(L'RCONKEY),SVCKEY ALL DONE, RESTORE ORIGINAL COMBO K         
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVKNUM                                                   
*                                                                               
PCMB0180 DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE ALL MON FIELDS BEFORE PROCESSING THE CONTRACTS                       
**********************************************************************          
VALCOMBO NTR1                                                                   
         MVC   SVCKEY,RCONREC      SAVE FIRST COMBO K KEY                       
         MVC   SVKNUM,TWACNUM      SAVE OFF CURRENT TWACNUM                     
*                                                                               
VCOMBO05 MVI   CMBNDX,1            INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R2,TWACMBC1                                                      
         B     VCOMBO15                                                         
*                                                                               
VCOMBO10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         ZAP   MYWORK+10(5),=P'0'                                               
         MVO   MYWORK+10(5),0(4,R2) CHANGE TO PACK WITH SIGN                    
         ZAP   MYWORK+5(5),=P'99999999'                                         
         SP    MYWORK+5(5),MYWORK+10(5) GET 9'S COMPLEMENT                      
         MVO   MYWORK(5),MYWORK+5(5) CHANGE TO PWOS                             
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,MYWORK     NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCOMBO15 DS    0H                                                               
         BAS   RE,BLDBUCK          BUILD TOTAL BUCKETS                          
         BAS   RE,CHKBUCK          GO VALIDATE BUCKETS                          
*                                                                               
VCOMBO20 DS    0H                                                               
         ZIC   RF,CMBNDX           BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,CMBNDX                                                        
*                                                                               
         CLC   CMBNDX,TWACOMBO     WE HAVE THIS MANY COMBO K'S TO DO            
         BH    VCOMBO30                                                         
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
         B     VCOMBO10                                                         
*                                                                               
VCOMBO30 DS    0H                                                               
         MVC   KEY(L'RCONKEY),SVCKEY ALL DONE, RESTORE ORIGINAL COMBO K         
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVKNUM                                                   
*                                                                               
VCOMBOX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*                                                                 *             
* IO3 IS THE STARTING BUCKET TOTAL   (BUILT IN DISBUCK)           *             
*    AREA+0(2)=YYMM                                               *             
*    AREA+2(4)=DOLLARS                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
* BUILD TOTAL BUCKETS                                                           
BLDBUCK  NTR1                                                                   
         L     R4,AIO3                                                          
***>>>   LA    RF,L'IO3                                                         
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R4),(RF)                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   ELCODE,X'04'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   BBX                 NO BUCKETS YET                               
BB4      MVC   0(2,R4),2(R6)       SAVE BUCK YYMM                               
BB5      L     R1,6(R6)            R1=BUCK AMT                                  
         A     R1,2(R4)            ADD RUNNING TOTAL                            
         ST    R1,2(R4)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BBX                                                              
         CLC   0(2,R4),2(R6)       SAME BDCST MON?                              
         BE    BB5                                                              
         LA    R4,6(R4)            NEXT TOTAL BUCKET                            
         B     BB4                                                              
BBX      B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DON'T VALIDATE IF FIRST TIME THROUGH                                          
* READ CAREFULLY: IF NOT PREVIOUSLY VALIDATED, SKIP VALIDATE!!!                 
* REASON: THE MONTH FLDS ARE PROTECTED, BUT AREN'T ON THE SCREEN UNTIL          
*         THE FIRST DISPLAY IS DONE, AT WHICH TIME IT'S VALID BITS ARE          
*         SET.                                                                  
**********************************************************************          
VALBUCK  NTR1                                                                   
         TM    MO2STRTH+4,X'20'    PRE-VALID?                                   
         BZ    VBX                 NO - SKIP VALIDATE                           
*              GET MONDAY DATE OF CURRENT WEEK                                  
*              GET TODAY'S DATE                                                 
         GOTO1 DATCON,DMCB,(5,0),DUB                                            
*              FIND DAY OF WEEK                                                 
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=3C' '                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R6,DMCB             DAY OF WEEK                                  
         BCTR  R6,R0                                                            
         LNR   R6,R6               BACK UP TO MONDAY                            
*              GET CURRENT MONDAY                                               
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R6)                                      
*              GET 2-BYTE CURRENT MONDAY-WEEK DATE                              
         GOTO1 DATCON,DMCB,DMCB+12,(2,DATE)                                     
*                                                                               
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         LA    R4,MONTBL                                                        
         LA    R2,MO2STRTH         FIRST FLD HDR                                
VB10     DS    0H                  SET R4 TO FIRST CORRESPOND                   
         CLC   1(3,R4),8(R2)        W/FIRST MON ON SCREEN                       
         BE    VB20                                                             
         LA    R4,L'MONTBL(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   VB10                                                             
         DC    H'0'                NO MONTH?                                    
VB20     DS    0H                                                               
*                                  Y2K COMPLIANT                                
         GOTO1 DATVAL,DMCB,(2,8(R2)),OUTDATE                                    
         GOTO1 DATCON,DMCB,(0,OUTDATE),(3,BINDATE)                              
         MVC   BYEAR,BINDATE                                                    
*                                                                               
*        PACK  DUB,11(2,R2)        GET YEAR                                     
*        CVB   R1,DUB                                                           
*        STC   R1,BYEAR                                                         
*                                                                               
         ZIC   R1,CMBNDX           BUMP TO CORRESPONDING MONEY FLD              
VB25     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,VB25                                                          
*                                                                               
*                                                                               
* CK IF THIS BRDCST MON HAS A TOTAL BUCKET                                      
         CLC   0(1,R4),1(R5)       SAME BRDCAST MON?  (BIN)                     
         BE    VB30                YES - SEE IF ANY BUCKET CHANGE               
*                                                                               
* IF NO PREVIOUS VALUE, THEN SEE IF ONE NOW                                     
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VB60                NO, CK NEXT FLD                              
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   VB50                                                             
*        DC    H'0'                IF ERROR, MUST ABEND OTHERWISE               
*                                  CONTRACTS WILL BE OUT OF SYNC!               
         LA    R3,40               NON-NUMERIC INPUT FLD                        
         B     ERROR                                                            
*                                                                               
* NOW SEE IF VALUE ON SCREEN IS SAME AS IN TOTAL BUCKET                         
VB30     DS    0H                                                               
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   VB40                                                             
*        DC    H'0'                IF ERROR, MUST ABEND OTHERWISE               
*                                  CONTRACTS WILL BE OUT OF SYNC!               
         LA    R3,50               NON-NUMERIC INPUT FLD                        
         B     ERROR                                                            
VB40     CLC   DMCB+4(4),2(R5)                                                  
         BNE   VB70                BUILD NEW BUCKET                             
         CLC   =C'ACC-BB',CONBUY                                                
*                                  NEED TO CHANGE EXISTING                      
         BE    VB70                ACTIVITY DATE FOR BACK BILLING               
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB60                                                             
*                                                                               
* ADD NEW 03 (NO EXISTING 03'S FOR THIS MONTH)                                  
VB50     DS    0H                                                               
         OI    CFLAG,CFSCNCHG      SET CHANGED                                  
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'030A'                                                 
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   WORK,X'04'                                                       
         MVC   WORK+2(1),BYEAR     BROADCAST YY                                 
         MVC   WORK+3(1),0(R4)     MM                                           
         MVC   WORK+4(2),DATE      ACTIVITY MONDATE                             
         MVC   WORK+6(4),DMCB+4    BUCKET $                                     
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   VB55                                                             
*                                                                               
* FOR BACK BILLING, CHANGE ACTIVITY DATE TO THE QUARTER PREVIOUS TO             
* THE BUCKET MONTH                                                              
*                                                                               
         BAS   RE,BILLING                                                       
*                                                                               
VB55     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
*                                                                               
* LOOP BACK FOR NEXT MONTH                                                      
VB60     DS    0H                                                               
         OI    4(R2),X'20'         YES, SET PREVALID                            
*                                                                               
         LA    R1,5                                                             
         ZIC   R0,CMBNDX                                                        
         SR    R1,R0               BUMP TO BEGINNING OF NEXT LINE/              
VB65     ZIC   R0,0(R2)            BUCKET FLD                                   
         AR    R2,R0                                                            
         BCT   R1,VB65                                                          
*                                                                               
         CLI   0(R2),0             LAST FIELD?                                  
         BE    VBX                                                              
         CLI   8(R2),0             IS FIELD IN USE?                             
         BE    VBX                 NO, NO MORE FIELDS                           
         LA    R4,L'MONTBL(R4)     NEXT TBL ENTRY                               
         CLI   0(R4),X'FF'         LAST MONTH?                                  
         BNE   VB20                                                             
         LA    R4,MONTBL           BACK TO FIRST MON                            
         B     VB20                                                             
*                                                                               
* FIRST SEE IF AN 03 EL EXISTS FOR THIS BDCST MON/ACTIVITY WEEK,                
* IF NOT, JUST ADD NEW ELEMENT,                                                 
* ELSE CHANGE EXISTING EL FOR NEW TOTAL.                                        
VB70     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,3                                                         
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   ELCODE,X'04'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   VB100                                                            
*                                                                               
*                                                                               
* IF BDCST MON FOUND, SEE IF ANY ACTIVITY THIS WEEK                             
VB80     DS    0H                                                               
         CLC   2(2,R6),0(R5)       MUST BE RIGHT BDCST MON                      
         BL    VB90                                                             
         BH    VB100               NO ACTIVITY THIS WEEK FOR BDCST MON          
*                                                                               
         CLC   =C'ACC-BB',CONBUY   BACK BILLING, BACK UP                        
         BNE   VB85                ACTIVITY DATE                                
         MVC   WORK(10),0(R6)                                                   
         MVC   WORK+10(24),DMCB    SAVE OFF DMCB SINCE                          
         BAS   RE,BILLING          IT IS USED AFTER THIS BILLING CALL           
         MVC   4(2,R6),WORK+4                                                   
         MVC   DMCB(24),WORK+10                                                 
         B     VB110                                                            
*                                                                               
VB85     DS    0H                                                               
         CLC   4(2,R6),DATE        HAVE RIGHT BDCST MON,ANY ACT THIS WK         
         BE    VB110               CHANGE EXISTING 03,DON'T ADD NEW ONE         
VB90     BAS   RE,NEXTEL                                                        
         BE    VB80                                                             
*                                                                               
* NO ACTIVITY FOR THIS BROADCAST MONTH THIS WEEK                                
VB100    L     R0,DMCB+4           $ IN FLD                                     
         S     R0,2(R5)                                                         
         ST    R0,DMCB+4                                                        
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB50                JUST ADD NEW 03                              
* CHANGE EXISTING 03 OR 04                                                      
VB110    DS    0H                                                               
         OI    CFLAG,CFSCNCHG      SET CHANGED                                  
         L     R1,2(R5)            CURRENT TOT BUCKET                           
         S     R1,6(R6)            SUBTRACT THIS WEEKS ACTIVITY                 
         L     R0,DMCB+4           NEW MONTH TOTAL                              
         SR    R0,R1                                                            
         ST    R0,6(R6)                                                         
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     VB60                                                             
*                                                                               
VBX      B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* THIS IS ACTUALLY A COPY OF VALBUCK BUT WITHOUT THE SCREEN AND FILE            
* UPDATES.  THIS IS TO ENSURE THAT ALL FIELDS ARE VALID BEFORE WE               
* PROCESS THEM SO THE CONTRACTS WON'T BE UPDATED OUT OF SYNC.                   
**********************************************************************          
CHKBUCK  NTR1                                                                   
*              GET MONDAY DATE OF CURRENT WEEK                                  
*              GET TODAY'S DATE                                                 
         GOTO1 DATCON,DMCB,(5,0),DUB                                            
*              FIND DAY OF WEEK                                                 
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=3C' '                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R6,DMCB             DAY OF WEEK                                  
         BCTR  R6,R0                                                            
         LNR   R6,R6               BACK UP TO MONDAY                            
*              GET CURRENT MONDAY                                               
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R6)                                      
*              GET 2-BYTE CURRENT MONDAY-WEEK DATE                              
         GOTO1 DATCON,DMCB,DMCB+12,(2,DATE)                                     
*                                                                               
         L     R5,AIO3             R5 TO TOTAL BUCKETS                          
         LA    R4,MONTBL                                                        
         LA    R2,MO2STRTH         FIRST FLD HDR                                
CB10     DS    0H                  SET R4 TO FIRST CORRESPOND                   
         CLC   1(3,R4),8(R2)        W/FIRST MON ON SCREEN                       
         BE    CB20                                                             
         LA    R4,L'MONTBL(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   CB10                                                             
         DC    H'0'                NO MONTH?                                    
CB20     DS    0H                                                               
         PACK  DUB,11(2,R2)        GET YEAR                                     
         CVB   R1,DUB                                                           
         STC   R1,BYEAR                                                         
*                                                                               
         ZIC   R1,CMBNDX           BUMP TO CORRESPONDING MONEY FLD              
CB25     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,CB25                                                          
*                                                                               
*                                                                               
* CK IF THIS BRDCST MON HAS A TOTAL BUCKET                                      
         CLC   0(1,R4),1(R5)       SAME BRDCAST MON?  (BIN)                     
         BE    CB30                YES - SEE IF ANY BUCKET CHANGE               
*                                                                               
* IF NO PREVIOUS VALUE, THEN SEE IF ONE NOW                                     
         CLI   5(R2),0             ANY INPUT?                                   
         BE    CB60                NO, CK NEXT FLD                              
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   CB50                                                             
         LA    R3,3                NON-NUMERIC INPUT FLD                        
         B     ERROR                                                            
*                                                                               
* NOW SEE IF VALUE ON SCREEN IS SAME AS IN TOTAL BUCKET                         
CB30     DS    0H                                                               
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BNE   CB40                                                             
         LA    R3,3                NON-NUMERIC INPUT FLD                        
         B     ERROR                                                            
*                                                                               
CB40     CLC   DMCB+4(4),2(R5)                                                  
         BNE   CB70                                                             
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     CB60                                                             
*                                                                               
* NO EXISTING 03'S FOR THIS MONTH                                               
CB50     DS    0H                                                               
         OI    CFLAG,CFSCNCHG      SET CHANGED                                  
*                                                                               
* LOOP BACK FOR NEXT MONTH                                                      
CB60     DS    0H                                                               
         LA    R1,5                                                             
         ZIC   R0,CMBNDX                                                        
         SR    R1,R0               BUMP TO BEGINNING OF NEXT LINE/              
CB65     ZIC   R0,0(R2)            BUCKET FLD                                   
         AR    R2,R0                                                            
         BCT   R1,CB65                                                          
*                                                                               
         CLI   0(R2),0             LAST FIELD?                                  
         BE    CBX                                                              
         CLI   8(R2),0             IS FIELD IN USE?                             
         BE    CBX                 NO, NO MORE FIELDS                           
         LA    R4,L'MONTBL(R4)     NEXT TBL ENTRY                               
         CLI   0(R4),X'FF'         LAST MONTH?                                  
         BNE   CB20                                                             
         LA    R4,MONTBL           BACK TO FIRST MON                            
         B     CB20                                                             
*                                                                               
* FIRST SEE IF AN 03 EL EXISTS FOR THIS BDCST MON/ACTIVITY WEEK,                
* IF NOT, JUST ADD NEW ELEMENT,                                                 
* ELSE CHANGE EXISTING EL FOR NEW TOTAL.                                        
CB70     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,3                                                         
* IF BACK BILLING, CONVERT TO INVOICE BUCKETS                                   
         CLC   =C'ACC-BB',CONBUY                                                
         BNE   *+8                                                              
         MVI   ELCODE,X'04'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* IF BDCST MON FOUND, SEE IF ANY ACTIVITY THIS WEEK                             
CB80     DS    0H                                                               
         CLC   2(2,R6),0(R5)       MUST BE RIGHT BDCST MON                      
         BL    CB90                                                             
         BH    CB100               NO ACTIVITY THIS WEEK FOR BDCST MON          
         CLC   4(2,R6),DATE        HAVE RIGHT BDCST MON,ANY ACT THIS WK         
         BE    CB110               CHANGE EXISTING 03,DON'T ADD NEW ONE         
CB90     BAS   RE,NEXTEL                                                        
         BE    CB80                                                             
*                                                                               
* NO ACTIVITY FOR THIS BROADCAST MONTH THIS WEEK                                
CB100    L     R0,DMCB+4           $ IN FLD                                     
         S     R0,2(R5)                                                         
         ST    R0,DMCB+4                                                        
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     CB50                JUST ADD NEW 03                              
* CHANGE EXISTING 03                                                            
CB110    DS    0H                                                               
         OI    CFLAG,CFSCNCHG      SET CHANGED                                  
         L     R1,2(R5)            CURRENT TOT BUCKET                           
         S     R1,6(R6)            SUBTRACT THIS WEEKS ACTIVITY                 
         L     R0,DMCB+4           NEW MONTH TOTAL                              
         SR    R0,R1                                                            
         ST    R0,6(R6)                                                         
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     CB60                                                             
*                                                                               
CBX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY BUCKETS                                                               
***********************************************************************         
DISBUCK  NTR1                                                                   
         TM    CFLAG,CFSCNCHG      HAVE ANY BUCKETS CHANGED?                    
         BZ    *+8                                                              
         BAS   RE,BLDBUCK          IF SO, REBUILD TOTAL BUCKETS                 
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
* WORK HAS EBCDIC START AND WORK+6 HAS END DATE                                 
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,TEMP+6),(20,STRTDATE)  K START DATE               
         GOTO1 DATCON,DMCB,(0,TEMP+18),(20,ENDDATE)  K END DATE                 
*                                                                               
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
* WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END                  
* BROADCAST MONTH AND YEAR (YMD - BINARY)                                       
*                                                                               
         L     R5,AIO3             R5 TO OLD TOTAL BUCKETS                      
         MVC   YEAR,STRTDATE+2     K START YEAR                                 
         MVC   BYEAR,WORK          SAVE BIN YEAR                                
         ZIC   R4,WORK+1           START MON                                    
         ZIC   R3,WORK+4           END MON                                      
         CLC   WORK(1),WORK+3      SAME YEAR?                                   
         BE    *+8                                                              
         LA    R3,12(R3)           ADJUST FOR NEXT YEAR                         
         SR    R3,R4                                                            
         LA    R3,1(R3)            NUM MONTHS                                   
         BCTR  R4,0                MONTHS ARE 0 RELATIVE                        
         MH    R4,=Y(L'MONTBL)     R4 NOW AT STARTING MONTH                     
         LA    R4,MONTBL(R4)       IT HELPS TO POINT IT AT THE TABLE            
         LA    R2,MO2STRTH         FIRST FLD HDR                                
DB10     DS    0H                                                               
         MVC   8(3,R2),1(R4)       MMM                                          
         MVI   11(R2),C'/'                                                      
         MVC   12(2,R2),YEAR       YY                                           
*                                                                               
         CLC   CMBNDX,TWACOMBO     IF ALL COMBO DISPLAYED                       
         BNE   DB13                                                             
         OI    4(R2),X'20'         SET PRE-VALID ON MON FLD                     
*                                                                               
DB13     ZIC   R1,CMBNDX                                                        
DB15     ZIC   R0,0(R2)            BUMP TO CORRESPONDING MONEY FLD              
         AR    R2,R0                                                            
         BCT   R1,DB15                                                          
*                                                                               
         OI    4(R2),X'20'         SET PREVALID BIT                             
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT                             
*                                                                               
* DISPLAY $ (IF ANY)                                                            
*                                                                               
         CLC   0(1,R5),BYEAR       THIS YEAR?                                   
         BNE   DB20                                                             
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   DB20                                                             
         EDIT  (4,2(R5)),(10,8(R2)),2                                           
         OI    6(R2),X'80'         XMIT FLD                                     
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
*                                                                               
DB20     DS    0H                                                               
         LA    R1,5                                                             
         ZIC   R0,CMBNDX                                                        
         SR    R1,R0               BUMP TO BEGINNING OF NEXT LINE/              
DB25     ZIC   R0,0(R2)            BUCKET FLD                                   
         AR    R2,R0                                                            
         BCT   R1,DB25                                                          
*                                                                               
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   DB30                NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   YEAR,ENDDATE+2      K END YEAR (EBCDIC)                          
         ZIC   R1,BYEAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYEAR                                                         
DB30     BCT   R3,DB10                                                          
DBX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* UPDATE VERSION NUMBER AND UNCONFIRM CONTRACT AND DISPLAY NEW VER NO           
UPDVER   NTR1                                                                   
         CLC   RCONMODD,TODAY                                                   
         BE    *+8                                                              
         OI    TAREQ,X'01'                                                      
*         MVC   RCONMODD,TODAY                                                  
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    UV10                                                             
         DROP  R6                                                               
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
                                                                                
UV10     DS    0H                                                               
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    UV20                                                             
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
UV20     OI    RCONCONF,X'80'      NOT CONFIRMED                                
*                                                                               
* NOW DISPLAY NEW VERSION NUMBER                                                
         FOUT  CONMODH,MYSPACES,11                                              
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    UV50                                                             
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUM.                     
         BO    UV50                                                             
         SPACE 1                                                                
         ZIC   R3,1(R6)                                                         
         AR    R6,R3               GET NEXT ELEMENT                             
         SPACE 1                                                                
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    UV25                                                             
         DC    H'0',C'MISSING X20 - SEND ELEM'                                  
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
UV25     CLC   RCONSRV,RCONSSV                                                  
         BH    UV30                                                             
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     UV40                                                             
UV30     EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
UV40     MVC   CONMOD(7),=C'WIP VER'                                            
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    XIT                                                              
         MVC   CONMOD(7),=C'ESL VER'                                            
         DROP  R6                                                               
         B     XIT                                                              
         SPACE 1                                                                
UV50     CLI   RCONMOD,0           K MOD NUM                                    
         BE    XIT                                                              
         MVC   CONMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,CONMOD+8),ALIGN=LEFT                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
TRUDATE  NTR1                                                                   
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
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* ALTCAL  - SETS ALTERNATE CALENDAR BUCKETS, IF NEEDED              *           
*********************************************************************           
ALTCAL   NTR1                                                                   
*                                  DROP X'53'/X'54' ELTS                        
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS FOR MON             
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS FOR MON             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
*                                                                               
         CLC   =C'ACC-BB',CONBUY   BACK BILLING?                                
         BNE   *+8                 NO                                           
         MVI   ELCODE,X'04'        YES - COPY X'04' -> X'54'                    
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   ALCA0060            NO BUCKETS AT ALL                            
         B     ALCA0040            PROCESS BUCKET FOUND                         
ALCA0020 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   ALCA0060            BUCKETS FINISHED                             
ALCA0040 EQU   *                                                                
         MVC   WORK(10),0(R6)      MOVE BUCKET TO WORK AREA                     
         OI    WORK,X'50'          SET ALTERNATE CALENDAR BUCKET                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
*                                  ADD BUCKET TO CONTRACT                       
         B     ALCA0020            GO BACK FOR NEXT BUCKET                      
ALCA0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ROUTINE TO SET ACTIVITY DATE TO PREVIOUS QUARTER FOR BACK BILLING             
* WORK HAS EITHER THE X'03' OR X'04' BUCKET ELEMENT                             
*                                                                               
BILLING  NTR1                                                                   
         XC    WORK3,WORK3         MONTH OF BUCKET DICTATES                     
         MVC   WORK2(2),WORK+2     WHICH MONTH TO BACK UP                       
         MVI   WORK2+2,15          THIS WILL ENSURE CORRECT MONTH               
         GOTO1 DATCON,DMCB,(3,WORK2),(0,WORK3)                                  
         XC    WORK2,WORK2                                                      
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,WORK3),WORK3+6,GETDAY,ADDAY                     
         GOTO1 DATCON,DMCB,(0,WORK3+12),(5,WORK3+18)                            
         MVC   WORK3+21(2),=C'15'                                               
         MVC   WORK3+26(6),=C'(- M)'                                            
*                                                                               
         TM    PROFILES+CNTBBILB,CNTBBILA                                       
         BZ    BILL05              PROFILE TO GO GET LAST MONTH                 
         MVI   WORK3+28,C'1'       INSTEAD OF LAST QUARTER?                     
         B     BILL25                                                           
*                                                                               
BILL05   DS    0H                  FIND BROADCAST MONTH LAST QUARTER            
         LA    R3,QTABLE           GET FIRST MONDAY OF PREVIOUS QUARTER         
BILL10   CLC   WORK3+18(3),0(R3)                                                
         BE    BILL20                                                           
         LA    R3,L'QTABLE(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   BILL10                                                           
         LA    R3,579                                                           
         B     ERROR                                                            
*                                                                               
BILL20   DS    0H                                                               
         MVC   WORK3+28(1),3(R3)                                                
*                                                                               
BILL25   DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         LA    R3,579                                                           
         GOTO1 (RF),DMCB,(14,WORK3+18),(0,WORK2)                                
         CLI   DMCB+4,1                                                         
         BE    ERROR                                                            
*                                  FIND START DATE OF BROADCAST MONTH           
PDATED   USING PERVALD,WORK2                                                    
         GOTO1 VGTBROAD,DMCB,(1,PDATED.PVALESTA),WORK3,GETDAY,ADDAY             
         DROP  PDATED              MOVE COMPRESSED DATE                         
*                                                                               
BILL30   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK3),(2,WORK+4)                                 
*                                                                               
         B     XIT                                                              
*                                                                               
QTABLE   DS    0CL4                                                             
         DC    C'JAN',C'3'                                                      
         DC    C'FEB',C'4'                                                      
         DC    C'MAR',C'5'                                                      
         DC    C'APR',C'3'                                                      
         DC    C'MAY',C'4'                                                      
         DC    C'JUN',C'5'                                                      
         DC    C'JUL',C'3'                                                      
         DC    C'AUG',C'4'                                                      
         DC    C'SEP',C'5'                                                      
         DC    C'OCT',C'3'                                                      
         DC    C'NOV',C'4'                                                      
         DC    C'DEC',C'5'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*********************************************************************           
* SARCOMS - CHANGES PENDING COMMENTS TO SPL COMMENTS                *           
*********************************************************************           
SARCOMS  NTR1                                                                   
SARCM2   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   SARCMX                                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
*                                                                               
         LA    R6,RCONELEM                                                      
SARCM4   CLI   0(R6),0                                                          
         BE    SARCM6                                                           
         CLI   0(R6),X'07'                                                      
         BH    SARCM6                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     SARCM4                                                           
*                                                                               
SARCM6   MVI   WORK2,X'07'                                                      
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),WORK2,(R6)                            
         B     SARCM2                                                           
*                                                                               
SARCMX   B     XIT                                                              
       EJECT                                                                    
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
XIT      XIT1                                                                   
         DROP  R7                                                               
*                                                                               
MONTBL   DS    0CL4                                                             
         DC    X'01',C'JAN'                                                     
         DC    X'02',C'FEB'                                                     
         DC    X'03',C'MAR'                                                     
         DC    X'04',C'APR'                                                     
         DC    X'05',C'MAY'                                                     
         DC    X'06',C'JUN'                                                     
         DC    X'07',C'JUL'                                                     
         DC    X'08',C'AUG'                                                     
         DC    X'09',C'SEP'                                                     
         DC    X'0A',C'OCT'                                                     
         DC    X'0B',C'NOV'                                                     
         DC    X'0C',C'DEC'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE2D                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
MYWORKD  DSECT                                                                  
YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                         
DATE     DS    CL2                 EBCDIC - FOR DISPLAY                         
BYEAR    DS    X                   BINARY YEAR                                  
*                                                                               
CFLAG    DS    X                   CHANGED FLAG                                 
CFSCNCHG EQU   X'80'               SCREEN CHANGED                               
*                                                                               
CMBNDX   DS    XL1                 COMBO COUNTER                                
MYWORK   DS    XL20                                                             
SVCKEY   DS    CL27                SAVED CONTRACT KEY                           
SVKNUM   DS    F                   SAVED ORIGINAL TWACNUM                       
SVCMB17  DS    XL36                SAVED 17 ELEMENT                             
STRTDATE DS    CL8                 YYYYMMDD DATCON TYPE 20                      
ENDDATE  DS    CL8                 YYYYMMDD DATCON TYPE 20                      
OUTDATE  DS    CL6                                                              
BINDATE  DS    XL3                                                              
MYWORKX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023RECNT55   05/01/02'                                      
         END                                                                    
