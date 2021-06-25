*          DATA SET RECNT38    AT LEVEL 056 AS OF 06/04/03                      
*PHASE T80238A                                                                  
*INCLUDE REFLSCAN                                                               
*INCLUDE RETIMVAL                                                               
         TITLE 'MULTIPLE BUY - EDIT - COMBO ORDERS - T80238'                    
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT38 (T80238) --- MULTIPLE BUY - EDIT - COMBO ORDERS      *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* SEE RECNTHIST FOR PAST HISTORY                                      *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
* 04JUN03 HQ  FIX END DATE BUG                                        *         
* 01JUN00 BU  TRADE PROCESSING                                        *         
* 01JUN00 BU  TRADE PROCESSING                                        *         
* 18APR00 SKU INCREASE REP TO SPOT TRANSFER LIMIT FROM 50 TO 169      *         
* 03FEB98 RHV DISABLE MCI PROCESSING                                  *         
* 07NOV97 JRD ALTERNATE CALENDAR BUCKETS                              *         
* 24JUL97 SKU 4K CONTRACT SUPPORT                                     *         
* 14JUL97 SKU ALLOW CANCEL FOR TRANSFERRED NETWORK BUYS               *         
* 09DEC96 SKU LENGTH MUST BE LESS THAN 180                            *         
* 03OCT96 SKU SUPPORT LOW POWER STATION                               *         
* 10SEP96 SKU FIX CC END TIME BUG                                     *         
* 05SEP96 SKU DON'T DIE ON SEND ELEMENT NOT FOUND                     *         
* 18JUN96 SKU ENABLE MCI CAN                                          *         
*             FIX OOWR BUG IN MBI/MCI                                 *         
* 02JAN95 SKU MOVE MULTBUY DSECT AREA FROM SPOOLAR UP TO IO4          *         
*                                                                     *         
***********************************************************************         
* NOTE: THIS OVERLAY USES IO4 AND SPOOLAR AREA TO BUILD MULTIPLE BUYS           
* NOTE: THE MULTBUY DSECT COVERS IO4 AND SPOOLAR AREA                           
* NOTE: DO NOT USE IO4 FOR ANYTHING ELSE IN THIS MODULE                         
***********************************************************************         
*                                                                               
T80238   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80238,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         CLC   =C'MBI',CBIACT                                                   
         BE    MADD0000                                                         
         DC    H'0'                                                             
         SPACE                                                                  
MADD0000 DS    0H                                                               
         L     R8,AIO4             COVER WORKING STORAGE STARTING               
         USING MULTBUY,R8          AT IO4+SPOOLAR WITH SPECIAL DSECT            
         ST    R5,RELO                                                          
*                                                                               
         L     R2,4(R1)            CHECK REQUEST TYPE                           
         CLC   =C'DISP',0(R2)      DISPLAY REQUEST?                             
         BNE   MADD0010            NO                                           
*                                                                               
         GOTO1 =A(DISPSTAS),DMCB,(RC),RR=RELO                                   
         LA    R2,CBIACTH                                                       
         B     EXXMOD                                                           
MADD0010 EQU   *                                                                
         LA    R2,CBIACTH          READ CONTRACT IN LOCKED FOR                  
         LA    R3,BACERR           UPDATE AND TEST FOR DISPLAY                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  ONLY STATUS (01 BIT) ON CONTRACT             
         DROP  RF                                                               
*                                                                               
         MVI   UPDATE,YES                                                       
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     RF,AIO3                                                          
         TM    29(RF),X'01'                                                     
         BO    ERROR                                                            
         GOTO1 VMOVEREC,(R1),AIO3,RCONREC                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
*                                                                               
         GOTOX (RFVALTCL,VREPFACS),DMCB,(0,RCONREC),VGTBROAD,0,WORK             
         BE    *+16                                                             
         LA    R2,CONDTESH                                                      
         L     R3,0(R1)                                                         
         B     ERROR                                                            
*                                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE CONTRACT?                              
         BNO   MADD0005            NO                                           
         OI    BUYFLAGS,X'80'      YES - TURN ON INDICATOR                      
         DROP  R6                                                               
MADD0005 EQU   *                                                                
         MVC   CONKSTA,RCONKSTA    SAVE STATION OF ORIGINAL ORDER               
         LA    R3,199              INVALID FOR NON-RADIO STATIONS               
         CLI   RCONKSTA+4,C'A'     RADIO BAND?                                  
         BE    MADD0020                                                         
         CLI   RCONKSTA+4,C'F'                                                  
         BE    MADD0020                                                         
         CLI   RCONKSTA+4,C'C'                                                  
         BNE   ERROR                                                            
         SPACE 1                                                                
MADD0020 LA    R3,TYPBERR          PROHIBIT BUYS ON TYPE B                      
         CLI   RCONTYPE,C'B'        OR CANCELLED CONTRACTS.                     
         BE    ERROR                                                            
         SPACE 1                                                                
         CLC   CONADV(3),=C'GEN'   NO BUYS ON GENERAL AVAILS                    
         BNE   MADD0030            I.E. ADV=GEN, CATEGORY=ZZ                    
         CLC   CONCAT,=C'ZZ'                                                    
         BNE   MADD0030                                                         
         LA    R3,192              NO BUYING ON GENERAL AVAILS                  
         B     ERROR                                                            
         SPACE 1                                                                
MADD0030 DS    0H                                                               
         LA    R5,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   MADD0040                                                         
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN'                                              
         BNE   MADD0040                                                         
         B     ERROR               CANCELLED CONTRACT                           
         DROP  R6                                                               
         SPACE                                                                  
MADD0040 EQU   *                   REQUIRE BOP FOR NEW CONTRACTS                
*                                                                               
*   RETRIEVE AND SAVE COMBO CONTROL ELEMENT FROM CONTRACT RECORD.               
*      ABORT IF NOT FOUND, BECAUSE WE SHOULDN'T EVEN BE IN THIS                 
*      OVERLAY.                                                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        COMBO CONTROL ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                MUST BE FOUND                                
         ZIC   R2,1(R6)            SAVE IT BY LENGTH                            
         BCTR  R2,0                DECREMENT FOR EX                             
         LA    R3,COMBOCTL         A(STORAGE AREA)                              
         EX    R2,MADD0050                                                      
         B     MADD0060                                                         
MADD0050 MVC   0(0,R3),0(R6)       SAVE IT                                      
*                                                                               
MADD0060 EQU   *                                                                
*                                                                               
*  CALCULATE # OF CONTRACTS IN COMBO BY DIVIDING THE LENGTH OF THE              
*    COMBO CONTROL ELEMENT (MINUS ELEMENT CODE AND LENGTH BYTES)                
*    BY LENGTH OF ONE COMBO CONTRACT ENTRY                                      
*                                                                               
         ZIC   R3,1(R6)            L(COMBO CONTROL ELEMENT)                     
         SR    R2,R2               EVEN REGISTER OF PAIR                        
         BCTR  R3,0                SUBTRACT L(ELEMENT CODE BYTE)                
         BCTR  R3,0                SUBTRACT L(ELEMENT LEN  BYTE)                
         LA    R1,9                L(CONTROL ELEMENT)                           
         DR    R2,R1               DIVIDED LEN BY SIZE TO CALCULATE             
         STC   R3,CONCTR              NUMBER OF ENTRIES                         
*                                                                               
         CLC   CBIACT(4),=C'MBIX'  SKIP BOP CHECK FOR SPECIAL                   
         BE    MADD0080            OVERRIDE ACTION                              
         SPACE                                                                  
         XC    KEY,KEY             BUILD KEY FOR STATION RECORD                 
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    MADD0070                                                         
         DC    H'0',C'MISSING STA REC'                                          
MADD0070 GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    RSTASTAT,X'80'      TEST FOR BOP CHECK OVERRIDE                  
         BO    MADD0080            SKIP CHECK.                                  
         SPACE                                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    MADD0080            CONTRACT HAS BOP                             
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=Y                                       
         BZ    MADD0080                                                         
         LA    R2,CBIACTH                                                       
         LA    R3,BOPERR                                                        
         B     ERROR                                                            
         SPACE                                                                  
MADD0080 MVC   DMCB+4(4),=X'D9000A03'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VDAYVAL,DMCB        FIND ADDRESSES OF CORE-RESIDENT              
*         MVI   DMCB+7,X'0E'        MODULES NEEDED IN THIS OVERLAY              
*         BASR  RE,RF                                                           
*         MVC   VTIMVAL,DMCB                                                    
         SPACE 2                                                                
* EDIT OPTIONAL FLIGHT NUMBER FIELD                                             
*                                                                               
FLNUM    LA    R2,CBIFLNH                                                       
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    FLT                 NON-FLIGHT BUY(S)                            
         GOTO1 VPACK                                                            
         LA    R3,INVERR                                                        
         LTR   R0,R0               TEST FOR ZERO INPUT                          
         BZ    ERROR                                                            
         CH    R0,=H'99'           LARGEST FLIGHT NUMBER IS 99                  
         BH    ERROR                                                            
         STC   R0,FLTNUM                                                        
         SPACE 2                                                                
* EDIT FLIGHT FIELD                                                             
*                                                                               
FLT      LA    R2,CBIFLTH                                                       
         CLI   5(R2),0             TEST FOR MISSING INPUT                       
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
*  EDIT THE 2 FLIGHT FIELDS AS 1 LONG FIELD                                     
         SPACE 1                                                                
         MVC   WORK3(L'CBIFLT+8),CBIFLTH                                        
         SR    RE,RE                                                            
         IC    RE,CBIFLTH+5        LENGTH OF FIELD 1                            
         SR    RF,RF                                                            
         IC    RF,CBIFLT2H+5       LENGTH OF FIELD 2                            
         LA    R4,WORK3+8(RE)      END OF FIELD 1                               
         LTR   RF,RF                                                            
         BZ    FLT1                                                             
         SPACE 1                                                                
         LA    R3,WORK3+7(RE)                                                   
         CLI   0(R3),C'*'          LAST CHAR. ON LINE 1 MUST BE COMMA           
         BE    *+12                IF THERE'S A LINE 2                          
         LA    R3,15               NEED COMMA AT END OF LINE                    
         B     ERROR                                                            
         SPACE 1                                                                
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),CBIFLT2     FIELD 2                                      
         LA    RE,1(RF,RE)         NEW LENGTH                                   
         STC   RE,WORK3+5                                                       
         SPACE 1                                                                
FLT1     DS    0H                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,CONST,0,DUB                       
         L     R3,ACOMFACS                                                      
         GOTO1 =V(REFLSCAN),(R1),(R3),CONST,WORK3,ENTRIES,RR=RELO               
         CLI   DMCB,0                                                           
         BE    FLT2                NO ERRORS FOUND                              
         ZIC   R3,DMCB                                                          
         B     ERROR                                                            
         SPACE                                                                  
FLT2     MVC   NUMFLTS,DMCB+5      NUMBER OF FLIGHT DATE ENTRIES                
         GOTO1 GETDAY,(R1),CONST,DMCB+8                                         
         MVC   CSTDAY,DMCB         DAY OF CONTRACT START                        
         GOTO1 (RF),(R1),CONEND,DMCB+8                                          
         MVC   CENDAY,DMCB         DAY OF CONTRACT END                          
         SPACE 2                                                                
SETUP    EQU   *                                                                
         XC    STACTR,STACTR       ZERO OUT STATION COUNTER                     
         LA    R4,BUYLINE          POINT R4 AT RECORD AREA                      
BUYL     USING RBUYKEY,R4                                                       
         MVI   BUYL.RBUYKTYP,X'0B'                                              
         MVC   BUYL.RBUYKREP,REPALPHA REP CODE                                  
*                                                                               
*   1ST BUYLINE IS SET UP WITH CONTRACT NUMBER OF 1ST STATION                   
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
*                                                                               
*   CONTRACT NUMBER (9'S COMP) OF PRIMARY STATION IS INSERTED                   
*                                                                               
         USING TWAWORK,RF                                                       
         MVC   BUYL.RBUYKCON,TWACNUM CONTRACT NUM                               
         DROP  RF                                                               
         MVC   BUYL.RBUYKPLN,=3X'FF' NON-PLAN BUY                               
         MVC   BUYL.RBUYLEN,=H'77' L' KEY AREA PLUS FIRST ELEMENT               
NEXTSTA  EQU   *                                                                
         ZIC   RF,STACTR           INCREMENT STATION COUNTER                    
         LA    RF,1(RF)                                                         
         STC   RF,STACTR           SAVE NEW VALUE                               
         CLC   STACTR,CONCTR       ALL COMBO CONTRACTS PROCESSED?               
         BH    NEXTLIN             YES - TRY TO OUTPUT RECORDS                  
         LA    R2,CBIDAY1H         POINT R2 TO FIRST LINE: START                
         ST    R2,THISFLG1                                                      
         ST    R2,THISLINE                                                      
         CLI   5(R2),0             DO NOT ENTER SCAN                            
         BNE   SCAN                UNLESS THE FIRST DAY FIELD                   
         LA    R3,MISINP           CONTAINS DATA AND WILL NOT BE                
         B     ERROR               VIEWED AS A BLANK LINE ENDING SCAN.          
         EJECT                                                                  
* SCAN A SCREEN LINE TO SEE IF IT IS DEVOID OF INPUT.  IF SO                    
* CHECK REST OF SCREEN FOR ANY INPUT.  IF THE SCAN DOES NOT                     
* FIND ANY INPUT, THE SCREEN EDIT IS DONE.                                      
*                                                                               
SCAN     SR    R1,R1               CLEAR R1                                     
         LA    RE,CBIENDH          TEST TO SEE IF END OF SCREEN                 
         L     R2,THISFLG1         A(START OF CURRENT LINE)                     
         CR    R2,RE               HAS BEEN REACHED                             
         BNL   NEXTSTA             YES - MORE CON#S IN ORDER?                   
         LA    RF,LINELEN(R2)      NO  - POINT RF TO NEXT LINE                  
         L     R2,THISLINE         A(CURRENT LINE DATE FIELD)                   
         SPACE                                                                  
SCAN2    CLI   5(R2),0                                                          
         BNE   FRSTEL              INPUT IN ONE FIELD-GO AND EDIT               
         IC    R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT HEADER                         
         CR    R2,RF               LOOP UNTIL NEXT LINE IS REACHED              
         BL    SCAN2                                                            
         SPACE                                                                  
SCAN4    EQU   *                   BLANK LN FOUND-LOOK AT REST OF SCRN          
         CLI   5(R2),0                                                          
         BNE   SCANR               ERROR-INPUT FOUND                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,RE               STOP WHEN END IS REACHED                     
         BL    SCAN4                                                            
         B     NEXTSTA             NOTHING ON REMAINDER OF SCREEN               
         SPACE                                                                  
SCANR    DS    0H                                                               
         LA    R3,345              NO INPUT MAY FOLLOW BLANK LINE               
         B     ERROR                                                            
         SPACE 2                                                                
* PREPARE TO BUILD FIRST ELEMENT AND EDIT NECESSARY FIELDS                      
*                                                                               
FRSTEL   L     R2,THISLINE         POINT R2 TO DAY HEADER                       
         ST    R4,THISBUY                                                       
         OC    LASTBUY,LASTBUY     IS THIS FIRST LINE                           
         BZ    FRSTEL2             YES                                          
         L     R5,LASTBUY                                                       
         MVC   BUYL.RBUYKEY(25),0(R5) TRANSFER KEY AREA FROM LAST REC           
         MVC   BUYL.RBUYLEN,=H'77'                                              
         CLI   STACTR,1            1ST CONTRACT OF ORDER?                       
         BE    FRSTEL2             YES -                                        
*                                                                               
*  NOT 1ST CONTRACT OF ORDER:  MUST RESET KEY TO APPROPRIATE                    
*    CONTRACT NUMBER FROM CONTRACT CONTROL ELEMENT                              
*                                                                               
         GOTO1 =A(NEWCONNO),DMCB,(RC),(R8),RR=RELO                              
         MVC   BUYL.RBUYKCON,WORK2 INSERT NEW CONTRACT NUMBER                   
         SPACE                                                                  
FRSTEL2  MVI   BUYL.RBUYCODE,X'01'                                              
         MVI   BUYL.RBUYELLN,X'2B'                                              
         MVC   BUYL.RBUYFLT,FLTNUM                                              
         B     DAYED                                                            
         SPACE 2                                                                
         SPACE 4                                                                
* EDIT DAY FIELD                                                                
*                                                                               
DAYED    XC    WORK2,WORK2         CLEAR AREA WHERE X'02' ELEMENT               
         LA    R7,WORK2            WILL BE BUILT.                               
         USING RBUYDYEL,R7         POINT R7 AT X'02' ELEMENT                    
         MVC   RBUYDYCD(2),=X'0209'                                             
         SPACE                                                                  
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   DAYED2              EDIT FIELD                                   
         SPACE                                                                  
DAYED1   OC    LASTBUY,LASTBUY     FIRST LINE MUST CONTAIN INPUT                
         BZ    DAYEDR                                                           
         L     R5,LASTBUY          TRANSFER DATA FROM LAST RECORD               
         MVC   BUYL.RBUYSTED,RBUYSTED-RBUYKEY(R5)                               
         MVC   RBUYDAYS,RBUYDAYS-RBUYKEY(R5)                                    
         MVC   RBUYDYIN,BUYL.RBUYSTED MOVE START/END DAYS TO EL                 
         B     DAYED8                                                           
         SPACE                                                                  
DAYEDR   LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
DAYED2   XC    WORK3,WORK3         CLEAR AREA                                   
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         STC   R1,BYTE2            SAVE FOR DAYVAL                              
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE SCREEN INPUT TO AREA                    
         B     DAYED4                                                           
         MVC   WORK3(0),8(R2)                                                   
         SPACE                                                                  
DAYED4   GOTO1 VDAYVAL,DMCB,(BYTE2,WORK3),BYTE3,BYTE4                           
         LA    R3,DAYERR                                                        
         CLI   BYTE3,0                                                          
         BE    ERROR               INVALID EXPRESSION                           
         SPACE                                                                  
         MVC   BUYL.RBUYSTED,BYTE4                                              
         SPACE                                                                  
DAYED6   MVC   RBUYDYIN,BUYL.RBUYSTED                                           
         MVC   RBUYDAYS,BYTE3      DAYS OF BUY                                  
         B     DAYED8                                                           
         SPACE                                                                  
DAYED8   B     TIMED                                                            
         SPACE 2                                                                
* EDIT TIME FIELD                                                               
*                                                                               
TIMED    L     R2,THISLINE                                                      
         LA    R2,TIME(R2)         POINT TO TIME HEADER                         
         CLI   5(R2),0                                                          
         BNE   TIMED2              EDIT SCREEN INPUT                            
         SPACE                                                                  
TIMED1   OC    LASTBUY,LASTBUY     TEST FOR FIRST LINE                          
         BZ    TIMEDR              ERROR-INPUT REQUIRED                         
         L     R6,LASTBUY                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   RBUYDYT1,RBUYDYT1-RBUYDYEL(R6)                                   
         MVC   RBUYDYT2,RBUYDYT2-RBUYDYEL(R6)                                   
         B     TIMED6                                                           
         SPACE                                                                  
TIMEDR   LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
TIMED2   LA    R3,TIMERR           EDIT SCREEN FIELD                            
         XC    WORK3,WORK3                                                      
         ZIC   R6,5(R2)            INPUT LENGTH                                 
         STC   R6,BYTE2                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8              MOVE SCREEN INPUT TO WORK AREA               
         B     TIMED3                                                           
         MVC   WORK3(0),8(R2)                                                   
         SPACE                                                                  
TIMED3   EX    R6,*+8              ALLOW INPUT OF 'VARIOUS'                     
         B     *+10                                                             
         CLC   WORK3(0),=C'VARIOUS'                                             
         BNE   TIMED4                                                           
         CLI   5(R2),3             MUST INPUT AT LEAST 3 CHARACTERS             
         BL    ERROR                                                            
         MVC   FULL,=C'VARY'                                                    
         B     TIMED5                                                           
         SPACE 1                                                                
TIMED4   EX    R6,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   WORK3(0),=C'NONE'                                                
         BNE   TIMED4A             OR GO TO TIMVAL                              
         CLI   5(R2),3             MUST INPUT AT LEAST 3 CHARACTERS             
         BL    ERROR                                                            
         MVC   FULL,=C'NONE'                                                    
         B     TIMED5                                                           
         SPACE 1                                                                
*TIMED4A  GOTO1 VTIMVAL,DMCB,(BYTE2,WORK3),FULL                                 
TIMED4A  GOTO1 =V(RETIMVAL),DMCB,(BYTE2,WORK3),FULL,RR=RELO                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         OC    FULL+2(2),FULL+2    IS THERE AN END TIME?                        
         BZ    TIMED5              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'80'       USE 6A-559A B'CAST DAY?                      
         BZ    TIMED4B                                                          
         DROP  RF                                                               
*                                                                               
         CLC   FULL(2),=H'0600'    START TIME LT 6AM?                           
         BNL   TIMED5              NO                                           
         CLC   =C'CC',FULL+2       CC END TIME?                                 
         BE    TIMED5              YES                                          
         CLC   FULL+2(2),=H'0600'  END TIME GT = 6AM?                           
         BL    TIMED5              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
TIMED4B  DS    0H                                                               
         CLC   FULL(2),=H'0500'    START TIME LT 5AM?                           
         BNL   TIMED5              NO                                           
         CLC   =C'CC',FULL+2       CC END TIME?                                 
         BE    TIMED5              YES                                          
         CLC   FULL+2(2),=H'0500'  END TIME GT = 5AM?                           
         BL    TIMED5              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
TIMED5   MVC   RBUYDYT1(4),FULL    MOVE TIMES TO ELEMENT                        
         SPACE                                                                  
TIMED6   MVI   RBUYDYWT,1          WEIGHTING FACTOR                             
         DROP  R7                                                               
         SPACE 2                                                                
* EDIT LENGTH FIELD                                                             
*                                                                               
LENEDIT  L     R2,THISLINE                                                      
         LA    R2,TYP(R2)          POINT TO LENGTH HEADER                       
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BNE   LENE0060            EDIT SCREEN                                  
         OC    LASTBUY,LASTBUY                                                  
         BZ    LENE0040                                                         
         SPACE                                                                  
LENE0020 L     R5,LASTBUY          DITTO FUNCTION                               
         MVC   BUYL.RBUYDUR,RBUYDUR-RBUYKEY(R5)                                 
         B     LENE0240                                                         
         SPACE                                                                  
LENE0040 LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
LENE0060 LA    R3,LENERR                                                        
         TM    4(R2),X'08'         TEST FOR NUMERIC INPUT                       
         BZ    LENE0140            NO-TRY MINUTE EXPRESSION                     
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*                                                                               
***      CLI   RCONKSTA+4,C'C'     COMBO STN?                                   
***      BE    LENE0120            THEN DON'T CARE                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0080            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0100            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0080 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0100 EQU   *                                                                
         CH    R0,=H'120'                                                       
         BNH   LENE0120                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
*                                                                               
LENE0120 CH    R0,=H'999'          ONLY 3 DIGITS ALLOWED                        
         BH    ERROR                                                            
         STCM  R0,3,BUYL.RBUYDUR                                                
         B     LENE0240                                                         
         SPACE                                                                  
LENE0140 ZIC   R1,5(R2)            INPUT LENGTH                                 
         CH    R1,=H'4'                                                         
         BH    ERROR               TOO LONG                                     
         LA    RE,8(R1,R2)         ADD INPUT LEN TO A(FIELD)-PUT IN RE          
         BCTR  RE,0                POINT RE AT LAST BYTE                        
         CLI   0(RE),C'M'          IS MINUTE SUFFIX THERE                       
         BNE   ERROR               NO                                           
         CH    R1,=H'1'            LENGTH ONLY 1                                
         BE    ERROR               YES-MISSING NUMBER OF MINUTES                
         SH    R1,=H'2'                                                         
         MVC   WORK(3),=3X'F0'     TEST FOR NON-NUMERIC PREFIX TO 'M'           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(3),=3X'F0'                                                  
         BL    ERROR                                                            
*                                                                               
* PACK MINUTES (MINUTES NOT ALLOWED FOR SPOTPAK XFER)                           
****     CLI   RCONKSTA+4,C'C'     COMBO STN?                                   
****     BE    LENE0220            THEN DON'T CARE                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0180            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0200            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYP             
LENE0180 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0220                                                         
LENE0200 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0220 EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,BUYL.RBUYDUR+1                                                
         OI    BUYL.RBUYDUR,X'80'  MINUTE INDICATOR                             
*                                                                               
LENE0240 DS    0H                  LENGTH MUST BE LESS THAN 180                 
         MVC   HALF,BUYL.RBUYDUR                                                
         NI    HALF,X'FF'-X'80'                                                 
         CLC   HALF,=H'180'                                                     
         BNH   SPTSED                                                           
         LA    R3,638                                                           
         B     ERROR                                                            
*                                                                               
* EDIT SPOTS/WK FIELD                                                           
*                                                                               
SPTSED   L     R2,THISLINE                                                      
         LA    R2,SPTS(R2)         POINT TO SPOTS HEADER                        
         CLI   5(R2),0             NO INPUT TEST                                
         BE    SPTSED2             DO DITTO FUNCTION                            
         SPACE                                                                  
         GOTO1 VPACK                                                            
         LA    R3,NPWERR                                                        
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,BUYL.RBUYNW                                                   
         B     RATE0000                                                         
         SPACE                                                                  
SPTSED2  OC    LASTBUY,LASTBUY     FIRST TIME                                   
         BZ    SPTSEDR             YES-ERROR                                    
         SPACE                                                                  
         L     R5,LASTBUY                                                       
         TM    RBUYCOMB-RBUYKEY(R5),X'80'                                       
*                                  IS LAST RECORD 'NA' FOR COST?                
         BNO   SPTSED5             NO                                           
         MVC   BUYL.RBUYNW,RBUYCOMB-RBUYKEY(R5)                                 
*                                  YES -MOVE # SPOTS                            
*                                     FROM 'COMBO' NA STORE                     
         NI    BUYL.RBUYNW,X'7F'   TURN OFF 'NA' BIT                            
         B     RATE0000                                                         
SPTSED5  EQU   *                                                                
         MVC   BUYL.RBUYNW,RBUYNW-RBUYKEY(R5)                                   
         B     RATE0000                                                         
         SPACE                                                                  
SPTSEDR  LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE 2                                                                
* EDIT COST FIELD                                                               
*                                                                               
RATE0000 L     R2,THISLINE                                                      
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BNE   RATE0010            NO  - GO TO NEXT FIELD                       
         LA    R2,RTE1(R2)                                                      
         B     RATE0040                                                         
RATE0010 EQU   *                                                                
         CLI   STACTR,2            2ND STATION IN PROGRESS?                     
         BNE   RATE0020            NO  - GO TO NEXT FIELD                       
         LA    R2,RTE2(R2)                                                      
         B     RATE0040                                                         
RATE0020 EQU   *                                                                
         CLI   STACTR,3            3RD STATION IN PROGRESS?                     
         BNE   RATE0030            NO  - GO TO NEXT FIELD                       
         LA    R2,RTE3(R2)                                                      
         B     RATE0040                                                         
RATE0030 EQU   *                                                                
         CLI   STACTR,4            4TH STATION IN PROGRESS?                     
         BE    *+6                 NO  - NUMBER INVALID                         
         DC    H'0'                ABORT AS ILLOGICAL                           
         LA    R2,RTE4(R2)                                                      
RATE0040 EQU   *                                                                
         CLI   5(R2),0                                                          
         BNE   RATE0060                                                         
*                                                                               
*   IF NO COST IS ENTERED, THE COST IS SET TO ZERO.  PRIOR VERSION              
*      TOOK COST FROM PREVIOUS LINE.  THIS IS NOT POSSIBLE HERE.                
*                                                                               
         XC    BUYL.RBUYCOS,BUYL.RBUYCOS SET COST TO ZERO                       
         B     RATE0140                                                         
         SPACE                                                                  
RATE0060 EQU   *                                                                
         CLI   5(R2),2             TWO CHARACTERS INPUT?                        
         BNE   RATE0080            NO  - TREAT AS DOLLAR VALUE                  
         CLC   =C'NA',8(R2)        YES - ARE THEY 'NA'?                         
         BNE   RATE0080            NO  - TREAT AS DOLLAR VALUE                  
         XC    BUYL.RBUYCOS,BUYL.RBUYCOS YES - ZERO OUT COST                    
         MVC   BUYL.RBUYCOMB,BUYL.RBUYNW SAVE NUMBER PER WEEK                   
         XC    BUYL.RBUYNW,BUYL.RBUYNW   ZERO OUT NUMBER PER WEEK               
         OI    BUYL.RBUYCOMB,X'80'       TURN ON 'NA' FLAG                      
         B     RATE0140                                                         
RATE0080 EQU   *                                                                
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
         LA    R3,RATERR                                                        
         ZIC   R6,5(R2)            INSERT INPUT LENGTH FOR CASHVAL              
         ZIC   RF,5(R2)            CHECK FOR 'TRADE' FLAG                       
         LA    RE,8(R2)            SET A(CASH VALUE FIELD)                      
         AR    RE,RF               ADD L(CASH VALUE INPUT)                      
         BCTR  RE,0                BACK UP 1 CHARACTER                          
         CLI   0(RE),C'T'          'TRADE' INDICATOR?                           
         BE    RATE0090            YES                                          
         TM    BUYFLAGS,X'80'      NO  - TRADE CONTRACT?                        
         BNO   RATE0095            NO                                           
         LA    R1,8(R2)            YES - NEED TO INSERT 'T'                     
         AR    R1,RF               ADD L(INPUT DATA)                            
         MVI   0(R1),C'T'          INSERT 'T' IN LAST CHARACTER                 
         B     RATE0092                                                         
RATE0090 EQU   *                                                                
         BCTR  RF,0                DECREMENT LENGTH BY 1                        
RATE0092 EQU   *                                                                
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
         OI    BUYL.RBUYFLG2,X'02' SET TRADE INDICATOR IN BUY                   
RATE0095 EQU   *                                                                
         LR    R6,RF               INSERT INPUT LENGTH FOR CASHVAL              
*                                                                               
         B     RATE0120                                                         
RATE0100 EQU   *                                                                
         MVC   DMCB+7(1),5(R2)                                                  
RATE0120 EQU   *                                                                
         GOTO1 CASHVAL,DMCB,8(R2),(R6)                                          
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   BUYL.RBUYCOS,DMCB+4                                              
         B     RATE0140                                                         
         SPACE                                                                  
RATE0140 B     SECEL                                                            
         SPACE 2                                                                
SECEL    EQU   *                   ADD X'02' ELEMENT TO RECORD                  
         GOTO1 VADDELEM,DMCB,(R4),WORK2                                         
         SPACE 2                                                                
* CONSTRUCT EFFECTIVE DATE ELEMENTS AND ADD TO RECORD                           
*                                                                               
THIRDEL  L     R2,THISLINE                                                      
         GOTO1 =A(EFFDAT),DMCB,(RC),(R8),RR=RELO  BUILD X'03' ELEMENTS          
         LTR   R0,R0                                                            
         BZ    THIRDEL2                                                         
         LR    R3,R0                                                            
         B     ERROR                                                            
         SPACE                                                                  
THIRDEL2 ZIC   R3,NUMFLTS          NUMBER OF ELEMENTS PASSED BACK               
         LA    R7,WORK2            POINTER                                      
         SPACE                                                                  
THIRDEL4 GOTO1 VADDELEM,(R1),(R4),(R7)                                          
         LA    R7,LTHRDEL(R7)                                                   
         BCT   R3,THIRDEL4                                                      
         B     COMED                                                            
         SPACE 2                                                                
* EDIT COMMENTS FIELD                                                           
*                                                                               
COMED    LA    R2,COM(R2)          POINT TO COMMENT HEADER                      
         CLI   5(R2),0                                                          
         BE    ADDSPOT             NO DITTO FUNCTION FOR COMMENTS               
         SPACE                                                                  
         LA    R3,INVINP                                                        
         CLC   8(3,R2),=C'MG='     DEFEND AGAINST A COMMENT STARTING            
         BE    ERROR               WITH MAKEGOOD OR CREDIT NOTATION             
         CLC   8(3,R2),=C'CR='                                                  
         BE    ERROR                                                            
         SPACE                                                                  
         ZIC   R1,5(R2)            COMMENT LENGTH                               
         XC    WORK3,WORK3                                                      
         LA    RF,2(R1)            ADD 2 TO FORM EL LENGTH IN RF                
         MVI   WORK3,X'04'         EL CODE                                      
         STC   RF,WORK3+1          EL LENGTH                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     COMED2                                                           
         MVC   WORK3+2(0),8(R2)    MOVE COMMENT TO WORK AREA                    
         SPACE                                                                  
COMED2   GOTO1 VADDELEM,DMCB,(R4),WORK3                                         
         B     ADDSPOT                                                          
         SPACE 2                                                                
ADDSPOT  DS    0H                                                               
         LA    R3,265                                                           
***      CLI   RCONKSTA+4,C'C'     COMBO STN?                                   
***      BE    RESET               NO XFER OF COMBO...                          
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    ADDSP000            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    ADDSP010            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
ADDSP000 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   RESET                                                            
ADDSP010 EQU   *                                                                
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BAS   RE,GETEL                                                         
         BE    RESET                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         CLI   TWASPES,0                                                        
         BNE   ADDSDATA                                                         
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    RESET               IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     ERROR                                                            
*                                                                               
ADDSDATA DS    0H                                                               
         XC    WORK2,WORK2                                                      
WKD      USING RBUYSPEL,WORK2                                                   
         MVC   WKD.RBUYSPCD(2),=X'0830'                                         
         MVC   WKD.RBUYSPAG,TWASPAG   SPOTPAK AGENCY POWER CODE                 
         MVC   WKD.RBUYSPMD,TWASPMD   SPOTPAK MEDIA CODE                        
         MVC   WKD.RBUYSPCL,TWASPCL   SPOTPAK CLIENT CODE                       
         MVC   WKD.RBUYSPPD,TWASPPD   SPOTPAK PRODUCT CODE                      
         MVC   WKD.RBUYSPES,TWASPES   SPOTPAK ESTIMATE NUMBER                   
         MVC   WKD.RBUYSPPP,TWASPPP   SPOTPAK PIGGY PRODUCT CODE                
         MVC   WKD.RBUYSPP1,TWASPP1   SPOTPAK PRODUCT 1 SPLIT                   
         MVC   WKD.RBUYSPP2,TWASPP2   SPOTPAK PRODUCT 2 SPLIT                   
         MVC   WKD.RBUYSPST,RCONKSTA  STATION CALL LETTERS                      
         MVC   WKD.RBUYSADV,RCONKADV  REPPAK ADVERTISER CODE                    
         MVC   WKD.RBUYSPRD,RCONPRD   REPPAK PRODUCT CODE                       
         DROP  WKD,RF                                                           
         GOTO1 VADDELEM,DMCB,(R4),WORK2   ADD SPOTPAK INTERFACE ELEM            
         SPACE 2                                                                
RESET    ST    R4,LASTBUY          SAVE ADDR OF LAST BUY RECORD                 
         ZIC   R5,LINCNT                                                        
         LA    R5,1(R5)            INCREMENT COUNT OF BUY RECORDS               
         STC   R5,LINCNT                                                        
         SPACE                                                                  
         LA    R4,L'BUYLINE(R4)    POINT R4 TO NEXT BUY RECORD AREA             
         L     R2,THISFLG1         POINT R2 TO NEXT LINE                        
         LA    R2,LINELEN(R2)      AND                                          
         ST    R2,THISFLG1         UPDATE ADDR OF LINE TO BE EDITED             
         ST    R2,THISLINE                                                      
         B     SCAN                GO AND CHECK NEXT LINE FOR DATA              
         EJECT                                                                  
* READ FOR NEXT LINE NUMBER, THEN TEST TO SEE THAT ALL BUYS CAN                 
* BE ADDED WITHIN 255 LIMIT                                                     
*                                                                               
NEXTLIN  EQU   *                                                                
         ZIC   R3,LINCNT           CALCULATE # BUYS PER CONTRACT                
         SR    R2,R2                  BY DIVIDING TOTAL BUYS BY                 
         ZIC   R1,CONCTR                  # STATIONS IN COMBO                   
         DR    R2,R1                                                            
         STC   R3,LINCNT           RESET # BUYS PER STATION                     
         XC    KEY,KEY                                                          
         MVC   KEY(22),BUYLINE     BORROW REP/CON NO/                           
         MVI   BYTE3,0             BYTE WILL CONTAIN HIGHEST NUM FOUND          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(22),KEY     SAME CONTRACT?                               
         BNE   NEXTLIN4            NO  - ADDING FIRST BUY                       
         CLI   KEY+26,X'FF'        YES - PLAN RECORD?                           
         BE    NEXTLIN2            YES                                          
         MVC   BYTE3,KEY+26        SAVE FIRST LINE NUMBER FOUND                 
         SPACE                                                                  
NEXTLIN2 OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         CLC   KEYSAVE(22),KEY     SAME CONTRACT?                               
         BNE   NEXTLIN4            NO  - READ FINISHED                          
         CLI   KEY+26,X'FF'        YES - PLAN RECORD?                           
         BE    NEXTLIN2 YES                                                     
         CLC   BYTE3,KEY+26        CHECK HIGHEST SO FAR VS. KEY                 
         BNL   *+10                                                             
         MVC   BYTE3,KEY+26        NEW HIGHEST SO FAR                           
         B     NEXTLIN2                                                         
         SPACE                                                                  
NEXTLIN4 ZIC   R6,BYTE3            HIGHEST NUMBER                               
         ZIC   RF,LINCNT           NUMBER OF NEW LINES                          
         LR    R5,R6                                                            
         AR    R5,RF                                                            
         CH    R5,=H'255'          WILL LINES FIT?                              
         BH    NEXTLIN6            NO  - ERROR MESSAGE AND EXIT                 
         LA    R6,1(R6)            YES - BUMP R6 TO 1ST NEW #                   
         STC   R6,BYTE3                                                         
         B     UPDATCON                                                         
         SPACE                                                                  
NEXTLIN6 SH    R5,=H'255'          CALCULATE POINTER TO LINE THAT               
         SR    RF,R5               WOULD EXCEED 255 LIMIT                       
         LA    RE,LINELEN                                                       
         MR    RE,RE               FIND DISPLACEMENT TO THAT LINE               
         LA    R2,CBIDAY1H                                                      
         LA    R2,0(R2,RF)         POINT R2 TO LINE                             
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* ROUTINE TO UPDATE MODIFICATION DATE AND NUMBER ON CONTRACT                    
UPDATCON EQU   *                                                                
         MVI   STACTR,1            SET TO PROCESS 1ST STATION                   
UPDA0010 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    UPDA0020            YES - USE THE ORIGINAL ORDER                 
*                                                                               
*  NO  - MUST GO TO THE COMBO CONTROL ELEMENT AND GET THE NEXT                  
*     CONTRACT NUMBER, RETRIEVE THAT CONTRACT RECORD                            
*                                                                               
         GOTO1 =A(NEXTCON#),DMCB,(RC),(R8),RR=RELO                              
*                                  GET NEXT CONTRACT                            
*                                                                               
UPDA0020 EQU   *                                                                
         MVI   TAREQ,0             INITIALIZE T/A REQ INDICATOR                 
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CONTRACT                        
         BNZ   *+12                                                             
         CLI   RCONMOD,X'FF'                                                    
         BE    UPDA0040                                                         
         CLI   BYTE3,1                                                          
         BNE   UPDA0040                                                         
         CLC   RCONCREA,TODAY                                                   
         BE    UPDA0030                                                         
         OI    TAREQ,X'01'         T/A REQUEST INDICATOR                        
         MVC   RCONCREA,TODAY      LINE 1 ONLY                                  
         SPACE                                                                  
UPDA0030 DS    0H                                                               
         OI    RCONMODR,X'20'+X'10' X'20'=BUYS ADDED-LINE 1                     
*                                   X'10'=NOT PENDING/BUYLINE(S) WERE           
*                                         ADDED AT ONE POINT.  THIS BIT         
*                                         DOES NOT GET RESET.                   
         B     UPDA0050            LINE 1 DOES NOT BUMP MOD NUMBER              
         SPACE                                                                  
UPDA0040 BAS   RE,BUMPNUM                                                       
         SPACE                                                                  
UPDA0050 B     BUYUPDTE                                                         
         EJECT                                                                  
BUMPNUM  OI    RCONMODR,X'40'      BUY CHANGE INDICATOR                         
         CLC   RCONCREA,TODAY      NEW CONTRACT TODAY                           
         BER   RE                  YES-EXIT                                     
         SPACE                                                                  
         CLC   RCONMODD,TODAY      MODIFIED TODAY ALREADY                       
         BER   RE                  YES-EXIT                                     
         SPACE                                                                  
         OI    TAREQ,1             T/A REQUEST INDICATOR                        
         NI    RCONMODR,X'5F'      NO CONTRACT HEADLINE CHANGE                  
         TM    RCONMODR+1,X'C0'    'ACE' - DON'T BUMP MOD NUM                   
         BO    BUMP10                                                           
         ZIC   R1,RCONMOD          INCREMENT MOD NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY      SET MODIFICATION DATE                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE COMPLETES EACH BUY RECORD'S KEY, X'01' ELEMENT AND                    
* UPDATES CONTRACT BUCKETS.  AT ENTRY BYTE3 HAS FIRST LINE NUMBER               
*                                                                               
BUYUPDTE EQU   *                                                                
         ZIC   R2,LINCNT           COUNTER                                      
         LA    R4,BUYLINE          1ST CONTRACT'S BUYLINES                      
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    BUYU0020            YES                                          
         BAS   RE,SETBUYLN         NO  - SET NEXT CON#'S BUYLINES               
         L     R4,FULL             SET ADDRESS CALCULATED                       
BUYU0020 EQU   *                                                                
         ZIC   R5,BYTE3                                                         
BUYU0030 STC   R5,BUYL.RBUYKMLN                                                 
         STC   R5,BUYL.RBUYKLIN                                                 
         MVC   BUYL.RBUYCHGI(2),=C'A ' SET LAST CHANGE INDICATOR                
         MVC   BUYL.RBUYCREA,TODAY                                              
         MVC   BUYL.RBUYKMOD,RCONMOD BUY MOD NUMBER IS SAME AS CONTRACT         
*        CLI   RCONMOD,X'FF'       UNLESS CON MOD NUM IS 255 WHICH              
*        BNE   *+8                 CAUSES BUY MOD TO BE SET TO ZERO.            
*        MVI   BUYL.RBUYKMOD,0                                                  
         SPACE 2                                                                
* UPDATE REP VERSION NUMBER AND UNCONFIRM IF NECESSARY                          
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    BUYU0060                                                         
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    BUYU0040                                                         
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
                                                                                
BUYU0040 DS    0H                                                               
         MVC   BUYL.RBUYVER,RCONSRV STORE REP VERSION NO. IN BUY                
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    BUYU0050                                                         
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
BUYU0050 OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         SPACE 1                                                                
BUYU0060 DS    0H                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
         TM    TWAFLAGS,X'08'      REP USING 'DAILY PACING'?                    
         BNO   BUYU0065            YES                                          
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
BUYU0065 EQU   *                                                                
*                                                                               
         MVC   WORK(4),VGTBROAD    BUILD ROUTINE LIST FOR REGENBUC              
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         GOTOX (RFGENBUC,VREPFACS),DMCB,(R4),WORK2,WORK                         
         XC    WORK2,WORK2                                                      
         SPACE                                                                  
         GOTO1 =A(BUCKUP),(R1),(R4),(RC),RR=RELO                                
         SPACE                                                                  
BUYU0070 LA    R4,L'BUYLINE(R4)    POINT TO NEXT RECORD                         
         LA    R5,1(R5)            INCREMENT LINE NUMBER                        
         BCT   R2,BUYU0030                                                      
         B     BUYU0080                                                         
         EJECT                                                                  
SETBUYLN NTR1                                                                   
         ZIC   R3,STACTR           STATION IN PROGRESS                          
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         SR    R2,R2               EVEN REG OF PAIR                             
         ZIC   R1,LINCNT           NUMBER OF BUYLINES IN SET                    
         MR    R2,R1                                                            
         LA    R1,450              SIZE OF BUYLINE                              
         MR    R2,R1               FINAL DISPLACEMENT                           
         LA    R1,BUYLINE                                                       
         AR    R1,R3               ADD DISPLACEMENT                             
         ST    R1,FULL             SET FOR RETURN                               
         XIT1                                                                   
         EJECT                                                                  
* WRITE BACK BUY RECORDS AND PUT CONTRACT                                       
* CHECK ENTIRE BLOCK OF BUYLINES BEFORE GOING TO DATAMGR                        
BUYU0080 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BNE   BUYU0110            NO  - ONLY DO TEST FOR 1ST STATION           
         LA    R4,BUYLINE          POINTER                                      
         ZIC   R3,LINCNT           COUNTER                                      
         SPACE                                                                  
BUYU0090 DS    0H                                                               
* MAKE SURE NO MORE THAN 169 SPTS FOR SPOTPAK XFER                              
         LR    R6,R4                                                            
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUYU0100                                                         
*                                                                               
*   FOR EACH BUY RECORD, LOAD THE CURRENT STATION FROM THE RCONREC              
*       INTO THE SPOTPAK INTERFACE ELEMENT.                                     
*                                                                               
         MVC   RBUYSPST-RBUYSPEL(5,R6),RCONKSTA                                 
*                                                                               
         TM    BUYL.RBUYCOMB,X'80' IS THIS A 'NA' RATE BUY?                     
         BZ    BUYU0095                                                         
*                                                                               
         SR    R0,R0               YES, CHECK FOR MAX 169 SPOTS                 
         NI    BUYL.RBUYCOMB,X'FF'-X'80' CALULATE TOTAL SPOTS SINCE             
         ZIC   R1,BUYL.RBUYCOMB    NA RATE LINES DO NOT HAVE TOTAL SPTS         
         OI    BUYL.RBUYCOMB,X'80' CLEAR AND RESTORE COMBO FLAG                 
*                                  RBUYCOMB LESS HOB IS #SPOT                   
         ZIC   RE,BUYL.RBUYTWKS                                                 
         MR    R0,RE               MULTIPLY R1(#SPT) & RE(TOTAL WKS)            
         B     BUYU0098            R1 HAS PRODUCT(TOTAL SPTS)                   
*                                                                               
BUYU0095 DS    0H                                                               
         LH    R1,BUYL.RBUYTSPT                                                 
         LTR   R1,R1               SKIP CHECK IF NEGATIVE                       
         BM    BUYU0100                                                         
BUYU0098 CH    R1,=H'169'          MAX OF 169 SPOTS                             
         BNH   BUYU0100                                                         
         LA    R2,CBIACTH                                                       
         LA    R3,266                                                           
         B     ERROR                                                            
*                                                                               
BUYU0100 LA    R4,L'BUYLINE(R4)                                                 
         BCT   R3,BUYU0090                                                      
*                                                                               
BUYU0110 EQU   *                                                                
         LA    R4,BUYLINE          1ST STATION'S BUYLINES                       
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    BUYU0120            YES                                          
         BAS   RE,SETBUYLN         NO  - SET NEXT CON#'S BUYLINES               
         L     R4,FULL             SET ADDRESS CALCULATED                       
BUYU0120 EQU   *                                                                
         ZIC   R3,LINCNT           COUNTER                                      
*                                                                               
BUYU0130 EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BE    BUYU0140            YES - SPOTPAK INTERFACE OKAY                 
         LR    R6,R4               NO  - NEED CORRECT STATION                   
         MVI   ELCODE,X'08'        FIND INTERFACE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   BUYU0140            NO ELEMENT FOR THIS RECORD                   
*                                                                               
*   FOR EACH BUY RECORD, LOAD THE CURRENT STATION FROM THE RCONREC              
*       INTO THE SPOTPAK INTERFACE ELEMENT.                                     
*                                                                               
         MVC   RBUYSPST-RBUYSPEL(5,R6),RCONKSTA                                 
*                                                                               
BUYU0140 EQU   *                                                                
         OI    BUYL.RBUYRTS,X'40'  MARK AS COMBO                                
*                                                                               
         GOTO1 =A(ADDEXTRA),RR=Y                                                
*                                                                               
         GOTO1 VADDREC,DMCB,(R4)                                                
* UPDATE BUY PASSIVE POINTERS                                                   
         MVC   KEYSAVE,0(R4)                                                    
         MVC   KEYSAVE+28(4),KEY                                                
         MVC   KEY,KEYSAVE                                                      
*                                                                               
         GOTO1 VLOAD,DMCB,(X'19',0),(R4)                                        
*                                                                               
         LA    R4,L'BUYLINE(R4)                                                 
         BCT   R3,BUYU0130                                                      
         B     CONMDUP                                                          
         SPACE                                                                  
CONMDUP  EQU   *                                                                
         CLI   STACTR,1            1ST STATION IN PROGRESS?                     
         BNE   PUTCON              NO  - SKIP MOD DISPLAY                       
         MVC   CONMOD,MYSPACES     DISPLAY CON MOD NUMBER                       
         OI    CONMODH+6,X'80'                                                  
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET?                                
         BZ    CONM40                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         SPACE 1                                                                
         TM    RCONCONF,X'40'      CONFIRMED - SHOW MOD NO.                     
         BO    CONM40                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLC   RCONSRV,RCONSSV                                                  
         BH    CONM15                                                           
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     CONM20                                                           
CONM15   EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
CONM20   TM    RCONMODR+1,X'40'    GRAPH?                                       
         BZ    *+14                                                             
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     *+10                                                             
         MVC   CONMOD(7),=C'WIP VER'                                            
         DROP  R6                                                               
         B     PUTCON                                                           
         SPACE 1                                                                
CONM40   CLI   RCONMOD,0                                                        
         BE    PUTCON                                                           
         MVC   CONMOD(7),=C'MOD NUM'                                            
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT,FLOAT=-                         
         B     PUTCON                                                           
         SPACE                                                                  
PUTCON   EQU   *                                                                
         CLI   STACTR,1            1ST CONTRACT IN PROGRESS?                    
         BNE   PUTC0010                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  RE-READ CON SINCE THERE HAS BEEN             
         DROP  RF                     INTERVENING I/O                           
         B     PUTC0020                                                         
PUTC0010 EQU   *                                                                
         MVC   KEY+28(4),SAVEDADD  INSERT DISK ADDRESS                          
PUTC0020 EQU   *                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,(R1),AIO3                                                
         GOTO1 VPUTREC,(R1),RCONREC                                             
         CLC   STACTR,CONCTR       ALL CONTRACTS PROCESSED?                     
         BE    PUTC0030            YES                                          
         ZIC   RF,STACTR           NO  -                                        
         LA    RF,1(RF)            BUMP STATION COUNT IN PROGRESS               
         STC   RF,STACTR           SAVE IT BACK                                 
         B     UPDA0010            GO BACK AND DO NEXT                          
         EJECT                                                                  
* PRODUCE CONTRACT HEADER MESSAGE                                               
*                                                                               
PUTC0030 EQU   *                                                                
         ZIC   R6,BYTE3            FIRST LINE NUMBER                            
         ZIC   R7,LINCNT                                                        
         AR    R7,R6                                                            
         BCTR  R7,0                R7 HAS LAST LINE NUM                         
         XC    TEMP,TEMP                                                        
         LA    R3,TEMP                                                          
         SPACE                                                                  
         EDIT  (R6),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0               ADD OUTPUT LEN TO POINTER                    
         CR    R6,R7               ONLY ONE BUY LINE ADDED                      
         BE    PUTC0040            YES                                          
         MVI   0(R3),DASH                                                       
         LA    R3,1(R3)                                                         
         EDIT  (R7),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         SPACE                                                                  
PUTC0040 DS    0H                                                               
         LA    R0,TEMP                                                          
         SR    R3,R0                                                            
         GOTO1 VDISMSG,DMCB,(0,47),0,0,((R3),TEMP)                              
         B     EXXMOD                                                           
         DROP  BUYL                                                             
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONSHRTH                                                         
       ++INCLUDE RECNTF8D                                                       
         EJECT                                                                  
*              DDCOMFACS                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER FLIGHT DATE ENTRIES                                            
*                                                                               
FLTENTD  DSECT                                                                  
FLTSTART DS    CL6                 YYMMDD - MONDAY OF FIRST WEEK                
FLTEND   DS    CL6                        - SUNDAY OF LAST WEEK                 
FLTIND   DS    B                                                                
*                                  X'80' - EVERY WEEK                           
*                                  X'40' - ALTERNATE WEEKS                      
FLTWKS   DS    B                   NUMBER OF WEEKS                              
FLTCNTL  DS    B                   CONTROL VALUES PASSED BY REFLSCAN            
FLTDAYS  DS    B                   START AND END DAYS                           
*                                  BITS 0-3 START DAY (MONDAY = 1)              
*                                  BITS 4-7 END DAY (SUNDAY = 7)                
FLTUIWKS DS    B                   USER INPUT NUMBER OF WEEKS                   
LFLTENT  EQU   (*-FLTSTART)                                                     
         SPACE 2                                                                
* SPECIAL DSECT FOR THIS OVERLAY                                                
*                                                                               
MULTBUY  DSECT                                                                  
RELO     DS    A                                                                
CONST    DS    CL6                 YYMMDD                                       
CONEND   DS    CL6                 YYMMDD                                       
CSTDAY   DS    B                                                                
CENDAY   DS    B                                                                
EFFST    DS    CL6                 USED BY                                      
EFFEND   DS    CL6                 EFFDAT                                       
CONEFFST DS    CL6                                                              
FLTNUM   DS    C                   FLIGHT CODE                                  
ONUM     DS    X                   OLD FLIGHT NUMBER                            
NUMFLTS  DS    C                   NUMBER OF FLIGHTS                            
MODE     DS    C                                                                
EFFDMODE DS    C                   FLAG FOR EFFECTIVE DATE CHANGE               
LASTLIN  DS    X                   LAST LINE WRITTEN BACK                       
MODNUM   DS    X                   UPDATED CONTRACT MOD NUMBER                  
ERRFLAG  DS    C                   SET WHEN ERROR IS ENCOUNTERED                
ROLLSW   DS    C                   SWITCH SET WHEN END DATE MUST                
*                                  BE ROLLED TO KEEP N WEEKS INTACT             
VDAYVAL  DS    V                                                                
*VTIMVAL  DS    V                                                               
LASTBUY  DS    A                   PREVIOUS BUY RECORD                          
THISBUY  DS    A                   CURRENT BUY RECORD                           
THISLINE DS    A                   CURRENT LINE ADDRESS: DAY FIELD              
THISFLG1 DS    A                   CURRENT LINE ADDRESS: START                  
LINCNT   DS    C                   NUMBER OF BUY LINES                          
SAVEKEY  DS    CL32                LAST KEY USED IN SEQUENTIAL READ0000         
COMBOCTL DS    CL60                STORAGE FOR COMBO CONTROL ELEMENT            
CONCTR   DS    XL1                 CONTRACT COUNTER                             
STACTR   DS    XL1                 STATION COUNT IN PROGRESS                    
CONKSTA  DS    CL5                 STATION OF ORIGINAL ORDER                    
SAVEDADD DS    CL4                 SAVE AREA FOR DISK ADDRESS                   
ENTRIES  DS    CL500                                                            
BUYLINE  DS    16CL450             BUY RECORD AREA                              
         EJECT                                                                  
* EQUATES                                                                       
*                                                                               
NFORM    EQU   X'01'               N WEEKS FORMAT                               
DATES    EQU   X'02'               DATE TO DATE FORMAT                          
SONLY    EQU   X'04'               DATE EXPRESSION WAS ONLY 'S'                 
TYPBERR  EQU   190                                                              
BOPERR   EQU   173                 MISSING BOP DATA                             
ENBEFST  EQU   64                                                               
INVERR   EQU   2                                                                
MISINP   EQU   1                                                                
SEQOROUT EQU   132                 DATE SEQUENCE ERROR                          
ADD      EQU   X'01'                                                            
CHANGE   EQU   X'02'                                                            
DELETE   EQU   X'04'                                                            
EFFD     EQU   X'08'                                                            
ECHG     EQU   X'10'               ONE CHANGE OF EFFECTIVE DATES                
FCHG     EQU   X'20'               ONE CHANGE OF FLIGHT NUMBERS                 
CCHG     EQU   X'40'               ONE LINE CANCELLED                           
CANCEL   EQU   X'80'                                                            
EFFDYWRT EQU   X'01'               ALL BUYLINES CHECKED, OK TO WRITE            
DASH     EQU   C'-'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ALT      EQU   X'40'               ALTERNATE WEEKS INDICATOR VALUE              
LTHRDEL  EQU   (RBUYDTWK-RBUYDTEL)+L'RBUYDTWK                                   
LINELEN  EQU   CBIDAY2H-CBIDAY1H                                                
DAY      EQU   0                                                                
TIME     EQU   CBITIMH-CBIDAY1H                                                 
TYP      EQU   CBITYPH-CBIDAY1H                                                 
SPTS     EQU   CBISPTSH-CBIDAY1H                                                
RTE1     EQU   CBIRAT1H-CBIDAY1H                                                
RTE2     EQU   CBIRAT2H-CBIDAY1H                                                
RTE3     EQU   CBIRAT3H-CBIDAY1H                                                
RTE4     EQU   CBIRAT4H-CBIDAY1H                                                
COM      EQU   CBICOMH-CBIDAY1H                                                 
         EJECT                                                                  
***********************************************************************         
* ADD EXTRA DESCRIPTION ELEMENT X'80'                                           
***********************************************************************         
         DROP  RB                                                               
         CSECT                                                                  
         DS    0F                                                               
*                                                                               
ADDEXTRA NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*ADDEXT*'                                                    
*                                                                               
         XC    WORK,WORK                                                        
WKD      USING RBUYXXEL,WORK                                                    
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         MVC   WKD.RBUYXXMD,RCONMOD    CONTRACT MOD# AT BUY CREATION            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDEXT10                                                         
         USING RCONSEND,R6                                                      
         MVC   WKD.RBUYXXVR,RCONSRV    CONTRACT VER# AT BUY CREATION            
         TM    RCONSENF,X'20'                                                   
         BZ    ADDEXT10                                                         
         ZIC   R1,WKD.RBUYXXVR         REP VERSION NOT ADVANCED                 
         LA    R1,2(R1)                ADVANCE VERSION MANUALLY                 
         STC   R1,WKD.RBUYXXVR                                                  
         DROP  R6,WKD                                                           
*                                                                               
ADDEXT10 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
*                                                                               
ADDEXTX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
*                                  P2=RC                                        
***********************************************************************         
BUCKUP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(BUYREC)                                    
         L     R0,VRECUP                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         LA    R5,TWABYTOT         CALCULATE A(WORKSPACE)                       
         AH    R5,=Y(TWABYTRD-TWABYTOT)                                         
         DROP  RF                                                               
*                                                                               
*   NOTE:  THIS IS IN 'TRADE BUCKET TOTAL' AREA IN THE TWA, WHICH               
*        IS NOT USED WITHIN THIS MODULE.                                        
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(R2),(BUCKFLGS,RCONREC),       +        
               ACOMFACS,VGTBROAD,(R0),(R5)                                      
         BE    BUUP0040                                                         
*                                                                               
         LA    R2,CONBACTH                                                      
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
BUUP0040 DS    0H                                                               
         BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
***********************************************************************         
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
         XIT1                                                                   
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              PARAMETER 1 =       A(RECORD) BYTE 0=X'FF' FOR BUFFERS           
*                                            INSTEAD OF RECORDS WHERE           
*                                            1ST 2 BYTES = LENGTH OF            
*                                            BUFFER                             
*              PARAMETER 2 =       A(BUCKET TO BE INSERTED)                     
*              PARAMETER 3 =       RC                                           
*                                                                               
ADDBUCK  CSECT                                                                  
         NMOD1 0,*0218*                                                         
*              SET UP BXLE                                                      
         L     RC,8(R1)                                                         
         L     R2,0(R1)            A(REC)                                       
         L     R6,4(R1)            A(ELEM)                                      
         MVC   HALF,27(R2)                                                      
         LA    R3,34(R2)           1ST ELEM                                     
         MVI   BYTE,2              REPPAK IND FOR RECUP                         
*                                                                               
         CLI   0(R1),X'FF'         BUFFER (NOT RECORD)?                         
         BNE   *+18                                                             
         MVC   HALF,0(R2)          BUFFER LENGTH                                
         LA    R3,2(R2)            1ST ELEMENT IN BUFFER                        
         MVI   BYTE,X'FF'          BUFFER IND TO RECUP                          
*                                                                               
         LH    R5,HALF             REC LEN                                      
         LA    R5,0(R5,R2)         REC END                                      
         BCTR  R5,R0                                                            
         BCTR  R5,R0               REC END-2 IN CASE ONLY 1 ELEM                
         SR    R4,R4                                                            
         CR    R3,R5               NO ELEMENTS YET?                             
         BH    ADDB100                                                          
*                                                                               
         CLC   0(6,R6),0(R3)       NEW ELEM V OLD                               
         BL    ADDB100             LOW-ADD NEW ELEM                             
         BE    ADDB200             EQUAL-ADD TO OLD ELEM                        
         IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,*-18          NEXT ELEM                                    
*                                                                               
*              ADD ELEMENT                                                      
ADDB100  GOTO1 VRECUP,DMCB+12,(BYTE,(R2)),(R6),(R3)                             
*                                                                               
BUCKXIT  XMOD1                                                                  
*                                                                               
*              EQUAL BUCKETS                                                    
ADDB200  MVC   FULL,6(R6)          NEW AMOUNT                                   
         L     R6,FULL                                                          
         MVC   FULL,6(R3)          OLD AMOUNT                                   
         A     R6,FULL                                                          
         ST    R6,FULL                                                          
         MVC   6(4,R3),FULL        NEW AMOUNT                                   
*                                                                               
         LTR   R6,R6                                                            
         BNZ   BUCKXIT                                                          
*                                                                               
*              DELETE ZERO BUCKET                                               
         GOTO1 VRECUP,DMCB+12,(BYTE,(R2)),(R3),(R3)                             
         B     BUCKXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* SUB-ROUTINE FOR CONSTRUCTING EFFECTIVE DATE ELEMENTS FROM                     
* ENTRIES GENERATED BY REFLSCAN                                                 
*                                                                               
EFFDAT   NMOD1 0,*EFFDAT*                                                       
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
         USING MULTBUY,R8                                                       
         ST    R4,TEMP                                                          
         ZIC   R2,NUMFLTS                                                       
         LA    R6,ENTRIES          R6 POINTS TO ENTRIES                         
         USING FLTENTD,R6                                                       
         XC    WORK2,WORK2                                                      
         LA    R7,WORK2            R7 POINTS TO ELEMENTS                        
         USING RBUYDTEL,R7                                                      
         SPACE                                                                  
EFFINIT  SR    R4,R4                                                            
         SR    R5,R5                                                            
         CLC   =C'MB',CBIACT                                                    
         BNE   EFFI2                                                            
         L     RF,TEMP                                                          
         USING RBUYKEY,RF                                                       
         IC    R4,RBUYSTED                                                      
         DROP  RF                                                               
         B     *+12                                                             
EFFI2    LA    RE,IOAREA                                                        
         IC    R4,RBUYSTED-RBUYKEY(RE)                                          
         SRDL  R4,4                R4 HAS START DAY                             
         SRL   R5,28               R5 HAS END DAY                               
         SPACE                                                                  
EFF      MVI   RBUYDTEL,X'03'      ELEMENT CODE                                 
         MVI   RBUYDTLN,LTHRDEL    ELEMENT LENGTH                               
         MVC   RBUYDTWK,FLTWKS     NUMBER OF WEEKS                              
         LR    R3,R4                                                            
         CLC   FLTSTART,CONST      DOES ENTRY START IN FIRST WEEK OF            
         BH    EFF4                CONTRACT-NO                                  
         CLM   R3,1,CSTDAY         DOES BUY ST DAY PRECEDE CONST DAY            
         BL    EFF2                YES                                          
         B     EFF4                NO                                           
         SPACE                                                                  
EFF2     LA    R3,7(R3)            PUSH BACK EFFECTIVE START 1 WEEK             
         TM    FLTCNTL,SONLY       INPUT OF JUST 'S' AND BUY START              
         BO    EFF3                BEFORE CON ST IS AN ERROR                    
         TM    FLTCNTL,NFORM                                                    
         BNO   *+12                FOR N WEEKS FORMAT, SET SWITCH TO            
         MVI   ROLLSW,YES          ROLL BACK END DATE SO SCHEDULE WILL          
         B     EFF4                SPAN EXACTLY N WEEKS                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                                                              
         DC    H'0'                FOR DATES FORMAT, DECREASE WEEKS             
*        ZIC   R1,RBUYDTWK         SPANNED BY ONE TO REFLECT ROLLED             
*        BCTR  R1,0                BACK START DATE                              
*        STC   R1,RBUYDTWK                                                      
         B     EFF4                                                             
         SPACE                                                                  
EFF3     LA    R0,SDTERR                                                        
         B     EFFEXIT                                                          
         SPACE                                                                  
EFF4     BCTR  R3,0                SUBTRACT ONE FOR ADDING TO MONDAY            
         LTR   R3,R3               IS BUY START DAY MONDAY                      
         BZ    EFF5                YES                                          
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R3)                                   
         B     EFF6                                                             
*                                                                               
EFF5     MVC   EFFST,FLTSTART      ALREADY MONDAY                               
*                                                                               
EFF6     DS    0H                                                               
         BAS   RE,GETEFFST                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
         LR    R3,R5                                                            
         CLC   FLTEND,CONEND       DOES ENTRY END IN LAST WEEK OF CON           
         BL    EFF10               NO                                           
         CLM   R3,1,CENDAY         BUY END DAY VS. CONTRACT END DAY             
         BH    EFF8                ITS HIGHER-ROLL BACK EFFECTIVE DATES         
         B     EFF10                                                            
         SPACE                                                                  
EFF8     SH    R3,=H'14'           SUBTRACT TO FIND NUMBER FOR ADDAY            
*        ZIC   R0,RBUYDTWK         DECREMENT NUMBER OF WEEKS                    
*        SH    R0,=H'1'                                                         
*        BZ    EFF9                                                             
*        STC   R0,RBUYDTWK                                                      
         TM    FLTCNTL,NFORM       RECOGNIZE END DATE ERROR IF N WEEKS          
         BO    EFF9                CANNOT FIT WITHIN CONTRACT                   
         B     EFF12                                                            
         SPACE                                                                  
EFF9     LA    R0,EDTERR           ENT IS LAST WEEK ONLY AND HAD TO             
         B     EFFEXIT             BE ROLLED BACK                               
         SPACE                                                                  
EFF10    SH    R3,=H'7'                                                         
         BM    EFF12               DAY IS NOT SUNDAY                            
         MVC   EFFEND,FLTEND       SKIP ADDAY FOR BUY END DAY OF                
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
         CLI   ROLLSW,YES          SUNDAY. FOR BUY END DAY OF SUNDAY,           
         BE    EFF13               FIRST CHECK FOR ROLL SWITCH AND              
         B     EFF14               BRANCH OR SKIP ADDAY IF ROLL IS OFF.         
*                                                                               
EFF12    GOTO1 ADDAY,DMCB,FLTEND,EFFEND,(R3)                                    
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
         CLI   ROLLSW,YES          ONLY ENTER THIS ROUTINE WHEN                 
         BNE   EFF14               SWITCH IS SET TO ROLL BACK END DATE          
         SPACE                                                                  
EFF13    MVI   ROLLSW,NO                                                        
         MVC   DMCB+8(4),=F'7'     ROLL BACK END DATE 7 DAYS UNLESS             
         CR    R4,R5               END DAY IS GT START DAY                      
         BNH   *+10                                                             
         MVC   DMCB+8(4),=F'14'                                                 
*        GOTO1 ADDAY,DMCB,EFFEND,EFFEND                                         
         CLC   EFFEND,CONEND                                                    
         BNH   *+12                                                             
         LA    R0,EDTERR           RUN PAST CONTRACT END                        
         B     EFFEXIT                                                          
         CH    R2,=H'1'            ANY MORE ENTRIES                             
         BE    EFF18               NO                                           
         LA    RF,LFLTENT(R6)      YES-POINT TO NEXT ENTRY AND CHECK            
         CLC   EFFEND,0(RF)        END DATE VS. MONDAY OF NEXT START            
         BL    EFF18               WEEK.                                        
         LA    R0,SEQOROUT         SEQUENCE ERROR-LAST SET OF EFFECTIVE         
         B     EFFEXIT             DATES WILL OVERLAP NEXT FLIGHT ENTRY         
         SPACE                                                                  
EFF14    CR    R4,R5               ADJUSTMENT LOGIC ENTERED ONLY                
         BNH   EFF16               WHEN START DAY GREATER THAN END DAY          
         TM    FLTCNTL,NFORM                                                    
         BO    EFF15               N WEEKS INPUT FORMAT                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                 DATE-DATE INPUT FORMAT                       
         DC    H'0'                                                             
*        ZIC   R1,RBUYDTWK         SUBTRACT 1 TO ADJUST FOR                     
*        BCTR  R1,0                BUY WEEKS SPANNING                           
*        STC   R1,RBUYDTWK         2 WEEKS OF CONTRACT.                         
         B     EFF16                                                            
         SPACE                                                                  
EFF15    MVC   DUB(6),EFFEND       PUSH BACK END DATE 7DAYS FOR N WKS           
*        GOTO1 ADDAY,DMCB,DUB,EFFEND,7                                          
         CLC   EFFEND,CONEND                                                    
         BNH   EFF16                                                            
         LA    R0,EDTERR           RUNS PAST CONTRACT END                       
         B     EFFEXIT                                                          
         SPACE                                                                  
EFF16    CLC   EFFST,EFFEND        CHECK SEQUENCE OF EFFECTIVE                  
         BL    EFF18               START/END                                    
         BE    EFF17                                                            
         LA    R0,ENBEFST          SEQUENCE ERROR                               
         B     EFFEXIT                                                          
         SPACE                                                                  
EFF17    CR    R4,R5               START/END MUST BE                            
         BE    EFF18               IDENTICAL                                    
         LA    R0,INVERR                                                        
         B     EFFEXIT                                                          
         SPACE                                                                  
EFF18    GOTO1 DATCON,DMCB,(0,EFFST),(3,RBUYDTST)                               
         GOTO1 (RF),(R1),(0,EFFEND),(3,RBUYDTED)                                
         MVC   RBUYDTIN,FLTIND                                                  
         CLC   =C'MB',CBIACT                                                    
         BNE   EFF19                                                            
         L     RF,TEMP                                                          
         USING RBUYKEY,RF                                                       
         MVC   RBUYDTNW,RBUYNW                                                  
         DROP  RF                                                               
         B     *+14                                                             
EFF19    LA    RE,IOAREA                                                        
         MVC   RBUYDTNW,RBUYNW-RBUYKEY(RE)                                      
         TM    RBUYDTIN,ALT        FOR ALTERNATE WEEKS ONLY                     
         BZ    EFF20                                                            
         ZIC   R1,RBUYDTWK         TAKE TOTAL WEEKS                             
         LA    R1,1(R1)            AND AFTER ADDING 1 TO STOP                   
         SRL   R1,1                ROUNDING DOWN, DIVIDE BY TWO                 
         STC   R1,RBUYDTWK         TO FIND NUMBER OF ACTIVE WEEKS               
         SPACE                                                                  
EFF20    LA    R7,LTHRDEL(R7)                                                   
         LA    R6,LFLTENT(R6)                                                   
         BCT   R2,EFF                                                           
         XR    R0,R0               SUCCESSFUL PROCESSING                        
         B     EFFEXIT                                                          
         SPACE                                                                  
EFFEXIT  XIT1 REGS=(R0)                                                         
         EJECT                                                                  
***********************************************************************         
* R4 START DAY                                                                  
* R5 END DAY                                                                    
* R6 POINTS TO ENTRIES WITH FLIGHT DATE INFO                                    
*                                                                               
* ROUTINE TO CALCULATE ACTUAL EFFECTIVE START AND END DATES                     
*                                                                               
* NOTE: RECNT18 ORIGINALLY SUPPORTS ONLY STANDARD WEEK ROTATIONS.               
*       IT CALLS REFLSCAN TO BUILD A TABLE WITH MON-SUN DATES.                  
*       THESE ROUTINES MASSAGES THE TABLE AND RECALCULATE NUMBER OF             
*       WEEKS, SO OUT-OF-WEEK-ROTATORS ARE SUPPORTED                            
***********************************************************************         
GETEFFST NTR1                                                                   
         CLC   CSTDAY,CENDAY                                                    
         BH    GETST20                                                          
*                                                                               
* STANDARD WEEK ONLY. START FROM CONTRACT START DATE OR LATER                   
*                                                                               
         CLC   FLTSTART,CONST      SAME WEEK AS CONTRACT START DATE?            
         BL    GETST10                                                          
         MVC   EFFST,FLTSTART      NO                                           
         LA    R2,1                START WITH MONDAY                            
         LR    R3,R4                                                            
         BCTR  R3,0                                                             
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R3)                                   
         MVC   CONEFFST,EFFST                                                   
         B     GETST80                                                          
*                                                                               
GETST10  DS    0H                  SAME WEEK AS CONTRACT START DATE             
         MVC   EFFST,CONST         SO, USE CONTRACT START DATE                  
         ZIC   R2,CSTDAY           AND CONTRACT START DAY, TOO                  
         MVC   CONEFFST,EFFST                                                   
*                                                                               
         CR    R2,R4                                                            
         BE    GETST80                                                          
         LR    R3,R4                                                            
         CR    R3,R2                                                            
         BE    GETST80                                                          
         BH    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R3)                                      
         B     GETST80                                                          
*                                                                               
*                                                                               
* FOR OUT-OF-WEEK-ROTATORS                                                      
*                                                                               
GETST20  DS    0H                                                               
         ZIC   R3,CSTDAY                                                        
         CR    R4,R3                                                            
         BNL   GETST25                                                          
         CR    R5,R3                                                            
         BL    GETST25                                                          
         LA    R0,613                                                           
         B     EFFEXIT                                                          
****                                                                            
GETST25  DS    0H                                                               
         ZIC   R2,FLTDAYS          START/END DAY                                
         SRL   R2,4                REMOVE END DAY                               
         LTR   R2,R2               DID USER SPECIFIED S FOR START DAY?          
         BNZ   GETST30                                                          
         ZIC   R2,CSTDAY           YES, USE CONTRACT START DAY                  
*                                                                               
GETST30  DS    0H                                                               
         LR    R3,R2                                                            
         BCTR  R2,0                                                             
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R2)                                   
*                                                                               
* FIRST, CALCULATE THE EFFECTIVE START DATE BASED ON THE ROTATION OF            
* THE CONTRACT FLIGHT DATES                                                     
*                                                                               
         ZIC   R2,CSTDAY                                                        
         CR    R3,R2                                                            
         BE    GETST40                                                          
         BH    *+8                 BACK UP TO EFFECTIVE START DAY               
         SH    R2,=H'7'                                                         
         SR    R2,R3                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R2)                                      
*                                                                               
* NEXT, FIND THE ACTUAL EFFECTIVE START DAY BASED ON THE BUY'S START            
* DAY                                                                           
*                                                                               
GETST40  DS    0H                                                               
         MVC   CONEFFST,EFFST      SAVE OFF FOR END DATE LATER                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFST,DMCB+8                                         
         ZIC   R2,DMCB                                                          
         LR    R3,R4                                                            
*                                                                               
         CR    R3,R2                                                            
         BE    GETST80                                                          
         BH    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFST,EFFST,(R3)                                      
*                                                                               
GETST80  DS    0H                  START DATE ON/AFTER CONTRACT START?          
         CLC   EFFST,CONST                                                      
         BNL   GETST90                                                          
         LA    R0,SDTERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETST90  DS    0H                  START DATE AND DAY EQUAL?                    
         GOTO1 GETDAY,DMCB,EFFST,DMCB+8                                         
         ZIC   R3,DMCB                                                          
         CR    R3,R4                                                            
         BE    GETEX               YES, EXIT                                    
         LA    R0,SDYERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETEX    DS    0H                                                               
         SR    R0,R0                                                            
         B     EFFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* R4 START DAY                                                                  
* R5 END DAY                                                                    
*                                                                               
* ROUTINE TO CALULATE END DATES                                                 
*                                                                               
***********************************************************************         
GETEFFED NTR1                                                                   
         CLC   CSTDAY,CENDAY                                                    
         BH    GETED40                                                          
***********************************************************************         
* FOR STANDARD IN-WEEK ROTATORS                                                 
***********************************************************************         
         MVC   EFFEND,FLTEND                                                    
         LA    R2,7                                                             
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED30                                                          
         SR    R3,R2                                                            
         CR    R4,R5               USER SPECIFIED OOW BUY                       
         BH    GETED10                                                          
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED20                                                          
*                                                                               
GETED10  DS    0H                  BUY IS OOW                                   
         AH    R3,=H'7'                                                         
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BNE   GETED30                                                          
*                                                                               
GETED20  DS    0H                  FIND BEST FIT                                
         CLC   EFFEND,FLTEND       WENT PAST FLIGHT END DATE                    
         BNH   GETED30             BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED20                                                          
*                                                                               
GETED30  DS    0H                  FIND BEST FIT                                
         CLC   EFFEND,CONEND       WENT PAST CONTRACT END DATE                  
         BNH   GETED110            BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED30                                                          
         EJECT                                                                  
***********************************************************************         
* FOR OOWR                                                                      
***********************************************************************         
GETED40  DS    0H                                                               
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BNE   GETED50                                                          
         ZIC   R2,FLTDAYS          DID USER SPECIFY END DATE?                   
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         LTR   R2,R2                                                            
         BNZ   GETED80                                                          
         CLI   FLTWKS,1            SINGLE DATE?                                 
         BNE   GETED60             NO MUST BE -E                                
         ZIC   R2,FLTWKS           YES, FAKE -1W                                
         B     GETED55                                                          
*                                                                               
***********************************************************************         
* CASE: -NW (IE -3W)                                                            
***********************************************************************         
*                                                                               
GETED50  DS    0H                                                               
         ZIC   R2,FLTUIWKS         YES, CALCULATE END DATE                      
GETED55  DS    0H                                                               
         MH    R2,=H'7'                                                         
         BCTR  R2,0                                                             
         GOTO1 ADDAY,DMCB,EFFST,EFFEND,(R2)                                     
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED110                                                         
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED110                                                         
*                                                                               
***********************************************************************         
* CASE: -E                                                                      
***********************************************************************         
GETED60  DS    0H                                                               
         GOTO1 PERVERT,DMCB,EFFST,CONEND                                        
         ZICM  R2,DMCB+12,2        NUMBER OF WEEKS IN DATES                     
         OC    DMCB+10(2),DMCB+10                                               
         BZ    *+8                 IF REMAINDER, COUNT AS ANOTHER WEEK          
         LA    R2,1(R2)                                                         
         MH    R2,=H'7'            GET THE NUMBER OF DAYS TO END DATE           
         BCTR  R2,0                MAKE START DAY RELATIVE                      
         GOTO1 ADDAY,DMCB,CONEFFST,EFFEND,(R2)                                  
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED70                                                          
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* IF EFFECTIVE END DATE IS PAST CONTRACT END DATE, BACK UP UNTIL FIT            
*                                                                               
GETED70  DS    0H                                                               
         CLC   EFFEND,CONEND       WENT PAST CONTRACT END DATE                  
         BNH   GETED110            BACKUP UNTIL WITHIN FLIGHT RANGE             
         LA    R3,7                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
         B     GETED70                                                          
*                                                                               
***********************************************************************         
* CASE: -MMMDD (IE -MAY21)                                                      
***********************************************************************         
*                                                                               
GETED80  DS    0H                                                               
         MVC   EFFEND,FLTEND                                                    
*                                                                               
         ZIC   R2,FLTDAYS                                                       
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         LA    R3,7                                                             
         SR    R2,R3                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R2)                                    
*                                                                               
* FIND THE END DATE AS SPECIFIED BY THE CONTRACT END DAY                        
*                                                                               
         ZIC   R2,FLTDAYS                                                       
         SLL   R2,28                                                            
         SRL   R2,28                                                            
*                                                                               
         ZIC   R3,CSTDAY           ONE COMPLETE ROTATION MEANS                  
         BCTR  R3,0                THE CONTRACT END DAY IS ALWAYS               
         LTR   R3,R3               ONE LESS THAN CONTRACT START DAY             
         BNZ   *+8                 REGARLESS OF THE ACTUAL CONTRACT             
         LA    R3,7                END DATE                                     
*                                                                               
         CR    R2,R3                                                            
         BE    GETED90                                                          
         BL    *+8                                                              
         AH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* FIND THE ACTUAL EFFECTIVE END DAY BASED ON THE BUY'S END DAY                  
*                                                                               
GETED90  DS    0H                                                               
*        ZIC   R2,CENDAY                                                        
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R2,DMCB                                                          
         LR    R3,R5                                                            
         CR    R3,R2                                                            
         BE    GETED110                                                         
         BL    *+8                                                              
         SH    R3,=H'7'                                                         
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,EFFEND,EFFEND,(R3)                                    
*                                                                               
* VALID RANGE CHECKS                                                            
*                                                                               
GETED110 DS    0H                                                               
         CLC   EFFEND,CONEND                                                    
         BNH   GETED120                                                         
         LA    R0,EDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
GETED120 DS    0H                                                               
         CLC   EFFEND,EFFST                                                     
         BNL   GETED130                                                         
         LA    R0,ENBEFST                                                       
         B     EFFEXIT                                                          
*                                                                               
GETED130 DS    0H                  END DATE AND DAY EQUAL ?                     
         GOTO1 GETDAY,DMCB,EFFEND,DMCB+8                                        
         ZIC   R3,DMCB                                                          
         CR    R3,R5                                                            
         BE    GETED140            YES, EXIT                                    
         LA    R0,EDYERR           NO, ERROR                                    
         B     EFFEXIT                                                          
*                                                                               
GETED140 DS    0H                                                               
         BAS   RE,ADJUSTWK                                                      
*                                                                               
         CLI   FLTUIWKS,0          USER SPECIFIED NUMBER OF WEEKS?              
         BE    GETEDX                                                           
         CLC   FLTUIWKS,RBUYDTWK   ERROR IF ACTUAL NUMBER OF WEEKS              
         BE    GETEDX              AND SPECIFIED NUMBER OF WEEKS DO NOT         
*                                  MATCH                                        
         LA    R0,EDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
GETEDX   DS    0H                                                               
         B     GETEX                                                            
         EJECT                                                                  
***********************************************************************         
* ADJUST THE NUMBER OF WEEKS BY CALCULATING THE ACTUAL NUMBER OF WEEKS          
* THE BUY HAS SPANNED                                                           
***********************************************************************         
ADJUSTWK NTR1                                                                   
         LA    R3,1                                                             
         MVC   WORK(6),EFFST                                                    
*                                                                               
ADJWK10  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK,F'7'                                        
         CLC   WORK(6),EFFEND                                                   
         BH    ADJWK20                                                          
         LA    R3,1(R3)                                                         
         B     ADJWK10                                                          
*                                                                               
ADJWK20  DS    0H                                                               
         STC   R3,RBUYDTWK                                                      
         B     EFFEXIT                                                          
         DROP  R6,R7,R8                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
*  INSERT STATION CALL LETTERS FOR THIS CONTRACT INTO THE SCREEN                
*                                                                               
***********************************************************************         
         DC    0H'0'                                                            
DISPSTAS NMOD1 0,*DISPST*                                                       
         L     R8,ASPULAR          COVER WORKING STORAGE STARTING               
         USING MULTBUY,R8          AT RBUYREC WITH SPECIAL DSECT                
         L     RC,0(R1)            RESET A(WORK AREA)                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,AIO3                                                
*                                                                               
*  NO FURTHER CHECKING OF CONTRACT IS DONE IN THIS SECTION                      
*                                                                               
         GOTO1 VMOVEREC,(R1),AIO3,RCONREC                                       
*                                                                               
         LA    R5,RCONELEM         A(DESCRIPTIVE ELEMENT)                       
DISP0010 EQU   *                                                                
         ZIC   R0,1(R5)            FIND X'17' ELEMENT                           
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                MUST HAVE AN X'17'                           
         CLI   0(R5),X'17'         X'17' ELEMENT?                               
         BNE   DISP0010            NO                                           
         ZIC   R3,1(R5)            YES - GET ITS LENGTH                         
         SR    R2,R2               EVEN REGISTER OF PAIR                        
         BCTR  R3,0                SUBTRACT L(ELEMENT CODE BYTE)                
         BCTR  R3,0                SUBTRACT L(ELEMENT LEN  BYTE)                
         LA    R4,9                L(CONTROL ELEMENT)                           
         DR    R2,R4               DIVIDED LEN BY SIZE TO CALCULATE             
*                                     NUMBER OF ENTRIES                         
*                                                                               
*   R3 CONTAINS # OF CONTRACTS IN COMBO CONTROL ELEMENT                         
*                                                                               
         BAS   RE,REINSCRN         RESET SCREEN BEFORE HDG LOAD                 
*                                                                               
*  SET STATIONS IN COMBO CONTROL ELEMENT INTO HEADING                           
*                                                                               
         LA    R2,CBICMB1H         A(HDR OF 1ST STATION)                        
         LA    R4,RCONKSTA         A(STATION OF ORDER)                          
         GOTO1 INSERTIT,DMCB,(R4),(R2)                                          
         LA    R5,2(R5)            1ST STATION IN COMBO CTL ELT                 
DISP0020 EQU   *                                                                
         CLC   RCONKSTA,0(R5)      STA OF ORDER VS COMBO CONTROL                
         BE    DISP0030            SAME - ALREADY INSERTED - SKIP IT            
         ZIC   R0,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,R0                                                            
*                                                                               
*  MOVE STATION TO HEADING                                                      
*                                                                               
         GOTO1 INSERTIT,DMCB,(R5),(R2)                                          
DISP0030 EQU   *                                                                
         LA    R5,9(R5)            NEXT STATION IN CONTROL ELEMENT              
         BCT   R3,DISP0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   INSERTIT - INSERTS STATION CALL LETTERS INTO SCREEN, TURNS ON               
*     TRANSMIT BIT.  AFTER CALLS ARE INSERTED, THE RATES FIELDS IN              
*     THAT COLUMN ARE UNPROTECTED TO PERMIT ENTRY OF DATA.  THIS                
*     ELIMINATES ANY UNNECESSARY TABBING.                                       
*          P1    =  A(STATION CALL LETTERS IN COMBO CONTROL ELEMENT)            
*          P2    =  SCREEN FIELD HEADER                                         
*                                                                               
FRSTRATE EQU   CBIRAT1H-CBICMB1H   DISPLACEMENT: HDG TO 1ST RATE                
NEXTRATE EQU   CBIRAT5H-CBIRAT1H   DISPLACEMENT: LINE-TO-LINE                   
INSERTIT NTR1                                                                   
         L     R2,0(R1)            LOAD A(STA CALL LETTERS)                     
         L     R1,4(R1)            LOAD A(SCREEN FIELD HEADER)                  
         MVC   8(4,R1),0(R2)       INSERT STATION LETTERS                       
         CLI   4(R2),C' '          NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),X'00'         NO MEDIA?                                    
         BE    INSE0010            NO MEDIA                                     
         CLI   4(R2),C'T'          MEDIA = TELEVISION?                          
         BE    INSE0010            YES                                          
         MVI   12(R1),C'-'         INSERT HYPHEN                                
         MVC   13(1,R1),4(R2)      INSERT MEDIA                                 
INSE0010 EQU   *                                                                
         FOUT  (R1)                TURN ON TRANSMIT BIT                         
         LA    R1,FRSTRATE(R1)     BUMP TO 1ST RATE IN COLUMN                   
         LA    R4,4                LOOP CONTROL                                 
INSE0020 EQU   *                                                                
         NI    1(R1),X'DF'         TURN OFF PROTECT BIT                         
         FOUT  (R1)                TURN ON TRANSMIT BIT                         
         LA    R1,NEXTRATE(R1)     BUMP TO NEXT LINE'S RATE                     
         BCT   R4,INSE0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   REINITIALIZE ALL STATION HEADINGS, PROTECT ALL RATE FIELDS.                 
*     THE RATE FIELDS FOR A COLUMN ARE UNPROTECTED WHEN STATION                 
*     CALL LETTERS ARE INSERTED, BASED ON THE NUMBER OF CONTRACTS               
*     IN THE COMBO CONTROL ELEMENT.  THIS PREVENTS UNNECESSARY                  
*     TABBING.                                                                  
*                                                                               
REINSCRN NTR1                                                                   
         LA    R4,4                LOOP CONTROL                                 
         LA    R1,CBICMB1H                                                      
REIN0010 EQU   *                                                                
         XC    8(7,R1),8(R1)       BLANK OUT HEADING                            
         FOUT  (R1)                TRANSMIT SPACES                              
         ZIC   RF,0(R1)            BUMP TO NEXT FIELD                           
         AR    R1,RF                                                            
         BCT   R4,REIN0010         GO BACK FOR NEXT                             
REIN0020 EQU   *                                                                
         LA    R1,CBIRAT1H         1ST RATE FIELD ON SCREEN                     
         LR    R2,R1               SAVE A(SCREEN LINE)                          
         LA    R5,4                LOOP CONTROL WITHIN SCREEN                   
REIN0030 EQU   *                                                                
         LA    R4,4                LOOP CONTROL WITHIN LINE                     
REIN0040 EQU   *                                                                
         OI    1(R1),X'20'         TURN ON PROTECT BIT                          
         FOUT  (R1)                TURN ON TRANSMIT BIT                         
         ZIC   R0,0(R1)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R1,R0                                                            
         BCT   R4,REIN0040         DO 4 FIELDS ON LINE                          
         LA    R2,NEXTRATE(R2)     BUMP TO NEXT LINE                            
         LR    R1,R2               RELOAD START OF LINE                         
         BCT   R5,REIN0030         DO 4 LINES ON SCREEN                         
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*  CALCULATE DISPLACEMENT INTO COMBO CONTROL ELEMENT, AND EXTRACT               
*    THE APPROPRIATE CONTRACT NUMBER.  THEN GET THE 9'S COMPLEMENT              
*    OF IT, AND PASS IT BACK IN WORK2.                                          
*                                                                               
NEWCONNO NMOD1 0,*NEWCON*                                                       
*                                                                               
*  FIRST FIND POSITION IN COMBO CONTROL ELEMENT OF STATION OF                   
*    ORIGINAL ORDER, WHICH HAS BEEN PROCESSED FIRST                             
*                                                                               
         L     RC,0(R1)            RESET A(WORK SPACE)                          
         L     R8,4(R1)            RESET WORKSPACE DSECT                        
         USING MULTBUY,R8                                                       
         LA    R2,COMBOCTL+2       A(1ST STA IN COMBO CTL ELT)                  
         LA    RF,4                LOOP CONTROL                                 
         LA    RE,1                SET COUNTER                                  
NEWC0010 EQU   *                                                                
         CLC   CONKSTA,0(R2)       STATION OF ORDER FOUND IN ELT?               
         BE    NEWC0020            YES                                          
         LA    R2,9(R2)            NO  - CHECK NEXT ONE                         
         LA    RE,1(RE)                  INCREMENT COUNT                        
         BCT   RF,NEWC0010         GO BACK FOR IT                               
NEWC0020 EQU   *                                                                
         SR    R2,R2                                                            
         ZIC   R3,STACTR           CONTRACT COUNTER IN PROGRESS                 
         CR    R3,RE               IN PROG VS ORIG STA POSITION                 
         BH    NEWC0030            AFTER ORIGINAL STA POSITION:                 
*                                    NO ADDITIONAL ADJUSTMENT NEEDED            
         BCTR  R3,0                BEFORE ORIGINAL STA POSITION:                
*                                    ADDITIONAL ADJUSTMENT NEEDED               
NEWC0030 EQU   *                                                                
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         LA    R1,9                SIZE OF COMBO CONTROL CONTRACT ELT           
         MR    R2,R1               CALCULATE DISPLACEMENT                       
         LA    R2,COMBOCTL+2       A(COMBO CONTROL ELEMENT)                     
         AR    R2,R3               ADD DISPLACEMENT                             
         L     RE,=X'99999999'     CALC 9'S COMP OF CONTRACT #                  
         MVC   FULL,5(R2)          LOAD CONTRACT #                              
         L     RF,FULL                                                          
         SR    RE,RF               GET 9'S COMP                                 
         ST    RE,FULL             SAVE IT                                      
*                                                                               
         MVC   WORK2(4),FULL                                                    
         PACK  WORK2(1),FULL+3(1)  REVERSE THE COMPLEMENT                       
         PACK  WORK2+1(1),FULL+2(1)                                             
         PACK  WORK2+2(1),FULL+1(1)                                             
         PACK  WORK2+3(1),FULL(1)                                               
*                                                                               
         XIT1                                                                   
         DROP  R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  CALCULATE DISPLACEMENT INTO COMBO CONTROL ELEMENT, AND EXTRACT               
*    THE APPROPRIATE CONTRACT NUMBER.  RETRIEVE THE CONTRACT RECORD             
*    VIA THE 8C KEY.                                                            
*                                                                               
NEXTCON# NMOD1 0,*NEXTCO*                                                       
*                                                                               
*  FIRST FIND POSITION IN COMBO CONTROL ELEMENT OF STATION OF                   
*    ORIGINAL ORDER, WHICH HAS BEEN PROCESSED FIRST                             
*                                                                               
         L     RC,0(R1)            RESET A(WORK SPACE)                          
         L     R8,4(R1)            RESET WORKSPACE DSECT                        
         USING MULTBUY,R8                                                       
         LA    R2,COMBOCTL+2       A(1ST STA IN COMBO CTL ELT)                  
         LA    RF,4                LOOP CONTROL                                 
         LA    RE,1                SET COUNTER                                  
NEXC0010 EQU   *                                                                
         CLC   CONKSTA,0(R2)       STATION OF ORDER FOUND IN ELT?               
         BE    NEXC0020            YES                                          
         LA    R2,9(R2)            NO  - CHECK NEXT ONE                         
         LA    RE,1(RE)                  INCREMENT COUNT                        
         BCT   RF,NEXC0010         GO BACK FOR IT                               
NEXC0020 EQU   *                                                                
         SR    R2,R2                                                            
         ZIC   R3,STACTR           CONTRACT COUNTER IN PROGRESS                 
         CR    R3,RE               IN PROG VS ORIG STA POSITION                 
         BH    NEXC0030            AFTER ORIGINAL STA POSITION:                 
*                                    NO ADDITIONAL ADJUSTMENT NEEDED            
         BCTR  R3,0                BEFORE ORIGINAL STA POSITION:                
*                                    ADDITIONAL ADJUSTMENT NEEDED               
NEXC0030 EQU   *                                                                
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         LA    R1,9                SIZE OF COMBO CONTROL CONTRACT ELT           
         MR    R2,R1               CALCULATE DISPLACEMENT                       
         LA    R2,COMBOCTL+2       A(COMBO CONTROL ELEMENT)                     
         AR    R2,R3               ADD DISPLACEMENT                             
         L     RE,=X'99999999'     CALC 9'S COMP OF CONTRACT #                  
         MVC   FULL,5(R2)          LOAD CONTRACT #                              
         L     RF,FULL                                                          
         SR    RE,RF               GET 9'S COMP                                 
         ST    RE,FULL             SAVE IT                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           INSERT KEY ID                                
         MVC   KEY+21(2),TWAAGY    INSERT REP ID                                
         MVC   KEY+23(4),FULL      INSERT CONTRACT # IN KEY                     
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE PRESENT!!                      
         MVI   UPDATE,YES          SET RETRIEVE FOR UPDATE                      
         MVC   SAVEDADD,KEY+28     SAVE DISK ADDRESS                            
         GOTO1 VGETREC,DMCB,AIO3                                                
         GOTO1 VMOVEREC,DMCB,AIO3,RCONREC                                       
         XIT1                                                                   
         DROP  R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
ISTAKOV  NMOD1 0,**ISTO**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    WORK2,WORK2                                                      
                                                                                
* ACTION MBI OVERLAYS THE COMMENT FIELD, NEED TO READ COMMENT ELEMENT           
* FOR TAKEOVER STATION CALLS                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    TKOVNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
                                                                                
         OC    WORK2(L'MYSPACES),MYSPACES                                       
*                                                                               
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   TKOVNO              WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
         CLC   =C'-TV',CONSTA+4                                                 
         BE    TKOV20                                                           
         CLC   =C'-TV',CONSTA+3    CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-L',CONSTA+4                                                  
         BE    TKOV20                                                           
         CLC   =C'-L',CONSTA+3     CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-C',WORK2+7      CHECK IF COMBO                               
         BE    TKOV50                                                           
         CLC   =C'-C',WORK2+8                                                   
         BE    TKOV50                                                           
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   TKOV10                                                           
         MVI   WORK+5,C'M'                                                      
                                                                                
TKOV10   DS    0H                                                               
         CLC   WORK(6),CONSTA                                                   
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* COMPARE FOR TV                                                                
TKOV20   DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),CONSTA     XXXX OR XXXX-T OR XXXX-L                      
         BNE   TKOVNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BE    TKOVYES                                                          
         CLC   =C'-L',WORK+4                                                    
         BE    TKOVYES                                                          
         B     TKOVNO                                                           
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),CONSTA                                                   
         BNE   TKOVNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+3                                                    
         BE    TKOVYES                                                          
         CLC   =C'-L',WORK+3                                                    
         BE    TKOVYES                                                          
         B     TKOVNO                                                           
                                                                                
* CHECK FOR COMBO TAKEOVER                                                      
TKOV50   DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TKOVNO                                                           
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    TKOV60              PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   TKOVNO                                                           
                                                                                
TKOV60   DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BE    TKOVYES                                                          
         DROP  R6                                                               
                                                                                
TKOVNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
TKOVYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056RECNT38   06/04/03'                                      
         END                                                                    
