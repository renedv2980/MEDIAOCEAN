*          DATA SET RECNT18    AT LEVEL 068 AS OF 10/08/15                      
*PHASE T80218A                                                                  
*INCLUDE REFLSCAN                                                               
*INCLUDE RETIMVAL                                                               
         TITLE 'MULTIPLE BUY - EDIT - T80218'                                   
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT18 (T80218) --- MULTIPLE BUY - EDIT                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY:                            *             
*                                                                 *             
* 20AUG15 KWA BYPASS AM VALIDATION                                *             
* 09SEP04 HQ  SHOULDN'T UP CONTRACT MOD# FOR ACT/GRAPHNET         *             
* 15OCT02 HQ  BUG FIX FOR NON-SEQUENTIAL WEEK MULTIPLE BUYLINES   *             
* 30MAY00 BU  TRADE PROCESSING                                    *             
* 18APR00 SKU INCREASE REP TO SPOT TRANSFER LIMIT FROM 50 TO 169  *             
* 03FEB98 RHV DISABLE MCI HANDLING                                *             
* 07NOV97 JRD ALTERNATE CALENDAR BUCKETS                          *             
* 14JUL97 SKU ALLOW CANCEL FOR NETWORK CONTRACTS                  *             
* 24APR97 BU  OPTIONAL CONTRACT TYPE FOR REP TO SPOT TRANSFER     *             
* 09DEC96 SKU MAX LENGTH OF 180 SECONDS/MINUTES                   *             
* 03OCT96 SKU SUPPORT LOW POWER STATION                           *             
* 10SEP96 SKU FIX CC END TIME BUG AND SCAN BUG                    *             
* 05SEP96 SKU DON'T DIE IF SEND ELEMENT DOESN'T EXISTS            *             
* 29JUL96 SKU FIX OOWR BUG IN MBI/MCI                             *             
* 02MAR96 SKU MAKEGOOD BUG FIX. ENABLE MCI/CAN                    *             
* 23SEP95 SKU 2K CONTRACT SUPPORT                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80218   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80218,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
         TM    TWAFLAGS,X'08'      REP USING 'DAILY PACING'?                    
         BNO   MAIN0020            YES                                          
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
MAIN0020 EQU   *                                                                
*                                                                               
         CLC   =C'MBI',MBYACT                                                   
         BE    MADD                                                             
         DC    H'0'                                                             
*                                                                               
MADD     DS    0H                                                               
         L     R8,ASPULAR          COVER WORKING STORAGE STARTING               
         USING MULTBUY,R8          AT SPOOLAR WITH SPECIAL DSECT                
         ST    R5,RELO                                                          
*                                                                               
         LA    R2,MBYACTH          READ CONTRACT IN LOCKED FOR                  
         LA    R3,BACERR           UPDATE AND TEST FOR DISPLAY                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  ONLY STATUS (01 BIT) ON CONTRACT             
         DROP  RF                                                               
         MVI   UPDATE,YES                                                       
         GOTO1 VGETREC,DMCB,AIO4                                                
         L     R4,AIO4                                                          
         TM    29(R4),X'01'                                                     
         BO    ERROR                                                            
         GOTO1 VMOVEREC,(R1),AIO4,RCONREC                                       
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
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE CONTRACT?                              
         BNO   MADD0005            NO                                           
         OI    BUYFLAGS,X'80'      YES - TURN ON INDICATOR                      
         B     MADD0008                                                         
         DROP  R6                                                               
MADD0005 EQU   *                                                                
*                                  CASH ORDERS:                                 
*                                                                               
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    MADD0008            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    MADD0008            YES - CAN BE CASH+TRADE                      
         TM    RCONMODR,X'20'      NO  - TV: FIRST BUYLINE ENTERED?             
         BNO   MADD0008            NO  - CAN BE EITHER CASH OR TRADE            
*                                     NOT SET YET                               
         OI    BUYFLAGS,X'40'      CASH ORDER FOR TV FOUND                      
MADD0008 EQU   *                                                                
         CLI   RCONKSTA+4,C' '     IF TV, NEED SPECIAL TESTS                    
         BE    MADD10                                                           
         CLI   RCONKSTA+4,C'L'     IF TV, NEED SPECIAL TESTS                    
         BNE   MADD30                                                           
*                                                                               
MADD10   DS    0H                                                               
         TM    RCONMODR+1,X'80'    IF 'ACE' THERE ARE SOME                      
         BZ    MADD30              SPECIAL TESTS TO BE DONE                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
*                                                                               
* CAN'T MAKE CHANGES IF OTHER SIDE IS IN PROCESS OF MAKING CHANGES              
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAACCS,C'$'        STATION                                      
         BE    MADD20                                                           
         DROP  RF                                                               
                                                                                
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    ERROR                                                            
         B     MADD30                                                           
                                                                                
MADD20   DS    0H                                                               
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         TM    RCONSENF,X'20'      X'20'=REP VERS. NOT ADVANCED                 
         BZ    ERROR                                                            
         DROP  R6                                                               
         SPACE 1                                                                
MADD30   DS    0H                                                               
***>     LA    R3,TYPBERR          PROHIBIT BUYS ON TYPE B                      
***>     CLI   RCONTYPE,C'B'        OR CANCELLED CONTRACTS.                     
***>     BE    ERROR                                                            
         SPACE 1                                                                
         CLC   CONADV(3),=C'GEN'   NO BUYS ON GENERAL AVAILS                    
         BNE   MADD50              I.E. ADV=GEN, CATEGORY=ZZ                    
         CLC   CONCAT,=C'ZZ'                                                    
         BNE   MADD50                                                           
         LA    R3,192              NO BUYING ON GENERAL AVAILS                  
         B     ERROR                                                            
         SPACE 1                                                                
MADD50   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   MADD60                                                           
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN'                                              
         BE    ERROR               CANCELLED CONTRACT                           
         DROP  R6                                                               
         SPACE                                                                  
MADD60   EQU   *                                                                
         CLI   RCONKSTA+4,C'T'     TV  - SKIP BOP REQUIREMENT                   
         BE    MADD80                                                           
         CLI   RCONKSTA+4,C'L'     TV  - SKIP BOP REQUIREMENT                   
         BE    MADD80                                                           
         CLI   RCONKSTA+4,C' '     TV  - SKIP BOP REQUIREMENT                   
         BE    MADD80                                                           
*                                  REQUIRE BOP: NEW RADIO CONTRACTS             
         CLC   MBYACT(4),=C'MBIX'  SKIP BOP CHECK FOR SPECIAL                   
         BE    MADD80              OVERRIDE ACTION                              
         SPACE                                                                  
         XC    KEY,KEY             BUILD KEY FOR STATION RECORD                 
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    MADD70                                                           
         DC    H'0'                                                             
MADD70   GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    RSTASTAT,X'80'      TEST FOR BOP CHECK OVERRIDE                  
         BO    MADD80              SKIP CHECK.                                  
         SPACE                                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    MADD80              CONTRACT HAS BOP                             
         GOTO1 =A(ISTAKOV),DMCB,(RC),RR=Y                                       
         BZ    MADD80                                                           
         LA    R3,BOPERR                                                        
         B     ERROR                                                            
         SPACE                                                                  
MADD80   MVC   DMCB+4(4),=X'D9000A03'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VDAYVAL,DMCB        FIND ADDRESSES OF CORE-RESIDENT              
*                                                                               
* EDIT OPTIONAL FLIGHT NUMBER FIELD                                             
*                                                                               
FLNUM    LA    R2,MBYFLNH                                                       
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
FLT      LA    R2,MBYFLTH                                                       
         CLI   5(R2),0             TEST FOR MISSING INPUT                       
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
*  EDIT THE 2 FLIGHT FIELDS AS 1 LONG FIELD                                     
         SPACE 1                                                                
         MVC   WORK3(L'MBYFLT+8),MBYFLTH                                        
         SR    RE,RE                                                            
         IC    RE,MBYFLTH+5        LENGTH OF FIELD 1                            
         SR    RF,RF                                                            
         IC    RF,MBYFLT2H+5       LENGTH OF FIELD 2                            
         LA    R4,WORK3+8(RE)      END OF FIELD 1                               
         LTR   RF,RF                                                            
         BZ    FLT1                                                             
         SPACE 1                                                                
         LA    R3,WORK3+7(RE)                                                   
         CLI   0(R3),C'*'          LAST CHAR. ON LINE 1 MUST BE *               
         BE    *+12                IF THERE'S A LINE 2                          
         LA    R3,15               NEED COMMA AT END OF LINE                    
         B     ERROR                                                            
         SPACE 1                                                                
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),MBYFLT2     FIELD 2                                      
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
SETUP    LA    R4,BUYLINE          POINT R4 AT RECORD AREA                      
BUYL     USING RBUYKEY,R4                                                       
         MVI   BUYL.RBUYKTYP,X'0B'                                              
         MVC   BUYL.RBUYKREP,REPALPHA REP CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   BUYL.RBUYKCON,TWACNUM CONTRACT NUM                               
         DROP  RF                                                               
         MVC   BUYL.RBUYKPLN,=3X'FF' NON-PLAN BUY                               
         MVC   BUYL.RBUYLEN,=H'77' L' KEY AREA PLUS FIRST ELEMENT               
         LA    R2,MBYDAY1H         POINT R2 TO FIRST LINE                       
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
         LA    RE,MBYENDH          TEST TO SEE IF END OF SCREEN                 
         CR    R2,RE               HAS BEEN REACHED                             
         BE    NEXTLIN             YES-GO TO BUCKET UPDATE +I/O RTNS            
         LA    RF,LINELEN(R2)      NO-POINT RF TO NEXT LINE                     
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
         B     NEXTLIN             NOTHING ON REMAINDER OF SCREEN               
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
         SPACE                                                                  
FRSTEL2  MVI   BUYL.RBUYCODE,X'01'                                              
         MVI   BUYL.RBUYELLN,X'2B'                                              
         MVC   BUYL.RBUYFLT,FLTNUM                                              
*                                                                               
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
         B     TIMED                                                            
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
*                                                                               
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
*                                                                               
         B     TIMED5              BY PASS AM VALIDATION                        
*                                                                               
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
*                                                                               
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
         B     LENE0220                                                         
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
*                                                                               
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0100 EQU   *                                                                
*                                                                               
         CH    R0,=H'120'                                                       
         BNH   LENE0120                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
*                                                                               
LENE0120 CH    R0,=H'999'          ONLY 3 DIGITS ALLOWED                        
         BH    ERROR                                                            
         STCM  R0,3,BUYL.RBUYDUR                                                
         B     LENE0220                                                         
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
***      CLI   RCONKSTA+4,C'C'     COMBO STN?                                   
***      BE    LENE0200            THEN DON'T CARE                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0160            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0180            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0160 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0200                                                         
LENE0180 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0200 EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,BUYL.RBUYDUR+1                                                
         OI    BUYL.RBUYDUR,X'80'  MINUTE INDICATOR                             
*                                                                               
LENE0220 DS    0H                  LENGTH MUST BE LESS THAN 180                 
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
         B     RATEDIT                                                          
         SPACE                                                                  
SPTSED2  OC    LASTBUY,LASTBUY     FIRST TIME                                   
         BZ    SPTSEDR             YES-ERROR                                    
         SPACE                                                                  
         L     R5,LASTBUY                                                       
         MVC   BUYL.RBUYNW,RBUYNW-RBUYKEY(R5)                                   
         B     RATEDIT                                                          
         SPACE                                                                  
SPTSEDR  LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE 2                                                                
* EDIT COST FIELD                                                               
*                                                                               
RATEDIT  L     R2,THISLINE                                                      
         LA    R2,RTE(R2)                                                       
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BNE   REDI0040                                                         
         OC    LASTBUY,LASTBUY                                                  
         BZ    REDI0020                                                         
         L     R5,LASTBUY                                                       
         MVC   BUYL.RBUYCOS,RBUYCOS-RBUYKEY(R5)                                 
         TM    RBUYFLG2-RBUYKEY(R5),X'02'                                       
*                                  LAST = TRADE BUY?                            
         BNO   REDI0100            NO                                           
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
         OI    BUYL.RBUYFLG2,X'02' YES                                          
         B     REDI0100                                                         
         SPACE                                                                  
REDI0020 LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
REDI0040 LA    R3,RATERR                                                        
         XC    DMCB+4(4),DMCB+4    CLEAR PARA 2                                 
         ZIC   RF,5(R2)            CHECK FOR 'TRADE' FLAG                       
         LA    RE,8(R2)            SET A(CASH VALUE FIELD)                      
         AR    RE,RF               ADD L(CASH VALUE INPUT)                      
         BCTR  RE,0                BACK UP 1 CHARACTER                          
         CLI   0(RE),C'T'          'TRADE' INDICATOR?                           
         BE    REDI0050            YES                                          
         TM    BUYFLAGS,X'80'      NO  - TRADE CONTRACT?                        
         BNO   REDI0060            NO                                           
         LA    R1,8(R2)            YES - NEED TO INSERT 'T'                     
         AR    R1,RF               ADD L(INPUT DATA)                            
         MVI   0(R1),C'T'          INSERT 'T' IN LAST CHARACTER                 
         B     REDI0055            YES                                          
REDI0050 EQU   *                                                                
         TM    BUYFLAGS,X'40'      CASH ORDER IN PROCESS?                       
         BNO   REDI0052            NO  - OKAY TO BOOK TRADE                     
         LA    R3,TRDINCSH         YES - NO TRADE IN CASH                       
         B     ERROR                                                            
REDI0052 EQU   *                                                                
         BCTR  RF,0                YES - DECREMENT LENGTH BY 1                  
REDI0055 EQU   *                                                                
         STC   RF,DMCB+7           INSERT INTO PARA LIST                        
*                                                                               
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
         OI    BUYL.RBUYFLG2,X'02' SET TRADE INDICATOR IN BUY                   
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    REDI0080            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    REDI0080            YES - CAN BE CASH+TRADE                      
*                                  ONLY SET FOR TV ORDERS                       
         GOTO1 =A(SETRADE),DMCB,(RC),RR=Y                                       
*                                                                               
         B     REDI0080                                                         
REDI0060 EQU   *                                                                
         MVC   DMCB+7(1),5(R2)                                                  
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    REDI0080            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    REDI0080            YES - CAN BE CASH+TRADE                      
         OI    BUYFLAGS,X'40'      NO  - TURN ON 'CASH ORDER' FLAG              
REDI0080 EQU   *                                                                
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   BUYL.RBUYCOS,DMCB+4                                              
*                                                                               
REDI0100 DS    0H                                                               
*                                                                               
SECEL    EQU   *                   ADD X'02' ELEMENT TO RECORD                  
         GOTO1 VADDELEM,DMCB,(R4),WORK2                                         
*                                                                               
* CONSTRUCT EFFECTIVE DATE ELEMENTS AND ADD TO RECORD                           
*                                                                               
THIRDEL  L     R2,THISLINE                                                      
*        BAS   RE,EFFDAT           CONSTRUCT X'03' ELEMENTS                     
         GOTO1 =A(EFFDAT),RR=Y                                                  
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
*                                                                               
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
         LA    R6,WORK2                                                         
WK2      USING RBUYSPEL,R6                                                      
         MVC   WK2.RBUYSPCD(2),=X'0830'                                         
         MVC   WK2.RBUYSPAG,TWASPAG   SPOTPAK AGENCY POWER CODE                 
         MVC   WK2.RBUYSPMD,TWASPMD   SPOTPAK MEDIA CODE                        
         MVC   WK2.RBUYSPCL,TWASPCL   SPOTPAK CLIENT CODE                       
         MVC   WK2.RBUYSPPD,TWASPPD   SPOTPAK PRODUCT CODE                      
         MVC   WK2.RBUYSPES,TWASPES   SPOTPAK ESTIMATE NUMBER                   
         MVC   WK2.RBUYSPPP,TWASPPP   SPOTPAK PIGGY PRODUCT CODE                
         MVC   WK2.RBUYSPP1,TWASPP1   SPOTPAK PRODUCT 1 SPLIT                   
         MVC   WK2.RBUYSPP2,TWASPP2   SPOTPAK PRODUCT 2 SPLIT                   
         MVC   WK2.RBUYSPST,RCONKSTA  STATION CALL LETTERS                      
         MVC   WK2.RBUYSADV,RCONKADV  REPPAK ADVERTISER CODE                    
         MVC   WK2.RBUYSPRD,RCONPRD   REPPAK PRODUCT CODE                       
         DROP  WK2,RF                                                           
         GOTO1 VADDELEM,DMCB,(R4),WORK2   ADD SPOTPAK INTERFACE ELEM            
*                                                                               
RESET    ST    R4,LASTBUY          SAVE ADDR OF LAST BUY RECORD                 
         ZIC   R5,LINCNT                                                        
         LA    R5,1(R5)            INCREMENT COUNT OF BUY RECORDS               
         STC   R5,LINCNT                                                        
         SPACE                                                                  
         LA    R4,L'BUYLINE(R4)    POINT R4 TO NEXT BUY RECORD AREA             
         L     R2,THISLINE         POINT R2 TO NEXT LINE                        
         LA    R2,LINELEN(R2)      AND                                          
         ST    R2,THISLINE         UPDATE ADDR OF LINE TO BE EDITED             
         B     SCAN                GO AND CHECK NEXT LINE FOR DATA              
         EJECT                                                                  
* READ FOR NEXT LINE NUMBER, THEN TEST TO SEE THAT ALL BUYS CAN                 
* BE ADDED WITHIN 255 LIMIT                                                     
*                                                                               
NEXTLIN  XC    KEY,KEY                                                          
         MVC   KEY(22),BUYLINE     BORROW REP/CON NO/                           
         MVI   BYTE3,0             BYTE WILL CONTAIN HIGHEST NUM FOUND          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(22),KEY                                                  
         BNE   NEXTLIN4            ADDING FIRST BUY                             
         CLI   KEY+26,X'FF'        PLAN RECORD                                  
         BE    NEXTLIN2            YES                                          
         MVC   BYTE3,KEY+26        SAVE FIRST LINE NUMBER FOUND                 
         SPACE                                                                  
NEXTLIN2 OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         CLC   KEYSAVE(22),KEY                                                  
         BNE   NEXTLIN4            READ FINISHED                                
         CLI   KEY+26,X'FF'        PLAN RECORD                                  
         BE    NEXTLIN2                                                         
         CLC   BYTE3,KEY+26        CHECK HIGHEST SO FAR VS. KEY                 
         BNL   *+10                                                             
         MVC   BYTE3,KEY+26        NEW HIGHEST SO FAR                           
         B     NEXTLIN2                                                         
         SPACE                                                                  
NEXTLIN4 ZIC   R6,BYTE3            HIGHEST NUMBER                               
         ZIC   RF,LINCNT           NUMBER OF NEW LINES                          
         LR    R5,R6                                                            
         AR    R5,RF                                                            
         CH    R5,=H'255'          WILL LINES FIT                               
         BH    NEXTLIN6            NO                                           
         LA    R6,1(R6)            YES-INCREMENT R6 TO FIRST NEW NUMBER         
         STC   R6,BYTE3                                                         
         B     UPCON                                                            
         SPACE                                                                  
NEXTLIN6 SH    R5,=H'255'          CALCULATE POINTER TO LINE THAT               
         SR    RF,R5               WOULD EXCEED 255 LIMIT                       
         LA    RE,LINELEN                                                       
         MR    RE,RE               FIND DISPLACEMENT TO THAT LINE               
         LA    R2,MBYDAY1H                                                      
         LA    R2,0(R2,RF)         POINT R2 TO LINE                             
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* ROUTINE TO UPDATE MODIFICATION DATE AND NUMBER ON CONTRACT                    
UPCON    MVI   TAREQ,0             INITIALIZE T/A REQ INDICATOR                 
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CONTRACT                        
         BNZ   *+12                                                             
         CLI   RCONMOD,X'FF'                                                    
         BE    UPCON4                                                           
         CLI   BYTE3,1                                                          
         BNE   UPCON4                                                           
         CLC   RCONCREA,TODAY                                                   
         BE    UPCON2                                                           
         OI    TAREQ,X'01'         T/A REQUEST INDICATOR                        
         MVC   RCONCREA,TODAY      LINE 1 ONLY                                  
         SPACE                                                                  
UPCON2   DS    0H                                                               
         OI    RCONMODR,X'20'+X'10' X'20'=BUYS ADDED-LINE 1                     
*                                   X'10'=NOT PENDING/ BUYLINE(S) WERE          
*                                         ADDED AT ONE POINT.  THIS BIT         
*                                         DOES NOT GET RESET.                   
         B     UPCON6              LINE 1 DOES NOT BUMP MOD NUMBER              
         SPACE                                                                  
UPCON4   BAS   RE,BUMPNUM                                                       
         SPACE                                                                  
UPCON6   B     BUYUP                                                            
         SPACE 2                                                                
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
         BNZ   BUMP10                                                           
         ZIC   R1,RCONMOD          INCREMENT MOD NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY      SET MODIFICATION DATE                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE COMPLETES EACH BUY RECORD'S KEY, X'01' ELEMENT AND                    
* UPDATES CONTRACT BUCKETS.  AT ENTRY BYTE3 HAS FIRST LINE NUMBER               
BUYUP    ZIC   R2,LINCNT           COUNTER                                      
         LA    R4,BUYLINE          POINTER TO BUY RECORDS                       
*                                                                               
         ZIC   R5,BYTE3                                                         
BUYKEY   STC   R5,BUYL.RBUYKMLN                                                 
         STC   R5,BUYL.RBUYKLIN                                                 
         MVC   BUYL.RBUYCHGI(2),=C'A ' SET LAST CHANGE INDICATOR                
         MVC   BUYL.RBUYCREA,TODAY                                              
         MVC   BUYL.RBUYKMOD,RCONMOD BUY MOD NUMBER IS SAME AS CONTRACT         
*        CLI   RCONMOD,X'FF'       UNLESS CON MOD NUM IS 255 WHICH              
*        BNE   *+8                 CAUSES BUY MOD TO BE SET TO ZERO.            
*        MVI   BUYL.RBUYKMOD,0                                                  
*                                                                               
* UPDATE REP VERSION NUMBER AND UNCONFIRM IF NECESSARY                          
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    BUYTOT                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    UPVER50                                                          
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
                                                                                
UPVER50  DS    0H                                                               
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
         BZ    UPVER70                                                          
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
UPVER70  OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         SPACE 1                                                                
BUYTOT   DS    0H                                                               
         MVC   WORK(4),VGTBROAD    BUILD ROUTINE LIST FOR REGENBUC              
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
                                                                                
****     GOTOX (RFGENBUC,VREPFACS),DMCB,(R4),WORK2,WORK                         
****     XC    WORK2,WORK2                                                      
                                                                                
         GOTO1 =A(BUCKUP),DMCB,(R4),(RC),RR=RELO                                
         SPACE                                                                  
BUYTOTX  LA    R4,L'BUYLINE(R4)    POINT TO NEXT RECORD                         
         LA    R5,1(R5)            INCREMENT LINE NUMBER                        
         BCT   R2,BUYKEY                                                        
         EJECT                                                                  
* WRITE BACK BUY RECORDS AND PUT CONTRACT                                       
* CHECK ENTIRE BLOCK OF BUYLINES BEFORE GOING TO DATAMGR                        
BUYIO    LA    R4,BUYLINE          POINTER                                      
         ZIC   R3,LINCNT           COUNTER                                      
         SPACE                                                                  
BUYIO2   DS    0H                                                               
* MAKE SURE NO MORE THAN 50 SPTS FOR SPOTPAK XFER                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUYIO3                                                           
         LH    R1,BUYL.RBUYTSPT                                                 
         LTR   R1,R1               SKIP CHECK IF NEGATIVE                       
         BM    BUYIO3                                                           
         CH    R1,=H'169'          MAX OF 169 SPOTS                             
         BNH   BUYIO3                                                           
         LA    R2,MBYACTH                                                       
         LA    R3,266                                                           
         B     ERROR                                                            
*                                                                               
BUYIO3   LA    R4,L'BUYLINE(R4)                                                 
         BCT   R3,BUYIO2                                                        
*                                                                               
BUYIO4   LA    R4,BUYLINE          POINTER                                      
         ZIC   R3,LINCNT           COUNTER                                      
*                                                                               
BUYIO5   DS    0H                                                               
         BAS   RE,ADDEXTRA         ADD EXTRA DESCRIPTION ELEMENT                
*                                                                               
         GOTO1 VADDREC,DMCB,(R4)                                                
* UPDATE BUY PASSIVE POINTERS                                                   
         MVC   KEYSAVE,0(R4)                                                    
         MVC   KEYSAVE+28(4),KEY                                                
         MVC   KEY,KEYSAVE                                                      
         GOTO1 VLOAD,DMCB,(X'19',0),(R4)                                        
*                                                                               
         LA    R4,L'BUYLINE(R4)                                                 
         BCT   R3,BUYIO5                                                        
         SPACE                                                                  
CONMDUP  MVC   CONMOD,MYSPACES     DISPLAY CON MOD NUMBER                       
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
         SPACE                                                                  
PUTCON   LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR  RE-READ CON SINCE THERE HAS BEEN             
         DROP  RF                                                               
         MVI   UPDATE,C'Y'         INTERVENING I/O                              
         GOTO1 VGETREC,(R1),AIO4                                                
         GOTO1 VPUTREC,(R1),RCONREC                                             
*                                                                               
*  WITH CONTRACT RECORD REWRITTEN, DETERMINE IF AN 'EC' KEY NEEDS               
*     TO BE GENERATEDIT. IF SO, DO IT.                                          
*                                                                               
         CLI   TRUFLAG,C'Y'        NEED 'EC' KEY?                               
         BNE   PUTCON1             NO  -                                        
****>    GOTO1 =A(GENECKEY),DMCB,(RC),RR=RELO                                   
*                                                                               
*    AS THIS FACILITY ONLY SUPPORTS RADIO MULTIPLE BUYS, AND THE SAR            
*       REPORT FOR RADIO USES THE 'S' (STATION KEY) SEQUENCE RATHER             
*       THAN THE 'E' (EC/SAR KEY) SEQUENCE, AN SAR KEY IS NOT PUT               
*       OUT.                                                                    
*                                                                               
* PRODUCE CONTRACT HEADER MESSAGE                                               
*                                                                               
PUTCON1  EQU   *                                                                
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
         BE    PUTCON2             YES                                          
         MVI   0(R3),DASH                                                       
         LA    R3,1(R3)                                                         
         EDIT  (R7),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         SPACE                                                                  
PUTCON2  DS    0H                                                               
         LA    R0,TEMP                                                          
         SR    R3,R0                                                            
         GOTO1 VDISMSG,DMCB,(0,47),0,0,((R3),TEMP)                              
         B     EXXMOD                                                           
         DROP  BUYL                                                             
         EJECT                                                                  
***********************************************************************         
* ADD EXTRA DESCRIPTION ELEMENT X'80'                                           
***********************************************************************         
ADDEXTRA NTR1                                                                   
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
         DS    CL1000              FORCE USE OF 2ND BASE REG                    
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         ORG   CONSHRTH                                                         
*                                                                               
       ++INCLUDE RECNTF3D                                                       
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
EFFDMODE DS    C                   FOR EFFECTIVE DATE CHANGE                    
LASTLIN  DS    X                   LAST LINE WRITTEN BACK                       
MODNUM   DS    X                   UPDATED CONTRACT MOD NUMBER                  
ERRFLAG  DS    C                   SET WHEN ERROR IS ENCOUNTERED                
ROLLSW   DS    C                   SWITCH SET WHEN END DATE MUST                
*                                  BE ROLLED TO KEEP N WEEKS INTACT             
VDAYVAL  DS    V                                                                
*VTIMVAL  DS    V                                                               
LASTBUY  DS    A                   PREVIOUS BUY RECORD                          
THISBUY  DS    A                   CURRENT BUY RECORD                           
THISLINE DS    A                   CURRENT LINE ADDRESS                         
LINCNT   DS    C                   NUMBER OF BUY LINES                          
SAVEKEY  DS    CL32                LAST KEY USED IN SEQUENTIAL READ             
ENTRIES  DS    CL500                                                            
BUYLINE  DS    10CL450             BUY RECORD AREA                              
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
LINELEN  EQU   MBYDAY2H-MBYDAY1H                                                
DAY      EQU   0                                                                
TIME     EQU   MBYTIMH-MBYDAY1H                                                 
TYP      EQU   MBYTYPH-MBYDAY1H                                                 
SPTS     EQU   MBYSPTSH-MBYDAY1H                                                
RTE      EQU   MBYRATEH-MBYDAY1H                                                
COM      EQU   MBYCOMH-MBYDAY1H                                                 
         EJECT                                                                  
         DROP  RB                                                               
         CSECT                                                                  
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
*                                                                               
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
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     EXXMOD                                                           
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET TRADE ORDER FLAG                                                          
***********************************************************************         
SETRADE  NMOD1 0,*SETR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OI    RCONRF1,X'08'       SET TRADE CONTRACT                           
         OI    BUYFLAGS,X'80'      TURN ON 'TRADE CONTRACT' FLAG                
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
ISTAKOV  NMOD1 0,**ISTO**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    WORK2,WORK2                                                      
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
         CLC   WORK(4),CONSTA     XXXX OR XXXX-T                                
         BNE   TKOVNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BE    TKOVYES                                                          
         CLC   =C'-L',WORK+4                                                    
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),CONSTA                                                   
         BNE   TKOVNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+3                                                    
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
         EJECT                                                                  
GENECKEY NMOD1 0,*ECKEY**                                                       
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'EC'           ESTABLISH SAR KEY                            
         LR    R3,RA                                                            
         AH    R3,=Y(TWAWORKQ)                                                  
*                                                                               
         USING TWAWORK,R3                                                       
*                                                                               
         MVC   KEY+23(4),TWACNUM                                                
         MVC   KEY+21(2),REPALPHA                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    GENEXIT             ALREADY THERE - DON'T READD                  
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
GENEXIT  EQU   *                                                                
         XMOD1                                                                  
*                                                                               
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE FOR CONSTRUCTING EFFECTIVE DATE ELEMENTS FROM                     
* ENTRIES GENERATEDIT BY REFLSCAN                                               
***********************************************************************         
EFFDAT   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*EFFDAT*'                                                    
*                                                                               
         ST    R4,TEMP                                                          
         ZIC   R2,NUMFLTS                                                       
         LA    R6,ENTRIES          R6 POINTS TO ENTRIES                         
         USING FLTENTD,R6                                                       
         XC    WORK2,WORK2                                                      
         LA    R7,WORK2            R7 POINTS TO ELEMENTS                        
         USING RBUYDTEL,R7                                                      
*                                                                               
EFF10    SR    R4,R4                                                            
         SR    R5,R5                                                            
         CLC   =C'MB',MBYACT                                                    
         BNE   EFF20                                                            
         L     RF,TEMP                                                          
         USING RBUYKEY,RF                                                       
         IC    R4,RBUYSTED                                                      
         DROP  RF                                                               
         B     *+12                                                             
EFF20    LA    RE,IOAREA                                                        
         IC    R4,RBUYSTED-RBUYKEY(RE)                                          
         SRDL  R4,4                R4 HAS START DAY                             
         SRL   R5,28               R5 HAS END DAY                               
*                                                                               
EFF30    MVI   RBUYDTEL,X'03'      ELEMENT CODE                                 
         MVI   RBUYDTLN,LTHRDEL    ELEMENT LENGTH                               
         MVC   RBUYDTWK,FLTWKS     NUMBER OF WEEKS                              
         LR    R3,R4                                                            
         CLC   FLTSTART,CONST      DOES ENTRY START IN FIRST WEEK OF            
         BH    EFF60               CONTRACT-NO                                  
         CLM   R3,1,CSTDAY         DOES BUY ST DAY PRECEDE CONST DAY            
         BNL   EFF60               NO                                           
*                                                                               
EFF40    LA    R3,7(R3)            PUSH BACK EFFECTIVE START 1 WEEK             
         TM    FLTCNTL,SONLY       INPUT OF JUST 'S' AND BUY START              
         BO    EFF50               BEFORE CON ST IS AN ERROR                    
         TM    FLTCNTL,NFORM                                                    
         BNO   *+12                FOR N WEEKS FORMAT, SET SWITCH TO            
         MVI   ROLLSW,YES          ROLL BACK END DATE SO SCHEDULE WILL          
         B     EFF60               SPAN EXACTLY N WEEKS                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                                                              
         DC    H'0'                FOR DATES FORMAT, DECREASE WEEKS             
*        ZIC   R1,RBUYDTWK         SPANNED BY ONE TO REFLECT ROLLED             
*        BCTR  R1,0                BACK START DATE                              
*        STC   R1,RBUYDTWK                                                      
         B     EFF60                                                            
*                                                                               
EFF50    LA    R0,SDTERR                                                        
         B     EFFEXIT                                                          
*                                                                               
EFF60    DS    0H                                                               
         BCTR  R3,0                SUBTRACT ONE FOR ADDING TO MONDAY            
         LTR   R3,R3               IS BUY START DAY MONDAY                      
         BZ    EFF70               YES                                          
         GOTO1 ADDAY,DMCB,FLTSTART,EFFST,(R3)                                   
         B     EFF80                                                            
*                                                                               
EFF70    MVC   EFFST,FLTSTART      ALREADY MONDAY                               
*                                                                               
EFF80    DS    0H                                                               
         BAS   RE,GETEFFST                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*        GOTO1 ADDAY,DMCB,EFFST,EFFST,F'7'                                      
*                                                                               
EFF85    DS    0H                                                               
         LR    R3,R5                                                            
         CLC   FLTEND,CONEND       DOES ENTRY END IN LAST WEEK OF CON           
         BL    EFF110              NO                                           
         CLM   R3,1,CENDAY         BUY END DAY VS. CONTRACT END DAY             
         BH    EFF90               ITS HIGHER-ROLL BACK EFFECTIVE DATES         
         B     EFF110                                                           
*                                                                               
EFF90    SH    R3,=H'14'           SUBTRACT TO FIND NUMBER FOR ADDAY            
*        ZIC   R0,RBUYDTWK         DECREMENT NUMBER OF WEEKS                    
*        SH    R0,=H'1'                                                         
*        BZ    EFF100                                                           
*        STC   R0,RBUYDTWK                                                      
         TM    FLTCNTL,NFORM       RECOGNIZE END DATE ERROR IF N WEEKS          
         BZ    EFF120              CANNOT FIT WITHIN CONTRACT                   
*                                                                               
EFF100   LA    R0,EDTERR           ENT IS LAST WEEK ONLY AND HAD TO             
         B     EFFEXIT             BE ROLLED BACK                               
*                                                                               
EFF110   SH    R3,=H'7'                                                         
         BM    EFF120              DAY IS NOT SUNDAY                            
         MVC   EFFEND,FLTEND                                                    
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
EFF115   DS    0H                  SKIP ADDAY FOR BUY END DAY OF                
         CLI   ROLLSW,YES          SUNDAY. FOR BUY END DAY OF SUNDAY,           
         BE    EFF130              FIRST CHECK FOR ROLL SWITCH AND              
         B     EFF140              BRANCH OR SKIP ADDAY IF ROLL IS OFF.         
*                                                                               
EFF120   GOTO1 ADDAY,DMCB,FLTEND,EFFEND,(R3)                                    
*                                                                               
         BAS   RE,GETEFFED                                                      
         LTR   R0,R0                                                            
         BNZ   EFFEXIT                                                          
*                                                                               
EFF125   DS    0H                                                               
         CLI   ROLLSW,YES          ONLY ENTER THIS ROUTINE WHEN                 
         BNE   EFF140              SWITCH IS SET TO ROLL BACK END DATE          
*                                                                               
EFF130   MVI   ROLLSW,NO                                                        
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
         BE    EFF180              NO                                           
         LA    RF,LFLTENT(R6)      YES-POINT TO NEXT ENTRY AND CHECK            
         CLC   EFFEND,0(RF)        END DATE VS. MONDAY OF NEXT START            
         BL    EFF180              WEEK.                                        
         LA    R0,SEQOROUT         SEQUENCE ERROR-LAST SET OF EFFECTIVE         
         B     EFFEXIT             DATES WILL OVERLAP NEXT FLIGHT ENTRY         
*                                                                               
EFF140   CR    R4,R5               ADJUSTMENT LOGIC ENTERED ONLY                
         BNH   EFF160              WHEN START DAY GREATER THAN END DAY          
         TM    FLTCNTL,NFORM                                                    
         BO    EFF150              N WEEKS INPUT FORMAT                         
         TM    FLTCNTL,DATES                                                    
         BO    *+6                 DATE-DATE INPUT FORMAT                       
         DC    H'0'                                                             
*        ZIC   R1,RBUYDTWK         SUBTRACT 1 TO ADJUST FOR                     
*        BCTR  R1,0                BUY WEEKS SPANNING                           
*        STC   R1,RBUYDTWK         2 WEEKS OF CONTRACT.                         
         B     EFF160                                                           
*                                                                               
EFF150   MVC   DUB(6),EFFEND       PUSH BACK END DATE 7DAYS FOR N WKS           
*        GOTO1 ADDAY,DMCB,DUB,EFFEND,7                                          
         CLC   EFFEND,CONEND                                                    
         BNH   EFF160                                                           
         LA    R0,EDTERR           RUNS PAST CONTRACT END                       
         B     EFFEXIT                                                          
*                                                                               
EFF160   CLC   EFFST,EFFEND        CHECK SEQUENCE OF EFFECTIVE                  
         BL    EFF180              START/END                                    
         BE    EFF170                                                           
         LA    R0,ENBEFST          SEQUENCE ERROR                               
         B     EFFEXIT                                                          
*                                                                               
EFF170   CR    R4,R5               START/END MUST BE                            
         BE    EFF180              IDENTICAL                                    
         LA    R0,INVERR                                                        
         B     EFFEXIT                                                          
*                                                                               
EFF180   GOTO1 DATCON,DMCB,(0,EFFST),(3,RBUYDTST)                               
         GOTO1 (RF),(R1),(0,EFFEND),(3,RBUYDTED)                                
         MVC   RBUYDTIN,FLTIND                                                  
         CLC   =C'MB',MBYACT                                                    
         BNE   EFF190                                                           
         L     RF,TEMP                                                          
         USING RBUYKEY,RF                                                       
         MVC   RBUYDTNW,RBUYNW                                                  
         DROP  RF                                                               
         B     *+14                                                             
EFF190   LA    RE,IOAREA                                                        
         MVC   RBUYDTNW,RBUYNW-RBUYKEY(RE)                                      
         TM    RBUYDTIN,ALT        FOR ALTERNATE WEEKS ONLY                     
         BZ    EFF200                                                           
         ZIC   R1,RBUYDTWK         TAKE TOTAL WEEKS                             
         LA    R1,1(R1)            AND AFTER ADDING 1 TO STOP                   
         SRL   R1,1                ROUNDING DOWN, DIVIDE BY TWO                 
         STC   R1,RBUYDTWK         TO FIND NUMBER OF ACTIVE WEEKS               
*                                                                               
EFF200   LA    R7,LTHRDEL(R7)                                                   
         LA    R6,LFLTENT(R6)                                                   
         BCT   R2,EFF30                                                         
         XR    R0,R0               SUCCESSFUL PROCESSING                        
*                                                                               
EFFEXIT  DS    0H                                                               
         XIT1  REGS=(R0)                                                        
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
GETST20 DS     0H                                                               
* CANNOT CROSS AN OUT-OF-WEEK WEEK                                              
         ZIC   R3,CSTDAY                                                        
         CR    R4,R3                                                            
         BNL   GETST25                                                          
         CR    R5,R3                                                            
         BL    GETST25                                                          
         LA    R0,613                                                           
         B     EFFEXIT                                                          
****                                                                            
GETST25 DS     0H                                                               
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
         CLI   FLTWKS,0            USER SPECIFIED NUMBER OF WEEKS?              
         BNE   GETED50                                                          
         ZIC   R2,FLTDAYS          DID USER SPECIFY END DATE?                   
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         LTR   R2,R2                                                            
         BZ    GETED60                                                          
         B     GETED80                                                          
*                                                                               
***********************************************************************         
* CASE: -NW (IE -3W)                                                            
***********************************************************************         
*                                                                               
GETED50  DS    0H                                                               
         ZIC   R2,FLTWKS                                                        
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
         CLI   FLTWKS,0            USER SPECIFIED NUMBER OF WEEKS?              
         BE    GETEDX                                                           
         CLC   FLTWKS,RBUYDTWK     ERROR IF ACTUAL NUMBER OF WEEKS              
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
         DROP  R6,R7                                                            
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068RECNT18   10/08/15'                                      
         END                                                                    
