*          DATA SET GAJIM00    AT LEVEL 148 AS OF 08/22/00                      
*PHASE TB2100A                                                                  
*INCLUDE NUMVAL                                                                 
         TITLE 'GRADES'                                                         
JSHAGRAD CSECT                                                                  
*        PRINT NOGEN                                                            
         NMOD1 GRDWRKX-GRDWRKD,**GRAD**,RR=R2                                   
         USING GRDWRKD,RC          RC = BASE REGISTER FOR STORAGE               
         L     RA,4(R1)            LOAD ADDRESS OF TABLE INTO RA                
         USING TB21FFD,RA          RA BASE REGISTER FOR SCREEN DSECT            
         L     RE,16(R1)           RE = A(COMFAC)                               
         USING COMFACSD,RE                                                      
         MVC   VSCANNER,CSCANNER   MOVING ADDR. OF SCANNER INTO STORAGE         
         ST    R2,RELO             STORE ADJUSTMENT TO ADDR. RELOCATOR          
*                                                                               
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
*  MAIN PROGRAM                                                      *          
**********************************************************************          
         SPACE 1                                                                
*              CLEAR OUT PRVIOUS HEADER MESSAGES                                
*                                                                               
         XC    JSHHEAD,JSHHEAD     CLEAR OUT MESSAGE FIELD                      
         OI    JSHHEADH+6,X'80'    TRANSMIT                                     
         NI    FLAG,X'FF'-X'80'    TURNOFF ERROR BIT - NOT 1ST RUN              
*                                                                               
*              INPUT OPTIONS AND CLEAR OPTION LABELS                            
*                                                                               
*AIN     CLI   16(RA),0            TEST IF 1ST TIME THROUGH                     
         BAS   RE,SCAN             SCAN OPTIONS FIELD                           
         TM    FLAG,X'80'          TEST IF SUB CAME BACK WITH ERROR             
         BO    EXIT                  IF SET, EXIT                               
*                                                                               
*        CLEAR ALL PREVIOUS GRADES AND SCORES IF ANY                            
*                                                                               
MAIN10   LA    R3,JSHSCRFH         R3 = A(JSHSCRFH) 1ST SCORE HEADER            
         LA    R4,JSHSCRL          R4 = A(JSHSCRL)  LAST SCORE                  
         LR    R2,R3               R2 = A(JSHSCRFH) SAME AS R3                  
*                                                                               
         TM    FLAG,X'20'          TEST IF 1ST TIME THROUGH                     
         BNO   MAIN30              IF SET SKIP FIRST ROUTINE                    
MAIN20   CR    R2,R4               COMPARE FOR LAST FIELD                       
         BH    MAIN30                                                           
         SR    R1,R1               INTIALIZE R1 FOR XC                          
         IC    R1,0(R2)            INSERT LENGTH OF INPUT FOR XC                
         AR    R2,R1                                                            
         XC    8(1,R2),8(R2)       CLEAR OUT CURRENT FIELD                      
         OI    6(R2),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R2)            LENGTH OF RECORD                             
         AR    R2,R1               BUMP R2 TO NEXT FIELD                        
         B     MAIN20                                                           
*                                                                               
MAIN30   TM    FLAG,X'01'          TEST IF NOCURVE BIT IS SET                   
         BO    *+12                  IF SET CONTINUE WITH MAIN PROGRAM          
         BAS   RE,CURVE              ELSE BRANCH TO CURVE SUB                   
         B     EXIT                  AND THEN EXIT                              
         MVC   GRDTAB,TABLE        MOVE GRADE TABLE INTO CURR. STORAGE          
         ZAP   PACK16,=P'0'        INITIALIZE PACK16 FOR TEST AVERAGE           
         ZAP   PKCOUNT,=P'0'       INITIALIZE FOR COUNTER                       
MAIN40   CR    R3,R4               COMPARE FOR END OF LIST                      
         BH    EXIT                                                             
         BAS   RE,VALID            VALIDATE DATA                                
         TM    FLAG,X'80'          TEST IF INVALID BYTE WAS SET                 
         BO    EXIT                  END IF IT WAS SET                          
         ZAP   PKSCORE,=P'0'       INITIAL PKSCORE FOR EX PACK                  
         ICM   R1,1,5(R3)          PUT INPUT LENGTH INTO R1                     
         BZ    MAIN70              IF FIELD IS EMPTY EXIT LOOP                  
         BCTR  R1,0                DECREMENT R1 BY 1 FOR ACTUAL LEN             
         EX    R1,*+8              EXECUTE PACK                                 
         B     *+10                SKIP PACK INSTRUCTION                        
         PACK  PKSCORE,8(0,R3)     PACK GRADE FOR COMPARE                       
         AP    PACK16,PKSCORE      ADD EACH TEST SCORE TO TOTAL                 
         AP    PKCOUNT,=P'1'       INCREMENT COUNTER FOR DENOMINATOR            
*                                                                               
         SR    R1,R1               CLEAR OUT R1 TO BUMP TO NEXT FIELD           
         IC    R1,0(R3)                                                         
         AR    R3,R1                 ADD DISPL. TO BUMP TO GRADE FIELD          
         LA    R2,GRDTAB           R2 = A(GRDTAB)                               
         TM    FLAG,X'04'          TEST IF MINIMUM PASSING WAS ENTERED          
         BNO   MAIN50                IF NOT, CONTINUE                           
         MVC   9(2,R2),PKMINPS       CHANGE FAILING GRADE TO MINPASS            
MAIN50   CLI   0(R2),X'FF'         COMPARE FOR EOF                              
         BE    MAIN60                                                           
         CP    PKSCORE,0(2,R2)       COMP. GRADE TO CUR. POS. IN GRDTAB         
         BL    *+18                  IF LESS, DO NEXT COMPARE                   
         MVC   8(1,R3),2(R2)       MOVE CORRESPONDING GRADE INTO FIELD          
         OI    6(R3),X'80'         TRANSMIT                                     
         B     MAIN60                                                           
         LA    R2,3(R2)            BUMP TO NEXT POSITION IN TABLE               
         B     MAIN50                                                           
*                                                                               
MAIN60   DS    0H                                                               
         SR    R1,R1               CLEAR OUT R1 TO BUMP TO NEXT FIELD           
         IC    R1,0(R3)              INSERT LENGTH OF FIELD INTO R1             
         AR    R3,R1                 ADD DISPL. TO BUMP TO NEXT FIELD           
         B     MAIN40                                                           
*                                                                               
MAIN70   CP    PKCOUNT,=P'0'       COMPARE IF COUNT IS ZERO                     
         BH    *+12                  IF NOT ZERO CONTINUE                       
         BAS   RE,INVAL              IF COUNT IS ZERO BRANCH TO INVALID         
         B     EXIT                  AND EXIT                                   
         DP    PACK16,PKCOUNT      DIVIDE TOTAL BY # OF TESTS                   
         EDIT  (P8,PACKQ),(3,JSHAVER)                                           
         OI    JSHAVERH+6,X'80'    TRANSMIT AVERAGE                             
         LA    R2,GRDTAB           R2 = A(GRDTAB)                               
MAIN80   CLI   0(R2),X'FF'         COMPARE FOR EOF                              
         BE    MAIN90                                                           
         CP    PACKQ,0(2,R2)         COMP. GRADE TO CUR. POS. IN GRDTAB         
         BL    *+18                  IF LESS, DO NEXT COMPARE                   
         MVC   JSHAVGD,2(R2)       MOVE CORRESPONDING GRADE INTO FIELD          
         OI    JSHAVGDH+6,X'80'    TRANSMIT                                     
         B     MAIN90                                                           
         LA    R2,3(R2)            BUMP TO NEXT POSITION IN TABLE               
         B     MAIN80                                                           
*                                                                               
MAIN90   DS    0H                                                               
         TM    FLAG,X'08'          TEST IF 1ST TIME THROUGH                     
         BNO   MAIN100             IF SET SKIP ZAP INSTRUCTION                  
         EDIT  (P8,PKLTEST),(3,JSHLTST)                                         
         OI    JSHLTSTH+6,X'80'     TRANSMIT                                    
         ZAP   PKPERC,PACKQ         FINDING PERCENT OF CHANGE OF NEW            
         SP    PKPERC,PKLTEST        AVERAGE FROM OLD BY SUBT. OLD              
         MP    PKPERC,=P'100'        FROM NEW & MULTIPLY IT BY 100              
         DP    PKPERC,PACKQ          AND THEN DIVIDING BY NEW                   
         EDIT  (P8,PKPERCQ),(4,JSHCHNG),MINUS=YES                               
         OI    JSHCHNGH+6,X'80'    TRANSMIT                                     
         MVI   JSHSIGN,C'%'        PUT IN PERCENT SIGN                          
         OI    JSHSIGNH+6,X'80'    TRANSMIT                                     
MAIN100  ZAP   PKLTEST,PACKQ       PUT NEW TEST SCORE INTO OLD                  
         OI    JSHOPTNH+6,X'40'+X'80'                                           
         OI    FLAG,X'08'          SET FLAG FOR LAST TEST                       
*                                                                               
EXIT     MVI   16(RA),1            SETTING 1ST TIME THROUGH FLAG                
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*  CURVE  SUBROUTINE                                                 *          
**********************************************************************          
         SPACE 1                                                                
*        THIS SUBROUTINE FIGURES OUT THE DIFFERENTIAL BETWEEN                   
*        HIGH/LOW SCORES AND TOSSES THOSE SCORES IF REQUESTED                   
*        AND GRADES WITH A MINIMUM PASSING GRADE IF NECCESSARY                  
*                                                                               
CURVE    NTR1                                                                   
         MVC   GRDTAB,TABLE        MOVE GRADE TABLE INTO CURR. STORAGE          
         LA    R3,JSHSCRFH         R3 = A(JSHSCRFH) 1ST SCORE HEADER            
         LA    R4,JSHSCRL          R4 = A(JSHSCRL)  LAST SCORE                  
         ZAP   PKTEMP,=P'0'        INITIALIZING FOR TABLE ALTERING              
         ZAP   PACK16,=P'0'        INITIALIZING FOR TOTAL                       
         LA    R1,PKSCRH1          R1 = A(PKSCRH1) 1ST TO INITIALIZE            
         LA    R2,PKEQUT           R2 = NUMBER OF TIMES TO LOOP                 
INIT     ZAP   0(8,R1),=P'0'       INITIALIZING CUR. PACKED FIELD               
         LA    R1,L'PACKQ(R1)      BUMPING TO NEXT FIELD                        
         BCT   R2,INIT             LOOP BACK FOR NEXT INITIALIZING              
*                                                                               
CURVE10  CR    R3,R4               COMPARE FOR END OF LIST                      
         BH    CURVE20                                                          
         BAS   RE,VALID            VALIDATE DATA                                
         TM    FLAG,X'80'          TEST IF INVALID BYTE WAS SET                 
         BO    CURVEX                END IF IT WAS SET                          
         ZAP   PKSCORE,=P'0'       INITIAL PKSCORE FOR EX PACK                  
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          PUT INPUT LENGTH INTO R1                     
         BZ    CURVE20             IF FIELD IS EMPTY EXIT LOOP                  
         BCTR  R1,0                DECREMENT R1 BY 1 FOR ACTUAL LEN             
         EX    R1,*+8              EXECUTE PACK                                 
         B     *+10                SKIP PACK INSTRUCTION                        
         PACK  PKSCORE,8(0,R3)     PACK GRADE FOR COMPARE                       
         AP    PACK16,PKSCORE      ADD EACH TEST SCORE TO TOTAL                 
         AP    PKCOUNT,=P'1'       INCREMENT COUNTER FOR DENOMINATOR            
*                                                                               
*        SETTING HIGH  AND LOW SCORES                                           
*                                                                               
         CP    PKSCRH1,=P'0'       TEST IF 1ST TIME THROUGH                     
         BNE   *+16                                                             
         AP    PKSCRH1,PKSCORE     SET HIGH AND LOW = TO 1ST SCORE              
         AP    PKSCRL1,PKSCORE       FOR COMPARING                              
         CP    PKSCRH1,PKSCORE     COMPARE CURR. SCORE W/PREV. HIGH             
         BH    *+16                  STILL HIGH-BRANCH TO NEXT COMPARE          
         ZAP   PKSCRH2,PKSCRH1       IF NOT, PUT OLD HIGH INTO 2ND HI           
         ZAP   PKSCRH1,PKSCORE       AND PUT IN NEW HIGH                        
         CP    PKSCRL1,PKSCORE     COMPARE CURR. SCORE W/PREV. LOW              
         BL    *+16                  STILL LOW - BRANCH TO NEXT COMPARE         
         ZAP   PKSCRL2,PKSCRL1       IF NOT, PUT IN OLD LOW IN 2ND LOW          
         ZAP   PKSCRL1,PKSCORE       AND PUT IN NEW LOW                         
         SR    R1,R1               RE-INITIALIZE R1                             
         IC    R1,0(R3)            INSERT LENGTH OF FIELD INTO R1               
         AR    R3,R1               BUMP TO THE GRADE FIELD                      
         SR    R1,R1               RE-INITIALIZE R1                             
         IC    R1,0(R3)            INSERT LENGTH OF FIELD INTO R1               
         AR    R3,R1               BUMP TO THE NEXT SCORE FIELD                 
         B     CURVE10                                                          
*                                                                               
*                                                                               
CURVE20  DS    0H                                                               
         CP    PKCOUNT,=P'2'       COMPARE IF COUNT IS ZERO                     
         BNL   CURVE25               IF LESS THAN 2                             
         MVC   JSHHEAD,OPTMESS4      SEND MESSAGE TO THAT EFFECT                
         OI    JSHHEADH+6,X'80'     TRANSMIT                                    
         OI    6(R3),X'80'+X'40'    POSITION CURSOR AND TRANSMIT                
         B     CURVEX               AND EXIT                                    
*                                                                               
CURVE25  ZAP   PKAVER,PACK16       ADD TOTAL FOR AVERAGING                      
         DP    PKAVER,PKCOUNT      DIVIDE TOTAL BY # OF TESTS                   
         EDIT  (P8,PKAVERQ),(3,JSHAVER)                                         
         OI    JSHAVERH+6,X'80'    TRANSMIT AVERAGE                             
*                                                                               
         TM    FLAG,X'02'          TEST IF TOSSHI/LO BIT IS SET                 
         BNO   CURVE40               IF NOT SET SKIP NEXT ROUTINE               
         CP    PKCOUNT,=P'4'       A MINIMUM OF 4 GRADES NEED TO BE             
         BNL   CURVE30               ENTERED TO TOSS HI/LO OR INVALID           
         MVC   JSHHEAD,OPTMESS3      SEND MESSAGE TO THAT EFFECT                
         OI    JSHHEADH+6,X'80'     TRANSMIT                                    
         OI    6(R3),X'80'+X'40'    POSITION CURSOR AND TRANSMIT                
         B     CURVEX               AND EXIT                                    
*                                                                               
CURVE30  DS    0H                                                               
         AP    PKSCRH1,PKSCRL1     ADDING HI\LO FOR SUBTRACTING                 
         SP    PACK16,PKSCRH1      SUBTRACT HI\LO FROM TOTAL                    
         SP    PKCOUNT,=P'2'       SUBTRACT TWO FROM COUNT                      
         ZAP   PKSCRH1,PKSCRH2     AFTER TOSS 2ND HI BECOMES HIGHEST            
         ZAP   PKSCRL1,PKSCRL2     AFTER TOSS 2ND LOW BECOMES LOWEST            
*                                                                               
CURVE40  DS    0H                                                               
         ZAP   PKTEMP,PKSCRH1      SETTING FIELD TO BE TOP SCORE                
         SP    PKSCRH1,PKSCRL1     SUBTRACT LO FROM HI                          
         ZAP   PACK16,PKSCRH1      PUT HI INTO PACK16 FOR DIVIDING              
         DP    PACK16,PKCOUNT      FIND DIFFERENTIAL                            
         LA    R2,GRDTAB           R2 = A(GRDTAB)                               
*                                                                               
*              ADJUSTING TABLE TO SUIT CURVE                                    
*                                                                               
CURVE45  CLI   0(R2),X'FF'         COMPARE FOR EOF                              
         BE    CURVE47                                                          
         SP    PKTEMP,PACKQ        NEXT SCORE VALUE                             
         ZAP   0(2,R2),PKTEMP      ADJUST CURRENT GRADE IN TABLE                
         LA    R2,3(R2)                                                         
         B     CURVE45                                                          
*                                                                               
CURVE47  SH    R2,=H'3'            MANUALLY ADJUST GRADE TABLE TO STATE         
         MVC   0(2,R2),=P'00'        THAT 00 IS THE START OF 'F' GRADE          
         LA    R3,JSHSCRFH                                                      
         LA    R4,JSHSCRL                                                       
CURVE50  CR    R3,R4                                                            
         BH    CURVE60                                                          
         ZAP   PKSCORE,=P'0'       INITIAL PKSCORE FOR EX PACK                  
         ICM   R1,1,5(R3)          PUT INPUT LENGTH INTO R1                     
         BZ    CURVE80             IF FIELD IS EMPTY EXIT LOOP                  
         BCTR  R1,0                DECREMENT R1 BY 1 FOR ACTUAL LEN             
         EX    R1,*+8              EXECUTE PACK                                 
         B     *+10                SKIP PACK INSTRUCTION                        
         PACK  PKSCORE,8(0,R3)     PACK GRADE FOR COMPARE                       
*                                                                               
         SR    R1,R1               CLEAR OUT R1 TO BUMP TO NEXT FIELD           
         IC    R1,0(R3)                                                         
         AR    R3,R1                 ADD DISPL. TO BUMP TO GRADE FIELD          
         LA    R2,GRDTAB           R2 = A(GRDTAB)                               
         TM    FLAG,X'04'          TEST IF MINIMUM PASSING WAS ENTERED          
         BNO   CURVE60               IF NOT, CONTINUE                           
         MVC   9(2,R2),PKMINPS       CHANGE FAILING GRADE TO MINPASS            
CURVE60  CLI   0(R2),X'FF'         COMPARE FOR EOF                              
         BE    CURVE70                                                          
         CP    PKSCORE,0(2,R2)       COMP. GRADE TO CUR. POS. IN GRDTAB         
         BL    *+18                  IF LESS, DO NEXT COMPARE                   
         MVC   8(1,R3),2(R2)       MOVE CORRESPONDING GRADE INTO FIELD          
         OI    6(R3),X'80'         TRANSMIT                                     
         B     CURVE65                                                          
         LA    R2,3(R2)            BUMP TO NEXT POSITION IN TABLE               
         B     CURVE60                                                          
*                                                                               
CURVE65  DS    0H                                                               
         SR    R1,R1               CLEAR OUT R1 TO BUMP TO NEXT FIELD           
         IC    R1,0(R3)              INSERT LENGTH OF FIELD INTO R1             
         AR    R3,R1                 ADD DISPL. TO BUMP TO NEXT FIELD           
         B     CURVE50                                                          
*                                                                               
CURVE70  LA    R2,GRDTAB           R2 = A(GRDTAB)                               
CURVE75  CLI   0(R2),X'FF'         COMPARE FOR EOF                              
         BE    CURVE80                                                          
         CP    PKAVERQ,0(2,R2)     COMP. GRADE TO CUR. POS. IN GRDTAB           
         BL    *+18                  IF LESS, DO NEXT COMPARE                   
         MVC   JSHAVGD,2(R2)       MOVE CORRESPONDING GRADE INTO FIELD          
         OI    JSHAVGDH+6,X'80'    TRANSMIT                                     
         B     CURVE80                                                          
         LA    R2,3(R2)            BUMP TO NEXT POSITION IN TABLE               
         B     CURVE75                                                          
*                                                                               
CURVE80  DS    0H                                                               
         TM    FLAG,X'08'          TEST IF 1ST TIME THROUGH                     
         BNO   CURVE90             IF SET SKIP ZAP INSTRUCTION                  
         EDIT  (P8,PKLTEST),(3,JSHLTST)                                         
         OI    JSHLTSTH+6,X'80'     TRANSMIT                                    
         ZAP   PKPERC,PKAVERQ       FINDING PERCENT OF CHANGE OF NEW            
         SP    PKPERC,PKLTEST        AVERAGE FROM OLD BY SUBT. OLD              
         MP    PKPERC,=P'100'        FROM NEW & MULTIPLY IT BY 100              
         DP    PKPERC,PKAVERQ        AND THEN DIVIDING BY NEW                   
         EDIT  (P8,PKPERCQ),(4,JSHCHNG),MINUS=YES                               
         OI    JSHCHNGH+6,X'80'    TRANSMIT                                     
         MVI   JSHSIGN,C'%'        PUT IN PERCENT SIGN                          
         OI    JSHSIGNH+6,X'80'    TRANSMIT                                     
CURVE90  ZAP   PKLTEST,PKAVERQ     PUT NEW TEST SCORE INTO OLD                  
         OI    JSHOPTNH+6,X'40'+X'80'                                           
         OI    FLAG,X'08'          SET FLAG FOR LAST TEST                       
*                                                                               
CURVEX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  SCANNER SUBROUTINE                                                *          
**********************************************************************          
         SPACE 1                                                                
SCAN     NTR1                                                                   
         NI    FLAG,X'FF'-X'01'-X'02'-X'04'   TURN OFF ALL SCAN BITS            
         ZAP   PKMINPS,=P'0'       INITIALIZING MINIMUM PASSING GRADE           
         GOTO1 VSCANNER,DMCB,JSHOPTNH,SCANBLK                                   
         LA    R2,SCANBLK                                                       
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BNZ   SCAN10              IF ZERO, INVALID - SEND MESSAGE              
         MVC   JSHHEAD,BEGMESS          MOVE INVALID OPTIONS MESSAGE            
         OI    JSHHEADH+6,X'80'         TRANSMIT MESSAGE                        
         OI    JSHOPTNH+6,X'40'+X'80'   POSITION CURSOR AND TRANSMIT            
         OI    FLAG,X'80'          SET ERROR FLAG                               
         B     SCANX                                                            
*                                                                               
SCAN10   CLC   =C'NOCURVE',12(R2)                                               
         BNE   *+12                                                             
         OI    FLAG,X'01'          SET FLAG FOR NOCURVE SELECTION               
         B     SCAN40                                                           
         CLC   =C'TOSSHILO',12(R2)                                              
         BNE   *+12                                                             
         OI    FLAG,X'02'          SET FLAG FOR TOSSHI/LO SELECTION             
         B     SCAN40                                                           
         CLC   =C'MINPASS',12(R2)                                               
         BNE   SCAN30                                                           
         OI    FLAG,X'04'          SET FLAG FOR MINPASS SELECTION               
         TM    3(R2),X'80'         CHECK IF VALID NUMERIC                       
         BO    SCAN20                IF VALID BIT IS ON CONTINUE                
         MVC   JSHHEAD,OPTMESS5         MOVE INVALID OPTIONS MESSAGE            
         OI    JSHHEADH+6,X'80'         TRANSMIT MESSAGE                        
         OI    JSHOPTNH+6,X'40'+X'80'   POSITION CURSOR AND TRANSMIT            
         OI    FLAG,X'80'          SET ERROR FLAG                               
         B     SCANX                                                            
SCAN20   PACK  PKMINPS,22(2,R2)    PACKING MINIMUM PASSING GRADE                
         B     SCAN40                                                           
SCAN30   MVC   JSHHEAD,OPTMESS2         MOVE INVALID OPTIONS MESSAGE            
         OI    JSHHEADH+6,X'80'         TRANSMIT MESSAGE                        
         OI    JSHOPTNH+6,X'40'+X'80'   POSITION CURSOR AND TRANSMIT            
         OI    FLAG,X'80'          SET ERROR FLAG                               
         B     SCANX                                                            
*                                                                               
SCAN40   LA    R2,32(R2)                                                        
         BCT   R3,SCAN10           LOOP BACK FOR NEXT COMPARE                   
         TM    FLAG,X'01'+X'02'    TEST IF CONDITION OF NOCURVE AND             
         BNO   SCANX                 TOSS HI\LO EXIST,  IF IT DOES              
         MVC   JSHHEAD,OPTMESS          MOVE INVALID OPTIONS MESSAGE            
         OI    JSHHEADH+6,X'80'         TRANSMIT MESSAGE                        
         OI    JSHOPTNH+6,X'40'+X'80'   POSITION CURSOR AND TRANSMIT            
         OI    FLAG,X'80'               SET ERROR FLAG                          
*                                                                               
SCANX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  VALIDATE DATA SUBROUTINE                                          *          
**********************************************************************          
         SPACE 1                                                                
VALID    NTR1                                                                   
         TM    FLAG,X'20'          TEST IF 1ST TIME THROUGH                     
         BO    VALID10             IF SET SKIP FIRST ROUTINE                    
         SR    R1,R1               CLEAR R1                                     
         ICM   R1,1,5(R3)          PUT INPUT LENGTH INTO R1                     
         BNZ   VALID10             CONTINUE IF NOT EMPTY                        
         MVC   JSHHEAD,BEGMESS     MOVE IN MESSAGE                              
         OI    JSHHEADH+6,X'80'    TRANSMIT FIELD                               
         OI    6(R3),X'40'+X'80'   POSITION CURSOR AND TRANSMIT                 
         OI    FLAG,X'80'          SET INVALID FLAG                             
         B     VALIDX                                                           
*                                                                               
VALID10  DS    0H                                                               
         OI    FLAG,X'20'          SETTING 2ND RUN THROUGH FLAG                 
         CR    R3,R4               COMPARE CUR. POS. IN TABLE TO END            
         BH    VALIDX                IF GREATER EXIT                            
         SR    R5,R5               CLEAR R1 FOR EX CLC                          
         ICM   R5,1,5(R3)          PUT INPUT LENGTH INTO R1                     
         BZ    VALIDX              EXIT IF EMPTY                                
         CH    R5,=H'2'                                                         
         BNH   VALID20                                                          
         CLC   8(3,R3),=C'100'     COMPARE IF NUMBER TO 100                     
         BNH   *+12                  IF LESS OR EQUAL DO NEXT COMPARES          
         BAS   RE,INVAL              ELSE GOTO INVALID ROUTINE                  
         B     VALIDX                AND THEN EXIT                              
VALID20  GOTO1 =V(NUMVAL),DMCB,(0,8(R3)),(1,0),RR=RELO                          
         CLI   0(R1),X'00'           CHECK NUMVAL RETURN CODE                   
         BE    *+12                  IF NOT X'FF' CONTINUE                      
         BAS   RE,INVAL              IF GREATER GOTO INVALID ROUTINE            
         B     VALIDX                EXIT                                       
         IC    R5,0(R3)            INSERT FIELD LENGTH INTO R1                  
         AR    R3,R5               ADD DISPL. TO R3 TO BUMP TO NEXT ONE         
         SR    R5,R5               CLEAR R1 FOR NEXT BUMP                       
         IC    R5,0(R3)            INSERT NEW FILED'S LENGTH INTO R1            
         AR    R3,R5               ADD DISPL. TO R3 TO BUMP PAST GRADE          
         B     VALID10             DO NEXT SET OF COMPARES                      
*                                                                               
VALIDX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  INVALID  DATA  MESSAGE  SUBROUTINE                                *          
**********************************************************************          
         SPACE 1                                                                
INVAL    NTR1                                                                   
         MVC   JSHHEAD,VALMESS          MOVE INVALID MESSAGE INTO HEAD          
         OI    JSHHEADH+6,X'80'         TRANSMIT MESSAGE                        
         OI    6(R3),X'40'+X'80'        POSITION CURSOR AND TRANSMIT            
         OI    FLAG,X'80'               SET FLAG FOR ERROR                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
*  CONSTANT DECLARATIONS                                             *          
**********************************************************************          
         SPACE 1                                                                
RELO     DS    F                                                                
VALMESS  DC    CL60'NUMBER IS NOT VALID - PLEASE RE-ENTER'                      
BEGMESS  DC    CL60'MISSING FIELD - PLEASE RE-ENTER'                            
REQMESS  DC    CL27'UPDATE THE OPTIONS? Y/N <N>'                                
OPTMESS  DC    CL60'INCORRECT OPTIONS - INVALID NOCURVE/TOSSHILO'               
OPTMESS2 DC    CL60'INCORRECT OPTIONS - PLEASE RE-ENTER'                        
OPTMESS3 DC    CL60'INCORRECT HI/LO - NEEDS MINIMUM 4 SCORES'                   
OPTMESS4 DC    CL60'INCORRECT CURVE - NEEDS MINIMUM 2 SCORES'                   
OPTMESS5 DC    CL60'INCORRECT MINPASS - NEEDS VALID NUMERIC VALUE'              
LABMESS  DC    CL38'OPTIONS ENTERED - PLEASE ENTER SCORES '                     
TABLE    DC    P'90',C'A'                                                       
         DC    P'80',C'B'                                                       
         DC    P'70',C'C'                                                       
         DC    P'60',C'D'                                                       
         DC    P'00',C'F'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*  LITERALS                                                          *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*  TWA DSECT                                                         *          
**********************************************************************          
         SPACE 1                                                                
       ++INCLUDE GAJIMFFD                                                       
*                                                                               
PKLTEST  DS    PL8                 LAST TEST                                    
PKMINPS  DS    PL2                 PACKED MINIMUM PASSING GRADE                 
FLAG     DS    X                   FLAG FOR VALIDITY AND RUN                    
         EJECT                                                                  
**********************************************************************          
*  COMFACS DSECT                                                     *          
**********************************************************************          
         SPACE 1                                                                
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
**********************************************************************          
*  STORAGE DSECT                                                     *          
**********************************************************************          
         SPACE 1                                                                
GRDWRKD  DSECT                                                                  
VSCANNER DS    V                   ADDRESS OF VALIABLE                          
DMCB     DS    6F                                                               
DUB      DS    D                                                                
PKPERC   DS    0PL16               PERCENT OF CHANGE FROM LAST TO NEW           
PKPERCQ  DS    PL8                 PERCENT QUOTIENT                             
PKPERCR  DS    PL8                 PERCENT REMAINDER                            
PKAVER   DS    0PL16               SAVED PACK AVERAGE                           
PKAVERQ  DS    PL8                 PACKED AVERAGE QUOTIENT                      
PKAVERR  DS    PL8                 PACKED AVERAGE REMAINDER                     
PKSCORE  DS    PL8                 EACH INDIVIDUAL TEST                         
PKTEMP   DS    PL2                 TEMPORARY STORAGE FOR ADJ. TABLE             
PACK16   DS    0PL16               AVERAGE OF ALL SCORES                        
PACKQ    DS    PL8                 QUOTIENT                                     
PACKR    DS    PL8                 REMAINDER                                    
PKSCRH1  DS    PL8                 HIGHEST TEST SCORE                           
PKSCRH2  DS    PL8                 2ND HIGHEST TEST SCORE                       
PKSCRL1  DS    PL8                 LOWEST TEST SCORE                            
PKSCRL2  DS    PL8                 2ND LOWEST TEST SCORE                        
PKCOUNT  DS    PL8                 COUNTER                                      
PKEQUT   EQU   (*-PKSCRH1)/8       EQUATED NUMBER OF ITEMS                      
WORK     DS    CL120                                                            
SCANBLK  DS    10CL32              BLOCK FOR SCANNER                            
GRDTAB   DS    CL16                STORAGE FOR TABLE                            
GRDWRKX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148GAJIM00   08/22/00'                                      
         END                                                                    
