*          DATA SET SPWRI33    AT LEVEL 003 AS OF 01/11/16                      
*PHASE T20433C,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI33 (T20433) - S C JOHNSON SPOT ESTIMATE INTERFACE   *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 17OCT11 01 AKAT -- INITIAL DEVELOPMENT                            *           
* 27AUG15 03 AKAT -- PROCESS UDEF INSTEAD OF UCOM FOR AGENCY OO     *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20433 - S C JOHNSON SPOT ESTIMATE INTERFACE'                   
T20433   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20433**,RR=R2                                             
         LR    R5,RC                                                            
         USING WORKD,R5                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    R2,RELO                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPFIRST      INITIALIZATION                               
         BE    FIRST                                                            
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
*                                                                               
* INITIALIZATION                                                                
*                                                                               
FIRST    OI    SBQSKIP,SBQSKGL+SBQSKBIL                                         
         MVI   PRINTH,C'N'         DIDN'T PRINT HEADER YET                      
         OI    SBQPER,SBQPMN       MONTHS                                       
         MVI   SBQPERLO,1          FORCE WHOLE PERIOD                           
         MVI   SBQPERHI,X'FF'      FORCE WHOLE PERIOD                           
*                                                                               
         CLC   AGENCY,=C'OO'       AGENCY OO?                                   
         BE    FIRST05             YES - EXTRACT UDEF INSTEAD OF UCOM           
         OI    DATAIND9,DIUCOM     SET EXTRACT UCOM DATA                        
         B     FIRST10                                                          
*                                                                               
FIRST05  OI    SBEUDEF,SBEUEST1    EXTRACT ESTIMATE UDEF FIELD 1                
*                                                                               
FIRST10  OI    OPTIND6,OPT6FILE    SEND OUTPUT DATA TO FILE                     
         OI    OPTIND6,OPT6SCJE    SC JOHNSON ESTIMATE                          
         MVI   WIDTHOPT,C'W'       WIDE PRINTING (165)                          
         B     XIT                 EXIT                                         
*                                                                               
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    MVI   PEROPT,PEROB3       USE B3 PROFILE                               
         B     XIT                 EXIT                                         
*                                                                               
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         B     XIT                                                              
*                                                                               
* DRIVER INPUT ROUTINE                                                          
*                                                                               
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLPUTSRT     PUT A RECORD TO SORT                         
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
*                                                                               
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  DS    0H                                                               
         B     XIT                                                              
*                                                                               
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST         SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
*                                                                               
ROUTLIST DS    0F                                                               
         DC    CL8'IHEADER ',A(IHEADER)                                         
         DC    CL8'OHEADER ',A(OHEADER)                                         
         DC    CL8'IUCOM   ',A(IUCOM)                                           
         DC    CL8'OUCOM   ',A(OUCOM)                                           
         DC    CL8'OPRD    ',A(OPRD)                                            
         DC    CL8'OEST    ',A(OEST)                                            
         DC    CL8'OMON    ',A(OMON)                                            
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'ONET    ',A(ONET)                                            
         DC    CL8'OTAX    ',A(OTAX)                                            
         DC    X'FF'                                                            
*                                                                               
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  DS    0H                                                               
         B     XIT                                                              
*                                                                               
* PRINT A LINE                                                                  
*                                                                               
PRINT    DS    0H                                                               
         B     XIT                                                              
*                                                                               
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   DS    0H                                                               
FIRSTX   B     XIT                                                              
*                                                                               
* DRIVER HOOK PUTSRT                                                            
*                                                                               
PUTSRT   DS    0H                                                               
PUTSRTX  B     XIT                                                              
*                                                                               
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    DS    0H                                                               
         B     XIT                                                              
*                                                                               
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     DS    0H                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
*                                                                               
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
*                                                                               
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
IHEADER  MVI   0(R2),1             GENERATE DATA                                
         B     XIT                 EXIT                                         
*                                                                               
OHEADER  CLI   PRINTH,C'Y'         PRINTED HEADER ALREADY?                      
         BE    XIT                 YES - ONLY PRINT ONCE                        
         MVC   P+1(6),=C'HEADER'   HEADER                                       
         MVC   P+11(14),=C'SPOT ESTIMATES'                                      
         GOTO1 DATCON,DMCB,(2,SBBTODAY),(20,P+31)                               
                                                                                
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(4),GETBROAD    GETBROAD                                     
         MVC   WORK+4(4),ADDAY     ADDAY                                        
         MVC   WORK+8(4),GETDAY    GETDAY                                       
         MVC   WORK+12(4),DATCON   DATCON                                       
                                                                                
         GOTO1 GETBROAD,DMCB,(1,SBQTODAY),WORK+16,GETDAY,ADDAY                  
         CLI   0(R1),X'FF'         ANY ERRORS?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
                                                                                
         LA    R6,1                GO FORWARD 1 DAY TO GET NEXT BRDCST          
         GOTO1 ADDAY,DMCB,WORK+22,WORK+28,(R6)                                  
                                                                                
         GOTO1 GETBROAD,DMCB,(1,WORK+28),WORK+34,GETDAY,ADDAY                   
         CLI   0(R1),X'FF'         ANY ERRORS?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
                                                                                
         MVC   WORK+22(6),WORK+40  END OF NEXT BROADCAST MONTH                  
         LA    R6,WORK+16          BROADCAST MONTH BASED ON TODAYS DATE         
         ST    R6,DMCB             P1 FOR MOBILE                                
         MVI   DMCB,NMONTHS-1      MAX NUMBER OF MONTHS TO BUILD                
                                                                                
         LA    R6,ELEM             MONTH TABLE                                  
         ST    R6,DMCB+4           P2 FOR MOBILE                                
         MVC   DMCB+4(1),DATEFORM  DATE FORMAT                                  
                                                                                
         GOTO1 MOBILE,DMCB,,,WORK,SBSPPROF                                      
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    *+12                YES                                          
         LA    R6,4(R6)            NO - BUMP TO NEXT DATE                       
         B     *-12                LOOP BACK AND CHECK EOT                      
         MVI   0(R6),0             0 AFTER X'FF'                                
                                                                                
         LA    R1,ELEM             MONTH TABLE                                  
                                                                                
OHEAD10  OC    0(4,R1),0(R1)       ANY ENTRIES LEFT?                            
         BZ    OHEAD20             NO                                           
         CLC   SBBTODAY,0(R1)      BEFORE PERIOD?                               
         BL    OHEAD20             YES - NO MOS                                 
         CLC   SBBTODAY,2(R1)      WITHIN PERIOD?                               
         BNH   OHEAD15             YES - USE THIS DATE                          
         LA    R1,4(R1)            BUMP TO NEXT ENTRY                           
         B     OHEAD10             CHECK NEXT ENTRY                             
*                                                                               
OHEAD15  MVC   FULL,0(R1)          START/END MONTH                              
         GOTO1 DATCON,DMCB,(2,FULL),(20,P+39)                                   
         GOTO1 DATCON,DMCB,(2,FULL+2),(20,P+47)                                 
*                                                                               
OHEAD20  ICM   R1,15,AOUTPDCB      HAVE OUTPUT DCB?                             
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         PUT   (R1),P+1            OUTPUT TO FILE                               
         GOTO1 SBPRINT,DMCB,P,=C'BL01'                                          
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVI   PRINTH,C'Y'         PRINTED HEADER                               
         B     XIT                 EXIT                                         
*                                                                               
IUCOM    CLC   AGENCY,=C'OO'       AGENCY OO?                                   
         BE    IUDEF               YES - PROCESS UDEF INSTEAD                   
*                                                                               
         LA    R6,UCDE1FLD-UCOMD   R6=A(USER FIELD)                             
         L     RE,AUCOMTAB         A(UCOM TABLE)                                
         AR    R6,RE               INDEX INTO ESTIMATE 1 UCOM FIELD             
         CLI   UCDE1TYP,C'D'       DATA TYPE = DATE                             
         BNE   IUCOM5              NO                                           
         CLC   0(6,R6),SPACES      IS FIELD SET?                                
         BNH   XIT                 NO - EXIT                                    
         GOTO1 DATVAL,DMCB,(0,(R6)),DUB GET THE DATE                            
         OC    0(4,R1),0(R1)       IS THE DATE VALID?                           
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         GOTO1 DATCON,DMCB,DUB,(2,(R2)) CONVERT TO PACKED                       
         B     XIT                 EXIT                                         
*                                                                               
IUCOM5   LLC   RE,UCDE1LEN         LENGTH OF ESTIMATE UCOM 1 FIELD              
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,*+8              ** EXECUTE **                                
         B     *+10                BRANCH FOR IDF                               
         MVC   0(0,R2),0(R6)       MOVE EST UCOM INTO DRIVER INPUT FLD          
         EX    RE,*+8              ** EXECUTE **                                
         B     *+10                BRANCH FOR IDF                               
         OC    0(0,R2),SPACES      SPACE PAD                                    
         B     XIT                 EXIT                                         
*                                                                               
IUDEF    LA    RA,SBLOCK           RA = A(SBLOCK)                               
         USING SBLOCK,RA           SPOTBLOCK DSECT                              
         LA    R6,SBUE1FLD         R6=A(USER FIELD)                             
         CLI   SBUE1TYP,C'D'       DATA TYPE = DATE                             
         BNE   IUDEF5              NO                                           
         CLC   0(6,R6),SPACES      IS FIELD SET?                                
         BNH   XIT                 NO - EXIT                                    
         GOTO1 DATVAL,DMCB,(0,(R6)),DUB GET THE DATE                            
         OC    0(4,R1),0(R1)       IS THE DATE VALID?                           
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         GOTO1 DATCON,DMCB,DUB,(2,(R2)) CONVERT TO PACKED                       
         B     XIT                 EXIT                                         
*                                                                               
IUDEF5   XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(8),0(R6)       ESTIMATE UDEF 1 FIELD                        
***                                                                             
* PASS ESTIMATE UDEF RIGHT ALIGNED AND ZERO PADDED                              
***                                                                             
         LA    RE,8                8 BYTE ESTIMATE UDEF                         
         LA    R1,WORK             SEE HOW MANY BYTES TO RIGHT ALIGN            
*                                                                               
IUDEF10  CLI   0(R1),X'40'         HAVE UDEF DATA IN THIS POSITION?             
         BNH   IUDEF15             NO                                           
         LA    R1,1(R1)            BUMP TO NEXT POSITION                        
         BCT   RE,IUDEF10          CHECK NEXT POSITION                          
*                                                                               
IUDEF15  MVC   0(8,R2),ZEROS       ZERO PAD INPUT                               
         LA    RF,0(R2,RE)         ESTIMATE UDEF STARTS HERE                    
         LA    R6,8                FULL LENGTH OF ESTIMATE UDEF                 
         SR    R6,RE               LENGTH OF THIS ESTIMATE UDEF                 
         BCTR  R6,0                -1 FOR EX                                    
         EX    R6,*+8              ** EXECUTE **                                
         B     *+10                BRANCH FOR IDF                               
         MVC   0(0,RF),WORK        MOVE EST UCOM INTO DRIVER INPUT FLD          
         B     XIT                 EXIT                                         
         DROP  RA                  DROP SPOTBLOCK USING                         
*                                                                               
         USING DETAILD,P+1                                                      
OUCOM    MVC   P,SPACES            CLEAR P                                      
         MVC   P2,SPACES           CLEAR P2                                     
         MVC   DETUCOM,0(R2)       UCOMM                                        
         B     XIT                 EXIT                                         
*                                                                               
OPRD     MVC   DETPRD,0(R2)        PRODUCT                                      
         MVC   DETEDPRD,0(R2)      PRODUCT                                      
         B     XIT                                                              
*                                                                               
OEST     MVC   SBBEST,1(R2)        ESTIMATE                                     
         EDIT  SBBEST,(3,SBEST),FILL=0                                          
***      MVC   SBBPRD,2(R2)        GETESTNM NEEDS PRODUCT                       
***      GOTO1 GETESTNM            GET EST START/END DATES                      
***      BNE   XIT                 EXIT IF NOT FOUND                            
         MVC   DETEST(3),SBEST     ESTIMATE                                     
         B     XIT                                                              
*                                                                               
OMON     L     R6,AMONTHS          MONTH TABLE                                  
                                                                                
OMON10   OC    0(4,R6),0(R6)       ANY ENTRIES LEFT?                            
         BZ    XIT                 NO                                           
         CLC   0(2,R2),0(R6)       BEFORE PERIOD?                               
         BL    XIT                 YES - NO MOS                                 
         CLC   0(2,R2),2(R6)       WITHIN PERIOD?                               
         BNH   OMON15              YES - USE THIS DATE                          
         LA    R6,4(R6)            BUMP TO NEXT ENTRY                           
         B     OMON10              CHECK NEXT ENTRY                             
*                                                                               
OMON15   GOTO1 DATCON,DMCB,(2,0(R6)),(20,DETES)                                 
         GOTO1 DATCON,DMCB,(2,2(R6)),(20,DETEE)                                 
         MVC   HALF,0(R6)          START DATE FOR MONTH                         
                                                                                
OMON20   GOTO1 DATCON,DMCB,(2,HALF),(0,WORK)                                    
         CLI   WORK+4,C'0'         IS THIS THE CORRECT MONTH?                   
         BE    OMON25              YES - NOT THE END OF PRIOR MONTH             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         B     OMON20                                                           
                                                                                
OMON25   GOTO1 DATCON,DMCB,(2,HALF),(22,WORK)                                   
         MVC   DETEDMOS(3),WORK    JUST MOVE MONTH AND YEAR                     
         MVC   DETEDMOS+3(4),WORK+6                                             
         B     XIT                 EXIT                                         
*                                                                               
ODOL     TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    *+16                NO                                           
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P2,SPACES           CLEAR PRINT LINE                             
         MVC   DETGROSS(4),0(R2)   ORDERED GROSS                                
         B     XIT                 EXIT                                         
*                                                                               
ONET     MVC   DETNET(4),0(R2)     ORDERED NET                                  
         B     XIT                 EXIT                                         
*                                                                               
OTAX     MVC   DETTAX(4),0(R2)     ORDERED TAX                                  
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BNZ   OTAX10              YES                                          
*                                                                               
         MVC   DETDET(6),=C'DETAIL'                                             
         MVI   DETEDSYS,C'S'       SYSTEM = SPOT                                
         MVC   DETEDMED,SBQMED     MEDIA                                        
         ICM   R1,15,DETNET        ORDERED NET                                  
         ICM   R6,15,DETTAX        ORDERED TAX                                  
         AR    R6,R1               ORDERED NET + TAX                            
         EDIT  (R6),(16,DETETOT),2,ZERO=NOBLANK,MINUS,FLOAT=-                   
*                                                                               
         LA    RF,DETETOT+L'DETETOT-1                                           
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         ICM   R6,15,DETGROSS      ORDERED GROSS                                
         EDIT  (R6),(16,DETGROSS),2,ZERO=NOBLANK,MINUS,FLOAT=-                  
*                                                                               
         LA    RF,DETGROSS+L'DETGROSS-1                                         
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         ICM   R6,15,DETNET        ORDERED NET                                  
         EDIT  (R6),(16,DETNET),2,ZERO=NOBLANK,MINUS,FLOAT=-                    
*                                                                               
         LA    RF,DETNET+L'DETNET-1                                             
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         ICM   R6,15,DETTAX        ORDERED TAX                                  
         EDIT  (R6),(16,DETTAX),2,ZERO=NOBLANK,MINUS,FLOAT=-                    
*                                                                               
         LA    RF,DETTAX+L'DETTAX-1                                             
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         LH    R1,LINECNT          LINE COUNT                                   
         AHI   R1,1                BUMP LINE COUNT                              
         STH   R1,LINECNT          NEW LINE COUNT                               
         B     OTAX20              END OF DETAIL LINE                           
                                                                                
OTAX10   MVC   P+1(6),=C'FOOTER'   FOOTER                                       
         LH    R1,LINECNT          LINE COUNT                                   
         AHI   R1,2                PLUS HEADER AND FOOTER                       
         EDIT  (R1),(10,P+11),FILL=0                                            
*                                                                               
         EDIT  (B4,DETGROSS),(16,P+21),2,ZERO=NOBLANK,MINUS,FLOAT=-             
         LA    RF,P+36             TOTAL GROSS                                  
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         EDIT  (B4,DETNET),(16,P+37),2,ZERO=NOBLANK,MINUS,FLOAT=-               
         LA    RF,P+52             TOTAL GROSS                                  
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         EDIT  (B4,DETTAX),(16,P+53),2,ZERO=NOBLANK,MINUS,FLOAT=-               
         LA    RF,P+68             TOTAL GROSS                                  
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         MVC   DETGROSS(48),SPACES MOVE SPACES IN                               
*                                                                               
OTAX20   ICM   R1,15,AOUTPDCB      HAVE OUTPUT DCB?                             
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         PUT   (R1),P+1            OUTPUT TO FILE                               
         GOTO1 SBPRINT,DMCB,P,=C'BL01'                                          
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    OTAX30              NO                                           
         ICM   R1,15,AOUTPDCB      OUTPUT DCB                                   
         CLOSE ((R1),)             CLOSE TAPE                                   
*                                                                               
OTAX30   MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P2,SPACES           CLEAR PRINT LINE                             
         B     XIT                 EXIT                                         
*                                                                               
* FORMAT A 16 CHAR NUMBER BY LEADING WITH +/- SIGN AND ZERO PADDING             
*                                                                               
FMTNUM   LA    R6,16               FIELD LENGTH = 16                            
*                                                                               
FNUM00   CLI   0(RF),X'40'         SPACE?                                       
         BE    FNUM10              YES - NUMBER ENDS HERE                       
         BCTR  RF,0                DECREMENT POINTER                            
         BCT   R6,FNUM00           LOOP BACK & CHECK PREVIOUS POSITION          
         BR    RE                  SOMEHOW HAVE A 14 DIGIT NUMBER!              
*                                                                               
FNUM10   MVI   BYTE,C'+'           DEFAULT TO POSITIVE                          
         CLI   1(RF),C'-'          NEGATIVE NUMBER                              
         BNE   *+8                 NO                                           
         MVI   BYTE,C'-'           YES - MAKE IT NEGATIVE                       
*                                                                               
FNUM20   BCTR  R6,0                -1                                           
         SR    RF,R6               MOVE SIGN HERE                               
         MVC   0(1,RF),BYTE        +/- SIGN                                     
         LA    RF,1(RF)            PAD WITH 0'S STARTING HERE                   
         BCTR  R6,0                -1 FOR EX                                    
         EX    R6,*+8              ** EXECUTE **                                
         B     *+10                BRANCH FOR IDF                               
         MVC   0(0,RF),ZEROS       LEADING ZEROS BEFORE THE NUMBER              
         BCTR  RF,0                FIRST POSITION                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                      DO NOT CHANGE THIS!!!                        
*                                                                               
         LTORG                                                                  
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
ZEROS    DC    CL16'0000000000000000'                                           
*                                                                               
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
RELO     DS    F                                                                
SAVERD   DS    A                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
DETAILD  DSECT                                                                  
DETDET   DS    CL10                DETAIL LINE                                  
DETUCOM  DS    CL8                 UCOM (UDEF FOR AGENCY OO)                    
DETPRD   DS    CL3                 PRODUCT                                      
DETEST   DS    CL8                 ESTIMATE                                     
DETES    DS    CL8                 ESTIMATE START DATE                          
DETEE    DS    CL8                 ESTIMATE END DATE                            
DETEDPRD DS    CL3                 ESTIMATE DESCRIPTION PRODUCT                 
DETEDMOS DS    CL7                 ESTIMATE DESCRIPTION MOS                     
DETEDSYS DS    CL1                 ESTIMATE DESCRIPTION SYSTEM                  
DETEDMED DS    CL1                 ESTIMATE DESCRIPTION MEDIA                   
DETEDBLN DS    CL18                ESTIMATE DESCRIPTION BLANKS                  
DETETOT  DS    CL16                ORDERED NET + TAX                            
DETGROSS DS    CL16                ORDERED GROSS                                
DETNET   DS    CL16                ORDERED NET                                  
DETTAX   DS    CL16                ORDERED TAX                                  
DETLENQ  EQU   *-DETDET                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
         ORG   GLOBALD+1144                                                     
AOUTPDCB DS    A                                                                
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIF5D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPWRI33   01/11/16'                                      
         END                                                                    
