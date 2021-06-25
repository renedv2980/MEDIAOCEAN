*          DATA SET RECNT54    AT LEVEL 074 AS OF 05/04/99                      
*PHASE T80254A,+0                                                               
         TITLE 'REPPAK CONTRACT - HIATUS DISPLAY/EDIT T80254'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT54 (T80254) --- HIATUS DISPLAY/EDIT                   *           
*                                                                   *           
* ---------------------------------------------------------------   *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 02MAY96 WSB ORIGINAL DEVELOPMENT                                  *           
*                                                                   *           
*                      *** END TOMBSTONE ***                        *           
*********************************************************************           
*                                                                               
T80254   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80254,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VPERVAL,CPERVAL                                                  
         DROP  RE                                                               
*                                                                               
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)      DISPLAY HIATUS?                              
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)      EDIT HIATUS?                                 
         BE    EDIT                                                             
         DC    H'0'                DIE OTHERWISE                                
         EJECT                                                                  
DISP     DS    0H                                                               
* MUCH OF THIS CODE ADAPTED FROM RECNT25                                        
         NI    FLAGS,X'FF'-X'20'   NO OVERFLOW ERROR YET                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   DISP50              NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      YES, IS CONTR LINKED TO AGY ORDER?           
         BNO   DISP50              NO, CONTINUE                                 
         DROP  R6                                                               
*                                                                               
* DARE                                                                          
         OI    HIADATEH+1,X'20'    PROTECT FIELD                                
         OI    HIADAT2H+1,X'20'                                                 
         OI    HIACOMMH+1,X'20'                                                 
         OI    HIACOM2H+1,X'20'                                                 
         MVC   CONBACT,MYSPACES    CLEAR OUT BUY ACTION FIELD                   
         OI    CONBACTH+6,X'80'                                                 
         BAS   RE,DARE                                                          
         B     EXXMOD                                                           
*                                                                               
* NON-DARE                                                                      
DISP50   DS    0H                                                               
         NI    HIADATEH+1,X'FF'-X'20'   UNPROTECT FIELD                         
         NI    HIADAT2H+1,X'FF'-X'20'                                           
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2   CLEAR OUT WORK2                       
*                                                                               
*              DISPLAY EFFECTIVE DATES                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        HIATUS DATES ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DISP200             CLEAR FIELDS AND PRINT COMMENTS              
*                                                                               
         LA    R7,WORK2            OUTPUT                                       
         LA    R8,WORK2+60         OUTPUT END                                   
         ZIC   R4,1(R6)                                                         
         AR    R4,R6               R4 = ELEMENT END                             
         LA    R6,2(R6)            R6 = POSITION IN ELEMENT                     
*                                                                               
DISPDTES LA    R3,WORK+20          BUILD AREA                                   
*              DISPLAY DATES                                                    
* 3 BYTE DATE ENTRIES: 2 BYTE COMPRESSED START DATE, 1 BYTE NUM OF              
* DAYS THAT HIATUS LASTS FROM START DATE                                        
         GOTO1 DATCON,DMCB,(2,(R6)),(4,(R3))     START DATE                     
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DISP100                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
DISP100  AR    R3,RE                                                            
*                                                                               
         CLI   2(R6),0             NON-ZERO NUM OF DAYS FROM START DAT?         
         BE    DISP160             NO                                           
*                                                                               
         MVI   0(R3),C'-'          YES, THERE ARE SOME DAYS                     
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R6)),WORK3   START DATE FOR ADDAY                
         ZIC   RE,2(R6)                     GET NUMBER OF DAYS                  
         ST    RE,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK3,WORK3+6,    END DATE IN ADDAY FORM              
*                                                                               
         GOTO1 DATCON,DMCB,WORK3+6,(4,(R3))  END DATE IN SCREEN FORM            
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DISP120                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
DISP120  AR    R3,RE                                                            
*                                                                               
DISP160  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R8               WORK2+60                                     
         BNH   DISP164                                                          
* FIRST LINE EXCEEDED - START AT HIADAT2                                        
         LA    R8,500(R8)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DISP164                                                          
         LA    R7,1(R7)                                                         
*                                                                               
DISP164  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
*                                                                               
         LA    RE,WORK2+120                                                     
         CR    R7,RE               SECOND LINE EXCEEDED?                        
         BNH   *+16                NO                                           
         MVI   WORK2+119,C'>'      YES, DOESN'T FIT                             
         OI    FLAGS,X'20'         ERROR-DATES WON'T FIT ON 2 LINES             
         B     DISP200                                                          
*                                                                               
         LA    R6,3(R6)                                                         
         CR    R6,R4               END OF ELEMENT?                              
         BNL   DISP200             YES                                          
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DISPDTES                                                         
*                                                                               
DISP200  MVC   HIADATE,WORK2       DATES                                        
         MVC   HIADAT2,WORK2+60    MOVE 2D LINE                                 
         OI    HIADATEH+6,X'80'    TRANSMIT                                     
         OI    HIADAT2H+6,X'80'                                                 
*                                                                               
* DISPLAY COMMENTS                                                              
         NI    HIACOMMH+1,X'FF'-X'20'   UNPROTECT FIELD                         
         NI    HIACOM2H+1,X'FF'-X'20'                                           
         MVC   HIACOMM,MYSPACES                                                 
         MVC   HIACOM2,MYSPACES                                                 
         OI    HIACOMMH+6,X'80'                                                 
         OI    HIACOM2H+6,X'80'                                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL            ANY COMMENT ELEMENTS?                        
         BNE   DISP240             NO, GO OUT                                   
*                                                                               
         LA    R2,HIACOMM          1ST LINE                                     
         NI    FLAGS,X'FF'-X'80'   1ST LINE NOT COMPLETED YET                   
         USING RCONHCEL,R6                                                      
DISP220  ZIC   R1,RCONHCLN         ELEMENT LENGTH                               
         LA    R4,2                                                             
         SR    R1,R4               SUBTRACT OVERHEAD                            
         LTR   R1,R1               ZERO LENGTH?                                 
         BZ    DISP230             YES, DON'T DISPLAY ANYTHING                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RCONHCCM    MOVE TO SCREEN                               
         DROP  R6                                                               
*                                                                               
DISP230  TM    FLAGS,X'80'         1ST LINE ALREADY BEEN DONE?                  
         BO    DISP240             YES, SECOND LINE JUST DONE ALSO              
         BAS   RE,NEXTEL           NO, GET ANOTHER ELEMENT                      
         BNE   DISP240             NO MORE TO DISPLAY                           
*                                                                               
         LA    R2,HIACOM2          SECOND LINE                                  
         OI    FLAGS,X'80'         1ST LINE BEEN DONE                           
         B     DISP220                                                          
*                                                                               
DISP240  DS    0H                                                               
         LA    R2,CONBACTH         PUT CURSOR IN BUY ACTION FIELD               
         LA    R3,NDOVFLER         DATES DON'T FIT IN SPACE AVAILABLE           
         TM    FLAGS,X'20'         OVERFLOW ERROR?                              
         BO    ERROR                                                            
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* PRINT DARE AGENCY HIATUS DATES IF ANY                                         
*********************************************************************           
* ADAPTED FROM RECNT63 (SUBROUTINE HIATUS)                                      
DARE     NTR1                                                                   
         MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3    INITIALIZE WORK3 TO SPACES           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL            ANY DARE ORDER ELEMENT?                      
         BNE   DARE80              NO, GO OUT                                   
         LR    R5,R6                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL            ANY HIATUS DATE ELEMENT?                     
         BNE   DARE80              NO                                           
         USING RCONHIEL,R6                                                      
                                                                                
         CLI   RCONHILN,2          SKIP IF NO DATES                             
         BNH   DARE80                                                           
                                                                                
         ZIC   R2,RCONHILN                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RCONHIDT                                                      
         DROP  R6                                                               
                                                                                
         LA    R4,WORK3            BUILD AREA                                   
         NI    FLAGS,X'FF'-(X'80'+X'40')     CLEAR FLAGS                        
                                                                                
* IF WEEKLY, WILL TRY TO COLLASP DATES. IE AUG24-3W                             
                                                                                
DARE20   DS    0H                                                               
         LA    R3,1                NUMBER OF CONSECUTIVE WEEKS                  
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,0(R4))                                  
         LA    R4,5(R4)                                                         
                                                                                
         USING RCONDREL,R5                                                      
         TM    RCONDRFG,X'08'      DAILY?                                       
         BO    DARE50                                                           
         DROP  R5                                                               
                                                                                
         CH    R2,=H'1'                                                         
         BNH   DARE40                                                           
                                                                                
DARE30   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R6)),(0,WORK2)                                  
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,WORK2+6)                                
         GOTO1 ADDAY,DMCB,WORK2,WORK,7                                          
         CLC   WORK(6),WORK2+6     IF NEXT DATE IS EXACTLY ONE WEEK             
         BNE   DARE40              AWAY, KEEP LOOKING                           
                                                                                
         MVC   WORK2(6),WORK2+6                                                 
         LA    R3,1(R3)                                                         
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         CH    R2,=H'1'                                                         
         BH    DARE30                                                           
         SR    R2,R2                                                            
                                                                                
DARE40   DS    0H                                                               
         MVI   0(R4),C'-'                                                       
         EDIT  (R3),(2,1(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   1(R4),C'W'                                                       
         LA    R4,2(R4)                                                         
                                                                                
DARE50   DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    *+12                                                             
         BCTR  R2,0                                                             
         LTR   R2,R2               NO MORE IN ELEMENT?                          
         BNZ   DARE53              STILL MORE                                   
*                                                                               
         TM    FLAGS,X'80'         NO MORE, HAS ONE LINE BEEN DONE YET?         
         BO    *+12                                                             
         LA    RF,WORK3+60         NO, COMPARE TO END OF 1ST LINE               
         B     *+8                                                              
         LA    RF,WORK3+120        YES, COMPARE TO END OF 2ND LINE              
*                                                                               
         CR    R4,RF               HAS IT GONE PAST END OF LINE?                
         BNH   DARE80              NO, GO AHEAD AND DISPLAY                     
         OI    FLAGS,X'40'         YES, DO LOGIC TO COMPENSATE FOR END          
         B     DARE55                                                           
                                                                                
DARE53   MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
*                                                                               
DARE55   TM    FLAGS,X'80'         HAS 1ST LINE BEEN DONE YET?                  
         BO    DARE70              YES                                          
*                                                                               
         LA    RF,WORK3+60                                                      
         CR    R4,RF               HAS IT GONE PAST END OF LINE?                
         BH    DARE60              YES                                          
         BE    *+10                EXACTLY END OF LINE                          
         LR    R7,R4               LESS THAN END, SAVE END OF ENTRY             
         B     DARE20                                                           
*                                                                               
         OI    FLAGS,X'80'         EXACTLY END--GO TO NEXT LINE                 
         B     DARE20                                                           
*                                                                               
DARE60   SR    R4,R7               LENGTH OF ENTRY GOING OVER END               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),0(R7)    MOVE ENTRY TO NEUTRAL AREA                   
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK3+60(0),WORK+10   MOVE TO SECOND LINE                        
*                                                                               
         LA    R1,WORK3+60                                                      
         SR    R1,R7               DIFF BETWEEN LAST ENTRY AND LINE END         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),MYSPACES    CLEAR TO END OF 1ST LINE                     
*                                                                               
         TM    FLAGS,X'40'         IS THERE NOTHING MORE IN ELEMENT?            
         BO    DARE80              YES, GO TO END                               
*                                                                               
         LA    R4,1(R4)            COMPENSATE FOR BCTR                          
         LA    R1,WORK3+60                                                      
         AR    R4,R1               GO TO END OF ENTRY ON 2ND LINE               
         OI    FLAGS,X'80'         SECOND LINE                                  
         B     DARE20                                                           
*                                                                               
DARE70   LA    RF,WORK3+120        END OF SECOND LINE                           
         CR    R4,RF               DID IT GO PAST END?                          
         BL    DARE20              NOT TO END YET, GO ON                        
         BE    DARE80              EXACTLY ON END                               
         MVI   WORK3+119,C'>'      PAST END, PUT SPECIAL CHAR                   
         OI    FLAGS,X'20'         OVERFLOW ERROR                               
*                                                                               
DARE80   DS    0H                                                               
         MVC   HIADATE,WORK3       MOVE TO SCREEN                               
         OI    HIADATEH+6,X'80'    AND TRANSMIT                                 
         MVC   HIADAT2,WORK3+60                                                 
         OI    HIADAT2H+6,X'80'                                                 
*                                                                               
         LA    R2,CONBACTH         PUT CURSOR IN BUY ACTION FIELD               
         LA    R3,DAOVFLER         DATES DON'T FIT ON LINES                     
         TM    FLAGS,X'20'         OVERFLOW ERROR?                              
         BO    ERROR                                                            
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* MUCH OF THIS ADAPTED FROM RECNT15                                             
EDIT     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR     CONTRACT ADDRESS                          
         DROP  RF                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC   READ CONTRACT FOR UPDATE                  
*                                                                               
         LA    R2,HIADATEH         1ST FIELD                                    
*                                                                               
* SET UP 2 DATE INPUT FIELDS IN WORK3(PRETEND 1 FIELD)                          
         MVC   WORK3(L'HIADATE+8),HIADATEH                                      
         ZIC   RE,HIADATEH+5       LEN OF FIELD 1                               
         ZIC   RF,HIADAT2H+5       LEN OF FIELD 2                               
         LA    R4,WORK3+8(RE)      END OF FIELD 1                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    DATE30                                                           
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),HIADAT2     FIELD 2                                      
         LA    RE,1(RF,RE)         NEW LEN                                      
         STC   RE,WORK3+5                                                       
DATE30   DS    0H                                                               
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'25',RCONREC)      DELETE OLD HIATUS             
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         LA    R4,WORK2                                                         
         MVI   WORK2,X'25'         BUILD X'25' ELEM IN WORK2                    
         LA    R4,2(R4)                                                         
*                                                                               
         XC    WORK+24(6),WORK+24     TO TEST IF IN CONSECUTIVE ORDER           
         LA    R7,WORK3+7             FOR SCAN                                  
         ST    R7,DMCB+12                                                       
*                                                                               
*              EDIT START DATE                                                  
STARTED  MVC   DMCB(4),DMCB+12                                                  
         GOTO1 SCAN,DMCB,,WORK3       SCAN FOR NEXT DATE FIELD                  
*                                                                               
         CLI   DMCB,0              NONE?                                        
         BNE   DATE50                                                           
*              NO DATE                                                          
         OC    WORK+24(6),WORK+24  ANY DATES GIVEN AT ALL?                      
         BZ    DATE280             NO, EDIT COMMENTS (DATES OPTIONAL)           
         B     DATE270             YES, END OF DATA, ADD ELEMENT                
*                                                                               
DATE50   L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
*                                                                               
*              EDIT START DATE                                                  
         LA    R3,SDTERR                                                        
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
         MVC   WORK+12(2),WORK     K START YEAR                                 
         CLC   WORK+14(4),WORK+2   HIAT MMDD V K MMDD                           
         BNL   DATE150                                                          
*              HIAT MMDD LOW                                                    
DATE90   CLC   WORK(2),WORK+6      K START AND END YEARS SAME?                  
         BE    ERROR                                                            
         MVC   WORK+12(2),WORK+6   USE K END YEAR                               
*                                                                               
DATE150  CLC   WORK+12(6),WORK+6   HIAT START V K END                           
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
         CLC   WORK+12(6),WORK+24  HIAT START DATE V LAST ELEM END DATE         
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
*                                                                               
DATE175  EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATE177                                                          
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATE176                                                          
         CLI   0(R6),0                                                          
         BE    DATE176                                                          
         CLI   0(R6),C' '                                                       
         BNE   DATE180                                                          
* NO END DATE GIVEN                                                             
DATE176  LR    R5,R6                                                            
DATE177  MVC   WORK+18(6),WORK+12  MAKE END DATE SAME AS START DATE             
         B     DATE230                                                          
*                                                                               
DATE180  GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18   END DATE                          
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATE200                                                          
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
*                                                                               
* END DATE IS VALID MONTH-DAY                                                   
DATE200  MVC   WORK+18(2),WORK+6   K END YEAR                                   
         CLC   WORK+20(4),WORK+8   HIAT END MMDD V K END MMDD                   
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE K START YEAR                            
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
DATE215  LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   HIAT END V K END DATE                        
         BH    ERROR                                                            
*                                                                               
         CLC   WORK+18(6),WORK+12  HIAT END V HIAT START                        
         BL    ERROR                                                            
*                                                                               
DATE230  MVC   WORK+24(6),WORK+18  SAVE HIAT END DATE TO TEST IF IN             
*                                  CONSECUTIVE ORDER                            
*                                                                               
         GOTO1 DATCON,DMCB,WORK+12,(2,(R4))     START DATE                      
*                                                                               
         CLC   WORK+18(6),WORK+12  ONLY ONE DAY?                                
         BNE   DATE240             NO                                           
*                                                                               
         LA    RE,0                YES, 0 DAYS PAST START DATE                  
         B     DATE260                                                          
*                                                                               
DATE240  GOTO1 DATCON,DMCB,WORK+12,(11,WORK+30)  START DATE FOR PERVAL          
         MVI   WORK+38,C'-'                                                     
         GOTO1 DATCON,DMCB,WORK+18,(11,WORK+39)  END DATE FOR PERVAL            
*                                                                               
         GOTO1 VPERVAL,DMCB,(17,WORK+30),PERAREA  GET NUM OF DAYS               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PERAREA        NUMBER OF DAYS                               
*                                                                               
         LA    R3,HIALGERR         HIATUS MAY NOT BE > 256 DAYS                 
         LA    R1,256                                                           
         CR    RE,R1               GREATER THAN 256 DAYS?                       
         BH    ERROR               YES                                          
*                                                                               
         LTR   RE,RE                                                            
         BZ    ERROR                                                            
         BCTR  RE,0                NUM OF DAYS PAST START DATE                  
*                                                                               
DATE260  STC   RE,2(R4)            STORE # OF DAYS PAST START DATE              
         LA    R4,3(R4)                                                         
         B     STARTED             SEE IF ANOTHER DATE ENTRY                    
*                                                                               
DATE270  DS    0H                                                               
         LA    RE,WORK2            BEGINNING OF BUILD AREA                      
         SR    R4,RE               GET ELEM LENGTH                              
         STC   R4,WORK2+1          ELEMENT LENGTH                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
* EDIT COMMENTS                                                                 
DATE280  DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'26',RCONREC)      DELETE COMMENTS               
*                                                                               
         NI    FLAGS,X'FF'-X'80'   NO LINES DONE YET                            
         LA    R2,HIACOMMH         1ST LINE                                     
         CLI   5(R2),0             ANYTHING THERE?                              
         BNE   DATE290             YES                                          
*                                                                               
DATE285  OI    FLAGS,X'80'         GO TO SECOND LINE                            
         LA    R2,HIACOM2H                                                      
         CLI   5(R2),0             ANYTHING THERE?                              
         BE    DATE300             NO                                           
*                                                                               
DATE290  LA    R4,WORK2            BUILD COMMENT ELEMENT                        
         USING RCONHCEL,R4                                                      
         MVI   RCONHCCO,X'26'                                                   
         ZIC   R1,5(R2)            DATA LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCONHCCM(0),8(R2)   MOVE INTO ELEMENT                            
         LA    R1,3(R1)            COMPENSATE FOR BCTR AND ADD OVERHEAD         
         STC   R1,RCONHCLN                                                      
         DROP  R4                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         TM    FLAGS,X'80'                                                      
         BNO   DATE285                                                          
*                                                                               
DATE300  DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         MVC   CONBACT,MYSPACES    CLEAR BUY ACTION                             
         OI    CONBACTH+6,X'80'                                                 
         B     EXXMOD                                                           
*********************************************************************           
* FROM RECNT15                                                                  
         TITLE 'SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS'                
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1                                                                   
*                                                                               
         L     R2,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R2)       TOTAL LENGTH OF INPUT                             
         LA    R2,8(R4,R2)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R2          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         XIT1                                                                   
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+12                                                             
         MVI   4(R1),C'*'                                                       
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD5D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
***** LOCAL STORAGE                                                             
GENOLD   DSECT                                                                  
         ORG   LOCALVAR                                                         
HIALGERR EQU   589                 HIATUS MAY NOT BE > 256 DAYS                 
NDOVFLER EQU   591                 NON-DARE DATES DON'T FIT ON SCREEN           
DAOVFLER EQU   594                 DARE DATES DON'T FIT ON SCREEN               
VPERVAL  DS    A                                                                
PERAREA  DS    CL56                                                             
FLAGS    DS    X                   X'80'=ONE LINE HAS ALREADY BEEN DONE         
*                                  X'40'=NOTHING MORE IN ELEMENT                
*                                  X'20'=ERROR-DATES WON'T FIT                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074RECNT54   05/04/99'                                      
         END                                                                    
