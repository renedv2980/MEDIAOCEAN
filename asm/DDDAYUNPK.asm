*          DATA SET DDDAYUNPK  AT LEVEL 031 AS OF 05/01/02                      
*PHASE T00A0FA                                                                  
******CATALP DAYUNPK                                                            
DAYUNPK  TITLE 'DAYUNPK  CONNVERT 1-BYTE DAY CODE TO DAY LITERAL'               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* CODAY - PARAMETER LIST                                              *         
*                                                                     *         
*    WORD 0  BYTE 0     X'FF'= REPPAK (11 BYTE OUTPUT)                *         
*                       X'80'= REPPAK (11 BYTE OUTPUT)                *         
*                       START DAY FOR TRUE 'OUT-OF-WEEKS'             *         
*                       IN FIRST NIBBLE                               *         
*            BYTE 1-3   A(1 BYTE DAY CODE)                            *         
*                       X'40'=MON,X'01'=SUN                           *         
*                       X'80' = OLD STYLE OUT OF WEEK                 *         
*                                                                               
*    WORD 1  BYTE 0     X'07' = FORCE MTWTFSS OUTPUT                  *         
*                       X'80' = OUTPUT IN FRENCH                      *         
*            BYTE 1-3   A(OUTPUT AREA)                                *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
UNDAY    CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY DAYUNPK                                                          
         ENTRY CODAY                                                            
CODAY    EQU   *                                                                
DAYUNPK  EQU   *                                                                
         NMOD1 WORKX-WORKD,DAYUNPK,CLEAR=YES                                    
         USING WORKD,RC                                                         
         MVC   CDPARS,0(R1)        SAVE PARAMETERS                              
*                                                                               
         TM    CDPAR2,X'80'        FRENCH OUTPUT?                               
         BZ    *+16                                                             
         MVC   DTAB,DTAB2          OVERWRITE LONG FORM                          
         MVC   DTAB1,DTAB3         OVERWRITE SHORT FORM                         
*                                                                               
         L     R1,0(R1)            GET BDAY ADDRESS                             
         TM    0(R1),X'7F'         TEST ANY DAY BITS ON                         
         BNZ   CDY00               YES                                          
         L     RE,CDPAR2           INPUT NOT VALID                              
         MVC   0(7,RE),=C'???????' SET THAT INPUT WAS WRONG                     
         B     EXIT                                                             
*                                                                               
CDY00    SR    R2,R2                                                            
         IC    R2,0(R1)            INSERT DAY CDE                               
         LA    R4,CDAYWK+6         POINT TO SUNDAY POSITION                     
         LA    R5,7                SET FOR SUNDAY                               
*                                                                               
CDY01    SR    R3,R3               CLEAR R3                                     
         SRDL  R2,1                SHIFT BIT TO R3                              
         LTR   R3,R3               TEST IF ON                                   
         BZ    *+8                 NO                                           
         STC   R5,0(R4)            YES-STORE DAY NUMBER                         
         BCTR  R4,0                BUMP TO NEXT LOWER DAY                       
         BCT   R5,CDY01            BUMP CDE TO NXT LWR DAY                      
         LTR   R2,R2               TEST IF OUT-OF-WK ROTATOR                    
         BZ    CDY03               NO                                           
         LA    R4,CDAYWK           YES-POINT TO MONDAY                          
         LA    R5,CDAYWK+7         POINT TO NEXT MONDAY                         
         LA    R6,6                SET BCT REG FOR MON THRU SAT                 
*                                                                               
CDY02    CLI   0(R4),0             TEST IF END OF ROTATION                      
         BE    CDY03               YES                                          
         MVC   0(1,R5),0(R4)       NO-MOVE TO NEXT WEEK                         
         MVI   0(R4),0             ZERO THIS WEEK                               
         LA    R5,1(R5)            BUMP NXT WEEK                                
         LA    R4,1(R4)            AND THIS WEEK POINTERS                       
         BCT   R6,CDY02            TEST NEXT DAY                                
*                                                                               
CDY03    LA    R8,13               SET BCT REG FOR MAX 13 DAY SCAN              
         LA    RE,DYLTL            POINT TO DAY LITERAL FORMAT AREA             
         MVI   DSCMSW,128          SET FOR 1ST TIME                             
         LA    R4,CDAYWK            POINT TO THIS MONDAY                        
         LA    R5,DTAB-3            BASE ADDRESS FOR DAYTAB                     
         SR    R7,R7                                                            
*                                  SET UP FOR TRUE OUT-OF-WEEKS                 
*                                  ----------------------------                 
         CLI   CDPAR1,X'FF'        NOT IF OLD REP CALL                          
         BE    CDY04                                                            
         MVC   BYTE,CDPAR1         START DAY IN FIRST NIBBLE                    
         NI    BYTE,X'7F'          STRIP OUT ANY REP INDICATOR                  
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         SRA   R1,4                NO START DAY MEANS NOT OUT OF WEEK           
         BZ    CDY04                                                            
         MVC   CDAYWK+7(7),CDAYWK  COPY DAYS                                    
         LA    R4,CDAYWK-1(1)      POINT TO FIRST USED DAY                      
         LA    R8,7                ONLY SCAN 7 DAY POSITIONS                    
*                                                                               
CDY04    CLI   0(R4),0             TEST FOR ACTIVE DAY                          
         BNE   CDY045              YES                                          
         TM    DSCMSW,16           TEST IF 2ND TIME THRU ON ROTATION            
         BZ    CDY041              NO                                           
         LA    RE,2(RE)            YES-BUMP POINTER BY 2                        
         B     CDY043              SET FOR COMMA                                
*                                                                               
CDY041   TM    DSCMSW,1            TEST IF 3RD TIME THRU ON ROTATION            
         BZ    CDY044              NO                                           
         LA    RE,1(RE)            BUMP OVER END DAY OF ROTATION                
         CLI   0(RE),C' '          TEST IF TRUNCATED                            
         BZ    *+8                 YES                                          
         LA    RE,1(RE)            NO - BUMP OVER 2ND CHAR OF DAY               
*                                                                               
CDY043   MVI   DSCMSW,2            SET FOR COMMA                                
*                                                                               
CDY044   LA    R4,1(R4)            BUMP TO NEXT DAY                             
         BCT   R8,CDY04                                                         
         B     CDY048              END-COMPUTE LENGTH OF LTL                    
*                                                                               
CDY045   SR    R6,R6                                                            
         IC    R6,0(R4)            INSERT DAY NUMBER                            
         MH    R6,=H'3'            MULT BY DAY ABBREV LGTH                      
         LA    R6,0(5,R6)          POINT TO TABLE ENTRY                         
         LA    R2,1                SET MOVE LGTH                                
         TM    DSCMSW,128                                                       
         BZ    *+12                IF 1ST TIME                                  
         MVI   DSCMSW,0                                                         
         LA    R2,2                MOVE 3 CHARS                                 
         TM    DSCMSW,16           IF LOOKING FOR NXT DAY OF ROTATION           
         BZ    *+20                                                             
         MVI   DSCMSW,1            SET FOR SUBSEQUENT DAYS                      
         LA    RE,2(RE)            BUMP OVER 1ST DAY OF ROTATION                
         MVI   0(RE),C'-'          MOVE IN DASH                                 
         LA    RE,1(RE)                                                         
         CLI   DSCMSW,1            TEST IF DASH OR COMMA WANTED                 
         BNP   CDY046                                                           
         MVI   0(RE),C','                                                       
         TM    CDPAR1,X'80'        REPPAK?                                      
         BNZ   *+8                                                              
         MVI   0(RE),C'/'          SLASH(INSTEAD OF COMMA)                      
         LA    RE,1(RE)                                                         
CDY046   EX    2,MVDAY             MVC  0(0,14),0(6)                            
         TM    CDPAR2,X'80'        FRENCH REPORT?                               
         BNZ   CDY0460             YES                                          
         CLI   0(RE),C'T'          TEST IF TUE/THU                              
         BE    CDY047                                                           
         CLI   0(RE),C'S'          TEST IF SAT/SUN                              
         BE    CDY047                                                           
         B     CDY0461                                                          
*                                                                               
CDY0460  CLI   0(RE),C'M'          TEST IF MAR                                  
         BE    CDY047                                                           
         CLI   0(RE),C'J'          TEST IF JEU                                  
         BE    CDY047                                                           
         CLI   0(RE),C'S'          TEST IF SAM                                  
         BE    CDY047                                                           
         CLI   0(RE),C'D'          TEST IF DIM                                  
         BE    CDY047                                                           
*                                                                               
CDY0461  BCTR  RE,0                1 CHARACTER IS UNIQUE                        
         CLI   DSCMSW,1            TEST IF ROTATOR                              
         BNE   *+12                                                             
         MVI   2(RE),C' '          BLANK  CHARACTER 2                           
         LA    RE,1(RE)            DONT MOVE R14                                
CDY047   CLI   DSCMSW,1            TEST IF LOOKINGFOR  ROTATOR END              
         BE    CDY043+4            YES- RETURN                                  
         MVI   DSCMSW,16           NO-SET FOR 2ND DAY OF ROTATION               
         B     CDY043+4            RETURN FOR NXT DAY                           
*                                                                               
CDY048   LA    RE,DYLTL            POINT TO FORMATTED LITERAL                   
         SR    RF,RF               ZERO LGTH COUNTER                            
         TM    0(RE),X'BF'         EXIT ON FIRST BLANK OR ZERO                  
         BZ    CDY05                                                            
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
* LITERAL FORMATTED - MOVE TO CENTER OF OUTPUT FIELD                            
*                                                                               
CDY05    EQU   *                                                                
         LA    R1,DYLTL(RF)        POINT TO END OF LTRL                         
         BCTR  R1,0                POINT TO 2ND LAST CHAR                       
         BCTR  R1,0                                                             
*                                                                               
         TM    CDPAR2,X'80'        FRENCH REPORT?                               
         BNZ   CDY050              YES                                          
         CLI   0(R1),C'F'                                                       
         BE    CDY055                                                           
         CLI   0(R1),C'W'                                                       
         BE    CDY055                                                           
         CLI   0(R1),C'M'                                                       
         BNE   CDY056                                                           
*                                                                               
CDY050   CLI   0(R1),C'V'                                                       
         BE    CDY055                                                           
         CLC   0(2,R1),=C'ME'                                                   
         BE    CDY055                                                           
         CLI   0(R1),C'L'                                                       
         BNE   CDY056                                                           
*                                                                               
CDY055   BCTR  RF,0                                                             
CDY056   L     R1,CDPAR2           OUTPUT AREA                                  
         TM    CDPAR1,X'80'        REPPAK?                                      
         BZ    SPOTCD                                                           
         MVC   0(11,1),=11C' '                                                  
         STC   RF,CDPAR1           LENGTH                                       
         CH    RF,=H'11'           MAX                                          
         BNH   CODAYX                                                           
         LA    RF,11               MAX LENGTH                                   
         B     CODAYX                                                           
SPOTCD   XC    0(8,R1),0(R1)       SPOTPAK                                      
         CLI   CDPAR2,7                                                         
         BE    FORCE7                                                           
         CH    RF,=H'8'            MAX                                          
         BNH   CODAYX                                                           
         EJECT                                                                  
         BAS   R9,COMPR1           COMPRESS THREE TO TWO                        
         CH    RF,=H'8'            NOW DOES IT FIT                              
         BNH   CODAYX              YES, SEND IT BACK                            
         BAS   R9,COMPR6           COMPRESS TWO TO ONE                          
         CH    RF,=H'8'            DOES IT FIT                                  
         BNH   CODAYX              YES                                          
FORCE7   BAS   R9,SHORT            DO SHORT METHOD (M.W.F..)                    
         SPACE 1                                                                
CODAYX   MVI   0(R1),C'?'          PRESET FOR STRATA EXPRESSION                 
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         BCTR  RF,0                                                             
         EX    RF,MVLTL            MVC  0(0,1),DYLTL                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
MVDAY    MVC   0(0,RE),0(R6)       MOVE DAY ABBRV                               
MVLTL    MVC   0(0,R1),DYLTL                                                    
*                                                                               
DTAB     DC    C'MONTUEWEDTHUFRISATSUN'                                         
DTAB1    DC    C'MOWEFR'                                                        
DTAB2    DC    C'LUNMARMERJEUVENSAMDIM'                                         
DTAB3    DC    C'LUMEVE'                                                        
*                                                                               
         EJECT                                                                  
*              COMPRESS ALL THREE CHARACTER CODES TO TWO                        
         SPACE 1                                                                
COMPR1   LA    R3,DTAB                                                          
         LA    R4,DYLTL                                                         
COMPR2   TM    0(R4),X'C0'         - / OR  ,                                    
         BNO   COMPR4              ARE SKIPPED                                  
         LA    R5,7                                                             
         CLC   0(3,R4),0(R3)       OUTPUT MATCH DAY TABLE                       
         BE    *+16                                                             
         LA    R3,3(R3)            NEXT DAY                                     
         BCT   R5,*-14                                                          
         B     COMPR4              NO MATCH                                     
         SPACE 1                                                                
         LA    RE,DYLTL(RF)        END                                          
         SR    RE,4                LESS ADDRESS OF MATCHING                     
         SH    RE,=H'4'            GET LENGTH FOR EX                            
         BM    COMPR3                                                           
         EX    RE,*+8                                                           
         B     COMPR3                                                           
         MVC   2(0,R4),3(R4)       SHIFT LEFT                                   
COMPR3   BCTR  RF,0                NEW LENGTH                                   
         LA    RE,DYLTL(RF)        NEW END                                      
         MVI   0(RE),X'40'         CLEAR LAST CHARACTER                         
         B     COMPR1              START OVER AGAIN                             
         SPACE 1                                                                
COMPR4   LA    R4,1(R4)                                                         
         CLI   0(R4),X'41'         END OF DYLTL                                 
         BL    0(R9)                                                            
         LA    R3,DTAB                                                          
         B     COMPR2                                                           
         EJECT                                                                  
*              COMPRESS TWO CHARACTER CODES TO ONE                              
         SPACE 1                                                                
COMPR6   LA    R3,DTAB1                                                         
         LA    R4,DYLTL                                                         
COMPR7   TM    0(R4),X'C0'         - / OR ,                                     
         BNO   COMPR9              ARE SKIPPED                                  
         LA    R5,3                                                             
         CLC   0(2,R4),0(R3)       OUTPUT MATCH DAY TABLE                       
         BE    *+16                                                             
         LA    R3,2(R3)            NEXT DAY                                     
         BCT   R5,*-14                                                          
         B     COMPR9              NO MATCH                                     
         SPACE 1                                                                
         LA    RE,DYLTL(RF)                                                     
         SR    RE,4                LESS ADDRESS OF MATCHING                     
         SH    RE,=H'3'            GET LENGTH FOR EX                            
         BM    COMPR8                                                           
         EX    RE,*+8                                                           
         B     COMPR8                                                           
         MVC   1(0,R4),2(R4)       SHIFT LEFT                                   
COMPR8   BCTR  RF,0                                                             
         LA    RE,DYLTL(RF)                                                     
         MVI   0(RE),X'40'         CLEAR LAST CHARACTER                         
         B     COMPR6                                                           
         SPACE 1                                                                
COMPR9   LA    R4,1(R4)                                                         
         CLI   0(R4),X'41'                                                      
         BL    0(R9)                                                            
         LA    R3,DTAB1                                                         
         B     COMPR7                                                           
         EJECT                                                                  
SHORT    MVC   DYLTL(7),=7C'.'                                                  
         SR    R2,R2                                                            
         L     R1,CDPAR1                                                        
         IC    R2,0(R1)                                                         
         LA    R6,DTAB+18          START AT SUNDAY WORK BACK                    
         LA    R4,DYLTL+6                                                       
         LA    R5,7                                                             
         SPACE 1                                                                
SHORT1   SR    R3,R3                                                            
         SRDL  R2,1                                                             
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   0(1,R4),0(R6)       FIRST LETTER TO DYLTL                        
         BCTR  R4,0                                                             
         SH    R6,=H'3'                                                         
         BCT   R5,SHORT1                                                        
*                                                                               
         CLI   BYTE,0              OUT OF WEEK ROTATOR                          
         BE    SHORT2                                                           
         ZIC   15,BYTE                                                          
         SRA   15,4                                                             
         BZ    SHORT2                                                           
         MVC   WORK(7),DYLTL                                                    
         MVC   WORK+7(7),DYLTL                                                  
         LA    15,WORK-1(15)                                                    
         MVC   DYLTL(7),0(15)                                                   
*                                                                               
SHORT2   LA    RF,7                SET LENGTH                                   
         L     R1,CDPAR2                                                        
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
WORKD    DSECT                                                                  
CDPARS   DS    0CL8                                                             
CDPAR1   DS    F                                                                
CDPAR2   DS    F                                                                
DYLTL    DS    D                                                                
WORK     DS    XL16                                                             
CDAYWK   DS    CL14                                                             
DSCMSW   DS    C                                                                
BYTE     DS    C                                                                
         DS    CL4                                                              
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DDDAYUNPK 05/01/02'                                      
         END                                                                    
