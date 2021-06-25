*          DATA SET REKFACS    AT LEVEL 154 AS OF 05/01/02                      
*PHASE T00ABEA                                                                  
*INCLUDE RETIMVAL                                                               
*INCLUDE REDAYVAL                                                               
         TITLE 'REKFACS  - REPPAK CONTRACT SUBROUTINE FACILITY'                 
** THIS MODULE IS CORE RESIDENT **                                              
***********************************************************************         
* HISTORY                                                             *         
***********************************************************************         
*                                                                     *         
* 06JUN01 RHV  VOILA!                                                 *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*  PARAMETERS FOR CALLING REKFACS:                                              
*                                                                               
*              (RF) = BYTE 0: SUBROUTINE CODE (REKFACSQ)                        
*                                                                               
*              ALL DMCB PARAMETERS DEFINED BY SUBROUTINE                        
*                                                                               
*      NOTE: SOME ROUTINES REQUIRE P4=A(COMFACS)                                
*                                                                               
***********************************************************************         
* REKFACS MAIN PROGRAM                                                          
***********************************************************************         
REKFACS  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*REKFA*,RR=R7                                                  
         LR    R9,RB                                                            
         A     R9,=A(COMMON-REKFACS)                                            
         USING COMMON,R9                                                        
*                                                                               
RKMAIN05 DS    0H                                                               
         LA    RE,RKTAB            FIND SUBROUTINE CODE IN RFTAB                
         B     *+8                                                              
RKMAIN10 LA    RE,6(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID SUBROUTINE CODE!!                    
         CLM   RF,8,0(RE)                                                       
         BNE   RKMAIN10                                                         
*                                                                               
         TM    5(RE),X'80'         EXTERNAL ADDRESS?                            
         BZ    RKMAIN20            NO - INTERNAL                                
         GOTO1 1(RE),(R1),,,,,,,RR=Y  YES -DO IT                                
         B     EXIT                                                             
*                                                                               
RKMAIN20 DS    0H                  INTERNAL ADDRESS                             
         BAS   R8,GETSTOR          GET SOME WORKING STORAGE                     
         USING WORKD,RC                                                         
         ST    RE,MYROUT           SAVE ROUTINE ENTRY                           
         ST    RA,SAVERA           SAVE CALLER'S RA                             
         XC    ANEWIO,ANEWIO                                                    
         TM    5(RE),X'20'         CREATE IO AREA IF P2 NULLS?                  
         BZ    RKMAIN25            NO                                           
         OC    4(4,R1),4(R1)       NULLS?                                       
         BNZ   RKMAIN25            NOT NULLS                                    
         BAS   R8,GETNEWIO         MAKE NEW IO AREA IN CALLERS STORAGE          
         OI    FLAGS,FNEWIO        REMEMBER WE DID THIS                         
         ST    R6,ANEWIO           A(NEW IOAREA) IN CALLER                      
*                                                                               
RKMAIN25 DS    0H                  NOW SAFE TO INIT WORKING STORAGE             
         ST    R7,RELO                                                          
         ST    R1,PSAVE            SAVE A(PARAMETERS)                           
*                                                                               
         L     RE,MYROUT                                                        
         TM    5(RE),X'40'         COMFACS PASSED IN P4?                        
         BZ    RKMAIN30            NO                                           
*                                                                               
         ICM   RF,15,12(R1)        SAVE OFF RFBLOCK PARAMS                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ACOMFACS                                                      
*                                  RESOLVE COMFACS ADDRESSES                    
         USING COMFACSD,RF                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   GETTXT,CGETTXT                                                   
         MVC   CALLOV,CCALLOV                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   DATVAL,CDATVAL                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0     REPFACS                          
         MVC   REPFACS,0(R1)                                                    
*                                                                               
RKMAIN30 DS    0H                                                               
         L     RE,MYROUT                                                        
         TM    5(RE),X'08'        CONTRACT PGM BLOCK IN P5 REQUIRED?            
         BZ    RKMAIN40           NO -SKIP                                      
         OC    16(4,R1),16(R1)    HAVE IT PASSED ALREADY?                       
         BZ    RKMAIN35           NO                                            
         L     RF,16(R1)                                                        
         MVC   ATWA,0(RF)         SAVE STORAGE ADDRESSES                        
         MVC   ATWAWORK,4(RF)                                                   
         B     RKMAIN40                                                         
RKMAIN35 DS    0H                 NEED TO INITALIZE DUMMY CON STORAGE           
         LA    RF,KSTOR                                                         
         ST    RF,ATWA            DUMMY TWA AREA                                
         LA    RE,64(RF)                                                        
         ST    RE,ATWAWORK        DUMMY TWAWORK AREA                            
         L     RE,SAVERA                                                        
         MVC   0(L'TWAHDR1,RF),0(RE) USE CONNECT INFO FROM CALLING PRG          
         GOTO1 =A(LGETPROF),RR=Y  INIT OUR DUMMY STORAGE                        
*                                                                               
RKMAIN40 DS    0H                                                               
         L     R1,PSAVE                                                         
         L     RE,MYROUT                                                        
         ICM   RF,15,1(RE)         TAKE ROUTINE ADDRESS                         
         A     RF,RELO             RELOCATE                                     
         BASR  RE,RF               AND GO!!                                     
*                                                                               
         BNL   EXIT                NOT LOW - GET OUT OF HERE                    
         TM    FLAGS,FNEWIO        DID WE MAKE IOAREA IN CALLER?                
         BZ    EXITL               NO                                           
         BAS   R8,REMNEWIO         WE DIDN'T USE IT, REMOVE IT                  
         B     EXITL                                                            
***********************************************************************         
* SUBROUTINE ADDRESS TABLE                                                      
***********************************************************************         
* RKTAB DEFINITON:                                                              
*        AL1   SUBROUTINE CODE EQUATE (FROM REKFACSQ)                           
*        AL4   SUBROUTINE ADDRESS / VL4 IF EXTERNAL                             
*        XL1   FLAGS: X'80' - EXTERNAL ROUTINE ADDRESS                          
*                     X'40' - REQUIRES A(COMFACS) P4 (INTERNAL ONLY)            
*                     X'20' - IF P2 NULLS, CREATE IO AREA IN CALLER             
*                     X'10' - UNUSED                                            
*                                                                               
*                     X'08' - REQUIRES P5=A(CON PGM STORAGE BLK)                
*                             ELSE AUTO CALL LGETPROF & DUMMY IT                
*                       DEFINED AS:                                             
*                              4-BYTE A(TWA) (RECNTWA)                          
*                              4-BYTE A(TWAWORK AREA)                           
*                                                                               
*                                                                               
*                                                                               
RKTAB    DS    0H                                                               
         DC    AL1(RKVDAYTM),AL4(VDAYTM),X'48'                                  
         DC    AL1(RKGETPRF),AL4(LGETPROF),X'48'                                
         DC    AL1(RKVBLEN),AL4(VBLEN),X'48'                                    
         DC    AL1(RKVBDATE),AL4(VBDATE),X'48'                                  
         DC    AL1(RKVBSPT),AL4(VBSPT),X'48'                                    
         DC    AL1(RKTIMVAL),VL4(RETIMVAL),X'80'                                
         DC    AL1(RKDAYVAL),VL4(REDAYVAL),X'80'                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* >>>>>>>>>>>>>>  START OF SUBROUTINE LIBRARY  <<<<<<<<<<<<<<<<<<<<<< *         
***********************************************************************         
***********************************************************************         
* VBSPT  - VALIDATES CONTRACT BUYLINE SPOTS/WK FIELD                  *         
***********************************************************************         
*                                                                               
* INPUT:   P1: A(SPOTS/WK FIELD HDR)                                            
*          P2: NULL                                                             
*          P3: A(3F PARAM BLOCK)                                                
*                    A(BUYREC)                                                  
*                    A(CONREC)                                                  
*          P4: A(COMFACS)                                                       
*          P5: A(CON PGM STORAGE BLOCK)                                         
*                                                                               
* OUTPUT:  CC: EQUAL                                                            
*          CC: NOT EQUAL                                                        
*              P1: ERROR MSG#                                                   
*              P2: A(ERROR FIELD HDR)                                           
*                                                                               
***********************************************************************         
VBSPT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ADAYFLD,0(R1)                                                    
         MVC   ATIMFLD,4(R1)                                                    
         XC    ABUYREC,ABUYREC                                                  
         XC    ACONREC,ACONREC                                                  
         ICM   RF,15,8(R1)                                                      
         BZ    SPT05                                                            
         MVC   ABUYREC,0(RF)                                                    
         MVC   ACONREC,4(RF)                                                    
*                                                                               
SPT05    L     R2,0(R1)            NUMBER PER WEEK                              
         LA    R3,NPWERR                                                        
*                                                                               
         GOTO1 =A(PACK),RR=Y                                                    
         BNZ   *+12                                                             
         TM    4(R2),X'08'                                                      
         BZ    BVERROR                                                          
         CH    R0,=H'255'                                                       
         BH    BVERROR                                                          
         ICM   R6,15,ABUYREC                                                    
         BZ    EXITOK                                                           
         USING RBUYREC,R6                                                       
         STC   R0,RBUYNW                                                        
         B     EXITOK                                                           
         DROP  R6                                                               
***********************************************************************         
* VBDATE - VALIDATES CONTRACT BUYLINE DATES INPUT FIELD               *         
***********************************************************************         
*                                                                               
* INPUT:   P1: 0   - X'80' - CONTRACT BUYACTION 'BUYF' IN PROGRESS              
*            : 1-3 - A(DATES FLD HDR) SINGLE FIELD                              
*          P2: NULL                                                             
*          P3: A(3F PARAM BLOCK)                                                
*                    A(BUYREC)                                                  
*                    A(CONREC)                                                  
*          P4: A(COMFACS)                                                       
*          P5: A(CON PGM STORAGE BLOCK)                                         
*                                                                               
* OUTPUT:  CC: EQUAL                                                            
*          CC: NOT EQUAL                                                        
*              P1: ERROR MSG#                                                   
*              P2: A(ERROR FIELD HDR)                                           
*                                                                               
***********************************************************************         
VBDATE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ADAYFLD,0(R1)                                                    
         MVC   ATIMFLD,4(R1)                                                    
         XC    ABUYREC,ABUYREC                                                  
         XC    ACONREC,ACONREC                                                  
         ICM   RF,15,8(R1)                                                      
         BZ    DATE05                                                           
         MVC   ABUYREC,0(RF)                                                    
         MVC   ACONREC,4(RF)                                                    
*                                                                               
DATE05   DS    0H                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
*              DELETE ALL PREVIOUS DATE ELEMENTS (CHANGE)                       
         LTR   R8,R8                                                            
         BZ    DATE20                                                           
         GOTO1 =A(DELELEM),DMCB,(3,ABUYREC),RR=Y                                
*                                                                               
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),TWAAGY                                                  
         DROP  RF                                                               
         GOTO1 (RFKFLT,REPFACS),DMCB,ACONREC,WORK,0,DUB                         
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*              GET LOWEST START DAY AND HIGHEST END DAY IN DAY ELEMENTS         
         SR    R5,R5                                                            
         L     R6,ABUYREC                                                       
         USING RBUYREC,R6                                                       
         ZIC   R4,RBUYSTED                                                      
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         STM   R4,R5,DMCB+16                                                    
         DROP  R6                                                               
*                                                                               
DATE20   DS    0H                                                               
         XC    WORK+24(6),WORK+24  FOR CONSECUTIVE TEST                         
         LA    R7,7(R2)          FOR SCAN                                       
         ST    R7,DMCB+12                                                       
*              EDIT START DATE                                                  
STARTED  MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 SCAN,DMCB,,(R2)      SCAN FOR NEXT DATE FIELD                    
*                                                                               
         CLI   DMCB,0              NONE?                                        
         BNE   DATE50                                                           
*              NO DATE                                                          
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    BVERROR                                                          
         B     DATE240                                                          
DATE50   L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      K START INDICATOR                            
         BNE   DATE75                                                           
*                                                                               
*              DETERMINE DATE IN FIRST WEEK OF CONTRACT                         
         LA    R5,2(R5)            NEXT FIELD                                   
         LTR   R8,R8                                                            
         BZ    DATE160                                                          
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),SPACES      VALID K START DATE?                          
         BNE   *+6                                                              
         DC    H'0'                K ERROR                                      
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4) GET 4ST DATE                        
         B     DATE150                                                          
*                                                                               
*              EDIT START DATE                                                  
DATE75   GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    BVERROR                                                          
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
*                                                                               
         LTR   R8,R8               HAVE RECORDS?                                
         BZ    DATE160             NO - SIMPLE VALIDATION                       
*                                                                               
         MVC   WORK+12(2),WORK     K START YEAR                                 
         CLC   WORK+14(4),WORK+2   BUY MMDD V K MMDD                            
         BNL   DATE100                                                          
*              BUY MMDD LOW                                                     
DATE90   CLC   WORK(2),WORK+6      K START AND END YEARS SAME?                  
         BE    BVERROR                                                          
         MVC   WORK+12(2),WORK+6   USE K END YEAR                               
DATE100  GOTO1 GETDAY,DMCB,WORK+12,FULL VALIDATE START DATE                     
*                                                                               
         CLC   FULL(3),SPACES                                                   
         BE    BVERROR                                                          
         LA    R3,SDYERR                                                        
         ZIC   R4,DMCB             START DAY                                    
         C     R4,DMCB+16          SAME AS 1ST DAY?                             
         BE    DATE150                                                          
* TEST FOR FLIGHT BUY WHERE WRONG START DAY OK TO EASE INPUT                    
***      CLC   CONBACT(4),=C'BUYF'                                              
***      BE    DATE110                                                          
         L     R1,PSAVE                                                         
         TM    0(R1),X'80'         'BUYF' ACTION IN PROGRESS?                   
         BO    DATE110             YES                                          
*                                                                               
         CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR                    
         BE    BVERROR                                                          
         B     DATE90              FOR CONTRACTS MORE THAN 1 CALENDER           
* GET CORRECT START DAY                                                         
DATE110  L     R7,DMCB+16          DAY FIELD DAY NO.                            
*                                  YEAR - BUY DATE COULD BE SECOND YEAR         
* GET K START DAY - DEPENDS ON WHETHER K HAS OUT-OF-WEEK DATES                  
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB             K START DAY                                  
*                                                                               
*              DETERMINE END DAY FROM K END DATE                                
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),SPACES      ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,5              IF END DAY IS A FRIDAY                       
         BE    *+12                                                             
         CLI   DMCB,7              OR IF IT IS A SUNDAY                         
         BNE   *+8                                                              
         LA    R6,1                MAKE WEEK NORMAL FLIGHT WEEK (HAVE           
*                                     IT START ON MONDAY)                       
         LR    R3,R7                                                            
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
         CR    R3,R6               DAY V K START DAY                            
         BL    DATE125                                                          
         CR    R4,R6               DATE DAY V K START DAY                       
         BNL   DATE140                                                          
         SH    R7,=H'7'            PREVIOUS WEEK                                
         B     DATE140                                                          
*                                                                               
DATE125  CR    R4,R6               DATE DAY V K START DAY                       
         BL    DATE140                                                          
         AH    R7,=H'7'            NEXT WEEK                                    
* GET PROPER DATE IN WEEK                                                       
DATE140  GOTO1 ADDAY,(R1),WORK+12,DUB,(R7)                                      
         MVC   WORK+12(6),DUB                                                   
         CLC   WORK+12(6),WORK     DATE V K START DATE                          
         BNL   *+12                                                             
         LA    R3,SDYERR                                                        
         B     BVERROR             JUST IN CASE                                 
DATE150  XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START V K END                            
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     BVERROR                                                          
         CLC   WORK+12(6),WORK+24  BUY START DATE V LAST ELEM END DATE          
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     BVERROR                                                          
*              EDIT END DATE                                                    
*              END DATE                                                         
DATE160  DS    0H                                                               
         LTR   R8,R8               HAVE RECORDS?                                
         BZ    EXITOK              NO                                           
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATE175                                                          
         LA    R5,1(R5)                                                         
*              DETERMINE END DATE FROM K END DATE                               
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),SPACES      BVERROR?                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB             K END DAY                                    
*                                                                               
         S     R4,DMCB+20          BUY END DAY                                  
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            PREVIOUS WEEK                                
         LNR   R4,R4                                                            
*              BACK UP K END DATE TO LAST BUY DATE                              
         GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R4)                                   
         B     DATE200                                                          
*                                                                               
DATE175  EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATE177                                                          
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATE176                                                          
         CLI   0(R6),C'('                                                       
         BE    DATE176                                                          
         CLI   0(R6),0                                                          
         BE    DATE176                                                          
         CLI   0(R6),C' '                                                       
         BNE   DATE180                                                          
* NO END DATE GIVEN                                                             
DATE176  LR    R5,R6                                                            
DATE177  DS    0H                                                               
         LTR   R8,R8                                                            
         BZ    DATE210                                                          
         LM    R6,R7,DMCB+16       START AND END DAYS                           
         CR    R6,R7               END IN NEXT WEEK?                            
         BNH   *+8                                                              
         LA    R7,7(R7)                                                         
         SR    R7,R6                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R7)                                  
         B     DATE210                                                          
*                                                                               
DATE180  GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATE199A                                                         
*                                                                               
* CHECK FOR END WEEKS OPTION                                                    
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATE193                                                          
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     BVERROR                                                          
* W HAS BEEN ENTERED - PACK WEEKS                                               
DATE193  LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
*                                                                               
DATE195  CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BVERROR                                                          
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BVERROR                                                          
         LA    R7,1(R7)                                                         
         BCT   R4,DATE195                                                       
* NUMERIC WEEKS ENTERED                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BVERROR                                                          
         MVC   WORK+30(6),WORK+12   START DATE                                  
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
         BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATE199                                                          
*                                                                               
* TEST FOR ALTERNATE WEEKS                                                      
         CLI   0(R5),C'A'                                                       
         BNE   DATE198                                                          
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
* GET NEXT WEEK                                                                 
DATE198  GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATE198          GET NUMBER OF WEEKS-1                        
*                                                                               
* GET DAY SPAN FOR WEEK                                                         
DATE199  L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATE215                                                          
*                                                                               
* END DATE IS VALID MONTH-DAY                                                   
DATE199A MVC   WORK+18(2),WORK+6   K END YEAR                                   
         CLC   WORK+20(4),WORK+8   BUY END MMDD V K END MMDD                    
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE K START YEAR                            
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
*              VALIDATE END DATE                                                
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),SPACES                                                   
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BVERROR                                                          
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DATE200                                                          
***      CLC   CONBACT(4),=C'BUYF'                                              
***      BE    *+12                                                             
         L     R1,PSAVE                                                         
         TM    0(R1),X'80'         'BUYF' ACTION IN PROGRESS?                   
         BO    *+12                YES                                          
         LA    R3,EDYERR                                                        
         B     BVERROR                                                          
* FLIGHT BUY NEED NOT HAVE PROPER END DATE - FIND WEEK AND ADJUST DATE          
         L     R7,DMCB+20          END DAY                                      
* GET K END DAY                                                                 
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB                                                          
         LR    R3,R7                                                            
*                                                                               
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
DATE199G GOTO1 ADDAY,(R1),WORK+18,DUB,(R7)                                      
         MVC   WORK+18(6),DUB                                                   
*                                                                               
         CLC   WORK+18(6),WORK+6   DATE V K END                                 
         BNH   *+12                                                             
         LA    R3,EDYERR                                                        
         B     BVERROR             JUST IN CASE                                 
*                                                                               
DATE200  CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATE210                                                          
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATE210  OI    WORK2+8,X'80'       EVERY WEEK                                   
DATE215  LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    BVERROR                                                          
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    BVERROR                                                          
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR CONSECUTIVE            
*                                  TEST                                         
* SEE IF NO. PER WEEK GIVEN                                                     
         CLI   0(R5),C'('                                                       
         BNE   DATE230                                                          
* GET NPW OVERRIDE - FORMAT IS JAN15(4) JAN15-JAN26(5) JAN5-E(6)                
*                           OR JAN15-8W(3)                                      
         LA    R3,NPWERR                                                        
         SR    R1,R1                                                            
         LA    R5,1(R5)                                                         
         LR    RF,R5                                                            
* CHECK NUMBER PER WEEK                                                         
DATE220  CLI   0(R5),X'F0'                                                      
         BL    BVERROR                                                          
         CLI   0(R5),X'F9'                                                      
         BH    BVERROR                                                          
*                                                                               
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'3'                                                         
         BH    BVERROR                                                          
         CLI   0(R5),C')'                                                       
         BNE   DATE220                                                          
*                                                                               
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    BVERROR                                                          
*                                                                               
         OI    WORK2+8,X'01'       NPW OVERRIDE INDICATOR                       
         STC   R1,WORK2+9          NPW                                          
* NOW GET TOTAL WEEKS                                                           
DATE230  SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATE235  LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATE235                                                          
*                                                                               
         STC   R7,WORK2+10                                                      
*              CONVERT DATES FOR BUYREC                                         
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)    START DATE                    
*                                                                               
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)      END DATE                      
*                                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 =A(ADDELEM),DMCB,ABUYREC,WORK2,RR=Y  ADD DATE ELEM               
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     STARTED             SEE IF ANOTHER DATE ENTRY                    
DATE240  EQU   *                                                                
         LTR   R8,R8                                                            
         BZ    EXITOK                                                           
         GOTO1 =A(CHKMGDS),DMCB,(RC),RR=Y                                       
         BZ    EXITOK              ALL MAKEGOODS OKAY                           
         LA    R3,MG1ERR           MAKEGOOD ERROR: SEND MESSAGE                 
         B     BVERROR                                                          
         EJECT                                                                  
*                                                                               
CHKMGDS  NTR1  BASE=*,LABEL=*                                                   
         L     R8,ABUYREC                                                       
         USING RBUYREC,R8                                                       
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'33'        FIND FIRST MG CHECK ELT                      
         BAS   RE,GETEL                                                         
         BNE   CKMG0100            NONE FOUND - CC = ZERO, EXIT                 
         LR    R5,R6               SAVE A(33 ELT)                               
CKMG0020 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        FIND FIRST 03 ELT                            
         BAS   RE,GETEL                                                         
         B     CKMG0080                                                         
CKMG0040 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
CKMG0060 EQU   *                                                                
         BNE   CKMG0120            NONE FOUND - ERROR                           
CKMG0080 EQU   *                                                                
         CLC   2(6,R6),2(R5)       COMPARE ELEMENT DATES:                       
*                                     NEW 03 ELT VS MG CHECK ELT                
         BNE   CKMG0040            NOT FOUND - GO BACK                          
         OI    RBUYDTIN-RBUYDTEL(R6),X'01'                                      
*                                  TURN ON OVERRIDE FLAG IN 03                  
         ZIC   RF,1(R5)            FOUND - CHECK NEXT 33 ELT                    
         AR    R5,RF                                                            
         CLI   0(R5),X'33'         ANOTHER 33 ELT?                              
         BE    CKMG0020            YES - CHECK IT OUT                           
         GOTO1 =A(DELELEM),DMCB,(X'33',RBUYREC),RR=Y                            
*                                  DELETE MG CHECK ELTS                         
CKMG0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CKMG0200                                                         
CKMG0120 EQU   *                                                                
         LTR   RB,RB               SET CC = NON-ZERO                            
CKMG0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* VBLEN - VALIDATES CONTRACT BUYLINE LENGTH INPUT FIELD               *         
***********************************************************************         
*                                                                               
* INPUT:   P1: A(LENGTH FLD HDR)                                                
*          P2: NULL                                                             
*          P3: A(3F PARAM BLOCK)                                                
*                    A(BUYREC)                                                  
*                    A(CONREC)                                                  
*          P4: A(COMFACS)                                                       
*          P5: A(CON PGM STORAGE BLOCK)                                         
*                                                                               
* OUTPUT:  CC: EQUAL                                                            
*          CC: NOT EQUAL                                                        
*              P1: ERROR MSG#                                                   
*              P2: A(ERROR FIELD HDR)                                           
*                                                                               
***********************************************************************         
VBLEN    NTR1  BASE=*,LABEL=*                                                   
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
         L     R8,ATWAWORK                                                      
         USING TWAWORK,R8                                                       
         L     R2,0(R1)                                                         
         LA    R3,LENERR                                                        
         GOTO1 =A(PACK),RR=Y       LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0060                                                         
* VALID SECONDS                                                                 
*                                                                               
         ICM   R6,15,ACONREC                                                    
         BZ    LENE0040            DON'T HAVE CONTRACT REC                      
         USING RCONREC,R6                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0020            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0030            YES - TREAT AS RTS BUY                       
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0020 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0040                                                         
LENE0030 EQU   *                                                                
         CH    R0,=H'120'                                                       
         BNH   LENE0040                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     BVERROR                                                          
         DROP  R6                                                               
*                                                                               
LENE0040 DS    0H                                                               
         ICM   R6,15,ABUYREC                                                    
         BZ    EXITOK                                                           
         USING RBUYREC,R6                                                       
         STH   R0,HALF                                                          
         MVC   RBUYDUR,HALF                                                     
         B     EXITOK                                                           
         DROP  R6                                                               
* TEST FOR MINUTES                                                              
LENE0060 LA    R4,4                                                             
         LA    R5,8(R2)                                                         
*                                                                               
LENE0080 CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0100                                                         
         CLI   0(R5),X'F0'                                                      
         BL    BVERROR                                                          
         CLI   0(R5),X'F9'                                                      
         BH    BVERROR                                                          
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0080                                                      
         B     BVERROR                                                          
* PACK MINUTES (MINUTES NOT ALLOWED FOR SPOTPAK XFER)                           
LENE0100 DS    0H                                                               
         ICM   R6,15,ACONREC                                                    
         BZ    LENE0120                                                         
         USING RCONREC,R6                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0110            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0115            YES - TREAT AS RTS BUY                       
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0110 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0115 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     BVERROR                                                          
         DROP  R6                                                               
*                                                                               
LENE0120 DS    0H                                                               
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   BVERROR                                                          
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         ICM   R6,15,ABUYREC                                                    
         BZ    EXITOK                                                           
         USING RBUYREC,R6                                                       
         MVC   RBUYDUR,HALF                                                     
         OI    RBUYDUR,X'80'       MINUTES IND                                  
         B     EXITOK                                                           
         DROP  R6                                                               
         DROP  R7,R8                                                            
*                                                                               
PACK     NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
VARPACK  PACK  DUB,8(0,R2)                                                      
***********************************************************************         
* VDAYTM - VALIDATES CONTRACT BUYLINE DAY/TIME INPUT FIELDS           *         
***********************************************************************         
*                                                                               
* INPUT:   P1: A(DAY FIELD HDR)                                                 
*                                                                               
*          P2: A(TIME FIELD HDR)                                                
*                                                                               
*          P3: A(3F PARAM BLOCK)                                                
*                    A(BUYREC)                                                  
*                    A(CONREC)                                                  
*                                                                               
*          P4: A(COMFACS)                                                       
*                                                                               
*          P5: A(CON PGM STORAGE BLOCK)                                         
*                                                                               
* OUTPUT:  CC: EQUAL                                                            
*          CC: NOT EQUAL                                                        
*                                                                               
*              P1: ERROR MSG#                                                   
*              P2: A(ERROR FIELD HDR)                                           
*                                                                               
*                                                                               
***********************************************************************         
VDAYTM   NTR1  BASE=*,LABEL=*                                                   
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
         L     R8,ATWAWORK                                                      
         USING TWAWORK,R8                                                       
         MVC   ADAYFLD,0(R1)                                                    
         MVC   ATIMFLD,4(R1)                                                    
         XC    ABUYREC,ABUYREC                                                  
         XC    ACONREC,ACONREC                                                  
         ICM   R1,15,8(R1)                                                      
         BZ    DAYTIM05                                                         
         MVC   ABUYREC,0(R1)                                                    
         MVC   ACONREC,4(R1)                                                    
*                                                                               
DAYTIM05 DS    0H                                                               
         OC    ABUYREC,ABUYREC     NO BUYREC, JUST VALIDATE                     
         BZ    DAYTIM20                                                         
*                                                                               
*              DELETE ALL DAY-TIME ELEMENTS                                     
         GOTO1 =A(DELELEM),DMCB,(2,ABUYREC),RR=Y                                
*                                                                               
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*              PREPARE TO EDIT STRING OF DAY-TIME FIELDS IN PARALLEL            
DAYTIM20 DS    0H                                                               
         L     R5,ADAYFLD          DAY FLD HEADER                               
         LA    R4,7(R5)            DAY FLD -1                                   
         L     R7,ATIMFLD          TIME FLD HEADER                              
         LA    R6,7(R7)            TIME FLD -1                                  
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
*                                                                               
DAYTIMED DS    0H                                                               
         ICM   R2,15,ADAYFLD                                                    
         BZ    DAY110              NOT VALIDATING DAY FIELD                     
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         XC    WORK2+2(6),WORK2+2                                               
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAYTIM50                                                         
*              NO DAY LENGTH                                                    
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BVERROR                                                          
         OC    ATIMFLD,ATIMFLD     VALIDATING TIMES?                            
         BZ    EXITOK              NO, DON'T WORRY ABOUT NEXT CHECK             
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BVERROR                                                          
* GET START AND END DAYS FOR ALL 02 ELEMENTS                                    
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         ICM   R1,15,ABUYREC                                                    
         BZ    EXITOK              ONLY IF WE HAVE BUYREC                       
         USING RBUYREC,R1                                                       
         STC   R3,RBUYSTED                                                      
         DROP  R1                                                               
*                                                                               
         B     EXITOK              ALL DONE - OK                                
DAYTIM50 MVC   DMCB2(4),DMCB+8      DAY FIELD ADDR + LEN                        
*                                                                               
*              EDIT DAY FIELD                                                   
         GOTO1 =V(REDAYVAL),DMCB2,,WORK2+3,WORK2+2,RR=YES                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BVERROR                                                          
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*              GET FIRST START DAY AND LAST END DAY                             
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    DAY100                                                           
* MAKE SURE NO MORE THAN 7 DAYS COVERED                                         
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     BVERROR             MORE THAN 7 DAYS                             
DAY100   CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
*              EDIT TIME FIELD                                                  
DAY110   ICM   R2,15,ATIMFLD                                                    
         BZ    DAY200              NOT CHECKING TIME FIELD                      
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   DAY120                                                           
         OC    ADAYFLD,ADAYFLD     CHECKING DAYS AS WELL?                       
         BZ    EXITOK              NO - ALL DONE                                
         LA    R3,TIMERR                                                        
         B     BVERROR                                                          
*                                                                               
DAY120   ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAY140                                                           
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BVERROR                                                          
         MVC   WORK2+4(4),=C'VARY'                                              
         B     DAY200                                                           
*                                                                               
DAY140   EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAY150              OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BVERROR                                                          
         MVC   WORK2+4(4),=C'NONE'                                              
         B     DAY200                                                           
*                                                                               
DAY150   MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
         LA    RF,WORK2+4                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY155              6AM - 559AM                                  
         MVI   DMCB+4,X'80'                                                     
*                                                                               
DAY155   DS    0H                                                               
         GOTO1 =V(RETIMVAL),DMCB,,,RR=YES  EDIT TIME                            
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BVERROR                                                          
*                                                                               
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAY200              NO                                           
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY160              6AM - 559AM                                  
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     BVERROR                                                          
*                                                                               
DAY160   DS    0H                                                               
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     BVERROR                                                          
*              ADD DAY-TIME ELEMENT TO BUYREC                                   
DAY200   DS    0H                                                               
         OC    ADAYFLD,ADAYFLD    UNLESS WE HAVE ALL THIS STUFF,                
         BZ    DAYTIMED           WE CAN'T DO ANY UPDATING                      
         OC    ATIMFLD,ATIMFLD                                                  
         BZ    DAYTIMED                                                         
         OC    ACONREC,ACONREC                                                  
         BZ    DAYTIMED                                                         
         OC    ABUYREC,ABUYREC                                                  
         BZ    DAYTIMED                                                         
*                                                                               
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAY230                                                           
*                                                                               
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAY230   DS    0H                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 =A(ADDELEM),DMCB,ABUYREC,WORK2,RR=Y                              
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     DAYTIMED            EDIT NEXT DAY TIME FIELD COMBO               
*                                                                               
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         L     R6,ACONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   VCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         L     R2,ADAYFLD                                                       
         B     BVERROR             CANNOT CROSS CROSS DAY DEFAULT               
                                                                                
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         L     R2,ADAYFLD                                                       
         B     BVERROR                                                          
                                                                                
VCROSS20 DS    0H                                                               
         MVC   RCONCCCD,WORK       UPDATE NEW CROSS DAY                         
***>     OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     VCROSSX                                                          
         DROP  R6                                                               
                                                                                
VCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 =A(ADDELEM),DMCB,ACONREC,WORK,RR=Y                               
         MVC   DMCB(24),DMCB2      RESTORE                                      
***>     OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
VCROSSX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R7,R8                                                            
*                                                                               
* SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS'                              
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1  BASE=*,LABEL=*                                                   
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
*                                                                               
*              ROUTINE TO ADD ELEMENT                                           
*                                                                               
*              PARAMETER 1 =       A(RECORD)                                    
*              PARAMETER 2 =       A(ELEMENT TO BE INSERTED)                    
*              ELEMENT IS ADDED IMMEDIATELY BEFORE HIGHER ELEM OR END           
*                                                                               
ADDELEM  NTR1  BASE=*,LABEL=*                                                   
         LR    R8,R2               A(FLD HEADER)                                
         L     R2,0(R1)                                                         
         L     R6,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R2),0(R6),=C'ADD=CODE'           
         TM    DMCB+12,X'05'       REC TOO LONG                                 
         BZ    ADEL05                                                           
         LR    R2,R8                                                            
                                                                                
*        LA    RF,64(RA)           CHECK IF CALLER SET R2 TO A                  
*        CR    R2,RF               VALID FIELD HEADER ADDRESS                   
*        BL    ADEL02              IF NOT, FORCE CURSOR TO CONCACTH             
*        LA    RF,3520(RA)                                                      
*        CR    R2,RF               MAX SIZE OF SCREEN DSECT                     
*        BNH   ADEL03                                                           
*                                                                               
*DEL02   DS    0H                                                               
*        LA    R2,CONCACTH                                                      
*                                                                               
*DEL03   DS    0H                                                               
         LA    R3,339              RECORD FULL - CHANGE NOT PROCESSED           
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',0),0,0,0                                
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
***      B     ERROR                                                            
ADEL05   EQU   *                                                                
*                                                                               
*- IF THIS IS CONTRACT RECORD, MAKE SURE COMMENTS DON'T EXCEED MAX.             
         CLI   0(R2),X'0C'                                                      
         BNE   ADEL99                                                           
*                                                                               
*- ONLY ADD UP COMMENTS IF WE ADDING A COMMENT TO RECORD.                       
*  (ALLOWS CHANGES TO EXISTING RECORDS)                                         
         LA    R1,CMTEL                                                         
         LA    R0,#CMTEL                                                        
ADEL10   CLC   0(1,R1),0(R6)       TBL -VS- ELEM WE ADDED.                      
         BE    ADEL15                                                           
         LA    R1,1(R1)            NEXT TBL ENTRY                               
         BCT   R0,ADEL10                                                        
         B     ADEL99              NOT A COMMENT ELEM                           
*                                                                               
ADEL15   EQU   *                                                                
         SR    R0,R0               COUNT LENGTH OF ALL COMMENTS HERE            
         LA    RE,34(R2)           A(1ST ELEM)                                  
ADEL20   CLI   0(RE),0                                                          
         BE    ADEL80              END OF RECORD.                               
*                                                                               
*- ACCUMULATE ELEMENT DATA SIZE IF ELEMENT CODE FOUND IN LIST.                  
         LA    R1,#CMTEL           NUMBER OF 1 BYTE ELEMENT CODES.              
         BAS   R3,ADEL40                                                        
CMTEL    DC    X'02'               CONTRACT COMMENT                             
         DC    X'07'               SPL COMMENT                                  
         DC    X'11'               SAR/BOP COMMENT                              
         DC    X'82'               REP ORDER COMMENT                            
         DC    X'92'               STA ORDER COMMENT                            
#CMTEL   EQU   *-CMTEL             INSERT NEW EL CODES ABOVE THIS               
*                                                                               
ADEL40   CLC   0(1,R3),0(RE)       TBL -VS- RECORD EL CODE                      
         BNE   ADEL50                                                           
         ZIC   RF,1(RE)            EL LEN                                       
         BCTR  RF,0                                                             
         BCTR  RF,0                LESS 2 = DATA LENGTH                         
         AR    R0,RF               ADD TO RUNNING LENGTH                        
         B     ADEL60                                                           
ADEL50   LA    R3,1(R3)            NEXT CODE IN LIST                            
         BCT   R1,ADEL40                                                        
ADEL60   EQU   *                   GET NEXT EL IN REC.                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ADEL20                                                           
*                                                                               
*  EXCEEDED MAX?                                                                
ADEL80   LA    R1,2001             MAX+1                                        
         CR    R1,R0                                                            
         BH    ADEL99              COMMENT SIZE OK.                             
*                                                                               
         BCTR  R1,0                R1=200                                       
         SR    R0,R1               R0 = AMOUNT OVER 200                         
         ST    R0,FULL             SAVE FOR EDITING                             
*                                                                               
         EDIT  (4,FULL),(3,TEMP),ALIGN=LEFT                                     
         LR    R4,R0                                                            
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),=H'346'                                                
         GOTO1 GETTXT,DMCB,,,(C'E',0),((R4),TEMP),(X'44',0)                     
         OI    6(R8),X'40'         PUT CURSOR HERE                              
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
***      L     RD,BASERD           INSTANT STACK UNWIND                         
*                                                                               
ADEL99   EQU   *                                                                
         B     EXITOK                                                           
         EJECT                                                                  
*                                                                               
*              ROUTINE TO DELETE ELEMENT                                        
*                                                                               
*              THIS ROUTINE DELETES ALL ELEMENTS WITH GIVEN CODE                
*              PARAMETER 1 =       BYTE  0   = ELEMENT CODE TO DELETE           
*                                  BYTES 1-3 = A(RECORD)                        
                                                                                
DELELEM  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(RECORD)                                    
         ZIC   R3,0(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),((R3),(R2)),0                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*- LGETPROF -- GET CONTRACT PROGRAM PROFILES FROM REP RECORD                    
*             AND SAVE IN THE TWA.  SET ALL BITS TO OFF IF                      
*             NO PROFILE ELEMENT FOUND.                                         
*                                                                               
*           P1 - NULL                                                           
*           P2 - NULL                                                           
*           P3 - NULL                                                           
*           P4 - ACOMFACS                                                       
*           P5 - A(CONTRACT STORAGE BLOCK) TO BE INITIALIZED                    
*                DEFINED AS: 4-BYTE A(TWA) (RECNTWA)                            
*                            4-BYTE A(TWAWORK AREA)                             
*                                                                               
***********************************************************************         
LGETPROF NTR1  BASE=*,LABEL=*                                                   
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
         L     R8,ATWAWORK                                                      
         USING TWAWORK,R8                                                       
*                                                                               
         XC    TWATIME,TWATIME     INIT                                         
         XC    PROFDATA,PROFDATA   START CLEAN                                  
         MVI   PROFEQU,RREPQCNT    WE CHECKED FOR PROFILES                      
*                                                                               
         XC    KEY,KEY             GET REP RECORD                               
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),TWAAGY                                                 
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),TWAAGY                                                  
         GOTO1 (RFGETREC,REPFACS),DMCB,KEY,IOAREA,0,DUB                         
         BNE   GPROFEXT            REC NOT FOUND                                
*                                                                               
         LA    R5,IOAREA                                                        
         USING RREPREC,R5                                                       
*                                                                               
         MVC   TWARTS,RREPPROF+11  SAVE REP TO SPOT XFER CONTYPE                
*                                                                               
         CLI   RREPPROF+27,C'Y'    DAILY PACING?                                
         BNE   *+8                 NO                                           
         OI    TWAFLAGS,X'08'      YES - SET TWA INDICATOR                      
*                                                                               
         CLI   RREPPROF+4,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'80'       USE 6A-559 INSTEAD                           
*                                  IF N USE NSI DEFAULT, IF Y USE ARB           
         CLI   RREPPROF+5,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'40'       USE ARB                                      
*                                                                               
         CLI   RREPPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'20'       USE ALTERNATE SPL SCREEN                     
*                                                                               
         CLI   RREPPROF+7,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'10'       ALLOW HIST/INV DISPLAY                       
*                                                                               
         LA    R6,RREPREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   GPROFEXT            NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    GPROF20                                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GPROFEXT            CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
GPROF20  MVC   PROFDATA,RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GPROFEXT EQU   *                                                                
*                                                                               
*  BYPASS LOCKOUT TESTS:  ACTIVATE NEXT STATEMENT                               
*                                                                               
*        B     GPROFXT3                                                         
*                                                                               
*        CLC   RREPKREP,=C'AQ'     ALLIED LOCKOUT?                              
*        BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'NK'     KATZ NATIONAL LOCKOUT?                       
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'GP'     GROUP W LOCKOUT?                             
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'CM'     CONCERT LOCKOUT?                             
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'I1'     MAJOR MARKET LOCKOUT?                        
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'TO'     TORBET LOCKOUT?                              
         BNE   GPROFXT3            NO                                           
GPROFXT2 EQU   *                                                                
         MVI   PROFEQU+1,1         YES - SET FLAG                               
GPROFXT3 EQU   *                                                                
         TM    TWAFLAGS,TWAFLAV2   AVN TERMINAL?                                
         BZ    GPROFXT4                                                         
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA CAN'T USE EXP BUY SCRN          
GPROFXT4 EQU   *                                                                
         B     EXITOK                                                           
         DROP  R5,R7,R8                                                         
***********************************************************************         
* >>>>>>>>>>>>>>>>  END OF SUBROUTINE LIBRARY  <<<<<<<<<<<<<<<<<<<<<< *         
***********************************************************************         
***********************************************************************         
* COMMON ROUTINES & CONSTANTS                                                   
***********************************************************************         
COMMON   DS    0H                                                               
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
*                                                                               
BVERROR  DS    0H                  BUY VALIDATION ROUTINE ERROR EXIT            
         L     R1,PSAVE                                                         
         ST    R3,0(R1)                                                         
         ST    R2,4(R1)                                                         
         B     EXITL                                                            
*                                                                               
         GETEL  R6,34,ELCODE       REPFILE                                      
         GETEL2 R6,28,ELCODE       CTFILE                                       
*----------------------------------------------------------------------         
* GETS WORKING STORAGE FOR CURRENT MODULE                             *         
* USES R2,R3,R4,R5,RD THIS ROUTINE CLEARS WORKING STORAGE!!!!!        *         
*----------------------------------------------------------------------         
GETSTOR  DS    0H                                                               
         ICM   R4,15,=AL4((((WORKDX-WORKD)/8)+1)*8)                             
         L     R3,=AL4(WORKDX-WORKD)                                            
         TM    5(RE),X'08'         REQUIRES CONTRACT WORK AREAS?                
         BZ    GETSTOR4            NO                                           
         OC    16(4,R1),16(R1)     HAVE A(WORK AREAS)?                          
         BNZ   GETSTOR4            YES                                          
         ICM   R4,15,=AL4((((WORKDKSX-WORKD)/8)+1)*8)                           
         L     R3,=AL4(WORKDKSX-WORKD)  MAKE EXTRA STORAGE                      
GETSTOR4 L     RD,4(RD)                                                         
         L     R2,8(RD)            OLD FWD PTR                                  
         AR    R2,R4               PLUS STORAGE LEN                             
         STCM  R2,15,8(RD)         NEW FWD PTR                                  
         STCM  RD,15,4(R2)         NEW BACK PTR                                 
         LR    RD,R2               NEW RD                                       
*                                                                               
         LR    R2,RC               CLEAR NEW WORKING STORAGE                    
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
* GRAB IO SPACE IN CALLERS WORKING STORAGE                            *         
* USES R2,R3,R4,R5,RD,RC THIS ROUTINE CLEARS WORKING STORAGE!!!!!               
*----------------------------------------------------------------------         
GETNEWIO DS    0H                                                               
         ICM   R4,15,=AL4(NEWIOLEN) STORAGE LENGTH TO ACQUIRE                   
         L     RD,4(RD)                                                         
         LR    R2,RD                                                            
         AR    R2,R4                                                            
         MVC   0(72,R2),0(RD)      POINTERS & REGS IN NEW LOCATION              
         L     RD,4(R2)                                                         
         L     R6,8(RD)                                                         
         ST    R2,8(RD)                                                         
         L     RD,8(R2)                                                         
         AR    RD,R4                                                            
         ST    R2,4(RD)                                                         
         ST    RD,8(R2)                                                         
         AR    RC,R4               NEW WORKING STORAGE AREA                     
*                                                                               
         LR    R2,R6               CLEAR NEW IOAREA                             
         LR    R3,R4                                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         LR    R2,RC               CLEAR NEW WORKING STORAGE                    
         L     R3,=AL4(WORKDX-WORKD)                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
* REMOVE NEW IO AREA FROM CALLERS WORKING STORAGE                     *         
* USES R2,R3,R4,RD EXTRA SPACE IS PUT AT FRONT OF REKFACS STORAGE     *         
*----------------------------------------------------------------------         
REMNEWIO DS    0H                                                               
         ICM   R4,15,=AL4(NEWIOLEN) STORAGE LENGTH TO REMOVE                    
         L     RD,4(RD)                                                         
         LR    R2,RD                                                            
         SR    R2,R4                                                            
         MVC   0(72,R2),0(RD)      POINTERS & REGS IN NEW LOCATION              
         L     RD,4(R2)                                                         
         ST    R2,8(RD)                                                         
         L     RD,8(R2)                                                         
         SR    RD,R4                                                            
         ST    R2,4(RD)                                                         
         ST    RD,8(R2)                                                         
         SR    RC,R4               NEW WORKING STORAGE AREA                     
         BR    R8                  ALL DONE                                     
*----------------------------------------------------------------------         
NEWIOLEN EQU   1000                                                             
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
DMREAD   DC    C'DMREAD'          COMMANDS                                      
DMRDHI   DC    C'DMRDHI'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
DMADD    DC    C'DMADD'                                                         
DMWRT    DC    C'DMWRT'                                                         
GETREC   DC    C'GETREC'                                                        
PUTREC   DC    C'PUTREC'                                                        
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
REPFIL   DC    C'REPFIL'         FILES                                          
REPDIR   DC    C'REPDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
         LTORG                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
*                                                                               
MYROUT   DS    A                                                                
PSAVE    DS    F                   SAVE A(INCOMING PARAM LIST)                  
RELO     DS    F                                                                
SAVERA   DS    F                                                                
ANEWIO   DS    A                   IOAREA CREATED IN CALLERS STORAGE            
ACOMFACS DS    A                                                                
DATAMGR  DS    A                                                                
DATCON   DS    A                                                                
HELLO    DS    A                                                                
GETTXT   DS    A                                                                
CALLOV   DS    A                                                                
GETDAY   DS    A                                                                
ADDAY    DS    A                                                                
DATVAL   DS    A                                                                
REPFACS  DS    A                                                                
FULL     DS    F                                                                
ACONREC  DS    A                                                                
ABUYREC  DS    A                                                                
ATWA     DS    A                   A(CONTRACT PRG TWAUSER STORAGE)              
ATWAWORK DS    A                   A(CONTRACT PRG TWAWORK STORAGE)              
TEMP     DS    6F                                                               
HALF     DS    H                                                                
HALF2    DS    H                                                                
ELCODE   DS    C                                                                
BYTE     DS    X                                                                
*                                                                               
FLAGS    DS    X                   PROGRAM STATUS FLAGS                         
FNEWIO   EQU   X'80'               CREATED NEW IO AREA IN CALLER                
*                                                                               
WORK     DS    CL200                                                            
KEY      DS    CL32                KEY                                          
KEYSAVE  DS    CL32                KEY SAVED BEFORE READ HIGH                   
IOAREA   DS    XL1000                                                           
*                                                                               
ROUTSTOR DS    0D      ***ORG POINT FOR INDIVIDUAL ROUTINE STORAGE***           
*                                                                               
* VDAYTIM LOCAL STORAGE                                                         
*                                                                               
         ORG   ROUTSTOR                                                         
WORK2    DS    CL256                                                            
**RK3    DS    CL240                                                            
ADAYFLD  DS    F                                                                
ATIMFLD  DS    F                                                                
*                                                                               
WORKDX   EQU   *                                                                
*                                                                               
* CONTRACT PROGRAM DUMMY STORAGE AREA - THIS STORAGE IS ONLY DECLARED           
* FOR MODULES THAT REQUIRE USE OF THIS STORAGE, AND DO NOT HAVE IT'S            
* ADDRESS EXPLICITLY PASSED (FROM CONTRACT PROGRAM CALLS)                       
*                                                                               
KSTOR    DS    CL8000              CONTRACT PROGRAM DUMMY STORAGE AREA          
WORKDKSX EQU   *                                                                
*                                                                               
ACTERR   EQU   75                  CONTRACT ACTION INVALID                      
BACERR   EQU   76                  BUY ACTION INVALID                           
AGYERR   EQU   152                 AGENCY INVALID                               
AOFERR   EQU   77                  AGENCY OFFICE INVALID                        
ADVERR   EQU   153                 ADVERTISER INVALID                           
STAERR   EQU   150                 STATION INVALID                              
PRDERR   EQU   109                 PRODUCT INVALID                              
SALERR   EQU   154                 SALESMAN INVALID                             
OFFERR   EQU   151                 REP OFFICE INVALID                           
RTGERR   EQU   78                  RATING SERVICE INVALID                       
SDTERR   EQU   79                  START DATE INVALID                           
EDTERR   EQU   80                  END DATE INVALID                             
CHGERR   EQU   81                  FIELD CANNOT BE CHANGED                      
CONERR   EQU   82                  CONTRACT NUMBER NOT FOUND                    
DAYERR   EQU   83                  DAY FIELD INVALID                            
TIMERR   EQU   84                  TIME FIELD INVALID                           
LENERR   EQU   85                  LENGTH INVALID                               
SDYERR   EQU   86                  START DAY AND DATE NOT EQUAL                 
EDYERR   EQU   87                  END DAY AND DATE NOT EQUAL                   
NPWERR   EQU   88                  NUMBER PER WEEK INVALID                      
RATERR   EQU   89                  RATE INVALID                                 
BUYERR   EQU   90                  BUY NOT FOUND                                
KDTERR   EQU   91                  CONTRACT DATES OVERLAP BUY DATES             
MGDERR   EQU   92                  MAKE-GOOD FORMAT = MG=MMMDD-N(LINE)          
RESERR   EQU   95                  INVALID RESET FUNCTION                       
RE2ERR   EQU   96                  NO DOUBLE RESET                              
RE3ERR   EQU   97                  K ADDED TODAY OR ALREADY CHANGED             
CATERR   EQU   98                  CATEGORY ERROR                               
KEYERR   EQU   99                  NO KEY CHANGES AFTER 15 DAYS                 
MGNERR   EQU   42                  MISSED LINE NOT FOUND                        
MGLERR   EQU   43                  MG= VALID ONLY IN FIRST COMMENT              
SECERR   EQU   44                  VALID SECTIONS = 1-99                        
BIASSEC  EQU   419                 BIAS SECTION: 2 CHARS NUMERIC                
MGRERR   EQU   45                  MAKE-GOOD RATES MUST BE ZERO                 
MG1ERR   EQU   46                  MUST DELETE MAKE-GOODS FIRST                 
MG2ERR   EQU   47                  MISSED DATE INVALID                          
MG3ERR   EQU   104                 MG MISSED SPOTS = MG=JAN15-1(5)              
PLNERR   EQU   48                  PLAN MUST BE 3 NUMERIC OR 3 ALPHA            
CALERR   EQU   49                  K DATES MUST NOT EXCEED CALENDAR YR          
BUCERR   EQU   100                 K DATES MUST OVERLAP WEEKLY BUCKETS          
DELERR   EQU   103                 HISTORICAL DATA OR BUYS EXIST FOR K          
MAXERR   EQU   93                  MAXIMUM BUY LINES EXCEEDED                   
AODERR   EQU   94                  MULTI-OFFICE AGENCIES REQUIRE OFFICE         
INVINP   EQU   2                   INVALID INPUT FIELD                          
PCTERR   EQU   213                 PCTS TOTAL MORE THAN 100                     
BLNKERR  EQU   214                 ONLY I BLANK FLD ALLOWED                     
BALERR   EQU   215                 AMTS DONT TOT RIGHT                          
PTOTERR  EQU   216                 NO PCT IN TOT                                
MONERR   EQU   13                  INVALID MONTH                                
SPLERR1  EQU   219                 DATA CAN'T BE CHANGED                        
SPLERR2  EQU   220                 THIS MONTH ALREADY REPORTED                  
SPLERR3  EQU   221                                                              
COMERR   EQU   225                 EASI STA/AGY - COMMENT REQUIRED              
PDEMERR  EQU   243                 MUST DESIGNATE PRIMARY DEMO                  
BUD2BIG  EQU   388                 BUDGET FIELD TOO BIG                         
NODATAIN EQU   392                 DATA NOT PERMITTED FOR TYPE N CONS           
*                                     OR GEN/ZZ CONTRACTS                       
BUYORSPL EQU   409                 BUYS AND/OR SPL EXIST FOR CONTRACT           
DAREORDR EQU   434                 DARE ORDER PROHIBITS FIELD CHANGE            
ORDRAWIN EQU   443                 ORDER A WIN: NEED SPL VALUE                  
TRDINCSH EQU   867                 NO TRADE $ IN CASH ORDER                     
*                                                                               
       ++INCLUDE REKFACSQ                                                       
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENACL                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENOWN                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE RECNTWA                                                        
       ++INCLUDE RECNTPROF                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154REKFACS   05/01/02'                                      
         END                                                                    
