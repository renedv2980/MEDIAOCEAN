*          DATA SET RELFM19    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T80419A,*                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE RIGHT                                                                  
         TITLE 'T80419 - RELFM19 - OFFICE BUDGET REC'                           
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM19 - T80419 - REP FILE PROGRAM                        *           
*                                                                   *           
*********************************************************************           
*  HISTORY OF CHANGES                                               *           
*********************************************************************           
*  04NOV92  (BU ) --- ORIGINAL ENTRY                                *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80419   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80419,R9                                                      
*                                  R9 = SECOND BASE REGISTER                    
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         TITLE 'T80419 - RELFM19 - OFFICE BUDGET RECORD'                        
*                  OFFICE BUDGET RECORDS                                        
*                                                                               
DALLOC$  EQU   ROBD$TOT-ROBDELEM-2      FROM 1ST $ IN ELEMENT                   
DSTATOT$ EQU   ROBDSTOT-ROBDELEM-2                                              
DBUDTAG  EQU   ROBDTAG-ROBDELEM-2                                               
*                                                                               
BUD      EQU   *                                                                
         CLI   BFMTSW,0            DISPLAY OR EDIT?                             
         BNE   BUDGEDIT            GO EDIT NEW DATA                             
         SPACE 1                                                                
*                  DISPLAY ROUTINE                                              
         BAS   RE,GETREC                                                        
BUDGDISP EQU   *                                                                
*                                                                               
*     DISPLAY THE REGULAR BUDGET DOLLARS BY MONTH                               
*                                                                               
         LA    R8,12                                                            
         LA    R2,OBDIHEDH                                                      
         SPACE 1                                                                
         LA    R4,ROBDSTA          A(STATION BUDGETS)                           
         ST    R4,SAVER4           SAVE FOR LATER                               
         ZIC   R5,STARTMO          FISCAL START MONTH FROM                      
*                                  REP RECORD                                   
         BCTR  R5,0                MAKE ZERO RELATIVE                           
         MH    R5,=H'4'            MULTIPLY BY BUCKET SIZE                      
         LA    R4,0(R5,R4)         A(START OF FISCAL STN BUDGET)                
         SPACE 1                                                                
* FIND STARTING POINT TO DISPLAY MONTHS                                         
         SPACE 1                                                                
         LA    R6,MONTHS           CHECK MON# AGAINST LITERAL TABLE             
BUDI0030 CLC   0(1,R6),STARTMO                                                  
         BE    BUDI0050                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   BUDI0040                                                         
         DC    H'0'                PROBLEM WITH START MONTH                     
BUDI0040 LA    R6,4(R6)                                                         
         B     BUDI0030                                                         
         SPACE 1                                                                
BUDI0050 EQU   *                                                                
*                                                                               
*   NOTE:  MONTH LITERALS WERE LOADED TO SCREEN HERE.  HOWEVER, AS              
*     THEY ARE INSERTED BY THE BASE MODULE WHEN THE SCREEN IS LOADED,           
*     THIS IS REDUNDANT, AND HAS BEEN DELETED.                                  
*                                                                               
         SPACE 1                                                                
         BAS   RE,NEXTUF           NEXT UNPROTECTED FIELD                       
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
BUDI0060 EQU   *                                                                
         BAS   RE,MYEDIT           PUT OUT $ AMOUNT FOR MONTH                   
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R6,4(R6)            BUMP A(MONTH TABLE)                          
         ZIC   RF,0(R2)            BUMP TO FORECAST MONTH $$ FIELD              
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            BUMP TO MONTH FIELD                          
         AR    R2,RF                                                            
         BCT   R8,BUDI0050         GO BACK FOR NEXT FIELD                       
         SPACE 1                                                                
         LA    R2,OBDBTOTH         POINT TO STATION TOTAL FIELD                 
         L     R4,SAVER4           RELOAD R4 FOR STA TOTAL AMT                  
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
*                                                                               
BUDI0090 EQU   *                                                                
*                                                                               
*     DISPLAY THE FORECAST BUDGET DOLLARS BY MONTH                              
*                                                                               
         LA    R8,12                                                            
         LA    R4,ROBDELEM         FIND FORECAST BUDGET ELEMENT                 
BUDI0100 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         CLI   0(R4),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(R4),4             FORECAST BUDGET ELEMENT?                     
         BNE   BUDI0100            NO  - GO BACK FOR NEXT                       
         LA    R4,2(R4)            YES - SET A(STATION BUDGETS)                 
         ST    R4,SAVER4           SAVE FOR LATER                               
         ZIC   R5,STARTMO          FISCAL START MONTH FROM                      
*                                  REP RECORD                                   
         BCTR  R5,0                MAKE ZERO RELATIVE                           
         MH    R5,=H'4'            MULTIPLY BY BUCKET SIZE                      
         LA    R4,0(R5,R4)         A(START OF FISCAL STN BUDGET)                
         LA    R2,OBDFJANH         A(FIRST FORECAST BUDGET $$                   
         B     BUDI0120                                                         
BUDI0110 EQU   *                                                                
         BAS   RE,NEXTUF           BUMP TO FORECAST BUDGET $$                   
BUDI0120 EQU   *                                                                
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
BUDI0140 EQU   *                                                                
         BAS   RE,MYEDIT           PUT OUT $ AMOUNT FOR MONTH                   
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R6,4(R6)            BUMP A(MONTH TABLE)                          
         ZIC   RF,0(R2)            BUMP TO MONTH FIELD                          
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            BUMP TO REGULAR BUDGET $$ FIELD              
         AR    R2,RF               POINT TO NEXT MONTH                          
         BCT   R8,BUDI0110                                                      
         SPACE 1                                                                
         LA    R2,OBDFTOTH         POINT TO STATION TOTAL FIELD                 
         L     R4,SAVER4           RELOAD R4 FOR STA TOTAL AMT                  
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
         B     EXXMOD                                                           
         SPACE                                                                  
*                                                                               
*- BUDGET STATUS TABLE.  CL1'TAG BYTE IN RECORD'                                
*                        CL20'TRANSLATION'                                      
BUDSTATB EQU   *                                                                
         DC    C'A',CL20'ALLOCATED'                                             
         DC    C'D',CL20'DE-ALLOCATED'                                          
         DC    X'FF'               END OF TBL                                   
         DS    0H                  ALIGNMENT                                    
         EJECT                                                                  
*                                                                               
*- BUDGET RECORD EDITING ROUTINE                                                
*                                                                               
BUDGEDIT EQU   *                                                                
*                                                                               
*    THIS ROUTINE IS NO LONGER REBUILDING THE ENTIRE RECORD.  IT                
*       MUST RETRIEVE THE EXISTING RECORD IF NEEDED.                            
*                                                                               
         CLI   BACT,C'A'                 ACTION = ADD?                          
         BE    BEDT0010                  YES - SET RECORD LENGTH                
         BAS   RE,GETREC                 NO  - GET RECORD                       
         B     BEDT0020                                                         
BEDT0010 XC    ROBDELEM(110),ROBDELEM    ZERO OUT 01 ELEMENT                    
         MVC   ROBDELEM(2),=X'016E'      ELCODE=01,LENGTH=108                   
         MVC   REC+27(2),=Y(254)         LEN=KEY+CNTRL+01+04 ELTS               
         XC    ROBDELEM+110(110),ROBDELEM+110    ZERO OUT 04 ELEMENT            
         MVC   ROBDELEM+110(2),=X'046E'      ELCODE=01,LENGTH=108               
*                                                                               
* FOR ADDING A RECORD, THE 01 AND 04 ELEMENTS ARE SKELETONIZED                  
*                                                                               
BEDT0020 EQU   *                                                                
*                                                                               
*    EDIT REGULAR BUDGET COLUMN                                                 
*                                                                               
         XC    BYTE,BYTE                                                        
         LA    R4,ROBDSTA          A(BASIC BUDGET FIGURES)                      
*                                                                               
         XC    DALLOC$(4,R4),DALLOC$(R4)                                        
BEDT0030 EQU   *                                                                
         ST    R4,SAVER4           SAVE A($ BUCKETS) FOR TOTALS                 
         ZIC   R5,STARTMO          STARTING MONTH NUMBER                        
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R4,0(R5,R4)         R4=ADDRESS OF STARTING MONTH                 
         ST    R4,ASTART                                                        
         SPACE 1                                                                
         LR    R5,R4                                                            
         LA    R2,OBDSJANH         A(1ST MONTH BUDGET)                          
         LA    R8,OBDIDECH         LAST INPUT FIELD                             
         B     BEDT0050                                                         
BEDT0040 EQU   *                                                                
         BAS   RE,NEXTUF           SKIP FORECAST BUDGET FIELD                   
         BAS   RE,NEXTUF           NEXT REGULAR BUDGET FIELD                    
BEDT0050 EQU   *                                                                
         SR    R0,R0                                                            
         ZIC   R1,5(R2)            ANY DATA?                                    
         LTR   R1,R1               INPUT NOT REQUIRED                           
         BZ    BEDT0080            NO DATA                                      
         SPACE 1                                                                
         CLI   8(R2),C'A'          ANNUAL AMOUNT                                
         BE    BUDA0010                                                         
         CLI   8(R2),C'Q'          QUARTERLY AMOUNT                             
         BE    BUDQ0010                                                         
         SPACE 1                                                                
         CLI   5(R2),8             MAX 8 DIGITS                                 
         BH    FLERR2                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         BCTR  R1,0                                                             
         EX    R1,BEDT0060                                                      
         B     BEDT0070                                                         
*                                                                               
BEDT0060 PACK  DUB,8(0,R2)                                                      
*                                                                               
BEDT0070 EQU   *                                                                
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    FLERR2              MUST BE POSITIVE                             
         B     BEDT0080                                                         
         SPACE 2                                                                
* DEAL WITH ANNUAL OR QUARTERLY AMOUNTS                                         
         SPACE 1                                                                
BUDA0010 CR    R5,R4               A MUST BE IN FIRST MONTH                     
         BNE   FLERR2                                                           
         BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,12               DIVISOR (NO. OF MONTHS)                      
         B     BUDQ0040                                                         
         SPACE 1                                                                
BUDQ0010 LA    R6,4                                                             
BUDQ0020 CR    R5,R4          Q MUST BE IN 1ST,4TH,7TH, OR 10TH MONTH           
         BE    BUDQ0030                                                         
         LA    R5,12(R5)                                                        
         BCT   R6,BUDQ0020                                                      
         B     FLERR2                                                           
         SPACE 1                                                                
BUDQ0030 BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,3                DIVISOR (NO. OF MONTHS)                      
         SPACE 1                                                                
BUDQ0040 L     R1,FULL             TOTAL ANNUAL OR QUARTERLY AMT                
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         ST    R1,MNTHAMT          SAVE AMOUNT                                  
         MR    R0,RF               MULTIPLY AMOUNT BY DIVISOR                   
         S     R1,FULL                                                          
         LCR   R1,R1                                                            
         A     R1,MNTHAMT          GIVES LAST MONTH AMOUNT -                    
         ST    R1,MNTHXAMT         (ENTIRE REMAINDER IN LAST MTH)               
         SPACE 1                                                                
*  PUT AMOUNTS INTO ARRAY                                                       
         BCTR  RF,0                                                             
BUDQ0050 MVC   0(4,R4),MNTHAMT                                                  
         LA    R4,4(R4)                                                         
         BAS   RE,NEXTUF           SKIP OTHER INPUT COLUMN                      
         BAS   RE,NEXTUF                                                        
         BCT   RF,BUDQ0050                                                      
         MVC   0(4,R4),MNTHXAMT    LAST MONTH                                   
         B     BEDT0090                                                         
         SPACE 2                                                                
BEDT0080 ST    R0,0(R4)                                                         
         SPACE 1                                                                
BEDT0090 LA    R4,4(R4)                                                         
         SPACE 1                                                                
         CR    R2,R8               ARE WE AT DEC FOR INTERNAL                   
         BL    BEDT0040            NO, DO NEXT FIELD                            
         BH    BEDT0100            YES, ADD UP TOTALS                           
         SPACE 1                                                                
         L     R5,ASTART                                                        
         BAS   RE,SHIFTMO          GET DATA IN JAN-DEC FORMAT                   
         SPACE 1                                                                
         SPACE 2                                                                
BEDT0100 EQU   *                   *** NO MORE INTERNAL ***                     
         SPACE 1                                                                
*  ADD UP TOTALS                                                                
         LA    R3,12                                                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     R4,SAVER4           SET A($ BUCKETS) FOR TOTALS                  
         LA    R4,48(R4)           BUMP TO 2ND SET OF BUCKETS                   
         SPACE 1                                                                
BEDT0110 A     R6,0(R4)            ACCUMULATE IN R6                             
         LA    R4,4(R4)            BOUNCE THRU MONTHLY FIGURES                  
         BCT   R3,BEDT0110                                                      
         SPACE 1                                                                
         ST    R6,0(R4)            STATION BUDGET TOTAL                         
         SPACE 1                                                                
         LA    R2,OBDBTOTH         POINT TO TOTAL FIELD                         
         SPACE 1                                                                
         LR    R5,R6                                                            
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
         SPACE                                                                  
*                                                                               
*    EDIT FORECAST BUDGET COLUMN                                                
*                                                                               
         LA    R2,OBDFJANH         A(1ST FORECAST BUDGET FIELD)                 
         LA    R4,ROBDELEM         FIND X'04' ELEMENT IN RECORD                 
BEDT0120 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         CLI   0(R4),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(R4),4             X'04' ELEMENT?                               
         BNE   BEDT0120            NO  - GO BACK FOR NEXT                       
         LA    R4,2(R4)            YES - SET A(BASIC BUDGET FIGURES)            
         ST    R4,SAVER4           SAVE A($ BUCKETS) FOR TOTALS                 
         ZIC   R5,STARTMO          STARTING MONTH NUMBER                        
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R4,0(R5,R4)         R4=ADDRESS OF STARTING MONTH                 
         ST    R4,ASTART                                                        
         SPACE 1                                                                
         LR    R5,R4                                                            
         LA    R8,OBDFDECH         LAST INPUT FIELD                             
         B     BEDT0140            SKIP FIELD BUMP                              
BEDT0130 EQU   *                                                                
         BAS   RE,NEXTUF           SKIP BUDGET COLUMN                           
         BAS   RE,NEXTUF           NEXT FORECAST FIELD                          
BEDT0140 EQU   *                                                                
         SR    R0,R0                                                            
         ZIC   R1,5(R2)            ANY DATA?                                    
         LTR   R1,R1               INPUT NOT REQUIRED                           
         BZ    BEDT0170            NO DATA                                      
         SPACE 1                                                                
         CLI   8(R2),C'A'          ANNUAL AMOUNT                                
         BE    BFDA0010                                                         
         CLI   8(R2),C'Q'          QUARTERLY AMOUNT                             
         BE    BFDQ0010                                                         
         SPACE 1                                                                
         CLI   5(R2),8             MAX 8 DIGITS                                 
         BH    FLERR2                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         BCTR  R1,0                                                             
         EX    R1,BEDT0150                                                      
         B     BEDT0160                                                         
*                                                                               
BEDT0150 PACK  DUB,8(0,R2)                                                      
*                                                                               
BEDT0160 EQU   *                                                                
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    FLERR2              MUST BE POSITIVE                             
         B     BEDT0170                                                         
         SPACE 2                                                                
* DEAL WITH ANNUAL OR QUARTERLY AMOUNTS                                         
         SPACE 1                                                                
BFDA0010 CR    R5,R4               A MUST BE IN FIRST MONTH                     
         BNE   FLERR2                                                           
         BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,12               DIVISOR (NO. OF MONTHS)                      
         B     BFDQ0040                                                         
         SPACE 1                                                                
BFDQ0010 LA    R6,4                                                             
BFDQ0020 CR    R5,R4          Q MUST BE IN 1ST,4TH,7TH, OR 10TH MONTH           
         BE    BFDQ0030                                                         
         LA    R5,12(R5)                                                        
         BCT   R6,BFDQ0020                                                      
         B     FLERR2                                                           
         SPACE 1                                                                
BFDQ0030 BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,3                DIVISOR (NO. OF MONTHS)                      
         SPACE 1                                                                
BFDQ0040 L     R1,FULL             TOTAL ANNUAL OR QUARTERLY AMT                
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         ST    R1,MNTHAMT          SAVE AMOUNT                                  
         MR    R0,RF               MULTIPLY AMOUNT BY DIVISOR                   
         S     R1,FULL                                                          
         LCR   R1,R1                                                            
         A     R1,MNTHAMT          GIVES LAST MONTH AMOUNT -                    
         ST    R1,MNTHXAMT         (ENTIRE REMAINDER IN LAST MTH)               
         SPACE 1                                                                
*  PUT AMOUNTS INTO ARRAY                                                       
         BCTR  RF,0                                                             
BFDQ0050 MVC   0(4,R4),MNTHAMT                                                  
         LA    R4,4(R4)                                                         
         BAS   RE,NEXTUF           SKIP OTHER INPUT COLUMN                      
         BAS   RE,NEXTUF                                                        
         BCT   RF,BFDQ0050                                                      
         MVC   0(4,R4),MNTHXAMT    LAST MONTH                                   
         B     BEDT0180                                                         
         SPACE 2                                                                
BEDT0170 ST    R0,0(R4)                                                         
         SPACE 1                                                                
BEDT0180 LA    R4,4(R4)                                                         
         SPACE 1                                                                
         CR    R2,R8               ARE WE AT DEC FOR INTERNAL                   
         BL    BEDT0130            NO, DO NEXT FIELD                            
         BH    BEDT0190            YES, ADD UP TOTALS                           
         SPACE 1                                                                
         L     R5,ASTART                                                        
         BAS   RE,SHIFTMO          GET DATA IN JAN-DEC FORMAT                   
*                                                                               
BEDT0190 EQU   *                                                                
         LA    R3,12               ACCUMULATE TOTALS                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     R4,SAVER4           SET A($ BUCKETS) FOR TOTALS                  
         LA    R4,48(R4)           BUMP TO 2ND SET OF BUCKETS                   
*                                                                               
BEDT0200 A     R6,0(R4)            ACCUMULATE IN R6                             
         LA    R4,4(R4)            BOUNCE THRU MONTHLY FIGURES                  
         BCT   R3,BEDT0200                                                      
*                                                                               
         ST    R6,0(R4)            STATION BUDGET TOTAL                         
*                                                                               
         LA    R2,OBDFTOTH         POINT TO TOTAL FIELD                         
*                                                                               
         LR    R5,R6                                                            
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
*                                                                               
FLFILE   CLI   BACT,C'A'           ADD ACTION?                                  
         BE    FLADD               YES                                          
*                                  NO  - CHANGE: READ REC, WRITE NEW            
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
         LA    R4,REC              A(OLD RECORD)                                
         LA    R5,REC2             A(NEW RECORD)                                
*                                                                               
FLCH20   EQU   *                                                                
         BAS   RE,XCREC                                                         
         BAS   RE,PUTREC                                                        
         LA    R2,LFMLAST          DISPLAY CHANGED RECORD                       
         B     BUDGDISP            RETURN TO DISPLAY                            
         SPACE 1                                                                
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
         LA    R2,LFMLAST          DISPLAY NEW RECORD                           
         B     BUDGDISP                                                         
         EJECT                                                                  
         TITLE 'T80419 - RELFM19 - GENERAL ROUTINES'                            
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BCR   8,RE                                                             
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
         BR    RE                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
         SPACE 2                                                                
* SUBROUTINE TO MOVE 1000 BYTES TO R4 FROM R5                                   
MOVEREC  MVC   000(250,R4),000(R5)                                              
         MVC   250(250,R4),250(R5)                                              
         MVC   500(250,R4),500(R5)                                              
         MVC   750(250,R4),750(R5)                                              
         BR    RE                                                               
         SPACE 2                                                                
XCREC    LA    R0,4                                                             
         XC    0(250,R4),0(R5)                                                  
         XC    0(250,R5),0(R4)                                                  
         XC    0(250,R4),0(R5)                                                  
         LA    R4,250(R4)                                                       
         LA    R5,250(R5)                                                       
         BCT   R0,XCREC+4                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUBROUTINE TO FORMAT BUDGET DISPLAY                                           
         SPACE 2                                                                
MYEDIT   EDIT  (R5),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
         BR    RE                                                               
         SPACE 2                                                                
TYPEDIT  EDIT  (R5),(10,8(R2)),ZERO=NOBLANK                                     
         BR    RE                                                               
         SPACE 2                                                                
* SUBROUTINE TO SHIFT FISCAL MONTHS TO JAN-DEC FORMAT                           
         SPACE 2                                                                
SHIFTMO  ZIC   R6,STARTMO          SET UP BCT                                   
         SH    R6,=H'13'                                                        
         LCR   R6,R6                                                            
         SPACE 1                                                                
SHIFT5   MVC   0(4,R4),0(R5)                                                    
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,SHIFT5                                                        
         BR    RE                                                               
         SPACE 2                                                                
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL30' '                                                          
MNTHAMT  DS    F                                                                
MNTHXAMT DS    F                                                                
ALLOCTOT DS    F                   TOTAL ALLOCATION                             
ASTART   DS    F                                                                
SAVEYM   DS    CL2                                                              
SAVBUC   DS    CL52                STORE YR/MNTH & 1ST ACTIVITY DATE            
         SPACE 3                                                                
MONTHS   DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CTNF     DC    CL41'NO SUB-RECORD FOUND FOR CONTRACT TYPE    '                  
NRFCT    DC    CL47'NO UPDATE: MAX 5 BUDGET TYPES ALREADY ENTERED'              
BADTOT   DC    CL47'MONTHLY TOTAL NOT EQUAL TO TOTAL ALLOCATION  '              
         EJECT                                                                  
*  SUBROUTINE TO EDIT ANNUAL OR QUARTERLY AMOUNTS                               
         SPACE 1                                                                
TESTAMT  NTR1                                                                   
         LA    R4,9(R2)                                                         
         CLI   5(R2),9             MAX 8 DIGITS & A OR Q                        
         BH    TESTERR                                                          
         ZIC   R0,5(R2)                                                         
         BCTR  R0,0                DON'T INCLUDE A OR Q IN COUNT                
         LR    RE,R0                                                            
         SPACE 1                                                                
TESTA2   CLI   0(R4),C'0'                                                       
         BL    TESTERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    TESTERR                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,TESTA2                                                        
         SPACE 1                                                                
         BCTR  RE,0                SUBTRACT 1 FOR EXECUTE                       
         EX    RE,PCK                                                           
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         B     XIT                                                              
         SPACE 1                                                                
PCK      PACK  DUB,9(0,R2)                                                      
         SPACE 1                                                                
TESTERR  MVI   BYTE,C'Y'           ERROR                                        
XIT      XIT1                                                                   
         EJECT                                                                  
*  ROUTINE TO ADD ELEMENT TO RECORD                                             
ADDEL    NTR1                                                                   
*  SET UP BXLE                                                                  
         LA    R2,REC              A(REC)                                       
         LA    R6,WORK2            A(ELEM)                                      
         MVC   HALF,27(R2)         RECORD LENGTH                                
         LA    R3,34(R2)           1ST ELEM                                     
         MVI   BYTE,2              REPPAK INDICATOR FOR RECUP                   
         SPACE 1                                                                
         LH    R5,HALF             REC LENGTH                                   
         LA    R5,0(R5,R2)         REC END                                      
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         CR    R3,R5               NO ELEMENTS YET?                             
         BH    ADD100                                                           
         SPACE 1                                                                
         CLC   0(6,R6),0(R3)       NEW ELEM V OLD                               
         BL    ADD100              LOW,ADD NEW ELEM                             
         IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,*-14                                                       
         SPACE 2                                                                
*  ADD ELEMENT                                                                  
ADD100   GOTO1 VRECUP,DMCB,(BYTE,(R2)),(R6),(R3)                                
         B     XIT                                                              
         EJECT                                                                  
* RELFMGEN                                                                      
* REGENOBUD                                                                     
* REGENREP                                                                      
* RELFMTWA                                                                      
* RELFMF4D                                                                      
* DDCOMFACS                                                                     
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOBUD                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREP                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF4D                                                       
*                                                                               
*   BUDGET SCREEN WORK AREA                                                     
*                                                                               
         ORG   OBDWORK+128                                                      
BCONTYP  DS    CL1                                                              
BCONDESC DS    CL20                                                             
FOUNDTYP DS    CL1                                                              
NOERRMSG DS    CL1                                                              
BUDACT   DS    CL3                                                              
BUDSCRN  DS    CL1                                                              
SAVER4   DS    F                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RELFM19   05/01/02'                                      
         END                                                                    
