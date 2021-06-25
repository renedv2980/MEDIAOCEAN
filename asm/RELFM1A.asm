*          DATA SET RELFM1A    AT LEVEL 038 AS OF 05/01/02                      
*PHASE T8041AA,*                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE RIGHT                                                                  
         TITLE 'T8041A - RELFM1A - SALESPERSON BUDGET RECS'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM1A - T8041A - REP FILE PROGRAM: S/P BUDGET RECORDS    *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*  MOD LOG                                                          *           
*  -------                                                          *           
*  12/07/98 (BU ) --- ORIGINAL ENTRY:  BASED ON RELFM13             *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T8041A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8041A,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         EJECT                                                                  
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
         SPACE 2                                                                
         LA    R2,LFMLAST          POINT TO FIRST TITLE                         
         CLI   BREC,X'15'          S/P BUDGET RECORD?                           
         BE    *+6                 YES                                          
         DC    H'0'                NO  - UNRECOGNIZED                           
         CLI   KEY+1,X'04'         S/P+STATION KEY?                             
         BE    SASTBUD             YES                                          
         CLI   KEY+1,X'05'         S/P W/O STATION KEY?                         
         BE    SALBUD              YES                                          
         DC    H'0'                NO  - UNRECOGNIZED                           
         EJECT                                                                  
*                S/P BUDGET RECORDS                                             
*                                                                               
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
DALLOC$  EQU   RBUD$TOT-RBUDELEM-2      FROM 1ST $ IN ELEMENT                   
DSTATOT$ EQU   RBUDSTOT-RBUDELEM-2                                              
DBUDTAG  EQU   RBUDTAG-RBUDELEM-2                                               
*                                                                               
SASTBUD  EQU   *                                                                
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   SSED0020                                                         
         SPACE 1                                                                
*                  FORMAT ROUTINE                                               
         BAS   RE,GETREC                                                        
SSFM0020 EQU   *                                                                
         MVC   BUDCTYP,=C'TOTAL     ' DEFAULT CONTYPE TITLE                     
         FOUT  BUDCTYPH                                                         
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SSFM0040            NO                                           
         MVC   BUDCTYP,BCONDESC    YES - MOVE CONTRACT TYPE DESC                
SSFM0040 EQU   *                                                                
         LA    R8,12                                                            
         LA    R2,BUDIHEDH                                                      
         SPACE 1                                                                
         LA    R4,RBUDSTA          A(STATION BUDGETS)                           
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SSFM0100            NO  - USE A(RBUDSTA)                         
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
SSFM0060 ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   SSFM0080            END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   SSFM0060            NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A(STA FIGURES) IN FOUND ELEMENT              
         B     SSFM0100            YES - PROCESS IT                             
SSFM0080 MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
         LA    R4,RBUDSTA          LOAD A(BASIC ELEMENT)                        
         LA    R5,0                PUT OUT 0 ALLOC $                            
         B     SSFM0120                                                         
SSFM0100 EQU   *                                                                
*                                                                               
*- DISPLAY TOTAL ALLOCATION AMOUNT, IF ANY.                                     
*                                                                               
         L     R5,DALLOC$(R4)      GET ALLOC $ FROM ELEMENT                     
SSFM0120 EQU   *                                                                
         EDIT  (R5),(15,BUD$TOT),ALIGN=LEFT                                     
         FOUT  BUD$TOTH                                                         
*                                                                               
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
SSFM0140 CLC   0(1,R6),STARTMO                                                  
         BE    SSFM0180                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   SSFM0160                                                         
         DC    H'0'                PROBLEM WITH START MONTH                     
SSFM0160 LA    R6,4(R6)                                                         
         B     SSFM0140                                                         
         SPACE 1                                                                
SSFM0180 MVC   8(3,R2),1(R6)       FILL IN MONTH                                
         FOUT  (R2)                                                             
         SPACE 1                                                                
         BAS   RE,NEXTUF           NEXT UNPROTECTED FIELD                       
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    SSFM0200            NO  - PUT OUT ZERO DOLLARS                   
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
SSFM0200 EQU   *                                                                
         BAS   RE,MYEDIT           PUT OUT $ AMOUNT FOR MONTH                   
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R6,4(R6)            BUMP A(MONTH TABLE)                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO NEXT MONTH                          
         BCT   R8,SSFM0180                                                      
         SPACE 1                                                                
         ZIC   RF,0(R2)            POINT TO STATION TOTAL FIELD                 
         AR    R2,RF                                                            
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    SSFM0220            NO  - PUT OUT ZERO DOLLARS                   
         L     R4,SAVER4           RELOAD R4 FOR STA TOTAL AMT                  
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
SSFM0220 EQU   *                                                                
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
*                                                                               
*- DISPLAY ALLOCATION STATUS (SET BY BUDGET SPREADER)                           
         LA    R1,BUDSTATB                                                      
         LA    R2,BUDSTATH                                                      
         FOUT  (R2),=CL20' ',20                                                 
SSFM0240 CLI   0(R1),X'FF'                                                      
         BE    SSFM0280            NO MATCH.  LEAVE BLANK                       
         CLC   DBUDTAG(1,R4),0(R1) MATCH ELEMENT FLAG VS TABLE                  
         BE    SSFM0260            MATCH ON TAG BYTE.                           
         LA    R1,21(R1)                                                        
         B     SSFM0240                                                         
*                                                                               
SSFM0260 MVC   8(20,R2),1(R1)      TRANSLATION TO SCREEN                        
*                                                                               
SSFM0280 EQU   *                                                                
         MVI   NOERRMSG,C'N'                                                    
         CLI   FOUNDTYP,C'Y'       CONTRACT TYPE FOUND?                         
         BE    SSFM0300            YES                                          
         MVI   NOERRMSG,C'Y'       THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'CTNF),CTNF LOAD MESSAGE: CON TYPE NOT FOUND             
         FOUT  LFMMSGH                                                          
SSFM0300 EQU   *                                                                
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
*                                                                               
*- CL9 DESCRIPTIONS VERSION BY RHV FOR BUDGET ALLOC SCRN COLUMNS                
*                                                                               
BUDSTATN EQU   *                                                                
         DC    C'A',CL9'ALLOC'                                                  
         DC    C'D',CL9'DE-ALLOC'                                               
         DC    X'FF'               END OF TBL                                   
         DS    0H                  ALIGNMENT                                    
         EJECT                                                                  
*                                                                               
*- BUDGET RECORD EDITING ROUTINE                                                
*                                                                               
SSED0020 EQU   *                                                                
*                                                                               
*  NO LONGER REBUILDING ENTIRE RECORD:  RETRIEVE EXISTING IF NEEDED             
*                                                                               
         CLI   BACT,C'A'                 ACTION = ADD?                          
         BE    SSED0040                  YES - SET RECORD LENGTH                
         BAS   RE,GETREC           NO  - RETRIEVE RECORD FROM STORAGE           
         B     SSED0060                                                         
SSED0040 XC    RBUDELEM(110),RBUDELEM    ZERO OUT RECORD                        
         MVC   REC+34(2),=X'016E'        ELCODE=01,LENGTH=108                   
         MVC   REC+27(2),=Y(144)         LEN=KEY+CONTROL+01 ELEMENT             
SSED0060 EQU   *                                                                
         XC    BYTE,BYTE                                                        
         LA    R4,RBUDSTA          A(BASIC BUDGET FIGURES)                      
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SSED0140            NO  -                                        
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                ELIMINATE L'NULL TERMINATOR                  
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
SSED0080 ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   SSED0100            END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   SSED0080            NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A($ BUCKETS IN ELEMENT)                      
         B     SSED0140            YES - PROCESS IT                             
SSED0100 EQU   *                                                                
         CLI   RBUDCNTR,X'5'       THIS IS THE MAX COUNTER                      
         BL    SSED0120            ROOM FOR ONE MORE                            
         LA    R2,LFMACTH                                                       
         MVC   LFMMSG(L'NRFCT),NRFCT LOAD 'NO ROOM' MESSAGE                     
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                EXIT                                         
SSED0120 MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
*                                                                               
*  CONTRACT TYPE BUDGET ELEMENTS ARE SAME FORMAT AS BASIC ELEMENT               
*                                                                               
         LA    R4,WORK2+2               LOAD A($ BUCKETS IN BUILD AREA)         
         XC    WORK2(144),WORK2         INITIALIZE AREA                         
         MVC   WORK2(2),=X'026E'        SET EL CODE, LENGTH                     
         MVC   WORK2+DCONTYP(1),BCONTYP SET CONTRACT TYPE                       
SSED0140 EQU   *                                                                
*                                                                               
*- EDIT TOTAL ALLOCATION AMOUNT FIELD.                                          
         XC    DALLOC$(4,R4),DALLOC$(R4)                                        
         LA    R2,BUD$TOTH                                                      
         CLI   5(R2),0                                                          
         BE    SSED0160            OPTION INPUT.                                
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 =V(NUMVAL),DMCB,BUD$TOT,(R3),RR=YES                              
         CLI   DMCB,X'0'                                                        
         BNE   FLERR3                                                           
         MVC   DALLOC$(4,R4),DMCB+4     MOVE AMOUNT INTO RECORD                 
         MVC   ALLOCTOT,DMCB+4          SAVE AMT FOR TOTAL CHECK                
         SPACE 1                                                                
SSED0160 EQU   *                                                                
         ST    R4,SAVER4           SAVE A($ BUCKETS) FOR TOTALS                 
         ZIC   R5,STARTMO          STARTING MONTH NUMBER                        
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R4,0(R5,R4)         R4=ADDRESS OF STARTING MONTH                 
         ST    R4,ASTART                                                        
         SPACE 1                                                                
         LR    R5,R4                                                            
         LA    R8,BUDIDECH         LAST INPUT FIELD                             
SSED0180 BAS   RE,NEXTUF                                                        
         SR    R0,R0                                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               INPUT NOT REQUIRED                           
         BZ    SSED0360                                                         
         SPACE 1                                                                
         CLI   8(R2),C'A'          ANNUAL AMOUNT                                
         BE    SSED0240                                                         
         CLI   8(R2),C'Q'          QUARTERLY AMOUNT                             
         BE    SSED0260                                                         
         SPACE 1                                                                
         CLI   5(R2),9             MAX 8 DIGITS                                 
         BH    FLERR2                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         BCTR  R1,0                                                             
         EX    R1,SSED0200                                                      
         B     SSED0220                                                         
*                                                                               
SSED0200 PACK  DUB,8(0,R2)                                                      
*                                                                               
SSED0220 EQU   *                                                                
         CVB   R0,DUB                                                           
         C     R0,=F'150000000'                                                 
         BH    FLERR2                                                           
         LTR   R0,R0                                                            
         BM    FLERR2              MUST BE POSITIVE                             
         B     SSED0360                                                         
         SPACE 2                                                                
* DEAL WITH ANNUAL OR QUARTERLY AMOUNTS                                         
         SPACE 1                                                                
SSED0240 CR    R5,R4               A MUST BE IN FIRST MONTH                     
         BNE   FLERR2                                                           
         BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,12               DIVISOR (NO. OF MONTHS)                      
         B     SSED0320                                                         
         SPACE 1                                                                
SSED0260 LA    R6,4                                                             
SSED0280 CR    R5,R4          Q MUST BE IN 1ST,4TH,7TH, OR 10TH MONTH           
         BE    SSED0300                                                         
         LA    R5,12(R5)                                                        
         BCT   R6,SSED0280                                                      
         B     FLERR2                                                           
         SPACE 1                                                                
SSED0300 BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,3                DIVISOR (NO. OF MONTHS)                      
         SPACE 1                                                                
SSED0320 L     R1,FULL             TOTAL ANNUAL OR QUARTERLY AMT                
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
SSED0340 MVC   0(4,R4),MNTHAMT                                                  
         LA    R4,4(R4)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   RF,SSED0340                                                      
         MVC   0(4,R4),MNTHXAMT    LAST MONTH                                   
         B     SSED0380                                                         
         SPACE 2                                                                
SSED0360 ST    R0,0(R4)                                                         
         SPACE 1                                                                
SSED0380 LA    R4,4(R4)                                                         
         SPACE 1                                                                
         CR    R2,R8               ARE WE AT DEC FOR INTERNAL                   
         BL    SSED0180            NO, DO NEXT FIELD                            
         BH    SSED0400            YES, ADD UP TOTALS                           
         SPACE 1                                                                
         L     R5,ASTART                                                        
         BAS   RE,SHIFTMO          GET DATA IN JAN-DEC FORMAT                   
         SPACE 1                                                                
         SPACE 2                                                                
SSED0400 EQU   *                   *** NO MORE INTERNAL ***                     
         SPACE 1                                                                
*  ADD UP TOTALS                                                                
         LA    R3,12                                                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     R4,SAVER4           SET A($ BUCKETS) FOR TOTALS                  
         LA    R4,48(R4)           BUMP TO 2ND SET OF BUCKETS                   
         SPACE 1                                                                
SSED0420 A     R6,0(R4)            ACCUMULATE IN R6                             
         LA    R4,4(R4)            BOUNCE THRU MONTHLY FIGURES                  
         BCT   R3,SSED0420                                                      
         SPACE 1                                                                
         ST    R6,0(R4)            STATION BUDGET TOTAL                         
         SPACE 1                                                                
         LA    R2,BUDIDECH         POINT TO INTERNAL DECEMBER BUDGET            
         ZIC   RF,0(R2)            POINT TO TOTAL                               
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            POINT TO STATION TOTAL                       
         AR    R2,RF                                                            
         SPACE 1                                                                
         LR    R5,R6                                                            
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
         TM    SVPGPBIT,X'04'      IGNORE TOTAL = ALLOC CHECK?                  
         BO    SSED0440            YES - OVERRIDE ERROR CHECK                   
*                                                                               
*  IF THE MONTHLY TOTAL IS NOT EQUAL TO THE TOTAL ALLOCATION, AN                
*     ERROR HAS OCCURRED.                                                       
*                                                                               
         C     R5,ALLOCTOT         MONTH TOTAL = TOTAL ALLOC?                   
         BE    SSED0440            YES -                                        
         LA    R2,BUD$TOTH         THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'BADTOT),BADTOT                                          
*                                  LOAD MSG: TOTAL NOT = ALLOCATION             
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                                                             
SSED0440 EQU   *                                                                
         SPACE                                                                  
         XC    RBUDTAG,RBUDTAG     STARTS AS 0 ON ADD                           
         CLI   FOUNDTYP,C'Y'       TYPE FOUND?                                  
         BE    FLFILE              YES - CHECK FOR ADD OR CHANGE                
         ZIC   R2,RBUDCNTR         NO  - BUMP CONTRACT TYPE COUNTER             
         LA    R2,1(R2)            BUMP BY 1                                    
         STC   R2,RBUDCNTR         REINSERT COUNTER INTO RECORD                 
         BAS   RE,ADDEL            ADD RECORD FROM WORK2                        
         EJECT                                                                  
***->>                                                                          
SALBUD   EQU   *                                                                
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   SAED0020                                                         
         SPACE 1                                                                
*                  FORMAT ROUTINE                                               
         BAS   RE,GETREC                                                        
SAFM0020 EQU   *                                                                
         MVC   BUDCTYP,=C'TOTAL     ' DEFAULT CONTYPE TITLE                     
         FOUT  BUDCTYPH                                                         
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SAFM0040            NO                                           
         MVC   BUDCTYP,BCONDESC    YES - MOVE CONTRACT TYPE DESC                
SAFM0040 EQU   *                                                                
         LA    R8,12                                                            
         LA    R2,BUDIHEDH                                                      
         SPACE 1                                                                
         LA    R4,RBUDSTA          A(STATION BUDGETS)                           
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SAFM0100            NO  - USE A(RBUDSTA)                         
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
SAFM0060 ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   SAFM0080            END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   SAFM0060            NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A(STA FIGURES) IN FOUND ELEMENT              
         B     SAFM0100            YES - PROCESS IT                             
SAFM0080 MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
         LA    R4,RBUDSTA          LOAD A(BASIC ELEMENT)                        
         LA    R5,0                PUT OUT 0 ALLOC $                            
         B     SAFM0120                                                         
SAFM0100 EQU   *                                                                
*                                                                               
*- DISPLAY TOTAL ALLOCATION AMOUNT, IF ANY.                                     
*                                                                               
         L     R5,DALLOC$(R4)      GET ALLOC $ FROM ELEMENT                     
SAFM0120 EQU   *                                                                
         EDIT  (R5),(15,BUD$TOT),ALIGN=LEFT                                     
         FOUT  BUD$TOTH                                                         
*                                                                               
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
SAFM0140 CLC   0(1,R6),STARTMO                                                  
         BE    SAFM0180                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   SAFM0160                                                         
         DC    H'0'                PROBLEM WITH START MONTH                     
SAFM0160 LA    R6,4(R6)                                                         
         B     SAFM0140                                                         
         SPACE 1                                                                
SAFM0180 MVC   8(3,R2),1(R6)       FILL IN MONTH                                
         FOUT  (R2)                                                             
         SPACE 1                                                                
         BAS   RE,NEXTUF           NEXT UNPROTECTED FIELD                       
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    SAFM0200            NO  - PUT OUT ZERO DOLLARS                   
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
SAFM0200 EQU   *                                                                
         BAS   RE,MYEDIT           PUT OUT $ AMOUNT FOR MONTH                   
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R6,4(R6)            BUMP A(MONTH TABLE)                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO NEXT MONTH                          
         BCT   R8,SAFM0180                                                      
         SPACE 1                                                                
         ZIC   RF,0(R2)            POINT TO STATION TOTAL FIELD                 
         AR    R2,RF                                                            
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    SAFM0220            NO  - PUT OUT ZERO DOLLARS                   
         L     R4,SAVER4           RELOAD R4 FOR STA TOTAL AMT                  
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
SAFM0220 EQU   *                                                                
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
*                                                                               
*- DISPLAY ALLOCATION STATUS (SET BY BUDGET SPREADER)                           
         LA    R1,BUDSTATB                                                      
         LA    R2,BUDSTATH                                                      
         FOUT  (R2),=CL20' ',20                                                 
SAFM0240 CLI   0(R1),X'FF'                                                      
         BE    SAFM0280            NO MATCH.  LEAVE BLANK                       
         CLC   DBUDTAG(1,R4),0(R1) MATCH ELEMENT FLAG VS TABLE                  
         BE    SAFM0260            MATCH ON TAG BYTE.                           
         LA    R1,21(R1)                                                        
         B     SAFM0240                                                         
*                                                                               
SAFM0260 MVC   8(20,R2),1(R1)      TRANSLATION TO SCREEN                        
*                                                                               
SAFM0280 EQU   *                                                                
         MVI   NOERRMSG,C'N'                                                    
         CLI   FOUNDTYP,C'Y'       CONTRACT TYPE FOUND?                         
         BE    SAFM0300            YES                                          
         MVI   NOERRMSG,C'Y'       THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'CTNF),CTNF LOAD MESSAGE: CON TYPE NOT FOUND             
         FOUT  LFMMSGH                                                          
SAFM0300 EQU   *                                                                
         B     EXXMOD                                                           
         SPACE                                                                  
*                                                                               
*- BUDGET RECORD EDITING ROUTINE                                                
*                                                                               
SAED0020 EQU   *                                                                
*                                                                               
*  NO LONGER REBUILDING ENTIRE RECORD:  RETRIEVE EXISTING IF NEEDED             
*                                                                               
         CLI   BACT,C'A'                 ACTION = ADD?                          
         BE    SAED0040                  YES - SET RECORD LENGTH                
         BAS   RE,GETREC           NO  - RETRIEVE RECORD FROM STORAGE           
         B     SAED0060                                                         
SAED0040 XC    RBUDELEM(110),RBUDELEM    ZERO OUT RECORD                        
         MVC   REC+34(2),=X'016E'        ELCODE=01,LENGTH=108                   
         MVC   REC+27(2),=Y(144)         LEN=KEY+CONTROL+01 ELEMENT             
SAED0060 EQU   *                                                                
         XC    BYTE,BYTE                                                        
         LA    R4,RBUDSTA          A(BASIC BUDGET FIGURES)                      
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    SAED0140            NO  -                                        
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                ELIMINATE L'NULL TERMINATOR                  
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
SAED0080 ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   SAED0100            END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   SAED0080            NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A($ BUCKETS IN ELEMENT)                      
         B     SAED0140            YES - PROCESS IT                             
SAED0100 EQU   *                                                                
         CLI   RBUDCNTR,X'5'       THIS IS THE MAX COUNTER                      
         BL    SAED0120            ROOM FOR ONE MORE                            
         LA    R2,LFMACTH                                                       
         MVC   LFMMSG(L'NRFCT),NRFCT LOAD 'NO ROOM' MESSAGE                     
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                EXIT                                         
SAED0120 MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
*                                                                               
*  CONTRACT TYPE BUDGET ELEMENTS ARE SAME FORMAT AS BASIC ELEMENT               
*                                                                               
         LA    R4,WORK2+2               LOAD A($ BUCKETS IN BUILD AREA)         
         XC    WORK2(144),WORK2         INITIALIZE AREA                         
         MVC   WORK2(2),=X'026E'        SET EL CODE, LENGTH                     
         MVC   WORK2+DCONTYP(1),BCONTYP SET CONTRACT TYPE                       
SAED0140 EQU   *                                                                
*                                                                               
*- EDIT TOTAL ALLOCATION AMOUNT FIELD.                                          
         XC    DALLOC$(4,R4),DALLOC$(R4)                                        
         LA    R2,BUD$TOTH                                                      
         CLI   5(R2),0                                                          
         BE    SAED0160            OPTION INPUT.                                
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 =V(NUMVAL),DMCB,BUD$TOT,(R3),RR=YES                              
         CLI   DMCB,X'0'                                                        
         BNE   FLERR3                                                           
         MVC   DALLOC$(4,R4),DMCB+4     MOVE AMOUNT INTO RECORD                 
         MVC   ALLOCTOT,DMCB+4          SAVE AMT FOR TOTAL CHECK                
         SPACE 1                                                                
SAED0160 EQU   *                                                                
         ST    R4,SAVER4           SAVE A($ BUCKETS) FOR TOTALS                 
         ZIC   R5,STARTMO          STARTING MONTH NUMBER                        
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R4,0(R5,R4)         R4=ADDRESS OF STARTING MONTH                 
         ST    R4,ASTART                                                        
         SPACE 1                                                                
         LR    R5,R4                                                            
         LA    R8,BUDIDECH         LAST INPUT FIELD                             
SAED0180 BAS   RE,NEXTUF                                                        
         SR    R0,R0                                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               INPUT NOT REQUIRED                           
         BZ    SAED0360                                                         
         SPACE 1                                                                
         CLI   8(R2),C'A'          ANNUAL AMOUNT                                
         BE    SAED0240                                                         
         CLI   8(R2),C'Q'          QUARTERLY AMOUNT                             
         BE    SAED0260                                                         
         SPACE 1                                                                
         CLI   5(R2),9             MAX 8 DIGITS                                 
         BH    FLERR2                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         BCTR  R1,0                                                             
         EX    R1,SAED0200                                                      
         B     SAED0220                                                         
*                                                                               
SAED0200 PACK  DUB,8(0,R2)                                                      
*                                                                               
SAED0220 EQU   *                                                                
         CVB   R0,DUB                                                           
         C     R0,=F'150000000'                                                 
         BH    FLERR2                                                           
         LTR   R0,R0                                                            
         BM    FLERR2              MUST BE POSITIVE                             
         B     SAED0360                                                         
         SPACE 2                                                                
* DEAL WITH ANNUAL OR QUARTERLY AMOUNTS                                         
         SPACE 1                                                                
SAED0240 CR    R5,R4               A MUST BE IN FIRST MONTH                     
         BNE   FLERR2                                                           
         BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,12               DIVISOR (NO. OF MONTHS)                      
         B     SAED0320                                                         
         SPACE 1                                                                
SAED0260 LA    R6,4                                                             
SAED0280 CR    R5,R4          Q MUST BE IN 1ST,4TH,7TH, OR 10TH MONTH           
         BE    SAED0300                                                         
         LA    R5,12(R5)                                                        
         BCT   R6,SAED0280                                                      
         B     FLERR2                                                           
         SPACE 1                                                                
SAED0300 BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,3                DIVISOR (NO. OF MONTHS)                      
         SPACE 1                                                                
SAED0320 L     R1,FULL             TOTAL ANNUAL OR QUARTERLY AMT                
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
SAED0340 MVC   0(4,R4),MNTHAMT                                                  
         LA    R4,4(R4)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   RF,SAED0340                                                      
         MVC   0(4,R4),MNTHXAMT    LAST MONTH                                   
         B     SAED0380                                                         
         SPACE 2                                                                
SAED0360 ST    R0,0(R4)                                                         
         SPACE 1                                                                
SAED0380 LA    R4,4(R4)                                                         
         SPACE 1                                                                
         CR    R2,R8               ARE WE AT DEC FOR INTERNAL                   
         BL    SAED0180            NO, DO NEXT FIELD                            
         BH    SAED0400            YES, ADD UP TOTALS                           
         SPACE 1                                                                
         L     R5,ASTART                                                        
         BAS   RE,SHIFTMO          GET DATA IN JAN-DEC FORMAT                   
         SPACE 1                                                                
         SPACE 2                                                                
SAED0400 EQU   *                   *** NO MORE INTERNAL ***                     
         SPACE 1                                                                
*  ADD UP TOTALS                                                                
         LA    R3,12                                                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     R4,SAVER4           SET A($ BUCKETS) FOR TOTALS                  
         LA    R4,48(R4)           BUMP TO 2ND SET OF BUCKETS                   
         SPACE 1                                                                
SAED0420 A     R6,0(R4)            ACCUMULATE IN R6                             
         LA    R4,4(R4)            BOUNCE THRU MONTHLY FIGURES                  
         BCT   R3,SAED0420                                                      
         SPACE 1                                                                
         ST    R6,0(R4)            STATION BUDGET TOTAL                         
         SPACE 1                                                                
         LA    R2,BUDIDECH         POINT TO INTERNAL DECEMBER BUDGET            
         ZIC   RF,0(R2)            POINT TO TOTAL                               
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            POINT TO STATION TOTAL                       
         AR    R2,RF                                                            
         SPACE 1                                                                
         LR    R5,R6                                                            
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
         TM    SVPGPBIT,X'04'      IGNORE TOTAL = ALLOC CHECK?                  
         BO    SAED0440            YES - OVERRIDE ERROR CHECK                   
*                                                                               
*  IF THE MONTHLY TOTAL IS NOT EQUAL TO THE TOTAL ALLOCATION, AN                
*     ERROR HAS OCCURRED.                                                       
*                                                                               
         C     R5,ALLOCTOT         MONTH TOTAL = TOTAL ALLOC?                   
         BE    SAED0440            YES -                                        
         LA    R2,BUD$TOTH         THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'BADTOT),BADTOT                                          
*                                  LOAD MSG: TOTAL NOT = ALLOCATION             
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                                                             
SAED0440 EQU   *                                                                
         SPACE                                                                  
         XC    RBUDTAG,RBUDTAG     STARTS AS 0 ON ADD                           
         CLI   FOUNDTYP,C'Y'       TYPE FOUND?                                  
         BE    FLFILE              YES - CHECK FOR ADD OR CHANGE                
         ZIC   R2,RBUDCNTR         NO  - BUMP CONTRACT TYPE COUNTER             
         LA    R2,1(R2)            BUMP BY 1                                    
         STC   R2,RBUDCNTR         REINSERT COUNTER INTO RECORD                 
         BAS   RE,ADDEL            ADD RECORD FROM WORK2                        
         EJECT                                                                  
***->>                                                                          
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
*        CHANGE - READ REC, THEN WRITE NEW                                      
*                                                                               
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
*                                                                               
FLCH10   EQU   *                                                                
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
         LA    R4,REC              A(OLD RECORD)                                
         LA    R5,REC2             A(NEW RECORD)                                
*                                                                               
* INCASE CONTRACT IS 2K, RECORDS WAS SAVED IN WORK2. NEED TO MOVE BACK          
* TO REC BEFORE WRITING THE RECORD OUT                                          
*                                                                               
*                                                                               
*- PRESERVE BUDGET RECORD TAG ON CHANGES                                        
FLCH15   EQU   *                                                                
         CLI   BREC,X'15'                                                       
         BNE   FLCH20                                                           
*                                                                               
*   WARNING:  WHAT IS GOING ON HERE???                                          
*                                                                               
         MVC   (RBUDTAG-RBUDREC)(1,R5),(RBUDTAG-RBUDREC)(R4)                    
*                                                                               
FLCH20   EQU   *                                                                
         BAS   RE,XCREC                                                         
         BAS   RE,PUTREC                                                        
*                                                                               
FLCH25   DS    0H                                                               
         LA    R2,LFMLAST                                                       
         CLI   BREC,X'15'          S/P BUDGET RECORD?                           
         BE    SSFM0020            YES - REDISPLAY                              
         DC    H'0'                NO  - UNRECOGNIZED                           
         SPACE 1                                                                
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
         LA    R2,LFMLAST          REDISPLAY NEW RECORD                         
         B     SSFM0020                                                         
         EJECT                                                                  
* SUBROUTINE TO MOVE 1000 BYTES TO R4 FROM R5                                   
MOVEREC  MVC   000(250,R4),000(R5)                                              
         MVC   250(250,R4),250(R5)                                              
         MVC   500(250,R4),500(R5)                                              
         MVC   750(250,R4),750(R5)                                              
         BR    RE                                                               
*                                                                               
MOVERECL NTR1                                                                   
         L     R4,0(R1)            'FROM' ADDRESS                               
         LH    R5,27(R4)           'FROM' LENGTH                                
         L     RE,4(R1)            'TO' ADDRESS                                 
         LR    RF,R5               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R4)                                                        
         L     R4,4(R1)                                                         
         LH    R5,27(R4)                                                        
         AR    R4,R5                                                            
         MVI   0(R4),0                                                          
         XIT1                                                                   
*                                                                               
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
TYPEDIT  EDIT  (R5),(9,8(R2)),ZERO=NOBLANK                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUBROUTINE TO FORMAT CONTRACT BUCKET FIX DISPLAY                              
         SPACE 2                                                                
FIXEDIT  EDIT  (R5),(10,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                
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
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
NEXTUF   EQU   *                                                                
         SR    R0,R0                                                            
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
         EJECT                                                                  
* RELFMGEN                                                                      
* REGENBUD                                                                      
* REGENSBUD                                                                     
* REGENREP                                                                      
* REGENEOM                                                                      
* REGENDEM                                                                      
* REGENCON                                                                      
* RELFMTWA                                                                      
* RELFME7D                                                                      
* RELFME6D                                                                      
* RELFMEAD                  DEMO MENU SCREEN                                    
* DDCOMFACS                                                                     
* DEDBLOCK                                                                      
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENBUD                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSBUD                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCTY                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREP                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENEOM                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENDEM                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME7D                                                       
*                                                                               
*   BUDGET SCREEN WORK AREA                                                     
*                                                                               
         ORG   BUDWORK+128                                                      
BCONTYP  DS    CL1                                                              
BCONDESC DS    CL20                                                             
FOUNDTYP DS    CL1                                                              
NOERRMSG DS    CL1                                                              
BUDACT   DS    CL3                                                              
BUDSCRN  DS    CL1                                                              
SAVER4   DS    F                                                                
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF3D                                                       
*                                                                               
*   BUDGET ALLOCATION SCREEN WORK AREA                                          
*                                                                               
         ORG   BDGWORK+128                                                      
SAVER6   DS    F                   SAVE AREA: R6 FOR ALLOC SCREEN               
SAVER4A  DS    F                   SAVE AREA: R4 FOR ALLOC SCREEN               
ACTYPE   DS    F                   A(NEXT CONTRACT TYPE)                        
CTYPES   DS    CL8                 SAVE AREA FOR CONTRACT TYPES                 
OFFALLOC DS    CL1                 OFFICE ALLOCATION FLAG                       
BUDTOTS  DS    14F                 BUDGET ALLOC SCRN TOTALS COLUMN              
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME1D          FIX SCREEN                                   
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME6D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMEAD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038RELFM1A   05/01/02'                                      
         END                                                                    
