*          DATA SET RERES0C    AT LEVEL 002 AS OF 12/10/12                      
*          DATA SET RERES0C    AT LEVEL 134 AS OF 07/21/09                      
*PHASE T8190CC                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE KHDUMMY                                                                
*        TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - HISTORY'              
*                                                                               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*------------------------------------------------------------------*            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
*  APR/09  (SMY) --- >SUPPORT NEW INVENTORY KEY                    *            
*                                                                  *            
* SEP18/08 (SKU) --- >FIX NO FILTER CONDITION                      *            
*                                                                  *            
* MAY15/08 (SKU) --- >2-CHAR BOOKTYPE SUPPORT                      *            
*                                                                  *            
* JUN21/07 (BU ) --- >LONGER BOOKS LIST                            *            
*                                                                  *            
* SEP28/04 (BU ) --- >MULTI-STATION: SKIP MKT TESTING.             *            
*                                                                  *            
* MAR20/01 (FJD) --- >ADDED FOOTNOTE TO DENOTE BOOKS CONTAINING    *            
*                     OVERRIDDEN DEMOS.  ALSO ADDED NOTATION FOR   *            
*                     BOOKS WITH MULTI HUT DAYS/TIMES              *            
*                                                                  *            
* JAN02/01 (FJD) --- >CHANGE UNIVERSE PRECISION                    *            
*                      AND ADD NEW ERROR MESSAGE ROUTINE (NEWERRS) *            
*                                                                  *            
* JUN28/00 (BU ) --- >REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG*            
*                                                                  *            
* NOV  /98 (BOB) --- >FIX FORCED TEXT OPTION                       *            
*                                                                  *            
* JAN  /98 (BOB) --- >ALLOW *ABCD TO INDICATE MENU                 *            
*                                                                  *            
* SEP09/97 (GL)  --- >SUPPRESS UPGRADE FOOTNOTE IF TRACK HAS MULTI-*            
*                      SHARE BOOKS                                 *            
*                                                                  *            
* FEB18/97 (BOB) --- >ADD %DIFF CALCULATIONS                       *            
*                                                                  *            
* MAY03/94 (BU ) --- >FIX STACKED FORMAT SHARE CALCULATION.        *            
*                                                                  *            
* AUG28/92 (BU ) --- >FIX STATION MARKET NAME DISPLAY PROBLEM      *            
*                                                                  *            
* JUL01/92 (BU ) --- >RANGE TEST:  IF TABLE FULL, DON'T ABORT.     *            
*                     WILL DISPLAY 56 ITEMS, REST ARE SKIPPED.     *            
*                                                                  *            
* JAN06/92 (MRR) --- >'FIX' CONTINUED MESSAGE                      *            
*                                                                  *            
* APR01/91 (MRR) --- >MULTSW PRINTING OF A 'N' LEFT 1 NUMBER SHOWNG*            
*                    >HOME SHARE WAS NOT SUPPRESSED                *            
*                                                                  *            
* SEP25/90 (MRR) --- >INSTALL OPTION FOR 1 DECIMAL PRINTING        *            
*                    >CHANGE HEADLINE TO STANDARD                  *            
*                    >COL WIDTH 5>7 AND #DEMS 19>17, MENU 24>17    *            
*                                                                  *            
* AUG02/90 (MRR) --- 1) USE ALTERNATE DAYPART NAME TABLE IFF       *            
*                       REP = 'NB' (NBC)                           *            
*                    2) CHANGE LABELS FOR INV X'92' KEY AS PER     *            
*                       REGENINV DSECT CHANGES                     *            
*                                                                  *            
* FEB26/90 (MRR) --- 1) IF REQUEST CAN'T RUN NOW, SOON IS AS GOOD  *            
*                       AS OVERNIGHT                               *            
*                    2) SKIP SINGLE DAYPART/SSU FOR DDS TERMINALS  *            
*                    3) MOVE A NUMBER OF ROUTINES TO NMOD          *            
*                                                                  *            
* AUG09/89 (SNS) ALLOW OPTION TO TRANSFER INFORMATION              *            
*                                                                  *            
* APR08/89 (SNS) ALLOW OPTION TO SUPRESS MULTIPLE DAYPARTS         *            
*                                                                  *            
********************************************************************            
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - INIT'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER - INITIALIZATION       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
T8190C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STCKL*STCKMAXQ,**190C**,RA,RR=R2                                 
         LR    R0,RC               SAVE WORKING STORAGE POINTER                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         ST    R0,ASTACK           SAVE A(STACK AREA)                           
*                                                                               
         LH    R2,=Y(STLIST-SYSD)                                               
         LA    R2,SYSD(R2)                                                      
         ST    R2,ASTLIST                                                       
*                                                                               
         LH    R2,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R2,SYSD(R2)                                                      
         ST    R2,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - CHKMODE'              
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        CHECK CALLING MODE                                        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
CHKMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        HANDLE PF KEYS                               
         BE    PPFK                                                             
         CLI   MODE,VALKEY         VALIDATE KEY  1                              
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                 UNKNOWN MODE                                 
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VKEY'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE KEY                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                     ANALYZE PFKEYS                            
         MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
         LA    RF,SAVELN                                                        
         LA    RE,SYSSPARE         INIT SAVEAREA                                
         XCEF                                                                   
         MVC   STAMP,=CL8'T8190C'  STAMP SAVEAREA                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PPFK'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        ANALYZE PFKEY ENTRY                                       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PPFK     DS    0H                                                               
*                                     ANALYZE PFKEYS                            
         CLI   PFAID,0             IF PFKEY ENTERED                             
         BE    PPFKX                                                            
*                                                                               
         GOTO1 =A(PFKEYS),RR=RELO     GO ANALYZE                                
         BE    PPFKX                  NO ERRORS                                 
*                                                                               
         GOTO1 ERREX                  CHECK FOR ERRORS                          
*                                                                               
PPFKX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VREC'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE RECORD                                           *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VREC     MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
*                                                                               
         CLI   THISLSEL,REPSELQ    SKIP IF REPORT REQUESTED                     
         BE    *+8                                                              
         OI    GENSTAT2,RETEQSEL   RE-DISPLAY AFTER CHANGE                      
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PAREP'                
********************************************************************            
*                                                                  *            
*     GET PARENT REP FROM REP RECORD FOR X'62' AND X'E2' LOOKUP.   *            
*         (USE GIVEN REP (AGENCY) FOR DEMO MENU LOOKUP)            *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PAREP    DS    0H                                                               
*                                                                               
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
         MVC   MYFILE,DMFILE       SAVE CURRENT FILE                            
*                                                                               
         XC    KEY,KEY             GET PARENT REP FROM REP RECORD               
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
*                                                                               
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING RREPELEM,R6                                                      
*                                                                               
         MVC   REPPAR,RREPPAR      SAVE PARENT REP CODE                         
*                                                                               
         MVC   KEY,MYKEY           RESTORE CURRENT FILE POINTER                 
         MVC   DMFILE,MYFILE       RESTORE CURRENT FILE                         
         GOTO1 HIGH                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *           SOURCE               *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASSRCEH         VALIDATE SOURCE                              
*                                                                               
         CLI   5(R2),0             IF THERE IS NO ENTRY                         
         BNE   *+16                                                             
         MVI   8(R2),C'N'             DEFAULT TO NIELSEN                        
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'            FORCE TRANSMISSION                        
*                                                                               
         GOTO1 VVALSRC                                                          
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VBK'                  
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE DEMO BOOKS                                       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VBK      DS    0H                                                               
*                                                                               
         XC    SVDTOBK,SVDTOBK     INIT DIFFERENCE CALC SAVEAREAS               
         XC    SVDTOBKA,SVDTOBKA                                                
         XC    SVDFRBK,SVDFRBK                                                  
         XC    SVDFRDEM,SVDFRDEM                                                
         XC    BOOKS,BOOKS                                                      
*                                                                               
         LA    R2,MASBKSH          POINT TO BOOK(S) INPUT                       
*                                                                               
         GOTO1 ANY                 FORCES INPUT REQUIRED CONDITION              
*                                                                               
*        SCAN INPUT TO ALLOW EXAMINATION OF INDIVIDUAL BOOKS                    
*              '-' TREATED LIKE '='                                             
*                  IN ORDER TO HIGHLIGHT A RANGE OF BOOKS                       
*                                                                               
         MVC   DMCB+8(4),=C',=,-'   SCAN FOR ',' AND '-'                        
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(22,SBUFF)                                     
*                                                                               
*        LOOP THROUGH SCANNER BLOCKS TO VALIDATE EACH BOOK INDIVIDUALLY         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4         GET NUMBER OF SCANNER BLOCKS                 
         BZ    VBKNOTVE               AT LEAST ONE BOOK REQUIRED                
*                                                                               
         STC   R0,SAVSCNNM         SAVE NUMBER OF SCAN BLOCKS                   
*                                                                               
         MVI   NUMBOOK,0           INIT NUMBER OF ENTERED BOOKS                 
*                                                                               
         L     R4,SBUFF            POINT TO SCANNER RESULTS                     
         LA    R5,BOOKS            POINT TO BOOKS SAVEAREA                      
*                                                                               
VBKLOOP  DS    0H                                                               
*                                                                               
         MVC   FLDH,0(R2)          COPY FIELD HEADER                            
         MVC   FLD,SPACES          INIT FIELD WORKAREA                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          GET LENGTH OF BOOK                           
         BZ    VBKNOTVE            MUST HAVE LENGTH                             
*                                                                               
         LA    R1,12(R4)           POINT TO ENTERED BOOK                        
*                                                                               
         CLI   0(R1),C'%'          IF BOOK STARTS WITH %                        
         BNE   *+16                                                             
         LA    R1,1(R1)               BUMP POINTER                              
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VBKNOTVE               MUST HAVE LENGTH GREATER THAN 1           
*                                                                               
         STC   RF,FLDH+5           SET FIELD LENGTH                             
*                                                                               
         BCT   RF,*+8              DECREMENT FOR EXECUTE                        
         B     VBKNOTVE            MUST HAVE LENGTH GREATER THAN 1              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R1)        MOVE INPUT TO WORKAREA                       
*                                                                               
*        VALIDATE ENTERED BOOK                                                  
*                                                                               
         MVI   BKTYPE,0            INIT BOOKTYPE                                
         GOTO1 BOOKVAL,PARAS,(SVSOURCE,FLDH),(1,WORK),(C'B',SCANNER),  X        
               BKTYPE,(C'C',ACOMFACS)                                           
         CLI   4(R1),0                                                          
         BE    VBKNOTVE            INPUT IS INVALID                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,NUMBOOK          INCREMENT NUMBER OF REQUESTED BOOKS          
         LA    RF,1(RF)                                                         
         STC   RF,NUMBOOK                                                       
*                                                                               
         MVC   0(3,R5),WORK        SAVE VALIDATED BOOK                          
         MVC   3(1,R5),BKTYPE      SAVE BOOK TYPE                               
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
         MVC   GSIBITS,0(R5)       BOOKVAL BITS                                 
         MVC   GSIBKTYP,3(R5)      BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),(0,GSRCOUT),ACOMFACS                 
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   VBKSRCE                                                          
*                                                                               
         CLC   GSORSVC,SVSOURCE    ALL BOOKS MUST HAVE SAME SOURCE              
         BNE   VBKSRCE                                                          
*                                                                               
         CLI   12(R4),C'%'         SKIP IF NOT %DIFF                            
         BNE   VBKLPPCX                                                         
*                                                                               
         OC    SVDTOBK,SVDTOBK     MAX ONE DIFFERENCE CAN BE REPORTED           
         BNZ   VBK1BKE                                                          
*                                                                               
         MVC   SVDTOBK,0(R5)       SAVE TO BOOK FOR DIFF CALCULATION            
         ST    R5,SVDTOBKA         SAVE ADDRESS IN LIST                         
*                                                                               
*        ADD YEAR AGO BOOK TO LIST                                              
*                                                                               
         MVC   SVDFRBK,0(R5)       COPY CURRENT BOOK                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SVDFRBK+1        DECREMENT YEAR                               
         BCTR  RF,0                                                             
         STC   RF,SVDFRBK+1                                                     
*                                                                               
*        FROM BOOK MUST BE IN A LIST                                            
*        OR BOTH FROM AND TO BOOK MUST BE IN RANGE                              
*                                                                               
         TM    PRINTOPT,X'10'      SKIP IF NOT DOING A RANGE                    
         BNO   VBKDFRGN                                                         
*                                                                               
         XC    0(4,R5),0(R5)       CLEAR BOOK FROM LIST                         
         SH    R5,=H'4'            BACK UP ENTRY IN LIST                        
         XC    SVDTOBKA,SVDTOBKA   NOT NEEDED WITH RANGE                        
*                                                                               
         CLC   SVDFRBK+1(2),BOOKS+1 FROM BOOK ON OR AFTER  RANGE START          
         BL    VBKNBK2E                                                         
*                                                                               
         CLC   SVDFRBK+1(2),BOOKS+5 FROM BOOK ON OR BEFORE RANGE END            
         BH    VBKNBK2E                                                         
*                                                                               
         CLC   SVDTOBK+1(2),BOOKS+1 TO   BOOK ON OR AFTER  RANGE START          
         BL    VBKNBK2E                                                         
*                                                                               
         CLC   SVDTOBK+1(2),BOOKS+5 TO   BOOK ON OR BEFORE RANGE END            
         BH    VBKNBK2E                                                         
*                                                                               
         CLC   BOOKS+3(1),SVDTOBK+3   BKTYPES MUST BE THE SAME                  
         BNE   VBKBTPE                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,NUMBOOK          DECREMENT NUMBER OF REQUESTED BOOKS          
         BCTR  RF,0                                                             
         STC   RF,NUMBOOK                                                       
*                                                                               
         B     VBKLPPCX                                                         
*                                                                               
VBKDFRGN DS    0H                                                               
*                                                                               
         LA    RF,BOOKS            FROM BOOK MUST BE IN LIST ALREADY            
*                                                                               
         CR    RF,R5                  CHECK FOR END OF LIST                     
         BNL   VBKNBKE                                                          
         CLC   SVDFRBK,0(RF)          CHECK IF BOOK ALREADY IN LIST             
         BE    *+12                                                             
         LA    RF,4(RF)               BUMP LIST POINTER                         
         B     *-20                                                             
*                                                                               
VBKLPPCX DS    0H                                                               
*                                                                               
         CLI   1(R4),0             IF ANY SECOND ENTRY EXISTS                   
         BE    VBKLRNGX               WE HAVE A RANGE                           
*                                                                               
         CLI   12(R4),C'%'         NEITHER BOOK IN RANGE CAN BE %DIFF           
         BE    *+8                                                              
         CLI   22(R4),C'%'                                                      
         BE    VBKNOTVE                                                         
*                                                                               
         MVC   3(1,R5),BKTYPE      SAVE BOOK TYPE                               
         OI    PRINTOPT,X'10'      ELSE INDICATE RANGE OF BOOKS                 
*                                                                               
         CLI   GSOQLF,C' '         NO QUALIFIER IS ALLOWED                      
         BH    VBKPFIXE                                                         
*                                                                               
*        VALIDATE SECOND BOOK OF RANGE                                          
*                                                                               
         MVC   FLDH,0(R2)          COPY FIELD HEADER                            
         MVC   FLD,SPACES          INIT FIELD WORKAREA                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R4)          GET LENGTH OF BOOK                           
         BZ    VBKNOTVE            MUST HAVE LENGTH                             
*                                                                               
         LA    R1,22(R4)           POINT TO ENTERED BOOK                        
*                                                                               
         STC   RF,FLDH+5           RESET FIELD LENGTH                           
*                                                                               
         BCT   RF,*+8              DECREMENT FOR EXECUTE                        
         B     VBKNOTVE            MUST HAVE LENGTH GREATER THAN 1              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R1)        MOVE INPUT TO WORKAREA                       
*                                                                               
*        VALIDATE ENTERED BOOK                                                  
*                                                                               
         MVI   BKTYPE,0            INIT BOOKTYPE                                
         GOTO1 BOOKVAL,PARAS,(SVSOURCE,FLDH),(1,WORK),(C'B',SCANNER),  X        
               BKTYPE,(C'C',ACOMFACS)                                           
         CLI   4(R1),0                                                          
         BE    VBKNOTVE            INPUT IS INVALID                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,NUMBOOK          INCREMENT NUMBER OF REQUESTED BOOKS          
         LA    RF,1(RF)                                                         
         STC   RF,NUMBOOK                                                       
*                                                                               
         MVC   4(3,R5),WORK        SAVE VALIDATED BOOK                          
         MVC   7(1,R5),BKTYPE      SAVE BOOK TYPE                               
*                                                                               
         CLC   1(2,R5),5(R5)       START CANNOT BE GREATER THAN END             
         BH    VBKRNGE                                                          
*                                                                               
         CLC   3(1,R5),7(R5)       BKTYPES MUST BE THE SAME                     
         BNE   VBKBTPE                                                          
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
         MVC   GSIBITS,4(R5)       BOOKVAL BITS                                 
         MVC   GSIBKTYP,7(R5)      BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT,ACOMFACS                     
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   VBKSRCE                                                          
*                                                                               
         CLI   GSOQLF,C' '         NO QUALIFIER IS ALLOWED                      
         BH    VBKPFIXE                                                         
*                                                                               
         CLC   GSORSVC,SVSOURCE    ALL BOOKS MUST HAVE SAME SOURCE              
         BNE   VBKSRCE                                                          
*                                                                               
         LA    R5,4(R5)            BUMP TO NEXT POSITION IN BOOK LIST           
*                                                                               
VBKLRNGX DS    0H                                                               
*                                                                               
VBKCONT  DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           POINT TO NEXT SCANNER BLOCK                  
         LA    R5,4(R5)            POINT TO NEXT ENTRY IN BOOK LIST             
*                                                                               
         BCT   R0,VBKLOOP                                                       
*                                                                               
VBKDONE  DS    0H                                                               
*                                                                               
         CLI   NUMBOOK,20          REALLY ONLY 20 BOOKS ALLOWED                 
         BH    VBKMAXE                                                          
*                                                                               
         TM    PRINTOPT,X'10'      SKIP IF NOT A RANGE                          
         BNO   VBKDNRGX                                                         
*                                                                               
         LA    RF,1                MAX NUMBER OF SCAN BLOCKS ALLOWED            
         OC    SVDTOBK,SVDTOBK     IF  DIFFERENCE ALSO REPORTED                 
         BZ    *+8                                                              
         LA    RF,2                ALLOW TWO SCAN BLOCKS                        
*                                                                               
         CLM   RF,1,SAVSCNNM       CHECK ON NUMBER OF SCAN BLOCKS               
         BL    VBKRNG2E               TOO MANY AND LIST  AND BKS MIXED          
*                                                                               
VBKDNRGX DS    0H                                                               
*                                                                               
         B     VBKX                                                             
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VBKBTPE  DS    0H                  BOOK TYPE MUST BE SAME OVER RANGE            
         MVC   CONHEAD(L'BADBK4),BADBK4                                         
         B     MYEND                                                            
*                                                                               
VBKSRCE  DS    0H                  SOURCES MUST BE CONSISTENT                   
         MVC   CONHEAD(L'BADBK2),BADBK2                                         
         B     MYEND                                                            
*                                                                               
VBKMAXE  DS    0H                  TOO MANY DEMO BOOKS                          
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
*                                                                               
VBKPFIXE DS    0H                  NO PREFIXES ALLOWED IN A BOOK RANGE          
         MVC   CONHEAD(L'BADBK1),BADBK1                                         
         B     MYEND                                                            
*                                                                               
VBKRNGE  DS    0H                  START OF RANGE AFTER END                     
         MVC   CONHEAD(L'BADBK3),BADBK3                                         
         B     MYEND                                                            
*                                                                               
VBKNBKE  DS    0H                  FROM DIFFERENCE BK MUST BE IN LIST           
         L     RF,=A(NODFRBK)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'NODFRBK),0(RF)                                         
         B     MYEND                                                            
*                                                                               
VBKNBK2E DS    0H                  BOTH FROM AND TO BOOKS MUST BE IN            
         L     RF,=A(NODFRBK2)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'NODFRBK2),0(RF)       RANGE                            
         B     MYEND                                                            
*                                                                               
VBK1BKE  DS    0H                  MAX 1 DIFFERENCE CALCULATION                 
         L     RF,=A(ONEDFRBK)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'ONEDFRBK),0(RF)                                        
         B     MYEND                                                            
*                                                                               
VBKRNG2E DS    0H                  RANGE MUST BE ONLY BOOK ENTRY                
         MVC   CONHEAD(L'BADBK6),BADBK6                                         
         B     MYEND                                                            
*                                                                               
VBKNOTVE DS    0H                                                               
         MVI   ERROR,232           INVALID BOOK EXPRESSION                      
         B     ERREND                                                           
*                                                                               
VBKX     DS    0H                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VSTA'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX,*XXXX  *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VSTA     DS    0H                                                               
         GOTO1 =A(VSTAT),RR=RELO                                                
*                                                                               
         TM    WHEN,X'40'          IF NOW PROCESSING                            
         BNO   VSTAWHNX                                                         
*                                                                               
         CLI   NOREQSTA,1          OKAY IF ONE STATION                          
         BNH   VSTAWHNX                                                         
*                                                                               
         L     RF,=A(ONESTA)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'ONESTA),0(RF)   NOW REPORTING EXCEEDED.                
         B     MYEND                                                            
*                                                                               
*!!      MVC   CONHEAD(L'ONESTA),ONESTA                                         
*!!      B     MYEND                                                            
*                                                                               
VSTAWHNX DS    0H                                                               
*                                                                               
VSTAX    DS    0H                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VDEM'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE DEMOS (& DEMO MENU)                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VDEM     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *     DEMOS (& DEMO MENU)        *           
*                                  *                                *           
*                                  **********************************           
DEMO00   LA    R2,MASDEMH          VALIDATE DEMOS                               
         MVI   NFLDS,2             SET FOR 2 DEMO FIELDS                        
         MVI   ERROR,2             INVALID INPUT FIELD                          
         CLC   8(3,R2),=C'ALL'     DEFAULT MENU                                 
         BE    DEM10                                                            
         CLI   8(R2),C'*'          MENU ID                                      
         BNE   DEMO02                                                           
         CLI   5(R2),3             MENU IS 2 A/N CHARACTERS                     
         BE    DEM10                                                            
         B     DEMO03                                                           
DEMO02   DS    0H                                                               
         CLC   8(2,R2),=C'M='      OR SPECIFIC MENU                             
         BNE   DEM50                                                            
         CLI   5(R2),4             MENU IS 2 A/N CHARACTERS                     
         BE    DEM10                                                            
DEMO03   DS    0H                                                               
         L     RF,=A(BADMENU)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADMENU),0(RF)                                         
         B     MYEND                                                            
*                                                                               
DEM10    XC    KEY,KEY             VALIDATE DEMO MENU                           
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),REPPAR    USE PARENT REP                               
         MVC   KEY+25(2),=C'ZZ'    DEFAULT MENU                                 
*                                                                               
         CLI   8(R2),C'*'          MENU                                         
         BNE   *+14                                                             
         MVC   KEY+25(2),9(R2)     MENU ID                                      
         B     DEM11                                                            
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+25(2),10(R2)    OR CHOSEN MENU                               
*                                                                               
DEM11    DS    0H                                                               
*                                                                               
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         USING RDEMREC,R6                                                       
         MVC   NUMDEMS,RDEMNUM     SAVE NUMBER OF DEMOS                         
         DROP  R6                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   DEM19               THEN NEW 24 IS MAX NUMBER OF DEMOS           
         CLI   NUMDEMS,24                                                       
         BNH   *+8                                                              
         MVI   NUMDEMS,24                                                       
         B     DEM20                                                            
DEM19    EQU   *                                                                
         CLI   NUMDEMS,20          MAX 20 NORMAL DEMOS                          
         BNH   DEM20                                                            
         MVI   NUMDEMS,20                                                       
DEM20    EQU   *                                                                
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RDEMDEL,R6                                                       
         MVC   DEMLST,RDEMDEM      SAVE DEMOS                                   
                                                                                
         BRAS  RE,CHKDMA           ANY DMA IMPS REQUESTED?                      
         BNE   DEM80               NO, CONTINUE                                 
         LHI   R1,DMAERROR         YES, LOAD ERROR EQUATE                       
         BRAS  RE,NEWERRS          REPORT ERROR                                 
         DC    H'0'                SHOULD NOT RETURN                            
*                                                                               
         DROP  R6                                                               
DEM50    MVI   MAX,22              DUMMY SO I CAN DO ERROR MSG                  
*                                                                               
*        ALWAYS ALLOW UP TO 20 DEMOS-DISPLAY MAY LOOK ODD                       
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   *+8                 THEN NEW 24 IS MAX NUMBER OF DEMOS           
         MVI   MAX,26              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALDEM                                                          
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   DEM52               THEN NEW 24 IS MAX NUMBER OF DEMOS           
         CLI   ACTUAL,24           REALLY ALLOW 20 DEMOS HERE                   
         BNH   DEM60                                                            
         B     DEM55                                                            
DEM52    DS    0H                                                               
         CLI   ACTUAL,20           REALLY ALLOW 17 DEMOS HERE                   
         BNH   DEM60                                                            
DEM55    DS    0H                                                               
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
DEM60    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         MVC   DEMLST(60),DEMOS                                                 
         MVI   DEMLST+60,X'FF'     FORCE END OF LIST IF EXACTLY 20 DEM          
*                                                                               
*        THE FOLLOWING IS A HORRIBLE KLUGE TO AVOID THE FORMATTING              
*        PROBLEM ENCOUNTERED WHEN LARGE UNIVERSE #S ARE REPORTED                
*        WITH ROUNDING OPTION AND/OR MORE THAN 17 DEMOS REQUESTED               
                                                                                
DEM80    BRAS  RE,CHKUNIV          WERE UNIVERSES REQUESTED?                    
         BNE   DEMVALX             NO, SKIP TESTS                               
         CLI   NUMDEMS,17          YES, IF MORE THAN 17 DEMOS                   
         BNH   DEM90                                                            
         LHI   R1,UNIVERR1              THEN, ERROR                             
         BRAS  RE,NEWERRS                                                       
         DC    H'0'                           SHOULD NOT RETURN                 
                                                                                
DEM90    CLI   MASOP5,C'Y'              ELSE, IF ROUNDING OPT='Y'               
         BNE   DEMVALX                                                          
         LHI   R1,UNIVERR2                    THEN, ERROR                       
         BRAS  RE,NEWERRS                                                       
         DC    H'0'                                 SHOULD NOT RETURN           
                                                                                
DEMVALX  DS    0H                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - DPTVAL'               
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPTVAL   LA    R2,MASDPTH          VALIDATE DAYPART                             
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   *+8                                                              
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         CLI   8(R2),C'*'          MENU IF IT STARTS '*'                        
         BNE   DPT06                                                            
*                                                                               
         MVC   DPMENU,9(R2)        SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT06    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         STC   RF,NOREQDPT         SAVE NUMBER OF REQUESTED DAYPARTS            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,REPPAR     SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         STC   RF,NOREQDPT         SET NUMBER OF DAYPARTS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,REPPAR     SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
*                                                                               
         ZIC   R0,NOREQDPT         # OF REQUESTED DAYPARTS                      
*                                                                               
         LH    R3,=Y(DPTBL-SYSD)                                                
         LA    R3,SYSD(R3)         ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBLD   INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DPTCONT             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVE SHORT NAME                              
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBLD  INIT NEXT ENTRY                           
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
         L     RF,=A(DPMENUER)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'DPMENUER),0(RF)                                        
         B     MYEND                                                            
*                                                                               
DPTINVE  DS    0H                  INVALID DAYPART                              
         L     RF,=A(DPINVER)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'DPINVER),0(RF)                                         
         B     MYEND                                                            
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VDAY'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE DAY                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VDAY     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *               DAY              *           
*                                  *                                *           
*                                  **********************************           
DAY      LA    R2,MASDAYH          DAY (OPTIONAL)                               
         MVI   DAYOPT,X'FF'        DAYOPT                                       
         CLI   5(R2),0                                                          
         BE    FTR                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    FTR                                                              
         SPACE 1                                                                
         GOTO1 VVALDAY                                                          
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
         MVC   DAYOPT,ACTUAL                                                    
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *             FILTER             *           
*                                  *                                *           
*                                  **********************************           
FTR      LA    R2,MASFTRH          FILTER (OPTIONAL)                            
         CLI   5(R2),0                                                          
         BE    FTRX                                                             
         OI    PRINTOPT,X'08'      INDICATE FILTERS SELECTED                    
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6             CAN HAVE UP TO 6                             
         BNH   FTRX                                                             
         MVC   CONHEAD(L'MANYFLT),MANYFLT                                       
         B     MYEND                                                            
*                                                                               
FTRX     DS    0H                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VINV'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE INVENTORY NUMBER(S)                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *        INVENTORY NUMBER        *           
*                                  *                                *           
*                                  **********************************           
INVMAX   EQU   30                  MAX INVENTORY NUMBERS ALLOWED                
*                                                                               
VINV     LA    R2,MASINVH          INVENTORY NUMBER (OPTIONAL)                  
*                                                                               
         L     RF,=A(INVLIST-SYSD)                                              
         AR    RF,R9                                                            
         XC    0(L'INVLIST,RF),0(RF) INIT INV SAVEAREA                          
*                                                                               
         CLI   5(R2),0             IF THERE IS NO INPUT                         
         BNE   VINV10                                                           
*                                                                               
         CLC   STMENU,SPACES       INV REQUIRED IF USING STATION MENU           
         BNH   *+12                                                             
         LA    R0,INVMAX               FORCES CURSOR TO FIELD START             
         B     VINVREQE                                                         
*                                                                               
         CLI   MASDPTH+5,0            WE MUST HAVE A DAYPART ENTERED            
         BNE   VINVDONE                                                         
*                                                                               
         LA    R2,MASDPTH                                                       
         MVI   ERROR,1                MISSING INPUT                             
         B     ERREND                                                           
*                                                                               
VINV10   DS    0H                                                               
*                                                                               
         GOTO1 ANY                 READ IN INVENTORY NUMBERS                    
*                                                                               
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         MVC   DMCB+8(4),=C',=,-'  SCAN FOR SINGLE AND RANGES                   
*                                                                               
         LA    RF,INVMAX                                                        
         STC   RF,INVMAX2                                                       
         GOTO1 SCANNER,DMCB,(R2),(INVMAX2,SBUFF)                                
*                                                                               
         CLI   DMCB+4,0            MUST HAVE AT LEAST ONE INV NO.               
         BH    VINV20                                                           
*                                                                               
         MVI   ERROR,2             INVALID INPUT FIELD                          
         BL    ERREND                                                           
*                                                                               
VINV20   DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4         NUMBER OF SCANNED ENTRIES                    
*                                                                               
         CH    R0,=Y(INVMAX)       MAKE SURE IT IS NOT TOO MANY                 
         BH    VINVMAXE                                                         
*                                                                               
         L     R4,SBUFF            POINT TO SCANNER BLOCK                       
         L     R5,=A(INVLIST-SYSD) POINT TO INVENTORY SAVEAREA                  
         AR    R5,R9                                                            
*                                                                               
         CH    R0,=H'1'            IF ONLY ONE INVENTORY NUMBER                 
         BH    VINV30                                                           
*                                                                               
         CLI   0(R4),3             OF LENGTH 3                                  
         BNE   VINV30                                                           
*                                                                               
         CLC   =C'ALL',12(R4)      COULD BE 'ALL'                               
         BE    VINVDONE            AND IS ALLOWED                               
*                                                                               
VINV30   DS    0H                                                               
*                                                                               
VINVLOOP DS    0H                                                               
*                                                                               
         MVC   0(4,R5),SPACES      INIT SAVED INVENTORY NUMBER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          LENGTH OF FIRST INVENTORY NUMBER             
         BZ    VINVDONE            END OF INPUT                                 
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    VINVLNGE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R4)      FIRST INVENTORY NUMBER                       
*                                                                               
         MVC   4(4,R5),0(R5)       COPY INV NUMBER                              
*                                                                               
         ICM   RF,1,1(R4)          LENGTH OF SECOND INVENTORY NUMBER            
         BZ    VINVLP10            SINGLE INVENTORY NUMBER                      
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    VINVLNGE                                                         
*                                                                               
         MVC   4(4,R5),SPACES      INIT SECOND INVENTORY NUMBER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),22(R4)      SECOND INVENTORY NUMBER                      
*                                                                               
VINVLP10 DS    0H                                                               
*                                                                               
         CLI   NOREQSTA,1          SKIP IF MORE THAN ONE STATION                
         BH    VINVCONT                                                         
*                                                                               
*        READ FOR INVENTORY MASTER POINTER                                      
*                                                                               
         XC    KEY,KEY             ESTABLISH INVENTORY RECORD KEY               
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET INVENTORY RECORD TYPE                    
         MVC   RINVKREP,REPPAR     USE PARENT REP                               
         L     R1,ASTLIST          POINT TO STATION LIST                        
         MVC   RINVKSTA,STLSSTAC-STLISTD(R1)   ACTIVE STATION                   
         MVC   RINVKINV,0(R5)      USE STARTING INVENTORY NUMBER                
*                                                                               
         GOTO1 HIGH                READ FOR POINTER                             
*                                                                               
         CLC   RINVKEY(RINVKINV-RINVKEY),KEYSAVE   SAME STATION                 
         BNE   VINVNFE                                                          
*                                                                               
         CLC   RINVKINV,0(R5)      INV NO. MUST BE IN RANGE                     
         BL    VINVNFE                                                          
*                                                                               
         CLC   RINVKINV,4(R5)      INV NO. MUST BE IN RANGE                     
         BH    VINVNFE                                                          
*                                                                               
VINVCONT DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           NEXT SCANNED BLOCK                           
         LA    R5,8(R5)            NEXT SAVEAREA                                
         BCT   R0,VINVLOOP                                                      
*                                                                               
VINVDONE DS    0H                  ALL INV NOS. VALID                           
*                                                                               
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
*                                                                               
         B     VINVX                                                            
*                                                                               
*        INVENTORY VALIDATION ERROR MESSAGES                                    
*                                                                               
VINVNFE  DS    0H                  INVENTORY NUMBER NOT FOUND                   
*                                                                               
         MVC   CONHEAD(L'VINVNFM),VINVNFM                                       
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVNFM  DC    C'** ERROR ** INVENTORY NUMBER NOT ON FILE'                      
*                                                                               
VINVLNGE DS    0H                  INVENTORY NUMBER TOO LONG                    
*                                                                               
         MVC   CONHEAD(L'VINVLNGM),VINVLNGM                                     
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVLNGM DC    C'** ERROR ** INVENTORY NUMBER MUST BE AT MOST 4 LONG'           
*                                                                               
VINVMAXE DS    0H                  TOO MANY INVENTORY NUMBERS                   
*                                                                               
         MVC   CONHEAD(L'VINVMAXM),VINVMAXM                                     
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVMAXM DC    C'** ERROR ** TOO MANY INVENTORY NUMBERS'                        
*                                                                               
VINVREQE DS    0H                  STATIONS MENU REQUIRES INV ENTRY             
*                                                                               
         MVC   CONHEAD(L'VINVREQM),VINVREQM                                     
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVREQM DC    C'** ERROR ** INPUT REQUIRED WITH STATION MENU'                  
*                                                                               
VINVERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,INVMAX           MAXIMUM ALLOWED INV NOS.                     
         SR    RF,R0                                                            
         LA    RF,1(RF)            NUMBER OF INV NO. IN ERROR                   
         STC   RF,FADDR            PASS ITEM IN ERROR NUMBER                    
*                                                                               
         MVI   ERROR,SUPPLIED      ERROR MESSAGE SUPPLIED                       
*                                                                               
         GOTO1 VMYCURS                                                          
*                                                                               
VINVX    DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VOPT'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE OPTIONS                                          *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VOPT     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *     PRINT SHARES & H/P/T       *           
*                                  *                                *           
*                                  **********************************           
OP1      LA    R2,MASOP1H          PRINT SHARES & HPT OPTION (OPTIONAL)         
         MVI   ERROR,2             INVALID INPUT                                
         CLI   5(R2),0             DEFAULT NO                                   
         BE    OP2                                                              
         CLI   8(R2),C'N'                                                       
         BE    OP2                                                              
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         OI    PRINTOPT,POPTSHRQ                                                
*                                                                               
         CLI   DDS,C'Y'                                                         
         BE    OP2                                                              
*                                                                               
         TM    PRINTOPT,X'20'      FILTER ON DAYPART OR DAY OR INV#             
         BO    OP2                 --------------------------------             
*                                                                               
         TM    WHEN,X'10'          OVERNITE                                     
         BO    OP2                 --------                                     
         TM    WHEN,X'20'          SOON                                         
         BO    OP2                 ----                                         
*                                                                               
         L     RF,=A(BADSHARE)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADSHARE),0(RF)                                        
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *     SUPPRESS TEXT OPTION       *           
*                                  *                                *           
*                                  **********************************           
OP2      LA    R2,MASOP2H          TEXT (OPTIONAL)                              
*                                                                               
         CLI   5(R2),1             MAX ONE OPTION                               
         BNH   OP210                                                            
*                                                                               
         L     RF,=A(OP2ERR)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'OP2ERR),0(RF)    ONLY ONE OPTION ALLOWED               
*                                                                               
         B     MYEND                                                            
*                                                                               
OP210    DS    0H                                                               
         MVI   ERROR,2             INVALID INPUT                                
*                                                                               
         CLI   5(R2),0             DEFAULT NO                                   
         BE    *+8                                                              
         CLI   8(R2),C'N'                                                       
         BE    *+8                                                              
         CLI   8(R2),C'M'          SUPPRESS MARKET         TEXT                 
         BE    *+8                                                              
         CLI   8(R2),C'S'          SUPPRESS STATION/MARKET TEXT                 
         BE    OP2X                                                             
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
*                                                                               
         OI    PRINTOPT,X'80'      SUPPRESS TEXT                                
*                                                                               
OP2X     DS    0H                                                               
*                                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *     SUPPRESS PRINTING FOR      *           
*                                  *     DUPLICATE DAYPART          *           
*                                  **********************************           
OPT3     MVI   OPTFLAG,0          DEFAULT                                       
         LA    R2,MASOP3H         SUPRRESS PRINTING FOR                         
         CLI   5(R2),0            MORE THAN ONE DAYPART                         
         BE    OPT4                                                             
         CLI   8(R2),C'N'                                                       
         BE    OPT4                                                             
         MVI   OPTFLAG,1                                                        
         CLI   8(R2),C'Y'                                                       
         BE    OPT4                                                             
         MVI   OPTFLAG,2                                                        
         CLI   8(R2),C'P'          PRIMARY DEMO ONLY                            
         BE    OPT4                                                             
         BNE   ERREND                                                           
         SPACE                                                                  
*                                  **********************************           
*                                  *     TRANSFER INFORMATION       *           
*                                  **********************************           
OPT4     LA    R2,MASOP4H                                                       
         CLI   5(R2),0                                                          
         BE    OPT5                                                             
         CLI   8(R2),C'N'                                                       
         BE    OPT5                                                             
         CLI   8(R2),C'S'                                                       
         BE    OPT5                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         SPACE                                                                  
*                                  **********************************           
*                                  *     ROUNDED DEMOS?             *           
*                                  **********************************           
OPT5     LA    R2,MASOP5H                                                       
         CLI   5(R2),0                                                          
         BE    OPT5X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT5X                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
OPT5X    DS    0H                                                               
         SPACE                                                                  
OPT6     LA    R2,MASOP6H                                                       
         CLI   5(R2),0                                                          
         BE    OPT6X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT6X                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
OPT6X    DS    0H                                                               
*                                  **********************************           
*                                  *     PRINT LOOK-UP DAY/TIMES    *           
*                                  **********************************           
OPT7     LA    R2,MASOP7H                                                       
         CLI   5(R2),0                                                          
         BE    OPT7X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT7X                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
OPT7X    DS    0H                                                               
*                                  **********************************           
*                                  *     PRINT INTERNAL TEXT        *           
*                                  **********************************           
OPT8     LA    R2,MASOP8H                                                       
         CLI   5(R2),0                                                          
         BE    OPT8X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT8X                                                            
         CLI   8(R2),C'Y'                                                       
         BE    OPT8X                                                            
         CLI   8(R2),C'O'                                                       
         BNE   ERREND                                                           
OPT8X    DS    0H                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *     SHOW OVERRIDE FLAGS        *           
*                                  **********************************           
OPT9     LA    R2,MASOP9H                                                       
         CLI   5(R2),0                                                          
         BE    OPT9X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT9X                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
OPT9X    DS    0H                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     EFFECTIVE START DATE       *           
*                                  *                                *           
*                                  **********************************           
ESDTV    LA    R2,MASESDTH          EFFECTIVE START DATE (OPTIONAL)             
*                                                                               
         CLI   5(R2),0                                                          
         BE    ESDTX                                                            
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    ESDTRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    ESDTRFPN            NONE                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MASESDT(0),RFPVSYMB MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),MASESDT MATCH INPUT TO SYMB PARM          
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-32                                                          
         B     ESDTRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(RE#RFPSD),RFPVSYME+1 MUST BE SDATE SYMBOLIC                 
         BE    *+14                                                             
         MVC   CONHEAD(L'INVSYMB),INVSYMB    INVALID SYMBOLIC                   
         B     MYEND                                                            
*                                                                               
         XC    MASESDT,MASESDT     INIT START FIELD                             
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'MASESDT        MAX LENGTH FOR START DATE                    
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     ESDTX               VALID PERIOD                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
ESDTRFPN DS    0H                                                               
*                                                                               
         GOTO1 VVALDATE            VALIDATE DATE ENTRY                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,ESDT)   ESDT=2 BYTE COMPRESSED           
*                                                                               
ESDTX    DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *       EFFECTIVE END DATE       *           
*                                  *                                *           
*                                  **********************************           
EEDTV    DS    0H                   EFFECTIVE END DATE (OPTIONAL)               
*                                                                               
         LA    R2,MASEEDTH                                                      
*                                                                               
         MVC   EEDT,=X'FFFF'                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    EEDTX                                                            
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    EEDTRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    EEDTRFPN            NONE                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MASEEDT(0),RFPVSYMB MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),MASEEDT MATCH INPUT TO SYMB PARM          
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-32                                                          
         B     EEDTRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(RE#RFPEN),RFPVSYME+1 MUST BE EDATE SYMBOLIC                 
         BE    *+14                                                             
         MVC   CONHEAD(L'INVSYMB),INVSYMB    INVALID SYMBOLIC                   
         B     MYEND                                                            
*                                                                               
*                                                                               
         XC    MASEEDT,MASEEDT     INIT START FIELD                             
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'MASEEDT        MAX LENGTH FOR START DATE                    
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     EEDTX               VALID PERIOD                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
EEDTRFPN DS    0H                                                               
*                                                                               
         GOTO1 VVALDATE                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,EEDT)   EEDT=2 BYTE COMPRESSED           
*                                                                               
         CLC   ESDT,EEDT           START CANNOT BE GREATER THAN END             
         BH    ERREND                                                           
*                                                                               
EEDTX    DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *      ACTIVITY START DATE       *           
*                                  *                                *           
*                                  **********************************           
ADT10    LA    R2,MASADTH          ACTIVITY START DATE (OPTIONAL)               
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,ADT)   ADT=YMD BINARY                    
*                                                                               
VRECX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PREP'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        PRINT REPORT                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=V(UPOUT)                                                     
         A     R1,RELO                                                          
         ST    R1,AUPOUT                                                        
*                                                                               
         CLC   STAMP,=CL8'T8190C'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   MAXPLINS,45         45 PRINT LINES FIT ON A PAGE                 
         SPACE 1                                                                
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         ZIC   RE,MAXPLINS                                                      
         MH    RE,=H'132'                                                       
         LA    R1,0(RE,R1)                                                      
         ST    R1,XBUFF            SAVE ADDRESS OF END OF 1 FULL PAGE           
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PRINIT'               
********************************************************************            
*                                                                  *            
*     REPORT INITIALIZATION                                        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PRINIT   DS    0H                                                               
*                                                                               
*       ALWAYS USE ALTERNATE FORMAT WITH DAY-TIMES/PROGRAMS ABOVE DEMOS         
*                                                                               
         GOTO1 =A(DHED),DMCB,(RC),RR=RELO   CALC DEMO HEADS ONCE                
*                                                                               
         L     RE,SBUFF            START  OF BUFFER                             
         L     RF,=AL4(L'BUFF)     LENGTH OF BUFFER                             
*                                                                               
         XCEF  (RE),(RF)           CLEAR BUFFER AREA                            
*                                                                               
         MVI   RCSUBPRG,0                                                       
*                                                                               
         LHI   R3,STCKL*STCKMAXQ   ON-LINE STACK SIZE                           
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE GET STORAGE FOR STACK            
         BNE   PRINIT10                                                         
*                                                                               
         MHI   R3,10               STACK SIZE - 10 TIMES BIGGER                 
         LA    R4,ASTACK           A(START OF STACK)                            
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)    GET STACK STORAGE AREA                    
*                                                                               
         LTR   RF,RF               TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,STACKLN          SAVE STACK LENGTH                            
*                                                                               
PRINIT10 DS    0H                                                               
*                                                                               
         ICM   RF,15,ASTACK        CALCULATE END OF STACK                       
         AR    RF,R3               START PLUS STACK SIZE                        
         ST    RF,ASTACKX          END OF STACK                                 
*                                                                               
         LH    R2,=Y(TXTWRK-SYSD)                                               
         LA    R2,SYSD(R2)                                                      
         ST    R2,ATXTWRK                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,XTODAY)                                
*                                                                               
*              DEAL WITH MARKET AND STATION FACTS                               
*              ----------------------------------                               
*                                                                               
         TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
         BO    PMKTSTAX                                                         
*                                                                               
         CLI   MASOP2,C'S'         SKIP IF SUPPRESSING STA/MKT TEXT             
         BE    PMKTSTAX                                                         
*                                                                               
         CLI   MASOP2,C'M'         SKIP IF SUPPRESSING MARKET TEXT              
         BE    PMKTSTA1                                                         
*                                                                               
         MVI   OPTION,C'M'                                                      
         BAS   RE,MAS300                                                        
*                                                                               
PMKTSTA1 DS    0H                                                               
*                                                                               
         MVI   OPTION,C'S'                                                      
         BAS   RE,MAS300                                                        
*                                                                               
PMKTSTAX DS    0H                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - STKDPT'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        LOOP THROUGH DAYPARTS AND PRINT REPORT FOR EACH           *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
STKDPT   DS    0H                                                               
*                                                                               
         LA    R2,DPLIST           POINT TO DAYPART LIST                        
         SR    R3,R3                                                            
         IC    R3,NOREQDPT         NUMBER OF DAYPARTS                           
*                                                                               
STKDPTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             CHECK FOR END OF DAYPARTS                    
         BE    STKDPTDN                                                         
*                                                                               
         MVC   SVDPT,0(R2)         ACTIVATE NEXT DAYPART                        
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   PAGE,=H'1'          START RENUMBERING PAGES                      
*                                                                               
         BAS   RE,STKBLD           BUILD STACK OF D/A FOR DAYPART               
*                                                                               
STKDPTCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            POINT TO NEXT DAYPART IN LIST                
         BCT   R3,STKDPTLP                                                      
*                                                                               
STKDPTDN DS    0H                                                               
*                                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE RELEASE STACK STORAGE            
         BNE   PREPX10                                                          
*                                                                               
         L     R3,STACKLN           STACK SIZE                                  
         LA    R4,ASTACK           A(START OF STACK)                            
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)    FREE STACK STORAGE AREA                  
*                                                                               
         LTR   RF,RF               TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREPX10  DS    0H                                                               
*                                                                               
PREPX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - STKDPT'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        BUILD STACK OF DISK ADDRESSES USING DAYPART PASSIVE PTR   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
STKBLD   NTR1                      BUILD A STACK OF D/A                         
*                                                                               
         L     R5,ASTACK           POINT TO START OF D/A STACK                  
         USING STCKD,R5            ESTABLISH ENTRY IN STACK                     
*                                                                               
         SR    R6,R6               INIT STACK ITEM COUNTER                      
*                                                                               
         L     R3,ASTLIST          POINT TO LIST OF STATIONS                    
*                                                                               
STKSTALP DS    0H                                                               
*                                                                               
         USING STLISTD,R3          ESTABLISH ENTRY IN STATION LIST              
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    STKSTADN                                                         
*                                                                               
         LA    R4,KEY              ESTABLISH DAYPART PASSIVE PTR                
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ   SET PASSIVE PTR ID                           
         MVC   RIDPKREP,REPPAR     USE PARENT REP                               
         MVC   RIDPKSTA,STLSSTAC   SET ACTIVE STATION                           
         CLI   RIDPKSTA+4,C' '     DEFAULT TO TV IF NO BAND                     
         BH    *+8                                                              
         MVI   RIDPKSTA+4,C'T'                                                  
         MVC   RIDPKDPT,0(R2)      SET CURRENT DAYPART                          
*                                                                               
         GOTO1 HIGH                READ FIRST POINTER                           
*                                                                               
MASSTKLP DS    0H                                                               
*                                                                               
         CLC   RIDPKEY(RIDPKDAY-RIDPKEY),KEYSAVE DONE AT STA CHANGE             
         BNE   MASSTKDN                                                         
*                                                                               
         CLI   RIDPKINV,0          ACCEPT ONLY NEW FORMAT KEY                   
         BE    MASSTKCN                                                         
*                                                                               
*        FILTER ON INVENTORY NUMBER IF NEEDED                                   
*                                                                               
         L     RF,=A(INVLIST-SYSD) POINT TO INVENTORY NO. FILTERS               
         AR    RF,R9                                                            
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  SKIP FILTER IF NONE                     
         BNH   MASINVOK                                                         
*                                                                               
MASINVLP DS    0H                                                               
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  CHECK FOR EOL                           
         BNH   MASINVDN                                                         
*                                                                               
         CLC   RIDPKINV,0(RF)      INV NO MUST LIE IN RANGE                     
         BL    MASINVCN                                                         
         CLC   RIDPKINV,L'RIDPKINV(RF)                                          
         BH    MASINVCN                                                         
*                                                                               
         B     MASINVOK                                                         
*                                                                               
MASINVCN DS    0H                                                               
*                                                                               
         LA    RF,2*L'RIDPKINV(RF)  BUMP TO NEXT INV NO IN FILTER               
         B     MASINVLP                                                         
*                                                                               
MASINVOK DS    0H                                                               
*                                                                               
*        INVENTORY NUMBER PASSED FILTER                                         
*                                                                               
*        BUILD ENTRY IN STACK                                                   
*                                                                               
         C     R5,ASTACKX          DON'T LET STACK OVERFLOW                     
         BNH   MASINV05                                                         
*                                                                               
         TM    WHEN,X'40'          IF NOT NOW PROCESSING                        
         BO    *+6                                                              
         DC    H'0'                   STACK TOO SMALL                           
*                                  ELSE ERROR MESSAGE                           
         L     RF,=A(NOWERR)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'NOWERR),0(RF)   NOW REPORTING EXCEEDED.                
         B     MYEND                                                            
*                                                                               
MASINV05 DS    0H                                                               
*                                                                               
         XC    STCKD(STCKL),STCKD  INIT ENTRY IN LIST                           
*                                                                               
         TM    RMPPROF+RMPVTYPB,RMPVTYPA SKIP IF NOT PRGM GEN'D INV #           
         BO    MASINV10                                                         
*                                                                               
         MVC   STCKDAY,RIDPKDAY    DAY CODE                                     
         MVC   STCKDTE,RIDPKDTE    EFFECTIVE DATE                               
         MVC   STCKQTR,RIDPKQTR    QUARTER HOUR CODE                            
         MVC   STCKLEN,RIDPKLEN    PROGRAM LENGTH                               
*                                                                               
MASINV10 DS    0H                                                               
*                                                                               
         MVC   STCKINV,RIDPKINV    INVENTORY NUMBER                             
         MVC   STCKSTD,RIDPKSTD    INVENTORY NUMBER START DATE                  
         MVC   STCKSTCD,STLSSTCD   SORT CODE                                    
         MVC   STCKSTAC,STLSSTAC   STATION CALL LETTERS                         
         MVC   STCKDA,KEY+28       DISK ADDRESS                                 
*                                                                               
         LA    R5,STCKL(R5)        NEXT ENTRY IN LIST                           
         LA    R6,1(R6)            BUMP ITEM COUNTER                            
*                                                                               
MASINVDN DS    0H                                                               
*                                                                               
MASSTKCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT PASSIVE                            
*                                                                               
         B     MASSTKLP                                                         
*                                                                               
MASSTKDN DS    0H                                                               
*                                                                               
STKSTACN DS    0H                                                               
*                                                                               
         LA    R3,STLISTL(R3)      BUMP TO NEXT STATION IN LIST                 
         B     STKSTALP                                                         
*                                                                               
STKSTADN DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTR   R6,R6               DONE IF STACK OF DISK ADDR IS EMPTY          
         BZ    XIT                                                              
*                                                                               
         GOTO1 XSORT,DMCB,(0,ASTACK),(R6),STCKL,STCKL,0  SORT STACK             
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - MAS70'                
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        LOOP THROUGH DISK ADDRESS STACK AND PRINT DATA FOR EACH   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         L     R5,ASTACK                                                        
         B     MAS80                                                            
*                                                                               
MAS70    LM    R5,R6,SAVESTAK                                                   
         LA    R5,STCKL(R5)                                                     
         BCT   R6,MAS80                                                         
*                                                                               
         XIT1                                                                   
*                                                                               
MAS80    DS    0H                                                               
*                                                                               
         XC    SVDFRDEM,SVDFRDEM   RESET DIFFERENCE DEMOS SAVEAREA              
*                                                                               
         MVC   KEY+28(4),STCKDA    PUT NEXT DISK ADDR IN KEY                    
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         MVC   SVSTCKD,STCKD       SAVE STACK ENTRY                             
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         MVC   KEY(27),IO                                                       
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    MAS70                                                            
*                                                                               
         USING RINVKEY,R4                                                       
*                                                                               
         OC    INVNO,INVNO         INVENTORY FILTER                             
         BZ    MAS90                                                            
*                                                                               
         CLC   RINVKINV,INVNO                                                   
         BNE   MAS70                                                            
*                                                                               
*        FILTER ON DAY                                                          
*                                                                               
MAS90    CLI   DAYOPT,X'FF'        SKIP IF NO DAY FILTER                        
         BE    MAS100                                                           
*                                                                               
         MVI   ELCODE,X'02'        FIND DAY/TIME ELEMENTS                       
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   MAS70               NO ELEMENT FOUND                             
*                                                                               
MASDTMLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6                                                      
*                                                                               
         CLC   DAYOPT,RIDTDAY      MATCH ON DAY                                 
         BE    MASDTMFD                                                         
*                                                                               
MASDTMCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    MASDTMLP                                                         
         B     MAS70               NO MATCH                                     
*                                                                               
MASDTMFD DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
MAS100   MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         CLI   OPTFLAG,0          OPTION-SUPRESS PRNT IN MULT DPTS              
         BE    MAS102                                                           
         ZIC   R1,NOREQDPT        # OF REQ. DAYPARTS                            
         CH    R1,=H'1'           NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    MAS102                                                           
*                                                                               
         BAS   RE,TSTDPT                                                        
*                                                                               
         CLI   SKIPREC,0                                                        
         BNE   MAS70               DON'T PRINT IT IN THIS DAYPART               
*                                                                               
MAS102   TM    PRINTOPT,X'08'      ANY FILTER FILTERS                           
         BZ    MAS125                                                           
*                                                                               
         ZIC   RE,NUMFILT                                                       
         LTR   RE,RE                                                            
         BZ    MAS70                                                            
         LA    R5,MASFTR                                                        
*                                                                               
MAS105   LA    R1,RINVPFLT                                                      
         LA    R0,6                                                             
*                                                                               
MAS110   CLI   0(R5),C'A'          IF ANY FILTER SPECIFIED                      
         BL    MAS123                                                           
*                                                                               
MAS115   CLC   0(1,R5),0(R1)       MATCHES ANY FILTER ON RECORD                 
         BE    MAS125              THEN WE WANT IT                              
*                                                                               
MAS120   LA    R1,1(R1)                                                         
         BCT   R0,MAS115                                                        
*                                                                               
MAS123   LA    R5,1(R5)                                                         
         BCT   RE,MAS105                                                        
         B     MAS70               NOTHING MATCHES-DON'T WANT IT                
         DROP  R6                                                               
*                                                                               
MAS125   OC    ADT,ADT             WAS ACTIVITY DATE SPECIFIED                  
         BZ    MAS130                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   MAS70                                                            
         USING RINVAEL,R6                                                       
         CLC   RINVAFST,ADT        SELECT ADDITIONS SINCE DATE                  
         BNL   *+14                                                             
         CLC   RINVALST,ADT        AND SELECT CHANGES SINCE DATE                
         BL    MAS70                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
MAS130   LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         CLC   ESDT(4),=X'0000FFFF' FILTER ON EFFECTIVE DATE(S)                 
         BE    MAS136                                                           
         CLC   RINVPEFF+0(2),EEDT  IGNORE ANY                                   
         BH    MAS70               STARTING AFTER SELECTED END DATE             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),ESDT  IGNORE ANY                                   
         BL    MAS70               ENDING BEFORE SELECTED START DATE            
         B     MAS140                                                           
*                                                                               
MAS136   OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),XTODAY SEE IF IT ENDS BEFORE TODAY                 
         BL    MAS70                                                            
*                                                                               
MAS140   DS    0H                                                               
*                                                                               
         GOTO1 =A(HDRNEW),DMCB,(RC),RR=RELO     PRINT DETAILS                   
*                                                                               
         TM    PRINTOPT,X'10'                RANGE OF BOOKS                     
         BO    MAS150                                                           
*                                                                               
         BAS   RE,GTBOOK           GO AND LOOK FOR BOOKS                        
*                                                                               
         B     *+8                                                              
MAS150   BAS   RE,RANGE            GO LOOK FOR RANGE OF BOOKS                   
*                                                                               
*****    TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
*****    BO    MAS160                                                           
*                                                                               
         MVI   OPTION,C'I'         INDICATE INVENTORY TEXT                      
         BAS   RE,MAS300                                                        
*                                                                               
MAS160   EQU   *                   IF ALTERNATE FORMAT (19+ DEM0S),             
*                                                                               
         CLI   PRNTDISP+1,0                                                     
         BNE   *+8                                                              
         MVI   CONTSW,C'N'                                                      
*                                                                               
         B     MAS70                                                            
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - TSTDPT'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        CHECKS IF PROGRAM HAS BEEN PRINTED ALREADY                *            
*        PROGRAMS PRINT N REQUEST ORDER                            *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
TSTDPT   NTR1                                                                   
         MVI   SKIPREC,0          0=USE RECORD                                  
*                                                                               
         CLI   OPTFLAG,2           IF ONLY TO APPEAR IN PRIMARY DPT             
         BNE   TSTD15                                                           
*                                                                               
         CLC   0(1,R2),RINVDP         THEN CURRENT DPT MUST = PRIMARY           
         BE    *+8                                                              
         MVI   SKIPREC,1              ELSE SET TO SKIP RECORD                   
*                                                                               
         B     XIT                                                              
*                                                                               
TSTD15   DS    0H                                                               
*                                                                               
         LA    R1,DPLIST          REQUEST LIST                                  
TSTD20   CLC   0(1,R1),0(R2)      CURRENT DAYPART?                              
         BE    XIT                USE THIS RECORD                               
*                                                                               
         LA    R3,6               # OF DPTS IN RECORD                           
         LA    RE,RINVDP          DAYPARTS CODED FOR PROGRAM                    
TSTD50   CLI   0(RE),C' '         ANY MORE DPTS?                                
         BE    TSTD80             NO                                            
*                                                                               
         CLC   0(1,R1),0(RE)                                                    
         BNE   TSTD70                                                           
         MVI   SKIPREC,1          SKIP RECORD - ALREADY PRINTED                 
         B     XIT                                                              
*                                                                               
TSTD70   LA    RE,1(RE)           CHECK NEXT DPT IN PROGRAM                     
         BCT   R3,TSTD50                                                        
*                                                                               
TSTD80   LA    R1,1(R1)           CHECK NEXT DPT IN REQUEST                     
         B     TSTD20                                                           
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - RANGE'                
***********************************************************************         
*                                                                     *         
*              CONTROL READING OF BOOKS - RANGE                       *         
*              ---------------------------------                      *         
*                                                                     *         
* 1. PRINT HEADER                                                     *         
* 2. CREATE LIST (RNGLST) OF BOOKVAL BITS(1),BOOKS(2),BKTYP(1),KSRC(1)*         
*         AND DISK ADDRESS(4)                                         *         
*         OF ALL INVENTORY THAT FITS RANGE (MAX 56 BOOKS)             *         
* 3. SORT LIST IN BOOK, SOURCE ORDER (FILE IS IN BOOK,BKTYP,KSRC ORDER*         
* 4. THEN GET BOOKS IN NON-RANGE CODE USING RNGLIST DISK ADDRESSES    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RANGE    NTR1  LABEL=*                                                          
*                                                                               
         MVI   HDRSW,C'Y'          PUT HDR LINE TO BUFF 1ST-TIME-THRU           
*                                                                               
         LA    R4,KEY              ESTABLISH INVENTORY RECORD KEY               
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE HEADER PART OF KEY                      
*                                                                               
         XC    RNGLST,RNGLST                                                    
         XC    RNGLST2,RNGLST2                                                  
*                                                                               
         LH    RF,=Y(RNGLSTND-SYSD)                                             
         LA    RF,SYSD(RF)                                                      
         MVI   0(RF),X'FF'         SET DELIMITER FOR EOT                        
*                                                                               
         LA    R3,RNGLST                                                        
         SR    R2,R2                                                            
*                                                                               
         GOTO1 HIGH                POINT TO INVENTORY HEADER                    
*                                                                               
         GOTO1 SEQ                 READ IN FIRST TRACK                          
*                                                                               
RANGLOOP DS    0H                                                               
*                                                                               
         CLC   RINVKEY(RINVKSPR-RINVKEY),KEYSAVE                                
         BNE   RANGDONE            DONE ON CHANGE IN INV HEADER                 
*                                                                               
         CLI   RINVKRSR,C'Z'       SKIP AVAIL RECORDS                           
         BE    RANGCONT                                                         
*                                                                               
         CLI   RINVKRSR,X'FF'      DONE IF RATIONALE RECORDS REACHED            
         BE    RANGDONE                                                         
*                                                                               
*        TEST RATING SERVICE, BOOKTYPE                                          
*                                                                               
         CLC   SVSOURCE,RINVKRSR   MUST BE SAME RATING SERVICE                  
         BNE   RANGCONT                                                         
         CLC   BOOKS+3(1),RINVKBTP  MUST BE SAME BOOKTYPE                       
         BNE   RANGCONT                                                         
                                                                                
********************************************                                    
*                                          *                                    
*        CHECK WITHIN DATE RANGE?          *                                    
*                                          *                                    
********************************************                                    
                                                                                
RANGE20  DS    0H                                                               
*****    CLI   RINVKRSR,C'R'          IF AN EST BOOK,                           
         TM    RINVKQLF,X'20'      IF AN EST BOOK                               
         BZ    RANGE40                                                          
         CLC   BOOKS+1(1),RINVKBK     THEN JUST CHECK YEAR                      
         BH    RANGCONT                                                         
         CLC   BOOKS+5(1),RINVKBK                                               
         BL    RANGCONT                                                         
         B     RANGE50                                                          
*                                                                               
RANGE40  DS    0H                                                               
         CLC   BOOKS+1(2),RINVKBK     ELSE,                                     
         BH    RANGCONT               CHECK BOTH YEAR AND MONTH                 
         CLC   BOOKS+5(2),RINVKBK                                               
         BL    RANGCONT                                                         
*                                                                               
*        SAVE BOOK, KSRC & DISK ADDRESS                                         
*                                                                               
RANGE50  DS    0H                                                               
         USING RLTBLD,R3                                                        
         MVC   RLTBBKBT,RINVKQLF                                                
         MVC   RLTBBOOK,RINVKBK                                                 
         MVC   RLTBBKTP,RINVKBTP                                                
         MVC   RLTBSRCE,RINVKRSR                                                
         MVC   RLTBDA,KEY+28       DISK ADDRESS                                 
         DROP  R3                                                               
*                                                                               
         LA    R3,RLTBLQ(R3)       BUMP LIST POINTER                            
         LA    R2,1(R2)            BUMP LIST ENTRY COUNTER                      
*                                                                               
         CHI   R2,RLTBMAXQ         TABLE FULL?                                  
         BE    RANGDONE            YES - DON'T ALLOW MORE INPUT                 
*                                                                               
RANGCONT DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 NEXT DEMO TRACK                              
         B     RANGLOOP                                                         
*                                                                               
RANGDONE DS    0H                                                               
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE HEADER PART OF KEY                   
*                                                                               
         STC   R2,BYTE             SAVE NUMBER OF DEMO TRACKS FOUND             
*                                                                               
         CH    R2,=H'1'                                                         
         BE    RNG50               SKIP XSORT FOR 1 BOOK                        
*                                                                               
         CHI   R2,RLTBMAXQ         TABLE FULL?                                  
         BNH   *+6                                                              
*                                                                               
*  NOTE:  THIS DUMP CAN NO LONGER HAPPEN....                                    
*                                                                               
         DC    H'0'                TOO MANY BOOKS IN RANGE                      
*                                                                               
         GOTO1 XSORT,DMCB,RNGLST,(R2),9,4,1                                     
*                                                                               
RNG50    LA    R2,RNGLST                                                        
         ZIC   RE,BYTE                                                          
         MHI   RE,RLTBLQ                                                        
         LA    R2,0(RE,R2)                                                      
         MVI   0(R2),X'FF'         MARK END OF TABLE                            
         LA    R2,RNGLST                                                        
         B     MAS200                                                           
         SPACE 1                                                                
RNGX     B     MAS240              NO BOOKS TO PRINT                            
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - GTBOOK'               
***********************************************************************         
*                                                                     *         
*              CONTROL READING OF BOOKS - NON RANGE                   *         
*              ------------------------------------                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTBOOK   NTR1  LABEL=*                                                          
*                                                                               
*                                                                               
         MVI   HDRSW,C'Y'          PUT HDR LINE TO BUFF 1ST-TIME-THRU           
*                                                                               
         LA    R2,BOOKS                                                         
         ZIC   R3,NUMBOOK                                                       
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
         USING RINVKEY,R4                                                       
*                                                                               
GTBLOOP  DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE HEADER KEY                           
*                                                                               
*                                                                               
GTBPCTX  DS    0H                                                               
*                                                                               
         MVC   RINVKRSR,SVSOURCE   SET RATING SERVICE                           
         MVC   RINVKQLF,0(R2)      SET BOOKVAL BITS                             
         MVC   RINVKBK,1(R2)       SET BOOK                                     
         MVC   RINVKBTP,3(R2)      SET BOOKTYPE                                 
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
*SMY*    CLC   RINVKEY(RINVKSRC-RINVKEY),KEYSAVE  DONE ON INV # CHANGE          
*SMY*    BNE   GTBCONT                                                          
*                                                                               
*SMY*    CLC   KEY+24(3),KEYSAVE+24     SAME SOURCE AND BOOK                    
*SMY*    BE    MAS200                                                           
*                                                                               
         CLC   KEY(L'RINVKEY),KEYSAVE   ENTIRE KEY MUST MATCH                   
         BE    MAS200                                                           
*                                                                               
GTBCONT  LA    R2,4(R2)            R2 POINTS TO NEXT BOOK                       
         BCT   R3,GTBLOOP                                                       
         B     MAS240              ALL THROUGH                                  
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - MAS200'               
***********************************************************************         
*                                                                     *         
*     NOW GETREC FOR BOTH RANGE/NON-RANGE BOOKS AND THEN PRINT        *         
*     --------------------------------------------------------        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MAS200   CLI   HDRSW,C'Y'                                                       
         BNE   MAS201                                                           
***           PUT HDR LINE TO BUFF 1ST-TIME-THRU                                
         BAS   RE,BUFFADD                                                       
         BAS   RE,BUFFFULL                                                      
         MVI   HDRSW,0                                                          
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BNE   MAS201                                                           
*                                                                               
MAS201   TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   MAS202                                                           
*                                                                               
         CLI   0(R2),X'FF'         END OF LIST                                  
         BE    MAS240                                                           
*                          MAKE IT LOOK LIKE WE READ FILE TO GET RECORD         
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKRSR-RINVKEY),KEYSAVE                                    
IKEYD    USING RINVKEY,KEY                                                      
         USING RLTBLD,R2                                                        
         MVC   IKEYD.RINVKRSR,RLTBSRCE  RATING SERVICE                          
         MVC   IKEYD.RINVKQLF,RLTBBKBT  QUALIFIER                               
         MVC   IKEYD.RINVKBTP,RLTBBKTP  BOOK TYPE                               
         MVC   IKEYD.RINVKBK,RLTBBOOK   BOOK                                    
         MVC   KEY+28(4),RLTBDA    BUT REALLY USE SAVED DISK ADDRESS            
         DROP  IKEYD,R2                                                         
*                                                                               
MAS202   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    MAS231                                                           
*                                                                               
*        FIND RATING SERVICE, BOOKTYPE                                          
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         LA    RF,IO               POINT TO FOUND RECORD                        
         MVC   GSIRSVC,RINVKRSR-RINVKEY(RF)    SET RATING SOURCE                
*                                                                               
*              ** BELOW ADDED FOR "NEW" GETKSRC **                              
         MVC   GSIQLF,RINVKQLF-RINVKEY(RF)     SET QUALIFIER                    
         MVC   GSIBKTYP,RINVKBTP-RINVKEY(RF)   SET BOOK TYPE                    
*                                                                               
         PRINT GEN                                                              
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',GSRCIN),(0,GSRCOUT),ACOMFACS                 
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                 SHOULD NOT HAPPEN                            
         DC    H'0'                                                             
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
MAS203   DS    0H                                                               
*                                                                               
         BAS   RE,DATA             FORMAT DEMOS                                 
*                                                                               
         GOTO1 =A(BOOKSQ),RR=RELO  FORMAT BOOK NAME                             
*                                                                               
         BAS   RE,BUFFADD          POINT TO NEXT LINE                           
         BAS   RE,BUFFFULL                                                      
*                                                                               
         XC    BLOCK1,BLOCK1       USE BLOCK1 TO SAVE SHR/LVL VALUES            
*                                                                               
         TM    PRINTOPT,POPTSHRQ   SKIP IF SHRS/LVLS ARE SUPPRESSED             
         BNO   MAS210                                                           
*                                                                               
         CLI   MULTSW,C'Y'         OR ON COMBOS                                 
         BE    MAS210                                                           
*                                                                               
         CLC   SAVECODE,=C'PA'     IF PROGRAM CODE IS PA, PRINT SHR/LVL         
         BE    MAS208                                                           
*                                                                               
MAS208   BAS   RE,SHRLVL           FORMAT (& PRINT) SHARES & LEVELS             
*                                                                               
MAS210   DS    0H                                                               
*                                                                               
         CLI   MASOP9,C'Y'                                                      
         BNE   MAS212                                                           
         BRAS  RE,OVFLAG           CHK IF DEMOS OVERRIDDEN, AND FLAG            
         BAS   RE,BUFFFULL                                                      
*                                                                               
MAS212   DS    0H                                                               
         CLI   GSOQLF,C'P'         IF P OR E BOOK                               
         BE    *+8                                                              
         CLI   GSOQLF,C'E'                                                      
         BNE   MAS228                                                           
*                                                                               
         TM    RMPPROF+RMPFTNTB,RMPFTNTA SKIP IF SUPPRESSING FOOTNOTES          
         BO    MAS230                                                           
*                                                                               
MAS228   DS    0H                                                               
*                                                                               
         MVI   OPTION,C'A'         PRINT ANY AUTO FOOTNOTES                     
*                                                                               
         GOTO1 =A(RAT),RR=RELO     PRINT FOOTNOTES                              
*                                                                               
MAS230   TM    PRINTOPT,X'02'      ANY UPGRADE                                  
         BZ    MAS230A                                                          
*                                                                               
         GOTO1 =A(UPPRNT),RR=RELO  YES, PRINT UPGRADE EXPRESSION                
*                                                                               
MAS230A  DS    0H                                                               
*                                                                               
         MVI   SPACING,1                                                        
         BAS   RE,BUFFADD         SPACE AFTER EVERY GROUP                       
         BAS   RE,BUFFFULL                                                      
*                                                                               
MAS231   DS    0H                                                               
*                                                                               
         CLC   SVDFRBK,0(R2)       IF DOING FROM DIFFERENCE BOOK                
         BNE   *+14                                                             
         MVC   SVDFRDEM,ACTDEMOS      SAVE DEMOS FROM BOOK                      
         B     MASDIFX                                                          
*                                                                               
         CLC   SVDTOBK,0(R2)       IF DOING TO   DIFFERENCE BOOK                
         BNE   MASDIFX                                                          
*                                                                               
         OC    SVDTOBKA,SVDTOBKA   IF ADDR OF BOOK IN LIST PRESENT              
         BZ    *+12                                                             
         C     R2,SVDTOBKA            MUST MATCH CURRENT BOOK IN LIST           
         BNE   MASDIFX                                                          
*                                                                               
         LA    R4,ACTDEMOS            POINT TO TO   DEMOS                       
         LA    R5,SVDFRDEM            POINT TO FROM DEMOS                       
*                                                                               
         GOTO1 =A(GTDIFF),RR=RELO     PRINT DIFFERENCE                          
*                                                                               
         MVI   SPACING,1                                                        
         BAS   RE,BUFFADD         SPACE AFTER EVERY GROUP                       
         BAS   RE,BUFFFULL                                                      
*                                                                               
         MVI   SPACING,1                                                        
         BAS   RE,BUFFADD         SPACE AFTER EVERY GROUP                       
         BAS   RE,BUFFFULL                                                      
*                                                                               
         XC    SVDFRDEM,SVDFRDEM      RESET FROM DEMOS SAVEAREA                 
*                                                                               
MASDIFX  DS    0H                                                               
*                                                                               
         TM    PRINTOPT,X'10'      SKIP IF DOING RANGE OF BOOKS                 
         BO    MAS235                                                           
*                                                                               
         B     GTBCONT             NEXT BOOK-NON RANGE                          
*                                                                               
MAS235   DS    0H                                                               
*                                                                               
         LA    R2,RLTBLQ(R2)                                                    
         B     MAS200              NEXT BOOK-RANGE                              
*                                                                               
MAS240   CLI   HDRSW,C'Y'          HAS ANY DATA BEEN PRINTED                    
         BNE   MAS244                                                           
*                                                                               
         BAS   RE,BUFFADD          NO, SO POINT PAST HEADER LINE                
         BAS   RE,BUFFFULL                                                      
*                                                                               
         CLI   PRNTDISP+1,0        AND IF THIS IS 19+ FORMAT                    
         BNE   MAS244                                                           
*                                                                               
         BAS   RE,BUFFADD          THEN LEAVE SPACING LINE                      
         BAS   RE,BUFFFULL                                                      
*                                                                               
MAS244   L     RE,SBUFF                                                         
         LA    RF,15               COULD BE 15 HEADLINES                        
*                                                                               
MAS247   OC    0(40,RE),0(RE)                                                   
         BZ    MAS248                                                           
         LA    RE,132(RE)                                                       
         BCT   RF,MAS247                                                        
*                                                                               
MAS248   L     RF,ABUFF                                                         
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RE,RF                                                            
         LA    RE,132(RE)          SPACING AFTER EVERY GROUP                    
         ST    RE,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
         SPACE 1                                                                
         BAS   RE,PRNTALL                                                       
         B     XIT                                                              
         EJECT                                                                  
*     PRINT ALL BOOKS THAT BELONG TO A PIECE OF INVENTORY TOGETHER              
*     ------------------------------------------------------------              
*                  (TEXT CAN BE ON A DIFFERENT PAGE)                            
         SPACE 2                                                                
PRNTALL  NTR1                                                                   
         SR    R2,R2                                                            
         L     RE,SBUFF                                                         
         L     R3,ABUFF                                                         
         SR    R3,RE                                                            
         LTR   R3,R3                                                            
         BP    PR5                                                              
         TM    PRNTOPT2,X'40'      WAS BUFFER MORE THAN 1 PAGE (P2)             
         BO    PRXIT               THEN MAYBE THERE'S NO MORE TO PRINT          
         DC    H'0'                ELSE, SHOULD AT LEAST BE A HEADLINE          
PR5      D     R2,=F'132'          R3=NUMBER OF LINES                           
         SPACE 1                                                                
         LR    R5,R3                                                            
         STC   R3,BYTE                                                          
         L     R2,SBUFF                                                         
         MVI   HDRSW,C'Y'                                                       
         TM    PRNTOPT2,X'40'      BUFFER MORE THAN A PAGE                      
         BZ    *+8                 THEN DON'T RESET HDRSW FOR P2                
         MVI   HDRSW,C'N'                                                       
         ZIC   R4,MAXPLINS                                                      
PR10     CR    R5,R4                                                            
         BH    PR20                                                             
         LR    R3,R5               RESET R3 (MAYBE 2ND TIME THROUGH)            
         MVI   BYTE,0                                                           
         STC   R5,ALLOWLIN                                                      
         B     PR50                                                             
         SPACE 1                                                                
PR20     MVC   ALLOWLIN,MAXPLINS                                                
         SR    R5,R4                                                            
         STC   R5,BYTE       # OF LINES TO BE PRINTED 2ND TIME THROUGH          
         LR    R3,R4                                                            
         SPACE 1                                                                
PR50     MVC   P,0(R2)                                                          
         OC    P,SPACES                                                         
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   CONTSW,C'Y'                                                      
         XC    0(132,R2),0(R2)                                                  
         LA    R2,132(R2)                                                       
         BCT   R3,PR50                                                          
         SPACE 1                                                                
         CLI   BYTE,0              MORE LINES TO PRINT                          
         BNE   PR10                                                             
         MVI   CONTSW,C'N'                                                      
PRXIT    TM    PRNTOPT2,X'80'                                                   
         BZ    *+8                                                              
         MVI   HDRSW,C'N'                                                       
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* ROUTINE CHECKS TO SEE THAT PRINT BUFFER HASN'T GOTTEN TOO BIG                 
         SPACE 2                                                                
BUFFFULL ST    RE,FULL                                                          
         L     RE,ABUFF                                                         
         C     RE,XBUFF                                                         
         BL    BFXIT                                                            
         OI    PRNTOPT2,X'80'      BUFFER BIGGER THAN 1 PAGE (P1)               
         BAS   RE,PRNTALL          TOO BIG, SO PRINT IT                         
         OI    PRNTOPT2,X'40'      BUFFER BIGGER THAN 1 PAGE (P2)               
         L     RE,SBUFF                                                         
         ST    RE,ABUFF            AND START AT BEGINNING AGAIN                 
BFXIT    L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*              CONTROL HANDLING OF RATIONALE                                    
*              -----------------------------                                    
         SPACE 2                                                                
MAS300   NTR1  LABEL=*                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         L     R5,ASTLIST          ESTABLISH STATION LIST                       
         USING STLISTD,R5                                                       
*                                                                               
MASSTALP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    MASSTADN                                                         
*                                                                               
         MVC   KEY,KEYSAVE         INIT KEY                                     
         XC    RINVKRTP,RINVKRTP   INIT RECORD TYPE                             
*                                                                               
         CLI   OPTION,C'I'         SKIP IF INVENTORY TEXT                       
         BE    MAS310                                                           
*                                                                               
         XC    KEY,KEY             RE-INIT KEY FOR MKT/STA TEXT                 
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPPAR     USE PARENT REP                               
         MVC   RINVKSTA(5),STLSSTAC  STATION                                    
*                                                                               
         CLI   RINVKSTA+4,C' '     DEFAULT TO TV                                
         BH    *+8                                                              
         MVI   RINVKSTA+4,C'T'                                                  
*                                                                               
         MVC   RINVKRSR,OPTION     SET SOURCE                                   
         B     *+8                                                              
MAS310   MVI   RINVKRSR,X'FF'      FOR OPTION I                                 
*                                                                               
         XC    RINVKTXT,RINVKTXT   INIT TEXT NUMBER                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
MASRATLP DS    0H                                                               
*                                                                               
*SMY*    CLC   KEY(25),KEYSAVE     DONE ON CHANGE OF SOURCE                     
*                                  ????????????????????????????? BELOW          
         CLC   KEY(RINVKBK-RINVKEY),KEYSAVE   DONE ON CHANGE OF SOURCE          
         BNE   MASRATDN                                                         
*                                                                               
*                                                                               
         GOTO1 =A(RAT),RR=RELO     PRINT RATIONALE                              
*                                                                               
MASRATCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     MASRATLP                                                         
*                                                                               
MASRATDN DS    0H                                                               
*                                                                               
         CLI   OPTION,C'I'         DONE IF INVENTORY TEXT                       
         BE    MASSTADN                                                         
*                                                                               
MASSTACN DS    0H                                                               
*                                                                               
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION IN LIST                 
         B     MASSTALP                                                         
*                                                                               
MASSTADN DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINES FOR DATA RECORDS                                        
*              -------------------------                                        
         SPACE 2                                                                
DATA     NTR1                                                                   
         MVI   LINSW,C'R'          LINE = "RATING"                              
         LA    R6,IO               CHECK FOR CODE                               
         MVI   MULTSW,C'N'                                                      
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   DAT30                                                            
         USING RINVCEL,R6                                                       
*                                                                               
         MVC   SAVECODE,RINVCODE   SAVE CODE FOR LATER PRINTING                 
*                                                                               
         TM    RINVCTYP,X'80'                                                   
         BNO   DAT30                                                            
         MVI   MULTSW,C'Y'                                                      
         SPACE 2                                                                
DAT30    NI    PRINTOPT,X'FD'      TURN OFF UPGRADE INDICATOR                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,X'02'      INDICATE UPGRADE EXISTS                      
         LA    R5,ACTDEMOS         POINT AT OUTPUT FOR DEMO VALUES              
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO                                      
         B     XIT                                                              
         EJECT                                                                  
*              SHARES AND LEVELS -  FORMATTING AND PRINTING                     
*              --------------------------------------------                     
         SPACE 2                                                                
SHRLVL   NTR1                                                                   
         MVI   LINSW,C'L'          *** LEVELS ***                               
         MVC   SVDEMLST,DEMLST     SAVE DEMLST                                  
         BAS   RE,EFFDEMS          DEVELOPE DEMO TYPES                          
         BNZ   *+14                                                             
         MVC   DEMLST,SVDEMLST     RESTORE DEMLST                               
         B     SHRLVLX             NO LEVELS TO BE PRINTED                      
*                                                                               
         BAS   RE,BUFFADD         LEVELS PRINT AFTER SHARES                     
         LA    R5,BLOCK1+100                                                    
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO    FORMAT THE DEMOS                  
         MVC   DEMLST,SVDEMLST                RESTORE DEMLST                    
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    RF,34(RE)                                                        
         MVC   0(5,RF),=C'H/P/T'                                                
         SH    RE,=H'132'          BACK UP A LINE TO PRINT SHARES               
         ST    RE,ABUFF                                                         
*                                                                               
         MVI   LINSW,C'S'          *** SHARES ***                               
         CLC   SAVECODE,=C'PA'                                                  
         BNE   SL45                                                             
         SPACE 1                                                                
SL40     BAS   RE,RECALC                                                        
         OI    PRINTOPT,X'01'                  INDICATE RECALC DEMOS            
         LA    R5,BLOCK1                       POINT TO SHARES                  
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT DEMOS                     
         NI    PRINTOPT,X'FE'                                                   
         B     SL50                                                             
         SPACE 1                                                                
SL45     MVC   SVDEMLST,DEMLST                 SAVE DEMLST                      
         BAS   RE,EFFDEMS                      DEVELOPE DEMO TYPES              
         BNZ   *+14                                                             
         MVC   DEMLST,SVDEMLST     RESTORE DEMLST                               
         B     SHRLVLX             NO LEVELS TO BE PRINTED                      
*                                                                               
         LA    R5,BLOCK1                                                        
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT THE DEMOS                 
         MVC   DEMLST,SVDEMLST                 RESTORE DEMLST                   
SL50     GOTO1 =A(PERCENT),RR=RELO             PERCENT SIGNS FOR SHARES         
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    RF,34(RE)                                                        
         MVC   0(5,RF),=C'SHARE'                                                
         LA    RE,132(RE)          SHARE LINE                                   
         LA    RE,132(RE)          LEVELS LINE                                  
         ST    RE,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
*                                                                               
SHRLVLX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*  RECALCULATE SHARES FOR PA                                                    
*                                                                               
*                    (RATING X 10000)                                           
*                    ---------------   = SHARE   (ROUND IN DAT50)               
*                         LEVEL                                                 
         SPACE 2                                                                
RECALC   NTR1                                                                   
         LA    R2,DEMLST                                                        
         LA    R3,BLOCK1                                                        
         LA    RE,BLOCK1+100                                                    
         LA    R5,ACTDEMOS                                                      
         SPACE 1                                                                
RC30     OC    0(4,RE),0(RE)       SKIP CALC IF LEVEL IS 0                      
         BZ    RC40                                                             
         L     R1,0(R5)            RATING                                       
         MH    R1,=H'10000'                                                     
         SR    R0,R0                                                            
         D     R0,0(RE)                                                         
         ST    R1,0(R3)                                                         
         SPACE 1                                                                
RC40     LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         LA    RE,4(RE)                                                         
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'         MORE DEMOS                                   
         BNE   RC30                                                             
         B     XIT                                                              
         EJECT                                                                  
*              WORK OUT EFFECTIVE DEMO TYPES FOR DIFFERENT LINES                
*              -------------------------------------------------                
         SPACE 2                                                                
EFFDEMS  NTR1                                                                   
*                                                                               
         SR    R2,R2               INIT HOMES SWITCH                            
*                                                                               
         CLI   LINSW,C'I'                                                       
         BE    EFFDEMSX                                                         
*                                                                               
         LA    R3,DEMLST           POINT TO LIST OF DEMO CODES                  
         ZIC   R0,NUMDEMS                                                       
*                                                                               
EFFDEMLP DS    0H                                                               
*                                                                               
         CLC   SAVECODE,=C'PJ'     PRINT PJ DEMOS                               
         BE    EFFDEM20                                                         
*                                                                               
         CLI   2(R3),1             SKIP IF DEMO IS HOMES                        
         BE    EFFDEM20                                                         
*                                                                               
* IS PROFILE SET TO PRINT SHARES H/P/T FOR ESTIMATE BOOKS                       
* WITH THE 5 UPGRADES                                                           
*                                                                               
         TM    RMPPROF+RESMASTB,RESMHPTA                                        
         BZ    EFFDEM30                                                         
*                                                                               
*        DON'T PRINT SHRS/LEVELS IN FOLLOWING CASES                             
*                                                                               
         TM    PRINTOPT,POPTUPGQ   SUPPRESS IF UPGRADES INVOLVED                
         BO    EFFDEM10                                                         
*                                                                               
         CLC   SAVECODE,=C'ES'     OR IF PROGRAM CODE IS ES                     
         BE    EFFDEM10                                                         
*                                                                               
         CLC   SAVECODE,=C'PE'     OR PE                                        
         BE    EFFDEM10                                                         
*                                                                               
         B     EFFDEM20            GO PRINT SHARES                              
*                                                                               
EFFDEM10 DS    0H                  CLEAR DEMO CODE FROM LIST                    
*                                                                               
         XC    0(3,R3),0(R3)       CLEAR ENTRY IN DEMO LIST                     
         B     EFFDEMCN                                                         
*                                                                               
*                                                                               
EFFDEM20 DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            INDICATE A DEMO FOR PRINTING FOUND           
         MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
         LA    R1,EFFTABLE                                                      
         B     EFFDEM1L                                                         
*                                                                               
*        DON'T PRINT SHRS/LEVELS IN FOLLOWING CASES                             
*                                                                               
EFFDEM30 DS    0H                                                               
*                                                                               
         CLC   SAVECODE,=C'ES'     DON'T SUPPRESS 'ES' AS OF                    
         BE    EFFDEM50                      5/3/00                             
*                                                                               
         TM    PRINTOPT,POPTUPGQ   DON'T SUPPRESS UPGRADES AS                   
         BO    EFFDEM50               OF 5/11/00                                
*                                                                               
         CLC   SAVECODE,=C'PE'     OR PE                                        
         BE    EFFDEM40                                                         
*                                                                               
         B     EFFDEM50            GO PRINT SHARES                              
*                                                                               
EFFDEM40 DS    0H                  CLEAR DEMO CODE FROM LIST                    
         MVI   ELCODE,X'5E'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   EFFDEM45                                                         
         USING RIBELEM,R6                                                       
*                                                                               
         CLC   RIBBOOK,=XL2'530C'  PRINT THIS   TESTS NO LONGER                 
         BE    EFFDEM50                         NEEDED AS OF                    
         CLC   RIBBOOK,=XL2'5406'  PRINT THIS   5/03/00 BECAUSE                 
         BE    EFFDEM50                         SHARES HUTS/PUTS                
         CLC   RIBBOOK,=XL2'5306'  PRINT THIS   PRINTING IS NO                  
         BE    EFFDEM50                         LONGER SUPPRESSED               
         CLC   RIBBOOK,=XL2'5A0C'  PRINT THIS   IN ANY CASE FOR                 
         BE    EFFDEM50                         SAVECODE 'ES'                   
         CLC   RIBBOOK,=XL2'5B06'  PRINT THIS                                   
         BE    EFFDEM50                                                         
*                                                                               
         MVI   ELCODE,X'01'        RESTORE R6 POINTER                           
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
EFFDEM45 DS    0H                                                               
         XC    0(3,R3),0(R3)       CLEAR ENTRY IN DEMO LIST                     
         B     EFFDEMCN                                                         
*                                                                               
EFFDEM50 DS    0H                                                               
         MVI   ELCODE,X'01'        RESTORE R6 POINTER                           
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         LA    R2,1(R2)            INDICATE A DEMO FOR PRINTING FOUND           
         MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
         LA    R1,EFFTABLE                                                      
*                                                                               
*        DETERMINE IF SHARE OR LEVEL TO PRINT                                   
*                                                                               
EFFDEM1L DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE AT END OF TABLE                         
         BE    EFFDEM1D                                                         
*                                                                               
         CLC   WORK(2),0(R1)       MATCH LINE AND DEMO TYPES TO TABLE           
         BE    EFFDEM1F                                                         
*                                                                               
EFFDEM1C DS    0H                                                               
*                                                                               
         LA    R1,3(R1)            BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         B     EFFDEM1L                                                         
*                                                                               
EFFDEM1F MVC   1(1,R3),2(R1)       SUBSTITUTE TYPE                              
*                                                                               
EFFDEM1D DS    0H                                                               
*                                                                               
EFFDEMCN DS    0H                                                               
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT DEMO                            
         BCT   R0,EFFDEMLP                                                      
*                                                                               
EFFDEMDN DS    0H                                                               
*                                                                               
EFFDEMSX DS    0H                                                               
         LTR   R2,R2               CC SET TO ZERO MEANS NO DEMOS                
*                                  TO BE PRINTED                                
         XIT1                                                                   
*                                                                               
EFFTABLE DS    0H                                                               
         DC    C'TRR'                                                           
         DC    C'TSX'                                                           
         DC    C'TLQ'                                                           
         DC    C'RSS'                                                           
         DC    C'RLP'                                                           
         DC    C'T2S'                                                           
         DC    C'T4P'                                                           
         DC    X'FFFF',C'-'                                                     
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
BUFFADD  L     R1,ABUFF                                                         
         LA    R1,132(R1)                                                       
         ST    R1,ABUFF                                                         
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL64'00'                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
*  MY OWN ERROR MESSAGES                                                        
*                                                                               
INVSYMB  DC    C'* ERROR * INVALID SYMBOLIC'                                    
MANYFLT  DC    C'* ERROR * TOO MANY FILTERS - LIMIT IS 6'                       
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 20'                        
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 17'                        
BADBK1   DC    C'* ERROR * BOOK CAN NOT HAVE PREFIX'                            
BADBK2   DC    C'* ERROR * BOOKS MUST BE THE SAME SOURCE'                       
BADBK3   DC    C'* ERROR * END BOOK LESS THAN START BOOK'                       
BADBK4   DC    C'* ERROR * BOOKS IN RANGE MUST HAVE SAME BOOKTYPE'              
BADBK6   DC    C'* ERROR * CAN NOT COMBINE A RANGE AND SINGLE BOOK(S)'          
NODFRBK  DC    C'* ERROR * PRIOR YEAR''S DEMO TRACK MUST BE IN LIST'            
NODFRBK2 DC    C'* ERROR * PRIOR AND CURRENT TRACKS MUST BE IN RANGE'           
ONEDFRBK DC    C'* ERROR * MAXIMUM ONE DIFFERENCE PER CENT ALLOWED'             
BADMENU  DC    C'* ERROR * MENU IS 2 A/N CHARACTERS'                            
DPMENUER DC    C'* ERROR * MENU NOT FOUND'                                      
DPINVER  DC    C'* ERROR * DAYPART NOT FOUND'                                   
BADSHARE DC   C'* ERROR * SHR-H/P/T MUST FILTER ON DYPT OR DAY OR INV#'         
OP2ERR DC C'ERROR - SELECT ONLY ONE OPTION - NOTE: ''S'' IMPLIES ''M'''         
ONESTA   DC    C'* ERROR * ONLY ONE STATION ALLOWED FOR NOW REPORTS'            
NOWERR   DC    C'* ERROR * NOW PROCESSING EXCEEDED. TRY SOON'                   
*                                                                               
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,055,C'INVENTORY MASTER LISTING'                               
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,055,24C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H3,056,C'DAYPART -'                                              
         PSPEC H3,099,C'SERVICE -'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
********************************************************************            
*                                                                  *            
*              STATION VALIDATION ROUTINE: RELOCATED               *            
*              -------------------------------------               *            
*                                                                  *            
********************************************************************            
VALRAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,IO                                                            
         USING RINVREC,R6                                                       
*                                                                               
         CLI   RINVKRSR,X'FF'                                                   
*                                  RATIONALE RECORD?                            
         BNE   INIT0080            NO  - SKIP OUT OKAY                          
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(10),=C'VALRAT IN:'                                             
         MVC   P+15(27),KEY                                                     
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   P(12),=C'VALRAT DATA:'                                           
         MVC   P+15(64),RINVKEY                                                 
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         LA    R1,RINVPEL          SET A(1ST ELEMENT)                           
*                                                                               
         CLI   0(R1),1             FIRST ELEMENT X'01'?                         
         BNE   INIT0100            NO  - DISPLAY AS ERROR                       
         ZIC   RF,1(R1)            YES                                          
         AR    R1,RF                                                            
         LA    R2,OKAYELTS                                                      
INIT0070 EQU   *                                                                
         CLI   0(R2),0             END OF LIST?                                 
         BE    INIT0100            YES - SKIP AS ERROR                          
         CLC   0(1,R1),0(R2)       ELEMENT IN LIST?                             
         BE    INIT0080            YES - GO BACK FOR NEXT RECORD                
         LA    R2,1(R2)            NO  - BUMP TO NEXT LIST ENTRY                
         B     INIT0070            GO BACK FOR NEXT                             
INIT0080 EQU   *                                                                
         SR    R0,R0               NO ERROR - SET CC ZERO                       
         B     INIT0120                                                         
INIT0100 EQU   *                                                                
         LTR   RB,RB               ERROR - SET CC NOT ZERO                      
INIT0120 EQU   *                                                                
         XIT1                                                                   
OKAYELTS DC    X'01020308EF0000'                                                
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
********************************************************************            
*                                                                  *            
*              VALIDATE RATIONALE RECORD                           *            
*              -------------------------                           *            
*                                                                  *            
********************************************************************            
VSTAT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =A(VALSTA),RR=RELO                                               
         L     R5,ASTLIST                                                       
         SR    R1,R1                                                            
VSTT0020 EQU   *                                                                
         OC    0(6,R5),0(R5)       ANY ENTRY IN SLOT?                           
         BZ    VSTT0040            NO  - FINISHED                               
         LA    R1,1(R1)            YES - BUMP COUNTER                           
         CH    R1,=H'1'            MORE THAN ONE STATION?                       
         BH    VSTT0120            YES - DON'T CHECK FOR MARKET NUMBER          
         LA    R5,6(R5)            BUMP TO NEXT STATION IN LIST                 
         B     VSTT0020            GO BACK FOR NEXT                             
VSTT0040 EQU   *                                                                
*                                                                               
*   TEST TO DETERMINE STATION'S MARKET NUMBER                                   
         LA    R2,MASSTNH          POINT TO STATION FIELD                       
         LA    R5,DBLOCKA2                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK       USE DEMAND TO RETRIEVE MARKET #              
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    RE,IO                                                            
         ST    RE,DBAREC                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   DBFUNCT,DBGETDEM    PHONY GETDEM CALL, BECAUSE                   
*                                     IT RETURNS MARKET NUMBER                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'       SET NSI AS SERVICE                           
         MVC   DBSELSTA,8(R2)      R2 -> STATION HEADER FIELD                   
         MVI   DBSELSTA+4,C'T'                                                  
         CLI   5(R2),4                                                          
         BE    VSTT0080            FOUR CHAR STATION                            
         BH    VSTT0060            MORE THAN FOUR CHARACTERS                    
         MVI   DBSELSTA+3,C' '     3 CHAR: SET 4TH CHAR TO SPACE                
         B     VSTT0080                                                         
*                                                                               
VSTT0060 LA    R1,12(R2)           S/B XXXX-X                                   
         CLI   5(R2),6                                                          
         BE    *+12                                                             
         MVI   DBSELSTA+3,C' '     OR XXX-X                                     
         LA    R1,11(R2)                                                        
         CLI   0(R1),C'-'                                                       
         BE    *+6                                                              
         DC    H'0'                ALREADY VALIDATED: CAN'T HAPPEN              
         MVC   DBSELSTA+4(1),1(R1)                                              
VSTT0080 EQU   *                                                                
         MVI   DBSELSRC,C'N'       SET SORT TO 'NSI'                            
         MVC   DBSELBK,=X'6805'    INSERT A BOOK DATE                           
         MVI   DBSELDAY,X'40'      SET DAY TO MONDAY                            
         MVC   DBSELAGY,AGENCY     INSERT AGENCY                                
         MVC   DBSELTIM,=X'06B306B3'                                            
*                                  INSERT TIME                                  
                                                                                
*                                                                               
VSTT0100 GOTO1 DEMAND,PARAS,DBLOCK,0                                            
*                                                                               
         LA    RE,DBACTRMK                                                      
         L     RF,=A(STAMKT#-SYSD)                                              
         AR    RF,R9                                                            
         MVC   0(2,RF),DBACTRMK    SAVE THE MARKET NUMBER                       
*                                                                               
VSTT0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - RAT'                  
********************************************************************            
*                                                                  *            
*              ROUTINES FOR RATIONALE RECORDS                      *            
*              ------------------------------                      *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
RAT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(07),=C'RAT IN:'                                                
         MVC   P+7(64),RINVREC                                                  
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         MVI   SVWRPOPT,0          INIT WRAP OPTION                             
         TM    RMPPROF+RMPWWRPB,RMPWWRPA   IF DEFAULT IS Y                      
         BNO   *+8                                                              
         MVI   SVWRPOPT,C'Y'                  RESET WRAP OPTION                 
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(VALRAT),RR=RELO  CHECK FOR VALID RATIONALE                    
         BNZ   RATX                ERROR - SKIP IT                              
*                                                                               
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(14),=C'RAT RETRIEVED:'                                         
         MVC   P+15(64),RINVREC                                                 
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    RATX                                                             
*                                                                               
         CLI   MASOP4,C'Y'        TRANSFER INFO?                                
         BNE   RATTRNSX           NO                                            
*                                                                               
         GOTO1 =A(HISTDATA),RR=RELO                                             
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(07),=C'HIST  :'                                                
         MVC   P+7(64),RINVREC                                                  
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         BAS   RE,BUFFFULL                                                      
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(07),=C'BUFF  :'                                                
         MVC   P+7(64),RINVREC                                                  
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
*                                                                               
RATTRNSX DS    0H                                                               
*                                                                               
*        CHECK SUPPRESS OPTION                                                  
*                                                                               
         CLI   OPTION,C'A'         SKIP IF AUTO TEXT                            
         BE    RSTSPRSX                                                         
*                                                                               
         TM    PRINTOPT,POPTSTXQ   IF SUPPRESSING TEXT                          
         BNO   RSTSPRSX                                                         
*                                                                               
         MVI   ELCODE,RINVTCCQ        FIND TEXT CONTROL ELEMENT                 
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   RATX                   NO ELEMENT AVAILABLE                      
*                                                                               
         USING RINVTCEL,R6            ESTABLISH TEXT CONTROL ELEMENT            
*                                                                               
         TM    RINVTCN1,RINVTCFQ      PRINT IF TEXT IS FORCED                   
         BO    RSTSPRSX                                                         
         B     RATX                   ELSE DROP                                 
*                                                                               
RSTSPRSX DS    0H                                                               
*                                                                               
RAT30    CLI   OPTION,C'A'   AUTO FOOTNOTES DON'T NEED FILTER CHECK             
         BNE   RAT40                                                            
*                                                                               
         L     R3,ABUFF            PUT AUTO FOOTNOTES TO BUFF WITH THE          
         LA    R5,28(R3)           START PRINTING AT +28 INTO LINE              
         B     RAT110              REST OF THE BOOK STUFF                       
*                                                                               
RAT40    L     R3,SBUFF       OTHER TEXT PRINTS IN A CHUNK OF ITS OWN           
         MVC   0(132,R3),SPACES                                                 
         SR    R2,R2               LINE COUNTER                                 
*                                                                               
         MVI   ELCODE,X'02'        IS A FILTER APPLICABLE                       
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BE    RATFLT              FILTER ELEMENT FOUND                         
*                                                                               
         CLI   MASOP8,C'O'         IF ONLY PRINTING INTERNAL TEXT               
         BE    RATX                   SKIP PRINTING                             
*                                                                               
         B     RAT110              ELSE PRINT THIS TEXT                         
*                                                                               
RATFLT   DS    0H                                                               
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
         CLI   RINVFWRP,0          IF WRAP OPTION PRESENT IN ELEMENT            
         BE    *+10                                                             
         MVC   SVWRPOPT,RINVFWRP      SAVE WRAP OPTION                          
*                                                                               
         CLC   REPPAR,AGENCY       IF NOT LOCAL REP                             
         BNE   *+12                                                             
         CLI   RINVFLOC,C'Y'       AND TEXT IS LOCAL ONLY                       
         BE    RATX                THEN DON'T WANT IT FOR MAIN REP              
*                                                                               
*        ANALYZE INTERNAL TEXT OPTION                                           
*                                                                               
         CLI   RINVFTYP,RINVFTIQ   IF THIS IS INTERNAL TEXT                     
         BNE   RATINT10                                                         
*                                                                               
         CLI   MASOP8,C'Y'            DROP IF NOT PRINTING INTERNAL TXT         
         BE    *+8                                                              
         CLI   MASOP8,C'O'            DROP IF NOT PRINTING INTERNAL TXT         
         BNE   RATX                                                             
*                                                                               
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
*                                                                               
         B     RATINTX                                                          
*                                                                               
RATINT10 DS    0H                  NOT INTERNAL TEXT                            
*                                                                               
         CLI   MASOP8,C'O'         SKIP IF PRINTING ONLY INTERNAL TEXT          
         BE    RATX                                                             
*                                                                               
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
*                                                                               
RATINTX  DS    0H                                                               
*                                                                               
RAT45    EQU   *                                                                
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(11),=C'RAT SOURCE:'                                            
         LA    RF,IO                                                            
         MVC   P+15(64),0(RF)                                                   
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         CLI   RINVFSRC,0          SOURCE FILTER                                
         BE    RAT50                                                            
         CLC   SVSOURCE,RINVFSRC                                                
         BNE   RATX                                                             
*                                                                               
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
*                                                                               
RAT50    OC    RINVFBK,RINVFBK     BOOK FILTER                                  
         BZ    RAT75                                                            
*                                                                               
         LA    R4,BOOKS                                                         
         LA    R0,20                                                            
*                                                                               
         TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   RAT55                                                            
*                                                                               
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(11),=C'RAT RANGE :'                                            
         MVC   P+15(2),RINVFBK                                                  
         MVC   P+18(2),1(R4)                                                    
         MVC   P+21(2),5(R4)                                                    
         MVC   P+25(27),KEY                                                     
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R0,20                                                            
*   TEST DATA FLOW END                                                          
*&&                                                                             
         CLI   RINVFBK+1,0         ESTIMATE BOOK? (0 = EST04, EG)               
         BNE   RAT53               NO                                           
         CLC   RINVFBK(1),1(R4)    YES, PRINT ALL TEXT BTWN START               
         BL    RATX                                                             
         CLC   RINVFBK(1),5(R4)    AND END RANGE: YEAR ONLY                     
         BH    RATX                                                             
         B     RAT54                                                            
RAT53    EQU   *                                                                
         CLC   RINVFBK,1(R4)       YES, PRINT ALL TEXT BTWN START               
         BL    RATX                                                             
         CLC   RINVFBK,5(R4)       AND END RANGE                                
         BH    RATX                                                             
*                                                                               
RAT54    EQU   *                                                                
         CLC   RINVFBTP,3(R4)      BOOK TYPES MUST MATCH                        
         BNE   RATX                                                             
*                                                                               
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
*                                                                               
         B     RAT75                                                            
*                                                                               
RAT55    DS    0H                                                               
*                                                                               
RAT60    DS    0H                                                               
*                                                                               
         OC    0(4,R4),0(R4)       DONE IF NO ENTRY IN TABLE                    
         BZ    RATX                                                             
*                                                                               
         CLC   RINVFBK,1(R4)       MATCH FILTER YEAR/MONTH                      
         BNE   RAT70                                                            
*                                                                               
         CLC   RINVFBKT,0(R4)      AND BOOKVAL BITS                             
         BNE   RAT70                                                            
*                                                                               
         CLC   RINVFBTP,3(R4)      AND BOOK TYPE                                
         BNE   RAT70                                                            
*                                                                               
RAT69    OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
         B     RAT75                                                            
*                                                                               
RAT70    LA    R4,4(R4)                                                         
         BCT   R0,RAT60                                                         
*                                                                               
         B     RATX                                                             
*                                                                               
RAT75    ZIC   R0,RINVFLEN         DEMO FILTERS                                 
         SH    R0,=H'10'                                                        
         BNP   RAT80                                                            
*                                                                               
         LA    R1,RINVFDEM                                                      
RAT76    LA    R4,DEMLST                                                        
         ZIC   RE,NUMDEMS                                                       
RAT77    CLC   0(1,R1),2(R4)                                                    
         BNE   RAT79                                                            
         OI    PRINTOPT,X'04'      ANY FILTERS TO PRINT                         
         B     RAT80                                                            
         SPACE 1                                                                
RAT79    LA    R4,3(R4)                                                         
         BCT   RE,RAT77                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RAT76                                                         
         B     RATX                                                             
*        SPACE 1                                                                
RAT80    DS    0H                                                               
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(11),=C'RAT 80    :'                                            
         LA    RF,IO                                                            
         MVC   P+15(64),0(RF)                                                   
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
*                                                                               
         TM    PRINTOPT,X'04'      ANY FILTERS TO PRINT                         
         BZ    RAT110                                                           
         MVC   4(12,R3),=C'TEXT FILTERS'                                        
         LA    R4,20(R3)                                                        
         CLI   RINVFSRC,0                                                       
         BE    RAT90                                                            
         MVC   0(7,R4),=C'SOURCE='                                              
         MVC   7(1,R4),RINVFSRC                                                 
         LA    R4,11(R4)                                                        
*                                                                               
RAT90    DS    0H                                                               
*                                                                               
         OC    RINVFBK,RINVFBK                                                  
         BZ    RAT95                                                            
*                                                                               
         MVC   0(5,R4),=C'BOOK='                                                
         LA    R5,5(R4)                                                         
*                                                                               
         CLI   RINVFBKT,0                                                       
         BE    RAT94                                                            
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN       INIT GETKSRC PARAMETERS             
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
*              START BOOK OF RANGE                                              
*                                                                               
         MVC   GSIRSVC,RINVFSRC    SET RATING SERVICE                           
         CLI   GSIRSVC,C' '        IF RATING SERVICE MISSING                    
         BH    *+8                                                              
         MVI   GSIRSVC,C'N'           DEFAULT TO NIELSEN                        
         MVC   GSIBITS,RINVFBKT    BOOKVAL BITS                                 
         MVC   GSIBKTYP,RINVFBTP   BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),(0,GSRCOUT),ACOMFACS                 
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RAT93    EQU   *                                                                
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(11),=C'RAT 93    :'                                            
         LA    RF,IO                                                            
         MVC   P+15(64),0(RF)                                                   
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         CLI   GSOQLF,C' '                                                      
         BNH   RAT94                                                            
         MVC   0(1,R5),GSOQLF                                                   
         LA    R5,1(R5)                                                         
*                                                                               
RAT94    DS    0H                                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,RINVFMN                                                     
         BZ    RAT94A              NO MONTH                                     
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
*                                                                               
         MVC   0(3,R5),0(R1)                                                    
*                                                                               
         LA    R5,3(R5)                                                         
*                                                                               
RAT94A   DS    0H                                                               
*                                                                               
         MVI   0(R5),C'/'                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVFYR        YEAR                                         
*                                                                               
         CH    RF,=H'100'          GET MODULO 100                               
         BL    *+8                                                              
         SH    RF,=H'100'                                                       
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         UNPK  1(2,R5),DUB         PRINT YEAR                                   
*                                                                               
         LA    R5,3(R5)                                                         
*                                                                               
         CLI   GSOBKTYP,C' '       PRINT BOOKTYPE IF PRESENT                    
         BNH   RAT94D                                                           
****                                                                            
****     GOTO1 GETBTYPE,DMCB,(GSOBKTYP,0)                                       
****     CLI   DMCB,0                                                           
****     BE    RAT94D                                                           
****                                                                            
         MVI   0(R5),C'('                                                       
                                                                                
****     ZIC   R1,DMCB                                                          
****     BCTR  R1,0                                                             
****     EX    R1,*+8                                                           
****     B     *+10                                                             
****     MVC   1(0,R5),DMCB+2                                                   
                                                                                
         MVC   1(1,R5),GSOBKTYP                                                 
                                                                                
         CLI   GSOBKTYP+1,C' '     IF SECOND CHARACTER                          
         BNH   *+14                                                             
         MVC   2(1,R5),GSOBKTYP+1     PRINT IT                                  
         LA    R5,1(R5)               BUMP POINTER                              
                                                                                
         LA    R5,2(0,R5)                                                       
         MVI   0(R5),C')'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
RAT94D   DS    0H                                                               
         LA    R4,16(R4)                                                        
*                                                                               
RAT95    ZIC   R0,RINVFLEN         PRINT OUT TEXT DEMO FILTERS                  
         SH    R0,=H'10'                                                        
         BNP   RAT100                                                           
         MVC   0(6,R4),=C'DEMOS='                                               
         LA    R4,6(R4)                                                         
         LA    R5,DBLOCKA1                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         LA    R1,RINVFDEM                                                      
RAT97    ST    R1,FULL    SAVE WHERE WE ARE IN RINVFDEM LIST                    
         LTR   R0,R0                                                            
         BZ    RAT99                                                            
         MVI   WORK,0                                                           
         MVI   WORK+1,C'I'                                                      
         MVC   WORK+2(1),0(R1)                                                  
         GOTO1 DEMOCON,PARAS,(0,WORK),(6,0(R4)),(0,DBLOCKD)                     
         DROP  R5                                                               
         LA    R4,6(R4)                                                         
RAT98    CLI   0(R4),C' '                                                       
         BNE   RAT98K                                                           
         BCTR  R4,R0                                                            
         B     RAT98                                                            
         SPACE 1                                                                
RAT98K   MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         SH    R0,=H'1'                                                         
         L     R1,FULL                                                          
         LA    R1,1(R1)                                                         
         B     RAT97                                                            
         SPACE 1                                                                
RAT99    BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
*                                                                               
RAT100   DS    0H                                                               
*                                                                               
         CLI   RINVFTYP,RINVFTIQ   IF INTERNAL TEXT                             
         BNE   RATPINTX                                                         
*                                                                               
         MVC   1(13,R4),=C'TYPE=INTERNAL'                                       
*                                                                               
         LA    R4,14(R4)           NEXT PRINT POSITION                          
*                                                                               
RATPINTX DS    0H                                                               
*                                                                               
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES    SPACE BTWN FILTERS & TEXT                    
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES                                                 
         LA    R2,2(R2)                                                         
RAT110   DS    0H                                                               
*                                                                               
*        PRINT LINES OF TEXT                                                    
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
*                                                                               
         CLI   OPTION,C'A'                                                      
         BE    RAT146                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   7(4,R3),=C'TEXT'                                                 
*                                                                               
         CLI   OPTION,C'M'                                                      
         BNE   *+10                                                             
         MVC   7(4,R3),=C'MKT.'                                                 
*                                                                               
         CLI   OPTION,C'S'                                                      
         BNE   *+10                                                             
         MVC   7(4,R3),=C'STN.'                                                 
*                                                                               
         CLC   7(4,R3),=C'TEXT'    SKIP IF INVENTORY TEXT                       
         BE    RAT115                                                           
*                                                                               
         CLI   NOREQSTA,1          SKIP IF ONLY ONE STATION REQUESTED           
         BNH   RAT115                                                           
*                                                                               
         CLC   SVSTAPRT,RINVKSTA   SKIP IF SAME STATION                         
         BE    RAT115                                                           
*                                                                               
         MVC   SVSTAPRT,RINVKSTA   SAVE CURRENT STATION                         
*                                                                               
         MVC   0(4,R3),RINVKSTA    PRINT STATION                                
         CLI   RINVKSTA+4,C'T'     SKIP BAND IF TV                              
         BE    *+14                                                             
         MVI   4(R3),C'-'                                                       
         MVC   5(1,R3),RINVKSTA+4                                               
*                                                                               
RAT115   DS    0H                                                               
*                                                                               
         EDIT  (2,RINVKTXT),(5,12(R3)),ALIGN=LEFT                               
*                                                                               
RAT120   LA    R5,13(R3)                                                        
*                                                                               
RAT130   CLC   0(3,R5),SPACES                                                   
         BE    RAT140                                                           
*                                                                               
         LA    R5,1(R5)                                                         
         B     RAT130                                                           
*                                                                               
RAT140   LA    R5,1(R5)            LEAVE SPACE                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
RAT145   DS    0H                                                               
*                                                                               
         CLI   SVWRPOPT,C'Y'       IF USING WORD WRAP                           
         BNE   RAT146                                                           
*                                                                               
         GOTO1 =A(RATWRP),RR=RELO     USE SPECIAL CODE                          
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(13),=C'AFTER RATWRP:'                                          
         LA    RF,IO                                                            
         MVC   P+15(64),0(RF)                                                   
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         B     RAT170                 GO DO PRINTING                            
*                                                                               
RAT146   DS    0H                                                               
*                                                                               
         USING RINVTEL,R6                                                       
         BAS   RE,GETEL            FIND FIRST LINE OF TEXT                      
         BE    RAT160              LINE OF TEXT EXISTS                          
*                                                                               
         LA    R2,1(R2)            PRINT CURENT LINE                            
         B     RAT170              END OF TEXT                                  
*                                                                               
RAT160   BNE   RAT170              END OF TEXT                                  
*                                                                               
         LA    RE,RINVTEXT                                                      
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
*                                                                               
         CLI   OPTION,C'A'                                                      
         BNE   RAT166                                                           
*                                                                               
         LA    R5,30(R3)                                                        
*                                                                               
         SH    R1,=H'9'            PROGRAM NAME HAS LENGTH OF 16                
         LA    RE,0(R1,R6)         START OF PROGRAM NAME                        
         LA    R1,15               EXECUTE LENGTH OF PROGRAM NAME               
*                                                                               
RAT166   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)                                                    
*                                                                               
         LA    R3,132(R3)                                                       
         MVC   30(104,R3),SPACES   DON'T CREAM HEADLINE STUFF THERE             
         LA    R5,132(R5)                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         B     RAT160                                                           
*                                                                               
RAT170   CLI   OPTION,C'A'                                                      
         BNE   RAT173                                                           
*                                                                               
         ST    R3,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
         B     RATX                                                             
*                                                                               
RAT173   LTR   R2,R2               ANYTHING TO PRINT                            
         BNZ   RAT176                                                           
         CLC   P(40),SPACES        HD2-HD5 TO BE PRINTED                        
         BNE   RAT190                                                           
         B     RATX                                                             
RAT176   STC   R2,ALLOWLIN         FORCES ALL TEXT TO SAME PAGE                 
         L     R3,SBUFF                                                         
         SPACE 2                                                                
RAT180   EQU   *                                                                
         MVI   CONTSW,C'Y'                                                      
         MVC   P,0(R3)                                                          
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         XC    0(132,R3),0(R3)     CLEAR OUT BUFF                               
         MVI   ALLOWLIN,0                                                       
         LA    R3,132(R3)                                                       
         BCT   R2,RAT180                                                        
         MVI   CONTSW,C'N'                                                      
*                                                                               
         CLI   OPTION,C'A'                                                      
         BE    RATX                                                             
RAT190   MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)    SPACE AFTER RATIONALE                        
         B     RATX                                                             
*                                                                               
RATX     DS    0H                                                               
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(10),=C'AFTER RAT:'                                             
         LA    RF,IO                                                            
         MVC   P+15(64),0(RF)                                                   
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - UPPRNT'               
********************************************************************            
*                                                                  *            
*              PRINT UPGRADE COMMENTS                              *            
*              ----------------------                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
UPPRNT   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'        ANY UPGRADE ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   UPPRNTX                                                          
*                                                                               
*        FOLLOWING DISABLED AS OF 11/17/00 FJD                                  
*        CLC   SAVECODE,=C'PJ'     ONLY SHOW UPGRADE IF                         
*        BNE   UPPRNTX             CODE IS PJ                                   
                                                                                
         CLC   SAVECODE,=C'PJ'     ONLY SHOW UPGRADE IF                         
         BE    UPPRNT30            CODE IS PJ                                   
                                                                                
         TM    RMPPROF+RMPESUPB,RMPESUPA   (CHECK PROFILE)                      
         BNO   UPPRNTX                                                          
                                                                                
         CLC   SAVECODE,=C'ES'     OR ES IF PROFILE BIT WAS ON                  
         BNE   UPPRNTX                                                          
                                                                                
UPPRNT30 DS    0H                 SUPPRESS UPGD FOOTNT IF >1 SHARE BK           
         ST    R6,AUPELEM          SAVE A(UPGRADE ELEMENT)                      
         LA    R6,IO                                                            
         MVI   ELCODE,RIDHCDQ      LOOK FOR DEMO HISTORY ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   UPPRNT50                                                         
                                                                                
         USING RIDHEL,R6                                                        
         MVC   SHRSRC,RIDHSRC      REMEMBER RATING SERVICE,                     
         MVC   SHRBK,RIDHBK         BOOK                                        
         MVC   SHRBTYP,RIDHBTYP     AND BOOKTYPE OF 1ST SHARE BOOK              
         DROP  R6                                                               
                                                                                
UPPRNT40 DS    0H                  MATCH AGAINST SUBSEQUENT SHARE BOOKS         
         BAS   RE,NEXTEL                                                        
         BNE   UPPRNT50                                                         
         USING RIDHEL,R6                                                        
         CLC   SHRSRC,RIDHSRC                                                   
         BNE   UPPRNT45                                                         
         CLC   SHRBK,RIDHBK                                                     
         BNE   UPPRNT45                                                         
         CLC   SHRBTYP,RIDHBTYP                                                 
         BNE   UPPRNT45                                                         
         B     UPPRNT40                                                         
         DROP  R6                                                               
                                                                                
UPPRNT45 DS    0H                  SKIP UPGRADE EXPRESSION EDIT                 
         B     UPPRNTX                                                          
                                                                                
UPPRNT50 EQU   *                                                                
         L     R6,AUPELEM          RESTORE R6 TO UPGRADE ELEMENT                
*                                                                               
         L     R3,ABUFF                                                         
         LA    R2,28(R3)           EDIT UPGRADE EXPRESSION                      
         GOTO1 AUPOUT,DMCB,(R6),(R2),(C'C',ACOMFACS)                            
         BAS   RE,PEEPMTRS         PERFORM PEOPLE METER CLEANUP                 
         LA    R3,132(R3)                                                       
         ST    R3,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
UPPRNTX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PEEPMTRS:                                                                   
*       DROP INAPPROPRIATE PEOPLE METER INDICATORS IF DATES                     
*        DID NOT ACTUALLY USE PEOPLE METER DATA.                                
*   R3 --> RETURNED UPGRADE PRINTOUT                                            
*   R6 --> UPGRADE ELEMENT X'05'                                                
*                                                                               
PEEPMTRS NTR1                                                                   
*                                                                               
         USING RAVLNCOD,R6                                                      
*                                                                               
         L     RF,=A(STAMKT#-SYSD)                                              
         AR    RF,R9                                                            
         MVC   HALF,0(RF)          RETRIEVE STATION MARKET #                    
*                                                                               
*   CHECK UPGRADE BOOK TYPE:  IF NOT REGULAR OR HISP PM, EXIT                   
*                                                                               
         MVC   UPSTRING+6(1),RAVLNBT     CONSTRUCT COMPARISON STRING            
*                                  INSERT BOOK TYPE                             
*                                                                               
         CLI   RAVLNBT,C'I'        HISPANIC PEOPLE METER?                       
         BE    PEEP0100            YES - PROCESS HISP MARKETS                   
         CLI   RAVLNBT,C'P'        REGULAR  PEOPLE METER?                       
         BNE   PEEP0800            NO  - NO FURTHER CHECK NEEDED                
*                                                                               
*        REGULAR PEOPLE METER MARKETS                                           
*                                                                               
         MVC   DTSTRING,=X'68046808'                                            
         CLC   HALF,=H'101'        NY?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'68056807'                                            
         CLC   HALF,=H'403'        LA?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'68076809'                                            
         CLC   HALF,=H'202'        CHI?                                         
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'680A680C'                                            
         CLC   HALF,=H'407'        SF?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'69056906'                                            
         CLC   HALF,=H'111'        WAS                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'69056906'                                            
         CLC   HALF,=H'104'        PHL                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         B     PEEP0800            NOT USING PEOPLE METER - EXIT                
*                                                                               
*                                                                               
*        HISPANIC PEOPLE METER MARKETS                                          
*                                                                               
PEEP0100 EQU   *                                                                
         MVC   DTSTRING,=X'68046808'                                            
         CLC   HALF,=H'101'        NY?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'68076809'                                            
         CLC   HALF,=H'202'        CHI?                                         
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         MVC   DTSTRING,=X'680A680C'                                            
         CLC   HALF,=H'407'        SF?                                          
         BE    PEEP0200            YES - CHECK ALL BOOKS                        
         B     PEEP0800            NOT USING PEOPLE METER - EXIT                
*                                                                               
PEEP0200 EQU   *                                                                
         LA    R4,RAVLNOP1                                                      
PEEP0220 EQU   *                                                                
         OC    0(2,R4),0(R4)       ANYTHING IN OPERAND?                         
         BZ    PEEP0800            NO  - FINISHED SCAN                          
         CLC   0(2,R4),DTSTRING    OPERAND PRIOR TO PM START?                   
         BL    PEEP0240            YES - EDIT RETURNED FOOTNOTE                 
*                                  NO                                           
PEEP0230 EQU   *                                                                
         LA    R4,2(R4)            BUMP TO NEXT OPERAND                         
         B     PEEP0220            GO BACK FOR NEXT                             
PEEP0240 EQU   *                                                                
         MVC   WORK(2),0(R4)       LIFT YYMM FROM OPERAND                       
         MVI   WORK+2,1            SET DAY TO 01                                
         GOTO1 DATCON,DMCB,(3,WORK),(9,WORK+3)                                  
         MVC   UPSTRING(3),WORK+3  ELIMINATE '/'                                
         MVC   UPSTRING+3(2),WORK+7                                             
         LR    RF,R3               SET START OF UPGRADE FOOTNOTE                
         LA    RE,132              CHECK EACH POSITION FOR STRING               
PEEP0260 EQU   *                                                                
         CLC   0(8,RF),UPSTRING    MMMYY(X) STRING FOUND IN FTNTE?              
         BE    PEEP0300            YES - RESET IT                               
PEEP0280 EQU   *                                                                
         LA    RF,1(RF)            NO  - BUMP TO NEXT POSITION                  
         BCT   RE,PEEP0260         CHECK NEXT EIGHT POSITIONS                   
         B     PEEP0230            GO BACK FOR NEXT OPERAND                     
PEEP0300 EQU   *                                                                
         MVC   5(3,RF),SPACES      CLEAR (X) FROM OPERAND                       
         B     PEEP0280            TAKE CARE OF REPEATS ON LINE                 
PEEP0800 EQU   *                                                                
         XIT1                                                                   
UPSTRING DC    C'MMMYY(X)'                                                      
DTSTRING DS    XL4                                                              
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - RATWRP'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        ROUTINE FOR WORD WRAPPING. TEXT IS STORED                 *            
*        CONSECUTIVELY AND THEN CHOPPED                            *            
*        NEW LINE OCCURS WHEN NEXT ELEMENT STARTS WITH 2 SPACES    *            
*                                                                  *            
*NTRY  R3==> FIRST PRINT LINE IN BUFFER TO BE USED                 *            
*      R5==> FIRST POSITION ON LINE FOR PRINTING                   *            
*      R2==> NUMBER OF LINES IN BUFFER                             *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
RATWRP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ATXTWRK          POINT TO TEXT WORKAREA                       
*                                                                               
         BAS   RE,GETEL            FIND FIRST LINE OF TEXT                      
         BE    RWRLOOP              TEXT FOUND                                  
*                                                                               
         LA    R2,1(R2)            PRINT CURENT LINE                            
         B     RWRDONE             END OF TEXT                                  
*                                                                               
RWRLOOP  DS    0H                  END OF TEXT                                  
*                                                                               
         BNE   RWRDONE                                                          
*                                                                               
         USING RINVTEL,R6          ESTABLISH TEXT ELEMENT                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         TEXT ELEMENT LENGTH                          
         SH    RF,=H'7'            TEXT'S EXECUTE LENGTH                        
         BM    RWRCONT             MUST HAVE TEXT                               
*                                                                               
         CH    RF,=H'1'            MAX 2 POSITIONS CHECKED                      
         BNH   *+8                                                              
         LA    RF,1                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RINVTEXT(0),SPACES IF ELEMENT STARTS WITH SPACES                 
         BH    RWRLP20                                                          
*                                     NEED TO CHOP WHAT WE HAVE                 
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RE,ATXTWRK          START OF PRINT AREA                          
         LR    RF,R4               COPY CURRENT END OF TEXT                     
         SR    RF,RE               LENGTH OF TEXT TO PRINT                      
         BNP   RWRLP10             NOTHING TO PRINT                             
         ST    RF,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         GOTO1 CHOPPER,DMCB,(0,ATXTWRK),(90,(R5)),(C'P',20)                     
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+14                IF NOTHING TO PRINT                          
         LA    R1,1                   ALWAYS PRINT ONE LINE                     
         MVC   0(2,R5),=X'0000'       FORCE LINE PRINT                          
*                                                                               
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
         LA    RF,132              PRINT LINE WIDTH                             
         MR    RE,R1               DISPLACEMENT USED                            
*                                                                               
         AR    R5,RF               UPDATE PRINT START                           
*                                                                               
RWRLP10  DS    0H                                                               
*                                                                               
         L     R4,ATXTWRK          RESET PRINT POINTERS                         
*                                                                               
RWRLP20  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         TEXT ELEMENT LENGTH                          
         SH    RF,=H'7'            TEXT'S EXECUTE LENGTH                        
         BM    RWRCONT             MUST HAVE TEXT                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RINVTEXT(0),SPACES  IF BLANK LINE                                
         BH    RWRLP30                                                          
*                                                                               
         MVC   0(2,R5),=X'0000'       FORCE LINE PRINT                          
         LA    R5,132(R5)             BUMP PRINT POINTER BY A LINE              
         LA    R2,1(R2)               UPDATE BUFFER COUNTER                     
*                                                                               
         B     RWRCONT                                                          
*                                                                               
RWRLP30  DS    0H                                                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RINVTEXT    MOVE TEXT TO PRINTAREA                       
*                                                                               
         LA    R4,1(RF,R4)         NEXT PRINT AREA                              
         MVI   0(R4),C' '          PUT SPACE BETWEEN TEXT PIECES                
         LA    R4,1(R4)            BUMP POINTER                                 
*                                                                               
RWRCONT  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         B     RWRLOOP                                                          
*                                                                               
RWRDONE  DS    0H                                                               
*                                                                               
*        CHOP ANY REMANING TEXT                                                 
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RF,ATXTWRK          START OF PRINT AREA                          
         SR    R4,RF               LENGTH OF TEXT TO PRINT                      
         BZ    RWRDN10             NOTHING TO PRINT                             
         ST    R4,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         GOTO1 CHOPPER,DMCB,(0,ATXTWRK),(90,(R5)),(C'P',20)                     
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+6                 MUST HAVE PRINTED SOMETHING                  
         DC    H'0'                                                             
*                                                                               
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
RWRDN10  DS    0H                                                               
*                                                                               
RATWRPX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PDTM'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        ROUTINE TO PRINT LOOK-UP DAY/TIMES                        *            
*                                                                  *            
*NTRY    R3 ==> PRINT POSITION                                     *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PRNTDTM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PRINT DAY/TIME ELEMENTS                                                
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   PDTMDTX             NONE FOUND - SKIP PRINTING                   
*                                                                               
PDTMDTLP DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,PARAS,RIDTDAY,WORK       DAY                               
*                                                                               
         OC    WORK,SPACES                                                      
         GOTO1 UNTIME,PARAS,RIDTTIME,WORK+20  TIME                              
*                                                                               
         GOTO1 SQUASHER,PARAS,WORK,40         SQUASH DAY/TIME                   
*                                                                               
         MVC   0(22,R3),WORK                  PRINT DAY/TIME                    
*                                                                               
         LA    R0,22                                                            
         LA    R3,21(R3)           FIND LAST PRINTABLE CHARACTER                
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(R3),C','          SET SEPARATOR                                
         LA    R3,3(R3)            NEXT PRINT POSITION                          
*                                                                               
PDTMDTCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    PDTMDTLP            ONE FOUND                                    
*                                                                               
PDTMDTDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         CH    R2,=H'1'            IF MORE THAN ONE DAY-TIME ELEMENT            
         BNH   *+8                                                              
         MVI   PRNTDISP+1,0           USE ALTERNATE FORMAT                      
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,2(R3)            RESET END OF DATA POINTER                    
*                                                                               
         LR    R0,R3               CALCULATE DATA LENGTH                        
         LA    RE,P                                                             
         SR    R0,RE                                                            
*                                                                               
         LTR   RF,R0               COPY LENGTH                                  
         BZ    PDTMDTX             NO DATA TO CHOP                              
*                                                                               
         CLI   PRNTDISP+1,0        IF ALTERNATE FORMAT                          
         BE    PDTMDTX                SKIP CHOPPING                             
*                                                                               
         LA    R1,P-1(RF)          POINT TO LAST BYTE IN DATA                   
*                                                                               
         CLI   0(R1),C'-'          CONVERT HYPHENS TO X'FF' TO                  
         BNE   *+8                 STOP CHOPPER FROM SPLITTING ON THEM          
         MVI   0(R1),X'FF'                                                      
         BCTR  R1,0                                                             
         BCT   RF,*-14                                                          
*                                                                               
         L     R2,ABUFF            CURRENT BUFFER POSITION                      
*                                                                               
         GOTO1 CHOPPER,DMCB,((R0),P),(27,(R2)),(C'P',10)                        
*                                                                               
         L     RF,DMCB+8           NUMBER OF PRINT LINES                        
         LA    RF,0(RF)                                                         
         L     R1,ABUFF            START OF DAY/TIMES PRINT AREA                
*                                                                               
PDTMDT1L DS    0H                                                               
*                                                                               
         LA    R0,27               WIDTH OF PRINT AREA                          
         LR    RE,R1               START OF PRINT AREA TO CHECK                 
*                                                                               
PDTMDT2L DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         RESTORE HYPHENS                              
         BNE   *+8                                                              
         MVI   0(RE),C'-'                                                       
*                                                                               
PDTMDT2C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,PDTMDT2L                                                      
*                                                                               
PDTMDT2D DS    0H                                                               
*                                                                               
PDTMDT1C DS    0H                                                               
*                                                                               
         LA    R1,132(R1)          NEXT PRINT LINE                              
         BCT   RF,PDTMDT1L                                                      
*                                                                               
PDTMDT1D DS    0H                                                               
*                                                                               
         L     RF,DMCB+8           NUMBER OF LINES USED                         
         LA    RF,0(RF)            CLEAR HIGH ORDER BYTE                        
*                                                                               
         BAS   RE,BUFFADD          UPDATE BUFFER POINTER                        
         BCT   RF,*-4                                                           
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         LA    R3,P                POINT TO NEXT WORK PRINT POSITION            
*                                                                               
PDTMDTX  DS    0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - PMHDTM'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        ROUTINE TO PRINT LOOK-UP MULTI HUT DAY/TIMES              *            
*                                                                  *            
* ENTRY   R3 ==> PRINT POSITION                                    *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PRNTMHDT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PRINT MULTI HUT DAY/TIME ELEMENTS                                      
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'09'        SET TO FIND A MH DAY/TIME ELEMENTS           
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   PMHDTX              NONE FOUND - SKIP PRINTING                   
*                                                                               
PMHDTLP  DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIMHELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,PARAS,RIMHDAY,WORK       DAY                               
*                                                                               
         OC    WORK,SPACES                                                      
         GOTO1 UNTIME,PARAS,RIMHTIME,WORK+20  TIME                              
*                                                                               
         GOTO1 SQUASHER,PARAS,WORK,40         SQUASH DAY/TIME                   
*                                                                               
         MVC   0(22,R3),WORK                  PRINT DAY/TIME                    
*                                                                               
         LA    R0,22                                                            
         LA    R3,21(R3)           FIND LAST PRINTABLE CHARACTER                
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(R3),C','          SET SEPARATOR                                
         LA    R3,3(R3)            NEXT PRINT POSITION                          
*                                                                               
PMHDTCN  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    PMHDTLP             ONE FOUND                                    
*                                                                               
PMHDTDN  DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         CH    R2,=H'1'            IF MORE THAN ONE DAY-TIME ELEMENT            
         BNH   *+8                                                              
         MVI   PRNTDISP+1,0           USE ALTERNATE FORMAT                      
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,2(R3)            RESET END OF DATA POINTER                    
*                                                                               
         LR    R0,R3               CALCULATE DATA LENGTH                        
         LA    RE,P                                                             
         SR    R0,RE                                                            
*                                                                               
         LTR   RF,R0               COPY LENGTH                                  
         BZ    PMHDTX              NO DATA TO CHOP                              
*                                                                               
         CLI   PRNTDISP+1,0        IF ALTERNATE FORMAT                          
         BE    PMHDTX                 SKIP CHOPPING                             
*                                                                               
         LA    R1,P-1(RF)          POINT TO LAST BYTE IN DATA                   
*                                                                               
         CLI   0(R1),C'-'          CONVERT HYPHENS TO X'FF' TO                  
         BNE   *+8                 STOP CHOPPER FROM SPLITTING ON THEM          
         MVI   0(R1),X'FF'                                                      
         BCTR  R1,0                                                             
         BCT   RF,*-14                                                          
*                                                                               
         L     R2,ABUFF            CURRENT BUFFER POSITION                      
*                                                                               
         GOTO1 CHOPPER,DMCB,((R0),P),(27,(R2)),(C'P',10)                        
*                                                                               
         L     RF,DMCB+8           NUMBER OF PRINT LINES                        
         LA    RF,0(RF)                                                         
         L     R1,ABUFF            START OF DAY/TIMES PRINT AREA                
*                                                                               
PMHDT1L  DS    0H                                                               
*                                                                               
         LA    R0,27               WIDTH OF PRINT AREA                          
         LR    RE,R1               START OF PRINT AREA TO CHECK                 
*                                                                               
PMHDT2L  DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         RESTORE HYPHENS                              
         BNE   *+8                                                              
         MVI   0(RE),C'-'                                                       
*                                                                               
PMHDT2C  DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,PMHDT2L                                                       
*                                                                               
PMHDT2D  DS    0H                                                               
*                                                                               
PMHDT1C  DS    0H                                                               
*                                                                               
         LA    R1,132(R1)          NEXT PRINT LINE                              
         BCT   RF,PMHDT1L                                                       
*                                                                               
PMHDT1D  DS    0H                                                               
*                                                                               
         L     RF,DMCB+8           NUMBER OF LINES USED                         
         LA    RF,0(RF)            CLEAR HIGH ORDER BYTE                        
*                                                                               
         BAS   RE,BUFFADD          UPDATE BUFFER POINTER                        
         BCT   RF,*-4                                                           
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         LA    R3,P                POINT TO NEXT WORK PRINT POSITION            
*                                                                               
PMHDTX   DS    0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - HOOK'                 
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*              HOOK ROUTINE FOR HEADLINE DETAILS                   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PRINT STATION ID                                                       
*                                                                               
         MVC   H3(46),SPACES       INIT STATION ID AREA                         
*                                                                               
         LA    R5,SVSTCKD          ESTABLISH CURRENT STACK ENTRY                
         USING STCKD,R5                                                         
*                                                                               
         CLI   NOREQSTA,1          SKIP IF MORE THAN ONE STATION                
         BH    HKSTA5                                                           
*                                                                               
         MVC   H3(9),=C'STATION -'                                              
         MVC   H3+10(4),STCKSTAC   STATION CALL LETTERS                         
*                                                                               
         CLI   STCKSTAC+4,C' '     SKIP IF TV                                   
         BE    *+8                                                              
         CLI   STCKSTAC+4,C'T'                                                  
         BE    *+14                                                             
         MVI   H3+14,C'-'                                                       
         MVC   H3+15(1),STCKSTAC+4 PRINT BAND                                   
*                                                                               
         MVC   H3+17(20),MKTSV     MARKET NAME                                  
*                                                                               
         B     HKSTAX                                                           
*                                                                               
HKSTA5 DS      0H                                                               
*                                                                               
         OC    STMENU,STMENU       SKIP IF NOT A STATION MENU                   
         BZ    HKSTA8                                                           
*                                                                               
         MVC   H3(9),=C'MENU    -'                                              
         MVC   H3+10(4),STMENU     MENU ID                                      
         MVC   H3+17(29),STMENUNM                                               
*                                                                               
         B     HKSTAX                                                           
*                                                                               
HKSTA8 DS      0H                  MULTIPLE STATIONS                            
*                                                                               
         MVC   H3(9),=C'STATIONS-'                                              
         MVC   H3+10(7),SPACES                                                  
         MVC   H3+17(20),=CL29'VARIOUS'                                         
*                                                                               
HKSTAX DS      0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   H3+108(3),MASSRCE   SOURCE                                       
*                                                                               
         LH    R3,=Y(DPTBL-SYSD)                                                
         LA    R3,SYSD(R3)         LOOK UP DAYPART                              
         USING DPTBLD,R3           ESTABLISH DAYPART TABLE                      
*                                                                               
HK10     CLC   SVDPT,DPTBCODE      MATCH DAYPART CODE                           
         BE    HK20                                                             
         LA    R3,DPTBLL(R3)                                                    
         OC    DPTBLD(DPTBLL),DPTBLD  CHECK FOR END OF TABLE                    
         BNZ   HK10                                                             
         B     *+10                NO MATCH                                     
HK20     MVC   H3+65(L'DPTBLNAM),DPTBLNAM  DAYPART NAME                         
*                                                                               
         CLI   PRNTDISP+1,0        IF NOT ALTERNATE FORMAT                      
         BE    HK70                                                             
*                                                                               
         MVC   H5,SVH7             MOVE IN SAVED DEMO HEADLINES                 
         MVC   H6,SVH8                                                          
         MVC   H7,SVH9                                                          
*                                                                               
         MVC   H5(13),=C'INV  DAY TIME'                                         
*                                                                               
         CLI   NOREQSTA,1          IF MULTIPLE STATIONS                         
         BNH   *+10                                                             
         MVC   H5(21),=C'INV-STATION  DAY TIME'                                 
*                                                                               
         MVC   H5+30(7),=C'DAYPART'                                             
         MVC   H6(17),=C'PROGRAM/ADJACENCY'                                     
         MVC   H7(9),=C'EFFECTIVE'                                              
         MVC   H7+31(12),=C'BOOK      CD'                                       
         SPACE 1                                                                
         CLI   CONTSW,C'Y'                                                      
         BNE   HOOKX                                                            
         MVC   H8(23),CONTINUE     NNNN (CONTINUED)                             
         B     HOOKX                                                            
         SPACE 1                                                                
HK70     MVC   H7,SVH7             MOVE IN SAVED DEMO HEADLINES                 
         MVC   H8,SVH8                                                          
         MVC   H9,SVH9                                                          
*                                                                               
         MVC   H5(21),=CL21'INV  DAY TIME'                                      
*                                                                               
         CLI   NOREQSTA,1          IF MULTIPLE STATIONS                         
         BNH   *+10                                                             
         MVC   H5(21),=C'INV-STATION  DAY TIME'                                 
*                                                                               
         MVC   H5+022(17),=C'PROGRAM/ADJACENCY'                                 
         MVC   H5+107(9),=C'EFFECTIVE'                                          
         MVC   H5+121(7),=C'DAYPART'                                            
         MVC   H7+001(12),=C'BOOK      CD'                                      
         SPACE 1                                                                
         CLI   CONTSW,C'Y'                                                      
         BNE   HOOKX                                                            
         MVC   H10(23),CONTINUE    NNNN (CONTINUED)                             
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*              PERCENT SIGN TACKED ONTO SHARES                                  
*              -------------------------------                                  
         SPACE 2                                                                
PERCENT  NTR1  BASE=*,LABEL=*                                                   
         L     R3,ABUFF                                                         
         LA    R3,12(R3)           FORMATTED LINE                               
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
         LA    R2,DEMLST                                                        
*                                                                               
         ZIC   R4,NUMDEMS                                                       
*                                                                               
PC100    EQU   *                   NON-ZERO VALUES ONLY                         
*                                                                               
         CLI   1(R2),C'U'          UNIVERSE?                                    
         BE    PC140                                                            
*                                                                               
         CLI   MASOP5,C'Y'         DIFFERENT DISPLACEMENTS IF ROUNDING          
         BNE   PC110                                                            
*                                                                               
         CLI   4(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   5(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
         B     PC140                                                            
*                                                                               
PC110    DS    0H                                                               
*                                                                               
         CLI   NUMDEMS,17          DIFFERENT DISPLACEMENTS IF >17 DEMOS         
         BH    PC120                                                            
*                                                                               
         CLI   5(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   6(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
         B     PC140                                                            
*                                                                               
PC120    DS    0H                  >17 DEMOS                                    
*                                                                               
         CLI   4(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   5(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
PC140    LA    R2,3(R2)                                                         
         LA    R3,5(R3)                                                         
*                                                                               
         CLI   MASOP5,C'Y'         DIFFERENT DISPLACEMENTS IF ROUNDING          
         BE    PC150                                                            
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   NUMDEMS,17          DIFFERENT DISPLACEMENTS IF >17 DEMOS         
         BH    *+8                                                              
         LA    R3,1(R3)                                                         
*                                                                               
PC150    DS    0H                  >17 DEMOS                                    
*                                                                               
         BCT   R4,PC100                                                         
*                                                                               
         L     R3,ABUFF                                                         
         MVI   132(R3),C' '        KILL POSSIBLE % AT START OF NXT LINE         
*                                                                               
PCX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
         TITLE 'RERES0C - MASTER REPORT - PFKEYS'                               
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*                                                                     *         
*              PF10 - TITLES REPORT                                   *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLI   TIOBAID,9           CHECK FOR TRANSFER TO TITLES REPORT          
         BE    *+8                                                              
         CLI   TIOBAID,21          CHECK FOR TRANSFER TO TITLES REPORT          
         BE    PFKTTL                                                           
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
***********************************************************************         
*                                                                     *         
*        TRANSFER TO TITLES REPORT USING GLOBBER                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKTTL   DS    0H                  NORMAL EXIT                                  
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'REP'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'RSC'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'REP'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'RSC'    SET TO   PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL  SEND XCTL ELM            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'TITLES',6,GLVXREC  RECORD FLD          
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'REPORT',6,GLVXACT  ACTION              
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'DEFAULT',7,GLRKEY  KEY                 
         LA    R2,CONWHEN                                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'NOW',3,GLRWHEN  WHEN FIELD             
*                                                                               
*        BUILD VARIABLE DATA FOR TITLES REQUEST SCREEN                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING TTLDATAD,R2         ESTABLISH TITLE DATA                         
*                                                                               
         MVC   TTLSTNL,MASSTNH+5   PASS STATION LENGTH                          
         MVC   TTLSTN,MASSTN       PASS STATION                                 
         MVC   TTLDPTL,MASDPTH+5   PASS DAYPART LENGTH                          
         MVC   TTLDPT,MASDPT       PASS DAYPART                                 
         MVC   TTLESDL,MASESDTH+5  PASS EFFECTIVE START DATE LENGTH             
         MVC   TTLESD,MASESDT      PASS EFFECTIVE START DATE                    
         MVC   TTLEEDL,MASEEDTH+5  PASS EFFECTIVE END   DATE LENGTH             
         MVC   TTLEED,MASEEDT      PASS EFFECTIVE END   DATE                    
         MVC   TTLFTRL,MASFTRH+5   PASS FILTERS LENGTH                          
         MVC   TTLFTR,MASFTR       PASS FILTERS                                 
         MVC   TTLOP3L,MASOP3H+5   PASS NO DUPLICATES OPTION LENGTH             
         MVC   TTLOP3,MASOP3       PASS NO DUPLICATES OPTION                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,TTLDATAL,GLRDATA  SEND DATA          
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'RERES0C - MASTER REPORT - PFKEYS - EXITS'                       
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERES0C - MASTER REPORT - PFKEYS - HISTDATA'                    
***********************************************************************         
*                                                                     *         
*           ROUTINE TO PRINT '03' ELEMENT OUT                         *         
*         ------------------------------------                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HISTDATA NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   HISTX              SHOULD NEVER HAPPEN                           
         USING RINVFREL,R6                                                      
*                                                                               
         L     R3,ABUFF           ADD LINE TO BUFFER                            
         LA    R5,28(R3)                                                        
         MVC   0(15,R5),SPACES                                                  
         MVC   0(5,R5),RINVFRST   MOVE STATION                                  
         LA    R5,6(R5)                                                         
*                                                                               
*        FIND RATING SERVICE, BOOKTYPE                                          
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,SVSOURCE    SET RATING SERVICE                           
         MVC   GSIBITS,RINVFRBK    SET BOOKVAL BITS                             
         MVC   GSIBKTYP,RINVFRBT   SET BOOKTYPE                                 
*                                                                               
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),(0,GSRCOUT),ACOMFACS                 
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   HIST40              (COULD BE BOOK TYPE 'D')                     
*                                                                               
******   BE    *+6                                                              
******   DC    H'0'                                                             
*                                                                               
         CLI   GSOQLF,C' '         PRINT PREFIX IF PRESENT                      
         BNH   HIST40                                                           
*                                                                               
         MVC   0(1,R5),1(R2)                                                    
         LA    R5,1(R5)                                                         
*                                                                               
HIST40   ZIC   R1,RINVFRBK+2               MONTH                                
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
         MVI   3(R5),C'/'                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVFRBK+1     YEAR                                         
*                                                                               
         CH    RF,=H'100'          GET MODULO 100                               
         BL    *+8                                                              
         SH    RF,=H'100'                                                       
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         UNPK  4(2,R5),DUB         PRINT YEAR                                   
*                                                                               
         CLI   RINVFRBT,0          PRINT BOOKTYPE IF PRESENT                    
         BE    HIST43                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(RINVFRBT,0)                                       
         CLI   DMCB,0                                                           
         BE    HIST43                                                           
                                                                                
         MVI   6(R5),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   7(0,R5),DMCB+2                                                   
                                                                                
         LA    R5,8(R1,R5)                                                      
         MVI   0(R5),C')'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
HIST43   DS    0H                                                               
         LA    R5,7(R5)                                                         
*                                                                               
         MVC   0(1,R5),RINVFRTY      TYPE (P/I)                                 
         MVC   1(1,R5),RINVFRPR      FUNCTION                                   
         MVC   2(1,R5),RINVFRBT      BOOK TYPE                                  
         LA    R5,4(R5)                                                         
*                                                                               
         ZIC   R2,RINVFRLN        MOVE FROM DATA IF ANY                         
         SH    R2,=H'16'                                                        
         LTR   R2,R2                                                            
         BZ    HISTADD            NO OTHER DATA TO BE ADDED                     
         CH    R2,=H'70'          WILL IT FIT ON ONE LINE?                      
         BNH   HIST45             NO                                            
*                                                                               
         LA    R4,70                                                            
         GOTO1 CHOPPER,DMCB,((R2),RINVFRDT),((R4),(R5)),(C'P',3)                
         B     HISTADD                                                          
*                                                                               
HIST45   BCTR  R2,0                                                             
         EX    R2,HISTMV                                                        
         B     HISTADD                                                          
*                                                                               
HISTMV   MVC   0(0,R5),RINVFRDT                                                 
HISTADD  LA    R3,132(R3)                                                       
         ST    R3,ABUFF                                                         
HISTX    XIT1                                                                   
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - HDRNEW'               
***********************************************************************         
*                                                                     *         
*        PROCESSING HEADER RECORDS                                    *         
*        -------------------------                                    *         
*        R4, ON ENTRY, POINTS TO INVENTORY KEY                        *         
*        R6, ON ENTRY, POINTS TO INVENTORY 01 EL                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDRNEW   NMOD1 0,**HDRN**                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         CLI   MASOP6,C'Y'         IF SINGLE INV PER PAGE                       
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'          FORCE NEW PAGE                            
         B     HDRNEW1                                                          
*                                                                               
         MVC   P,SPACES            THEN PRINT                                   
         GOTO1 SPOOL,PARAS,(R8)    FORCE BLANK LINE                             
*                                                                               
         MVI   AVAILSW,0           INIT AVAIL PRINTED SWITCH                    
*                                                                               
HDRNEW1  DS    0H                                                               
*                                                                               
         USING RINVKEY,R4          ESTABLISH INVENTORY KEY                      
*                                                                               
         NI    PRNTOPT2,X'3F'      RESET BUFFER MORE THAN 1 PAGE BIT            
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         MVC   CONTINUE,SPACES                                                  
*                                                                               
         L     R2,SBUFF            INIT PRINT BUFFER                            
         ST    R2,ABUFF                                                         
*                                                                               
*        INVENTORY ID                                                           
*                                                                               
         CLI   NOREQSTA,1          SKIP IF MORE THAN ONE STATION                
         BH    HDNSTA10                                                         
*                                                                               
         MVC   P(L'RINVKINV),RINVKINV     PRINT INVENTORY ID                    
*                                                                               
         MVC   CONTINUE(4),P                                                    
         MVC   CONTINUE+5(11),=C'(CONTINUED)'                                   
*                                                                               
         LA    R3,P+5              STARTING PRINT POSITION                      
*                                                                               
         B     HDNSTAX                                                          
*                                                                               
HDNSTA10 DS    0H                                                               
*                                                                               
         LA    R5,SVSTCKD          ESTABLISH CURRENT STACK ENTRY                
         USING STCKD,R5                                                         
*                                                                               
         LA    R1,P                START OF INVENTORY NO. PRINT AREA            
*                                                                               
         CLC   SVINVPRT,RINVKINV   SKIP IF INV NO. UNCHANGED                    
         BE    HDNSTA15                                                         
*                                                                               
         MVC   SVINVPRT,RINVKINV   SAVE INV NO.                                 
*                                                                               
         MVC   0(L'RINVKINV,R1),RINVKINV     PRINT INVENTORY ID                 
         MVI   L'RINVKINV(R1),C'-'                                              
*                                                                               
HDNSTA15 DS    0H                                                               
*                                                                               
         LA    R1,L'RINVKINV+1(R1)     BUMP PRINT POINTER                       
*                                                                               
         MVC   0(4,R1),STCKSTAC    STATION CALL LETTERS                         
*                                                                               
         CLI   STCKSTAC+4,C' '     TV SKIPS BAND                                
         BE    *+8                                                              
         CLI   STCKSTAC+4,C'T'     TV SKIPS BAND                                
         BE    *+14                                                             
         MVI   4(R1),C'-'          PRINT BAND                                   
         MVC   5(1,R1),STCKSTAC+4                                               
*                                                                               
         MVC   CONTINUE(11),P                                                   
         MVC   CONTINUE+12(11),=C'(CONTINUED)'                                  
*                                                                               
         LA    R3,P+12             STARTING PRINT POSITION                      
*                                                                               
         DROP  R5                                                               
*                                                                               
HDNSTAX  DS    0H                                                               
*                                                                               
*                                                                               
*        MULTI HUT OPTION STATUS                                                
*                                                                               
HDNMHDT  DS    0H                                                               
                                                                                
         LA    R6,IO                                                            
         MVI   ELCODE,X'09'        MULTI HUT DAY/TIMES USED IF FOUND            
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   HDNMHEX                                                          
*                                                                               
         MVC   0(7,R3),=C'MH=ON, '                                              
         LA    R3,7(R3)                                                         
*                                                                               
HDNMHEX  DS    0H                                                               
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
*        USE AVAIL DAY/TIMES IF PRESENT IN INVENTORY RECORD                     
*                                                                               
HDNWAV   DS    0H                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'04'        AVAIL DAY/TIMES USED IF FOUND                
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   HDNWAVN             NONE FOUND                                   
*                                                                               
         MVI   AVAILSW,C'P'        INDICATE AVAILS PRINTED                      
*                                                                               
HDNWAVLP DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIAPELEM,R6         ESTABLISH AVAIL ELEMENT                      
*                                                                               
         MVC   WORK,SPACES         INIT WORKAREA                                
*                                                                               
         MVC   WORK(L'RIADAY),RIADAY   AVAIL DAYS                               
         MVC   WORK+L'RIADAY+1(L'RIATIME),RIATIME   AVAIL TIMES                 
*                                                                               
         OC    WORK,SPACES         MAKE UPPERCASE                               
*                                                                               
         LA    RF,L'RIADAY+L'RIATIME+1 TOTAL LENGTH                             
*                                                                               
         GOTO1 SQUASHER,PARAS,WORK,(RF)       SQUASH DAY/TIME                   
*                                                                               
         MVC   0(L'RIADAY+L'RIATIME+1,R3),WORK  PRINT AVAIL DAY/TIME            
*                                                                               
         LA    R0,L'RIADAY+L'RIATIME+1                                          
         LA    R3,L'RIADAY+L'RIATIME(R3) FIND LAST PRINTABLE CHARACTER          
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(R3),C','          SET SEPARATOR                                
         LA    R3,3(R3)            NEXT PRINT POSITION                          
*                                                                               
HDNWAVCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           NEXT AVAIL ELEMENT                           
         BE    HDNWAVLP            ONE FOUND                                    
*                                                                               
HDNWAVDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         CH    R2,=H'1'            IF MORE THAN ONE DAY-TIME ELEMENT            
         BNH   *+8                                                              
         MVI   PRNTDISP+1,0           USE ALTERNATE FORMAT                      
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,2(R3)            RESET END OF DATA POINTER                    
*                                                                               
         LR    R0,R3               CALCULATE DATA LENGTH                        
         LA    RE,P                                                             
         SR    R0,RE                                                            
*                                                                               
         LTR   RF,R0               COPY LENGTH                                  
         BZ    HDNWAVX             NO DATA TO CHOP                              
*                                                                               
         CLI   PRNTDISP+1,0        IF ALTERNATE FORMAT                          
         BE    HDNWAVX                SKIP CHOPPING                             
*                                                                               
         LA    R1,P-1(RF)          POINT TO LAST BYTE IN DATA                   
*                                                                               
         CLI   0(R1),C'-'          CONVERT HYPHENS TO X'FF' TO                  
         BNE   *+8                 STOP CHOPPER FROM SPLITTING ON THEM          
         MVI   0(R1),X'FF'                                                      
         BCTR  R1,0                                                             
         BCT   RF,*-14                                                          
*                                                                               
         L     R2,ABUFF            CURRENT BUFFER POSITION                      
*                                                                               
         GOTO1 CHOPPER,DMCB,((R0),P),(27,(R2)),(C'P',10)                        
*                                                                               
         L     RF,DMCB+8           NUMBER OF PRINT LINES                        
         LA    RF,0(RF)                                                         
         L     R1,ABUFF            START OF DAY/TIMES PRINT AREA                
*                                                                               
HDNWAV1L DS    0H                                                               
*                                                                               
         LA    R0,27               WIDTH OF PRINT AREA                          
         LR    RE,R1               START OF PRINT AREA TO CHECK                 
*                                                                               
HDNWAV2L DS    0H                                                               
*                                                                               
         CLI   0(RE),X'FF'         RESTORE HYPHENS                              
         BNE   *+8                                                              
         MVI   0(RE),C'-'                                                       
*                                                                               
HDNWAV2C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,HDNWAV2L                                                      
*                                                                               
HDNWAV2D DS    0H                                                               
*                                                                               
HDNWAV1C DS    0H                                                               
*                                                                               
         LA    R1,132(R1)          NEXT PRINT LINE                              
         BCT   RF,HDNWAV1L                                                      
*                                                                               
HDNWAV1D DS    0H                                                               
*                                                                               
         L     RF,DMCB+8           NUMBER OF LINES USED                         
         LA    RF,0(RF)            CLEAR HIGH ORDER BYTE                        
*                                                                               
         BAS   RE,BUFFADD          UPDATE BUFFER POINTER                        
         BCT   RF,*-4                                                           
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         LA    R3,P                POINT TO NEXT WORK PRINT POSITION            
*                                                                               
HDNWAVX  DS    0H                                                               
*                                                                               
         B     HDNWDTX                                                          
*                                                                               
HDNWAVN  DS    0H                                                               
*                                                                               
*        PRINT DAYS/TIMES                                                       
*                                                                               
HDNWDT   DS    0H                                                               
*                                                                               
         GOTO1 =A(PRNTDTM),RR=RELO                                              
         GOTO1 =A(PRNTMHDT),RR=RELO                                             
*                                                                               
HDNWDTX  DS    0H                                                               
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
*        IF NOT USING ALTERNATE FORMAT,                                         
*           USE IT ANYWAY IF THERE ARE 2 PROGRAM ELEMENTS                       
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
         LA    R2,1(R2)            BUMP ELEMENT COUNTER                         
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    *-8                 ONE FOUND                                    
*                                                                               
         CH    R2,=H'1'            IF MORE THAN ONE PROGRAM ELEMENT             
         BNH   *+8                                                              
         MVI   PRNTDISP+1,0           USE ALTERNATE FORMAT                      
*                                                                               
         CLI   PRNTDISP+1,0        IF ALTERNATE FORMAT                          
         BNE   HDNWPG10                                                         
*                                                                               
         LA    RF,P+22                PROGRAMS START ON/AFTER COL 23            
         CR    R3,RF                                                            
         BNL   *+6                                                              
         LR    R3,RF                                                            
*                                                                               
HDNWPG10 DS    0H                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
HDNWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RIPGNAME    PRINT PROGRAM NAME                           
*                                                                               
         LA    R3,0(RF,R3)         LAST PRINT POSITION                          
         LA    RF,1(RF)            RESTORE LENGTH COUNTER                       
*                                                                               
         CLI   0(R3),C' '          FIND LAST NON-BLANK                          
         BH    *+14                                                             
         BCTR  R3,0                BACK UP A BYTE                               
         BCT   RF,*-10                                                          
         B     HDNWPGCN            ALL BLANKS                                   
*                                                                               
         CLI   0(R3),C'-'          SKIP DELINEATOR IF END IS A HYPHEN           
         BE    *+12                                                             
         MVI   1(R3),C','          DATA SEPARATOR                               
         LA    R3,3(R3)            BUMP POINTER                                 
*                                                                               
HDNWPGCN DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    HDNWPGLP            ONE FOUND                                    
*                                                                               
HDNWPGDN DS    0H                                                               
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,1(R3)            RESET END OF DATA POINTER                    
*                                                                               
*        PRINT LOOK-UP DAYS/TIMES IF ASKED                                      
*                                                                               
HDNWLUP  DS    0H                                                               
*                                                                               
         CLI   MASOP7,C'Y'         CHECK FOR OPTION                             
         BNE   HDNWLUPX                                                         
*                                                                               
         CLI   AVAILSW,C'P'        SKIP IF AVAILS NOT PRINTED                   
         BNE   HDNWLUPX                                                         
*                                                                               
         MVC   0(9,R3),=C',LOOK-UP=' SET IDENTIFIER                             
         LA    R3,9(R3)            BUMP PRINT POSITION                          
*                                                                               
         GOTO1 =A(PRNTDTM),RR=RELO                                              
         GOTO1 =A(PRNTMHDT),RR=RELO                                             
*                                                                               
HDNWLUPX DS    0H                                                               
*                                                                               
         LR    R0,R3               CALCULATE DATA LENGTH                        
         LA    RE,P                                                             
         SR    R0,RE                                                            
*                                                                               
         LTR   R0,R0               COPY LENGTH                                  
         BZ    HDNWPGX             NO DATA TO MOVE/CHOP                         
*                                                                               
         CLI   PRNTDISP+1,0        IF NOT ALTERNATE FORMAT                      
         BNE   HDNWPG11               GO CHOP                                   
*                                                                               
*        PRINT DAY-TIMES/PROGRAMS ON 2 LINES                                    
*        CAN'T USE CHOPPER BECAUSE LENGTH CAN BE >256                           
*        KEEP PRINT TO FIRST 100 BYTES OF A LINE                                
*                                                                               
         L     R2,ABUFF            A(PRINT LINE)                                
*                                                                               
         MVC   0(5,R2),P           PRINT INVENTORY NUMBER PLUS SPACE            
         SH    R0,=H'5'            DECREMENT TOTAL LENGTH                       
*                                                                               
*        PRINT DAY-TIMES AND PROGRAM NAMES                                      
*                                                                               
         LA    R1,P+5              START OF DATA TO BE PRINTED                  
*                                                                               
HDNWPRLP DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          DON'T START WITH COMMA OR SPACE              
         BE    *+12                                                             
         CLI   0(R1),C' '          DON'T START WITH COMMA OR SPACE              
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         LA    RF,94(R1)           LAST OF DATA FOR A LINE                      
*                                                                               
         CLI   0(RF),C' '          SPLIT ON LAST SPACE OR COMMA                 
         BNH   *+16                                                             
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-16                                                          
*                                                                               
         LA    RF,1(RF)            INCLUDE SPACE OR COMMA                       
*                                                                               
         SR    RF,R1               LENGTH OF DATA TO MOVE                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R2),0(R1)       PRINT LINE                                   
*                                                                               
HDNWPRCN DS    0H                                                               
*                                                                               
         LA    R2,132(R2)          BUMP BUFFER POINTER                          
         LA    R1,1(RF,R1)         START OF NEXT PORTION                        
         SR    R0,RF               DECREMENT LENGTH COUNTER                     
         SH    R0,=H'1'            CONTINUE IF MORE DATA                        
         BP    HDNWPRLP                                                         
*                                                                               
HDNWPRDN DS    0H                                                               
*                                                                               
         ST    R2,ABUFF            UPDATE BUFFER POINTER                        
*                                                                               
         L     R3,ABUFF            NEXT AVAILABLE PRINT LINE                    
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         MVC   P3,SPACES           INIT WORKAREA                                
*                                                                               
         B     HDNWPGX                                                          
*                                                                               
HDNWPG11 DS    0H                                                               
*                                                                               
*                                                                               
         L     R2,ABUFF            CURRENT BUFFER POSITION                      
*                                                                               
         GOTO1 CHOPPER,DMCB,((R0),P),(28,(R2)),(C'P',10)                        
*                                                                               
         L     RF,DMCB+8           NUMBER OF PRINT LINES                        
         LA    RF,0(RF)                                                         
*                                                                               
         BAS   RE,BUFFADD          UPDATE BUFFER POINTER                        
         BCT   RF,*-4                                                           
*                                                                               
         L     R3,ABUFF            NEXT AVAILABLE PRINT LINE                    
*                                                                               
HDNWPGX  DS    0H                                                               
*        EFFECTIVE DATE(S)                                                      
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   HDNWEFX                                                          
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         MVC   WORK,SPACES                    EFFECTIVE DATES                   
*                                                                               
         CLI   PRNTDISP+1,0         IF ALTERNATE FORMAT                         
         BNE   HDNW12                                                           
*                                                                               
         L     R3,SBUFF                USE FIRST LINE IN BUFFER                 
         LA    R3,107(R3)                                                       
*                                                                               
HDNW12   DS    0H                                                               
*                                                                               
         CLC   RINVPEFF(2),RINVPEFF+2   CHECK FOR SINGLE DATE                   
         BNE   HDNW13                                                           
*                                                                               
         MVC   WORK(4),=C'ONLY'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
*                                                                               
         B     HDNW16                                                           
*                                                                               
HDNW13   DS    0H                                                               
*                                                                               
         MVC   WORK(4),=C'FROM'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HDNW16                                                           
*                                                                               
         MVC   WORK(8),WORK+5                                                   
         MVC   WORK+8(56),SPACES                                                
         MVI   WORK+8,C'-'                                                      
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF+2),(8,WORK+9)                           
*                                                                               
HDNW16   DS    0H                                                               
*                                                                               
         MVC   0(17,R3),WORK       EFFECTIVE DATE(S)                            
         LA    R3,17(R3)           NEXT PRINT POSITION                          
*                                                                               
         CLI   PRNTDISP+1,0        IF NOT ALTERNATE FORMAT                      
         BE    *+12                                                             
         L     R3,SBUFF               FIRST LINE OF BUFFER                      
         LA    R3,30(R3)                                                        
*                                                                               
         MVI   0(R3),C'-'                                                       
         MVC   1(6,R3),RINVDP      -DAYPART CODE(S)-                            
*                                                                               
         LA    R3,6(R3)            FIND END OF DAYPARTS                         
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         MVI   1(R3),C'-'                                                       
         LA    R3,2(R3)            NEXT PRINT POSITION                          
*                                                                               
HDNW20   DS    0H                                                               
*                                                                               
HDNWEFX  DS    0H                                                               
*                                                                               
         CLI   PRNTDISP+1,0        IF NOT ALTERNATE FORMAT                      
         BE    *+12                                                             
         L     R2,SBUFF               RESET TO START OF PRINT BUFFER            
         ST    R2,ABUFF                                                         
*                                                                               
HDRNEWX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER'                        
*              ------------------------------                                   
*              ONLY BUILD DEMO HEADLINES ONCE                                   
*              ------------------------------                                   
         SPACE 2                                                                
DHED     NMOD1 0,**DHED**                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         XC    SVH7,SVH7                                                        
         XC    SVH8,SVH8                                                        
         XC    SVH9,SVH9                                                        
*                                                                               
         LA    R3,SVH7+13          POINT TO WHERE 1ST DEMO SHOULD PRINT         
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
         LA    R5,SVH8+13                                                       
         AH    R5,PRNTDISP         ALTERNATE FORMAT                             
*                                                                               
         LA    R2,DEMLST                                                        
         ZIC   R6,NUMDEMS                                                       
*                                                                               
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
DHED40   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         GOTO1 DEMOCON,PARAS,(0,(R2)),(4,WORK),(0,DBLOCKD)                      
*                                                                               
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
         MVC   0(4,R3),WORK        FORMAT - .WM.1849                            
         MVC   0(4,R5),WORK+4                                                   
*                                                                               
         LA    R1,TYPTAB                                                        
*                                                                               
DHED50   CLC   0(1,R1),1(R2)                                                    
         BE    DHED60                                                           
*                                                                               
         LA    R1,5(R1)                                                         
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   DHED50                                                           
*                                                                               
DHED60   MVC   SVH9-SVH8(4,R5),1(R1)                                            
*                                                                               
         CLC   1(2,R2),=C'D702'    PUT=HUT FOR MET                              
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
         CLC   1(2,R2),=X'D703'    OR METB                                      
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
*                                                                               
         LA    R2,3(R2)                                                         
         LA    R3,5(R3)                                                         
         LA    R5,5(R5)                                                         
*                                                                               
         CLI   MASOP5,C'Y'         SKIP IF DOING ROUNDED DEMOS                  
         BE    DHED70                                                           
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
         CLI   NUMDEMS,17          SKIP IF DOING MORE THAN 17 DEMOS             
         BH    DHED70                                                           
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
DHED70   DS    0H                                                               
*                                                                               
         BCT   R6,DHED40                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
TYPTAB   DC    C'TTSA '            TSA IMPS                                     
         DC    C'RRTG '            ADI/DMA RTGS                                 
         DC    C'SSHR '            ADI/DMA SHRS                                 
         DC    C'PPUT '            ADI/DMA LVLS (PUTS)                          
         DC    C'CCHR '                                                         
         DC    C'XTSH '            TSA SHRS                                     
         DC    C'QTOT '            TSA LVLS (TOTS)                              
         DC    C'UUNV '            UNVS                                         
         DC    X'FF'                                                            
         DC    C'    '                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*              FORMAT BOOK SEQUENCE                                             
*              --------------------                                             
         SPACE 1                                                                
BOOKSQ   NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
*   TEST DATA FLOW                                                              
         MVC   P(07),=C'KEY IN:'                                                
         MVC   P+7(27),KEY                                                      
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
*   TEST DATA FLOW END                                                          
*&&                                                                             
         L     R3,ABUFF                                                         
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    R3,29(R3)                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        FIND QUALIFIER AND BOOKTYPE                                            
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,RINVKRSR    SET KSRC                                     
*                                                                               
*              ** BELOW ADDED FOR "NEW" GETKSRC **                              
         MVC   GSIQLF,RINVKQLF     SET QUALIFIER                                
         MVC   GSIBITS,RINVKQLF    SET BOOKVAL BITS                             
         MVC   GSIBKTYP,RINVKBTP   SET BOOK TYPE                                
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),(0,GSRCOUT),ACOMFACS                 
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   RNGX                                                             
*                                                                               
         CLI   GSOQLF,C' '         IF THERE IS A QUALIFIER                      
         BNH   *+14                                                             
         MVC   0(1,R3),GSOQLF         PRINT IT                                  
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,RINVKBK+1                                                     
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
*                                                                               
         CLI   RINVKBK+1,0             UNSPECIFIED MONTH                        
         BNE   BKSQ80                                                           
*                                                                               
         BCTR  R3,0                                                             
         MVC   0(3,R3),=C'EST'     E BECOMES EST                                
*                                                                               
BKSQ80   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVKBK        BOOK YEAR                                    
*                                                                               
         CH    RF,=H'100'          GET MODULO 100                               
         BL    *+8                                                              
         SH    RF,=H'100'                                                       
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SUGN                                   
*                                                                               
         UNPK  3(2,R3),DUB         PRINT YEAR                                   
*                                                                               
         CLI   RINVKBTP,0          IF THERE IS A BOOKTYPE                       
         BE    BKSQ85                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(RINVKBTP,0)                                       
         CLI   DMCB,0                                                           
         BE    BKSQ85                                                           
                                                                                
         MVI   5(R3),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),DMCB+2                                                   
                                                                                
         LA    R3,7(R1,R3)                                                      
         MVI   0(R3),C')'                                                       
*                                                                               
BKSQ85   DS    0H                                                               
*                                                                               
         L     R5,ABUFF                                                         
         LA    R5,11(R5)                                                        
*                                                                               
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    R5,29(R5)                                                        
*                                                                               
         CLI   MASOP4,C'S'         IF SUPPRESSING CODES                         
         BNE   BKSQ90                                                           
*                                                                               
         CLI   GSOQLF,C'P'            AND QUALIFIER IS P                        
         BE    *+8                                                              
         CLI   GSOQLF,C'E'            OR E                                      
         BE    BKSQ91                    SKIP PRINTING CODE                     
*                                                                               
BKSQ90   DS    0H                                                               
*                                                                               
         MVC   0(2,R5),SAVECODE       PRINT SAVECODE (FROM 'DATA' RTN)          
*                                                                               
BKSQ91   DS    0H                                                               
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES FOR HANDLING THE DEMO FORMATTING                        
*              -----------------------------------------                        
*      ON ENTRY, R5 POINTS TO OUTPUT FOR DEMO VALUES                            
         SPACE 2                                                                
DAT50    NMOD1 0,**DAT50*                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,DEMLST           DEMO LIST                                    
         TM    PRINTOPT,X'01'      RECALCULATED DEMOS                           
         BO    DAT55               ONLY NEED FORMATING                          
         SPACE 1                                                                
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    R6,IO                                                            
         ST    R6,DBAREC                                                        
         LA    R6,34(R6)                                                        
         ST    R6,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BE    DAT51A                                                           
*                                                                               
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
*                                                                               
         B     DAT51B                                                           
*                                                                               
DAT51A   EQU   *                                                                
*                                                                               
         MVI   DBXTTRP,X'00'                                                    
         MVI   DBXTTSP,X'00'                                                    
         MVI   DBXTTIP,X'03'                                                    
*                                                                               
DAT51B   EQU   *                                                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,(R5)                             
*                                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
DAT55    L     R3,ABUFF                                                         
         LA    R3,12(R3)           POINT TO PRINT LINE                          
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
*                                                                               
         ZIC   R4,NUMDEMS                                                       
*                                                                               
DAT60    L     R1,0(R5)                                                         
*                                                                               
         TM    PRINTOPT,X'01'      RECALCULATED DEMOS                           
         BNO   DAT65               ALWAYS NEED TO DROP DECIMAL                  
*                                                                               
         SR    R0,R0               THEN DROP DECIMAL                            
*                                                                               
         CLI   MASOP5,C'Y'         ROUNDED DEMOS?                               
         BE    DAT62               YES - DROP TWO DECIMAL POSITIONS             
*                                  NO  - ONLY DROP 1 DECIMAL                    
         LA    R1,5(R1)            HALF-ROUND ON 10                             
         D     R0,=F'10'           DIVIDE BY 10                                 
*                                                                               
         B     DAT64                                                            
*                                                                               
DAT62    EQU   *                                                                
*                                                                               
         LA    R1,50(R1)           HALF-ROUND ON 100                            
         D     R0,=F'100'          DIVIDE BY 100                                
*                                                                               
DAT64    EQU   *                                                                
*                                                                               
         ST    R1,0(R5)            STORE ROUNDED VALUE                          
*                                                                               
DAT65    EQU   *                                                                
*                                                                               
         LTR   R1,R1               IF NO VALUE                                  
         BNZ   DAT70                                                            
*                                                                               
         CLI   MASOP5,C'Y'         AND NOT ROUNDING                             
         BE    DAT70                                                            
*                                                                               
         CLI   NUMDEMS,17             IF MORE THAN 17 DEMOS                     
         BNH   *+12                                                             
         MVI   3(R3),C'.'                SHOW '.'                               
         B     DAT80                                                            
*                                                                               
         MVI   4(R3),C'.'                                                       
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT70    EQU   *                                                                
*                                                                               
         CLI   1(R2),C'U'         IF UNIVERSE NUMBER                            
         BNE   DAT71                                                            
         CLI   LINSW,C'S'               AND "SHARE" LINE                        
         BE    DAT100                   THEN SKIP                               
         CLI   LINSW,C'L'               OR  "LEVELS" LINE                       
         BE    DAT100                   THEN SKIP                               
         B     DAT77                    ELSE, NO DECIMAL(OR ROUNDING)           
*                                                                               
DAT71    CLI   MASOP5,C'Y'        ELSE,IF ROUNDING                              
         BE    DAT75                      BRANCH                                
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    DAT74                                                            
*                                                                               
DAT72    EQU   *                                                                
*                                                                               
         EDIT  (R1),(6,0(R3)),1                                                 
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT74    EQU   *                                                                
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         EDIT  (R1),(6,0(R3)),1                                                 
         LA    R3,1(R3)            RESTORE POINTER                              
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT75    EQU   *                                                                
*                                                                               
         EDIT  (R1),(4,1(R3)),0                                                 
         B     DAT80                                                            
*                                                                               
DAT77    EDIT  (R1),(6,0(R3)),0                                                 
*                                                                               
DAT80    EQU   *                                                                
*                                                                               
         CLC   1(2,R2),=X'D701'    IF HUT                                       
         BE    DAT90                                                            
         CLC   1(2,R2),=X'E201'    OR SHARE                                     
         BNE   DAT100                                                           
DAT90    CLI   MULTSW,C'Y'         AND IT'S A COMBO                             
         BNE   DAT100                                                           
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(4,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     DAT100                                                           
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    *+14                                                             
         MVC   0(6,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     DAT100                                                           
*                                                                               
         MVC   0(5,R3),=C' N   '   THEN DON'T SHOW VALUE                        
*                                                                               
DAT100   LA    R2,3(R2)            NEXT DEMO                                    
*                                                                               
         LA    R3,5(R3)            NEXT DEMO PRINTING AREA                      
*                                                                               
         CLI   MASOP5,C'Y'         SKIP IF ROUNDED DECIMALS                     
         BE    DAT110                                                           
*                                                                               
         LA    R3,1(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    *+8                    NO NEED FOR EXTRA ROOM                    
         LA    R3,1(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
DAT110   DS    0H                                                               
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,DAT60            LOOP FOR MORE DEMOS                          
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - GTDIFF'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        FORMAT PER CENT DIFFERENCE BETWEEN LAST YEAR AND THIS     *            
*                                                                  *            
*NTRY    R4 ==>  THIS YEAR'S DEMOS                                 *            
*        R5 ==>  LAST YEAR'S DEMOS                                 *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
GTDIFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
*                                                                               
         OC    0(L'ACTDEMOS,R5),0(R5) SKIP IF NO LAST YEAR'S DEMOS              
         BZ    GTDIFFX                                                          
*                                                                               
         OC    0(L'ACTDEMOS,R4),0(R4) SKIP IF NO THIS YEAR'S DEMOS              
         BZ    GTDIFFX                                                          
*                                                                               
         LA    R2,DEMLST           POINT TO DEMO LIST                           
*                                                                               
         L     R3,ABUFF                                                         
         LA    R3,12(R3)           POINT TO PRINT LINE                          
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
*                                                                               
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
*                                                                               
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    RF,34(RE)                                                        
*                                                                               
         MVC   0(5,RF),=C'%DIFF'                                                
*                                                                               
         ZIC   RA,NUMDEMS          NUMBER OF DEMOS IN LIST                      
*                                                                               
GTDLOOP  DS    0H                                                               
*                                                                               
         L     RF,0(R4)            THIS YEAR'S DEMO                             
         S     RF,0(R5)            CALCULATE DIFFERENCE IN PERFORMANCE          
*                                                                               
         LA    RE,1000             SCALING FOR 2 DECIMALS IN PCT                
         CLI   MASOP5,C'Y'         IF ROUNDING TO WHOLE PER CENTS               
         BNE   *+8                                                              
         LA    RE,100                 SCALE UP FOR 0 DECIMALS IN PCT            
*                                                                               
         MR    RE,RE               SCALE UP                                     
*                                                                               
         ICM   R1,15,0(R5)         BASE DEMO                                    
         BZ    GTDCONT                                                          
*                                                                               
         DR    RE,R1               PER CENT DIFFERENCE                          
*                                                                               
         LPR   RE,RE               ABSOLUTE VALUE OF REMAINDER                  
         SLL   RE,1                DOUBLE REMAINDER                             
         CR    RE,R1               CHECK FOR ROUNDING                           
         BNH   *+18                                                             
         AH    RF,=H'1'            ROUND UP                                     
         LTR   RF,RF               IF NEGATIVE NUMBER                           
         BP    *+8                                                              
         BCTR  RF,0                   ROUND DOWN                                
         BCTR  RF,0                                                             
*                                                                               
         LR    R6,RF                                                            
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BE    GTD75                                                            
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    GTD74                                                            
*                                                                               
GTD72    EQU   *                                                                
*                                                                               
         EDIT  (R6),(6,0(R3)),1,FLOAT=-                                         
*                                                                               
         B     GTD80                                                            
*                                                                               
GTD74    EQU   *                                                                
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         EDIT  (R6),(6,0(R3)),1,FLOAT=-                                         
         LA    R3,1(R3)            RESTORE POINTER                              
*                                                                               
         B     GTD80                                                            
*                                                                               
GTD75    EQU   *                                                                
*                                                                               
         EDIT  (R6),(5,0(R3)),0,FLOAT=-                                         
*                                                                               
GTD80    EQU   *                                                                
*                                                                               
         CLC   1(2,R2),=X'D701'    IF HUT                                       
         BE    *+10                                                             
         CLC   1(2,R2),=X'E201'    OR SHARE                                     
         BNE   GTD100                                                           
*                                                                               
         CLI   MULTSW,C'Y'         AND IT'S A COMBO                             
         BNE   GTD100                                                           
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(4,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     GTD100                                                           
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    *+14                                                             
         MVC   0(6,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     GTD100                                                           
*                                                                               
         MVC   0(5,R3),=C' N   '   THEN DON'T SHOW VALUE                        
*                                                                               
GTD100   DS    0H                                                               
*                                                                               
GTDCONT  DS    0H                                                               
*                                                                               
         LA    R2,3(R2)            NEXT DEMO                                    
*                                                                               
         LA    R3,5(R3)            NEXT DEMO PRINTING AREA                      
*                                                                               
         CLI   MASOP5,C'Y'         SKIP IF ROUNDED DECIMALS                     
         BE    GTD110                                                           
*                                                                               
         LA    R3,1(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
         CLI   NUMDEMS,17                                                       
         BH    *+8                    NO NEED FOR EXTRA ROOM                    
         LA    R3,1(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
GTD110   DS    0H                                                               
*                                                                               
         LA    R4,4(R4)            NEXT DEMO VALUE                              
         LA    R5,4(R5)            NEXT DEMO VALUE                              
*                                                                               
         BCT   RA,GTDLOOP          LOOP FOR MORE DEMOS                          
*                                                                               
         GOTO1 =A(PERCENT),RR=RELO ADD IN % SIGNS                               
*                                                                               
GTDIFFX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER - VALSTA'               
********************************************************************            
*                                                                  *            
*     RERES0C (T8190C) --- INVENTORY MASTER                        *            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VALSTA   NTR1  BASE=*,LABEL=*                                                   
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
*                                                                               
         XC    STMENU,STMENU       INIT STATION MENU CODE                       
         XC    STMENUNM,STMENUNM   INIT STATION MENU NAME                       
*                                                                               
         L     R5,ASTLIST          ESTABLISH STATION LIST                       
         USING STLISTD,R5                                                       
         XC    STLISTD(STLISTL),STLISTD   INIT FIRST ENTRY IN LIST              
*                                                                               
         LA    R2,MASSTNH          POINT TO STATION INPUT FIELD                 
*                                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(16,SBUFF),0,0 SCAN INPUT                      
*                                                                               
         MVC   ACTUAL,DMCB+4       SAVE NUMBER OF ENTRIES                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ACTUAL           NUMBER OF ENTRIES IN FIELD                   
*                                                                               
         L     R4,SBUFF            START OF SCAN BLOCK ENTRIES                  
*                                                                               
*        IF ENTRY STARTS WITH '*', MUST BE A MENU ID                            
*                                                                               
         CLI   12(R4),C'*'         MENU INDICATED                               
         BNE   VSTAMN20                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),5             MENU ID MAX 4 LONG (ID PLUS *)               
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,13(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     VSTAMN50                                                         
*                                                                               
VSTAMN20 DS    0H                                                               
*                                                                               
*        IF ENTRY IS 'M' THEN MUST BE A MENU ID                                 
*                                                                               
         CLI   0(R4),1             IF ENTRY IS 1 LONG                           
         BNE   VSTAMNUN                                                         
         CLI   12(R4),C'M'         AND MENU INDICATED                           
         BNE   VSTAMNUN                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),4             MENU ID MAX 4 LONG                           
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,22(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
VSTAMN50 DS    0H                                                               
*                                                                               
*        READ MARKET STATIONS LIST                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R3                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,REPPAR     SET REP CODE                                 
         MVC   RSETKSET,=C'MS'     SET RECORD ID                                
         MVC   RSETKID,STMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   VSTAMNNF                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETDESD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETDCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMN10               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSETDCDE,RSETDCDQ   LOOKING FOR DESCRIPTIVE ELEMENT              
         BE    *+16                                                             
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETDOV)      DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMN10            IGNORE IF NOT THERE                          
*                                                                               
         MVC   STMENUNM,SPACES     INIT DESCRIPTION                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STMENUNM(0),RSETDESC SAVE MENU NAME                              
*                                                                               
VSTAMN10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETMCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMNVD               MUST FIND ELEMENT                         
         CLI   RSETMCDE,RSETMCDQ   LOOKING FOR MEMBERS ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMNVD            MUST HAVE SOME MEMBERS                       
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'5'            CALCULATE NUMBER OF STATIONS IN LIST         
*                                                                               
         STC   RF,NOREQSTA         SET NUMBER OF STATIONS                       
         SR    R0,R0               INIT STATION SORT ORDER                      
*                                                                               
         LA    R1,RSETMEMB         POINT TO FIRST STATION IN LIST               
*                                                                               
VSTAMNLP DS    0H                                                               
*                                                                               
         MVC   STLSSTAC,0(R1)      SAVE STATION CALL LETTERS                    
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTAMNCN DS    0H                                                               
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT STATION                         
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RF,VSTAMNLP                                                      
*                                                                               
VSTAMNDN DS    0H                                                               
*                                                                               
         B     VSTASTAX            END OF MENU LIST                             
*                                                                               
VSTAMNUN DS    0H                                                               
*                                                                               
*        BUILD LIST OF INDIVIDUALLY ENTERED STATIONS                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ACTUAL           NUMBER OF ENTRIES                            
         STC   RE,NOREQSTA         NUMBER OF REQUESTED STATIONS                 
*                                                                               
         SR    R0,R0               INIT STATION SORT ORDER                      
         SR    RF,RF                                                            
*                                                                               
VSTASTLP DS    0H                                                               
*                                                                               
         OC    STLSSTAC,SPACES     INIT STATION CALL LETTERS                    
         ICM   RF,1,0(R4)          ENTRY LENGTH                                 
         BZ    VSTASTNE            ENTRY REQUIRED                               
*                                                                               
         LA    R3,12-1(RF,R4)      POINT TO LAST OF STATION ID                  
*                                                                               
         CLI   0(R3),C'-'          FIND '-'                                     
         BE    *+18                                                             
         BCTR  R3,0                BACK UP A CHARACTER                          
         BCT   RF,*-10                                                          
         IC    RF,0(R4)            USE FULL ID LENGTH                           
         B     *+6                                                              
*                                                                               
         BCTR  RF,0                RECTIFY CALL LETTERS LENGTH                  
*                                                                               
         CH    RF,=H'4'            MAX 4 CHARACTERS FOR CALL LETTERS            
         BH    VSTASTXE                                                         
*                                                                               
         MVC   STLSSTAC,SPACES     INIT CALL LETTERS                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STLSSTAC(0),12(R4)  SAVE STATION CALL LETTERS                    
*                                                                               
         MVI   STLSSTAC+4,C'T'     ASSUME TV                                    
*                                                                               
         CLI   0(R3),C'-'          IF THERE IS A BAND ENTERED                   
         BNE   *+10                                                             
         MVC   STLSSTAC+4(1),1(R3)    ADD IT TO CALL LETTERS                    
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTASTCN DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           POINT TO NEXT STATION                        
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RE,VSTASTLP                                                      
*                                                                               
VSTASTDN DS    0H                                                               
*                                                                               
VSTASTAX DS    0H                                                               
*                                                                               
*        VALIDATE STATIONS IN LIST                                              
*                                                                               
         L     R5,ASTLIST          LIST OF STATIONS TO BE VALIDATED             
         XC    MKTSV(20),MKTSV     INIT MARKET NAME                             
*                                                                               
VSTAVALL DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    VSTAVALD                                                         
*                                                                               
*        READ STATION FILE TO VALIDATE STATION                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION RECORD KEY                 
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      RECORD TYPE                                  
         MVC   RSTAKREP,AGENCY     REP ID                                       
         MVC   RSTAKSTA,STLSSTAC   STATION                                      
*                                                                               
         CLI   RSTAKSTA+4,C'T'     MEDIA IS BLANK FOR ANY TV STATION            
         BE    VSTASTE5                                                         
         CLI   RSTAKSTA+4,X'F0'    IE. MEDIA= T,1-9                             
         BL    VSTASTE9                                                         
         CLI   RSTAKSTA+4,X'F9'                                                 
         BH    VSTASTE9                                                         
*                                                                               
VSTASTE5 DS    0H                                                               
*                                                                               
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
VSTASTE9 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FOR STATION POINTER                     
*                                                                               
         CLC   RSTAKEY,KEYSAVE     MUST FIND STATION                            
         BNE   VSTASTNV                                                         
*                                                                               
         OC    MKTSV(20),MKTSV     SKIP IF NOT FIRST STATION                    
         BNZ   VSTAMKTX                                                         
*                                                                               
         GOTO1 GETREC              READ IN STATION RECORD                       
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSTAELEM-RSTAREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSTAELEM,R6         ESTABLISH STATION ELEMENT                    
*                                                                               
         CLI   RSTACODE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMKTX               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSTACODE,X'01'      LOOKING FOR STATION ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSTAELLN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         MVC   MKTSV(20),2(R6)     SAVE MARKET NAME                             
*                                                                               
VSTAMKTX DS    0H                                                               
*                                                                               
VSTAVALC DS    0H                                                               
*                                                                               
         LA    R5,STLISTL(R5)      NEXT STATION IN LIST                         
         B     VSTAVALL                                                         
*                                                                               
VSTAVALD DS    0H                                                               
*                                                                               
         B     VALSTAX                                                          
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VSTASTNV DS    0H                  STATION NOT ON FILE                          
*                                                                               
         MVC   CONHEAD(L'VSTASTNM),VSTASTNM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTNM DC    C'** ERROR ** STATION NOT ON FILE'                               
*                                                                               
VSTASTXE DS    0H                  STATION ID TOO LONG                          
*                                                                               
         MVC   CONHEAD(L'VSTASTXM),VSTASTXM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTXM DC    C'** ERROR ** STATION CALL LETTERS ARE 3-4 LONG'                 
*                                                                               
VSTASTNE DS    0H                  STATION ENTRY NEEDED                         
*                                                                               
         MVC   CONHEAD(L'VSTASTEM),VSTASTEM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTEM DC    C'** ERROR ** STATION CALL LETTERS REQUIRED'                     
*                                                                               
VSTAMNVD DS    0H                  EMPTY MENU                                   
*                                                                               
         MVC   CONHEAD(L'VSTAMNVM),VSTAMNVM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNVM DC    C'** ERROR ** NO STATIONS IN MENU'                               
*                                                                               
VSTAMNNF DS    0H                  MENU NOT FOUND                               
*                                                                               
         MVC   CONHEAD(L'VSTAMNFM),VSTAMNFM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNFM DC    C'** ERROR ** MENU RECORD NOT ON FILE'                           
*                                                                               
VSTAMNXE DS    0H                  MENU ID MAX 4 LONG                           
*                                                                               
         MVC   CONHEAD(L'VSTAMNXM),VSTAMNXM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNXM DC    C'** ERROR ** MENU ID MUST BE 1-4 CHARACTERS'                    
*                                                                               
VSTAMNNE DS    0H                  NO MENU ID                                   
*                                                                               
         MVC   CONHEAD(L'VSTAMNNM),VSTAMNNM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMNNM DC    C'** ERROR ** MENU ID MUST BE SUPPLIED'                          
*                                                                               
VSTAMN1E DS    0H                  AT MOST ONE MENU                             
*                                                                               
         MVC   CONHEAD(L'VSTAMAXM),VSTAMAXM                                     
*                                                                               
         B     VSTAERR1                                                         
*                                                                               
VSTAMAXM DC    C'** ERROR ** USE MENU ID OR STATION LIST.'                      
*                                                                               
VSTAERR  DS    0H                                                               
*                                                                               
         OC    STMENU,STMENU       SKIP IF VALIDATING A MENU                    
         BNZ   VSTAERR1                                                         
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         L     RF,ASTLIST          START OF STATIONS                            
         SR    R5,RF                                                            
         LR    RF,R5                                                            
         SR    RE,RE                                                            
         D     RE,=A(STLISTL)      RELATIVE NUMBER OF CURRENT STATION           
*                                                                               
         LA    RF,1(RF)            ABSOLUTE NUMBER                              
         STC   RF,FADDR            SET ITEM NUMBER                              
*                                                                               
         MVI   ERROR,SUPPLIED      ERROR MESSAGE SUPPLIED                       
*                                                                               
         GOTO1 VMYCURS                                                          
*                                                                               
VSTAERR1 DS    0H                                                               
*                                                                               
         MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VERRXIT                                                          
*                                                                               
SRCTBL   DC    C'NSA'              ARB, NSI, SRC                                
         DC    X'FF'                                                            
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
***********************************************************************         
*                                                                               
*   CHKDMA  ROUTINE - CHECKS IF ANY DMA IMPS REQUESTED                          
*                                          SETS CONDITION CODE                  
*                                                                               
***********************************************************************         
CHKDMA   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,DEMLST                                                        
CHKA10   CLI   0(R1),X'FF'                                                      
         BE    CHKANO                                                           
         CLI   1(R1),C'A'     DMA IMPRESSIONS?                                  
         BE    CHKAYES                                                          
         LA    R1,3(R1)                                                         
         B     CHKA10                                                           
                                                                                
CHKAYES  SR    R1,R1          SET CC TO =                                       
CHKANO   LTR   R1,R1          SET CC TO !=                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*                                                                               
*   CHKUNIV ROUTINE - CHECKS IF ANY UNIVERSE DEMOS REQUESTED                    
*                                            SET CONDITION CODE                 
*                                                                               
***********************************************************************         
CHKUNIV  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,DEMLST                                                        
CHKU10   CLI   0(R1),X'FF'                                                      
         BE    CHKUNO                                                           
         CLI   1(R1),C'U'                                                       
         BE    CHKUYES                                                          
         LA    R1,3(R1)                                                         
         B     CHKU10                                                           
                                                                                
CHKUYES  SR    R1,R1          SET CC TO =                                       
CHKUNO   LTR   R1,R1          SET CC TO !=                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*  NEWERRS ROUTINE - ALLOWS FOR OUTPUT OF NEW ERROR MSGS WITHOUT                
*                    HOGGING ADDRESSABILITY IN MAIN ROUTINE                     
*                    SHOULD NEVER RETURN TO CALLING ROUTINE!!!!                 
*                    R1 MUST CONTAIN ERROR EQUATE                               
**********************************************************************          
NEWERRS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    CONHEAD,CONHEAD    ENSURE CLEAR FOR OUTPUT                       
                                                                                
         CHI   R1,UNIVERR1                                                      
         BNE   NEWERR10                                                         
         MVC   CONHEAD(L'UDEMLIM),UDEMLIM                                       
         B     NEWERRX                                                          
                                                                                
NEWERR10 CHI   R1,UNIVERR2                                                      
         BNE   NEWERR20                                                         
         MVC   CONHEAD(L'UROUND),UROUND                                         
         B     NEWERRX                                                          
                                                                                
                                                                                
NEWERR20 CHI   R1,DMAERROR                                                      
         BNE   NEWERR30                                                         
         MVC   CONHEAD(L'DMAREQ),DMAREQ                                         
         B     NEWERRX                                                          
                                                                                
NEWERR30 DC    H'0'               INVALID ERROR EQUATE IN R1                    
NEWERRX  MVI   ERROR,X'FE'        DISPLAYING MY OWN MESSAGE                     
         GOTO1 VERRXIT                                                          
                                                                                
         DC    H'0'               SHOULD NEVER GET HERE                         
         EJECT                                                                  
                                                                                
*        NEW ERROR MESSAGES                                                     
UDEMLIM  DC    C'DEMO REQUEST LIMIT IS 17 IF UNIVERSES INCLUDED'                
UROUND   DC    C'UNIVERSES NOT COMPATIBLE WITH ROUNDED DEMOS OPTION'            
DMAREQ   DC    C'DMA IMPRESSIONS NOT SUPPORTED HERE'                            
                                                                                
*        NEW ERROR EQUATES                                                      
UNIVERR1 EQU   1                                                                
UNIVERR2 EQU   2                                                                
DMAERROR EQU   3                                                                
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*  OVFLAG ROUTINE - CHECKS FOR OVERRIDDEN DEMOS AND PRINTS CMMNT IF             
*                   FOUND                                                       
***********************************************************************         
OVFLAG   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'DE'                                                     
         BRAS  RE,GETEL                                                         
         BNE   OVFLAGX                                                          
         L     R2,ABUFF                                                         
         MVC   30(31,R2),=C'*BOOK CONTAINS OVERRIDDEN DEMOS'                    
         LA    R2,132(R2)                                                       
         ST    R2,ABUFF                                                         
*                                                                               
OVFLAGX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESFCD                                                       
         SPACE 1                                                                
* SAVE AREA FIELDS FOLLOWING SCREEN                                             
*                                                                               
SVH7     DS    CL132               SAVE DEMO HEADLINES (H7)                     
SVH8     DS    CL132               SAVE DEMO HEADLINES (H8)                     
SVH9     DS    CL132               SAVE DEMO HEADLINES (H9)                     
*                                                                               
HD1      DS    CL40                INV   DAY TIME   DAYPART                     
HD2      DS    CL40                PROGRAM#1                                    
HD3      DS    CL40                PROGRAM#2                                    
HD4      DS    CL40                PROGRAM#3                                    
HD5      DS    CL40                EFFECTIVE                                    
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-TTLDATAD'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR DATA PASSED TO TITLES SCREEN                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TTLDATAD DSECT                                                                  
TTLSTNL  DS    XL1          STATION LENGTH                                      
TTLSTN   DS    CL65         STATION                                             
TTLDPTL  DS    XL1          DAYPART LENGTH                                      
TTLDPT   DS    CL19         DAYPART                                             
TTLESDL  DS    XL1          EFFECTIVE START DATE LENGTH                         
TTLESD   DS    CL9          EFFECTIVE START DATE                                
TTLEEDL  DS    XL1          EFFECTIVE END DATE LENGTH                           
TTLEED   DS    CL9          EFFECTIVE END   DATE                                
TTLFTRL  DS    XL1          FILTERS LENGTH                                      
TTLFTR   DS    CL7          FILTERS                                             
TTLOP3L  DS    XL1          NO DUPLICATES OPTION LENGTH                         
TTLOP3   DS    CL2          NO DUPLICATES OPTION                                
*                                                                               
                                                                                
TTLDATAL EQU   *-TTLDATAD          LENGTH OF PASSED DATA                        
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-SYSSPARE'               
***********************************************************************         
*                                                                     *         
*        SAVE AREA VALUES                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
SAVESTAK DS    2F                                                               
*                                                                               
PRNTDISP DS    H                   ALTERNATE FORMAT (18 DEMO MAX.)              
*                                   PRINT LINE DISPLACEMENT                     
HDLINE   DS    XL1                 CURRENT LINE (HD1-HD5)                       
*                                                                               
SBK      DS    CL4                 START BOOK IN RANGE                          
EBK      DS    CL4                 END BOOK IN RANGE                            
*                                                                               
DPLIST   DS    CL25                DAYPART LIST                                 
*                                                                               
PRINTOPT DS    XL1                 PRINTING OPTIONS                             
POPTSTXQ EQU   X'80'               X'80'  SUPPRESS TEXT                         
POPTSHRQ EQU   X'40'               X'40'  PRINT SHARES AND H/P/T                
POPT1DPQ EQU   X'20'               X'20'  1 DPT/DAY/INV                         
POPTRBKQ EQU   X'10'               X'10'  RANGE OF BOOKS                        
POPTFLTQ EQU   X'08'               X'08'  FILTERS SPECIFIED                     
POPTPTFQ EQU   X'04'               X'04'  TEXT FILTERS TO PRINT                 
POPTUPGQ EQU   X'02'               X'02'  UPGRADE EXISTS                        
POPTRDMQ EQU   X'01'               X'01'  RECALCULATE DEMOS                     
*                                                                               
PRNTOPT2 DS    XL1                 X'80'  BUFFER MORE THAN 1 PAGE (P1)          
*                                  X'40'  BUFFER MORE THAN 1 PAGE (P2)          
DAYOPT   DS    CL1                                                              
*                                                                               
ESDT     DS    XL2                 EFFECTIVE START DATE (COMPRESSED)            
EEDT     DS    XL2                 EFFECTIVE END DATE (COMPRESSED)              
*                                                                               
ADT      DS    CL3                 ACTIVITY DATE (YMD BINARY)                   
SVWRPOPT DS    CL1                 'Y' - WRAP TEXT LINES                        
SVINVPRT DS    CL4                 LAST PRINTED INV NO.                         
SVSTAPRT DS    CL5                 LAST PRINTED STATION FOR TEXT                
INVNO    DS    CL4                 INVENTORY FILTER                             
CSET     DS    CL1                 NOT USED                                     
SAVEKSRC DS    CL1                 SAVE RINVKSRC                                
XTODAY   DS    XL2                 TODAY'S DATE (COMPRESSED)                    
SAVECODE DS    CL2                 RINVCODE SAVED FOR PRINTING                  
CONTINUE DS    CL23                INV# CONTINUATION LEGEND                     
NUMFILT  DS    XL1                 NUMBER OF FILTER CODES                       
*                                                                               
HDRSW    DS    CL1                 HDR SWITCH                                   
LINSW    DS    CL1                 LINE SWITCH                                  
MULTSW   DS    CL1                 COMBO SWITCH                                 
CONTSW   DS    CL1                 PRINT CONTINUED MESSAGE IN HOOK SW           
*                                                                               
DEMLST   DS    CL73                DEMO LIST...ALLOW 24 DEMOS + X'FF'           
SVDEMLST DS    CL73                DEMO LIST...SAVE AREA                        
*                                                                               
REPPAR   DS    CL2                 PARENT REP                                   
SBUFF    DS    A                   START OF BUFFER                              
ABUFF    DS    A                   POINTER TO NEXT LINE IN BUFF                 
XBUFF    DS    A                   END OF 1 FULL PAGE IN BUFF                   
AUPOUT   DS    A                   REUPOUT                                      
SVLST    DS    CL10                VALID ARB, NSI OR SRC SOURCE CODES           
INVSRC   DS    XL1                 1ST BYTE FROM BOOKVAL                        
         DS    0F                                                               
ACTDEMOS DS    XL96                24 DEMO VALUES (4 BYTES EACH)                
SVDFRDEM DS    XL96                24 DEMO VALUES (4 BYTES EACH)                
SVDTOBK  DS    XL4                 BOOK FOR DIFF TO   BOOK                      
SVDTOBKA DS    XL4                 A(BOOK FOR DIFF TO   BOOK)                   
SVDFRBK  DS    XL4                 BOOK FOR DIFF FROM BOOK                      
*                                                                               
MAXPLINS DS    XL1                 MAX NO. PRINT LINES (NOT HEADS)              
NOREQDPT DS    XL1                 # OF REQUESTED DAYPARTS                      
NOREQSTA DS    XL1                 # OF REQUESTED STATIONS                      
SKIPREC  DS    XL1                 FLAG TO SKIP RECORD OR NOT                   
OPTFLAG  DS    XL1                 FLAG INDICATE Y/N TO OPTION 3                
AVAILSW  DS    CL1                 C'P' - AVAIL DAY/TIMES WERE PRINTED          
*                                                                               
SVSTCKD  DS    XL(STCKL)           CURRENT STACK ENTRY                          
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
DPMENU   DS    CL4                 DAYPART MENU CODE                            
DPMENUNM DS    CL20                DAYPART MENU NAME                            
*                                                                               
STAMP    DS    CL8                 WORKING STORAGE STAMP                        
*                                                                               
AUPELEM  DS    A                   A(UPGRADE ELEMENT)                           
*                                                                               
SHRSRC   DS    CL1                 RATING SERVICE OF SHARE BOOK                 
SHRBK    DS    XL2                 SHARE BOOK                                   
SHRBTYP  DS    CL1                 SHARE BOOK BOOKTYPE                          
*                                                                               
INVMAX2  DS    XL1                                                              
*                                                                               
SAVELN   EQU   *-SYSSPARE                                                       
*                                                                               
MYBASE   DS    A                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
ASTACK   DS    A                                                                
ASTLIST  DS    A                   A(STLIST)                                    
ASTACKX  DS    A                   A(END OF STACK)                              
STACKLN  DS    F                   STACK LENGTH IF OFF-LINE                     
*                                                                               
ATXTWRK  DS    A                   A(TXTWRK)                                    
*                                                                               
SAVSCNNM DS    XL1                 NUMBER OF ITEMS FOUND BY SCANNER             
*                                                                               
MYKEY    DS    XL(L'KEY)           KEY SAVEAREA                                 
MYFILE   DS    XL(L'DMFILE)        FILE SAVEAREA                                
*                                                                               
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                                                              
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                                                              
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
RNGLST   DS    CL256               LIST OF BOOKS WITHIN RANGE                   
RNGLST2  DS    CL256                  9 BYTES X 56 BOOKS MAX                    
*                                        1 BYTE  BOOKVAL BITS                   
*                                        2 BYTES RINVKBK                        
*                                        1 BYTE  BOOK TYPE                      
*                                        1 BYTE  RINVKRSR                       
*                                        4 BYTES DISK ADDRESS                   
RNGLSTND DS    CL1                 END OF TABLE                                 
*                                                                               
DPTBL    DS    XL(25*DPTBLL)       DAYPART TABLE                                
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
INVLIST  DS    XL(31*8)            INVENTORY NUMBER LIST                        
*                                                                               
TXTWRK   DS    XL(32*61)           TEXT WORKAREA                                
*                                                                               
STAMKT#  DS    XL2                 STATION NSI MARKET NUMBER                    
*                                                                               
         DS    XL(BUFF-*)          SPARE PROGRAM WORKAREA                       
*                                                                               
         DS    0D                                                               
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-DPTBL'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR RANGE LIST ENTRY                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RLTBLD   DSECT                                                                  
RLTBBKBT DS    XL1                 1 BYTE  BOOKVAL BITS                         
RLTBBOOK DS    CL2                 2 BYTES RINVKBK                              
RLTBBKTP DS    CL1                 1 BYTE  BOOK TYPE                            
RLTBSRCE DS    CL1                 1 BYTE  RATING SOURCE                        
RLTBDA   DS    XL4                 4 BYTES DISK ADDRESS                         
RLTBLQ   EQU   *-RLTBLD                                                         
RLTBMAXQ EQU   56                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-STACKD'                 
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF INVENTORY DISK ADDRESSES                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STCKD    DSECT                                                                  
STCKDAY  DS    CL1                 DAY CODE                                     
STCKDTE  DS    XL2                 EFFECTIVE DATE COMPRESSED                    
STCKQTR  DS    CL1                 QUARTER HOUR                                 
STCKLEN  DS    CL1                 LENGTH                                       
STCKINV  DS    CL4                 INVENTORY NUMBER                             
STCKSTD  DS    CL3                 INVENTORY NUMBER START DATE                  
STCKSTCD DS    CL1                 STATION SORT CODE                            
STCKSTAC DS    CL5                 STATION CALL LETTERS                         
STCKDA   DS    CL4                 INVENTORY RECORD DISK ADDRESS                
STCKL    EQU   *-STCKD             LENGTH OF TABLE ENTRY                        
*                                                                               
STCKMAXQ EQU   700                 MAXIMUM NUMBER OF ENTRIES IN STACK           
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-STLISTD'                
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190C --- RERES0C --- INVENTORY MASTER-INCLUDES'               
***********************************************************************         
*                                                                     *         
*        OTHER INCLUDED BOOKS                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RESETRECD      DSECT                                                            
*REGENSET                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
RESTARECD      DSECT                                                            
*REGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
RERDPRECD      DSECT                                                            
*REGENRDP                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENRDP                                                       
         PRINT ON                                                               
*RERMPPROF                                                                      
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
*REDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE REDDEQUS                                                       
         PRINT ON                                                               
*GERFPIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
*DDGLVXCTLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RERES0C   12/10/12'                                      
         END                                                                    
