*          DATA SET RELFM13    AT LEVEL 069 AS OF 05/01/02                      
*PHASE T80413A,*                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE RIGHT                                                                  
         TITLE 'T80413 - RELFM13 - BUDGET/CON FIX/EOM/DEMO MENU RECS'           
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM13 - T80413 - REP FILE PROGRAM                        *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*  MOD LOG                                                          *           
*  -------                                                          *           
*  08/24/89  PJS  CHANGED PHASE CARD TO 'A' LEVEL                   *           
*                                                                   *           
*  11/21/89  PJS  CHANGES FOR NEW BUDGET RECORD.                    *           
*                 1) NO MORE 'INTERNAL' COLUMN                      *           
*                 2) ADDED TOTAL ALLOCATION AND STATUS FIELDS       *           
*                                                                   *           
*  12/11/89  PJS  CHANGE FIX TO USE E1 SCREEN INSTEAD OF SHARING    *           
*                 BUDGET SCREEN (E7)                                *           
*                                                                   *           
*  FEB02/90 (MRR) --- REMOVE EOM RECORD PREVIOUS/NEXT YEAR TESTS    *           
*                     FOR DDS TERMINALS                             *           
*                                                                   *           
*  AUG24/90 (BU)  --- ADD CONTRACT-TYPE BUDGET DIFFERENTIATION TO   *           
*                     BUDGET RECORDS.                               *           
*                                                                   *           
*  OCT16/90 (BU)  --- BUDGET ALLOCATION MULTI-TYPE DISPLAY SCREEN   *           
*                                                                   *           
*  NOV19/90 (BU)  --- PRODUCE TOTALS BY CORPORATE FOR BUDGET ALLOC  *           
*                     DISPLAY SCREEN                                *           
*                                                                   *           
*  MAR21/91 (BU)  --- FIX BUG IN TOTALS BY CORPORATE                *           
*                                                                   *           
*  MAR16/92 (SKU) --- FIX BUG TO SHOW NEG AMTS IN FIX               *           
*                                                                   *           
*  APR02/92 (BU ) --- FIX DISPLAY OF CORPORATE BUD/ALLOC            *           
*                                                                   *           
*  SEP01/92 (BU ) --- COLUMN TOTAL FOR BUDGET MUST = ALLOCATION $   *           
*                                                                   *           
*  OCT09/92 (BU ) --- OVERRIDE TOTAL = ALLOCATION IF LFM PROFILE    *           
*                     SIXTH (6TH) BIT IS ON                         *           
*                                                                   *           
*  APR12/96 (SKU) --- 2000-BYTE SUPPORT FOR FIX RECORDS             *           
*                     DOESN'T WORK YET                              *           
*                                                                   *           
*  MAR19/97 (SKU) --- FILE/FIX DDS ONLY                             *           
*                                                                   *           
*  JAN26/98 (BU ) --- INCREASE ALLOWED BUDGET IN XS OF $100M        *           
*                                                                   *           
*  JAN19/00 (BU ) --- INCREASE ALLOWED BUDGET TO $200M              *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80413   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80413                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T80413+4096,R9      2ND BASE REGISTER                            
         EJECT                                                                  
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
         SPACE 2                                                                
         LA    R2,LFMLAST          POINT TO FIRST TITLE                         
         CLI   BREC,X'13'          BUDGET RECORD                                
         BE    BUD                                                              
         CLI   BREC,X'8C'          CONTRACT FIX RECORD                          
         BE    FIX                                                              
         CLI   BREC,X'18'          END OF ACCOUNTING MONTH ELEMENT              
         BE    EOM                                                              
         CLI   BREC,X'23'          DEMO MENU                                    
         BE    DEM                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         TITLE 'T80413 - RELFM13 - BUDGET RECORD'                               
*                  BUDGET RECORDS                                               
*                                                                               
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
DALLOC$  EQU   RBUD$TOT-RBUDELEM-2      FROM 1ST $ IN ELEMENT                   
DSTATOT$ EQU   RBUDSTOT-RBUDELEM-2                                              
DBUDTAG  EQU   RBUDTAG-RBUDELEM-2                                               
*                                                                               
BUD      EQU   *                                                                
         CLI   BACT,C'L'           ALLOCATION REQUEST FOR BUDGET?               
         BE    BUDALLOC            YES                                          
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   BUDEDT                                                           
         SPACE 1                                                                
*                  FORMAT ROUTINE                                               
         BAS   RE,GETREC                                                        
BUDFMT0  EQU   *                                                                
         MVC   BUDCTYP,=C'TOTAL     ' DEFAULT CONTYPE TITLE                     
         FOUT  BUDCTYPH                                                         
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    BFMT000             NO                                           
         MVC   BUDCTYP,BCONDESC    YES - MOVE CONTRACT TYPE DESC                
BFMT000  EQU   *                                                                
         LA    R8,12                                                            
         LA    R2,BUDIHEDH                                                      
         SPACE 1                                                                
         LA    R4,RBUDSTA          A(STATION BUDGETS)                           
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    BFMT006             NO  - USE A(RBUDSTA)                         
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
BFMT002  ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   BFMT004             END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   BFMT002             NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A(STA FIGURES) IN FOUND ELEMENT              
         B     BFMT006             YES - PROCESS IT                             
BFMT004  MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
         LA    R4,RBUDSTA          LOAD A(BASIC ELEMENT)                        
         LA    R5,0                PUT OUT 0 ALLOC $                            
         B     BFMT007                                                          
BFMT006  EQU   *                                                                
*                                                                               
*- DISPLAY TOTAL ALLOCATION AMOUNT, IF ANY.                                     
*                                                                               
         L     R5,DALLOC$(R4)      GET ALLOC $ FROM ELEMENT                     
BFMT007  EQU   *                                                                
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
BFMT008  CLC   0(1,R6),STARTMO                                                  
         BE    BFMT012                                                          
         CLI   0(R6),X'FF'                                                      
         BNE   BFMT010                                                          
         DC    H'0'                PROBLEM WITH START MONTH                     
BFMT010  LA    R6,4(R6)                                                         
         B     BFMT008                                                          
         SPACE 1                                                                
BFMT012  MVC   8(3,R2),1(R6)       FILL IN MONTH                                
         FOUT  (R2)                                                             
         SPACE 1                                                                
         BAS   RE,NEXTUF           NEXT UNPROTECTED FIELD                       
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    BFMT014             NO  - PUT OUT ZERO DOLLARS                   
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
BFMT014  EQU   *                                                                
         BAS   RE,MYEDIT           PUT OUT $ AMOUNT FOR MONTH                   
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R6,4(R6)            BUMP A(MONTH TABLE)                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO NEXT MONTH                          
         BCT   R8,BFMT012                                                       
         SPACE 1                                                                
         ZIC   RF,0(R2)            POINT TO STATION TOTAL FIELD                 
         AR    R2,RF                                                            
         LA    R5,0                SET R5 FOR 'NO OUTPUT'                       
         CLI   FOUNDTYP,C'N'       CONTRACT TYPE FOUND?                         
         BE    BFMT018             NO  - PUT OUT ZERO DOLLARS                   
         L     R4,SAVER4           RELOAD R4 FOR STA TOTAL AMT                  
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
BFMT018  EQU   *                                                                
         BAS   RE,MYEDIT                                                        
         FOUT  (R2)                                                             
*                                                                               
*- DISPLAY ALLOCATION STATUS (SET BY BUDGET SPREADER)                           
         LA    R1,BUDSTATB                                                      
         LA    R2,BUDSTATH                                                      
         FOUT  (R2),=CL20' ',20                                                 
BFMT020  CLI   0(R1),X'FF'                                                      
         BE    BFMT040             NO MATCH.  LEAVE BLANK                       
         CLC   DBUDTAG(1,R4),0(R1) MATCH ELEMENT FLAG VS TABLE                  
         BE    BFMT030             MATCH ON TAG BYTE.                           
         LA    R1,21(R1)                                                        
         B     BFMT020                                                          
*                                                                               
BFMT030  MVC   8(20,R2),1(R1)      TRANSLATION TO SCREEN                        
*                                                                               
BFMT040  EQU   *                                                                
         MVI   NOERRMSG,C'N'                                                    
         CLI   FOUNDTYP,C'Y'       CONTRACT TYPE FOUND?                         
         BE    BFMT045             YES                                          
         MVI   NOERRMSG,C'Y'       THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'CTNF),CTNF LOAD MESSAGE: CON TYPE NOT FOUND             
         FOUT  LFMMSGH                                                          
BFMT045  EQU   *                                                                
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
*   BUDGET ALLOCATION MULTI-TYPE DISPLAY.                                       
*                                                                               
NEXTMO   EQU   BDGMO2-BDGMO1       MONTH-TO-MONTH   DISPLACEMENT                
NEXTCOL  EQU   BDGTYP2-BDGTYP1     COLUMN-TO-COLUMN DISPLACEMENT                
FRSTDLR  EQU   BDGDLR1-BDGTYP1     COLUMN-TO-DOLLAR DISPLACEMENT                
NEXTDLR  EQU   BDGDLR2-BDGDLR1     DOLLAR-TO-DOLLAR DISPLACEMENT                
COLALLOC EQU   BDGALC1-BDGTYP1     COLUMN-TO-ALLOC  DISPLACEMENT                
COLSTAT  EQU   BDGSTS1-BDGTYP1     COLUMN-TO-STATUS DISPLACEMENT                
*                                                                               
BUDALLOC EQU   *                                                                
         BAS   RE,CLRSCRN          WIPE OUT ANY PRE-EXISTING FIELDS             
         BAS   RE,GETREC                                                        
         CLI   OFFALLOC,C'Y'       OFFICE REQUESTED?                            
         BE    BALL00              YES - SKIP AGGREGATION LOGIC                 
         LA    R4,REC2             STORE 'CORPORATE' (NO OFFICE) REC            
         LA    R5,REC                                                           
         BAS   RE,MOVEREC                                                       
         BAS   RE,CYCLRECS         PROCESS OFFICES' BUDGETS                     
         LA    R4,REC              RESTORE CORPORATE' (NO OFFICE) REC           
         LA    R5,REC2                                                          
         BAS   RE,MOVEREC                                                       
BALL00   EQU   *                                                                
         BAS   RE,SCANTYPS         SET UP, SORT TYPES                           
         BAS   RE,SETMONS          SET MONTHS DISPLAY                           
         LA    R7,5                LOOP FOR MAX CONTRACT TYPES                  
         LA    R6,BDGTYP1H         A(FIRST COLUMN'S FIRST FIELD)                
         ST    R6,SAVER6                                                        
         LA    R2,CTYPES           A(CONTRACT TYPE SAVE AREA)                   
         XC    BUDTOTS(56),BUDTOTS CLEAR HORIZONTAL TOTALS                      
BALL01   EQU   *                                                                
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R5,R4               END OF RECORD                                
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
BALL02   EQU   *                                                                
         CLI   0(R2),X'FF'         ANY MORE TYPES?                              
         BE    BALL32              NO  - END OF SCAN                            
         L     R6,SAVER6           SET A(COLUMN)                                
         ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               SKIPS 01 ELEMENT FIRST PASS                  
         CR    R5,R4               TEST END OF RECORD                           
         BNH   BALL32              END OF RECORD REACHED                        
         CLI   0(R4),X'02'         CONTRACT TYPE ELEMENT?                       
         BNE   BALL02              NO  - SKIP IT                                
         CLC   0(1,R2),DCONTYP(R4) MATCH ON CONTRACT TYPE?                      
         BNE   BALL02              NO  - GO BACK FOR NEXT                       
*                                                                               
*   NOTE: AT TIME OF WRITING, THIS RECORD CONTAINED ONLY AN 01                  
*   ELEMENT, AND MAY HAVE HAD ONE OR MORE 02 ELEMENTS.  NO OTHER                
*   ELEMENTS WERE IN USE.  CODE WAS INSERTED ANYWAY.  BU. 10/90                 
*                                                                               
         BAS   RE,DISPTYP          DISPLAY ELEMENT FOUND                        
         L     R6,SAVER6           CALCULATE NEXT COLUMN                        
         LA    R6,NEXTCOL(R6)                                                   
         ST    R6,SAVER6           STORE NEW A(COLUMN)                          
         LA    R2,1(R2)            NEXT CONTRACT TYPE                           
         BCT   R7,BALL01           GO BACK FOR NEXT                             
BALL32   EQU   *                                                                
         BAS   RE,SETTYPES         SET CONTRACT TYPES                           
         BAS   RE,DHT              DISPLAY HORIZONTAL TOTALS                    
         LA    R2,LFMKEYH          A(CURSOR ON EXIT)                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   CLEAR SCREEN OF PRE-DISPLAYED DATA, RESET TRANSMIT BITS                     
*                                                                               
CLRSCRN  EQU   *                                                                
         NTR1                                                                   
         TWAXC BDGTYP1H,BDGTYP5H,PROT=Y                                         
         TWAXC BDGALC1H,BDGALCTH,PROT=Y                                         
         TWAXC BDGSTS1H,BDGSTSTH,PROT=Y                                         
         TWAXC BDGMO1H,BDGDLRXH,PROT=Y                                          
         TWAXC BDGTOT1H,BDGTOTTH,PROT=Y                                         
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   EXTRACT ALL CONTRACT TYPES, SORT INTO ASCENDING SEQUENCE                    
*                                                                               
SCANTYPS EQU   *                                                                
         NTR1                                                                   
         SR    R8,R8               INITIALIZE COUNTER                           
         LA    R6,CTYPES           A(CONTRACT TYPE TABLE)                       
         MVI   CTYPES,X'FF'        INITIALIZE TABLE                             
         MVC   CTYPES+1(7),CTYPES                                               
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R5,R4               END OF RECORD                                
         LA    R4,RBUDELEM         A(01 ELEMENT) OF RECORD)                     
SC002    EQU   *                                                                
         ZIC   R3,1(R4)            L(ELEMENT)                                   
         AR    R4,R3               SKIPS 01 ELEMENT FIRST PASS                  
         CR    R5,R4               TEST END OF RECORD                           
         BNH   SC006               END OF RECORD REACHED                        
         CLI   0(R4),X'02'         CONTRACT TYPE ELEMENT?                       
         BNE   SC002               NO  - SKIP IT                                
         MVC   0(1,R6),DCONTYP(R4) STORE CONTRACT TYPE                          
         LA    R6,1(R6)            BUMP A(NEXT CONTRACT TYPE)                   
         LA    R8,1(R8)                                                         
         B     SC002               GO BACK FOR NEXT TYPE                        
SC006    EQU   *                                                                
*                                                                               
*  CONTRACT TYPES ARE ENTERED IN TABLE CTYPES.  R8 CONTAINS THE COUNT.          
*    XSORT SEQUENCES THE ONE-BYTE ENTRIES.                                      
*                                                                               
         CLI   CTYPES+1,X'FF'      MORE THAN ONE ENTRY?                         
         BE    EXXMOD              NO  - SKIP SORT                              
         PRINT GEN                                                              
         GOTO1 VXSORT,DMCB,(0,CTYPES),(R8),1,1,0                                
         PRINT NOGEN                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   CALCULATE FISCAL START MONTH, DISPLAY MONTHS IN LEFT COLUMN                 
*                                                                               
SETMONS  EQU   *                                                                
         NTR1                                                                   
         LA    R2,BDGMO1H          A(FIRST MONTH)                               
         LA    R4,12               12 MONTH LOOP                                
*                                                                               
*   FIND STARTING POINT TO DISPLAY MONTHS                                       
*                                                                               
         LA    R6,MONTHS                                                        
SM001    EQU   *                                                                
         CLC   0(1,R6),STARTMO     CHECK MON# AGAINST LITERAL TABLE             
         BE    SM004                                                            
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BNE   SM003               NO                                           
         DC    H'0'                PROBLEM WITH START MONTH                     
SM003    EQU   *                                                                
         LA    R6,4(R6)                                                         
         B     SM001               GO BACK FOR NEXT                             
SM004    EQU   *                                                                
         MVC   8(3,R2),1(R6)       MOVE MONTH TO SCREEN                         
         FOUT  (R2)                                                             
         LA    R2,NEXTMO(R2)       NEXT MONTH SCREEN FIELD                      
         LA    R6,4(R6)            NEXT MONTH TITLE FIELD                       
         BCT   R4,SM004            GO BACK FOR NEXT                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   DISPLAY CONTRACT TYPE'S VALUES AND TYPE'S EXPANSION                         
*                                                                               
DISPTYP  EQU   *                                                                
         NTR1                                                                   
         LR    R2,R6               SET OUTPUT ADDRESS                           
         LA    R2,FRSTDLR(R2)      BUMP TO FIRST $ FIELD IN COL                 
         LA    R7,12               SET FOR 12 MONTHS' $                         
         ZIC   R5,STARTMO          FISCAL START MONTH                           
         BCTR  R5,0                MAKE ZERO RELATIVE                           
         MH    R5,=H'4'            MULTIPLY BY BUCKET SIZE                      
         LA    R4,2(R4)            A(FIRST $ IN ELEMENT)                        
         ST    R4,SAVER4A          SAVE A(ELEMENT) FOR LATER                    
         LA    R4,0(R5,R4)         A(START OF FISCAL STN BUDGET)                
         LA    R3,BUDTOTS          A(STORAGE FOR HOIZONTAL TOTALS)              
DT002    EQU   *                                                                
         L     R5,0(R4)            TAKE DOLLARS FROM RECORD                     
         L     R1,0(R3)            EXISTING ROW TOTAL                           
         AR    R1,R5               ADD NEW FIGURE TO TOTAL                      
         ST    R1,0(R3)            STORE NEW TOTAL IN TABLE                     
         LA    R3,4(R3)            BUMP TO NEXT TOTAL IN TABLE                  
         BAS   RE,TYPEDIT          PUT OUT $ AMOUNT FOR MONTH                   
         LA    R4,4(R4)            BUMP A($ BUCKET)                             
         LA    R2,NEXTDLR(R2)      A(NEXT SCREEN BUCKET)                        
         BCT   R7,DT002            GO BACK FOR NEXT                             
*                                                                               
*  R2 IS NOW SET TO THE TOTAL LINE, INTO WHICH THOSE FIGS ARE PUT               
*                                                                               
         L     R4,SAVER4A          RESET TO BEGINNING OF ELEMENT                
         L     R5,DSTATOT$(R4)     TAKE DOLLARS FROM ELEMENT                    
         L     R1,0(R3)            EXISTING ROW TOTAL                           
         AR    R1,R5               ADD NEW FIGURE TO TOTAL                      
         ST    R1,0(R3)            STORE NEW TOTAL IN TABLE                     
         LA    R3,4(R3)            BUMP TO NEXT TOTAL IN TABLE                  
         BAS   RE,TYPEDIT                                                       
         FOUT  (R2)                                                             
         LA    R2,COLALLOC(R6)     RESET TO ALLOCATION FIELD                    
         L     R5,DALLOC$(R4)      GET ALLOC $ FROM ELEMENT                     
         L     R1,0(R3)            EXISTING ROW TOTAL                           
         AR    R1,R5               ADD NEW FIGURE TO TOTAL                      
         ST    R1,0(R3)            STORE NEW TOTAL IN TABLE                     
         BAS   RE,TYPEDIT          EDIT ALLOCATION INTO SCREEN                  
         FOUT  (R2)                                                             
         LA    R2,COLSTAT(R6)      RESET TO STATUS FIELD                        
         FOUT  (R2),=CL9' ',9                                                   
         LA    R1,BUDSTATN         A(DESCRIPTION TABLE)                         
DT014    EQU   *                                                                
         CLI   0(R1),X'FF'                                                      
         BE    DT018               NOT FOUND                                    
         CLC   DBUDTAG(1,R4),0(R1) MATCH ELEMENT FLAG VS TABLE                  
         BE    DT016               MATCH ON TAG BYTE                            
         LA    R1,10(R1)                                                        
         B     DT014               BACK FOR NEXT                                
DT016    EQU   *                                                                
         MVC   8(9,R2),1(R1)      MOVE TO SCREEN                                
DT018    EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   DISPLAY HORZONTAL TOTALS                                                    
*                                                                               
DHT      EQU   *                                                                
         NTR1                                                                   
*                                                                               
         LA    R6,13               NUMBER OF MONTHS + BOTTOM TOTAL LINE         
         LA    R2,BDGDLRTH         A(FIRST FIELD IN TOTAL COLUMN)               
         LA    R3,BUDTOTS          A(HORIZONTAL TOTALS TABLE)                   
DHT010   L     R5,0(R3)            GET TOTAL VALUE                              
         BAS   RE,TYPEDIT                                                       
         FOUT  (R2)                                                             
         LA    R3,4(R3)            NEXT VALUE IN TABLE                          
         LA    R2,NEXTDLR(R2)      NEXT ROW ON SCREEN                           
         BCT   R6,DHT010                                                        
*                                                                               
         LA    R2,BDGALCTH         A(TOTAL COLUMN ON TOTAL ALLOC ROW)           
         L     R5,0(R3)            TOTAL VALUE FROM TABLE                       
         BAS   RE,TYPEDIT                                                       
         FOUT  (R2)                                                             
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   INSERT CONTRACT TYPE'S EXPANSIONS                                           
*                                                                               
SETTYPES EQU   *                                                                
         NTR1                                                                   
         LA    R6,CTYPES           A(SAVED CONTRACT TYPES                       
         LA    R8,8                MAX NUMBER SAVED (ONLY FIVE USED)            
         LA    R2,BDGTYP1H         A(TYPE ON SCREEN FIELD)                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'                                                        
         MVC   KEY+24(2),REPALPHA                                               
ST002    EQU   *                                                                
         CLI   0(R6),X'FF'         HI-VALUE?                                    
         BE    ST006               YES - FINISHED                               
         MVC   KEY+26(1),0(R6)     LOAD TYPE TO KEY                             
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    ST004                                                            
         DC    H'0'                                                             
ST004    EQU   *                                                                
         BAS   RE,GETREC                                                        
         MVC   8(9,R2),RCTYDESC   LOAD DESCRIPTION TO SCREEN                    
         LA    R7,8(R2)                                                         
         GOTO1 =V(RIGHT),DMCB,(R7),9,RR=YES                                     
         FOUT  (R2)                                                             
         LA    R2,NEXTCOL(R2)      BUMP A(NEXT TYPE FIELD)                      
         LA    R6,1(R6)            BUMP A(SAVED TYPE)                           
         BCT   R8,ST002            NO  - GO BACK FOR NEXT                       
ST006    EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  CYCLRECS ACCUMULATES THE INDIVIDUAL OFFICE BUDGETS, BY CONTRACT              
*    TYPE, INTO THE CORPORATE (OR FIRST OFFICE BUDGET) RECORD FOUND             
*    FOR THE REQUEST.  THE CORPORATE (OR FIRST OFFICE BUDGET) RECORD            
*    IS MOVED TO REC2.  UNTIL A KEY BREAK IS ENCOUNTERED, EACH SUB-             
*    SEQUENT OFFICE BUDGET RECORD IS SCANNED.  THE BASIC ELEMENTS               
*    ARE ACCUMULATED, AS A MATTER OF COURSE.  THEN, EACH CONTRACT               
*    TYPE IN THE OFFICE BUDGET RECORD IS SEARCHED FOR IN THE STORED             
*    RECORD.  IF FOUND, THE FIGURES ARE ACCUMULATED.  IF NOT FOUND,             
*    THE CONTRACT TYPE IS ADDED IF THERE IS ROOM TO FIT ANOTHER                 
*    ELEMENT.  (THE MAX IS 5 CONTRACT TYPES.)  WHEN THE ROUTINE IS              
*    FINISHED, THE STORED RECORD IS RESTORED TO REC, WHERE IT IS                
*    PROCESSED AS A SINGLE RECORD WHICH NOW REFLECTS THE SUM OF                 
*    ALL THE OFFICES.  AS THE RECORD IS NEVER RE-WRITTEN TO DISK,               
*    THIS 'COMPOSITE' RECORD ONLY EXISTS UNTIL THE END OF THE RUN.              
*          BILL UHR.  NOV/90                                                    
*                                                                               
S01REC2  EQU   RBUDELEM-RBUDREC                                                 
S01TOT   EQU   RBUDSTOT-RBUDELEM                                                
S01ATOT  EQU   RBUD$TOT-RBUDELEM                                                
S01CNTR  EQU   RBUDCNTR-RBUDELEM                                                
S01TYP   EQU   RBUDTYPE-RBUDELE2                                                
S01LEN   EQU   RBUDLEN-RBUDREC                                                  
*                                                                               
CYCLRECS EQU   *                                                                
         NTR1                                                                   
CRECS000 EQU   *                                                                
         BAS   RE,SEQ              GET NEXT RECORD                              
         BAS   RE,GETREC                                                        
         CLC   KEYSAVE(25),KEY     SAME YEAR,STATION?                           
         BNE   CRECS099            NO  - EXIT                                   
         LA    R1,RBUDELEM         A(01 ELEM NEW RECORD)                        
         LA    R2,S01REC2+REC2     A(01 ELEM OLD RECORD)                        
         BAS   RE,ADDBUDS          ADD BASIC (01 ELEM) BUDGETS                  
         LA    R6,RBUDREC          A(NEW BUDGET RECORD)                         
         ZICM  R4,RBUDLEN,2        L(NEW BUDGET RECORD)                         
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R6,R4               END OF NEW BUDGET RECORD                     
CRECS002 EQU   *                                                                
         LA    R7,REC2             A(OLD BUDGET RECORD)                         
         ZICM  R4,S01LEN(R7),2     L(OLD BUDGET RECORD)                         
         BCTR  R4,0                BACK UP OVER NULL TERMINATOR                 
         AR    R7,R4               END OF OLD BUDGET RECORD                     
CRECS008 EQU   *                                                                
         ZIC   R3,1(R1)            L(ELEM NEW RECORD)                           
         AR    R1,R3               A(ELEM)+L(ELEM)=A(NEXT ELEM)                 
         CR    R6,R1               TEST END OF NEW RECORD                       
         BNH   CRECS000            END OF NEW RECORD REACHED                    
         CLI   0(R1),X'02'         CONTRACT TYPE BUDGET ELEMENT?                
         BNE   CRECS008            NO  - SKIP IT                                
         BAS   RE,FINDTYP          PROCESS THIS NEW ELEMENT                     
         B     CRECS002            ACCOUNT FOR POSSIBLE INCREASE                
*                                  IN L(OLD RECORD)                             
CRECS099 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  NEW ELEMENT VS OLD RECORD:  FIND IT AND ACCUMULATE BUCKETS, OR               
*      ADD ELEMENT IN ENTIRETY TO OLD RECORD                                    
*        R1   =  A(NEW RECORD ELEMENT)                                          
*        R2   =  A(01 ELEMENT OF OLD RECORD)                                    
*        R7   =  END OF OLD RECORD                                              
*                                                                               
FINDTYP  EQU   *                                                                
         NTR1                                                                   
FTYP000  EQU   *                                                                
         LR    R4,R2               SAVE A(BASIC BUDGET ELEMENT)                 
         ZIC   R3,1(R2)            FIRST PASS SKIPS 01 ELEMENT                  
         AR    R2,R3               A(NEXT ELEMENT) IN OLD RECORD                
         CR    R7,R2               TEST END OF OLD RECORD                       
         BNH   FTYP020             END OF OLD RECORD REACHED                    
         CLI   0(R2),X'02'         IS IT CONTRACT TYPE ELEMENT?                 
         BNE   FTYP000             NO  - GET NEXT OLD REC ELEM                  
         CLC   S01TYP(1,R1),S01TYP(R2)        SAME TYPE?                        
         BNE   FTYP000             NO  - GET NEXT OLD REC ELEM                  
         BAS   RE,ADDBUDS          YES - ACCUM BUDGETS FOR TYPE                 
         B     FTYP099             EXIT                                         
FTYP020  EQU   *                                                                
         LR    R2,R4               RESET A(BASIC BUDGET ELEMENT)                
         BAS   RE,NEWELEM          NEW ELEM FOR OLD REC: ADD IT                 
FTYP099  EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   ACCUMULATES EACH OF THE TWENTY-FOUR BUCKETS FROM THE NEW RECORD             
*     INTO THE CORRESPONDING BUCKETS OF THE OLD RECORD                          
*        R1   =   A(ELEMENT IN NEW RECORD)                                      
*        R2   =   A(ELEMENT IN OLD RECORD)                                      
*                                                                               
ADDBUDS  EQU   *                                                                
         NTR1                                                                   
         L     R3,S01TOT(R1)       NEW STATION BUDGET TOTAL                     
         L     R4,S01TOT(R2)       OLD STATION BUDGET TOTAL                     
         AR    R4,R3               ACCUM STATION BUDGET TOTAL                   
         ST    R4,S01TOT(R2)       ADD TOTAL BACK TO OLD RECORD                 
         L     R3,S01ATOT(R1)      NEW ALLOC DOLLAR TOTAL                       
         L     R4,S01ATOT(R2)      OLD ALLOC DOLLAR TOTAL                       
         AR    R4,R3               ACCUM ALLOC DOLLAR TOTAL                     
         ST    R4,S01ATOT(R2)      ADD TOTAL BACK TO OLD RECORD                 
         LA    R1,2(R1)            BUMP TO 1ST BUDGET BUCKETS                   
         LA    R2,2(R2)                                                         
         LA    R7,24               LOOP CONTROL                                 
ABUDS002 EQU   *                                                                
         L     R3,0(R1)            NEW FIGURES                                  
         L     R4,0(R2)            OLD FIGURES                                  
         AR    R4,R3                                                            
         ST    R4,0(R2)            RESTORE ACCUM'D OLD FIGURES                  
         LA    R1,4(R1)            BUMP BUCKETS                                 
         LA    R2,4(R2)                                                         
         BCT   R7,ABUDS002         DO ALL 24 MONTHS                             
         B     EXXMOD              FINISHED                                     
         EJECT                                                                  
*                                                                               
*   NEW ELEMENT ADDED TO OLD (STORED) RECORD (IF ROOM)                          
*        R1   =  A(NEW RECORD ELEMENT) TO BE ADDED                              
*        REC2 =  A(OLD RECORD)                                                  
*                                                                               
NEWELEM  EQU   *                                                                
         NTR1                                                                   
         CLI   S01CNTR(R2),X'4'    RECORD MAX'D OUT? 5 IS MAX                   
         BH    NELE099             YES - DON'T DO ANYTHING                      
         LR    R2,R1               FREE R1 FOR MACRO USE                        
         GOTO1 VADDELEM,DMCB,REC2,(R2)                                          
         ZIC   R3,S01CNTR(R2)      INCREMENT COUNTER                            
         LA    R3,1(R3)                                                         
         STC   R3,S01CNTR(R2)      RESTORE COUNTER                              
NELE099  EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*- BUDGET RECORD EDITING ROUTINE                                                
*                                                                               
BUDEDT   EQU   *                                                                
*                                                                               
*  NO LONGER REBUILDING ENTIRE RECORD:  RETRIEVE EXISTING IF NEEDED             
*                                                                               
         CLI   BACT,C'A'                 ACTION = ADD?                          
         BE    BE0001                    YES - SET RECORD LENGTH                
         BAS   RE,GETREC           NO  - RETRIEVE RECORD FROM STORAGE           
         B     BE0010                                                           
BE0001   XC    RBUDELEM(110),RBUDELEM    ZERO OUT RECORD                        
         MVC   REC+34(2),=X'016E'        ELCODE=01,LENGTH=108                   
         MVC   REC+27(2),=Y(144)         LEN=KEY+CONTROL+01 ELEMENT             
BE0010   EQU   *                                                                
         XC    BYTE,BYTE                                                        
         LA    R4,RBUDSTA          A(BASIC BUDGET FIGURES)                      
         MVI   FOUNDTYP,C'Y'       SET 'CONTRACT TYPE FOUND' TO YES             
         CLI   BCONTYP,C' '        ANY CONTRACT TYPE ENTERED?                   
         BE    BE0014              NO  -                                        
         LA    R5,RBUDREC          A(BUDGET RECORD)                             
         ZICM  R4,RBUDLEN,2        L(BUDGET RECORD)                             
         BCTR  R4,0                ELIMINATE L'NULL TERMINATOR                  
         AR    R5,R4               FIND END OF RECORD                           
         LA    R4,RBUDELEM         A(01 ELEMENT OF RECORD)                      
BE0011   ZIC   R3,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,R3               A(NEXT ELEMENT)                              
         CR    R5,R4               COMPARE FOR END OF RECORD                    
         BNH   BE0012              END OF RECORD REACHED                        
         CLC   DCONTYP(1,R4),BCONTYP   SAME CONTRACT TYPE?                      
         BNE   BE0011              NO  - FIND NEXT ELEMENT                      
         LA    R4,2(R4)            A($ BUCKETS IN ELEMENT)                      
         B     BE0014              YES - PROCESS IT                             
BE0012   EQU   *                                                                
         CLI   RBUDCNTR,X'5'       THIS IS THE MAX COUNTER                      
         BL    BE0013              ROOM FOR ONE MORE                            
         LA    R2,LFMACTH                                                       
         MVC   LFMMSG(L'NRFCT),NRFCT LOAD 'NO ROOM' MESSAGE                     
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                EXIT                                         
BE0013   MVI   FOUNDTYP,C'N'       SET 'CONTRACT TYPE FOUND' FLAG               
*                                                                               
*  CONTRACT TYPE BUDGET ELEMENTS ARE SAME FORMAT AS BASIC ELEMENT               
*                                                                               
         LA    R4,WORK2+2               LOAD A($ BUCKETS IN BUILD AREA)         
         XC    WORK2(144),WORK2         INITIALIZE AREA                         
         MVC   WORK2(2),=X'026E'        SET EL CODE, LENGTH                     
         MVC   WORK2+DCONTYP(1),BCONTYP SET CONTRACT TYPE                       
BE0014   EQU   *                                                                
*                                                                               
*- EDIT TOTAL ALLOCATION AMOUNT FIELD.                                          
         XC    DALLOC$(4,R4),DALLOC$(R4)                                        
         LA    R2,BUD$TOTH                                                      
         CLI   5(R2),0                                                          
         BE    BE0015              OPTION INPUT.                                
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 =V(NUMVAL),DMCB,BUD$TOT,(R3),RR=YES                              
         CLI   DMCB,X'0'                                                        
         BNE   FLERR3                                                           
         MVC   DALLOC$(4,R4),DMCB+4     MOVE AMOUNT INTO RECORD                 
         MVC   ALLOCTOT,DMCB+4          SAVE AMT FOR TOTAL CHECK                
         SPACE 1                                                                
BE0015   EQU   *                                                                
         ST    R4,SAVER4           SAVE A($ BUCKETS) FOR TOTALS                 
         ZIC   R5,STARTMO          STARTING MONTH NUMBER                        
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R4,0(R5,R4)         R4=ADDRESS OF STARTING MONTH                 
         ST    R4,ASTART                                                        
         SPACE 1                                                                
         LR    R5,R4                                                            
         LA    R8,BUDIDECH         LAST INPUT FIELD                             
BE0016   BAS   RE,NEXTUF                                                        
         SR    R0,R0                                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               INPUT NOT REQUIRED                           
         BZ    BE0020                                                           
         SPACE 1                                                                
         CLI   8(R2),C'A'          ANNUAL AMOUNT                                
         BE    BUDA                                                             
         CLI   8(R2),C'Q'          QUARTERLY AMOUNT                             
         BE    BUDQ                                                             
         SPACE 1                                                                
         CLI   5(R2),9             MAX 8 DIGITS                                 
         BH    FLERR2                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         BCTR  R1,0                                                             
         EX    R1,BEPACK                                                        
         B     BE0018                                                           
*                                                                               
BEPACK   PACK  DUB,8(0,R2)                                                      
*                                                                               
BE0018   EQU   *                                                                
         CVB   R0,DUB                                                           
         C     R0,=F'200000000'                                                 
         BH    FLERR2                                                           
         LTR   R0,R0                                                            
         BM    FLERR2              MUST BE POSITIVE                             
         B     BE0020                                                           
         SPACE 2                                                                
* DEAL WITH ANNUAL OR QUARTERLY AMOUNTS                                         
         SPACE 1                                                                
BUDA     CR    R5,R4               A MUST BE IN FIRST MONTH                     
         BNE   FLERR2                                                           
         BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,12               DIVISOR (NO. OF MONTHS)                      
         B     BUDAQ                                                            
         SPACE 1                                                                
BUDQ     LA    R6,4                                                             
BUDQ5    CR    R5,R4          Q MUST BE IN 1ST,4TH,7TH, OR 10TH MONTH           
         BE    BUDQ10                                                           
         LA    R5,12(R5)                                                        
         BCT   R6,BUDQ5                                                         
         B     FLERR2                                                           
         SPACE 1                                                                
BUDQ10   BAS   RE,TESTAMT                                                       
         CLI   BYTE,C'Y'           WAS THERE ERROR IN TESTAMT                   
         BE    FLERR2                                                           
         LA    RF,3                DIVISOR (NO. OF MONTHS)                      
         SPACE 1                                                                
BUDAQ    L     R1,FULL             TOTAL ANNUAL OR QUARTERLY AMT                
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
BUDAQ20  MVC   0(4,R4),MNTHAMT                                                  
         LA    R4,4(R4)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   RF,BUDAQ20                                                       
         MVC   0(4,R4),MNTHXAMT    LAST MONTH                                   
         B     BE0024                                                           
         SPACE 2                                                                
BE0020   ST    R0,0(R4)                                                         
         SPACE 1                                                                
BE0024   LA    R4,4(R4)                                                         
         SPACE 1                                                                
         CR    R2,R8               ARE WE AT DEC FOR INTERNAL                   
         BL    BE0016              NO, DO NEXT FIELD                            
         BH    BE0030              YES, ADD UP TOTALS                           
         SPACE 1                                                                
         L     R5,ASTART                                                        
         BAS   RE,SHIFTMO          GET DATA IN JAN-DEC FORMAT                   
         SPACE 1                                                                
         SPACE 2                                                                
BE0030   EQU   *                   *** NO MORE INTERNAL ***                     
         SPACE 1                                                                
*  ADD UP TOTALS                                                                
         LA    R3,12                                                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         L     R4,SAVER4           SET A($ BUCKETS) FOR TOTALS                  
         LA    R4,48(R4)           BUMP TO 2ND SET OF BUCKETS                   
         SPACE 1                                                                
BE0036   A     R6,0(R4)            ACCUMULATE IN R6                             
         LA    R4,4(R4)            BOUNCE THRU MONTHLY FIGURES                  
         BCT   R3,BE0036                                                        
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
         BO    BE0040              YES - OVERRIDE ERROR CHECK                   
*                                                                               
*  IF THE MONTHLY TOTAL IS NOT EQUAL TO THE TOTAL ALLOCATION, AN                
*     ERROR HAS OCCURRED.                                                       
*                                                                               
         C     R5,ALLOCTOT         MONTH TOTAL = TOTAL ALLOC?                   
         BE    BE0040              YES -                                        
         LA    R2,BUD$TOTH         THIS MODULE INSERTING MSG                    
         MVC   LFMMSG(L'BADTOT),BADTOT                                          
*                                  LOAD MSG: TOTAL NOT = ALLOCATION             
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         B     EXIT                                                             
BE0040   EQU   *                                                                
         SPACE                                                                  
         XC    RBUDTAG,RBUDTAG     STARTS AS 0 ON ADD                           
         CLI   FOUNDTYP,C'Y'       TYPE FOUND?                                  
         BE    FLFILE              YES - CHECK FOR ADD OR CHANGE                
         ZIC   R2,RBUDCNTR         NO  - BUMP CONTRACT TYPE COUNTER             
         LA    R2,1(R2)            BUMP BY 1                                    
         STC   R2,RBUDCNTR         REINSERT COUNTER INTO RECORD                 
         BAS   RE,ADDEL            ADD RECORD FROM WORK2                        
         EJECT                                                                  
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
*        CHANGE - READ REC, THEN WRITE NEW                                      
*                                                                               
* INCASE CONTRACT IS 2K, WILL USE REC AND REC2 FOR A 2000-BYTE BUFFER           
* REC IS THEN SAVED TO WORK2                                                    
*                                                                               
         CLI   BREC,X'8C'          CONTRACT FIX RECORD                          
         BNE   FLCH05              SAVE REC IN WORK2                            
         GOTO1 MOVERECL,DMCB,REC,WORK2                                          
         B     FLCH10                                                           
*                                                                               
FLCH05   EQU   *                                                                
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
         CLI   BREC,X'8C'          CONTRACT FIX RECORD                          
         BNE   FLCH15              MOVE WORK2 TO REC                            
         GOTO1 MOVERECL,DMCB,WORK2,REC                                          
         BAS   RE,PUTREC           WRITE OUT RECORD                             
         B     FLCH25                                                           
*                                                                               
*                                                                               
*- PRESERVE BUDGET RECORD TAG ON CHANGES                                        
FLCH15   EQU   *                                                                
         CLI   BREC,X'13'                                                       
         BNE   FLCH20                                                           
         MVC   (RBUDTAG-RBUDREC)(1,R5),(RBUDTAG-RBUDREC)(R4)                    
*                                                                               
FLCH20   EQU   *                                                                
         BAS   RE,XCREC                                                         
         BAS   RE,PUTREC                                                        
*                                                                               
FLCH25   DS    0H                                                               
         LA    R2,LFMLAST   POINT TO TOP TO DISPLAY CHANGED RECORD              
         CLI   BREC,X'13'   IF BUDGET RECORD, BACK TO BUDGET DISPLAY            
         BE    BUDFMT0                                                          
         CLI   BREC,X'18'   IF EOM, BACK TO EOM DISPLAY                         
         BE    EOMFMT                                                           
         CLI   BREC,X'23'   IF DEMO MENU, BACK TO DEMO MENU DISPLAY             
         BE    DEMFMT                                                           
         B     FIX30               OTHERWISE, BACK TO FIX DISPLAY               
         SPACE 1                                                                
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
         LA    R2,LFMLAST    POINT TO TOP TO DISPLAY NEW RECORD                 
         CLI   BREC,X'18'                                                       
         BE    EOMFMT              IF EOM, BACK TO EOM DISPLAY                  
         CLI   BREC,X'23'                                                       
         BE    DEMFMT              IF DEMO MENU, BACK TO DEMO DISPLAY           
         B     BUDFMT0                                                          
         EJECT                                                                  
         TITLE 'T80413 - RELFM13 - CONTRACT BUCKET FIX'                         
*ROUTINE TO HANDLE BUCKET FIX FOR CONTRACTS                                     
         SPACE 1                                                                
FIX      DS    0H                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    FIX20               IF NOT, DON'T ALLOW ANY ACCESS               
         LA    R2,LFMRECH                                                       
         LA    R3,5                                                             
         B     ERROR                                                            
*                                                                               
FIX20    DS    0H                                                               
         CLI   BFMTSW,0                                                         
         BNE   FIXEDT                                                           
         SPACE 1                                                                
*  FORMAT ROUTINE                                                               
         BAS   RE,GETREC                                                        
FIX30    XC    SAVEYM,SAVEYM                                                    
         XC    FULL,FULL                                                        
         GOTO1 VGETEL,DMCB,(X'03',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    EXXMOD                                                           
         SPACE 1                                                                
         L     R7,DMCB+8                                                        
         USING RCONBKEL,R7                                                      
         SPACE 1                                                                
         LA    R2,FIXIHEDH         POINT TO FIRST MONTH FIELD                   
         LA    R8,FIXIDECH         POINT TO LAST AMOUNT FIELD                   
         LA    R6,MONTHS                                                        
         SPACE 1                                                                
FIX40    CLC   RCONBKYR(2),SAVEYM                                               
         BNE   FIX50                                                            
         L     R4,FULL          ADD UP ALL BUCKETS WITH SAME MONTH/YEAR         
         ICM   R3,15,RCONBKAM      REGARDLESS OF ACTIVITY DATE                  
         AR    R4,R3                                                            
         ST    R4,FULL                                                          
         SPACE 1                                                                
         ZIC   RF,1(R7)            NEXT ELEMENT                                 
         AR    R7,RF                                                            
         CLI   0(R7),X'03'                                                      
         BE    FIX40                                                            
         B     FIX60                                                            
         SPACE 1                                                                
FIX50    CLI   SAVEYM,0            FIRST TIME                                   
         BE    FIX90                                                            
         SPACE 1                                                                
FIX60    CLC   0(1,R6),SAVEYM+1    ONLY SHOW MONTHS WITH ACTIVITY               
         BE    FIX70                                                            
         LA    R6,4(R6)                                                         
         B     FIX60                                                            
         SPACE 1                                                                
FIX70    CR    R2,R8               ARE WE AT BOTTOM OF SCREEN                   
         BNH   FIX72               NO                                           
         LA    R2,FIXSJANH         YES,GO BACK TO TOP                           
         B     FIX75                                                            
FIX72    MVC   8(3,R2),1(R6)                                                    
         FOUT  (R2)                                                             
FIX75    BAS   RE,NEXTUF           POINT TO AMOUNT FIELD                        
         SPACE 1                                                                
         L     R5,FULL                                                          
         BAS   RE,FIXEDIT                                                       
         FOUT  (R2)                                                             
         SPACE 1                                                                
         CLI   0(R7),X'03'         MORE TO DO                                   
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         XC    FULL,FULL                                                        
         LA    R6,4(R6)                                                         
         BAS   RE,NEXTUF                                                        
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO NEXT MONTH FIELD                    
         SPACE 1                                                                
FIX90    MVC   SAVEYM(2),RCONBKYR                                               
         B     FIX40                                                            
         DROP  R7                                                               
         EJECT                                                                  
FIXEDT   BAS   RE,GETREC                                                        
         SPACE 1                                                                
         GOTO1 VGETEL,DMCB,(X'03',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    EXXMOD                                                           
         SPACE 1                                                                
         L     R7,DMCB+8                                                        
         USING RCONBKEL,R7                                                      
         SPACE 1                                                                
         LA    R8,SAVBUC                                                        
FIX140   MVC   0(4,R8),RCONBKYR    SAVE MTH/YR AND 1ST ACTIVITY DATE            
         SPACE 1                                                                
FIX150   GOTO1 VRECUP,DMCB,(X'02',REC),(R7),0   DELETE ELEMENT                  
         CLI   0(R7),X'03'                                                      
         BE    FIX155                                                           
         MVI   4(R8),X'FF'         END OF LIST                                  
         B     FIX160                                                           
         SPACE 1                                                                
FIX155   CLC   0(2,R8),RCONBKYR    DON'T NEED MORE ACTIVITY FOR                 
         BE    FIX150              SAME MONTH/YEAR                              
         LA    R8,4(R8)                                                         
         B     FIX140                                                           
         DROP  R7                                                               
         SPACE 1                                                                
FIX160   LA    R2,FIXSJANH         POINT TO FIRST AMOUNT FIELD                  
         LA    R3,FIXIDECH         POINT TO BOTTOM OF SCREEN                    
         XC    WORK2(48),WORK2                                                  
         MVI   WORK2,X'03'         BUILD NEW ELEMENT                            
         MVI   WORK2+1,X'0A'                                                    
         LA    R8,SAVBUC                                                        
         SPACE 1                                                                
FIX165   MVC   WORK2+2(4),0(R8)                                                 
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    FLERR2                                                           
         MVC   WORK2+6(4),DMCB+4                                                
         SPACE 1                                                                
         BAS   RE,ADDEL            ADD ELEMENT                                  
         LA    R8,4(R8)                                                         
         BAS   RE,NEXTUF                                                        
         BAS   RE,NEXTUF                                                        
         CLI   0(R8),X'FF'         END OF LIST?                                 
         BE    FLFILE              YES, ADD RECORD                              
         SPACE 1                                                                
         CR    R2,R3               ARE WE AT BOTTOM OF SCREEN                   
         BNH   FIX165              NO                                           
         LA    R2,FIXSJANH         YES, BACK TO TOP                             
         BAS   RE,NEXTUF                                                        
         B     FIX165                                                           
         EJECT                                                                  
         TITLE 'T80413 - RELFM13 - EOM / ACCOUNTING END OF MONTH'               
*  ROUTINE TO ACCOUNTING END OF MONTH DATES (EOM)                               
         SPACE 1                                                                
EOM      CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   EOMEDT                                                           
*    FORMAT ROUTINE                                                             
         BAS   RE,GETREC                                                        
EOMFMT   LA    R4,12                                                            
         LA    R5,EOMDECH                                                       
         BAS   RE,NEXTUF           POINT TO START DATE                          
         LA    R6,REOMDATE                                                      
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(2,0(R6)),(0,WORK)      TO YYMMDD                   
         LA    R3,1                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+3,(R3)     ADD 1 TO GET START DAY          
         GOTO1 VDATCON,DMCB,(0,WORK+3),(8,8(R2))    TO MMMDD/YY                 
         FOUT  (R2)                                                             
         SPACE 1                                                                
EOMFMT10 BAS   RE,NEXTUF           TO END DATE                                  
         LA    R6,2(R6)                                                         
         GOTO1 VDATCON,DMCB,(2,0(R6)),(8,8(R2))     TO MMMDD/YY                 
         FOUT  (R2)                                                             
         SPACE 1                                                                
         CR    R2,R5               R5 IS BOTTOM OF SCREEN                       
         BNL   EOMFMT30                                                         
         SPACE 1                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO NEXT MONTH NAME                     
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT TO NEXT MONTH START DATE               
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(2,0(R6)),(0,WORK)      TO YYMMDD                   
         LA    R3,1                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+3,(R3)     ADD 1 TO GET START DAY          
         GOTO1 VDATCON,DMCB,(0,WORK+3),(8,8(R2))    TO MMMDD/YY                 
         FOUT  (R2)                                                             
         SPACE 1                                                                
         BCT   R4,EOMFMT10                                                      
         SPACE 1                                                                
EOMFMT30 B     EXXMOD                                                           
         EJECT                                                                  
*    EDIT ROUTINE                                                               
EOMEDT   MVC   REC+34(2),=X'011C'  LENGTH OF ELEMENT                            
         MVC   REC+27(2),=Y(62)    LENGTH OF RECORD                             
         LA    R2,EOMSJANH                                                      
         SPACE 1                                                                
         CLI   5(R2),0             MUST INPUT EVERY FIELD                       
         BE    FLERR1                                                           
         SPACE 1                                                                
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK    VALIDATE START DATE               
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         SPACE 1                                                                
         LA    R4,1                                                             
         LNR   R4,R4                                                            
*   CONVERT START DATE OF JAN TO END DATE OF DEC                                
         GOTO1 VADDAY,DMCB,WORK,WORK+3,(R4)                                     
         GOTO1 VDATCON,DMCB,(0,WORK+3),(2,WORK+6)   COMPRESSED                  
         SPACE 1                                                                
*   MOVE REC TO REC 2, IN CASE REC IS NEEDED FOR LAST OR NEXT                   
*   YEAR'S EOM RECORD                                                           
         SPACE 1                                                                
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC                                                       
         SPACE 1                                                                
*   MAKE SURE END OF DEC MATCHES PREVIOUS YEAR RECORD                           
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    EOMEDT50            IF SO, DON'T CHECK LAST YEAR                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'18'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         ZIC   R4,REC+26           YEAR                                         
         BCTR  R4,0                LAST YEAR                                    
         STC   R4,KEY+26                                                        
         SPACE 1                                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   EOMEDT50            NO RECORD FOR LAST YEAR                      
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
         LA    R5,REC                                                           
         USING REOMREC,R5                                                       
         SPACE 1                                                                
         CLC   WORK+6(2),REOMDATE+24                                            
         LA    R3,142              ERROR, START DATE MUST FOLLOW                
         BNE   ERROR               LAST YEAR'S END DATE                         
         DROP  R5                                                               
         SPACE 1                                                                
EOMEDT50 LA    R5,REC2                                                          
         USING REOMREC,R5                                                       
         SPACE 1                                                                
         LA    R6,REOMDATE                                                      
         MVC   0(2,R6),WORK+6                                                   
         LA    R4,12                                                            
EOMEDT60 BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         SPACE 1                                                                
*   IF INPUT IS MONTH/DAY, USE YEAR FROM PREVIOUS MONTH                         
         SPACE 1                                                                
         CLI   5(R2),5                                                          
         BH    EOMEDT68            THEY SUPPLIED YEAR                           
         GOTO1 VDATVAL,DMCB,(1,8(R2)),WORK+12   VALIDATE M/D                    
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         MVC   WORK+2(4),WORK+14                                                
         B     EOMEDT72                                                         
         SPACE 1                                                                
EOMEDT68 GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK      VALIDATE M/D/Y                  
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         SPACE 1                                                                
EOMEDT72 GOTO1 VDATCON,DMCB,(0,WORK),(2,WORK+6)      COMPRESS DATE              
*   THIS MONTH MUST BE LATER THAN LAST MONTH                                    
         CLC   0(2,R6),WORK+6                                                   
         LA    R3,65               ERROR, OVERLAPPING DATES                     
         BNL   ERROR                                                            
         LA    R6,2(R6)                                                         
         MVC   0(2,R6),WORK+6                                                   
         BCT   R4,EOMEDT60                                                      
         DROP  R5                                                               
*                                                                               
*   MAKE SURE END DATE OF DEC MATCHES END DATE OF DEC FOR NEXT YEAR             
*   (IF RECORD EXISTS)                                                          
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    EOMEDT80            IF SO, DON'T CHECK NEXT YEAR                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'18'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         ZIC   R4,REC2+26                                                       
         LA    R4,1(R4)                                                         
         STC   R4,KEY+26                                                        
         SPACE 1                                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   EOMEDT80            NEXT YEAR'S RECORD DOESN'T EXIST             
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
         SPACE 1                                                                
         LA    R5,REC                                                           
         USING REOMREC,R5                                                       
         SPACE 1                                                                
         CLC   WORK+6(2),REOMDATE                                               
         LA    R3,143              ERROR, END DATE MUST IMMEDIATELY             
         BNE   ERROR               PRECEDE NEXT YEAR'S START DATE               
         DROP  R5                                                               
         SPACE 1                                                                
*   MOVE REC2 BACK TO REC                                                       
         SPACE 1                                                                
EOMEDT80 LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,MOVEREC                                                       
         SPACE 1                                                                
         B     FLFILE                                                           
         TITLE 'T80413 - RELFM13 - DMEN / DEMO MENU'                            
* ROUTINE FOR DEMO MENU                                                         
         SPACE 1                                                                
DEM      CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   DEMEDT                                                           
         SPACE 1                                                                
*   FORMAT ROUTINE                                                              
         SPACE 1                                                                
         BAS   RE,GETREC                                                        
DEMFMT   LA    R2,DEMDESH          DESCRIPTION                                  
         MVC   8(L'DEMDES,R2),RDEMDES                                           
         FOUT  (R2)                                                             
         SPACE 1                                                                
         LA    R4,4                                                             
         LA    R2,DEMDEMH          DEMOS                                        
DEM5     MVC   8(L'DEMDEM,R2),SPACES                                            
         FOUT  (R2)                                                             
         BAS   RE,NEXTUF           NEXT DEMO FIELD                              
         BCT   R4,DEM5                                                          
         SPACE 1                                                                
         XC    WORK3(200),WORK3                                                 
         LA    R2,RDEMDEM          DEMO LIST                                    
         LA    R4,WORK3                                                         
         ZIC   R5,RDEMNUM                                                       
         SPACE 1                                                                
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
DEM10    CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 VDEMOCON,DMCB,(0,0(R2)),(6,0(R4)),(0,DBLOCK),0                   
         CLI   1(R2),C'I'          REVERSE FUDGE FOR DEMOCON                    
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         DROP  R8                                                               
         SPACE 1                                                                
         LR    RE,R4                                                            
         LA    RF,6                                                             
DEM13    CLI   0(RE),C' '          PUT COMMA AT END OF DEMO EXPRESSION          
         BE    DEM14                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,DEM13                                                         
DEM14    MVI   0(RE),C','                                                       
         LA    R4,1(RE)                                                         
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'         END OF LIST                                  
         BE    *+8                                                              
         BCT   R5,DEM10                                                         
         SPACE 1                                                                
         LA    R4,WORK3                                                         
         LA    R2,DEMDEMH                                                       
         SPACE 1                                                                
*  BREAK UP WORK3 INTO CHUNKS THAT WILL FIT ON SCREEN PROPERLY                  
         SPACE 1                                                                
DEM15    ZIC   RE,0(R2)            LENGTH OF FIELD                              
         SH    RE,=H'8'            MINUS HEADER                                 
         LA    R5,0(RE,R4)                                                      
DEM20    CLI   0(R5),C','                                                       
         BE    DEM30                                                            
         BCTR  R5,R0               BACK UP ONE                                  
         BCTR  RE,R0                                                            
         B     DEM20                                                            
         SPACE 1                                                                
DEM30    BCTR  RE,R0               BACK UP FROM COMMA                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R4)                                                    
         SPACE 1                                                                
         LR    R4,R5                                                            
         BAS   RE,NEXTUF                                                        
         LA    R4,1(R4)            GET PAST COMMA                               
         OC    0(3,R4),0(R4)       MORE DEMOS                                   
         BNZ   DEM15                                                            
         SPACE 1                                                                
DEMX     B     EXXMOD                                                           
         EJECT                                                                  
DEMEDT   CLI   BACT,C'X'           ACTION DELETE                                
         BNE   DEME10                                                           
*                                                                               
         BAS   RE,READ                                                          
         OI    KEY+27,X'80'        MARK DELETED                                 
         BAS   RE,WRITE                                                         
         B     EXXMOD                                                           
*                                                                               
*   ACTION ADD OR CHANGE                                                        
*                                                                               
DEME10   MVC   REC+34(2),=X'0150'                                               
         MVC   REC+27(2),=Y(114)   TOTAL LENGTH OF KEY AND X'01'                
         SPACE 1                                                                
         LA    R2,DEMDESH          DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   RDEMDES,8(R2)                                                    
         OC    RDEMDES,SPACES                                                   
         XC    RDEMSPR,RDEMSPR                                                  
         SPACE 1                                                                
         XC    WORK2(80),WORK2    BUILD DEMO ELEMENT                            
         XC    WORK3(80),WORK3    FOR DEMOVAL                                   
         LA    R3,WORK2                                                         
         MVI   0(R3),X'02'                                                      
         LA    R2,DEMDEMH          DEMO                                         
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         SPACE 1                                                                
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         SPACE 1                                                                
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
         GOTO1 CDEMOVAL,DMCB,(4,(R2)),(24,WORK3),(0,DBLOCK)                     
         DROP  R7,R8                                                            
         CLI   4(R1),0                                                          
         BE    FLERR2              INVALID INPUT FIELD                          
         MVC   RDEMNUM,4(R1)       SAVE NUMBER OF DEMOS                         
         ZIC   R1,RDEMNUM                                                       
         MH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),WORK3       DEMO LIST TO ELEMENT                         
         LA    R1,RDEMDEM-RDEMDEL+1(R1)                                         
         STC   R1,1(R3)            COMPUTED ELEMENT LENGTH                      
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,REC,(R3)                                           
         B     FLFILE                                                           
         EJECT                                                                  
         TITLE 'T80413 - RELFM13 - GENERAL ROUTINES'                            
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
* RELFMGEN                                                                      
* REGENBUD                                                                      
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
**PAN#1  DC    CL21'069RELFM13   05/01/02'                                      
         END                                                                    
