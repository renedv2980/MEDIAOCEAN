*          DATA SET RESFM0EA   AT LEVEL 065 AS OF 05/01/02                      
*PHASE T8180EA,*                                                                
*INCLUDE NUMVAL                                                                 
         TITLE 'T8180E - RESFM0E - AHEADER AND PHEADER ADD/CHANGE'              
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESFM0E --- ADD/CHANGE FOR AHEADER AND PHEADER             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR05/89 (MRR) --- CHANGE CPP AND RERANK SCREEN FIELDS AND        *           
*                     ACTIVATE THE CPP FIELD                        *           
*                                                                   *           
* APR06/89 (SDE) --- FIX BUG IN COMPUTATION OF NEXT AVAILABLE       *           
*                    PROPOSAL DETAIL NUMBER                         *           
*                                                                   *           
* MAY12/89 (MRR) --- REMOVE CPP AND RERANK FIELDS AND VALIDATION    *           
*                     CODE                                          *           
*                    LIMIT DEMOS TO 7 (SEVEN)                       *           
*                    CHANGE CPP/CPM INDICATOR FROM '$' TO '='       *           
*                                                                   *           
* MAY26/89 (MRR) --- ALLOW AHEADER/PHEADER TO INDICATE A RATING     *           
*                     SERVICE OTHER THAN THAT IN THE CONTRACT.      *           
*                                                                   *           
* AUG01/89 (MRR) --- >PROGRAM DIES IS THERE ARE NO DEMOS ON THE     *           
*                     DEMO LINE                                     *           
*                    >CHANGE ACTION FROM 'ADD' TO 'CHA' IF A HEADER *           
*                     IS ADDED BUT THE DAYPARTS ARE INVALID         *           
*                                                                   *           
* 12JAN90  (EFJ) --- ALLOW TX=ALL AND BK=??? TO BE ADDED IN DPTS    *           
*                    FIELD                                          *           
*                                                                   *           
* FEB27/90 (MRR) --- ONLY ALLOW ONE SERVICE ON THE BOOK LINE        *           
*                                                                   *           
* APR18/90 (MRR) --- CHANGE INVENTORY REC PASSIVE POINTER LABLES    *           
*                     FROM 'RDPT' TO 'RIDP'                         *           
*                                                                   *           
* 07MAY90  (EFJ) --- REMOVE 'SOURCE' AND 'SCH/PER' FIELDS           *           
*                                                                   *           
* 04OCT90  (EFJ) --- ALLOW BK=+- TO BE USED ON AVAILS               *           
*                                                                   *           
* NOV19/90 (MRR+EFJ) SCAN CALL FOR DAYPARTS STUFF BLEW PAST BLOCK,  *           
*                    GIVE IT MY OWN BIG BUFFER                      *           
*                                                                   *           
* 14DEC90  (EFJ) --- LIMIT #OF DEMOS TO 6 ON AVAILS & PROPS         *           
*                                                                   *           
* 17DEC90  (EFJ) --- FIX BUG CAUSING OCCASIONAL DUMP - NOT          *           
*                     CLEARING REC CORRECTLY IN ADDET               *           
*                                                                   *           
*  7DEC95  (BOB) --- ADAPT TO DAYPART RECORDS AND NEW INVENTORY     *           
*                     RECORDS                                       *           
*                                                                   *           
*********************************************************************           
*                                                                               
T8180E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**180E**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         L     R3,=V(NUMVAL)                                                    
         A     R3,RELO                                                          
         ST    R3,ANUMVAL                                                       
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,XRECADD        RECORD HAS BEEN ADDED                        
         BE    XRECA                                                            
         CLI   MODE,XRECPUT        RECORD HAS BEEN PUT                          
         BE    XRECP                                                            
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE CONTRACT  (REQUIRED)                                  *           
*===================================================================*           
         SPACE 1                                                                
         LA    R2,HDRCONH                                                       
         GOTO1 VALICON,DMCB,(R2)                                                
         LA    R2,HDRSTNH          POINT TO CALL LETTERS FIELD                  
         GOTO1 DISPCON,DMCB,(R2)                                                
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE TYPE (REQUIRED)                                       *           
*            A = AVAIL,  P = PROPOSALS                              *           
*===================================================================*           
         SPACE 1                                                                
         MVC   ETYPE,CONREC        MUST BE EITHER 'A' OR 'P'                    
         CLI   ETYPE,C'A'                                                       
         BE    *+14                                                             
         CLI   ETYPE,C'P'                                                       
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED TO THE RECORD FIELD?           
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE SOURCE (REQUIRED)                                     *           
*             I = INVENTORY,  S = SID                               *           
*===================================================================*           
         SPACE 1                                                                
*         LA    R2,HDRSRCH                                                      
*         GOTO1 VALISRC                                                         
         MVI   ESOURCE,C'I'                                                     
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE HEADER NUMBER (REQUIRED UNLESS ACTION IS ADD)         *           
*===================================================================*           
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         MVI   XHDRNUM,0                                                        
         XC    EHDRNUM,EHDRNUM                                                  
         SPACE 1                                                                
         LA    R2,HDRHDRH                                                       
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   ACTNUM,ACTADD       USER SUPPLIES NUMBER ON NON-ADD              
         BNE   ERREND                                                           
         B     VK80                                                             
VK60     MVC   RERROR,=AL2(INVALID)                                             
         CLI   ACTNUM,ACTADD       SYSTEM SUPPLIES NUMBER ON ADD                
         BE    VK80                IGNORE NUMBER IF THEY INPUT ON ADD           
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,XHDRNUM                                                       
         MVC   EHDRNUM,8(R2)       SAVE EBCDIC HEADER NUMBER                    
         SPACE 1                                                                
VK80     XC    KEY,KEY             BUILD RECORD KEY                             
         LA    R6,KEY                                                           
         CLI   ETYPE,C'A'                                                       
         BNE   VK100                                                            
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
         MVI   RAVLKTYP,X'14'      BUILD AVAIL HEADER KEY                       
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         CLI   XHDRNUM,0                                                        
         BE    VK90                                                             
         SPACE 1                                                                
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         B     VK130                                                            
         SPACE 1                                                                
VK90     XC    KEY+25(2),KEY+25    CLEAR OUT SOURCE,DET                         
         SR    R1,R1               GET NEXT AVAIL NUMBER                        
         IC    R1,RAVLKAVN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RAVLKAVN                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(25),KEY     FOUND IT (SOURCE DOESN'T MATTER)             
         BE    VK90                LOOK FOR ANOTHER                             
         MVC   KEY(27),KEYSAVE                                                  
         MVC   RAVLKSRC,ESOURCE                                                 
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
VK100    MVI   RPRPKTYP,X'16'      BUILD PROPOSAL HEADER KEY                    
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         CLI   XHDRNUM,0                                                        
         BE    VK120                                                            
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         B     VK130                                                            
         SPACE 1                                                                
VK120    XC    KEY+22(5),KEY+22    CLEAR OUT SOURCE,PKG,DET                     
         SR    R1,R1               GET NEXT PROPOSAL NUMBER                     
         IC    R1,RPRPKPRP                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RPRPKPRP                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(22),KEY     FOUND IT (SOURCE DOESN'T MATTER)             
         BE    VK120               LOOK FOR ANOTHER                             
         MVC   KEY(27),KEYSAVE                                                  
         MVC   RPRPKSRC,ESOURCE                                                 
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
VK130    CLI   ACTNUM,ACTDEL                                                    
         BNE   VK140                                                            
         BAS   RE,DELPRPAV                                                      
         MVC   RERROR,=H'7'       RECORD HAS BEEN DELETED                       
         MVI   RMSGTYPE,C'I'                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VK140    CLI   ACTNUM,ACTREST                                                   
         BNE   XIT                                                              
         BAS   RE,RESTPRAV                                                      
         MVC   RERROR,=H'8'       RECORD HAS BEEN RESTORED                      
         MVI   RMSGTYPE,C'I'                                                    
         B     ERREND                                                           
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VREC     DS    0H                                                               
         SPACE 1                                                                
*===================================================================*           
* USER HAS CHANCE TO ASK FOR SAR DATA BY INPUTTING C'SAR' IN BOOK,  *           
*  DEMO, AND/OR LENGTH FIELD.  GETSAR ROUTINE WILL CHECK FOR 'SAR'  *           
*  IN 3 INPUT FIELDS.  IF IT IS 'SAR', THE DATA WILL BE DISPLAYED   *           
*  AND AN OKEXIT WILL BE TAKEN.  NO OTHER FIELDS WILL BE VALIDATED  *           
*  UNTIL THE NEXT TRANSACTION.                                      *           
*===================================================================*           
         SPACE 1                                                                
         GOTO1 =A(GETSAR),DMCB,(RC),RR=RELO                                     
         BNZ   ERREND                                                           
         SPACE 1                                                                
*===================================================================*           
*  DELETE ALL OLD ELEMENTS ON THE HEADER RECORD, EXCEPT FOR THE     *           
*     DETAIL LINE ORDER ELEMENT (X'0E' -- FUTURE?)                  *           
*===================================================================*           
         SPACE 1                                                                
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD AVAIL/PROPOSAL HDR ELEM           
         MVI   ELCODE,2                                                         
         GOTO1 REMELEM             AND OLD COMMENT ELEMENT(S)                   
         MVI   ELCODE,5                                                         
         GOTO1 REMELEM             AND OLD UPGRADE ELEMENT(S)                   
         MVI   ELCODE,X'0B'                                                     
         GOTO1 REMELEM             AND OLD BOOK LABEL ELEMENT                   
         MVI   ELCODE,X'0D'                                                     
         GOTO1 REMELEM             AND OLD SID ELEMENT                          
         SPACE 1                                                                
*===================================================================*           
*    NOW REBUILD X'01' AVAIL/PROPOSAL HEADER ELEMENT                *           
*===================================================================*           
         SPACE 1                                                                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RAVLELEM,R6                                                      
         MVI   RAVLCODE,X'01'                                                   
         MVI   RAVLELLN,RAVLELLQ                                                
         MVC   RAVLSRC,CSOURCE     RATING SERVICE                               
         XC    CSOURCE,CSOURCE                                                  
         MVC   CBKLABEL,SPACES                                                  
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE BOOKS - OPTIONAL                                      *           
*                    (BUT IF NO BOOKS, THEN NO DEMOS ALLOWED)       *           
*          MAX 6 BOOKS FOR AVAILS                                   *           
*          MAX 1 BOOK FOR PROPOSALS                                 *           
*===================================================================*           
         SPACE 1                                                                
         LA    R2,HDRBKSH          IF NO BOOKS                                  
         CLI   5(R2),0                                                          
         BNE   VR40                                                             
         LA    R2,HDRDEMH          THEN NO DEMOS ALLOWED                        
         CLI   5(R2),0                                                          
         BNE   VR30                                                             
         LA    R2,HDRPJ1H          AND NO PJ'S ALLOWED                          
         CLI   5(R2),0                                                          
         BNE   VR30                                                             
         LA    R2,HDRPJ1H                                                       
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         SPACE 1                                                                
VR30     MVC   RERROR,=AL2(NOBOOKS)                                             
         B     ERREND                                                           
         SPACE 1                                                                
VR40     XC    CBOOKS,CBOOKS                                                    
         MVI   MAX,10                                                           
         CLI   ETYPE,C'P'                                                       
         BNE   VR50                                                             
         GOTO1 VALIBKL             VALIDATE FOR BOOK LABELS                     
         MVC   CNUMBKS,ACTUAL      SAVE NUMBER OF BOOKS                         
         MVC   RERROR,=AL2(MANYBKS1)                                            
         CLI   ACTUAL,1            REALLY 1 BOOK ALLOWED FOR PROPOSALS          
         BH    ERREND                                                           
         MVC   RAVLBKS,CBOOKS                                                   
         B     VR70                                                             
         SPACE 1                                                                
VR50     EQU   *                                                                
         MVC   RERROR,=AL2(INVBOK)                                              
         ZIC   R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'-'          DISALLOW MINUS SIGNS                         
         BE    ERREND              FOUND ONE                                    
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         SPACE 1                                                                
         IC    R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'='          REPLACE '=' WITH '$'                         
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         IC    R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'$'          REPLACE '$' WITH '-'                         
         BNE   *+8                                                              
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         GOTO1 VALIBKL             VALIDATE FOR BOOK LABELS                     
         MVC   CNUMBKS,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,6            REALLY 6 BOOKS ALLOWED FOR AVAILS            
         BNH   *+14                                                             
         MVC   RERROR,=AL2(MANYBKS6)                                            
         B     ERREND                                                           
         SPACE 1                                                                
         ZIC   R0,CNUMBKS                                                       
         LA    RF,CBOOKS                                                        
         XI    0(RF),X'80'         FLIP CPP/CPM BITS                            
         LA    RF,3(RF)                                                         
         BCT   R0,*-8                                                           
         MVC   RAVLBKS,CBOOKS                                                   
         SPACE 1                                                                
         IC    R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'-'          RESTORE $ SIGNS                              
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         IC    R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'$'          RESTORE = SIGNS                              
         BNE   *+8                                                              
         MVI   0(RF),C'='                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         CLI   ACTUAL,1            CHECK FOR >1 SERVICE ON BOOKS                
         BE    VR70                ONLY 1 BOOK, NO CHECKING                     
         MVC   RERROR,=AL2(SINGSVC)                                             
         XC    BLOCK(18),BLOCK                                                  
         MVC   BLOCK(18),CBOOKS                                                 
         ZIC   RF,ACTUAL                                                        
         BCTR  RF,0                                                             
         LR    R0,RF                                                            
         LA    RF,BLOCK                                                         
VR60     EQU   *                                                                
         NI    0(RF),X'41'         ONLY LOOK AT SERVICE BITS                    
         NI    3(RF),X'41'                                                      
         CLC   0(1,RF),3(RF)                                                    
         BNE   ERREND                                                           
         LA    RF,3(RF)                                                         
         BCT   R0,VR60                                                          
         XC    BLOCK(18),BLOCK                                                  
         XC    RERROR(2),RERROR                                                 
VR70     EQU   *                                                                
         MVC   CSOURCE(1),HDRBKS                                                
         MVC   RAVLSRC,CSOURCE     RATING SERVICE                               
         SPACE 1                                                                
* IF CBKLABEL IS NOT SPACES, BUILD X'0B' (BOOK LABELS) ELEMENT LATER            
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE DEMOS                                                 *           
*===================================================================*           
         SPACE 1                                                                
         LA    R2,HDRDEMH                                                       
         XC    CDEMOS,CDEMOS                                                    
         XC    RAVLDEM,RAVLDEM                                                  
         MVI   CNUMDEM,0                                                        
*                                                                               
         ZICM  R0,5(R2),1                                                       
         BZ    VR80                     NO DEMOS                                
*                                                                               
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'='                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         MVI   MAX,24              FUDGE SO I CAN DO ERR MESSAGE                
         GOTO1 ANY                                                              
         GOTO1 VALIDEM,DMCB,1                                                   
         MVC   CNUMDEM,ACTUAL      SAVE NUMBER OF DEMOS                         
         CLI   ACTUAL,6                                                         
         BNH   *+14                                                             
         MVC   RERROR,=AL2(MANYDEM)                                             
         B     ERREND                                                           
         SPACE 1                                                                
         ZIC   RE,ACTUAL           NUMBER OF DEMOS                              
         MH    RE,=H'3'                                                         
         BCTR  RE,0                ELIMINATE EOL X'FF' MARKER                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RAVLDEM(0),CDEMOS   SAVE DEMOS                                   
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE LENGTHS                                               *           
*        IF AVAILS, UP TO 6 LENGTHS ARE ALLOWED AND                 *           
*                   EACH CAN HAVE A CLASS NUMBER                    *           
*        IF PROPOSALS, ONLY 1 LENGTH ALLOWED, AND NO CLASS          *           
*===================================================================*           
         SPACE 1                                                                
VR80     EQU   *                                                                
         LA    R2,HDRLENH                                                       
         GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   ETYPE,C'A'                                                       
         BE    VR90                                                             
         SPACE 1                                                                
         GOTO1 VPACK               FOR PROPOSALS, ONLY ONE LENGTH               
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,RAVLPLEN         SAVE SPOT LENGTH                             
         B     VR120                                                            
         SPACE 1                                                                
VR90     XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK),C',=,.'                              
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    ERREND                                                           
         MVC   RERROR,=AL2(MANYLEN)                                             
         CH    R3,=H'6'            MAXIMUM OF 6 LENGTHS FOR AVAILS              
         BH    ERREND                                                           
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         LA    RF,BLOCK            A(SCANNER BLOCK)                             
         LA    R1,RAVLRFRM         A(ELEMENT)                                   
         SPACE 1                                                                
VR100    TM    2(RF),X'80'         EDIT LENGTH                                  
         BNO   ERREND              NOT NUMERIC                                  
         ICM   RE,15,4(RF)                                                      
         BZ    ERREND              SPOT LENGTH ZERO                             
         CH    RE,=H'240'                                                       
         BH    ERREND              SPOT LENGTH TOO HIGH                         
         STC   RE,1(R1)            SAVE SPOT LENGTH                             
         SPACE 1                                                                
         CLI   1(RF),0             ANY CLASS GIVEN?                             
         BE    VR110               NO                                           
         TM    3(RF),X'80'                                                      
         BNO   ERREND              NOT NUMERIC                                  
         ICM   RE,15,8(RF)                                                      
         BZ    ERREND              CLASS ZERO                                   
         CH    RE,=H'9'                                                         
         BH    ERREND              CLASS > 9 NOT ALLOWED                        
         STC   RE,0(R1)            SAVE CLASS NUMBER                            
         SPACE 1                                                                
VR110    LA    R1,2(R1)            NEXT SLOT IN ELEMENT                         
         LA    RF,32(RF)           NEXT ENTRY IN SCANNER BLOCK                  
         BCT   R3,VR100                                                         
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE WEEKS - TO OVERRIDE CONTRACT WEEKS                    *           
*                  - OPTIONAL -- ONLY APPEARS ON PROPOSALS          *           
*                  - IF BLANK, GETS FILLED IN                       *           
*===================================================================*           
         SPACE 1                                                                
VR120    MVC   RAVLWKS,CCONWKS     USE CONTRACT WEEKS AS DEFAULT                
         CLI   ETYPE,C'P'          TEST PROPOSALS                               
         BNE   VR160               NO -- SKIP WEEKS AND DATES FIELD             
         SPACE 1                                                                
         LA    R2,HDRWKSH                                                       
         CLI   5(R2),0             THEY CAN INPUT THEIR OWN WEEKS               
         BNE   VR130                                                            
         ZIC   R3,RAVLWKS          PUT CONTRACT WEEKS TO SCREEN                 
         EDIT  (R3),(2,8(R2)),ALIGN=LEFT                                        
         OI    4(R2),X'08'         VALID NUMERIC                                
         STC   R0,5(R2)            NEW LENGTH OF FIELD                          
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VR140                                                            
         SPACE 1                                                                
VR130    MVC   RERROR,=AL2(INVALID) INPUT IS NUMBER OF WEEKS                    
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,RAVLWKS                                                       
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE CONTRACT OVERRIDE DATES                               *           
*                  - OPTIONAL -- APPEARS ONLY ON PROPOSALS          *           
*===================================================================*           
         SPACE 1                                                                
VR140    LA    R2,HDRDTSH          CONTRACT OVERRIDE DATES                      
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=-='                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   DMCB+4,2                                                         
         BNE   ERREND                                                           
         SPACE 1                                                                
         LA    R3,BLOCK            INPUT IS START-END DATES                     
         GOTO1 DATVAL,DMCB,(0,12(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         LA    R5,RAVLDATO                                                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,(R5))  START DATE                        
         SPACE 1                                                                
         LA    R3,32(R3)                                                        
         GOTO1 DATVAL,DMCB,(0,12(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,3(R5))  END DATE                         
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         CLC   RAVLDATO+3(3),RAVLDATO                                           
         BL    ERREND              START HIGHER THAN END DATE                   
VR160    EQU   *                                                                
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE COST - (TOTAL COST OF PROPOSAL)                       *           
*                  - OPTIONAL -- ONLY APPEARS ON PROPOSALS          *           
*===================================================================*           
         SPACE 1                                                                
VR165    LA    R2,HDRCOSH          NOTE -- PROTECTED FIELD FOR AVAILS           
         CLI   5(R2),0                                                          
         BE    VR180                                                            
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STCM  R0,15,RAVLCOST                                                   
         SPACE 1                                                                
VR180    GOTO1 ADDELEM             ADD X'01' ELEMENT                            
         SPACE 1                                                                
*===================================================================*           
*    IF THERE WERE BOOK LABELS, ADD THE X'0B' ELEMENT               *           
*===================================================================*           
         SPACE 1                                                                
         CLC   CBKLABEL,SPACES                                                  
         BE    VR190                                                            
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RAVLBEL,R6                                                       
         MVI   RAVLBCOD,X'0B'                                                   
         ZIC   RE,CNUMBKS                                                       
         MH    RE,=H'5'            LENGTH OF LABEL                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RAVLBLAB(0),CBKLABEL                                             
         LA    RE,3(RE)            TOTAL ELEMENT LENGTH                         
         STC   RE,RAVLBLEN                                                      
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD X'0B' ELEMENT                            
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE PROJECTION EXPRESSIONS                                *           
*         IF SOURCE IS 'I', SCHEME/PERIOD FIELDS ARE INVALID        *           
*               AND PJ FIELDS ARE OPTIONAL                          *           
*      VALID INPUT FOR PJ IS B#=ANY VALID UPGRADE EXPRESSION        *           
*                (WHERE # INDICATES BOOK THE UPGRADE REFERS TO)     *           
*                                                                   *           
*         IF SOURCE IS 'S', FIRST SCHEME/PERIOD FIELD IS REQUIRED   *           
*               AND OTHER SCHEME/PERIOD AND PJ FIELDS ARE OPTIONAL  *           
*      VALID INPUT FOR SCHEME/PERIOD FIELD IS                       *           
*                 SCHEME/PERIOD OR SCHEME/PERIOD-YEAR               *           
*      VALID INPUT FOR PJ IS UPT=ANY VALID UPGRADE EXPRESSION       *           
*                                                                   *           
*           IF THERE IS AN UPGRADE  ON THE SCREEN,                  *           
*           IT OVERRIDES THE UPGRADE FROM THE SCHEME                *           
*===================================================================*           
         SPACE 1                                                                
VR190    DS    0H                                                               
*VR190    LA    R2,HDRSP1H                                                      
*         CLI   ESOURCE,C'S'                                                    
*         BE    VR200                                                           
*         MVC   RERROR,=AL2(INVALID)                                            
*         CLI   5(R2),0                                                         
*         BNE   ERREND                                                          
*         LA    R2,HDRSP2H          NOTE -- SP2 PROTECTED FOR PROPOSALS         
*         CLI   5(R2),0                                                         
*         BNE   ERREND                                                          
*         B     VR220                                                           
         SPACE 1                                                                
*VR200    MVC   RERROR,=AL2(MISSING)                                            
*         CLI   5(R2),0                                                         
*         BE    ERREND                                                          
         SPACE 1                                                                
*         GOTO1 SSPOT               SWITCH TO SPOT                              
*         GOTO1 VALISID                                                         
         SPACE 1                                                                
*         LA    R2,HDRSP2H          NOTE -- SP2 PROTECTED FOR PROPOSALS         
*         CLI   5(R2),0                                                         
*         BE    VR210                                                           
*         GOTO1 VALISID                                                         
*VR210    GOTO1 SREP                SWITCH BACK TO REP                          
         SPACE 1                                                                
*         XC    ELEM,ELEM           BUILD X'0D' (SID) ELEMENT                   
*         LA    R6,ELEM                                                         
*         USING RAVLSID,R6                                                      
*         MVC   RAVLSCD(2),=X'0D12'                                             
*         MVC   RAVLSS1(16),CSCHEME                                             
*         GOTO1 ADDELEM             ADD X'0D' ELEMENT                           
         SPACE 1                                                                
VR220    LA    R2,HDRPJ1H          1ST PJ FIELD IS OPTIONAL                     
         CLI   5(R2),0                                                          
         BE    VR230                                                            
         LA    R3,CUPTYP1                                                       
         GOTO1 VALIUPT             VALIDATE FIELD & BUILD 05 ELEMENT            
         SPACE 1                                                                
VR230    LA    R2,HDRPJ2H          2ND PJ FIELD IS OPTIONAL. . .                
         CLI   5(R2),0              . . . AND PROTECTED FOR PROPOSALS           
         BE    VR250                                                            
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   HDRPJ1H+5,0                                                      
         BE    ERREND              PJ2 NOT ALLOWED IF NO PJ1 GIVEN              
         SPACE 1                                                                
*         CLI   ESOURCE,C'S'        AND IF SOURCE IS S                          
*         BNE   *+16                                                            
*         LA    R3,HDRSP1H          AND IF NO 2ND SCHEME/PER, NO PJ             
*         CLI   5(R3),0                                                         
*         BE    ERREND                                                          
         SPACE 1                                                                
         LA    R3,CUPTYP2                                                       
         GOTO1 VALIUPT             VALIDATE FIELD & BUILD 05 ELEMENT            
         SPACE 1                                                                
*===================================================================*           
*    VALIDATE COMMENTS                                              *           
*===================================================================*           
         SPACE 1                                                                
VR250    LA    R2,HDRCOM1H                                                      
         LA    R5,1                                                             
         LA    R3,2                                                             
         SPACE 1                                                                
VR260    CLI   5(R2),0                                                          
         BE    VR270                                                            
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RAVLCEL,R6                                                       
         MVI   RAVLCCOD,2                                                       
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         LA    RE,3(RE)                                                         
         STC   RE,RAVLCLEN         ELEMENT LENGTH                               
         STC   R5,RAVLCSEQ                                                      
         SH    RE,=H'3'            OVERHEAD LENGTH                              
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RAVLCOMM(0),8(R2)   DATA TO ELEMENT                              
         SPACE 1                                                                
         DROP  R6                                                               
         GOTO1 ADDELEM                                                          
VR270    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    R5,1(R5)                                                         
         BCT   R3,VR260            DO NEXT COMMENT LINE                         
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
DREC     BAS   RE,CLEARREC         CLEAR ALL DATA FIELDS                        
         SPACE 1                                                                
         XC    A0BELEM,A0BELEM                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'0B'        GET BOOK LABEL ELEMENT, IF ANY               
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         ST    R6,A0BELEM          SAVE ADDRESS OF BOOK LABEL ELEMENT           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RAVLELEM,R6                                                      
         SPACE 1                                                                
         LA    R2,HDRBKSH                                                       
         OC    RAVLBKS(3),RAVLBKS  IF NO BOOKS,                                 
         BZ    DR20                THEN NO DEMOS EITHER                         
         CLI   ETYPE,C'A'                                                       
         BE    DR10                                                             
         MVI   MAX,1                                                            
         GOTO1 DISPBKL,DMCB,RAVLBKS,A0BELEM,0,0                                 
         B     DR15                                                             
DR10     MVI   MAX,6                                                            
         GOTO1 DISPBKL,DMCB,(C'$',RAVLBKS),A0BELEM,0,0                          
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         LA    R2,8(R2)                                                         
DR11     EQU   *                                                                
         CLI   0(R2),C'$'                                                       
         BNE   DR12                                                             
         MVI   0(R2),C'='                                                       
DR12     EQU   *                                                                
         LA    R2,1(R2)                                                         
         BCT   R0,DR11                                                          
         SPACE 1                                                                
DR15     LA    R2,HDRDEMH          DEMO FIELD                                   
         XC    CDEMOS,CDEMOS                                                    
         LA    R5,CDEMOS                                                        
         MVC   0(L'RAVLDEM,R5),RAVLDEM    DEMOS + ENDING ZERO                   
         MVI   MAX,6                                                            
         GOTO1 DISPDEM,DMCB,CDEMOS                                              
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         LA    R2,8(R2)                                                         
DR16     EQU   *                                                                
         CLI   0(R2),C'$'                                                       
         BNE   DR17                                                             
         MVI   0(R2),C'='                                                       
DR17     EQU   *                                                                
         LA    R2,1(R2)                                                         
         BCT   R0,DR16                                                          
         SPACE 1                                                                
DR20     LA    R2,HDRLENH                                                       
         CLI   ETYPE,C'P'                                                       
         BE    DR30                                                             
         MVI   MAX,6                                                            
         GOTO1 DISPLEN,DMCB,RAVLRFRM                                            
         B     DR55                SKIP COST, DATES, WEEKS                      
         SPACE 1                                                                
DR30     MVC   HALF+1(1),RAVLPLEN  BUILD TO LOOK LIKE CLASS/LENGTH              
         MVI   HALF,0                                                           
         MVI   MAX,1                                                            
         GOTO1 DISPLEN,DMCB,HALF                                                
         SPACE 1                                                                
         OC    RAVLCOST,RAVLCOST                                                
         BZ    DR40                                                             
         LA    R2,HDRCOSH                                                       
         ICM   R5,15,RAVLCOST                                                   
         EDIT  (R5),(7,8(R2)),ALIGN=LEFT                                        
         SPACE 1                                                                
DR40     OC    RAVLDATO,RAVLDATO                                                
         BZ    DR50                                                             
         LA    R2,HDRDTSH          CONTRACT OVERRIDE DATES                      
         GOTO1 DATCON,DMCB,(3,RAVLDATO),(8,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RAVLDATO+3),(8,17(R2))                            
         SPACE 1                                                                
DR50     LA    R2,HDRWKSH          WEEKS                                        
         ZIC   R5,RAVLWKS                                                       
         EDIT  (R5),(2,8(R2)),ALIGN=LEFT                                        
         SPACE 1                                                                
DR55     EQU   *                                                                
         SPACE 1                                                                
DR60     LA    R2,HDRCOM1H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR70     BAS   RE,NEXTEL                                                        
         BNE   DR80                                                             
         SPACE 1                                                                
         SR    R3,R3                                                            
         IC    R3,1(R6)                                                         
         SH    R3,=H'3'            FOR OVERHEAD                                 
         BCTR  R3,0                FOR EX                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),3(R6)                                                    
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT R2 TO HDRCOM2H                         
         B     DR70                                                             
         SPACE 1                                                                
DR80     L     R6,AIO                                                           
         LA    R2,HDRPJ1H                                                       
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         GOTO1 VUNUPGR,DMCB,(1,(R2)),(R6),UNSCAN,DEMOCON,(R3)                   
         SPACE 1                                                                
         L     R6,AIO                                                           
         LA    R2,HDRPJ2H                                                       
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         GOTO1 VUNUPGR,DMCB,(2,(R2)),(R6),UNSCAN,DEMOCON,(R3)                   
         SPACE 1                                                                
*  DO DISPLAY OF SCHEME/PERIOD HERE                                             
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              MODE=XRECADD                                                     
****************************************************************                
****************************************************************                
         SPACE 1                                                                
XRECA    LA    R2,HDRHDRH          PUT HEADER NUMBER TO SCREEN                  
         OI    6(R2),X'80'         TRANSMIT                                     
         L     R6,AIO              AVAIL/PROPOSAL HEADER                        
         SPACE 1                                                                
         CLI   ETYPE,C'P'                                                       
         BNE   XRECA10                                                          
         USING RPRPREC,R6                                                       
         ZIC   R3,RPRPKPRP                                                      
         DROP  R6                                                               
         EDIT  (R3),(3,8(R2)),ALIGN=LEFT                                        
         MVC   EHDRNUM,8(R2)       SAVE EBCDIC HEADER NUMBER                    
         B     XRECADDX                                                         
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
XRECA10  ZIC   R3,RAVLKAVN                                                      
         DROP  R6                                                               
         EDIT  (R3),(3,8(R2)),ALIGN=LEFT                                        
         MVC   EHDRNUM,8(R2)       SAVE EBCDIC HEADER NUMBER                    
         SPACE 1                                                                
XRECADDX MVC   AIO,AIO1            RESTORE AIO                                  
         BAS   RE,DPT              VALIDATE DAYPARTS - ADD DETAIL LINES         
         SPACE 2                                                                
****NOTE TO DAVID - DPT MAY NOT RETURN HERE, SO RRK ROUTINE HAS TO BE           
*** REARRANGED                                                                  
         SPACE 2                                                                
         B     XIT                                                              
         SPACE 5                                                                
****************************************************************                
****************************************************************                
*              MODE=XRECPUT                                                     
****************************************************************                
****************************************************************                
         SPACE 1                                                                
XRECP    MVC   AIO,AIO1            RESTORE AIO                                  
         BAS   RE,DPT              VALIDATE DAYPARTS - ADD DETAIL LINES         
         SPACE 2                                                                
         B     XIT                                                              
         EJECT                                                                  
*===================================================================*           
*    VALIDATE DAYPARTS                                              *           
*===================================================================*           
         SPACE 1                                                                
DPT      NTR1                                                                   
         LA    R2,HDRDPT1H                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         BAS   RE,MULTADD                                                       
         SPACE 1                                                                
************** MULTADD NEVER RETURNS SO XIT IS USELESS!! *********              
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
* SUB-ROUTINE FOR MULTIPLE ADD OF DETAIL RECORDS                    *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
MULTADD  NTR1                                                                   
         LA    R4,BUFF             INITIALIZE SPECIAL WORKING STORAGE           
         LA    R4,1000(R4)                                                      
         LA    R4,1000(R4)         DON'T CREAM ITEMTAB                          
         USING MULTHDRD,R4                                                      
         LR    RE,R4                                                            
         LH    RF,=H'4000'                                                      
         XCEF                                                                   
         SPACE                                                                  
*   SATELLITE STATION - GET FROM OPTIONS - PUT T,1 OR 2 INTO TVSAT              
         SPACE                                                                  
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 =A(SCAN),DMCB,(RC),RR=RELO   BLDS ENTRIES IN 'FILTER'            
         BNE   MSG30                                                            
         SPACE                                                                  
* IF AVAILS, USE CONTRACT START-END DATES                                       
* IF PROPOSALS, USE CONTRACT S/E DATES UNLESS OVERRIDDEN BY RAVLDATO            
* FOR EITHER, CONVERT  S/E TO COMPRESSED AND FIND FIRST DETAIL NUMBER           
*    FOR MULTIPLE ADD                                                           
*                                                                               
         L     R6,AIO              POINT TO HEADER RECORD JUST BUILT            
         MVC   WORK(6),CCONDAT                                                  
         CLI   ETYPE,C'P'                                                       
         BNE   MA10                                                             
         USING RAVLREC,R6                                                       
         OC    RAVLDATO,RAVLDATO                                                
         BZ    MA10                                                             
         MVC   WORK(6),RAVLDATO                                                 
         DROP  R6                                                               
         SPACE                                                                  
MA10     GOTO1 DATCON,DMCB,(3,WORK),(2,HDRST)                                   
         GOTO1 (RF),(R1),(3,WORK+3),(2,HDREND)                                  
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)       BORROW KEY FROM HEADER                       
         LA    R6,KEY                                                           
         MVC   AIO,AIO2            USE IO2 FOR MISC. READING                    
         SPACE                                                                  
         CLI   ETYPE,C'A'                                                       
         BE    MA50                                                             
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
         GOTO1 HIGH                RE-READ HEADER RECORD                        
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 IT SHOULD BE THERE                           
         DC    H'0'                                                             
         MVI   XDETNUM,0                                                        
         XC    DETKEY,DETKEY                                                    
         MVC   DETKEY(23),KEY                                                   
         SPACE 1                                                                
MA20     OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(23),KEYSAVE     SAME PROPOSAL                                
         BNE   MA30                                                             
         CLC   XDETNUM,KEY+26                                                   
         BH    MA20                                                             
         MVC   XDETNUM,KEY+26      SAVE HIGHEST NUMBER                          
         B     MA20                                                             
         SPACE 1                                                                
MA30     MVC   DETKEY+23(2),=X'FFFF'                                            
         MVC   DETKEY+26(1),XDETNUM                                             
         ZIC   R1,XDETNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,FIRST                                                         
         B     MA60                                                             
         DROP  R6                                                               
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
MA50     ZIC   R1,RAVLKDET                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RAVLKDET                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY     KEEP READING IF RECORD IS FOUND              
         BE    MA50                                                             
         CLC   KEYSAVE(26),KEY     KEEP READING IN EVENT OF                     
         BE    MA50                SAME HEADER-MISSING LINE NUMBER              
         MVC   DETKEY,KEYSAVE      SAVE LAST KEY PASSED TO DATAMGR              
         MVC   FIRST,KEYSAVE+26    AND ITS DETAIL NUMBER                        
         ZIC   R1,KEYSAVE+26                                                    
         DROP  R6                                                               
         SPACE 1                                                                
MA60     BCTR  R1,0                                                             
         MVI   BYTE,C'N'                                                        
         LA    R0,250              IF LAST NUMBER IS 250 OR ABOVE, BAR          
         CR    R1,R0               ACCESS TO MULTIPLE ADD                       
         BNL   MSG                                                              
         SR    R0,R1               250 LESS LAST KEY NUMBER IS                  
         STC   R0,MAXINV           MAXIMUM SIZE OF INVENTORY LIST.              
         STC   R1,DETKEY+26        RESTORE HIGHEST DETAIL KEY SO FAR.           
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* BROWSE THROUGH INVENTORY USING FILTER ELEMENTS BUILT BY SCAN TO   *           
* CONSTRUCT DAYPART POINTERS.  A LIST OF INVENTORY NUMBERS MEETING  *           
* THE FILTER CRITERIA IS KEPT.  ONE AVAIL DETAIL RECORD IS CREATED  *           
* FOR EACH MEMBER OF THE LIST.                                      *           
*                                                                   *           
* R2-USED FOR KEY COMPARE LENGTH   R3-COUNTS FILTER ELEMENTS        *           
* R5-POINTS TO FILTERS             R6-POINTS TO KEY OR RECORD       *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
         LA    R5,FILTERS                                                       
         USING FILTD,R5                                                         
         ZIC   R3,FILCOUNT                                                      
         SPACE 2                                                                
* BUILD THE DAYPART POINTER KEY AT EACH CHANGE IN FILTER                        
*                                                                               
MA80     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RIDPKEY,R6                                                       
         SPACE                                                                  
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RIDPKSTA,CCONKSTA                                                
         CLI   RIDPKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RIDPKSTA+4,C'T'     INVENTORY USES T, NOT BLANK                  
         SPACE 1                                                                
         MVC   RIDPKSTA+4(1),TVSAT                                              
         OC    FILTDAT,FILTDAT                                                  
         BNZ   *+16                                                             
         MVC   FILTST,HDRST        USE THE CONTRACT START END TO                
         MVC   FILTEND,HDREND      FILTER IF DATE FILTER WAS NOT INPUT.         
         SPACE                                                                  
         XC    TIMRANGE,TIMRANGE                                                
         MVC   TIMRANGE,FILTTIME                                                
*                                                                               
* COMPLETE THE KEY                                                              
*                                                                               
         LA    R2,RIDPKDPT-RIDPKEY                                              
         MVC   RIDPKDPT,FILTDPT                                                 
*                                                                               
* READ DAYPART POINTERS                                                         
*                                                                               
MA110    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    MA140                                                            
         B     MA130               NEXT FILTER                                  
         SPACE                                                                  
MA120    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    MA140                                                            
         B     MA130               NEXT FILTER                                  
         SPACE 2                                                                
* END OF FILTER ROUTINE                                                         
*                                                                               
MA130    LA    R5,L'FILENTRY(R5)   UPDATE FILTER POINTER                        
         BCT   R3,MA80                                                          
         B     ADDDET                                                           
         SPACE 2                                                                
         DROP  R6                                                               
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* ROUTINE TO READ INVENTORY RECORD AND CHECK DATES AND TIME IF A    *           
* TIME FILTER WAS USED.                                             *           
*-------------------------------------------------------------------*           
*                                                                               
MA140    L     R6,AIO              (STILL USING IO2)                            
*                                                                               
         OI    DMINBTS,X'08'       ASK FOR DELETED RECORDS (PLEASE)             
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'                                                    
         TM    29(R6),X'80'        TEST IF KEN SCREWED IT UP                    
         BO    MA120               YES - IT'S DELETED                           
*                                                                               
         LA    R6,34(R6)                                                        
         USING RINVPEL,R6                                                       
         SPACE                                                                  
         CLC   RINVPEFF(2),FILTEND      PROGRAM ST VS. FILTER END               
         BH    MA120                    REJECTED                                
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAEFFX                   NO PROGRAM END                          
         CLC   RINVPEFF+2(2),FILTST     PROGRAM END VS. FILTER START            
         BL    MA120                    REJECTED                                
*                                                                               
MAEFFX   DS    0H                                                               
*                                                                               
*-------------------------------------------------------------------*           
* CHECK FOR TIME FIT                                                *           
*                                                                   *           
* TIME FILTERING BREAKS DOWN TO 4 CASES.  IF TIMST LT TIMEND, THEN  *           
* THEN USE MA170 LOGIC IF PROGRAM START LE PROGRAM END OR REJECT    *           
* OTHERWISE FOR TIMST GT TIMEND (OVERNIGHT), USE MA170 LOGIC        *           
* FOR AN OVERNIGHT PROGRAM ELSE USE MA160 IF PROGRAM BOTH STARTS    *           
* AND ENDS IN AM OR PM.                                             *           
*-------------------------------------------------------------------*           
*                                                                               
*        FILTER ON DAYS/TIMES IF NEEDED                                         
*                                                                               
         MVI   ELCODE,X'02'        SEARCH FOR DAY/TIME ELEMENT                  
         BAS   RE,GETEL                                                         
*                                                                               
MADTMLP  DS    0H                                                               
*                                                                               
         BNE   MADTMDN             END OF DAY/TIME ELMS - REJECT                
*                                                                               
         CLI   FILTDAY,0           SKIP IF NO DAYPART FILTER                    
         BE    MADTMDYX                                                         
*                                                                               
         TM    FILTCNTL,NODAYS     SKIP IF NODAYS OPTION                        
         BO    MADTMDYX                                                         
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         CLC   RIDTDAY,FILTDAY     KEEP IF DAY MATCHES                          
         BNE   MADTMCN                                                          
*                                                                               
MADTMDYX DS    0H                                                               
*                                                                               
         OC    TIMRANGE,TIMRANGE   OKAY IF NO TIME FILTER ITS A HIT             
         BZ    MADTMFD                                                          
*                                                                               
         MVC   PROGTIM,RIDTTIME    COPY TIME PERIOD                             
*                                                                               
         OC    PROGEND,PROGEND                                                  
         BNZ   *+10                                                             
         MVC   PROGEND,PROGST                                                   
*                                                                               
         CLC   TIMST,TIMEND                                                     
         BH    MA160                                                            
*                                                                               
         CLC   PROGST,PROGEND                                                   
         BH    MADTMDN             REJECT                                       
*                                                                               
         B     MA170                                                            
*                                                                               
MA160    CLC   PROGST,PROGEND                                                   
         BH    MA170               NOT AN OVERNIGHT PROGRAM                     
*                                                                               
         CLC   PROGST,=H'1200'     PROGST LE PROGEND, NOW DECIDE                
         BL    *+18                WHETHER IT STARTS IN AM OR PM.               
         CLC   PROGST,TIMST        IF PORGRAM STARTS ON OR AFTER                
         BL    MADTMCN             FILTER, THEN IT MUST END BEFORE              
         B     MADTMFD             FILTER END WHICH IS IN NEXT AM.              
*                                                                               
         CLC   PROGEND,TIMEND                                                   
         BH    MADTMCN             REJECT                                       
         B     MADTMFD             OK.                                          
*                                                                               
MA170    CLC   PROGST,TIMST                                                     
         BL    MADTMCN                                                          
         CLC   PROGEND,TIMEND                                                   
         BH    MADTMCN                                                          
*                                                                               
         B     MADTMFD                                                          
*                                                                               
MADTMCN  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
*                                                                               
         B     MADTMLP                                                          
*                                                                               
MADTMDN  DS    0H                                                               
         B     MA120               REJECT INVENTORY RECORD                      
*                                                                               
* ADD ACCEPTED INVENTORY TO LIST                                                
*                                                                               
         DROP  R6                                                               
MADTMFD L      R6,AIO              RESET R6 TO RECORD START                     
         USING RINVREC,R6                                                       
         SPACE 1                                                                
         CLI   INVCOUNT,0          IS THERE A LIST YET?                         
         BE    MA210                                                            
         ZIC   R1,INVCOUNT         SEE IF RECORD IS ALREADY IN LIST             
         LA    RE,LIST                                                          
MA190    DS    0H                                                               
         USING LISTD,RE            ESTABLISH INVENTORY LIST                     
         CLC   RINVKINV,LSTINV     MATCH ON INV NUMBER                          
         BNE   *+14                                                             
         CLC   RINVKSTD,LSTSTD     MATCH ON INV EFFECTIVE START DATE            
         BE    MA120               IF YES, RESUME FILE READ                     
*                                                                               
         LA    RE,LISTL(RE)        BUMP TO NEXT ITEM IN LIST                    
         BCT   R1,MA190                                                         
         SPACE 1                                                                
MA210    ZIC   R1,INVCOUNT         OTHERWISE, ADD RECORD TO LIST                
         LR    RF,R1                                                            
         LA    RF,1(RF)            INCREMENT LIST SIZE COUNT                    
         CLM   RF,1,MAXINV         TEST FOR LIST LIMIT                          
         BH    ADDDET              PAST LIMIT                                   
         STC   RF,INVCOUNT                                                      
         MH    R1,=Y(LISTL)        DISPLACEMENT INTO LIST                       
         LA    RE,LISTL(R1)        POINT TO NEXT POSITION IN LIST               
         MVC   LSTINV,RINVKINV     SAVE INVENTORY NUMBER                        
         MVC   LSTSTD,RINVKSTD     INVENTORY EFFECTIVE START DATE               
* 23JAN90      *** START ***                                                    
         MVC   LSTBK,FILTBK                                                     
         MVC   LSTBKF,FILTBKF                                                   
         MVC   LSTTX,FILTTX                                                     
* 23JAN90      *** END ***                                                      
         DROP  R5,R6,RE                                                         
         B     MA120               RESUME FILE READ                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
* BUILD DETAIL RECORD ONCE - THEN GO INTO A LOOP INCREMENTING THE   *           
* DETAIL NUMBER AND REPLACING INVENTORY NUMBER EACH TIME AND ADDING *           
* THE RECORD                                                        *           
*-------------------------------------------------------------------*           
*                                                                               
ADDDET   MVI   BYTE,C'Y'                                                        
         CLI   INVCOUNT,0                                                       
         BE    MSG                 NO AVAILS TO ADD                             
         SPACE                                                                  
         L     R6,AIO              NOW USE IO2 TO BUILD DETAIL RECS             
         LR    RE,R6               CLEAR IT OUT                                 
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE                                                                  
         CLI   ETYPE,C'P'          PROPOSALS?                                   
         BE    AD20                YES                                          
*                                                                               
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
         MVC   RAVLKEY,DETKEY                                                   
         LA    R1,34               LENGTH OF KEY +                              
         LA    R1,RAVLDLLQ(R1)     LENGTH OF DETAIL ELEMENT =                   
         STH   R1,HALF                                                          
         MVC   RAVLLEN,HALF        RECORD LENGTH                                
         SPACE 1                                                                
         LA    R5,34(R6)           A(FIRST ELEMENT)                             
         USING RAVLDEL,R5                                                       
         MVI   RAVLDCOD,X'01'                                                   
         MVI   RAVLDLEN,RAVLDLLQ                                                
         SPACE                                                                  
         ZIC   R3,INVCOUNT                                                      
         LA    R2,LIST             R2 POINTS TO LIST                            
         SPACE                                                                  
AD10     DS    0H                                                               
         USING LISTD,R2            ESTABLISH INV REC LIST                       
*                                                                               
         ZIC   R1,RAVLKDET         BUMP DETAIL NUMBER                           
         LA    R1,1(R1)                                                         
         STC   R1,RAVLKDET         AND MOVE IN THE NEXT INVENTORY               
         MVC   RAVLDINV,LSTINV     NUMBER IN LIST                               
         MVC   RAVLDATE,LSTSTD     EFFECTIVE START DATE                         
         MVC   RAVLDBKS,LSTBK      BOOKS (FROM BK=)                             
         MVC   RAVLDBKF,LSTBKF     C'+-' FILTER                                 
         CLI   TVSAT,C'T'                                                       
         BE    *+10                                                             
         MVC   RAVLDSAT,TVSAT      SATELLITE CODE                               
* 23JAN90      *** START ***                                                    
         CLI   LSTTX,1             WAS TX=ALL ENTERED?                          
         BNE   AD15                                                             
         MVC   RAVLDLLQ(7,R5),=X'080621E3000000'                                
         ICM   R1,3,RAVLLEN                                                     
         LA    R1,6(R1)                                                         
         STCM  R1,3,RAVLLEN                                                     
         GOTO1 ADDREC                                                           
         XC    RAVLDLLQ(6,R5),RAVLDLLQ(R5)                                      
         ICM   R1,3,RAVLLEN                                                     
         SH    R1,=H'6'                                                         
         STCM  R1,3,RAVLLEN                                                     
         B     AD17                                                             
* 23JAN90      *** END ***                                                      
AD15     DS    0H                                                               
         GOTO1 ADDREC                                                           
AD17     DS    0H                                                               
         LA    R2,LISTL(R2)        POINT TO NEXT LIST ITEM                      
         BCT   R3,AD10                                                          
*                                                                               
         MVC   LAST,RAVLKDET       SAVE LAST LINE NUMBER ADDED                  
         B     MSG                                                              
         DROP  R2,R5,R6                                                         
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
AD20     MVC   RPRPKEY,DETKEY                                                   
         ZIC   R3,INVCOUNT                                                      
         LA    R2,LIST             R2 POINTS TO LIST                            
AD30     DS    0H                                                               
         USING LISTD,R2            ESTABLISH INVENTORY REC LIST                 
*                                                                               
         LA    R1,34               LENGTH OF KEY +                              
         LA    R1,RPRPDELQ(R1)     LENGTH OF DETAIL ELEMENT =                   
         STH   R1,HALF                                                          
         MVC   RPRPLEN,HALF        RECORD LENGTH                                
         SPACE 1                                                                
         LA    R5,34(R6)           A(FIRST ELEMENT)                             
         USING RPRPDEL,R5                                                       
         MVI   RPRPDCOD,X'01'                                                   
         MVI   RPRPDLEN,RPRPDELQ                                                
         SPACE                                                                  
*         ZIC   R3,INVCOUNT                                                     
*         LA    R2,LIST             R2 POINTS TO LIST                           
         SPACE                                                                  
*AD30     DS    0H                                                              
         ZIC   R1,RPRPKDET         BUMP DETAIL NUMBER                           
         LA    R1,1(R1)                                                         
         STC   R1,RPRPKDET         AND MOVE IN THE NEXT INVENTORY               
         MVC   RPRPDINV,LSTINV     NUMBER IN LIST                               
         MVC   RPRPDATE,LSTSTD     EFFECTIVE START DATE                         
         MVI   RPRPDNUM,1          DEFAULT IS 1 SPOT                            
         MVI   RPRPDNC,C'W'        PER WEEK                                     
         CLI   TVSAT,C'T'                                                       
         BE    *+10                                                             
         MVC   RPRPDSAT,TVSAT      SATELLITE CODE                               
* 23JAN90      *** START ***                                                    
         OC    LSTBK,LSTBK         WAS A BOOK OVERRIDE ENTERED?                 
         BZ    AD35                                                             
         XI    LSTBK,X'80'         FLIP CPP/CPM BITS                            
         GOTO1 VUNBOOK,DMCB,(1,LSTBK),DUMMYH,0,0                                
         LA    R5,RPRPDELQ(R5)     PT @NEXT EL                                  
         USING RAVLOEL,R5                                                       
         MVI   RAVLOCOD,X'06'                                                   
         MVI   RAVLOLEN,11                                                      
         MVI   RAVLOTYP,X'07'                                                   
         MVC   RAVLODTA(8),DUMMY   BOOK (FROM BK=)                              
         ICM   R1,3,RPRPLEN                                                     
         LA    R1,11(R1)                                                        
         STCM  R1,3,RPRPLEN                                                     
         LA    R5,34(R6)           A(FIRST ELEMENT)                             
         USING RPRPDEL,R5                                                       
AD35     DS    0H                                                               
         CLI   LSTTX,1             WAS TX=ALL ENTERED?                          
         BNE   AD40                                                             
         LR    R5,R6                                                            
         XR    R1,R1                                                            
         ICM   R1,3,RPRPLEN                                                     
         AR    R5,R1                                                            
         LA    R1,6(R1)                                                         
         STCM  R1,3,RPRPLEN                                                     
         MVC   0(6,R5),=X'080621E30000'                                         
         LA    R5,34(R6)           A(FIRST ELEMENT)                             
* 23JAN90      *** END ***                                                      
AD40     DS    0H                                                               
         GOTO1 ADDREC                                                           
         XC    RPRPDELQ(17,R5),RPRPDELQ(R5)  (11 FOR BK, 6 FOR TX)              
AD50     DS    0H                                                               
         LA    R2,LISTL(R2)        POINT TO NEXT LIST ITEM                      
         BCT   R3,AD30                                                          
         SPACE                                                                  
         MVC   LAST,RPRPKDET       SAVE LAST LINE NUMBER ADDED                  
         DROP  R2,R5,R6                                                         
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* FORMAT MESSAGE - EDIT OUT FIRST NUMBER                            *           
*-------------------------------------------------------------------*           
*                                                                               
MSG      MVC   AIO,AIO1            RESTORE TO IO1                               
         CLI   INVCOUNT,0                                                       
         BE    MSG20                                                            
         SPACE                                                                  
         MVC   RERROR,=AL2(LINADDED)                                            
         CLC   FIRST,LAST          ONLY 1 LINE                                  
         BE    *+10                YES                                          
         MVC   RERROR,=AL2(LINESADD)                                            
         LA    R3,WORK+20          SINCE EDIT MACRO USES WORK+0                 
         ZIC   R6,FIRST                                                         
         EDIT  (R6),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         LR    R2,R0               NUMBER OF CHARACTERS SO FAR                  
         CLC   FIRST,LAST          ONLY 1 LINE                                  
         BE    MSG10               YES                                          
         MVI   0(R3),C'-'          NO-EDIT OUT LAST LINE IN THE                 
         LA    R3,1(R3)            RANGE ADDED                                  
         LA    R2,1(R2)                                                         
         ZIC   R6,LAST                                                          
         EDIT  (R6),(3,(R3)),ALIGN=LEFT                                         
         AR    R2,R0                                                            
         SPACE                                                                  
MSG10    STC   R2,RTXTLEN          LENGTH OF STRING                             
         LA    R3,WORK+20                                                       
         STCM  R3,7,RTXTADR        A(STRING)                                    
         SPACE                                                                  
         LA    R2,HDRDPT1H         CURSOR TO DAYPART FIELD                      
         BAS   RE,CLEAR            CLEAR OUT DAYPART FIELDS                     
         LA    R2,HDRDPT2H                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,CLEAR                                                         
         SPACE                                                                  
         LA    R2,HDRCONH          AND POINT TO CONTRACT FIELD                  
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 2                                                                
MSG20    LA    R2,HDRDPT1H         CURSOR TO DAYPART FIELD                      
         CLI   BYTE,C'Y'                                                        
         BE    MSG25               NO MATCHES                                   
         MVC   RERROR,=AL2(TOOHIGH)                                             
         B     MSG30                                                            
         SPACE                                                                  
MSG25    EQU   *                                                                
         MVC   RERROR,=AL2(NOMATCH)                                             
         SPACE                                                                  
MSG30    EQU   *                                                                
         CLI   CONACT,C'A'         ADD FUNCTION                                 
         BNE   ERREND                                                           
         MVC   CONACT(8),=C'CHANGE  '                                           
         FOUT  CONACTH                                                          
         B     ERREND                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
* THIS ROUTINE DELETES A PRP/AVA HEADER AND ALL DETAIL RECORDS      *           
* ASSOCIATED WITH IT.  NOTE:  ONLY THE DIRECTORY POINTERS ARE       *           
* DELETED, NOT THE FILE RECORDS.                                                
*-------------------------------------------------------------------*           
         SPACE 1                                                                
DELPRPAV NTR1                                                                   
         GOTO1 HIGH                                                             
         MVC   RERROR,=AL2(NOTFOUND)                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
DELPA10  OI    KEY+27,X'80'        TURN ON DELETED BIT                          
         GOTO1 WRITE               AND WRITE IT BACK TO DIRECTORY               
         SPACE 1                                                                
         GOTO1 SEQ                                                              
         CLI   ETYPE,C'A'                                                       
         BE    *+18                                                             
         CLC   KEY(23),KEYSAVE     SAME PROPOSAL                                
         BE    DELPA10                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLC   KEY(26),KEYSAVE     SAME AVAIL                                   
         BE    DELPA10                                                          
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
* THIS ROUTINE RESTORES A PRP/AVA HEADER AND ALL DETAIL RECORDS     *           
* ASSOCIATED WITH IT.  IT IS ASSUMED HERE THAT THE FILE RECORDS     *           
* HAVE *** NOT *** BEEN MARKED FOR DELETION.                        *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
RESTPRAV NTR1                                                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         MVC   RERROR,=AL2(NOTFOUND)                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         TM    KEY+27,X'80'        WAS HEADER DELETED                           
         BZ    RESTPAX             NO, SO DON'T RESTORE DETAILS                 
         B     RESTPA20                                                         
         SPACE 1                                                                
RESTPA10 TM    KEY+27,X'80'        WAS DETAIL DELETED                           
         BZ    RESTPA30            NO, CHECK NEXT DETAIL                        
RESTPA20 MVI   KEY+27,0            UNDELETE IT                                  
         GOTO1 WRITE               AND WRITE IT BACK TO DIRECTORY               
         SPACE 1                                                                
RESTPA30 OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 SEQ                                                              
         CLI   ETYPE,C'A'                                                       
         BE    *+18                                                             
         CLC   KEY(23),KEYSAVE     SAME PROPOSAL                                
         BE    RESTPA10                                                         
         B     RESTPAX                                                          
         SPACE 1                                                                
         CLC   KEY(26),KEYSAVE     SAME AVAIL                                   
         BE    RESTPA10                                                         
         SPACE 1                                                                
RESTPAX  NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
* THIS ROUTINE CLEARS OUT A FIELD                                   *           
* ON ENTRY, R2 POINTS TO FIELD HEADER                               *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
CLEAR    ZIC   R1,0(R2)            GET LENGTH OF FIELD                          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*  THIS ROUTINE CLEARS OUT ALL THE UNPROTECTED RECORD FIELDS        *           
*-------------------------------------------------------------------*           
CLEARREC NTR1                                                                   
         LA    R2,HDRBKSH                                                       
CR10     BAS   RE,CLEAR                                                         
         BAS   RE,NEXTUF                                                        
         CLI   0(R2),9             LAST FIELD ON SCREEN                         
         BNE   CR10                                                             
         B     XIT                                                              
         SPACE 3                                                                
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    NEXTUF                                                           
         BR    RE                                                               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* SUB-ROUTINE TO SCAN DAYPART FIELDS FOR INVENTORY FILTERS          *           
* BY DAYPART.  ROUTINE GENERATES FILTER ELEMENTS USED IN MULT.      *           
*-------------------------------------------------------------------*           
*                                                                               
SCAN     NMOD1 0,**SCAN**                                                       
         L     RC,0(R1)                                                         
*                                                                               
* BEGIN SCAN BY CONSTRUCTING A DUMMY HEADER AND A SINGLE STRING                 
* IN STORAGE                                                                    
*                                                                               
         XC    CBLOCK,CBLOCK                                                    
         LA    R2,HDRDPT1H                                                      
         MVC   CBLOCK(8),0(R2)     REAL HEADER USED AS BASE FOR DUMMY           
         LA    R5,CBLOCK+8         POINT R5 TO FIRST DATA POSITION              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                MOVE DATA FROM SCREEN                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),8(R2)                                                    
         AR    R5,R1               UPDATE POINTER TO LAST DATA BYTE             
         ZIC   RE,0(R2)                                                         
         LR    R0,RE               SAVE SECOND HEADER LENGTH                    
         AR    RE,R2               RE POINTS TO SECOND HEADER                   
         CLI   5(RE),0                                                          
         BE    SC20                ONLY 1 LINE OF DATA                          
         SPACE                                                                  
         ZIC   RF,CBLOCK+5         DATA LENGTH SO FAR                           
         CLI   0(R5),C','          DID FIRST LINE END IN COMMA                  
         BE    *+16                YES                                          
         MVI   1(R5),C','          NO                                           
         LA    RF,1(RF)            UPDATE DATA LENGTH AND                       
         LA    R5,1(R5)            POINTER                                      
         SPACE 2                                                                
         LA    R5,1(R5)            POINT TO NEXT BYTE FOR DATA                  
         ZIC   R1,0(RE)                                                         
         AR    R1,R0               ADD SECOND FIELD LENGTH TO                   
         SH    R1,=H'16'           DUMMY HEADER LENGTH (EXTENDED HDR)           
         STC   R1,CBLOCK                                                        
         ZIC   R1,5(RE)                                                         
         AR    RF,R1                                                            
         STC   RF,CBLOCK+5         TOTAL DATA LENGTH                            
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE SECOND FIELD DATA TO                    
         B     SC20                STRING                                       
         MVC   0(0,R5),8(RE)                                                    
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* REGISTER CONVENTIONS FOR SCAN    R2-POINTS TO SCANNER BLOCK       *           
* R3-COUNTER FOR LINES             R5-POINTS TO FILTER ELEMENTS     *           
*-------------------------------------------------------------------*           
*                                                                               
SC20     EQU   *                                                                
         L     R2,=A(MYBIGB)                                                    
         A     R2,RELO                                                          
         GOTO1 SCANNER,DMCB,CBLOCK,(R2),C',=,*'                                 
         CLI   DMCB+4,0                                                         
         BE    SCERR                                                            
         ZIC   R3,DMCB+4                                                        
         MVC   NUMLNS,DMCB+4       SAVE COUNT OF LINES                          
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* MAKE SURE STRING STARTS OUT WITH A VALID DAYPART.                 *           
* IF THEY INPUT DPT F (FRINGE), BUILD FILENTRYS (IN ORDER) FOR      *           
*                   E (EARLY FRINGE)                                *           
*                   R (EARLY NEWS)                                  *           
*                   A (PRIME ACCESS)                                *           
*                   T (LATE NEWS)                                   *           
*                   L (LATE FRINGE)                                 *           
*                   W (WEEKEND)                                     *           
*                   F (FRINGE)                                      *           
*  AND DON'T ALLOW INPUT OF ANY OF THOSE INDIVIDUAL DAYPARTS        *           
*                                                                   *           
* OR IF THEY INPUT A SUB-DAYPART, DON'T ALLOW INPUT OF MASTER (FRG) *           
*-------------------------------------------------------------------*           
*                                                                               
         MVI   FRGMFLAG,C'N'        YES=WE'VE DONE MASTER FRINGE                
         MVI   FRGSFLAG,C'N'        YES=WE'VE DONE A SUB-FRINGE CTGRY           
         MVI   FRG,C'N' YES=MIDST OF BLDING FILENTRY FOR MASTER FRINGE          
         MVI   DATEFLAG,C'N'       CONSECUTIVE DATES FLAG                       
         MVI   TARGET,0                                                         
         LA    R5,FILTERS                                                       
         USING FILTD,R5                                                         
         SPACE                                                                  
* LOOK FOR DAYPART                                                              
         SPACE                                                                  
         TM    2(R2),X'40'         ALPHA                                        
         BZ    SCERR                                                            
         CLI   0(R2),3             DAYPART MUST BE LEN OF 3                     
         BNE   SCERR                                                            
         LA    RE,DPTTAB           RE POINTS TO TABLE                           
         LA    R1,DAYPARTS         R1 IS A COUNTER                              
         CLC   12(3,R2),0(RE)                                                   
         BE    SC30                FOUND IT                                     
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     SCERR               INVALID                                      
         SPACE 2                                                                
* PROCESS VALID DAYPART                                                         
*                                                                               
SC30     MVC   FILTDPT,3(RE)       INTERNAL CODE                                
         MVC   FILTCNTL,4(RE)                                                   
         SPACE 1                                                                
         MVI   TVSAT,C'T'                                                       
         CLI   1(R2),0             FIRST DAYPART CAN BE DIVIDED                 
         BE    SC40                FIELD TO INDICATE SATELLITE CODE             
         CLI   1(R2),1             FOR THE ENTIRE MULTIPLE ADD                  
         BNE   SCERR                                                            
         CLI   22(R2),C'1'                                                      
         BE    *+12                                                             
         CLI   22(R2),C'2'                                                      
         BNE   SCERR                                                            
         MVC   TVSAT,22(R2)                                                     
         SPACE 1                                                                
SC40     OI    TARGET,X'0F'                                                     
         MVI   FILCOUNT,1          INITIALIZE ELEMENT COUNTER                   
         TM    FILTCNTL,SFRINGE    IS THIS A SUB-FRINGE CATEGORY                
         BZ    *+12                                                             
         MVI   FRGSFLAG,C'Y'                                                    
         B     SC50                                                             
         TM    FILTCNTL,MFRINGE    IS THIS MASTER FRINGE                        
         BZ    SC50                                                             
         MVI   FRGMFLAG,C'Y'                                                    
         MVI   FRG,C'Y'                                                         
         MVI   FILTDPT,C'E'        FIRST ENTRY IS E (LAST WILL BE F)            
         SPACE                                                                  
SC50     B     SC200               RESET                                        
         SPACE 2                                                                
* SC100 IS ENTRY POINT FOR REST OF SCAN.   CODE SPLIT INTO 2 SECTIONS-          
* ONE IF LENGTH IS 3, ONE IF NE TO 3.                                           
SC100    CLI   1(R2),0             NO KEYWORD INPUT                             
         BNE   SCERR                                                            
         CLI   0(R2),3                                                          
         BE    SC130                                                            
         SPACE                                                                  
* IF LEN NE 3, ONLY POSSIBILITIES ARE DATE, TIME, TEXT, AND BOOK                
* 12JAN90      *** START ***                                                    
         CLC   =C'TX=ALL',12(R2)                                                
         BNE   *+12                                                             
         MVI   FILTTX,1                                                         
         B     SC200                                                            
*                                                                               
         CLC   =C'BK=',12(R2)                                                   
         BNE   SC105                                                            
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'4'            (BK=) + 1 FOR EX                             
         CLI   15(R2),C'-'         MINUS FILT?                                  
         BE    *+12                YES                                          
         CLI   15(R2),C'+'         PLUS FILT USED?                              
         BNE   SC101                                                            
         CLI   ETYPE,C'P'          '+' NOT VALID ON PROPOSALS                   
         BE    SC103                                                            
         MVC   FILTBKF(1),15(R2)                                                
         BCTR  R1,R0               DON'T INCLUDE '+-'                           
         EX    R1,*+8                                                           
         B     SC102                                                            
         MVC   DUMMY(0),16(R2)                                                  
SC101    DS    0H                                                               
         MVI   FILTBKF,0                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUMMY(0),15(R2)                                                  
SC102    DS    0H                                                               
         LA    R1,1(R1)                                                         
         STC   R1,DUMMYHL                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAVLELEM,R6                                                      
         GOTO1 BOOKVAL,DMCB,(RAVLSRC,DUMMYH),(10,FILTBK),              X        
               (C'L',SCANNER),CBKLABEL                                          
         DROP  R6                                                               
         CLI   4(R1),0                                                          
         BE    SC103                                                            
         XI    FILTBK,X'80'        FLIP CPP/CPM BITS                            
         B     SC200                                                            
SC103    DS    0H                                                               
         LA    R2,HDRDPT1H                                                      
         MVC   RERROR,=AL2(INVBOK)                                              
         B     ERREND                                                           
* 12JAN90      *** END ***                                                      
SC105    DS    0H                                                               
         ZIC   R6,0(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R6),12(R2)),FULL                                   
         CLI   DMCB,X'FF'                                                       
         BE    SC110               IF NOT TIME, TRY DATE                        
         TM    TARGET,TIME         DUPLICATE TIME FILTER                        
         BZ    SCERR               YES                                          
         CLI   DATEFLAG,C'Y'       SECOND DATE EXPECTED                         
         BE    SCERR               YES                                          
         NI    TARGET,X'FF'-TIME   TIME HAS BEEN SPOKEN FOR                     
         MVC   FILTTIME,FULL                                                    
         OC    FULL+2(2),FULL+2    TIME MUST BE A RANGE                         
         BZ    SCERR                                                            
         CLC   FULL(2),FULL+2      START MUST NOT EQ END                        
         BE    SCERR                                                            
         B     SC200               RESET                                        
         SPACE 2                                                                
* TIME ELIMINATED - TRY DATE                                                    
*                                                                               
SC110    GOTO1 DATVAL,DMCB,(0,12(R2)),DUB                                       
         OC    DMCB(4),DMCB                                                     
         BZ    SCERR               INVALID DATE                                 
         TM    TARGET,START+END                                                 
         BZ    SCERR               BOTH START AND END ALREADY GIVEN             
         GOTO1 DATCON,(R1),(0,DUB),(2,HALF)                                     
         CLI   DATEFLAG,C'Y'                                                    
         BE    SC120               DATE IS AN END DATE                          
         MVC   FILTST,HALF                                                      
         MVI   DATEFLAG,C'Y'       SET FLAG FOR END DATE NEXT                   
         NI    TARGET,X'FF'-START  TURN OFF START BIT                           
         B     SC200               RESET                                        
         SPACE                                                                  
SC120    MVC   FILTEND,HALF                                                     
         MVI   DATEFLAG,C'N'       END DATE FOUND AFTER START                   
         NI    TARGET,X'FF'-END                                                 
         CLC   FILTST,FILTEND      START CANNOT BE GT END                       
         BH    SCERR                                                            
         B     SC200               RESET                                        
         SPACE 2                                                                
* LOGIC TO VALIDATE TERMS OF LEN 3.  TRY DAY OR DAYPART                         
*                                                                               
SC130    LA    R1,DAYS             COUNTER                                      
         LA    RE,DAYTAB           TABLE POINTER                                
         CLC   12(3,R2),0(RE)                                                   
         BE    SC140               FOUND IT                                     
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     SC150               TRY FOR A DAYPART                            
         SPACE                                                                  
SC140    TM    TARGET,DAY                                                       
         BZ    SCERR               DAY ALREADY THERE                            
         NI    TARGET,X'FF'-DAY                                                 
         CLI   DATEFLAG,C'Y'                                                    
         BE    SCERR               USER OMITTED END DATE                        
         MVC   FILTDAY,3(RE)                                                    
         B     SC200               RESET                                        
         SPACE 2                                                                
* LOOK FOR VALID DAYPART ENTRY                                                  
*                                                                               
SC150    LA    R1,DAYPARTS         COUNTER                                      
         LA    RE,DPTTAB           TABLE                                        
         CLC   12(3,R2),0(RE)                                                   
         BE    SC160               FOUND                                        
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R1,*-14                                                          
         B     SCERR                                                            
         SPACE                                                                  
SC160    CLI   DATEFLAG,C'Y'                                                    
         BE    SCERR               END DATE MISSING                             
         CLI   FRGSFLAG,C'Y'       HAVE WE DONE A SUB-FRINGE CATEGORY           
         BNE   SC170                                                            
         TM    4(RE),MFRINGE        YES, CAN'T NOW DO MASTER FRINGE             
         BO    SCERR                                                            
         B     SC180                                                            
SC170    CLI   FRGMFLAG,C'Y'       HAVE WE DONE MASTER FRINGE                   
         BNE   SC180                                                            
         TM    4(RE),SFRINGE        YES, CAN'T NOW DO A SUB-FRINGE              
         BO    SCERR                                                            
         SPACE 1                                                                
SC180    CLI   FRG,C'Y'       ARE WE BUILDING MASTER FRINGE FILENTRY            
         BNE   SC190                                                            
         ST    RE,FULL                                                          
         BAS   RE,BLDFRG           FINISH IT BEFORE GOING TO NEXT DPT           
         L     RE,FULL                                                          
         SPACE                                                                  
* PROCESS VALID DAYPART                                                         
         SPACE                                                                  
SC190    LA    R5,L'FILENTRY(R5)   BUMP FILTER ELEMENT POINTER                  
         MVC   FILTDPT,3(RE)       TAKE VALUES FROM TABLE ENTRY                 
         MVC   FILTCNTL,4(RE)                                                   
         MVI   TARGET,0                                                         
         OI    TARGET,X'0F'        RESET TARGET BITS                            
         ZIC   R1,FILCOUNT                                                      
         LA    R1,1(R1)            INCREMENT ELEMENT COUNT                      
         STC   R1,FILCOUNT                                                      
         TM    FILTCNTL,SFRINGE    IS THIS A SUB-FRINGE CATEGORY                
         BZ    *+12                                                             
         MVI   FRGSFLAG,C'Y'                                                    
         BZ    SC200               RESET                                        
         TM    FILTCNTL,MFRINGE    IS THIS MASTER FRINGE                        
         BZ    SC200               RESET                                        
         MVI   FRGMFLAG,C'Y'       YES                                          
         MVI   FRG,C'Y'            BLDING FILE ENTRY FOR MASTER FRINGE          
         MVI   FILTDPT,C'E'        FIRST ENTRY IS E (LAST WILL BE F)            
         B     SC200               RESET                                        
         SPACE 2                                                                
SC200    LA    R2,32(R2)           UPDATE SCANNER BLOCK POINTER                 
         BCT   R3,SC100                                                         
         CLI   DATEFLAG,C'Y'                                                    
         BE    SCERR               UNFINISHED BUSINESS                          
         CLI   FRG,C'Y'       ARE WE BUILDING MASTER FRINGE FILENTRY            
         BNE   *+8                                                              
         BAS   RE,BLDFRG           YES, SO FINISH IT BEFORE ENDING              
         CLI   FILCOUNT,30                                                      
         BH    SCERR               TOO MANY DAYPARTS                            
         MVI   BYTE,0              OK                                           
         B     SCX                                                              
         SPACE                                                                  
SCERR    MVI   BYTE,X'FF'                                                       
         SPACE                                                                  
SCX      CLI   BYTE,0                                                           
         XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*   THIS ROUTINE BUILDS A FEW TABLE ENTRIES FOR THE MASTER          *           
*   FRINGE CATEGORY                                                 *           
*-------------------------------------------------------------------*           
BLDFRG   NTR1                                                                   
         LA    RE,FRGTAB                                                        
         SPACE 1                                                                
BF10     MVC   L'FILENTRY(L'FILENTRY,R5),0(R5)  DUPLICATE ENTRY                 
         LA    R5,L'FILENTRY(R5)   POINT TO NEW ENTRY                           
         ZIC   R1,FILCOUNT         INCREMENT ELEMENT COUNT                      
         LA    R1,1(R1)                                                         
         STC   R1,FILCOUNT                                                      
         MVC   0(1,R5),0(RE)       PUT NEXT FRINGE CODE IN                      
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'FF'         DONE FRINGE                                  
         BNE   BF10                                                             
         MVI   FRG,C'N'                                                         
         XIT1  REGS=(R5)           SAVE PLACE IN TABLE                          
         SPACE 3                                                                
         DROP  R5                                                               
         SPACE 3                                                                
FRGTAB   DC    C'RATLWF'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* DAYPART TABLE                                                                 
*        BYTES 0-2 = DAY CODE                                                   
*        BYTE  3   = INTERNAL CODE                                              
*        BYTE  5   = CONTROL BITS                                               
*                                                                               
DPTTAB   DS    0CL5                                                             
         DC    CL3'MNG',C'M',AL1(0)                                             
         DC    CL3'DAY',C'D',AL1(0)                                             
         DC    CL3'ELY',C'E',AL1(SFRINGE)                                       
         DC    CL3'ENW',C'R',AL1(SFRINGE)                                       
         DC    CL3'ACC',C'A',AL1(SFRINGE)                                       
         DC    CL3'LNW',C'T',AL1(SFRINGE)                                       
         DC    CL3'LTE',C'L',AL1(SFRINGE)                                       
         DC    CL3'WKD',C'W',AL1(SFRINGE)                                       
         DC    CL3'KID',C'K',AL1(0)                                             
         DC    CL3'FRG',C'F',AL1(MFRINGE)      FRG = DO ALL FRINGES             
         DC    CL3'NWS',C'N',AL1(0)                                             
         DC    CL3'PRI',C'P',AL1(0)                                             
         DC    CL3'MOV',C'V',AL1(NODAYS)                                        
         DC    CL3'SPE',C'S',AL1(NODAYS)                                        
         DC    CL3'SPO',C'J',AL1(NODAYS)                                        
         DC    CL3'SPS',C'O',AL1(0)                                             
         DC    CL3'COM',C'U',AL1(0)                                             
         DC    CL3'LOC',C'X',AL1(0)                                             
DPTTABLN EQU   5                                                                
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         SPACE 2                                                                
* DAY TABLE                                                                     
*        BYTES 0-2 = DAY CODE                                                   
*        BYTE  3   = INTERNAL CODE                                              
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    CL3'MON',C'1'                                                    
         DC    CL3'TUE',C'2'                                                    
         DC    CL3'WED',C'3'                                                    
         DC    CL3'THU',C'4'                                                    
         DC    CL3'FRI',C'5'                                                    
         DC    CL3'SAT',C'6'                                                    
         DC    CL3'SUN',C'7'                                                    
         DC    CL3'M-F',C'0'                                                    
         DC    CL3'M-S',C'8'                                                    
         DC    CL3'S-S',C'9'                                                    
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
* INPUT OF C'SAR' IN THE BOOK, DEMO AND/OR LENGTH FIELD MEANS THE   *           
* USER WANTS THE OPPORTUNITY TO SEE THE SAR DATA FOR THAT FIELD.    *           
* SO, DISPLAY THE SAR DATA AND RETURN CONTROL TO THE USER           *           
* FOR CHANGES OR FURTHER INPUT TO OTHER DATA FIELDS                 *           
*-------------------------------------------------------------------*           
         SPACE 2                                                                
GETSAR   NMOD1 0,*GETSAR*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         NI    CSTATUS,X'7F'       RESET STATUS BYTE                            
         SPACE 1                                                                
         LA    R2,HDRBKSH                                                       
         CLC   8(3,R2),=C'SAR'     IF THEY INPUT C'SAR',                        
         BNE   GS20                THEN DISPLAY SAR BOOKS                       
         BAS   RE,CLEAR                                                         
         OI    CSTATUS,X'80'       SAR DATA TO BE DISPLAYED                     
         OC    CSARBKS,CSARBKS     MAKE SURE THERE ARE SAR BOOKS                
         BZ    GS20                                                             
         XC    A0BELEM,A0BELEM                                                  
         OC    CSAR0B,CSAR0B       IS THERE SAR BOOK LABEL ELEM                 
         BZ    GS10                                                             
         LA    R3,CSAR0B                                                        
         ST    R3,A0BELEM          YES, SAVE THE ADDRESS                        
GS10     MVI   MAX,1                                                            
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         MVI   MAX,6                                                            
         GOTO1 DISPBKL,DMCB,CSARBKS,A0BELEM,0,0                                 
         SPACE 1                                                                
GS20     LA    R2,HDRDEMH                                                       
         CLC   8(3,R2),=C'SAR'     IF THEY INPUT C'SAR',                        
         BNE   GS30                THEN DISPLAY SAR DEMOS                       
         BAS   RE,CLEAR                                                         
         OI    CSTATUS,X'80'       SAR DATA TO BE DISPLAYED                     
         OC    CSARDEM,CSARDEM     MAKE SURE THERE ARE SAR DEMOS                
         BZ    GS30                                                             
         LA    R5,CDEMOS                                                        
         XC    CDEMOS,CDEMOS                                                    
         MVC   0(L'CSARDEM,R5),CSARDEM     DEMOS + ENDING ZERO                  
         MVI   MAX,6                                                            
         GOTO1 DISPDEM,DMCB,CDEMOS                                              
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         LA    R2,8(R2)                                                         
GS26     EQU   *                                                                
         CLI   0(R2),C'$'                                                       
         BNE   GS27                                                             
         MVI   0(R2),C'='                                                       
GS27     EQU   *                                                                
         LA    R2,1(R2)                                                         
         BCT   R0,GS26                                                          
         SPACE 1                                                                
GS30     LA    R2,HDRLENH                                                       
         CLC   8(3,R2),=C'SAR'     AND THEY INPUT C'SAR',                       
         BNE   GSX                 THEN DISPLAY SAR LENGTH                      
         BAS   RE,CLEAR                                                         
         OI    CSTATUS,X'80'                                                    
         OC    CSARLEN,CSARLEN     IF THERE ARE SAR LENGTHS                     
         BZ    GSX                                                              
         MVI   MAX,1               1 LENGTH FOR PROPOSALS                       
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         MVI   MAX,6               UP TO 6 CLASS/LENGTHS FOR AVAILS             
         GOTO1 DISPLEN,DMCB,CSARLEN                                             
         SPACE 1                                                                
GSX      TM    CSTATUS,X'80'       ANY SAR DATA TO BE DISPLAYED                 
         BZ    GSARGOOD            NO                                           
         LA    R2,HDRBKSH          YES, SO POINT TO 1ST DATA FIELD              
         OI    1(R2),X'01'         (SAY FIELD HAS BEEN MODIFIED)                
         MVC   RERROR,=AL2(INPUTNOW)                                            
         MVI   RMSGTYPE,C'I'                                                    
         B     GSARBAD             AND GIVE INFORMATIONAL MESSAGE               
*                                                                               
*        GETSAR EXIT                                                            
*                                                                               
GSARGOOD EQU   *                                                                
         LA    R1,0                                                             
GSARXIT  EQU   *                                                                
         LTR   R1,R1                                                            
         XIT1                                                                   
GSARBAD  EQU   *                                                                
         LA    R1,1                                                             
         B     GSARXIT                                                          
         EJECT                                                                  
*                                                                               
*        PROGRAM EQUATES                                                        
*                                                                               
MFRINGE  EQU   X'01'                                                            
NODAYS   EQU   X'02'                                                            
SFRINGE  EQU   X'04'                                                            
DAY      EQU   X'01'                                                            
START    EQU   X'02'                                                            
END      EQU   X'04'                                                            
TIME     EQU   X'08'                                                            
XXXXXXXX EQU   X'1111'             TO BE DEFINED ERROR CODE                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         ENTRY MYBIGB                                                           
MYBIGB   DS    1024C                                                            
         EJECT                                                                  
* DSECT TO COVER FILTER ELEMENTS                                                
         SPACE 1                                                                
FILTD    DSECT                                                                  
FILENTRY DS    0CL16                                                            
FILTDPT  DS    C                                                                
FILTDAY  DS    C                                                                
FILTDAT  DS    0XL4                                                             
FILTST   DS    XL2                                                              
FILTEND  DS    XL2                                                              
FILTTIME DS    XL4                                                              
FILTCNTL DS    B                                                                
FILTTX   DS    X                                                                
FILTBK   DS    XL3                                                              
FILTBKF  DS    X                   BOOK FILTER (C'+' ONLY)                      
         SPACE 2                                                                
* DSECT TO COVER MULTIPLE ADD STORAGE                                           
         SPACE 1                                                                
MULTHDRD DSECT                                                                  
FILCOUNT DS    X                                                                
NUMLNS   DS    X                                                                
INVCOUNT DS    X                                                                
MAXINV   DS    X                                                                
         SPACE 1                                                                
TARGET   DS    B                                                                
FIRST    DS    X                                                                
LAST     DS    X                                                                
TVSTA    DS    C                   T=NORMAL STATION,1/2 SATELLITE CODE          
FRG      DS    C                   Y=BLDING FILENTRY FOR MASTER FRINGE          
         SPACE 1                                                                
FRGMFLAG DS    C                   ALREADY DID MASTER FRINGE                    
FRGSFLAG DS    C                   ALREADY DID A SUB-FRINGE CATEGORY            
DATEFLAG DS    C                   DONE START DATE, WAITING FOR END             
         SPACE 1                                                                
HDRST    DS    XL2                 COMPRESSED HEADER DATES                      
HDREND   DS    XL2                                                              
         SPACE 1                                                                
TIMRANGE DS    0F                                                               
TIMST    DS    H                                                                
TIMEND   DS    H                                                                
         SPACE 1                                                                
PROGTIM  DS    0F                                                               
PROGST   DS    H                                                                
PROGEND  DS    H                                                                
         SPACE 1                                                                
DETKEY   DS    CL27                                                             
         SPACE 1                                                                
FILTERS  DS    30CL16                                                           
LIST     DS    255CL11                                                          
DUMMYH   DC    X'4400000080'       LEN,ATTR,ADDR,INPT INDS                      
DUMMYHL  DS    X                   INPT LEN                                     
DUMMYHC  DC    X'0000'             OUTPT INDS,OUTPT LEN                         
DUMMY    DS    CL60                                                             
         DC    XL8'00'             END OF SCREEN MARKER                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF INVENTORY RECORDS                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTD    DSECT                     DSECT FOR INV RECORDS                        
LSTINV   DS    CL4                 INVENTORY NUMBER                             
LSTSTD   DS    CL3                 INVENTORY EFFECTIVE START DATE               
LSTBK    DS    CL3                 FILTER BOOK                                  
LSTBKF   DS    CL1                 FILTER BOOK FILTER                           
LSTTX    DS    CL1                 FILTER TEXT OPTION                           
LISTL    EQU   *-LISTD             LENGTH OF LIST ITEM                          
         EJECT                                                                  
* DDFLDIND                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD1D                                                       
         EJECT                                                                  
RAVLD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENAVLNN                                                     
         EJECT                                                                  
RPRPD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENPRPNN                                                     
         EJECT                                                                  
RINVD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
*  DEDBLOCK                                                                     
*  REGENCON                                                                     
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         PRINT ON                                                               
*  RESFMWORKD                                                                   
         SPACE 1                                                                
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*        WORK AREA                                                              
*                                                                               
         DS    0F                                                               
A0BELEM  DS    A                   A(BOOK LABEL ELEMENT)                        
ANUMVAL  DS    A                   A(LINKED NUMVAL)                             
TVSAT    DS    C                   SATELLITE CODE FOR MULTIPLE ADD              
WAREAEND EQU   *                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065RESFM0EA  05/01/02'                                      
         END                                                                    
