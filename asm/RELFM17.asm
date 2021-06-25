*          DATA SET RELFM17    AT LEVEL 116 AS OF 05/01/02                      
*PHASE T80417A,*                                                                
*INCLUDE REGETIUN                                                               
         TITLE 'RELFM17/T80417 - INTERFACE DEMO MODULES FOR INV. X-FER'         
*                                                                               
********************************************************************            
*                                                                  *            
*        RELFM17 --- CREATE DEMO INVENTORY RECORDS                 *            
*                                                                  *            
* ---------------------------------------------------------------- *            
*                                                                  *            
* UPDATE HISTORY:                                                  *            
* --------------                                                   *            
*                                                                  *            
* AUG27/90 (MRR) --- FORCE STATIONS 'UNIV' AND 'TELE' TO MARKET    *            
*                     NUMBER 98 FOR SRC/NOV89                      *            
*                                                                  *            
* OCT29/90 (MRR) --- UIN'TIZE DEMO RECORDS RETURNED IN THE HOOK    *            
*                                                                  *            
* FEB19/91 (MRR) --- ADD DBSELAGY TO DBLOCK SKELETON               *            
*                                                                  *            
* JUN26/91 (BU ) --- FOR TRANSFER FROM INVENTORY, SKIP TABLE       *            
*                    ENTRY WHEN NO DEMO TRACK FOR INVENTORY FOUND  *            
*                                                                  *            
* JUL26/91 (BU ) --- CHANGE FORMAT OF OVERRIDE ELEMENT X'DE' IN    *            
*                    ROUTINE INVEND5                               *            
*                                                                  *            
* OCT03/91 (BU ) --- FIX ERROR IN INVEND5 TO INCREASE TEST FOR     *            
*                    'RATING' VS 'IMPS'                            *            
*                                                                  *            
* OCT22/91 (BU ) --- CHANGE CREATION OF 5202 TO 520A ELEMENT TO FIX*            
*                    GETIUN DUMPS.                                 *            
*                                                                  *            
* DEC09/92 (BZ ) --- ADD DBSELMED = 'T' TO DBLOCK SKELETON         *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
T80417   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80417,RA,R9,RR=R2                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         ST    R2,RELO                                                          
*                                                                               
         MVI   U191,0              CLEAR V2149 UNIVERSE FLAGS                   
         MVI   INVIND,0                                                         
         MVI   TOTWGHT,0                                                        
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMULATORS                     
*                                                                               
         ZIC   R3,INVNO                                                         
         L     R5,INVLIST                                                       
         USING INVLD,R5                                                         
         L     R4,AIOAREA          A(DATA REC) SET BY CALLING PROGRAM           
         ST    R4,ADATAREC         SAVE ITS ADDRESS                             
         USING RINVD,R4                                                         
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         SPACE                                                                  
         CLI   INVTYP,C'P'                                                      
         BE    DEMO                                                             
***********************************************************                     
* REGISTER USAGE                                          *                     
* R3-INVENTORY LIST ENTRY COUNT  R4-POINTS TO DATA RECORD *                     
* R5-POINTS TO INV LIST ENTRY    R7-CONTAINS ACOMFACS     *                     
***********************************************************                     
         EJECT                                                                  
* TRANSFER DEMOS FROM INVENTORY (CODE ASSUMES THAT LIST CONTAINS                
* AT LEAST ONE ENTRY -- INVNO NE 0)                                             
INVFIL   DS    0H                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    RE,WORK3            READ INTO WORK3                              
         ST    RE,AIOAREA                                                       
         ST    RE,DBAREC           SET INPUT RECORD ADDRESS                     
         ST    R7,DBCOMFCS                                                      
         MVC   DBSELAGY,REPALPHA                                                
         MVC   DBFILE,=C'INV'                                                   
         MVI   INVFRBT,X'FF'       INITIALIZE FOR FIRST INV REC                 
         SPACE                                                                  
INV1     MVI   BYTE2,0                                                          
INV2     EQU   *                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,DEMSTA                                                  
         MVC   RINVKINV,INVLNUMB                                                
         MVC   RINVKSTD,INVLDATE                                                
         BAS   RE,HIGH                                                          
         SPACE 2                                                                
INV4     EQU   *                                                                
         CLC   RINVKEY(20),KEYSAVE                                              
         BNE   INV10               COULD NOT FIND NUMBER                        
         OC    INVLDATE,INVLDATE                                                
         BZ    *+14                                                             
         CLC   RINVKEY(24),KEYSAVE                                              
         BNE   INV10               DATE DID NOT MATCH                           
         MVI   HALF,1                                                           
         SPACE 2                                                                
INV5     EQU   *                                                                
         BAS   RE,GETREC                                                        
         L     R4,AIOAREA                                                       
         OC    INVLDATE,INVLDATE                                                
         BNZ   INV6                DATE FILTERING                               
         SPACE                                                                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    INV6                USE AN UNENDED INV HEADER                    
         BAS   RE,SEQ              KEEP READING                                 
         CLC   KEY(20),KEYSAVE     SAME NUMBER                                  
         BNE   *+20                NO                                           
         CLI   KEY+24,0            YES-IS IT A DATA RECORD                      
         BNE   *-18                YES-READ AGAIN                               
         MVI   HALF,2              NO                                           
         B     INV5                READ RECORD AND DATE CHECK                   
         SPACE                                                                  
         CLI   HALF,1              MORE THAN 1 IS ERROR                         
         BNE   INV10                                                            
         L     R4,AIOAREA                                                       
         MVC   KEY(27),0(R4)                                                    
         SPACE 2                                                                
INV6     EQU   *                                                                
         LA    R4,KEY              GET THE DATA RECORD                          
         SPACE                                                                  
         LA    R1,SVCLST           CONVERT BOOKVAL TO KSRC FMT                  
INV6E    CLC   INVSRC(1),3(R1)                                                  
         BE    INV6G                                                            
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   INV6E                                                            
         DC    H'0'                                                             
INV6G    MVC   RINVKSRC,2(R1)      PUT SOURCE                                   
         MVC   RINVKBK,INVFBK      AND BOOK IN THE KEY                          
         SPACE                                                                  
INV6L    BAS   RE,HIGH                                                          
         CLC   RINVKEY,KEYSAVE                                                  
         BNE   INV10               DID NOT FIND DATA REC                        
         BAS   RE,GETREC                                                        
         L     R4,AIOAREA          RESTORE R4                                   
         SPACE 2                                                                
INV7     MVI   DUB,0               SET DEFAULT FROM BOOK TYPE                   
         GOTO1 VGETEL,DMCB,(X'03',(R4)),DMCB+8                                  
         CLI   DMCB,X'FF'          TEST FOR MISSING FR DETAILS ELEM             
         BE    *+14                YES                                          
         L     RE,DMCB+8                                                        
         USING RINVFREL,RE                                                      
         MVC   DUB(1),RINVFRBT     GET FROM BOOK TYPE FROM RECORD               
         CLI   INVFRBT,X'FF'       TEST FOR FIRST RECORD                        
         BE    *+14                                                             
         CLC   INVFRBT,DUB         TEST FOR SAME BOOK TYPE AS PREVIOUS          
         BNE   INV7R               INVENTORY RECORDS                            
         MVC   INVFRBT,DUB         UPDATE FROM BOOK TYPE                        
         B     INV8                                                             
         DROP  RE                                                               
*                                                                               
INV7R    MVI   INVBAD,INCINV       INCONSISTENT BOOK TYPES ON INPUT             
         B     EXXMOD              INVENTORY RECORDS                            
         SPACE 2                                                                
INV8     EQU   *                                                                
         MVI   BYTE2,1                                                          
         GOTO1 VGETEL,DMCB,(X'CD',(R4)),DMCB+8                                  
         L     RE,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BE    *+10                                                             
         OC    INVIND,7(RE)        CUMULATIVE CODE ELEMENT BITS                 
         GOTO1 VGETEL,(R1),(X'5E',(R4)),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BNE   INV9                TEMPORARY PATCH TO FORCE IN                  
         XC    WORK,WORK           AN X'5E' ELEMENT UNTIL FILE IS               
         MVC   WORK(2),=X'5E07'    FULLY CONVERTED                              
         MVC   WORK+2(3),=C'PTN'                                                
         MVC   WORK+5(2),=X'520A'                                               
         GOTO1 VADDELEM,(R1),(R4),WORK                                          
         SPACE                                                                  
INV9     EQU   *                                                                
         BAS   RE,INVADD           ADD DEMOS                                    
         B     INV12                                                            
         SPACE 2                                                                
INV10    EQU   *                                                                
         CLI   INVLFLE,C'I'        IS TRANSFER FROM INVENTORY?                  
         BE    INV12               YES - NO DEMO TRACK - SKIP ENTRY             
         TM    INVLTYP,X'80'                                                    
         BO    INV17               ERROR IF INV NUM WAS INPUT                   
         CLI   BYTE2,1             ANYTHING IN I/O                              
         BNE   INV16               NO                                           
         BAS   RE,INVADD           ADD THAT AGAIN                               
         SPACE 2                                                                
INV12    EQU   *                                                                
         BCT   R3,*+8                                                           
         B     INVEND                                                           
         LA    R5,10(R5)                                                        
         TM    INVLTYP,X'80'                                                    
         BO    INV1                RESET BYTE2 IF INV NUM                       
         TM    INVLTYP,X'40'       OR FIRST IN DAY/TIME                         
         BO    INV1                                                             
         B     INV2                                                             
         SPACE 2                                                                
INV16    TM    INVLTYP,X'20'       CONTINUE IF NOT THE LAST                     
         BNO   INV12               IN DAY/TIME STRING                           
         SPACE                                                                  
INV17    MVI   INVBAD,ERRNF                                                     
         B     EXXMOD                                                           
         SPACE 2                                                                
INVEND   EQU   *                                                                
         L     R4,ADATAREC         RESET I/O POINTER TO DATA RECORD             
         ST    R4,AIOAREA                                                       
         LA    RE,DBLOCK                                                        
         ST    RE,MTHCFACS                                                      
         L     R5,INVLIST                                                       
         TM    INVLTYP,X'08'       SKIP DIVIDE FOR ADD ONLY                     
         BO    INVEND2                                                          
         SPACE                                                                  
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         ZIC   RE,TOTWGHT                                                       
         ST    RE,MTHFCTR                                                       
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',(R4),(R4),MATHFAC                       
         SPACE 2                                                                
INVEND2  EQU   *                                                                
*                                  X'80' - THERE IS NO U191                     
*                                  X'40' - THERE IS U191                        
         TM    U191,X'C0'          IF NEITHER IS ON, IGNORE                     
         BZ    INVEND8                                                          
         TM    U191,X'C0'          IF ONE OR THE OTHER IS ON, OK                
         BM    INVEND8                                                          
*                                                                               
*                THIS OCCURS WHEN WE ADD RECORDS WHICH HAVE THE                 
*                2149 BREAKS AND RECORDS WHICH DO NOT                           
*                                                                               
*   IF BOTH BITS ARE ON, BUILD DEMO O'RIDE ELEMS WITH VALUES OF ZERO            
*      NOTE:  OVERRIDES WILL BE SUBSEQUENTLY DELETED BY 'DEMUP'                 
*                                                                               
         LA    R6,OVRDTBL                                                       
         XC    WORK,WORK           INITIALIZE WORKSPACE                         
         MVC   WORK(2),=X'DE0C'    SET NEW ELEMENT FORMAT, LEN = 12             
INVEND5  CLI   0(R6),X'FF'         LOOP THROUGH TABLED ENTRIES                  
         BE    INVEND8             FINISHED                                     
         MVC   WORK+4(2),0(R6)     INSERT DEMO TYPE/NUMBER                      
         MVI   WORK+7,X'81'        FOR RATINGS: 1-DECL PRECISION                
         CLI   WORK+4,C'R'         RATING?                                      
         BE    INVEND6             YES                                          
         CLI   WORK+4,C'P'         RATING?                                      
         BE    INVEND6             YES                                          
         CLI   WORK+4,C'S'         RATING?                                      
         BE    INVEND6             YES                                          
         MVI   WORK+7,X'42'        FOR IMPS: HUNDREDS                           
INVEND6  EQU   *                                                                
         GOTO1 CHELLO,DMCB,(C'P',REPFILE),(R4),WORK                             
         LA    R6,L'OVRDTBL(R6)                                                 
         B     INVEND5                                                          
         SPACE 1                                                                
INVEND8  MVI   INVBAD,0                                                         
         B     EXXMOD                                                           
         SPACE 3                                                                
OVRDTBL  DS    0CL2                                                             
         DC    C'U',X'BF'          U191                                         
         DC    C'R',X'BF'          R191                                         
         DC    C'R',X'47'          R71                                          
         DC    C'R',X'73'          R115                                         
         DC    C'P',X'BF'          P191                                         
         DC    C'P',X'47'          P71                                          
         DC    C'P',X'73'          P115                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* SUB-ROUTINE TO ADD WEIGHTED INPUT INVENTORY RECORD INTO OUTPUT                
* DATA RECORD.  INDEX 'FF' DEMUP CALL INSURES THAT INPUT RECORD HAS             
* SET OF HPTS BEFORE WEIGHTING.  DBAREC=A(INPUT RECORD), ADATAREC               
* =A(OUTPUT RECORD)                                                             
*                                                                               
INVADD   NTR1                                                                   
         ZIC   R0,TOTWGHT                                                       
         LA    R2,1                                                             
         CLI   INVLWT,1                                                         
         BE    *+18                WEIGHT ONLY FOR INV NUM                      
         TM    INVLTYP,X'80'       INPUT                                        
         BZ    *+10                                                             
         ZIC   R2,INVLWT                                                        
         AR    R0,R2                                                            
         STC   R0,TOTWGHT                                                       
         SPACE                                                                  
         MVI   IUNSW,NO            IS INVENTORY RECORD IN IUN FORMAT            
         GOTO1 VGETEL,DMCB,(X'5E',DBAREC),DMCB+8                                
         CLI   DMCB,X'FF'                                                       
         BE    INVADD2                                                          
         L     R6,DMCB+8                                                        
         CLC   2(3,R6),=C'IUN'                                                  
         BNE   *+8                                                              
         MVI   IUNSW,YES                                                        
*                                                                               
INVADD2  DS    0H                                                               
         MVI   INDEXUP,NO          SET INDEX UPGRADED FLAG TO NO                
         GOTO1 CHELLO,DMCB,(C'G',REPFILE),(X'DE',DBAREC),(2,INDEX)              
         CLI   12(R1),0            TEST IF INDEX UPGRADED                       
         BNE   *+8                                                              
         MVI   INDEXUP,YES                                                      
*                                                                               
         LA    RE,DBLOCK           SET UP TO MAD INPUT INV RECORD               
         ST    RE,MTHCFACS                                                      
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
         ST    R2,MTHFCTR                                                       
         SPACE                                                                  
         XC    WORK,WORK           BUILD DUMMY UPGRADE ELEMENT                  
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVI   RAVLNCAT,C'I'       INVENTORY                                    
         MVC   RAVLNOP1,=X'FFFF'   INDEX UPGRADE TO GET OLD HPT                 
         MVC   RAVLNOP2,=H'1'      WEIGHTING-PASSING UNWEIGHTED REC             
         DROP  RE                                                               
         SPACE 1                                                                
         L     R6,DBAREC           GET HPT'S FOR INPUT RECORD THEN MAD          
*                                  UPGRADED INPUT RECORD INTO OUTPUT            
         GOTO1 VDEMUP,DMCB,34(R6),WORK,(R7),DEMODUB,HOMSHR                      
         CLI   IUNSW,YES                                                        
         BE    INVADD4                                                          
*                                                                               
         XC    WORK,WORK           CONVERT NON-IUN FORMAT TO IUN                
         LA    RE,WORK             BY MEANS OF INDEX 100 UPGRADE                
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         GOTO1 VADDELEM,DMCB,(R6),WORK                                          
         GOTO1 VDEMUP,(R1),(C'I',34(R6)),WORK,(R7),DEMODUB                      
         DROP  RE                                                               
         SPACE 1                                                                
*****************************************************************               
* A DEMOUT EXPLODE ON AN INDEX UPGRADED RECORD WILL PRODUCE     *               
* UNROUNDED RATINGS.  TO MAINTAIN THE USE OF ROUNDED RATINGS    *               
* IN THE REP SYSTEM, THE RATINGS AND PUTS ARE ROUNDED BEFORE    *               
* THE DEMOMATH 'MAD' CALL AND THE INDEX 'DE' ELEMENT DELETED.   *               
*****************************************************************               
*                                                                               
INVADD4  DS    0H                                                               
         CLI   INDEXUP,YES         TEST FOR INDEX UPGRADED RECORD               
         BNE   INVADD6                                                          
*******  GOTO1 CDEMOMTH,DMCB,=C'RROUND',DBAREC,DBAREC,MATHFAC                   
         GOTO1 CHELLO,(R1),(C'D',REPFILE),(X'DE',DBAREC),(2,INDEX)              
*                                                                               
INVADD6  DS    0H                                                               
         MVC   DEMCOD+1(2),=X'E4BF'  U191                                       
         L     RE,DBAREC                                                        
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
         GOTO1 CDEMOUT,DMCB,(C'D',DEMCOD),DBLOCK,DUB                            
         OC    DUB(4),DUB                                                       
         BZ    INVADD8                                                          
         OI    U191,X'40'         THERE WAS DEMO FOR U191 (21-49)               
         B     INVADD10                                                         
INVADD8  OI    U191,X'80'         NO DEMO FOR U191                              
         SPACE 1                                                                
INVADD10 GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,ADATAREC,MATHFAC                    
         B     INVADDX                                                          
         SPACE 1                                                                
INVADDX  B     EXXMOD                                                           
         EJECT                                                                  
* MANAGE INTERFACE WITH DEMO FILES - TIME PERIOD AND PROGRAM AVERAGE            
*                                                                               
DEMO     DS    0H                                                               
         MVI   TRWTOV,NO                                                        
         XC    DIVISOR,DIVISOR                                                  
         LA    RE,WORK3            A(I/O BUFFER FOR DEMAND)                     
         LA    RF,1000(RE)         WORK3+1000 IS A(INTERIM RECORD)              
         ST    RF,AINTEREC         SET POINTER TO IT                            
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   ADDSW,NO            INITIALIZE COMBO SWITCH                      
         TM    INVLTYP,X'08'       ADD SIGN GIVEN                               
         BZ    *+8                                                              
         MVI   ADDSW,YES           YES-SET SWITCH                               
         SPACE 1                                                                
         MVI   BYTE3,0             INITIALIZE FOR CAPTURE OF                    
         XC    PROGEL,PROGEL       PROGRAM NAME                                 
         MVI   PROGEL,1                                                         
         MVI   PROGEL+2,1                                                       
         B     DEMO2                                                            
         SPACE 2                                                                
DEMO2    DS    0H                  INITIALIZATION CODE                          
         BAS   RE,SETBLK           INITIALIZE DBLOCK                            
         BNE   EXXMOD                                                           
         SPACE 1                                                                
DEMO4    DS    0H                  PURE NUMBER PROCESSING                       
         TM    INVLTYP,X'80'       IS ENTRY A PURE NUMBER                       
         BZ    DEMO8               NO-GO TO DAY/TIME LOGIC                      
         SPACE                                                                  
         ZIC   R1,TOTWGHT                                                       
         ZIC   R0,INVLWT                                                        
         AR    R1,R0               UPDATE MANUAL WEIGHTS                        
         STC   R1,TOTWGHT                                                       
         TM    INVLTYP,X'04'       USER OVERRIDES WEIGHTING                     
         BZ    DEMO5               NO                                           
         CLI   TRWTOV,YES          CHECK FOR WEIGHTS DONE YET                   
         BE    DEMO5               YES                                          
         ZIC   R0,INVNO            NO-MAKE SURE ALL ENTRIES                     
         L     RE,INVLIST          HAVE WEIGHTING OVERRIDE                      
         TM    1(RE),X'04'         HAS WEIGHT BEEN INPUT                        
         BO    *+12                YES-GO LOOK AT NEXT ENTRY                    
         MVI   INVBAD,INVERR       NO-ERROR                                     
         B     EXXMOD                                                           
         LA    RE,10(RE)                                                        
         BCT   R0,*-20                                                          
         MVI   TRWTOV,YES          USER WANTS MANUAL WEIGHTING                  
         B     DEMO5                                                            
         SPACE 2                                                                
DEMO5    DS    0H                                                               
         GOTO1 CDEMAND,DMCB,DBLOCK,DEMO6                                        
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ERROR                               
         BNZ   DEMO10              NONE FOUND                                   
         MVI   INVBAD,NODEMOS                                                   
         B     EXXMOD                                                           
         SPACE 1                                                                
DEMO6    DS    0H                  HOOK ROUTINE FOR PURE NUMBER                 
         ST    RE,FULL             SAVE RETURN ADDRESS                          
         CLC   INVCODE,=C'PR'                                                   
         BNE   *+8                                                              
         BAS   RE,PURETEXT                                                      
*                                                                               
         LH    R2,DBFACTOR         INITIALIZE MATHFAC BLOCK FOR LATER           
         LA    RE,DBLOCK           WEIGHTING                                    
         ST    RE,MTHCFACS                                                      
         CLI   ADDSW,YES           FOR COMBOS, MAKE THE WEIGHT ONE              
         BNE   *+8                                                              
         LA    R2,1                UNLESS USER SPECIFIES WEIGHT                 
         CLI   TRWTOV,YES                                                       
         BNE   *+10                                                             
         ZIC   R2,INVLWT           MULTIPLY BY USER WEIGHT INSTEAD              
         ST    R2,MTHFCTR                                                       
         MVC   MTHIFIL,DBFILE                                                   
         MVC   MTHOSRC(1),DBSELSRC                                              
         BAS   RE,GETSHR           GET SHARES AND UPDATE ACCUMS                 
         SPACE 1                                                                
         XC    WORK,WORK           BUILD DUMMY UPGRADE ELEMENT                  
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVC   RAVLNBT,INVBTYPE    SET FROM BOOK TYPE                           
         MVI   RAVLNTYP,4                                                       
         MVI   RAVLNBKS,C'P'       FILE IS PAV                                  
         MVI   RAVLNCAT,C'P'       PURE NUMBER                                  
         MVC   RAVLNOP1,=X'FFFF'   INDEX UPGRADE TO GET OLD HPT                 
         MVC   RAVLNOP2,=H'1'      WEIGHTING                                    
         MVC   RAVLNOP3(4),MTHCFACS PASS A(DBLOCK)                              
         DROP  RE                                                               
*                                                                               
         L     R2,DBAREC           UPGRADE IN PLACE/MAD TO DATA REC             
         GOTO1 VDEMUP,DMCB,23(R2),WORK,(R7),DEMODUB,HOMSHR                      
*                                                                               
         MVC   MTHOFIL,=C'INV'     FORCE CONVERT TO IUN                         
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,ADATAREC,MATHFAC                    
         L     RE,FULL             RETURN POINT                                 
         BR    RE                                                               
         SPACE 2                                                                
DEMO8    DS    0H                  DAY/TIME PROCESSING                          
         MVI   TRHOOKSW,NO                                                      
         GOTO1 CDEMAND,DMCB,DBLOCK,HOOK                                         
         CLI   TRHOOKSW,YES                                                     
         BE    *+12                                                             
         MVI   INVBAD,NODEMOS      NO DEMOS FOUND BY DEMAND                     
         B     EXXMOD                                                           
*                                                                               
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BE    *+6                                                              
         DC    H'0'                DEMO MODULE ERROR                            
*                                                                               
*                                  UNWEIGHT INTERIM RECORD                      
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHFCTR+2(2),DBDIVSOR SET TOTAL WEIGHT                           
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC               
         BAS   RE,DIVSHR           UNWEIGHT THE SHARES                          
*                                                                               
         XC    WORK,WORK           BUILD DUMMY UPGRADE ELEMENT                  
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVC   RAVLNBT,INVBTYPE                                                 
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNBKS,DBFILE     SET FILE NAME FROM DBLOCK                    
         MVI   RAVLNCAT,C'D'       DAY/TIME                                     
         MVC   RAVLNOP1,=X'FFFF'   INDEX UPGRADE TO GET OLD HPT                 
         MVC   RAVLNOP2,=H'1'      WEIGHTING-UNWEIGHTED REC PASSED              
         LA    R1,DBLOCK           PASS A(DBLOCK)                               
         STCM  R1,15,RAVLNOP3                                                   
         DROP  RE                                                               
         SPACE 1                                                                
         L     R2,AINTEREC         UPGRADE INTERIM RECORD                       
         MVC   0(1,R2),DBFILE      FUDGE FIRST BYTE OF INTERIM RECORD           
         GOTO1 VDEMUP,DMCB,23(R2),WORK,(R7),DEMODUB,HOMSHR                      
*                                  MAD INTERIM TO FINAL DATA REC                
*                                  TO RESTORE WEIGHTING                         
         MVC   MTHIFIL,=C'PAV'     FORCE BOOK ELEMENT LOOKUP                    
         MVC   MTHOFIL,=C'INV'     CONVERT TO INVENTORY OUTPUT                  
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 CDEMOMTH,(R1),=C'MAD',AINTEREC,ADATAREC,MATHFAC                  
*                                                                               
         L     RE,AINTEREC         RE-CLEAR INTERIM RECORD AREA                 
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IT                                     
         B     DEMO10                                                           
         SPACE 2                                                                
DEMO10   EQU   *                                                                
         TM    INVCDCTL,TP         TIME PERIOD-PROGRAM NAMES                    
         BNO   DEMO12                                                           
         CLI   TRFNOVER,YES        TEST FOR FOOTNOTE SUPPRESSION                
         BE    DEMO12                                                           
         LA    R6,PROGEL                                                        
         USING PROGELD,R6                                                       
         OC    PNAME1,PNAME1       SKIP ELEMENT ADD IF NO                       
         BZ    DEMO11              ACCEPTABLE NAMES HAVE BEEN FOUND             
         MVI   PELLEN,PNAME2-PROGELD   ELEM LENGTH                              
         CLI   BYTE3,1             MORE                                         
         BE    *+18                THAN TWO PROGRAMS                            
         OC    PNAME2,PNAME2       FOR TWO PROGRAMS,                            
         BZ    *+8                 INCREASE EL LENGTH                           
         MVI   PELLEN,PROGELX-PROGELD                                           
         CLI   BYTE3,1                                                          
         BNE   *+16                                                             
         MVC   PNAME1,SPACES                                                    
         MVC   PNAME1(7),=C'VARIOUS'                                            
         GOTO1 VADDELEM,DMCB,(R4),(R6)                                          
         SPACE                                                                  
DEMO11   XC    PROGEL,PROGEL       RESET FOR NEXT ELEMENT                       
         MVI   PCODE,1                                                          
         MVI   PLIN,1                                                           
         MVI   BYTE3,0                                                          
         DROP  R6                                                               
         SPACE 2                                                                
DEMO12   EQU   *                                                                
         LH    R0,DBDIVSOR         UPDATE DIVISOR BUCKET TO PROVIDE             
         A     R0,DIVISOR          FOR MULTIPLE DEMAND CALLS.                   
         ST    R0,DIVISOR                                                       
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMS                           
         LA    R5,10(R5)                                                        
         BCT   R3,DEMO2                                                         
         SPACE                                                                  
         CLI   ADDSW,YES           SKIP DIVIDE BY TOTAL WEIGHT                  
         BE    DEMO15              FOR A COMBO                                  
         SPACE                                                                  
         LA    RE,DBLOCK                                                        
         ST    RE,MTHCFACS                                                      
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
         CLI   TRWTOV,NO           USE DEMO MODULE WEIGHTING                    
         BE    *+14                YES                                          
         ZIC   RE,TOTWGHT          NO TAKE USER OVERRIDE INSTEAD                
         B     *+8                                                              
         L     RE,DIVISOR                                                       
         ST    RE,MTHFCTR                                                       
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',(R4),(R4),MATHFAC                       
         SPACE 2                                                                
DEMO15   EQU   *                                                                
         MVI   INVBAD,0                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE DBLOCK.  ON EXIT CC=EQ FOR NO ERRORS                
* AND NEQ FOR ERROR WITH ERROR NUMBER SET IN INVBAD                             
*                                                                               
SETBLK   NTR1                                                                   
         XC    MATHFAC,MATHFAC                                                  
         XC    DBLOCK,DBLOCK                                                    
         LA    RE,WORK3            I/O BUFFER FOR INPUT RECORDS                 
         ST    RE,DBAREC           AT WORK3                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'S'                                                    
         MVC   DBSELAGY,REPALPHA   AGENCY                                       
         TM    INVSRC,X'41'        TEST FOR SRC                                 
         BO    SETB1                                                            
         MVI   DBSELSRC,C'N'                                                    
         TM    INVSRC,X'40'        TEST FOR NSI                                 
         BO    SETB1                                                            
         MVI   DBSELSRC,C'A'                                                    
SETB1    MVC   DBSELBK,INVFBK                                                   
         MVC   DBBTYPE,INVBTYPE    BOOK TYPE                                    
         MVC   DBSELSTA,DEMSTA                                                  
         TM    INVCDCTL,TP         TEST FOR TIME PERIOD CODE                    
         BO    SETB2                                                            
         TM    INVLTYP,X'80'       TEST FOR PURE NUMBER                         
         BO    *+12                                                             
         TM    INVCDCTL,MIX        DAY/TIME EXPRESSION FOR MIXED CODE           
         BO    SETB2               MEANS READ TIME PERIOD FILE                  
         SPACE 1                                                                
         MVC   DBFILE,=C'PAV'                                                   
         MVC   DBSELPUR,INVLNUMB   SET PURE NUMBER VALUE                        
         TM    INVLTYP,X'80'       TEST FOR PURE NUMBER                         
         BO    SETBX               EXIT IF YES                                  
         CLC   INVCODE,=C'PR'      CODE 'PR' REQUIRES PURE NUMBER               
         BNE   *+12                                                             
         MVI   INVBAD,INVERR                                                    
         B     SETBX                                                            
         SPACE 1                                                                
         XC    DBSELPUR,DBSELPUR                                                
         MVC   DBSELDAY,INVLDAY    DAY/TIME FROM LIST                           
         MVC   DBSELTIM,INVLSTIM                                                
         MVI   DBBEST,C'B'                                                      
         MVI   TRWTOV,NO           NO USER OVERRIDE IF A D/T IS IN LIST         
         B     SETBX                                                            
         SPACE 1                                                                
SETB2    DS    0H                                                               
         TM    INVLTYP,X'80'       TEST FOR PURE NUMBER                         
         BZ    *+12                                                             
         MVI   INVBAD,INVERR       TIME PERIOD REQUIRES DAY/TIME                
         B     SETBX                                                            
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBTPTT,C'T'         4 WK AVERAGE OR TYPICAL TIME                 
         CLC   INVCODE,=C'TP '     PT TRANSFERS GO TO TT                        
         BNE   *+8                                                              
         MVI   DBTPTT,C'P'                                                      
         SPACE 1                                                                
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+16                FOR A PS/1 STATION ON TIME PERIOD            
         CLI   DBSELSTA+4,C'2'                                                  
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   DBSELDAY,INVLDAY    SET DAY/TIME VALUES                          
         MVC   DBSELTIM,INVLSTIM                                                
         MVI   DBBEST,C'B'                                                      
         MVI   TRWTOV,NO                                                        
         B     SETBX                                                            
         SPACE 1                                                                
SETBX    EQU   *                                                                
         CLC   DBSELSTA(4),=C'UNIV'                                             
         BE    SETBX2                                                           
         CLC   DBSELSTA(4),=C'TELE'                                             
         BNE   SETBEXIT                                                         
SETBX2   EQU   *                                                                
         CLC   DBSELBK,=X'590B'                                                 
         BNE   SETBEXIT                                                         
         MVC   DBSELRMK(2),=H'98'                                               
SETBEXIT EQU   *                                                                
         CLI   INVBAD,0            SET CONDITION CODE ON EXIT                   
         B     EXXMOD                                                           
         EJECT                                                                  
* HOOK TO PROCESS DEMO RECORDS FROM DAY/TIME READ                               
*                                                                               
HOOK     NTR1                                                                   
         MVI   TRHOOKSW,YES                                                     
         L     R2,AINTEREC                                                      
         CLI   0(R2),0             HAS INTERIM RECORD AREA BEEN                 
         BNE   HOOK1               INITIALIZED                                  
         MVC   0(1,R2),DBFILE      NO-SET IT UP                                 
         MVC   20(2,R2),=H'24'     DUMMY RECORD LENGTH                          
*                                                                               
HOOK1    DS    0H                                                               
         LH    R1,DBFACTOR                                                      
         LA    R4,DBLOCK                                                        
         ST    R4,MTHCFACS                                                      
         CLI   ADDSW,YES           FOR COMBOS, MAKE THE WEIGHT ONE              
         BNE   *+8                                                              
         LA    R1,1                                                             
         ST    R1,MTHFCTR          MAD BY QUARTER HOUR WEIGHT TO                
         MVC   MTHIFIL,DBFILE      INTERIM RECORD                               
         MVC   MTHOFIL,DBFILE      KEEP FILE FORMAT ON OUTPUT                   
         MVC   MTHOSRC(1),DBSELSRC                                              
         BAS   RE,GETSHR           GET SHARES AND UPDATE ACCUMS                 
*                                                                               
*----->  B     HOOK1T              PUT THIS IN TO ROUND                         
         XCEFL IUNWORK,2016                                                     
         MVC   IUNWORK(1),DBFILE                                                
         MVC   IUNWORK+20,=H'24'   DUMMY RECORD LENGTH                          
         GOTO1 =V(REGETIUN),DMCB,(9,DBLOCK),IUNWORK+500,RR=RELO                 
         MVC   DBNUMVLS,=H'320'                                                 
         LA    R4,IUNWORK+500                                                   
         USING IUNREC,R4                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         GOTO1 CDEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                      
         L     R3,DBAQUART                                                      
         L     R4,DBAREC                                                        
         LA    RE,IUNWORK                                                       
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         GOTO1 CDEMAINT,DMCB,=C'PUT',DBLOCK,IUNWORK+500,OFORMAT                 
         DROP  R4                                                               
*                                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
         ST    R4,DBAREC           RESTORE DBLOCK STUFF                         
         ST    R3,DBAQUART                                                      
         B     HOOK2T                                                           
HOOK1T   GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
*                                                                               
HOOK2T   TM    INVCDCTL,TP         EXTRACT PROGRAM NAMES FOR                    
         BNO   HOOKX               TIME PERIOD FILE.                            
         CLI   TRFNOVER,YES        TEST FOR FOOTNOTE SUPPRESSION                
         BE    HOOKX                                                            
         LA    R6,PROGEL                                                        
         USING PROGELD,R6                                                       
         MVI   BYTE3,1                                                          
         OC    PNAME2,PNAME2                                                    
         BNZ   HOOKX               TWO PROGRAMS ALREADY                         
         LA    R2,PNAME1           R2 POINTS TO AREA FOR PROGRAM                
         OC    PNAME1,PNAME1                                                    
         BZ    *+8                                                              
         LA    R2,PNAME2                                                        
         GOTO1 VDEFINE,DMCB,=C'PROGRAM',DBLOCK,(R2)                             
         SPACE                                                                  
         MVI   BYTE3,0                                                          
         OC    PFBK,PFBK                                                        
         BNZ   HOOK7                                                            
         SPACE 1                                                                
         ZIC   R1,DBSELBK+1                                                     
         BCTR  R1,0                CONVERT MONTH TO ALPHA                       
         MH    R1,=H'3'                                                         
         LA    RE,MONTABLE(R1)                                                  
         MVC   PMON,0(RE)                                                       
         SPACE 1                                                                
         ZIC   R1,DBSELBK                                                       
         CVD   R1,DUB              MAKE YEAR PRINTABLE                          
         UNPK  PYR,DUB+6(2)                                                     
         OI    PYR+1,X'F0'                                                      
         MVC   PINVCODE,INVCODE    TP OR TT                                     
         MVI   PEQS,C'='                                                        
         OC    PFBK(9),SPACES                                                   
         SPACE 1                                                                
HOOK7    EQU   *                                                                
         CLC   PNAME1,PNAME2                                                    
         BNE   HOOKX               AND DUPLICATE PROGRAM NAMES                  
         XC    0(16,R2),0(R2)                                                   
         B     HOOKX                                                            
         DROP  R6                                                               
         SPACE 2                                                                
HOOKX    DS    0H                                                               
         B     EXXMOD                                                           
         SPACE 2                                                                
         DC    0H'0'                                                            
MONTABLE DC    CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE DAY/TIME TEXT FOR PR TRANSFER                         
*                                                                               
PURETEXT NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RINVTEL,R6                                                       
         MVI   RINVTCOD,X'01'      TEXT ELEMENT                                 
         MVI   RINVTLEN,25                                                      
         MVI   RINVTLIN,1                                                       
         SPACE 1                                                                
         ZIC   R1,DBSELBK+1                                                     
         BCTR  R1,0                CONVERT MONTH TO ALPHA                       
         MH    R1,=H'3'                                                         
         LA    RE,MONTABLE(R1)                                                  
         MVC   RINVTEXT(3),0(RE)                                                
         SPACE 1                                                                
         ZIC   R1,DBSELBK                                                       
         LA    R3,RINVTEXT+3       R3 POINTS TO OUTPUT                          
         CVD   R1,DUB              CONVERT YEAR TO CHARACTER                    
         UNPK  0(2,R3),DUB+6(2)    OUTPUT                                       
         OI    1(R3),X'F0'                                                      
         LA    R3,3(R3)                                                         
         SPACE 1                                                                
         XC    DUB,DUB                                                          
         MVC   0(3,R3),=C'PR='                                                  
         GOTO1 VDEFINE,DMCB,=C'DAY',DBLOCK,DUB                                  
         MVC   3(3,R3),DUB+2       DAY-ALPHA                                    
         XC    DUB,DUB                                                          
         LA    R3,8(R3)                                                         
         GOTO1 (RF),(R1),=C'TIME'                                               
         XC    DUB+4(2),DUB+4      START TIME ONLY                              
         GOTO1 VUNTIME,(R1),DUB+2,(R3)                                          
         OC    RINVTEXT(19),SPACES                                              
*                                  ADD ELEMENT TO RECORD                        
         LA    R2,34(R4)           POINT TO FIRST ELEMENT POSITION              
         ZIC   R1,RINVTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK        MOVE ELEMENT TO RECORD                       
         LA    R1,1(R1)            RESTORE ELEMENT LENGTH                       
         MVC   HALF2,27(R4)                                                     
         AH    R1,HALF2            UPDATE RECORD LENGTH                         
         STCM  R1,3,27(R4)                                                      
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
* ROUTINE TO GET SHARES AND UPDATE ACCUMULATORS                                 
*                                                                               
GETSHR   NTR1                                                                   
         MVC   DUB(2),DBACTBK                                                   
         GOTO1 CDEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHR                        
         MVC   DBACTBK,DUB         RESTORE BOOK VALUE                           
         SPACE 1                                                                
         LA    R1,HOMSHR           POINT TO OUTPUT AREA                         
         LA    RE,TOTSHR           POINT TO ACCUMS                              
         LA    R0,3                COUNTER                                      
*                                                                               
GETSHR2  L     RF,0(R1)                                                         
         MH    RF,MTHFCTR+2                                                     
         A     RF,0(RE)                                                         
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,GETSHR2                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO DIVIDE SHARES BY TOTAL WEIGHTING BEFORE DEMUP CALL                 
*                                                                               
DIVSHR   NTR1                                                                   
         LA    R0,3                COUNTER                                      
         LA    R1,TOTSHR           POINT TO SHARES                              
         LA    R2,HOMSHR           OUTPUT AREA FOR UNWEIGHTED SHARES            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DBDIVSOR  TOTAL WEIGHTING                              
         SPACE 1                                                                
DIVSHR2  L     RF,0(R1)                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                ROUNDED DIVIDE                               
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         ST    RF,0(R2)            UNWEIGHTED VALUE TO OUTPUT                   
         LA    R1,4(R1)            POINT TO NEXT SHARE                          
         LA    R2,4(R2)                                                         
         BCT   R0,DIVSHR2                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
         PRINT GEN                                                              
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
RELO     DS    A                                                                
REPFILE  DC    C'REPFILE'          HELLO FILE NAME                              
INDEX    DC    C'&&',X'FF'         INDEX UPGRADE MARKER                         
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
*                                                                               
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
SHARES   DC    X'00',C'S',AL1(1)                                                
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
IUNWORK  DS    2016C                                                            
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
*        EQUATES                                                                
MISINP   EQU   1                                                                
PRO      EQU   X'01'                                                            
INV      EQU   X'02'                                                            
TP       EQU   X'04'               READ TIME PERIOD FILE                        
MIX      EQU   X'08'               READ PAV AND TIME PERIOD FILES               
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
NODEMOS  EQU   140                                                              
INCINV   EQU   141                 INCONSISTENT BK TYPES ON INV RECDS           
         SPACE 2                                                                
* DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                  
*                                                                               
DEMOD    DSECT                                                                  
DIVISOR  DS    F                   DIVISOR BUCKET                               
ADATAREC DS    A                   A(DATA RECORD)                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
HOMSHR   DS    3F                                                               
         DS    0F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    A                   A(DBLOCK)                                    
MTHFCTR  DS    F                   WEIGHTING FACTOR FOR X AND /                 
MTHIFIL  DS    CL3                 INPUT FILE                                   
MTHOFIL  DS    CL3                 OUTPUT FILE                                  
MTHOSRC  DS    CL3                 OUTPUT SOURCE                                
         SPACE 2                                                                
ADDSW    DS    C                   Y=ADD DEMOS ONLY                             
IUNSW    DS    C                   Y=INVENTORY REC IN IUN FORMAT                
INDEXUP  DS    C                   Y=INV. REC HAS BEEN INDEX UPGRADED           
U191     DS    X                   X'80' - THERE IS NO U191                     
*                                  X'40' - THERE IS U191                        
DEMCOD   DS    CL3                                                              
         DS    0F                                                               
       ++INCLUDE DEDBLOCK                                                       
PROGEL   DS    CL60                PROGRAM NAME ELEMENT                         
         SPACE 2                                                                
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
*                                  X'04'  USER WEIGHTING OVERRIDE               
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
* PROGRAM NAME TEXT ELEMENT DSECT                                               
*                                                                               
PROGELD  DSECT                                                                  
PCODE    DS    X                                                                
PELLEN   DS    X                                                                
PLIN     DS    X                                                                
         DS    CL3                 SPARE                                        
PFBK     DS    0CL5                FROM BOOK                                    
PMON     DS    CL3                                                              
PYR      DS    CL2                                                              
         DS    CL1                 SPARE-BLANK                                  
PINVCODE DS    CL2                                                              
PEQS     DS    CL1                                                              
PNAME1   DS    CL16                FIRST PROGRAM NAME                           
PNAME2   DS    CL16                SECOND PROGRAM NAME                          
PROGELX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
*        DEMO RECORD IUN DSECT FOR USE BY FIXPAV                                
*                                                                               
IUNREC   DSECT                                                                  
UPREC    DS    0F                                                               
***********************************************************************         
*                                  ORIGINAL BOOK VALUES               *         
***********************************************************************         
OLDUNV   DS    (NUMVALS)F          UNIVERSES                          *         
OLDUNVX  EQU   *                                                      *         
***********************************************************************         
OLDRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   OLDRTG+(DISPHOM*4)                                               
UORHOMES DS    F                                                      *         
         ORG                                                                    
OLDIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
OLDRTGX  EQU   *                                                      *         
***********************************************************************         
OLDHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   OLDHPT+(DISPHOM*4)                                               
UOPHOMES DS    F                                                      *         
         ORG                                                                    
OLDTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
         ORG   OLDTOT+(DISPHOM*4)                                               
UOQHOMES DS    F                                                      *         
         ORG                                                                    
OLDHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  NEW VALUES                         *         
NEWUNV   EQU   OLDTOT              DEFINE ORIGIN FOR REGETIUN CALL    *         
*                                                                     *         
***********************************************************************         
NEWRTG   DS    (NUMVALS)F          RATINGS                            *         
         ORG   NEWRTG+(DISPHOM*4)                                               
UNRHOMES DS    F                                                      *         
         ORG                                                                    
NEWIMP   DS    (NUMVALS)F          IMPRESSIONS                        *         
NEWRTGX  EQU   *                                                      *         
***********************************************************************         
NEWHPT   DS    (NUMVALS)F          HUTS/PUTS                          *         
         ORG   NEWHPT+(DISPHOM*4)                                               
UNPHOMES DS    F                                                      *         
         ORG                                                                    
NEWTOT   DS    (NUMVALS)F          TSA TOTALS                         *         
NEWHPTX  EQU   *                                                      *         
***********************************************************************         
*                                  OTHER VALUES                       *         
***********************************************************************         
HOMESHR  DS    3F                  ORIGINAL HOMES SHARES              *         
HOMSHRX  EQU   *                                                      *         
HOMSHRLN EQU   *-HOMSHR                                               *         
***********************************************************************         
LUNV     DS    (NUMVALS)F          LOONEYVERSES                       *         
LUNVX    EQU   *                                                      *         
***********************************************************************         
UPRECX   DS    0F                                                               
*                                                                               
NUMVALS  EQU   32                                                               
DISPHOM  EQU   20                                                               
LENVALS  EQU   NUMVALS*4                                                        
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* REGENINV                                                                      
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* REGENAVL                                                                      
         PRINT OFF                                                              
RAVLD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116RELFM17   05/01/02'                                      
         END                                                                    
