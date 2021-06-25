*          DATA SET RERMP30    AT LEVEL 133 AS OF 08/12/09                      
*PHASE T81030C,*                                                                
         TITLE 'RERMP30/T81030 - INTERFACE DEMO MODULES FOR INV. X-FER'         
*                                                                               
********************************************************************            
*                                                                  *            
*        RERMP30 --- CREATE DEMO INVENTORY RECORDS                 *            
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
* MAY15/96 (GL ) --- REMOVED "*INCLUDE REGETIUN"--USING CORE VERSN *            
*          (GL ) --- FIXED COMPARE LENGTHS OF KEYS                 *            
*                                                                  *            
* MAY22/96 (NS ) --- CHANGED AIO3+1000, TO BUFF+4000 TO HANDLE INV *            
*                    RECS > 1000                                   *            
*                                                                  *            
* JUL24/96 (GL ) --- MEDIAFAX (MFX) SUPPORT                        *            
*                                                                  *            
* JUL31/96 (GL ) --- CODE TO SUPPORT FOOTNOTES                     *            
*                                                                  *            
* SEP19/96 (GL ) --- SUPPORT DEMO CALCULATION TAPE PRECISION       *            
*                                                                  *            
*                                                                  *            
* MAY22/96 (NS ) --- CHANGED AIO3+1000, TO BUFF+4500 TO HANDLE INV *            
*                    RECS > 1000                                   *            
*                                                                  *            
* DEC20/96 (GL ) --- PUT  PURETEXT  ROUTINE BACK IN ACTION FOR     *            
*                     PR TRANSFERS                                 *            
*                                                                  *            
* FEB27/97 (GL ) --- CHANGE CODE DUE TO CHANGE IN DEDEFINE--'TRAK' *            
*                                                                  *            
* OCT02/97 (GL ) --- PASS BACK PURE NUMBER IN INVLNUMB             *            
*                    !!NOTE!! INVLD ENTRIES WILL BE CLOBBERED HERE *            
*                                                                  *            
* OCT17/97 (GL ) --- RETURN A LIST OF DEMO HISTORY ELEMENTS        *            
*                                                                  *            
* MAR27/98 (GL ) --- SUPPORT WEIGHT-WITH-WEEKS                     *            
*                                                                  *            
* AUG03/99 (GL ) --- REPLACE INDEX/FF DEMUP CALL WITH INDEX/100    *            
*                                                                  *            
* JUN09/04 (BU ) --- SEND DBBTYPE AS LOWER CASE                    *            
*                                                                  *            
* MAR23/09 (KUI) --- NEW INVENTORY KEY                             *            
*                                                                  *            
********************************************************************            
*                                                                               
T81030   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 252,T81030,RA,RR=R2                                              
         LR    R0,RC                                                            
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,AIO3                                                          
         LA    R8,600(R8)                                                       
         USING DEMOD,R8                                                         
*                                                                               
         ST    R2,RELO                                                          
         ST    R0,AIUNREC                                                       
*                                                                               
         MVI   U191,0              CLEAR V2149 UNIVERSE FLAGS                   
         MVI   INVIND,0                                                         
         MVI   INVBAD,0                                                         
         XC    TOTWGHT,TOTWGHT                                                  
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR   CLEAR SHARE ACCUMULATORS                     
*                                                                               
         ZIC   R3,INVNO                                                         
         L     R5,INVLIST                                                       
         USING INVLD,R5                                                         
         L     R4,AIO              A(DATA REC) SET BY CALLING PROGRAM           
         ST    R4,ADATAREC         SAVE ITS ADDRESS                             
         USING RINVD,R4                                                         
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         SPACE                                                                  
         XC    FTNTWORK(FTNTWRKL),FTNTWORK  CLEAR WRK AREA FOR FTNOTES          
         BAS   RE,INITDH           INITIALIZE DEMO HISTORY NODE                 
*                                                                               
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
INVFILL  DS    0H                                                               
*                                                                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    RE,BUFF             READ INTO 1000 BYTE STORAGE                  
         A     RE,=F'6000'                                                      
         ST    RE,AIO                                                           
         ST    RE,DBAREC           SET INPUT RECORD ADDRESS                     
         ST    R7,DBCOMFCS                                                      
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBFILE,=C'INV'                                                   
         MVI   INVFRBT,X'FF'       INITIALIZE FOR FIRST INV REC                 
         SPACE                                                                  
INV1     MVI   BYTE2,0                                                          
         BAS   RE,INVWEGHT                                                      
INV2     EQU   *                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,DEMSTA                                                  
         MVC   RINVKINV,INVLNUMB                                                
         MVC   RINVKSTD,INVLDATE                                                
         GOTO1 HIGH                                                             
         SPACE 2                                                                
INV4     EQU   *                                                                
         CLC   RINVKEY(RINVKSTD-RINVKEY),KEYSAVE                                
         BE    *+12                                                             
         MVI   INVBAD,NODEMOS      COULD NOT FIND NUMBER                        
         B     EXXMOD                                                           
*!!!!    BNE   INV10               COULD NOT FIND NUMBER                        
         OC    INVLDATE,INVLDATE                                                
*!!!!    BZ    *+14                                                             
         BZ    INV4A                                                            
         CLC   RINVKEY(RINVKSPR-RINVKEY),KEYSAVE                                
*!!!!    BNE   INV10               DATE DID NOT MATCH                           
         BE    *+12                                                             
         MVI   INVBAD,NODEMOS      COULD NOT FIND NUMBER                        
         B     EXXMOD                                                           
INV4A    MVI   HALF,1                                                           
         SPACE 2                                                                
INV5     EQU   *                                                                
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         OC    INVLDATE,INVLDATE                                                
         BNZ   INV6                DATE FILTERING                               
         SPACE                                                                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    INV6                USE AN UNENDED INV HEADER                    
INV6C    GOTO1 SEQ                 KEEP READING                                 
*                                  SAME NUMBER                                  
         CLC   KEY(RINVKSTD-RINVKEY),KEYSAVE                                    
         BNE   INV5A               NO                                           
*                                  YES-IS IT A DATA RECORD                      
INVKEYD  USING RINVKEY,KEY                                                      
         OC    INVKEYD.RINVKRTP,INVKEYD.RINVKRTP                                
         BNZ   INV6C               YES-READ AGAIN                               
         DROP  INVKEYD                                                          
*                                                                               
         MVI   HALF,2              NO                                           
         B     INV5                READ RECORD AND DATE CHECK                   
*                                                                               
INV5A    DS    0H                                                               
*                                                                               
         CLI   HALF,2              GET LATEST EFFECT DATE HEADER?               
         BE    *+12                YES                                          
         CLI   HALF,1              MORE THAN 1 IS ERROR                         
         BNE   INV10                                                            
         L     R4,AIO                                                           
         MVC   KEY(27),0(R4)                                                    
         SPACE 2                                                                
INV6     EQU   *                                                                
         LA    R4,KEY              GET THE DATA RECORD                          
*                                                                               
         MVI   RINVKRSR,C'N'       RATING SOURCE                                
         MVC   RINVKQLF,INVSRC     BOOKVAL BITS                                 
         MVC   RINVKBTP,INVBTYPE   BOOKTYPE                                     
*                                                                               
         MVC   RINVKBK,INVFBK      SET BOOK                                     
*--MTA                                                                          
INV6L    GOTO1 HIGH                                                             
         CLC   RINVKEY,KEYSAVE                                                  
*!!      BNE   INV10               DID NOT FIND DATA REC                        
         BE    *+12                                                             
         MVI   INVBAD,NODEMOS                                                   
         B     EXXMOD                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO              RESTORE R4                                   
         SPACE 2                                                                
INV7     MVI   DUB,0               SET DEFAULT FROM BOOK TYPE                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R4)),0                         
         CLI   12(R1),0                                                         
         BNE   *+14                                                             
         L     RE,12(R1)                                                        
*                                                                               
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
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CD',(R4)),0                         
         L     RE,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
*                                                                               
         OC    INVIND,7(RE)        CUMULATIVE CODE ELEMENT BITS                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'5E',(R4)),0                         
         CLI   12(R1),0                                                         
         BE    INV9                TEMPORARY PATCH TO FORCE IN                  
         XC    WORK,WORK           AN X'5E' ELEMENT UNTIL FILE IS               
         MVC   WORK(2),=X'5E07'    FULLY CONVERTED                              
         MVC   WORK+2(3),=C'PTN'                                                
         MVC   WORK+5(2),=X'520A'                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),WORK                          
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
INV17    MVI   INVBAD,INVALID                                                   
         B     EXXMOD                                                           
         SPACE 2                                                                
INVEND   EQU   *                                                                
         L     R4,ADATAREC         RESET I/O POINTER TO DATA RECORD             
         ST    R4,AIO                                                           
         LA    RE,DBLOCK                                                        
         ST    RE,MTHCFACS                                                      
         L     R5,INVLIST                                                       
         TM    INVLTYP,X'08'       SKIP DIVIDE FOR ADD ONLY                     
         BO    INVEND2                                                          
         SPACE                                                                  
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         SR    RE,RE                                                            
         ICM   RE,3,TOTWGHT                                                     
         ST    RE,MTHFCTR                                                       
         MVC   MTHOSRC,=C'NSI'                                                  
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',(R4),(R4),MATHFAC                       
         SPACE 2                                                                
INVEND2  EQU   *                                                                
                                                                                
         BAS   RE,PUTFTNT          PUT FOOTNOTE ELEM INTO RECORD                
                                                                                
                                                                                
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
* GET INVENTORY WEIGHT OFF THE HEADER RECORD IF NOT PASSED                      
* BY THE USER (IF INVLWT NOT = 1)                                               
*                                                                               
INVWEGHT NTR1                                                                   
         CLI   INVTYP,C'I'                                                      
         BNE   INVWEX                                                           
         CLI   INVLWT,0                                                         
         BNE   INVWEX                                                           
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'IUN'                                                   
         LA    RE,BUFF             READ INTO 1000 BYTE STORAGE                  
         A     RE,=F'6000'                                                      
         ST    RE,AIO                                                           
         ST    RE,DBAREC           SET INPUT RECORD ADDRESS                     
         MVI   DBFUNCT,DBGETDEM                                                 
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'U'                                                    
         MVC   DBSELSTA,DEMSTA                                                  
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELINV,INVLNUMB                                                
         LA    RF,MYDBXTND                                                      
         USING DBXINVWK,RF                                                      
         MVC   DBXIWID,=C'RINV'                                                 
         XC    DBXIWNXT,DBXIWNXT                                                
         DROP  RF                                                               
         STCM  RF,15,DBEXTEND                                                   
                                                                                
         GOTO1 DEMAND,DMCB,DBLOCK,0,0,0,0,0                                     
         MVC   INVLWT,DBDIVSOR+1                                                
*                                                                               
INVWEX   B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO ADD WEIGHTED INPUT INVENTORY RECORD INTO OUTPUT                
* DATA RECORD.  INDEX 'FF' DEMUP CALL INSURES THAT INPUT RECORD HAS             
* SET OF HPTS BEFORE WEIGHTING.  DBAREC=A(INPUT RECORD), ADATAREC               
* =A(OUTPUT RECORD)                                                             
*                                                                               
INVADD   NTR1                                                                   
         BAS   RE,GETIFTNT         HANDLE FOOTNOTES                             
                                                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,TOTWGHT                                                     
         LA    R2,1                                                             
         CLI   INVLWT,1                                                         
         BE    *+18                WEIGHT ONLY FOR INV NUM                      
         TM    INVLTYP,X'80'       INPUT                                        
         BZ    *+10                                                             
         ZIC   R2,INVLWT                                                        
         AR    R0,R2                                                            
         STCM  R0,3,TOTWGHT                                                     
         SPACE                                                                  
         MVI   IUNSW,NO            IS INVENTORY RECORD IN IUN FORMAT            
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'5E',DBAREC),0                       
         CLI   12(R1),0                                                         
         BNE   INVADD2                                                          
         L     R6,12(R1)                                                        
         CLC   2(3,R6),=C'IUN'                                                  
         BNE   *+8                                                              
         MVI   IUNSW,YES                                                        
*                                                                               
INVADD2  DS    0H                                                               
         MVI   INDEXUP,NO          SET INDEX UPGRADED FLAG TO NO                
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'DE',DBAREC),(2,INDEX)               
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
* NOTE: EVEN THOUGH THE RECORD IS AN INVENTORY TRACK, IT STILL NEEDS            
*        TO BE EXPLODED OUT VIA  GETIUN TO GET RTG/IMP/PUT/TOT/SHRS.            
*       ALSO, WE DO NOT NEED TO GET THE H/P/T LINE TO DO THE 'MAD',             
*        SINCE THE CALLER WILL GET THE H/P/T FOR THE FINAL RECD ANYWAY.         
                                                                                
         PRINT OFF                                                              
*&&DO                                                                           
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
         MVI   BYTE,0                                                           
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                                                               
         NI    11(R6),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         L     R7,ACOMFACS                                                      
*                                                                               
         MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
*                                                                               
         GOTO1 DEMUP,DMCB,34(R6),(BYTE,WORK),(R7),RIDBLK,HOMSHR                 
         OI    11(R6),X'40'                                                     
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
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R6)),WORK                          
                                                                                
         MVI   BYTE,0                                                           
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                                                               
         NI    11(R6),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         L     R7,ACOMFACS                                                      
*                                                                               
         MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
*                                                                               
         GOTO1 DEMUP,(R1),(C'I',34(R6)),(BYTE,WORK),(R7),RIDBLK                 
         OI    11(R6),X'40'                                                     
         DROP  RE                                                               
         SPACE 1                                                                
*&&                                                                             
         PRINT ON                                                               
         DS    0H                                                               
         LHI   R0,(RINVPEL-RINVREC)                                             
         A     R0,DBAREC                                                        
         ST    R0,DBAQUART                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBTAPEP,TAPEOPT                                                  
         MVC   DBSELAGY,AGENCY                                                  
         MVI   DBERROR,0                                                        
*                                                                               
         BAS   RE,CNVIUN                                                        
         EJECT                                                                  
         PRINT OFF                                                              
*&&DO                                                                           
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
*&&                                                                             
         PRINT ON                                                               
         DS    0H                                                               
         MVI   DBINTFIL,C'I'                                                    
         MVI   DBINTMED,C'U'                                                    
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 CDEMOMTH,DMCB,=C'MAD',AIUNREC,ADATAREC,MATHFAC                   
                                                                                
*                                                                               
         MVC   DBFILE,=C'IUN'                                                   
         BAS   RE,BUILDDH          BUILD DEMO HISTORY ELEMENT                   
         B     INVADDX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
INVADDX  B     EXXMOD                                                           
         EJECT                                                                  
* MANAGE INTERFACE WITH DEMO FILES - TIME PERIOD AND PROGRAM AVERAGE            
*                                                                               
DEMO     DS    0H                                                               
         MVI   TRWTOV,NO                                                        
         XC    DIVISOR,DIVISOR                                                  
         L     RE,AIO2             A(I/O BUFFER FOR DEMAND)                     
         ST    RE,AINTEREC         SET POINTER TO IT                            
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
         SR    R1,R1                                                            
         ICM   R1,3,TOTWGHT                                                     
         ZIC   R0,INVLWT                                                        
         AR    R1,R0               UPDATE MANUAL WEIGHTS                        
         STCM  R1,3,TOTWGHT                                                     
         TM    INVLTYP,X'04'       USER OVERRIDES WEIGHTING                     
         BZ    DEMO5               NO                                           
         CLI   TRWTOV,YES          CHECK FOR WEIGHTS DONE YET                   
         BE    DEMO5               YES                                          
         ZIC   R0,INVNO            NO-MAKE SURE ALL ENTRIES                     
         L     RE,INVLIST          HAVE WEIGHTING OVERRIDE                      
         TM    1(RE),X'04'         HAS WEIGHT BEEN INPUT                        
         BO    *+12                YES-GO LOOK AT NEXT ENTRY                    
         MVI   INVBAD,INVALID      NO-ERROR                                     
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
                                                                                
*                                                                               
         CLC   INVCODE,=C'PR'                                                   
         BNE   *+8                                                              
         BAS   RE,PURETEXT                                                      
                                                                                
*                                                                               
         DS    0H                  PROGRAM-TITLE FOOTNOTE PROCESSING            
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 DEFINE,MYDMCB,=C'PROGRA',DBLOCK,WORK                             
         MVC   FNWPROG2,WORK                                                    
         OC    FNWPROG1,FNWPROG1                                                
         BNZ   *+10                                                             
         MVC   FNWPROG1,WORK                                                    
         MVC   FNWBOOK,DBACTBK                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVIUN           CONVERT TO IUN FMT, PLACE IN AIUNREC         
                                                                                
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
         CLI   DBSELSRC,C'H'       SWITCH TO NTI FOR NHTI FILE                  
         BNE   *+8                                                              
         MVI   MTHOSRC,C'N'                                                     
         BAS   RE,GETSHR           GET SHARES AND UPDATE ACCUMS                 
         PRINT OFF                                                              
*&&DO                                                                           
         SPACE 1                                                                
         XC    WORK,WORK           BUILD DUMMY UPGRADE ELEMENT                  
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
*&&DO                                                                           
         MVC   RAVLNBT,INVBTYPE    SET FROM BOOK TYPE                           
*&&                                                                             
         MVI   RAVLNTYP,4                                                       
*&&DO                                                                           
         MVI   RAVLNBKS,C'P'       FILE IS PAV                                  
         MVI   RAVLNCAT,C'P'       PURE NUMBER                                  
         MVC   RAVLNOP1,=X'FFFF'   INDEX UPGRADE TO GET OLD HPT                 
         MVC   RAVLNOP2,=H'1'      WEIGHTING                                    
         MVC   RAVLNOP3(4),MTHCFACS PASS A(DBLOCK)                              
*&&                                                                             
         MVC   RAVLNOP1,=Y(100)                                                 
         DROP  RE                                                               
*                                                                               
         L     R2,DBAREC           UPGRADE IN PLACE/MAD TO DATA REC             
         MVI   BYTE,0                                                           
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                                                               
         MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
         GOTO1 DEMUP,DMCB,23(R2),(BYTE,WORK),(R7),RIDBLK,HOMSHR                 
         MVC   MTHOFIL,=C'INV'     FORCE CONVERT TO IUN                         
         MVC   MTHOSRC,=C'NSI'                                                  
*                                                                               
         MVI   DUB,0               NETWORK FUDGE                                
*        CLI   DBSELMED,C'N'                                                    
*        BNE   *+18                                                             
*        MVC   DUB(1),DBSELSRC                                                  
*        MVI   DBSELMED,C'T'                                                    
*        MVI   DBSELSRC,C'N'                                                    
*&&                                                                             
         PRINT ON                                                               
                                                                                
*&&DO                                                                           
         GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,ADATAREC,MATHFAC                    
*&&                                                                             
         DS    0H                                                               
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
         MVI   DBINTFIL,C'I'                                                    
         MVI   DBINTMED,C'U'                                                    
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 CDEMOMTH,DMCB,=C'MAD',AIUNREC,ADATAREC,MATHFAC                   
                                                                                
*        CLI   DUB,0                                                            
*        BE    *+14                                                             
*        MVI   DBSELMED,C'N'                                                    
*        MVC   DBSELSRC,DUB                                                     
                                                                                
         BAS   RE,BUILDDH          BUILD DEMO HISTOR ELEMENT                    
         L     RE,FULL             RETURN POINT                                 
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
DEMO8    DS    0H                  DAY/TIME PROCESSING                          
         MVI   TRHOOKSW,NO                                                      
         GOTO1 CDEMAND,DMDMCB,DBLOCK,HOOK                                       
         CLI   TRHOOKSW,YES                                                     
         BE    *+12                                                             
         MVI   INVBAD,NODEMOS      NO DEMOS FOUND BY DEMAND                     
         B     EXXMOD                                                           
*                                                                               
         CLI   DBERROR,X'80'       TEST FOR E-O-F                               
         BE    *+6                                                              
         DC    H'0'                DEMO MODULE ERROR                            
*                                                                               
*        MVI   RESETDB,0            RESET AFTER MATH CALL                       
*        CLI   DBSELMED,C'N'       IF NETWK, FUDGE MEDIA & SRC FOR MAD          
*        BNE   *+22                                                             
*        MVI   RESETDB,C'Y'         RESET AFTER MATH CALL                       
*        MVI   DBSELMED,C'T'                                                    
*        MVC   SVSRC,DBSELSRC                                                   
*        MVI   DBSELSRC,C'N'                                                    
*                                  UNWEIGHT INTERIM RECORD                      
         XC    MTHFCTR,MTHFCTR                                                  
         MVC   MTHFCTR+2(2),DBDIVSOR SET TOTAL WEIGHT                           
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',AINTEREC,AINTEREC,MATHFAC               
         BAS   RE,DIVSHR           UNWEIGHT THE SHARES                          
*                                                                               
*        CLI   RESETDB,C'Y'         COMING FROM NETWORK CALL                    
*        BNE   *+12                                                             
*        LA    R1,DMCB             BYPASS DEMUP CALL                            
*        B     DEMO9                                                            
*                                                                               
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
DEMO8D   XC    WORK,WORK           BUILD DUMMY UPGRADE ELEMENT                  
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
         MVI   BYTE,0                                                           
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                                                               
         MVC   RIDBLK(4),=CL4'RID=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
*                                                                               
         GOTO1 DEMUP,DMCB,23(R2),(BYTE,WORK),(R7),RIDBLK,HOMSHR                 
*&&                                                                             
         PRINT ON                                                               
*                                  MAD INTERIM TO FINAL DATA REC                
*                                  TO RESTORE WEIGHTING                         
DEMO9    MVC   MTHIFIL,=C'PAV'     FORCE BOOK ELEMENT LOOKUP                    
         MVC   MTHIFIL,=C'INV'                                                  
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
*^^GYL  PROGRAM NAMES HAVE BEEN REPLACED BY PROGRAM-TITLE FOOTNOTES             
*&&DO                                                                           
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
         MVC   PNAME1,BLANKS                                                    
         MVC   PNAME1(7),=C'VARIOUS'                                            
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,(R4)),(R6)                          
         SPACE                                                                  
DEMO11   XC    PROGEL,PROGEL       RESET FOR NEXT ELEMENT                       
         MVI   PCODE,1                                                          
         MVI   PLIN,1                                                           
         MVI   BYTE3,0                                                          
         DROP  R6                                                               
*&&                                                                             
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
         SR    RE,RE                                                            
         ICM   RE,3,TOTWGHT        NO TAKE USER OVERRIDE INSTEAD                
         B     *+8                                                              
         L     RE,DIVISOR                                                       
         ST    RE,MTHFCTR                                                       
         GOTO1 CDEMOMTH,DMCB,=C'DIVIDE',(R4),(R4),MATHFAC                       
*                                                                               
*        CLI   RESETDB,C'Y'         RESET DBLOCK FLDS FROM NHTI FUDGE?          
*        BNE   *+18                                                             
*        MVI   RESETDB,0                                                        
*        MVI   DBSELMED,C'N'                                                    
*        MVC   DBSELSRC,SVSRC                                                   
*                                                                               
         SPACE 2                                                                
DEMO15   EQU   *                                                                
                                                                                
         CLC   INVCODE,=C'PR'      FOOTNOTE ELEM FOR PR TRANSFERS               
         BE    *+8                  ARE DONE DIFFERENTLY                        
         BAS   RE,PUTFTNT          PUT FOOTNOTE ELEM INTO RECORD                
                                                                                
*                                                                               
         MVI   INVBAD,0                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE DBLOCK.  ON EXIT CC=EQ FOR NO ERRORS                
* AND NEQ FOR ERROR WITH ERROR NUMBER SET IN INVBAD                             
*                                                                               
SETBLK   NTR1                                                                   
         XC    MATHFAC,MATHFAC                                                  
         XC    DBLOCK,DBLOCK                                                    
         LA    RE,BUFF             I/O BUFFER FOR INPUT RECORDS                 
         A     RE,=F'6000'                                                      
         ST    RE,DBAREC           AT BUFF+4500                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBFUNCT,DBGETDEM                                                 
         TM    INVLTYP,X'80'       TEST FOR PURE NUMBER                         
         BZ    *+8                                                              
         MVI   DBFUNCT,DBGETPUR    SET PURE LOOKUP                              
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'S'                                                    
         MVC   DBSELAGY,AGENCY     AGENCY                                       
         MVC   DBTIMCHG,TIMECHG    TIME CHANGE                                  
         MVC   DBPRGDUR,WGTWEEK    WEIGH-WITH-WEEKS FLAG                        
*&&DO                                                                           
         LA    RF,INVDYTIM                                                      
         ST    RF,DBEXTEND         SET DAY TIME BLOCK                           
*&&                                                                             
         LA    RF,INVDYTIM                                                      
         ST    RF,DBEXTEND         SET DAY TIME BLOCK                           
         MVC   DBDQD(2),=AL2(DBDQUXTD)  USE EXTENSION FOR DYTIMS                
*                                                                               
         TM    INVSRC,X'41'        TEST FOR SRC                                 
         BO    SETB1                                                            
         MVI   DBSELSRC,C'N'                                                    
         TM    INVSRC,X'40'        TEST FOR NSI                                 
         BO    *+12                                                             
         MVI   DBSELSRC,C'M'    <-MFX EQUATED TO ARB IN BKVAL-- WAS 'A'         
         B     SETB1                                                            
         CLI   DEMSTA+4,C'H'       IF NSI REQSTD BUT STATION -H ITS NHT         
         BNE   SETB1                FILE FOR NETWORK                            
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELMED,C'N'                                                    
*                                                                               
SETB1    MVC   DBSELBK,INVFBK                                                   
         MVC   DBBTYPE,INVBTYPE    BOOK TYPE                                    
*                                                                               
***>>>   NI    DBBTYPE,X'FF'-X'40' MAKE 'BOOK TYPE' LOWER CASE                  
*                                                                               
         MVC   DBSELSTA,DEMSTA                                                  
         MVC   DBTAPEP,TAPEOPT                                                  
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
         MVI   INVBAD,INVALID                                                   
         B     SETBX                                                            
         SPACE 1                                                                
         XC    DBSELPUR,DBSELPUR                                                
         MVC   DBSELDAY,INVLDAY    DAY/TIME FROM LIST                           
         MVC   DBSELTIM,INVLSTIM                                                
         MVI   DBBEST,C'B'                                                      
         OC    INVPRG#,INVPRG#     SPECIFIC PROGRAM # REQUESTED?                
         BZ    *+8                   NO, SKIP NEXT                              
         MVI   DBBEST,C'A'           YES, GET ALL PROGRAMS                      
         CLI   DBSELSTA+4,C'H'                                                  
         BNE   *+8                                                              
         MVI   DBBEST,C'L'         ALWAYS CALL DEMAND W/'L'                     
         MVI   TRWTOV,NO           NO USER OVERRIDE IF A D/T IS IN LIST         
         B     SETBX                                                            
         SPACE 1                                                                
SETB2    DS    0H                                                               
         TM    INVLTYP,X'80'       TEST FOR PURE NUMBER                         
         BZ    *+12                                                             
         MVI   INVBAD,INVALID      TIME PERIOD REQUIRES DAY/TIME                
         B     SETBX                                                            
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBTPTT,C'T'         4 WK AVERAGE OR TYPICAL TIME                 
         CLC   INVCODE,=C'TP '     PT TRANSFERS GO TO TT                        
         BNE   *+8                                                              
         MVI   DBTPTT,C'P'                                                      
         SPACE 1                                                                
         CLI   DBSELSTA+4,C'H'     STATION -H = NHTI FILE OF NETWORK            
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+16                FOR A PS/1 STATION ON TIME PERIOD            
         CLI   DBSELSTA+4,C'2'                                                  
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   DBSELDAY,INVLDAY    SET DAY/TIME VALUES                          
         MVC   DBSELTIM,INVLSTIM                                                
         OC    INVPRG#,INVPRG#     SPECIFIC PROGRAM # REQUESTED?                
         BZ    *+8                   NO, SKIP NEXT                              
         MVI   DBSELWKN,X'FF'        YES, ASK "DEMAND" FOR FULL SRCH            
         MVI   DBBEST,C'B'                                                      
         CLI   DBSELSTA+4,C'H'                                                  
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
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
*                                                                               
         OC    INVPRG#,INVPRG#            OVERRIDE REQUEST?                     
         BZ    HOOK5                                                            
                                                                                
         L     RF,DBAQUART                                                      
         USING PHELEM,RF                                                        
         CLI   PHDTYPE,0                  IF THIS IS "NORMAL" DATA              
         BE    HOOK3                      THEN ADJUST WEIGHT & SKIP             
         DROP  RF                                                               
                                                                                
         LA    R0,=C'PVNO'                                                      
         CLC   DBFILE,=C'PAV'             PAV OR TP FILE?                       
         BE    *+8                                                              
         LA    R0,=C'TPNO'                                                      
         GOTO1 DEFINE,PARAS,(R0),DBLOCK,PROGNUM                                 
         SR    R0,R0                                                            
         CLC   PROGNUM,=CL8'N/A'          VALID PROGNUM RETURNED?               
         BE    *+14                         NO,  PROGBNUM SET TO 0              
         PACK  DUB,PROGNUM                  YES, CONVERT TO BINARY              
         CVB   R0,DUB                                                           
         STCM  R0,7,PROGBNUM                                                    
         CLC   PROGBNUM,INVPRG#           DOES THIS MATCH REQUESTED?            
         BE    HOOK5                        YES, CONTINUE                       
HOOK3    ZICM  RF,DBDIVSOR,(3)              NO, ADJUST WEIGHTING                
         ZICM  RE,DBFACTOR,(3)                                                  
         SR    RF,RE                                                            
         STCM  RF,3,DBDIVSOR                                                    
         B     HOOKX                        AND END ROUTINE                     
*                                                                               
HOOK5    MVI   TRHOOKSW,YES                 YES, BEGIN PROCESSING               
*                                                                               
         DS    0H                  FOOTNOTE PROCESSING                          
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 DEFINE,MYDMCB,=C'PROGRA',DBLOCK,WORK                             
         MVC   FNWPROG2,WORK                                                    
         OC    FNWPROG1,FNWPROG1                                                
         BNZ   *+10                                                             
         MVC   FNWPROG1,WORK                                                    
         MVC   FNWBOOK,DBACTBK                                                  
                                                                                
*                                                                               
         L     R2,AINTEREC                                                      
         CLI   0(R2),0             HAS INTERIM RECORD AREA BEEN                 
         BNE   HOOK10              INITIALIZED                                  
*&&DO                                                                           
         MVC   0(1,R2),DBFILE      NO-SET IT UP                                 
         MVC   20(2,R2),=H'24'     DUMMY RECORD LENGTH                          
*&&                                                                             
         USING RINVREC,R2                                                       
         MVI   RINVREC,RINVKTYQ    NO-SET IT UP                                 
         MVC   RINVLEN,=Y(RINVPEL-RINVREC)                                      
         DROP  R2                                                               
*                                                                               
HOOK10   DS    0H                                                               
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
****  ---->  B     HOOK1T              PUT THIS IN TO ROUND                     
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
**       XCEFL IUNWORK,2016                                                     
         L     RE,AIUNREC                                                       
         LA    RF,2016                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R2,AIUNREC                                                       
*                                                                               
         MVC   0(1,R2),DBFILE                                                   
         MVC   20(2,R2),=H'24'   DUMMY RECORD LENGTH                            
         AHI   R2,700                                                           
         GOTO1 REGETIUN,DMCB,(9,DBLOCK),(R2)                                    
         MVC   DBNUMVLS,=H'320'                                                 
         LR    R4,R2                                                            
         USING IUNREC,R4                                                        
         MVC   NEWRTG(LENVALS),OLDRTG                                           
         MVC   NEWIMP(LENVALS),OLDIMP                                           
         MVC   NEWHPT(LENVALS),OLDHPT                                           
         MVC   NEWTOT(LENVALS),OLDTOT                                           
         GOTO1 CDEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                      
         L     R3,DBAQUART                                                      
         L     R4,DBAREC                                                        
         L     RE,AIUNREC                                                       
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         MVC   WORK+0(7),=C'INVUIUN'                                            
         MVC   WORK+7(2),=X'5A0B'                                               
         CLI   DBTAPEP,C'Y'                                                     
         BE    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         MVI   WORK+9,0                                                         
         L     R2,AIUNREC                                                       
         AHI   R2,700                                                           
         GOTO1 CDEMAINT,DMCB,=C'PUT',DBLOCK,(R2),WORK                           
         DROP  R4                                                               
*&&                                                                             
         PRINT ON                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVIUN           CONVERT TO IUN FMT, PLACE IN AIUNREC         
*                                                                               
HOOK1F   MVC   MTHOSRC,=C'NSI'                                                  
*&&DO                                                                           
         GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC                    
*&&                                                                             
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVI   DBINTFIL,C'I'                                                    
         MVI   DBINTMED,C'U'                                                    
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 CDEMOMTH,DMCB,=C'MAD',AIUNREC,AINTEREC,MATHFAC                   
*                                                                               
*&&DO                                                                           
HOOK1G   ST    R4,DBAREC           RESTORE DBLOCK STUFF                         
         ST    R3,DBAQUART                                                      
*&&                                                                             
         B     HOOK2T                                                           
*                                                                               
*SEE COMMENT ABOVE:  HOOK1T IS FOR ROUNDING?????                                
***  HOOK1T   GOTO1 CDEMOMTH,DMCB,=C'MAD',DBAREC,AINTEREC,MATHFAC               
*                                                                               
HOOK2T   DS    0H                                                               
         BAS   RE,BUILDDH          BUILD DEMO HISTORY ELEMENTS                  
*&&DO                                                                           
HOOK2T   TM    INVCDCTL,TP         EXTRACT PROGRAM NAMES FOR                    
*&&                                                                             
         TM    INVCDCTL,TP         EXTRACT PROGRAM NAMES FOR                    
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
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,(R2)                              
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
         OC    PFBK(9),BLANKS                                                   
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
         MVI   RINVTLEN,28                                                      
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
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,DUB                                   
         MVC   3(3,R3),DUB+2       DAY-ALPHA                                    
         XC    DUB,DUB                                                          
         LA    R3,8(R3)                                                         
         GOTO1 (RF),(R1),=C'TIME'                                               
         XC    DUB+4(2),DUB+4      START TIME ONLY                              
         GOTO1 UNTIME,(R1),DUB+2,(R3)                                           
         OC    RINVTEXT(22),BLANKS                                              
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
         CLI   DBSELMED,C'N'                                                    
         BNE   GETSH1                                                           
         GOTO1 CDEMOUT,DMCB,(C'P',SHARES),DBLOCK,HOMSHR                         
         B     GETSH1A                                                          
*                                                                               
GETSH1   GOTO1 CDEMOUT,DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHR                        
*                                                                               
GETSH1A  MVC   DBACTBK,DUB         RESTORE BOOK VALUE                           
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
***********************************************************************         
*============== GET FOOTNOTE FROM INVENTORY TRACK RECORD =============*         
                                                                                
* THIS IS A VERY SPECIFIC ROUTINE.  IT IS CALLED FROM THE "INVADD"              
*  ROUTINE AND ITS MAIN PURPOSE IS TO EXTRACT FOOTNOTE FROM THE                 
*  INVENTORY TRACK RECORD BEING ADDED.  WE HAVE TO BE CAREFUL THAT WE           
*  DO NOT CLOBBER OR BREAK THINGS FOR "INVADD"S CALLER(S).                      
                                                                                
GETIFTNT DS    0H                                                               
GIF      NTR1                                                                   
         DS    0XL(1-(GIF-GETIFTNT))  NO CODE IS S/B BETWN THE LABELS           
                                                                                
*                                                                               
** TAKE PRECAUTIONARY STEPS FIRST **                                            
*                                                                               
         DS    0H                  SAVE CALLER'S DBLOCK                         
         LA    R0,SVDBLOCK                                                      
         LA    R1,DBLOCK_L                                                      
         LA    RE,DBLOCK                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                  REMEMBER KEY OF INV TRACK                    
         L     RF,DBAREC                                                        
         MVC   ITRKKEY,0(RF)                                                    
                                                                                
*                                                                               
** EXTRACT FOOTNOTE **                                                          
*                                                                               
ITK      USING RINVKEY,ITRKKEY                                                  
         MVC   DBFILE,=C'IUN'                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELSRC,0                                                       
         MVI   DBSELMED,C'U'                                                    
         MVC   DBSELSTA,ITK.RINVKSTA                                            
         MVC   DBSELINV,ITK.RINVKINV                                            
         GOTO1 DATCON,DMCB,(3,ITK.RINVKSTD),(2,DUB),0                           
         MVC   DBSELDAT,DUB                                                     
*        MVC   DBSTYPE,ITK.RINVKSRC                                             
         MVI   DBSTYPE,X'FF'                                                    
         MVC   DBSELBK,ITK.RINVKBK                                              
         MVI   DBSELDAY,0                                                       
         MVC   DBSELTIM,=AL2(0600,2959)   DON'T LET DEMAND FUDGE TIMES          
         LA    RF,MYDBXTND                                                      
         USING DBXINVWK,RF                                                      
         MVC   DBXIWID,=C'RINV'                                                 
         XC    DBXIWNXT,DBXIWNXT                                                
         MVC   DBXIKRSR,ITK.RINVKRSR                                            
         MVC   DBXIKDSR,ITK.RINVKDSR                                            
         MVC   DBXIKQLF,ITK.RINVKQLF                                            
         MVC   DBXIKBTP,ITK.RINVKBTP                                            
         DROP  RF                                                               
         STCM  RF,15,DBEXTEND                                                   
                                                                                
         GOTO1 DEMAND,DMCB,DBLOCK,GIFHOOK,0,0,0,0                               
         DROP  ITK                                                              
                                                                                
*                                                                               
** RESTORE CALLER'S VALUES **                                                   
*                                                                               
         DS    0H                  RESTORE CALLER'S DBLOCK                      
         LA    R0,SVDBLOCK                                                      
         LA    R1,DBLOCK_L                                                      
         LA    RE,DBLOCK                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
*                                                                               
         L     RF,DBAREC                                                        
         CLC   ITRKKEY,0(RF)       MAKE SURE ORIG RECD IS IN DBAREC             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
** EXIT ROUTINE **                                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
** DEMAND HOOK **                                                               
*                                                                               
* AT ENTRY,                                                                     
*   ITRKKEY = KEY OF INVENTORY TRAK WE'RE SHOOTING FOR.                         
                                                                                
GIFHOOK  DS    0H                                                               
GIH      NTR1                                                                   
         DS    0XL(1-(GIH-GIFHOOK))  NO CODE IS S/B BETWN THE LABELS            
                                                                                
         L     RF,DBAREC                                                        
         CLC   ITRKKEY,0(RF)                                                    
         BNE   GIHX                                                             
                                                                                
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 DEFINE,MYDMCB,=C'TRAK',DBLOCK,WORK                               
         MVC   FNWPROG2,WORK                                                    
*&&DO                                                                           
         MVC   FNWPROG2,WORK+(RPGMNAME-RPGMFBK)                                 
*&&                                                                             
                                                                                
         OC    FNWPROG1,FNWPROG1                                                
         BNZ   *+10                                                             
         MVC   FNWPROG1,WORK                                                    
*&&DO                                                                           
         MVC   FNWPROG1,WORK+(RPGMNAME-RPGMFBK)                                 
*&&                                                                             
                                                                                
         MVC   FNWBOOK,DBACTBK                                                  
                                                                                
*                                                                               
GIHX     DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========= PUT FOOTNOTE ELEMENT INTO CREATED INVENTORY RECORD ========*         
                                                                                
PUTFTNT  DS    0H                                                               
PFN      NTR1                                                                   
         DS    0XL(1-(PFN-PUTFTNT))  NO CODE IS S/B BETWN THE LABELS            
                                                                                
*                                                                               
         LA    R2,ELEM                                                          
         USING RPGMELM,R2                                                       
         XC    RPGMELM(RPGMELML),RPGMELM                                        
         MVI   RPGMELM,X'01'                                                    
         MVI   RPGMELLN,RPGMELML                                                
         MVI   RPGMLIN,1                                                        
                                                                                
         MVC   DUB(2),FNWBOOK                                                   
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,WORK)                                     
         MVC   RPGMMON,WORK                                                     
         MVC   RPGMYR,WORK+4                                                    
                                                                                
         MVC   RPGMNAME,FNWPROG1                                                
         CLC   FNWPROG1,FNWPROG2                                                
         BE    PFN039                                                           
         MVI   RPGMNAME+7,C'/'                                                  
         MVC   RPGMNAME+8(7),FNWPROG2                                           
         MVI   RPGMNAME+15,C' '                                                 
PFN039   EQU   *                                                                
                                                                                
         GOTO1 HELLO,DMCB,(C'P',REPFILE),ADATAREC,RPGMELM,0                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
*                                                                               
PFNX     DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============ INITIALIZE DEMO HISTORY COMMUNICATIONS NODE ============*         
                                                                                
* ROUTINE SHOULD BE CALLED BEFORE A DEMAND CALL.                                
* AT EXIT,                                                                      
*   DEMO HISTORY NODE WITHIN COMMUNICATIONS LINK ADDRESSED BY ACCMNCTE          
*    IS INITIALIZED                                                             
*   ANXTDHEL = A(NEXT SLOT FOR DEMO HISTORY ELEMENT)                            
                                                                                
INITDH   DS    0H                                                               
IDH      NTR1                                                                   
         DS    0XL(1-(IDH-INITDH))  NO CODE IS S/B BETWN THE LABELS             
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R2,ACMMNCTE                                                      
         ICM   RE,15,0(R2)         GET A(COMMUNICATIONS LINK)                   
         BZ    IDH215               IF NONE, SET ONE UP                         
                                                                                
         USING NODE_D,RE                                                        
IDH212   CLC   NODE_ID,=C'HIST'    LOOK FOR DEMO HISTORY NODE                   
         BE    IDH219                                                           
         LA    R2,NODE_NXT                                                      
         ICM   RE,15,0(R2)                                                      
         BNZ   IDH212                                                           
         DROP  RE                                                               
*                                                                               
IDH215   DS    0H                  SET UP NEW LINK FOR DEMO HISTORY             
         LA    RE,NODHST                                                        
         ST    RE,0(R2)             SET ADDRESS FOR DEMO HISTORY NODE           
         USING NODE_D,RE                                                        
         MVC   NODE_ID,=C'HIST'     PUT IN IDENTIFIER                           
         XC    NODE_NXT,NODE_NXT    MAKE THIS THE LAST NODE                     
         DROP  RE                                                               
IDH219   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RE-->DEMO HISTORY NODE                       
         LA    RF,(NODHSTN-NODHST)(RE)                                          
         MVI   0(RF),0              NO DEMO HISTORY ELEMS YET                   
         LA    RF,(NODHSTEL-NODHST)(RE)                                         
         ST    RF,ANXTDHEL          (NOW IS A GOOD TIME TO SET THIS)            
         LA    R0,10                MAX 10 SLOTS                                
                                                                                
         XC    0(RIDHELLN,RF),0(RF) CLEAR SLOT                                  
         LA    RF,RIDHELLN(RF)                                                  
         BCT   R0,*-10                                                          
         MVI   0(RF),0              EOT MARKER                                  
                                                                                
*                                                                               
IDHX     DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============== BUILD DEMO HISTORY COMMUNICATIONS NODE ==============*         
                                                                                
* ROUTINE SHOULD ONLY BE CALLED FROM WITHIN A DEMAND HOOK.                      
* AT ENTRY,                                                                     
*   DBLOCK IS SET                                                               
                                                                                
BUILDDH  DS    0H                                                               
BDH      NTR1                                                                   
         DS    0XL(1-(BDH-BUILDDH)) NO CODE IS S/B BETWN THE LABELS             
                                                                                
*                                                                               
         DS    0H                                                               
         XC    DEMHST(DEMHSTL),DEMHST                                           
                                                                                
*                                                                               
** INFORMATION GATHERING **                                                     
*                                                                               
         DS    0H                                                               
         GOTO1 DEFINE,MYDMCB,=C'PURE',DBLOCK,WORK                               
         MVC   DHPURE,WORK         PURE NUMBER                                  
         CLI   DBSELMED,C'N'                                                    
         BNE   BDH020                                                           
         L     RF,ACOMFACS                                                      
         L     RF,(CHEXIN-COMFACSD)(RF)                                         
         GOTO1 (RF),MYDMCB,WORK+3,WORK,4                                        
         MVC   DHPURE,WORK                                                      
*                                                                               
BDH020   GOTO1 DEFINE,MYDMCB,=C'DAY',DBLOCK,WORK                                
         MVC   DHIDY,WORK          DAY                                          
         GOTO1 (RF),(R1),=C'TIME',,WORK                                         
         MVC   DHSETM,WORK+2       START AND END TIMES                          
*                                                                               
         GOTO1 DEFINE,MYDMCB,=C'WEEK',DBLOCK,WORK                               
         ZIC   RE,WORK             GET WEEK BITS  TO                            
         LA    RE,WEEKTAB(RE)      INDEX INTO WEEKTAB                           
         MVC   DHWEEK,0(RE)        WEEK                                         
*                                                                               
         DS    0H                  GET FROM BITS                                
         CLC   DBFILE,=C'IUN'                                                   
         BNE   BDH030                                                           
         L     RF,DBAREC                                                        
*        MVC   DHKSRC,(RINVKSRC-RINVKEY)(RF)                                    
INVKEYD  USING RINVKEY,RF                                                       
         MVC   DHSRC,INVKEYD.RINVKRSR                                           
         MVC   DHBTYP,INVKEYD.RINVKBTP                                          
         B     BDH034                                                           
         DROP  INVKEYD                                                          
                                                                                
BDH030   DS    0H                                                               
         XC    DUB,DUB                                                          
         LA    RF,DUB                                                           
         USING GKSPARMD,RF                                                      
         MVC   GKSPRSVC,DBACTSRC                                                
         CLI   DBSELMED,C'N'         NHT FILE USE NSI'S CODES                   
         BNE   *+8                                                              
         MVI   GKSPRSVC,C'N'                                                    
         MVI   GKSPQLFY,C' '                                                    
         MVC   GKSPBTYP,DBBTYPE                                                 
         DROP  RF                                                               
BDH034   EQU   *                                                                
                                                                                
         DS    0H                   REVERSE KEY SOURCE TO GET FROM BITS         
         EJECT                                                                  
*                                                                               
** BUILD ELEMENT **                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
E        USING RIDHEL,ELEM                                                      
         MVI   E.RIDHCD,RIDHCDQ     ELEMENT CODE                                
         MVI   E.RIDHLEN,RIDHELLN      "    LENGTH                              
         MVC   E.RIDHFIL,DBFILE     DEMO FILE                                   
         MVC   E.RIDHSRC,DBACTSRC   DEMO SOURCE                                 
         MVC   E.RIDHSTTN,DEMSTA    STATION                                     
         MVC   E.RIDHBK,DBACTBK     BOOK                                        
         MVC   E.RIDHBTYP,DBBTYPE   BOOK TYPE                                   
         MVC   E.RIDHFRBT,DHFRBT    FROM BOOK BITS                              
         TM    INVLTYP,X'08'        COMBO SWITCH ON?                            
         BZ    *+8                                                              
         OI    E.RIDHFLG,RIDHFTOT    YES, TURN IT ON IN ELEM ALSO               
                                                                                
         CLC   DBFILE,=C'PAV'                                                   
         BE    BDHPAV                                                           
         CLC   DBFILE,=C'TP '                                                   
         BE    BDHTP                                                            
         CLC   DBFILE,=C'IUN'                                                   
         BE    BDHIUN                                                           
         DC    H'0'                                                             
*                                                                               
BDHPAV   DS    0H                   PAV FILE                                    
         MVC   E.RIDHPURE,DHPURE     MOVE IN PURE NUMBER                        
         TM    INVLTYP,X'80'         IF REQUESTED BY PURE NUMBER,               
         BZ    BDHPAV4X                                                         
         OI    E.RIDHFLG,RIDHFPUR     FLAG IT AS SUCH                           
         TM    INVLTYP,X'04'          AND  IF WEIGHT OVERRIDE,                  
         BZ    BDHPAV4X                                                         
         MVC   E.RIDHWTOV,INVLWT       MOVE IT IN AS WELL                       
BDHPAV4X EQU   *                                                                
                                                                                
         L     RF,DBAQUART           CHECK FOR NORMAL PROGRAMMING               
         USING PHELEM,RF                                                        
         CLI   PHDTYPE,0              0=NORMAL, 1=FULL CYCLE                    
         BNE   *+8                                                              
         OI    E.RIDHFLG,RIDHFNOR     TURN FLAG ON FOR NOR PROGRAM              
         DROP  RF                                                               
                                                                                
         B     BDH199                                                           
*                                                                               
BDHTP    DS    0H                   TP  FILE                                    
         MVC   E.RIDHIDY,DHIDY       MOVE IN DAYS,                              
         MVC   E.RIDHSETM,DHSETM      TIMES,                                    
         MVC   E.RIDHWK,DBSELWKN      AND WEEK NUMBER                           
                                                                                
         OC    INVPRG#,INVPRG#      IF PROGRAM# REQUESTED,                      
         BZ    *+10                                                             
         MVC   E.RIDHWK,DHWEEK      THEN OVERRIDE WEEK #                        
                                                                                
         CLI   DBTPTT,C'P'           IF 4-WK AVERAGE,                           
         BNE   *+8                                                              
         OI    E.RIDHFLG,RIDHFTP4     TURN FLAG ON                              
                                                                                
         B     BDH199                                                           
*                                                                               
BDHIUN   DS    0H                   IUN FILE                                    
         L     R6,DBAREC                                                        
         USING RINVKEY,R6                                                       
         MVC   E.RIDHSRC,DHSRC       SOURCE                                     
         MVC   E.RIDHSTTN,RINVKSTA   STATION                                    
         MVC   E.RIDHBK,RINVKBK      BOOK                                       
         MVC   E.RIDHBTYP,DHBTYP     BOOK TYPE                                  
         MVC   E.RIDHINVN,RINVKINV   MOVE IN INVENTORY NUMBER                   
         GOTO1 DATCON,MYDMCB,(3,RINVKSTD),(2,DHEFFD),0                          
         MVC   E.RIDHEFFD,DHEFFD      AND EFFECTIVE DATE                        
         DROP  R6                                                               
                                                                                
         CLI   INVLWT,1              CHECK FOR WEIGHT OVERRIDE                  
         BE    *+18                                                             
         TM    INVLTYP,X'80'          (WEIGHT ONLY FOR INV NUM)                 
         BZ    *+10                                                             
         MVC   E.RIDHWTOV,INVLWT                                                
                                                                                
         B     BDH199                                                           
*                                                                               
BDH199   EQU   *                                                                
         EJECT                                                                  
*                                                                               
** PUT DEMO HISTORY INTO COMMUNICATIONS LINK **                                 
*                                                                               
BDH200   DS    0H                                                               
         ICM   R4,15,ACMMNCTE                                                   
BDH205   BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NODE_D,R4                                                        
         CLC   NODE_ID,=C'HIST'                                                 
         BE    *+12                                                             
         ICM   RE,15,NODE_NXT                                                   
         B     BDH205                                                           
*                                                                               
         DS    0H                  R4-->DEMO HISTORY NODE                       
         CLI   (NODHSTN-NODHST)(R4),10                                          
         BNL   BDHXN                NO MORE ROOM, EXIT (W/ ERROR)               
*                                                                               
         L     R3,ANXTDHEL         R3-->SLOT TO PUT DEMO HISTORY ELEM           
         CLC   E.RIDHFIL,=C'TP '   SPECIAL PROCESSING FOR TIME PERIOD           
         BNE   BDH224X                                                          
         CLI   0(R3),0                IF ALREADY AT A NEW SLOT,                 
         BE    BDH224X                 PUT DEMO HISTORY ELEM IN                 
         USING RIDHEL,R3                                                        
         CLC   RIDHFIL(14),E.RIDHFIL  SAME FIL/SRC/STTN/BK/BTYP/WK/DAY?         
         BNE   *+14                                                             
         MVC   RIDHETM,E.RIDHETM       YES, UPDATE TO A LATER END TIME          
         B     BDH249                                                           
         LA    R3,RIDHELLN(R3)         NO, BUMP TO AVAILABLE SLOT               
         DROP  R3                                                               
BDH224X  EQU   *                                                                
                                                                                
         DS    0H                  R3-->NEXT AVAILABLE SLOT                     
         MVC   0(RIDHELLN,R3),E.RIDHEL                                          
         ZIC   R1,NODHSTN                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NODHSTN                                                       
         CLC   E.RIDHFIL,=C'TP '    TIME PERIOD HAS ITS OWN                     
         BE    *+8                   BUMPING TO NEXT AVAILABLE SLOT             
         LA    R3,RIDHELLN(R3)                                                  
         ST    R3,ANXTDHEL                                                      
         DROP  R4                                                               
*                                                                               
BDH249   EQU   *                                                                
         B     BDHXY                                                            
         DROP  E                                                                
                                                                                
*                                                                               
BDHXY    DS    0H                  RETURN CC AS EQUAL                           
         SR    RC,RC                                                            
BDHXN    DS    0H                  RETURN CC AS NOT EQUAL                       
         LTR   RC,RC                                                            
         B     EXXMOD                                                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= CONVERT TO IUN FORMAT =======================*         
                                                                                
* ROUTINE IS CALLED FROM WITHIN A DEMAND HOOK AND CONVERTS A DEMO               
*  RECORD INTO AN IUN FORMAT.                                                   
*                                                                               
* AT ENTRY,                                                                     
*   DBLOCK   = DBLOCK OF DEMAND CALL                                            
* AT EXIT,                                                                      
*   AIUNREC  = DEMO RECORD IN IUN FORMAT                                        
*                                                                               
* !!WARNING!!  MAKE SURE  FULL  IS NOT CLOBBERED WHILE EXECUTING                
*               THIS ROUTINE                                                    
                                                                                
                                                                                
CNVIUN   NTR1  WORK=(R2,DBLOCK_L)                                               
         L     R3,AIUNREC                                                       
                                                                                
*                                                                               
** INITIALIZATION **                                                            
*                                                                               
         DS    0H                                                               
         LR    R0,R2                                                            
         LHI   R1,(((DBLOCK_L+7)/8)*8)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               INITIALIZE WORK AREA                         
*                                                                               
         DS    0H                                                               
         LR    R0,R3                                                            
         LHI   R1,2016                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               INITIALIZE OUTPUT AREA                       
*                                                                               
         DS    0H                                                               
         USING RINVREC,R3                                                       
         MVI   RINVREC,RINVKTYQ                                                 
         MVC   RINVLEN,=Y(RINVPEL-RINVREC)                                      
         DROP  R3                                                               
                                                                                
*                                                                               
** SEED DEMO VALUES INTO IUN BUCKETS **                                         
*                                                                               
         DS    0H                                                               
         LA    R4,700(R3)                                                       
         USING IUNREC,R4                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 REGETIUN,DMCB,(10,DBLOCK),(R4)                                   
*                                                                               
         DS    0H                                                               
         CLI   IUNSW,YES                                                        
         BE    CIUN059                                                          
                                                                                
         LA    R0,NEWRTG                                                        
         LHI   R1,(4*LENVALS)                                                   
         LA    RE,OLDRTG                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY "OLD" VALUES TO "NEW"                   
CIUN059  EQU   *                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 CDEMOUT,DMCB,(C'L',SHARES),DBLOCK,HOMESHR,0                      
                                                                                
*                                                                               
         DS    0H                                                               
         OC    UUV2149,UUV2149                                                  
         BZ    *+12                                                             
         OI    U191,X'40'          THERE WAS DEMO FOR U191 (UV21-49)            
         B     *+8                                                              
         OI    U191,X'80'          NO DEMO FOR U191                             
                                                                                
*                                                                               
** CONDENSE IUN BUCKETS **                                                      
*                                                                               
         DS    0H                                                               
         MVC   (DBFILE-DBLOCK)(,R2),=C'INV'                                     
         MVI   (DBACTMED-DBLOCK)(R2),C'U'                                       
         ST    R7,(DBCOMFCS-DBLOCK)(R2)                                         
         MVC   (DBSELAGY-DBLOCK)(,R2),AGENCY                                    
         MVC   (DBACTBK-DBLOCK)(,R2),DBACTBK                                    
         MVC   (DBFACTOR-DBLOCK)(,R2),DBFACTOR                                  
         MVC   (DBDIVSOR-DBLOCK)(,R2),DBDIVSOR                                  
         MVC   (DBTAPEP-DBLOCK)(,R2),DBTAPEP                                    
         ST    R3,(DBAREC-DBLOCK)(R2)                                           
         LA    R0,(RINVPEL-RINVREC)(R3)                                         
         ST    R0,(DBAQUART-DBLOCK)(R2)                                         
         LHI   R0,((UPRECX-UPREC)/4)                                            
         STCM  R0,3,(DBNUMVLS-DBLOCK)(R2)                                       
         MVC   WORK+0(7),=C'INVUIUN'                                            
         MVC   WORK+7(2),=X'5A0B'                                               
         CLI   DBTAPEP,C'Y'                                                     
         BE    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         MVI   WORK+9,0                                                         
*                                                                               
         DS    0H                                                               
         GOTO1 CDEMAINT,DMCB,=C'PUT',(R2),IUNREC,WORK                           
         DROP  R4                                                               
                                                                                
*                                                                               
** EXIT ROUTINE **                                                              
*                                                                               
CIUNX    DS    0H                                                               
         B     EXXMOD                                                           
***********************************************************************         
         EJECT                                                                  
EXXMOD   XMOD1 1                                                                
*** INCLUDE RESVCTAB   <<--- NOT USED                                           
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
BLANKS   DC    132X'40'                                                         
*                                                                               
WEEKTAB  DC    AL1(0,4,3,3,2,2,2,2,1,1,1,1,1,1,1,1)                             
*                                                                               
**IUNWORK  DS    2016C        <-- CANNOT HAVE SV STRG IN CORRES PRG             
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
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
         ORG   SYSSPARE                                                         
*       LOCAL WORKING STORAGE                                                   
*     FIELDS ABOVE DOUBLE ASTERISK LINES ARE COMMON TO RERMP(04,11,12           
*                                                            & 30)              
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFF   DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL2                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL20                TO BOOK CODES (WAS 15 INC FOR BKTYP)         
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL64                BOOK ENTRIES BLT BY REBKLST (WAS 60)         
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*                                                                               
TAPEOPT  DS    CL1                 Y ==> TAPE PRECISION FOR DEMO CALCS          
RIDBLK   DS    CL8                 DEMUP 4TH PARAMETER                          
*                                                                               
ACMMNCTE DS    A                   A(LINK TO "TALK" TO OTHER PHASES)            
*                                                                               
TIMECHG  DS    CL1                 TIME CHANGE (S=SPRING F=FALL)                
WGTWEEK  DS    CL1                 INCLUDE WEEKS IN WEIGHT FACTOR               
*                                                                               
INVPRG#  DS    XL3                 PROGRAM# FOR SPECIFIC LOOKUP                 
         DS    XL28                (SPARE)                                      
         SPACE 1                                                                
*********************************************************************           
*********************************************************************           
PROGBNUM DS    XL3                 BINARY PROG#, CONV FROM PROGNUM              
PROGNUM  DS    CL8                 EBCDIC PROG# RETURNED BY DEFINE              
*                                                                               
         EJECT                                                                  
* DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                  
*                                                                               
DEMOD    DSECT                                                                  
MYDMCB   DS    6F                                                               
DMDMCB   DS    6F                  DEMANDS DMCB (DON'T TOUCH IN HOOK)           
DIVISOR  DS    F                   DIVISOR BUCKET                               
ADATAREC DS    A                   A(DATA RECORD)                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
ANXTDHEL DS    A                   A(NEXT SLOT FOR DEMO HIST ELEM)              
AIUNREC  DS    A                   A(IUN WORK RECD)                             
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
*                                                                               
ITRKKEY  DS    XL(L'RINVKEY)       AREA TO HOLD KEY OF INV TRACK RECD           
*                                                                               
FTNTWORK DS    0X                  WORK AREA FOR FOOTNOTES                      
FNWBOOK  DS     XL2                 BOOK                                        
FNWPROG1 DS     CL16                NAME OF FIRST PROGRAM                       
FNWPROG2 DS     CL16                NAME OF LAST  PROGRAM                       
FTNTWRKL EQU   *-FTNTWORK                                                       
*                                                                               
IBLK     DS    CL5                 INPUT BLOCK FOR GETKSRC                      
OBLK     DS    CL5                 OUTPUT BLOCK FOR KSRC                        
*                                                                               
RESETDB  DS    C                   RESET DBLOCK FIELDS FLAG                     
SVSRC    DS    C                   FUDGE SOURCE IN DBLOCK BEFOR MATH            
*                                                                               
         DS    0F                                                               
       ++INCLUDE DEDBLOCK                                                       
DBLOCK_L EQU   *-DBLOCK                                                         
                                                                                
MYDBXTND DS    XL256               DBLOCK EXTENSION                             
*                                                                               
SVDBLOCK DS    XL(DBLOCK_L)        SAVE AREA FOR DBLOCK                         
*                                                                               
PROGEL   DS    CL60                PROGRAM NAME ELEMENT                         
*                                                                               
DEMHST   DS    0X                  DEMO HISTORY VALUES                          
DHSRC    DS     CL1                 SOURCE                                      
DHIDY    DS     XL1                 INTERNAL DAY                                
DHSETM   DS     0XL4                MILITARY TIMES                              
DHSTM    DS      XL2                 START TIME                                 
DHETM    DS      XL2                 END   TIME                                 
DHPURE   DS     XL2                 PURE NUMBER                                 
DHKSRC   DS     XL1                 KEY SOURCE                                  
DHFRBT   DS     XL1                 BOOKVAL FROM BITS                           
DHBTYP   DS     CL1                 BOOK TYPE                                   
DHEFFD   DS     XL2                 (INVENTORY) EFFECTIVE START DATE            
DHWEEK   DS     XL1                 WEEK BITS                                   
DEMHSTX  EQU   *                                                                
DEMHSTL  EQU   DEMHSTX-DEMHST                                                   
*                                                                               
NODHST   DS    0X                  NODE FOR DEMO HISTORY ELEMENTS               
         DS     XL(NODE_DTA-NODE_D) C'HIST'                                     
NODHSTN  DS     XL1                 NUMBER OF DEMO HISTORY ELEMENTS             
         DS     XL3                 (SPARE)                                     
NODHSTEL DS     10XL(RIDHELLN)      DEMO HISTORY ELEMENTS (MAX 10)              
         DS     XL1                 EOLIST MARKER (X'00')                       
NODHSTL  EQU   *-NODHST                                                         
         SPACE 2                                                                
         EJECT                                                                  
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
INVLNUMB DS    CL4                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         SPACE 2                                                                
*                                                                               
** DSECT TO COVER A NODE IN THE COMMUNICATIONS AREA **                          
*                                                                               
NODE_D   DSECT                                                                  
NODE_ID  DS    CL4                 NODE IDENTIFIER                              
NODE_NXT DS    XL4                 A(NEXT NODE), NULLS==>NO MORE NODES          
NODE_DTA DS    0X                  DATA                                         
                                                                                
                                                                                
*-------------------------- REGETKSRC PARMS --------------------------*         
                                                                                
GKSPARMD DSECT                                                                  
GKSPRSVC DS    CL1                 RATING SERVICE                               
GKSPQLFY DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GKSPKSRC DS    XL1                 RINVKSRC FOR KEY                             
GKSPBKBT DS    XL1                 BOOKVAL BITS                                 
GKSPBTYP DS    CL1                 BOOK TYPE                                    
GKSPARMX EQU   *                                                                
GKSPARML EQU   GKSPARMX-GKSPARMD                                                
         EJECT                                                                  
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
         ORG   OLDUNV+(DISP2149*4)                                              
UUV2149  DS    F                                                      *         
         ORG                                                                    
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
DISP2149 EQU   X'1C'               DISPL TO V2149 CATEGORY                      
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
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* REGENAVL                                                                      
         PRINT OFF                                                              
RAVLD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* RERMPPROF                                                                     
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
                                                                                
                                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
                                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133RERMP30   08/12/09'                                      
         END                                                                    
