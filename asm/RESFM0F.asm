*          DATA SET RESFM0F    AT LEVEL 091 AS OF 05/01/02                      
*PHASE T8180FA,*                                                                
         TITLE 'T8180F-RESFM0F-ADETAIL AND PDETAIL ADD/CHA/DEL/RESTORE'         
*******************************************************************             
*                                                                 *             
*        RESFM0F --- ADETAIL AND PDETAIL ADD/CHANGE/DELETE/RESTORE*             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* APR12/89 (MRR) --- FIX TO ALLOW REMOVAL OF BOOK OVERRIDE        *             
*                                                                 *             
* APR19/89 (MRR) --- RE-LINK WITH LARGER SCREEN FIELD WHICH ALLOWS*             
*                     THE USER TO USE SATELLITE INDICATOR         *             
*                                                                 *             
* MAY17/89 (MRR) --- CHANGE BOOK CPP/CPM INDICATOR FROM '$' TO '='*             
*                    LIMIT DEMOS TO 7                             *             
*                                                                 *             
* 08/30/89  PJS  --- LIMIT RATE INPUT TO 6 CHARACTERS (999999).   *             
*                                                                 *             
* FEB27/90 (MRR) --- DISALLOW MULTIPLE SERVICES ON BOOK LINE      *             
*                                                                 *             
* 07MAY90  (EFJ) --- REMOVED 'SOURCE' FIELD                       *             
*                                                                 *             
* 28NOV94  (SKU) --- FIX BUG OF NOT CLEARING R0 BEFORE LOOPING    *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8180F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**180F**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
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
*====================================================================*          
*    VALIDATE CONTRACT  (REQUIRED)                                   *          
*====================================================================*          
         SPACE 1                                                                
         LA    R2,DETCONH                                                       
         GOTO1 VALICON,DMCB,(R2)                                                
         LA    R2,DETSTNH          POINT TO CALL LETTERS FIELD                  
         GOTO1 DISPCON,DMCB,(R2)                                                
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TYPE (REQUIRED)                                        *          
*       A=AVAIL,  P=PROPOSAL                                         *          
*====================================================================*          
         SPACE 1                                                                
         MVC   ETYPE,CONREC        WILL BE EITHER 'A' OR 'P'                    
         CLI   ETYPE,C'A'                                                       
         BE    *+14                                                             
         CLI   ETYPE,C'P'                                                       
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED TO THE RECORD FIELD?           
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE SOURCE  (REQUIRED)                                     *          
*        I=INVENTORY,  S=SID                                         *          
*====================================================================*          
         SPACE 1                                                                
*         LA    R2,DETSRCH                                                      
*         GOTO1 VALISRC                                                         
         MVI   ESOURCE,C'I'                                                     
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE HEADER NUMBER  (REQUIRED)                              *          
*====================================================================*          
         SPACE 1                                                                
         MVC   RERROR,=AL2(MISSING)                                             
         LA    R2,DETHDRH                                                       
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,XHDRNUM                                                       
         MVC   EHDRNUM,8(R2)                                                    
         EJECT                                                                  
*====================================================================*          
*    VALIDATE LINE NUMBER                                            *          
*====================================================================*          
         SPACE 1                                                                
         LA    R2,DETLINH                                                       
         MVI   XDETNUM,0                                                        
         XC    EDETNUM,EDETNUM                                                  
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BNE   VK80                                                             
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   ACTNUM,ACTADD       USER SUPPLIES NUMER ON NON-ADD               
         BNE   ERREND                                                           
         B     VK90                                                             
         SPACE 1                                                                
VK80     CLI   ACTNUM,ACTADD       SYSTEM SUPPLIES NUMBER ON ADD                
         BE    VK90                (IGNORE WHAT'S THERE)                        
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,XDETNUM                                                       
         MVC   EDETNUM,8(R2)                                                    
         SPACE 1                                                                
VK90     XC    KEY,KEY             GET HEADER, THEN BUILD DETAIL KEY            
         LA    R6,KEY                                                           
         MVC   AIO,AIO3            READ HEADER RECORD IN IO3                    
         MVC   RERROR,=AL2(NOTFOUND)                                            
         CLI   ETYPE,C'A'                                                       
         BNE   VK130                                                            
         SPACE 1                                                                
         USING RAVLREC,R6                                                       
         MVI   RAVLKTYP,X'14'      BUILD AVAIL HEADER KEY                       
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R2,DETHDRH          HEADER RECORD NOT FOUND                      
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         BAS   RE,HDRINFO          FILL IN PROTECTED FIELDS ON SCREEN           
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         LA    R6,KEY              NOW BUILD DETAIL KEY                         
         LA    R2,DETLINH                                                       
         MVC   RERROR,=AL2(NOTFOUND)                                            
         GOTO1 HIGH                REREAD HEADER RECORD                         
         SPACE 1                                                                
VK100    CLI   ACTNUM,ACTADD       FOR ADD                                      
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST      AND RESTORE                                  
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         CLC   KEY(26),KEYSAVE     SAME AVAIL                                   
         BNE   VK120               NO                                           
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK110                                                            
         CLC   XDETNUM,KEY+26                                                   
         BH    VK100                                                            
         MVC   XDETNUM,KEY+26      IF ADD, SAVE HIGHEST NUMBER                  
         B     VK100                                                            
         SPACE 1                                                                
VK110    CLC   XDETNUM,KEY+26      IS IT THE ONE I WANT                         
         BNE   VK100               NO, GET NEXT                                 
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VK115                                                            
         BAS   RE,DELPRAV          DELETE THE DETAIL RECORD                     
         LA    R2,DETLINH                                                       
         MVC   RERROR,=H'7'        RECORD HAS BEEN DELETED                      
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
VK115    CLI   ACTNUM,ACTREST                                                   
         BNE   XIT                                                              
         BAS   RE,RESTPRAV         RESTORE THE DETAIL RECORD                    
         LA    R2,DETLINH                                                       
         MVC   RERROR,=H'8'        RECORD HAS BEEN RESTORED                     
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
VK120    CLI   ACTNUM,ACTADD                                                    
         BNE   ERREND                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(26),KEYSAVE                                                  
         ZIC   R1,XDETNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,XDETNUM          SAVE FOR OUTPUT TO SCREEN                    
         STC   R1,RAVLKDET                                                      
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 3                                                                
         USING RPRPREC,R6                                                       
VK130    MVI   RPRPKTYP,X'16'                                                   
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R2,DETHDRH          HEADER RECORD NOT FOUND                      
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         BAS   RE,HDRINFO          FILL IN PROTECTED FIELDS ON SCREEN           
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         LA    R6,KEY              NOW BUILD DETAIL KEY                         
         LA    R2,DETLINH                                                       
         MVC   RERROR,=AL2(NOTFOUND)                                            
         GOTO1 HIGH                REREAD HEADER RECORD                         
         SPACE 1                                                                
VK140    CLI   ACTNUM,ACTADD       FOR ADD                                      
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST      AND RESTORE                                  
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         CLC   KEY(23),KEYSAVE     SAME PROPOSAL                                
         BNE   VK170                                                            
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK150                                                            
         CLC   XDETNUM,KEY+26                                                   
         BH    VK140                                                            
         MVC   XDETNUM,KEY+26      IF ADD, SAVE HIGHEST NUMBER                  
         B     VK140                                                            
         SPACE 1                                                                
VK150    CLC   XDETNUM,KEY+26      IS IT THE ONE I WANT                         
         BNE   VK140               NO, GET NEXT                                 
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VK160                                                            
         BAS   RE,DELPRAV          DELETE THE DETAIL RECORD                     
         LA    R2,DETLINH                                                       
         MVC   RERROR,=H'7'        RECORD HAS BEEN DELETED                      
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
VK160    CLI   ACTNUM,ACTREST                                                   
         BNE   XIT                                                              
         BAS   RE,RESTPRAV         RESTORE THE DETAIL RECORD                    
         LA    R2,DETLINH                                                       
         MVC   RERROR,=H'8'        RECORD HAS BEEN RESTORED                     
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
VK170    CLI   ACTNUM,ACTADD                                                    
         BNE   ERREND                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(23),KEYSAVE                                                  
         MVC   RPRPKPLN,=X'FFFF'   START OUT AS NON-PACKAGE                     
         ZIC   R1,XDETNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,XDETNUM          SAVE FOR OUTPUT TO SCREEN                    
         STC   R1,RPRPKDET                                                      
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VREC     DS    0H                                                               
         SPACE 1                                                                
*  DELETE MOST OLD ELEMENTS ON THE DETAIL RECORD                                
*  (DON'T DELETE 06 - OVERRIDE ELEMENTS HERE - IF CHANGED, THEY'LL              
*  BE DELETED AND RE-ADDED IN BLDOVRD)                                          
         SPACE 1                                                                
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD DETAIL ELEMENT                    
         MVI   ELCODE,5                                                         
         GOTO1 REMELEM             AND OLD UPGRADE ELEMENT(S)                   
         MVI   ELCODE,8                                                         
         GOTO1 REMELEM             AND OLD TEXT ELEMENT(S)                      
         MVI   ELCODE,X'0B'                                                     
         GOTO1 REMELEM             AND OLD BOOK LABEL ELEMENT                   
         MVI   ELCODE,X'0D'                                                     
         GOTO1 REMELEM             AND OLD SID ELEMENT                          
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE INVENTORY NUMBER (REQUIRED FOR SOURCE I)               *          
*                              (NOT ALLOWED FOR SOURCE S)            *          
*====================================================================*          
         SPACE 1                                                                
         LA    R2,DETINVH                                                       
         CLI   ESOURCE,C'I'                                                     
         BE    VR10                                                             
         DC    H'0'                MUST BE C'I'                                 
*         MVC   RERROR,=AL2(INVALID)                                            
*         CLI   5(R2),0                                                         
*         BE    VR50                                                            
*         BNE   ERREND                                                          
         SPACE 1                                                                
VR10     GOTO1 ANY                                                              
         XC    CBLOCK,CBLOCK                                                    
         GOTO1 VALIINV                                                          
         MVC   DINV,CBLOCK         SAVE INVENTORY DATA                          
         MVC   DATE,CBLOCK+3                                                    
         MVC   DSAT,CBLOCK+6                                                    
         SPACE 1                                                                
         CLC   CBLOCK(3),=C'MAN'   IF 'MANUAL'                                  
         BE    VR20                                                             
         LA    R2,DETDAYH                                                       
         CLI   5(R2),0                                                          
         BNE   VR15A                                                            
         OI    4(R2),X'20'                                                      
VR15A    EQU   *                                                                
         LA    R2,DETTTLH                                                       
         CLI   5(R2),0                                                          
         BNE   VR15B                                                            
         OI    4(R2),X'20'                                                      
VR15B    EQU   *                                                                
         LA    R2,DETTIMH                                                       
         CLI   5(R2),0                                                          
         BNE   VR15C                                                            
         OI    4(R2),X'20'                                                      
VR15C    EQU   *                                                                
         B     VR50                                                             
         SPACE 1                                                                
VR20     EQU   *                                                                
         LA    R2,DETDAYH          MUST INPUT DAY                               
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         NI    4(R2),X'DF'         (TURN OFF PREVIOUSLY VALIDATED)              
         LA    R2,DETTTLH          AND TITLE                                    
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         NI    4(R2),X'DF'                                                      
         LA    R2,DETTIMH          AND TIME                                     
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         NI    4(R2),X'DF'                                                      
         SPACE 1                                                                
         LA    R2,DETPJ1H          AND CAN'T INPUT UPGRADES                     
         CLI   5(R2),0                                                          
         BNE   VR40                                                             
         CLI   ETYPE,C'P'          PJ2 PROTECTED ON PROPOSALS                   
         BE    *+16                                                             
         LA    R2,DETPJ2H                                                       
         CLI   5(R2),0                                                          
         BNE   VR40                                                             
         SPACE 1                                                                
         OC    CBOOKS,CBOOKS       AND MUST HAVE BOOKS IN HEADER                
         BNZ   VR50                                                             
         LA    R2,DETINVH                                                       
         MVC   RERROR,=AL2(MANINV3)                                             
         B     ERREND                                                           
         SPACE 1                                                                
VR30     MVC   RERROR,=AL2(MANINV)                                              
         B     ERREND                                                           
         SPACE 1                                                                
VR40     MVC   RERROR,=AL2(MANINV2)                                             
         B     ERREND                                                           
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE LENGTHS - OPTIONAL                                     *          
*     ANY INPUT HERE OVERRIDES THE HEADER                            *          
*        IF AVAILS, UP TO 6 LENGTHS ARE ALLOWED AND                  *          
*                   EACH CAN HAVE A CLASS NUMBER                     *          
*        IF PROPOSALS, ONLY 1 LENGTH ALLOWED, AND NO CLASS           *          
*====================================================================*          
         SPACE 1                                                                
VR50     XC    DFRM,DFRM                                                        
         MVI   DSEC,0                                                           
         MVI   NUMLENS,0                                                        
         LA    R2,DETLENH                                                       
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   ETYPE,C'A'                                                       
         BE    VR60                                                             
         SPACE 1                                                                
         GOTO1 VPACK               ONLY ONE LENGTH FOR PROPOSALS                
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,DSEC                                                          
         B     VR80                                                             
         SPACE 1                                                                
VR60     XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK),C',=,.'                              
         MVC   NUMLENS,DMCB+4      OVERRIDE NUMBER OF LENGTHS IN HEADER         
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    ERREND                                                           
         MVC   RERROR,=AL2(MANYLEN)                                             
         CH    R3,=H'6'            MAXIMUM OF 6 LENGTHS FOR AVAILS              
         BH    ERREND                                                           
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         LA    RF,BLOCK            A(SCANNER BLOCK)                             
         LA    R1,DFRM             BUILD FIELD HERE                             
         SPACE 1                                                                
VR70     TM    2(RF),X'80'         EDIT LENGTH                                  
         BNO   ERREND              NOT NUMERIC                                  
         ICM   RE,15,4(RF)                                                      
         BZ    ERREND              SPOT LENGTH ZERO                             
         CH    RE,=H'240'                                                       
         BH    ERREND              SPOT LENGTH TOO HIGH                         
         STC   RE,1(R1)            SAVE SPOT LENGTH                             
         SPACE 1                                                                
         CLI   1(RF),0             ANY CLASS GIVEN?                             
         BE    VR75                NO                                           
         TM    3(RF),X'80'                                                      
         BNO   ERREND              NOT NUMERIC                                  
         ICM   RE,15,8(RF)                                                      
         BZ    ERREND              CLASS ZERO                                   
         CH    RE,=H'9'                                                         
         BH    ERREND              CLASS > 9 NOT ALLOWED                        
         STC   RE,0(R1)            SAVE CLASS NUMBER                            
         SPACE 1                                                                
VR75     LA    R1,2(R1)            NEXT SLOT IN ELEMENT                         
         LA    RF,32(RF)           NEXT ENTRY IN SCANNER BLOCK                  
         BCT   R3,VR70                                                          
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE RATES - OPTIONAL                                       *          
*====================================================================*          
         SPACE 1                                                                
VR80     XC    DRTE,DRTE                                                        
         LA    R2,DETRATH                                                       
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         SPACE 1                                                                
         CLI   ETYPE,C'A'                                                       
         BE    VR85                                                             
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK               ONLY ONE RATE FOR PROPOSALS                  
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STCM  R0,15,DRTE                                                       
*                                                                               
*- CHECK FOR MAX RATE VALUE (999,999)                                           
         CLC   =F'999999',DRTE                                                  
         BL    ERREND                                                           
*                                                                               
         B     VR100                                                            
         SPACE 1                                                                
VR85     XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK),0                                    
         MVC   RERROR,=AL2(MANYRATE)                                            
         CLI   NUMLENS,0           DID THEY OVERRIDE THE LENGTHS?               
         BNE   *+10                YES                                          
         MVC   NUMLENS,CNUMLENS                                                 
         CLC   NUMLENS,DMCB+4      ARE THERE MORE RATES THAN LENGTHS?           
         BL    ERREND              YES                                          
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    ERREND                                                           
         SPACE 1                                                                
         LA    RF,BLOCK            A(SCANNER BLOCK)                             
         LA    R1,DRTE             BUILD RATE FIELD HERE                        
         SPACE 1                                                                
VR90     CLI   1(RF),0             NO SECOND HALF ALLOWED                       
         BNE   ERREND                                                           
         CLI   0(RF),0             ANY RATE GIVEN FOR THIS LENGTH?              
         BE    *+18                NO                                           
         TM    2(RF),X'80'         YES -- IS IT NUMERIC?                        
         BNO   ERREND              NO                                           
*                                                                               
*- CHECK FOR MAX RATE VALUE (999,999)                                           
         CLC   =F'999999',4(RF)                                                 
         BL    ERREND                                                           
*                                                                               
         MVC   0(4,R1),4(RF)       YES -- PUT RATE IN FIELD                     
         SPACE 1                                                                
         LA    R1,4(R1)                                                         
         LA    RF,32(RF)           BUMP TO NEXT ENTRY                           
         BCT   R3,VR90                                                          
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE # FIELD - REQUIRED -- ONLY APPEARS ON PROPOSALS        *          
*   VALID INPUT IS #W, FOR NUMBER OF TIMES PER WEEK                  *          
*                  #X, FOR NUMBER OF TIMES PER PROGRAM               *          
*                  #T, FOR NUMBER OF TIMES IN TOTAL FOR THE PACKAGE  *          
*                      (IF #T, THEN CODE FIELD IS REQUIRED)          *          
*====================================================================*          
         SPACE 1                                                                
VR100    MVI   DNC,0                                                            
         MVI   DNUM,0                                                           
         CLI   ETYPE,C'P'          TEST PROPOSALS                               
         BNE   VR170               NO                                           
         LA    R2,DETNUMH                                                       
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),1                                                          
         BE    ERREND                                                           
         SPACE 1                                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         LA    RF,8(RE,R2)                                                      
         MVC   DNC,0(RF)           LAST CHAR IN FIELD MUST BE W,X OR T          
         SPACE 1                                                                
         XC    CBLOCK,CBLOCK                                                    
         MVC   CBLOCK(8),0(R2)     SET UP PHONEY FIELD TO GET RID OF IT         
         STC   RE,CBLOCK+5         ADJUST LENGTH                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+8(0),8(R2)                                                
         LA    R2,CBLOCK                                                        
         SPACE 1                                                                
         LA    RF,8(R2)                                                         
         LA    R3,1(RE)            GET LENGTH OF FIELD IN R3                    
VR110    CLI   0(RF),C'0'          AND VALIDATE FOR NUMERIC                     
         BL    VR130                                                            
         CLI   0(RF),C'9'                                                       
         BH    VR130                                                            
         LA    RF,1(RF)                                                         
         BCT   R3,VR110                                                         
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         ZAP   DUB,=P'0'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         LA    R2,DETNUMH                                                       
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         SPACE 1                                                                
         CLI   DNC,C'W'            NUMBER PER WEEK                              
         BE    VR120                                                            
         CLI   DNC,C'X'            NUMBER PER PROGRAM                           
         BE    VR120                                                            
         CLI   DNC,C'T'            TOTAL NUMBER FOR PACKAGE                     
         BNE   VR130                                                            
         STC   R0,PSPT                                                          
         B     VR140                                                            
VR120    STC   R0,DNUM             SAVE NUMBER                                  
         B     VR140                                                            
VR130    MVC   RERROR,=AL2(BADNUM)                                              
         LA    R2,DETNUMH                                                       
         B     ERREND                                                           
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE CODE FIELD - REQUIRED IF # FIELD WAS #T,               *          
*                    ELSE OPTIONAL                                   *          
*          INPUT TO THIS FIELD SETS UP THIS LINE AS A PACKAGE        *          
*====================================================================*          
         SPACE 1                                                                
VR140    L     R6,AIO1                                                          
         LA    R2,DETCDH                                                        
         CLI   DNC,C'T'                                                         
         BE    VR150                                                            
         CLI   5(R2),0                                                          
         BNE   VR160                                                            
         MVC   RPRPKPLN,=X'FFFF'                                                
         B     VR170                                                            
VR150    MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
VR160    MVC   RPRPKPLN+1(1),DNC                                                
         MVC   RPRPKPLN(1),8(R2)                                                
         DROP  R6                                                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE DAY                                                    *          
*====================================================================*          
         SPACE 1                                                                
VR170    LA    R2,DETDAYH                                                       
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    VR180                                                            
         CLC   DETDAY,HYPHENS                                                   
         BE    VR180               NO OVERRIDE                                  
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),WORK,WORK+10                            
         CLI   WORK,0                                                           
         BE    ERREND                                                           
         MVI   BYTE,X'01'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
         LA    R2,DETDAYOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TITLE                                                  *          
*====================================================================*          
         SPACE 1                                                                
VR180    LA    R2,DETTTLH                                                       
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    VR190                                                            
         CLC   DETTTL,HYPHENS                                                   
         BE    VR190               NO OVERRIDE                                  
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   BYTE,X'03'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
         LA    R2,DETTTLOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TIME                                                   *          
*====================================================================*          
         SPACE 1                                                                
VR190    LA    R2,DETTIMH                                                       
         TM    4(R2),X'20'         FIELD VALIDATED PREVIOUSLY                   
         BO    VR200                                                            
         CLC   DETTIM,HYPHENS                                                   
         BE    VR200               NO OVERRIDE                                  
         MVC   RERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         ZIC   R5,5(R2)                                                         
         LA    R3,6(R5,R2)                                                      
         CLC   0(2,R3),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),WORK                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         MVI   BYTE,X'02'                                                       
         BAS   RE,BLDOVRD                                                       
         SPACE 1                                                                
         LA    R2,DETTIMOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE BOOKS - OPTIONAL                                       *          
*                INPUT TO THIS FIELD OVERRIDES THE HEADER            *          
*          MAX 6 BOOKS FOR AVAILS                                    *          
*          MAX 1 BOOK FOR PROPOSALS                                  *          
*====================================================================*          
         SPACE 1                                                                
VR200    MVC   CBKLABEL,SPACES                                                  
         XC    DBKS,DBKS                                                        
         XC    DBKF,DBKF                                                        
         LA    R2,DETBKSH                                                       
         CLI   5(R2),0                                                          
         BNE   VR205                                                            
         MVI   BYTE,X'07'          REMOVE BOOKOVERRIDE                          
         BAS   RE,BLDOVRD                                                       
         B     VR280                                                            
         SPACE 1                                                                
VR205    EQU   *                                                                
         OC    CBOOKS,CBOOKS       CAN'T HAVE DETAIL BOOKS                      
         BNZ   *+14                IF THERE WERE NO HEADER BOOKS                
         MVC   RERROR,=AL2(NOHDRBK)                                             
         B     ERREND                                                           
         SPACE 1                                                                
         CLI   ETYPE,C'A'          PROPOSALS CAN ONLY HAVE 1 BOOK. . .          
         BE    VR210               . . . SO JUST VALIDATE AS IS                 
         MVI   MAX,10                                                           
         GOTO1 VALIBKL             VALIDATE BOOKS WITH LABELS                   
         MVC   CNUMBKS,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   CNUMBKS,1           ONLY 1 BOOK ALLOWED FOR PROPOSALS            
         BNH   *+14                                                             
         MVC   RERROR,=AL2(MANYBKS1)                                            
         B     ERREND                                                           
         SPACE 1                                                                
         MVI   BYTE,X'07'          INDICATE BOOK OVERRIDE                       
         BAS   RE,BLDOVRD                                                       
         B     VR270                                                            
         SPACE 1                                                                
VR210    CLC   =C'NONE',8(R2)      DO THEY WANT TO SUPPRESS ALL BOOKS?          
         BNE   *+14                NO                                           
         MVC   DBKS(4),=C'NONE'                                                 
         B     VR280                                                            
         SPACE 1                                                                
         CLI   8(R2),C'+'          ARE THEY FILTERING?                          
         BE    VR215                                                            
         CLI   8(R2),C'-'                                                       
         BE    VR215               YES                                          
         SPACE 1                                                                
         ZIC   R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'-'          DISALLOW MINUS SIGNS                         
         BE    VR250               FOUND ONE - ERROR                            
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     VR240                                                            
         SPACE 1                                                                
VR215    XCEF  BLOCK,480           YES - CLEAR SCANNER BLOCK                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   DMCB+4,0            WAS SCAN SUCCESSFUL?                         
         BE    VR250               NO -- LET BOOKVAL CAUSE ERROR                
         SPACE 1                                                                
         LA    R1,BLOCK                                                         
         XC    WORK,WORK                                                        
         LA    RE,WORK+8           BUILD FIELD WITHOUT FILTERS IN WORK          
         LA    R5,DBKF             FILTER AREA                                  
         SR    R3,R3               KEEP TOTAL FIELD LENGTH                      
         SPACE 1                                                                
VR220    CLI   0(R1),0             ANY MORE BOOKS?                              
         BE    VR230               NO                                           
         CLI   0(R1),1             MAYBE THEY ENTERED A SOURCE                  
         BE    VR250               DISALLOW THIS                                
         CLI   1(R1),0             ANYTHING IN SECOND HALF?                     
         BNE   VR250               YES -- ERROR                                 
         CLI   12(R1),C'+'         MAKE SURE EACH BOOK HAS A FILTER             
         BE    *+12                                                             
         CLI   12(R1),C'-'                                                      
         BNE   VR250               NO FILTER -- ERROR                           
         SPACE 1                                                                
         ZIC   RF,0(R1)            FIELD LENGTH                                 
         AR    R3,RF               BUMP TOTAL LENGTH                            
         BCTR  RF,0                MINUS ONE FOR EX INSTRUCTION                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),13(R1)      BOOK                                         
         MVC   0(1,R5),12(R1)      FILTER                                       
         SPACE 1                                                                
         AR    RE,RF               POINT RE TO END OF STRING                    
         MVI   0(RE),C','          FOLLOW BOOK BY COMMA                         
         LA    RE,1(RE)                                                         
         LA    R1,32(R1)           BUMP TO NEXT BOOK                            
         LA    R5,1(R5)            BUMP TO NEXT FILTER                          
         B     VR220                                                            
         SPACE 1                                                                
VR230    BCTR  R3,0                DECREMENT TOTAL LENGTH                       
         STC   R3,WORK+5           FUDGE INPUT LENGTH                           
         LA    R3,8(R3)            LENGTH OF HEADER                             
         STC   R3,WORK             FUDGE FIELD LENGTH                           
         BCTR  RE,0                BACK UP TO LAST COMMA                        
         MVI   0(RE),C' '                                                       
         LA    R2,WORK                                                          
         SPACE 1                                                                
VR240    EQU   *                                                                
         ZIC   R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
VR245    EQU   *                                                                
         CLI   0(RF),C'='          REPLACE '=' WITH '-'                         
         BNE   VR245A                                                           
         MVI   0(RF),C'-'                                                       
VR245A   CLI   0(RF),C'$'          REPLACE '$' WITH '-'                         
         BNE   VR245B                                                           
         MVI   0(RF),C'-'                                                       
VR245B   LA    RF,1(RF)                                                         
         BCT   R0,VR245                                                         
         SPACE 1                                                                
         MVI   MAX,10                                                           
         XCEF  BLOCK,480                                                        
         GOTO1 BOOKVAL,DMCB,(CSOURCE,(R2)),(MAX,BLOCK),(C'L',SCANNER), +        
               CBKLABEL                                                         
         CLI   4(R1),0                                                          
         BNE   *+18                                                             
VR250    MVC   RERROR,=AL2(INVBOK)                                              
         LA    R2,DETBKSH                                                       
         B     ERREND                                                           
         SPACE 1                                                                
         MVC   CNUMBKS,4(R1)       SAVE NUMBER OF BOOKS                         
         CLI   CNUMBKS,6           6 BOOKS ALLOWED FOR AVAILS                   
         BNH   *+14                                                             
         MVC   RERROR,=AL2(MANYBKS6)                                            
         B     ERREND                                                           
         SPACE 1                                                                
         ZIC   R0,CNUMBKS                                                       
         LA    RF,BLOCK                                                         
         XI    0(RF),X'80'         FLIP CPP/CPM BITS                            
         LA    RF,3(RF)                                                         
         BCT   R0,*-8                                                           
         MVC   DBKS,BLOCK                                                       
         SPACE 1                                                                
         ZIC   R0,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'-'          RESTORE =, THE CPP/CPM INDICATOR             
         BNE   *+8                                                              
         MVI   0(RF),C'='                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         SPACE 1                                                                
         CLI   CNUMBKS,1           CHECK FOR >1 SERVICE ON BOOKS                
         BE    VR270               ONLY 1 BOOK, NO CHECKING                     
         MVC   RERROR,=AL2(SINGSVC)                                             
         XC    BLOCK(18),BLOCK                                                  
         MVC   BLOCK(18),DBKS                                                   
         ZIC   RF,CNUMBKS                                                       
         BCTR  RF,0                                                             
         LR    R0,RF                                                            
         LA    RF,BLOCK                                                         
VR260    EQU   *                                                                
         NI    0(RF),X'41'         ONLY LOOK AT SERVICE BITS                    
         NI    3(RF),X'41'                                                      
         CLC   0(1,RF),3(RF)                                                    
         BNE   ERREND                                                           
         LA    RF,3(RF)                                                         
         BCT   R0,VR260                                                         
         XC    BLOCK(18),BLOCK                                                  
         XC    RERROR(2),RERROR                                                 
         SPACE 1                                                                
VR270    CLC   CBKLABEL,SPACES     IF THERE WERE BOOK LABELS                    
         BE    VR280               ADD THE X'0B' ELEMENT                        
*                                  EVEN THOUGH THE LABEL WILL GO ON             
*                                  THE PROPOSAL OVERRIDE ELEMENT ALSO           
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
         DROP  R6                                                               
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD X'0B' ELEMENT                            
         SPACE 1                                                                
*====================================================================*          
*    NOW REBUILD X'01' DETAIL ELEMENT                                *          
*====================================================================*          
         SPACE 1                                                                
VR280    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         CLI   ETYPE,C'P'                                                       
         BE    VR290                                                            
         SPACE 1                                                                
         USING RAVLDEL,R6                                                       
         MVI   RAVLDCOD,X'01'                                                   
         MVI   RAVLDLEN,RAVLDLLQ                                                
         MVC   RAVLDINV,DINV       INVENTORY NUMBER                             
         MVC   RAVLDATE,DATE       SELECTED DATE                                
         MVC   RAVLDRTE,DRTE       RATE ($)                                     
         MVC   RAVLDFRM,DFRM       CLASSES/LENGTHS                              
         MVC   RAVLDSAT,DSAT       SATELLITE IF NON-0                           
         MVC   RAVLDBKS,DBKS       BOOKS                                        
         MVC   RAVLDBKF,DBKF       BOOK FILTERS                                 
         DROP  R6                                                               
         B     VR300                                                            
         SPACE 1                                                                
         USING RPRPDEL,R6                                                       
VR290    MVI   RPRPDCOD,X'01'                                                   
         MVI   RPRPDLEN,RPRPDELQ                                                
         MVC   RPRPDINV,DINV       INVENTORY NUMBER                             
         MVC   RPRPDATE,DATE       SELECTED DATE                                
         MVC   RPRPDRTE,DRTE       RATE ($)                                     
         MVC   RPRPDSEC,DSEC       SECONDS LENGTH                               
         MVC   RPRPDSAT,DSAT       SATELLITE IF NON-0                           
         MVC   RPRPDNUM,DNUM       NUMBER OF SPOTS (0 IF DNC=T)                 
         MVC   RPRPDNC,DNC         NUMBER CODE (W, X OR T)                      
         DROP  R6                                                               
         SPACE 1                                                                
VR300    GOTO1 ADDELEM             ADD X'01' ELEMENT                            
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE DEMO VALUE OVERRIDES - OPTIONAL                        *          
*====================================================================*          
         SPACE 1                                                                
         LA    R2,DETDEMH                                                       
         CLI   5(R2),0                                                          
         BE    VR340                                                            
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
         XCEF  BLOCK,480                                                        
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERREND                                                           
         ZIC   R4,CNUMDEM                                                       
         CR    R5,R4               CAN'T HAVE MORE DEMO OVERRIDES               
         BH    ERREND              THAN DEMO CATEGORIES                         
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
VR330    CLI   0(R4),0                                                          
         BE    *+12                                                             
         TM    2(R4),X'80'         VALID NUMERIC                                
         BZ    ERREND                                                           
         LA    R4,32(R4)                                                        
         BCT   R5,VR330                                                         
         SPACE 2                                                                
VR340    MVI   BYTE,X'04'          INDICATE DEMO OVERRIDES                      
         BAS   RE,BLDOVRD                                                       
         EJECT                                                                  
*====================================================================*          
*    VALIDATE PROJECTION EXPRESSIONS                                 *          
*                                                                    *          
*    ANY INPUT HERE OVERRIDES HEADER RECORD                          *          
*                                                                    *          
*         IF SOURCE IS 'I', SCHEME/PERIOD FIELDS ARE INVALID         *          
*               AND PJ FIELDS ARE OPTIONAL                           *          
*      VALID INPUT FOR PJ IS B#=ANY VALID UPGRADE EXPRESSION         *          
*                (WHERE # INDICATES BOOK THE UPGRADE REFERS TO)      *          
*                                                                    *          
*         IF SOURCE IS 'S', FIRST SCHEME/PERIOD FIELD IS REQUIRED    *          
*               AND OTHER SCHEME/PERIOD AND PJ FIELDS ARE OPTIONAL   *          
*      VALID INPUT FOR SCHEME/PERIOD FIELD IS                        *          
*                 SCHEME/PERIOD OR SCHEME/PERIOD-YEAR                *          
*      VALID INPUT FOR PJ IS UPT=ANY VALID UPGRADE EXPRESSION        *          
*                                                                    *          
*           IF THERE IS AN UPGRADE  ON THE SCREEN,                   *          
*           IT OVERRIDES THE UPGRADE FROM THE SCHEME                 *          
*====================================================================*          
         SPACE 1                                                                
*         LA    R2,DETSP1H                                                      
*         CLI   ESOURCE,C'S'                                                    
*         BE    VR350                                                           
*         MVC   RERROR,=AL2(INVALID)                                            
*         CLI   5(R2),0                                                         
*         BNE   ERREND                                                          
*         LA    R2,DETSP2H          PROTECTED FOR PROPOSALS                     
*         CLI   5(R2),0                                                         
*         BNE   ERREND                                                          
*         B     VR380                                                           
         SPACE 1                                                                
*VR350    CLI   5(R2),0                                                         
*         BNE   *+16                                                            
*         LA    R3,DETSP2H          PROTECTED FOR PROPOSALS                     
*         CLI   5(R3),0                                                         
*         BE    VR380                                                           
         SPACE 1                                                                
*         GOTO1 SSPOT               SWITCH TO SPOT                              
*         GOTO1 VALISID                                                         
         SPACE 1                                                                
*         LA    R2,DETSP2H          PROTECTED FOR PROPOSALS                     
*         CLI   5(R2),0                                                         
*         BE    VR370                                                           
*         GOTO1 VALISID                                                         
*VR370    GOTO1 SREP                SWITCH BACK TO REP                          
         SPACE 1                                                                
*         XC    ELEM,ELEM           BUILD X'0D' (SID) ELEMENT                   
*         LA    R6,ELEM                                                         
*         USING RAVLSID,R6                                                      
*         MVC   RAVLSCD(2),=X'0D12'                                             
*         MVC   RAVLSS1(16),CSCHEME                                             
*         DROP  R6                                                              
*         GOTO1 ADDELEM             ADD X'0D' ELEMENT                           
         SPACE 1                                                                
VR380    LA    R2,DETPJ1H          1ST PJ FIELD IS OPTIONAL                     
         XC    ELEM,ELEM           PREPARE FOR NEW 05 ELEMENT                   
         LA    R6,ELEM                                                          
         USING RAVLNEL,R6                                                       
         MVC   RAVLNCOD(2),=X'050E'                                             
         CLI   5(R2),0             ANY OVERRIDE?                                
         BE    VR385               NO                                           
         CLC   =C'NONE',8(R2)      DO THEY WANT TO IGNORE HEADER PJ?            
         BNE   VR388               NO                                           
         MVC   RAVLNOPS,=C'NONE'   YES -- PUT 'NONE' IN DUMMY ELEMENT           
         DROP  R6                                                               
         SPACE 1                                                                
VR385    GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,=C'ADD=CODE'                   
         CLI   DMCB+12,0                                                        
         BE    VR390               NO ERROR                                     
         MVC   RERROR,=AL2(TOOLONG)                                             
         CLI   DMCB+12,5                                                        
         BE    ERREND                                                           
         DC    H'0'                BAD RETURN CODE                              
         SPACE 1                                                                
VR388    LA    R3,CUPTYP1                                                       
         GOTO1 VALIUPT             VALIDATES FIELD & BUILD 05 ELEMENT           
         SPACE 1                                                                
VR390    LA    R2,DETPJ2H          2ND PJ FIELD IS OPTIONAL. . .                
         CLI   5(R2),0             . . . AND PROTECTED FOR PROPOSALS            
         BE    VR410                                                            
         SPACE 1                                                                
         MVC   RERROR,=AL2(INVALID)                                             
*         CLI   ESOURCE,C'S'        OR IF SOURCE IS S                           
*         BNE   VR400                                                           
*         LA    R3,DETSP1H          AND IF NO 2ND SCHEME/PER, NO PJ             
*         CLI   5(R3),0                                                         
*         BNE   VR400                                                           
*        OC    CSCHEME2(8),CSCHEME2   COULD BE 2ND SCHEME ON HDR RECORD         
*         BZ    ERREND                                                          
         SPACE 1                                                                
VR400    LA    R3,CUPTYP2                                                       
         GOTO1 VALIUPT             VALIDATES FIELD & BUILDS 05 ELEMENT          
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TEXT                                                   *          
*====================================================================*          
         SPACE 1                                                                
VR410    LA    R2,DETCOM1H                                                      
         L     R6,AIO                                                           
         GOTO1 VRETEXT,DMCB,(R2),(R6),(X'16',0),SCANNER,VRECUP                  
         MVC   RERROR,=AL2(INVALID)                                             
         OC    DMCB,DMCB                                                        
         BZ    *+12                                                             
         L     R2,DMCB                                                          
         B     ERREND                                                           
         SPACE 3                                                                
         LA    R2,DETLINH          PUT LINE NUMBER TO SCREEN                    
         ZIC   R3,XDETNUM                                                       
         EDIT  (R3),(3,8(R2)),ALIGN=LEFT                                        
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   EDETNUM,8(R2)                                                    
         EJECT                                                                  
*====================================================================*          
* DO RECORD ADD/PUTS HERE (BECAUSE OF COMPLICATIONS WITH PACKAGES)   *          
*====================================================================*          
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVC   DETKEY(27),0(R6)                                                 
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR600                                                            
         GOTO1 ADDREC                                                           
         B     VR620                                                            
         SPACE 1                                                                
VR600    CLI   ACTNUM,ACTCHA                                                    
         BNE   VR620                                                            
         BAS   RE,CHAPRAV                                                       
         SPACE 2                                                                
VR620    CLI   ACTNUM,ACTADD                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VR650                                                            
         SPACE                                                                  
         CLI   DNC,C'T'            IF THIS IS A T PACKAGE                       
         BNE   VR650                                                            
         BAS   RE,PKG              ADD/CHANGE PACKAGE HEADER                    
         SPACE                                                                  
VR650    LA    R2,DETINVH                                                       
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+18                                                             
         MVC   RERROR,=H'6'        RECORD HAS BEEN ADDED                        
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     VR660                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RERROR,=H'5'        RECORD HAS BEEN CHANGED                      
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         SPACE                                                                  
VR660    EQU   *                                                                
         B     DREC                NOW DISPLAY ADDED/CHANGED RECORD             
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
         MVI   ELCODE,X'01'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ETYPE,C'A'          AVAIL?                                       
         BE    DR30                                                             
         SPACE 1                                                                
         USING RPRPDEL,R6                                                       
         CLI   RPRPDNC,C'T'                                                     
         BNE   DR10                                                             
         DROP  R6                                                               
* NEED TO GET TOTAL NUMBER FROM PACKAGE HEADER,NOT DETAIL RECORD                
         L     R6,AIO              CURRENT DETAIL RECORD                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(26),0(R6)       GET PACKAGE HEADER                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'13'        PACKAGE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPPEL,R6                                                       
         ZIC   R5,RPRPPSPT         GET TOTAL NUMBER OF SPOTS                    
         DROP  R6                                                               
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    DR20                                                             
         DC    H'0'                                                             
         USING RPRPDEL,R6                                                       
         SPACE 1                                                                
DR10     ZIC   R5,RPRPDNUM                                                      
DR20     LTR   R5,R5                                                            
         BZ    DR25                                                             
         EDIT  (R5),(3,DETNUM),ALIGN=LEFT                                       
         LR    R3,R0               R0=NUM OF SIGNIF. CHARS                      
         LA    R3,DETNUM(R3)                                                    
         MVC   0(1,R3),RPRPDNC                                                  
         SPACE 1                                                                
DR25     MVC   CBLOCK(3),RPRPDINV                                               
         MVC   CBLOCK+3(3),RPRPDATE                                             
         MVC   CBLOCK+6(1),RPRPDSAT                                             
         DROP  R6                                                               
         B     DR40                                                             
         SPACE 1                                                                
         USING RAVLDEL,R6                                                       
DR30     OC    RAVLDBKS,RAVLDBKS   ANY OVERRIDE BOOKS?                          
         BZ    DR35                NO                                           
         CLC   =C'NONE',RAVLDBKS   ANY BOOKS AT ALL?                            
         BNE   *+14                                                             
         MVC   DETBKS(4),=C'NONE'  NO                                           
         B     DR35                                                             
         SPACE 1                                                                
         XC    DMCB+8(8),DMCB+8                                                 
         MVI   MAX,6                                                            
         LA    R2,DETBKSH                                                       
         OC    RAVLDBKF,RAVLDBKF   ANY FILTERS?                                 
         BZ    *+16                NO                                           
         LA    RE,RAVLDBKF                                                      
         ST    RE,DMCB+12                                                       
         MVI   DMCB+12,C'+'                                                     
         GOTO1 DISPBKL,DMCB,(C'$',RAVLDBKS),A0BELEM                             
         SPACE 1                                                                
         LA    RF,DETBKSH                                                       
         ZIC   R0,0(RF)                                                         
         SH    R0,=H'8'                                                         
         LA    RF,8(RF)                                                         
DR31     CLI   0(RF),C'$'                                                       
         BNE   DR31A                                                            
         MVI   0(RF),C'='                                                       
DR31A    LA    RF,1(RF)                                                         
         BCT   R0,DR31                                                          
         SPACE 1                                                                
DR35     MVC   CBLOCK(3),RAVLDINV                                               
         MVC   CBLOCK+3(3),RAVLDATE                                             
         MVC   CBLOCK+6(1),RAVLDSAT                                             
         DROP  R6                                                               
         SPACE 1                                                                
DR40     LA    R2,DETINVH          DISPLAY INVENTORY NUMBER                     
         GOTO1 DISPINV                                                          
         SPACE 1                                                                
         CLI   8(R2),C'P'          SKIP PURE NUMBER STUFF FOR NOW               
         BE    DR45                                                             
         SPACE 1                                                                
         CLC   CBLOCK(3),=C'MAN'                                                
         BE    DR45                                                             
         SPACE 1                                                                
         GOTO1 DISIDTT             GET INVENTORY DAY/TIME/TITLE                 
         OC    CBLOCK+10(L'DETDAY),CBLOCK+10                                    
         BNZ   DR42                INVENTORY RECORD WAS FOUND                   
         MVC   DETDAY,HYPHENS                                                   
         MVC   DETTIM,HYPHENS                                                   
         MVC   DETTTL,HYPHENS                                                   
         B     DR45                                                             
         SPACE 1                                                                
DR42     MVC   DETDAY,CBLOCK+10    DAY                                          
         MVC   DETTIM,CBLOCK+20    TIME                                         
         MVC   DETTTL,CBLOCK+40    TITLE                                        
         SPACE 1                                                                
DR45     OI    DETDAYH+4,X'20'     FIELD HAS BEEN VALIDATED PREV                
         MVI   DETDAYO,C' '        CLEAR OUT OVERRIDE INDICATOR                 
         OI    DETDAYOH+6,X'80'     TRANSMIT FIELD                              
         SPACE 1                                                                
         OI    DETTIMH+4,X'20'                                                  
         MVI   DETTIMO,C' '                                                     
         OI    DETTIMOH+6,X'80'                                                 
         SPACE 1                                                                
         OI    DETTTLH+4,X'20'                                                  
         MVI   DETTTLO,C' '                                                     
         OI    DETTTLOH+6,X'80'                                                 
         SPACE 1                                                                
         CLI   ETYPE,C'P'          PROPOSAL?                                    
         BE    DR60                                                             
         SPACE 1                                                                
         USING RAVLDEL,R6                                                       
         OC    RAVLDRTE,RAVLDRTE   ANY RATES?                                   
         BZ    DR55                NO                                           
         SPACE 1                                                                
         LA    R2,DETRAT                                                        
         LA    R5,RAVLDRTE                                                      
         LA    R3,6                MAXIMUM NO. OF RATES                         
         SPACE 1                                                                
DR50     CH    R3,=H'6'            DON'T PUT IN COMMA BEFORE FIRST RATE         
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         ICM   RE,15,0(R5)                                                      
         EDIT  (RE),(6,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0               LENGTH OF OUTPUT                             
         LA    R5,4(R5)                                                         
         BCT   R3,DR50                                                          
         SPACE 1                                                                
         BCTR  R2,0                                                             
         CLI   0(R2),C','          REMOVE FINAL COMMA                           
         BNE   *+12                                                             
         MVI   0(R2),C' '                                                       
         B     *-14                                                             
         SPACE 1                                                                
DR55     LA    R2,DETLENH          LENGTHS                                      
         MVI   MAX,6                                                            
         GOTO1 DISPLEN,DMCB,RAVLDFRM                                            
         B     DR80                                                             
         SPACE 1                                                                
         USING RPRPDEL,R6                                                       
DR60     OC    RPRPDRTE,RPRPDRTE   OUTPUT RATE, IF ANY                          
         BZ    DR70                                                             
         ICM   R5,15,RPRPDRTE                                                   
         EDIT  (R5),(6,DETRAT),ALIGN=LEFT                                       
         SPACE 1                                                                
DR70     LA    R2,DETLENH                                                       
         MVC   HALF+1(1),RPRPDSEC  BUILD TO LOOK LIKE CLASS/LENGTH              
         MVI   HALF,0                                                           
         MVI   MAX,1                                                            
         GOTO1 DISPLEN,DMCB,HALF                                                
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING RPRPKEY,R6                                                       
         CLC   RPRPKPLN,=X'FFFF'                                                
         BE    DR80                                                             
         LA    R2,DETCDH                                                        
         MVC   8(1,R2),RPRPKPLN                                                 
         DROP  R6                                                               
         SPACE 1                                                                
DR80     L     R6,AIO                                                           
         MVI   ELCODE,X'06'        OVERRIDES                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR90     BAS   RE,NEXTEL                                                        
         BNE   DR160                                                            
         SPACE 1                                                                
         USING RAVLOEL,R6                                                       
         SPACE 1                                                                
         CLI   RAVLOTYP,1          DAY                                          
         BNE   DR110                                                            
         LA    R2,DETDAYOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,DETDAYH                                                       
         MVC   DETDAY(L'DETDAY),SPACES                                          
         B     DR150                                                            
         SPACE 1                                                                
DR110    CLI   RAVLOTYP,2          TIME                                         
         BNE   DR120                                                            
         LA    R2,DETTIMOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,DETTIMH                                                       
         MVC   DETTIM(L'DETTIM),SPACES                                          
         B     DR150                                                            
         SPACE 1                                                                
DR120    CLI   RAVLOTYP,3          TITLE                                        
         BNE   DR130                                                            
         LA    R2,DETTTLOH                                                      
         MVI   8(R2),C'*'          INDICATE OVERRIDE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,DETTTLH                                                       
         MVC   DETTTL(L'DETTTL),SPACES                                          
         B     DR150                                                            
         SPACE 1                                                                
DR130    CLI   RAVLOTYP,4          DEMO                                         
         BNE   DR140                                                            
         LA    R2,DETDEMH                                                       
         MVC   DETDEM(L'DETDEM),SPACES                                          
         B     DR150                                                            
         SPACE 1                                                                
DR140    CLI   RAVLOTYP,7          BOOKS                                        
         BNE   DR90                                                             
         LA    R2,DETBKSH                                                       
         MVC   DETBKS(L'DETBKS),SPACES                                          
         SPACE 1                                                                
DR150    ZIC   RE,RAVLOLEN                                                      
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RAVLODTA                                                 
         B     DR90                                                             
         DROP  R6                                                               
         SPACE 1                                                                
DR160    LA    R2,DETCOM1H         TEXT                                         
         L     R6,AIO                                                           
         GOTO1 VUNTEXT,DMCB,(R2),(R6)                                           
         SPACE 1                                                                
         LA    R3,BLOCK            PREPARE DBLOCK FOR UNUPGR                    
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         DROP  R3                                                               
         SPACE 1                                                                
         L     R6,AIO              ANY UPGRADES?                                
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR200               NO                                           
         SPACE 1                                                                
         ST    R6,FULL                                                          
         USING RAVLNEL,R6                                                       
         CLC   =C'NONE',RAVLNOPS   DO THEY WANT TO IGNORE THE DEFAULT?          
         BNE   *+14                                                             
         MVC   DETPJ1(4),=C'NONE'  YES                                          
         B     DR170                                                            
         OC    RAVLNBKS(12),RAVLNBKS  IS THERE AN OVERRIDE?                     
         BZ    DR170               NO                                           
         L     R6,AIO              DISPLAY FIRST UPGRADE                        
         GOTO1 VUNUPGR,DMCB,(1,DETPJ1H),(R6),UNSCAN,DEMOCON,(R3)                
         SPACE 1                                                                
DR170    L     R6,FULL             R6 POINTS TO FIRST UPGRADE ELEMENT           
         BAS   RE,NEXTEL           IS THERE A SECOND?                           
         BNE   DR200               NO                                           
         SPACE 1                                                                
         USING RAVLNEL,R6                                                       
         CLC   =C'NONE',RAVLNOPS   DO THEY WANT TO IGNORE THE DEFAULT?          
         BNE   *+14                                                             
         MVC   DETPJ2(4),=C'NONE'  YES                                          
         B     DR200                                                            
         OC    RAVLNBKS(12),RAVLNBKS  IS THERE AN OVERRIDE?                     
         BZ    DR200               NO                                           
         L     R6,AIO              DISPLAY SECOND UPGRADE                       
         GOTO1 VUNUPGR,DMCB,(2,DETPJ2H),(R6),UNSCAN,DEMOCON,(R3)                
         DROP  R6                                                               
         SPACE 1                                                                
*  DO DISPLAY OF SCHEME/PERIOD HERE (ONLY FOR SID)                              
         SPACE 1                                                                
DR200    EQU   *                                                                
         CLI   RMSGTYPE,C'I'                                                    
         BNE   XIT                                                              
         CLC   RERROR,=H'6'                                                     
         BE    ERREND                                                           
         CLC   RERROR,=H'5'                                                     
         BE    ERREND                                                           
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*  THIS ROUTINE GETS THE DEMO CATEGORIES FROM THE DEMO HEADER        *          
*   AND PUTS THEM TO THE SCREEN                                      *          
*  IT ALSO SAVES BOOK INFO, AND 2ND SCHEME/PERIOD INFO FOR USE       *          
*   IN VALREC ROUTINE, SO HEADER DOESN'T HAVE TO BE LOOKED UP AGAIN. *          
*--------------------------------------------------------------------*          
HDRINFO  NTR1                                                                   
         L     R6,AIO3             POINT TO HEADER RECORD                       
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RAVLELEM,R6                                                      
         LA    R2,DETDC1H          PUT DEMO CATEGORIES TO SCREEN                
         MVC   8(L'DETDC1,R2),SPACES                                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         MVI   CNUMDEM,0                                                        
         OC    RAVLDEM,RAVLDEM                                                  
         BZ    HI20                THERE MAY BE NO DEMOS                        
         SPACE 1                                                                
         XC    CDEMOS,CDEMOS                                                    
         LA    R5,CDEMOS                                                        
         MVC   0(L'RAVLDEM,R5),RAVLDEM DEMOS + ENDING ZERO                      
         MVI   MAX,7                                                            
         GOTO1 DISPDEM,DMCB,CDEMOS                                              
         LA    RF,DETDC1H                                                       
         ZIC   R0,0(RF)                                                         
         SH    R0,=H'8'                                                         
         LA    RF,8(RF)                                                         
HI05     CLI   0(RF),C'$'                                                       
         BNE   HI05A                                                            
         MVI   0(RF),C'='                                                       
HI05A    LA    RF,1(RF)                                                         
         BCT   R0,HI05                                                          
         SPACE 1                                                                
         LA    R5,CDEMOS+18        COUNT DEMOS                                  
         LA    R3,7                MAXIMUM OF 7                                 
         SPACE 1                                                                
HI10     OC    0(3,R5),0(R5)                                                    
         BNZ   *+12                                                             
         SH    R5,=H'3'                                                         
         BCT   R3,HI10                                                          
         SPACE 1                                                                
         STC   R3,CNUMDEM                                                       
         SPACE 1                                                                
HI20     XC    CBOOKS,CBOOKS                                                    
         MVC   CBOOKS(L'RAVLBKS),RAVLBKS                                        
         CLI   ETYPE,C'A'          AVAILS?                                      
         BNE   HI50                NO                                           
         SPACE 1                                                                
         SR    R5,R5               COUNT NUMBER OF LENGTHS                      
         LA    R3,RAVLRFRM         A(LENGTHS)                                   
HI30     CH    R5,=H'6'            ARE WE AT MAX OF SIX?                        
         BE    HI40                YES                                          
         CLI   1(R3),0             END OF LIST?                                 
         BE    HI40                YES                                          
         LA    R3,2(R3)            BUMP TO NEXT LENGTH IN LIST                  
         LA    R5,1(R5)            INCREMENT LENGTH COUNTER                     
         B     HI30                                                             
         DROP  R6                                                               
HI40     STC   R5,CNUMLENS         SAVE NUMBER OF LENGTHS IN HEADER             
         SPACE 1                                                                
HI50     XC    CSCHEME2(8),CSCHEME2                                             
*         CLI   ESOURCE,C'S'                                                    
*         BNE   HIX                                                             
*         L     R6,AIO3                                                         
*         MVI   ELCODE,X'0D'                                                    
*         BAS   RE,GETEL                                                        
*         BE    *+6                                                             
*         DC    H'0'                SHOULD BE AT LEAST A SCHEME 1               
*         USING RAVLSID,R6                                                      
*         MVC   CSCHEME2(8),RAVLSS2                                             
         SPACE 1                                                                
HIX      B     XIT                                                              
*         DROP  R6                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*   THIS ROUTINE ADDS A PACKAGE HEADER RECORD AND AN X'13' ELEMENT   *          
*     WHEN CODE IS T.  THE PACKAGE HEADER IS TO HOLD THE TOTAL NUMBER*          
*     OF SPOTS FOR THE PACKAGE. (THAT'S WHY X AND W DON'T HAVE       *          
*     HEADERS.)                                                      *          
*--------------------------------------------------------------------*          
PKG      NTR1                                                                   
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
*********L     R6,AIO1             DETAIL RECORD JUST ADDED                     
*********MVC   KEY(26),0(R6)                                                    
         MVC   KEY(26),DETKEY                                                   
         OI    DMINBTS,X'08'       READ DELETES AS WELL                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NEWREC                                                           
         SPACE 1                                                                
         TM    KEY+27,X'80'        IS IT DELETED                                
         BZ    PKG10               NO                                           
         NI    KEY+27,X'7F'        YES                                          
         GOTO1 WRITE               UNDELETE OLD                                 
         SPACE 1                                                                
PKG10    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPPEL,R6                                                       
         ZIC   RE,RPRPPSPT         GET TOTAL NUMBER OF SPOTS                    
         ZIC   RF,PSPT                                                          
         CR    RE,RF                                                            
         BE    PKGX                RECORD IS ALREADY CORRECT                    
         MVC   RPRPPSPT,PSPT                                                    
         GOTO1 PUTREC              RECORD NEEDS TO BE CHANGED                   
         B     PKGX                                                             
         DROP  R6                                                               
         SPACE 1                                                                
NEWREC   L     R6,AIO                                                           
         LR    RE,R6                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE 1                                                                
         USING RPRPREC,R6                                                       
         MVC   RPRPKEY,KEYSAVE                                                  
         MVC   RPRPLEN,=H'46'      LEN OF KEY(34) + X'13' ELEM (12)             
         LA    R5,34(R6)                                                        
         USING RPRPPEL,R5                                                       
         MVC   RPRPPCOD(2),=X'130C'                                             
         MVC   RPRPPSPT,PSPT                                                    
         SPACE 1                                                                
         GOTO1 ADDREC                                                           
         DROP  R5,R6                                                            
         SPACE 1                                                                
PKGX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE ADDS A X'06' OVERRIDE ELEMENT ON A DETAIL RECORD      *          
* BUT FIRST IT DELETES AN OLD X'06' OF THE SAME TYPE IF NECESSARY.   *          
* THE TYPE OF OVERRIDE IS IN BYTE                                    *          
*   1=DAY, 2=TIME, 3=PROGRAM TITLE, 4=DEMOS, 7=BOOK                  *          
* R2 POINTS TO THE (VALIDATED) FIELD                                 *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
BLDOVRD  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
BOV10    BAS   RE,NEXTEL                                                        
         BNE   BOV20                                                            
         USING RAVLOEL,R6                                                       
         CLC   RAVLOTYP,BYTE       IF SAME TYPE, THEN DELETE ELEMENT            
         BNE   BOV10                                                            
         GOTO1 VRECUP,DMCB,(C'R',AIO),(R6),0                                    
         SPACE 1                                                                
BOV20    XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         MVI   RAVLOCOD,6                                                       
         MVC   RAVLOTYP,BYTE                                                    
         SPACE 1                                                                
         ZIC   RE,5(R2)            LENGTH OF INPUT. . .                         
         LTR   RE,RE                                                            
         BZ    XIT                 FIELD IS EMPTY -- NO OVERRIDE                
         SPACE 1                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RAVLODTA(0),8(R2)                                                
         LA    RE,4(RE)            . . . + LEN OF OVERHEAD + 1 FOR EX           
         STC   RE,RAVLOLEN         . . . EQUALS LENGTH OF ELEMENT               
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CHANGES A DETAIL RECORD.                              *          
* -IF THE CHANGED RECORD DOESN'T EXIST (CHANGE IN PKG), IT'S ADDED   *          
* -IF IT WAS DELETED, IT'S RESTORED                                  *          
* -IF RECORD WAS A PACKAGE, AND IS NOW DIFFERENT, THE OLD PKG HDR    *          
*  IS DELETED, IF THERE ARE NO MORE DETAIL LINES LEFT.               *          
*                                                                    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
CHAPRAV  NTR1                                                                   
         SPACE 1                                                                
         LA    R5,22               ASSUME PROPOSALS (23 - 1 FOR EX)             
         CLI   ETYPE,C'P'                                                       
         BE    *+8                                                              
         LA    R5,25               NO, IT'S AVAILS (26 - 1 FOR EX)              
         SPACE 1                                                                
         MVC   AIO,AIO2            OLD RECORD                                   
         XC    KEY,KEY                                                          
         MVC   KEY,DETKEY          LOOK FOR DETAIL KEY                          
         OI    DMINBTS,X'08'       READ DELETES AS WELL                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   PCHA6               NO KEY SO ADD RECORD                         
         SPACE 1                                                                
         MVC   KEYSAVE,KEY         SAVE STATUS BIT                              
         TM    KEY+27,X'80'        IS IT DELETED                                
         BZ    PCHA5               NO                                           
         NI    KEY+27,X'7F'        YES                                          
         GOTO1 WRITE               UNDELETE OLD                                 
         SPACE 1                                                                
PCHA5    GOTO1 GETREC              GET OLD RECORD (FOR DMGR SEQUENCE)           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              PUT NEW RECORD                               
         TM    KEYSAVE+27,X'80'    DID WE DO A RESTORE?                         
         BZ    PCHAX               NO                                           
         B     PCHA7                                                            
         SPACE 1                                                                
PCHA6    MVC   AIO,AIO1                                                         
         GOTO1 ADDREC                                                           
         SPACE 1                                                                
PCHA7    XC    KEY,KEY             LOOK FOR THE ACTIVE POINTER                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),DETKEY                                                    
         GOTO1 HIGH                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEYSAVE(0),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                SHOULD FIND HEADER RECORD                    
         SPACE 1                                                                
PCHA8    GOTO1 SEQ                                                              
         CLC   KEY(27),DETKEY      THIS IS THE ONE I JUST ADDED                 
         BE    PCHA8                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                THERE IS NO OLD POINTER                      
         SPACE 1                                                                
         CLC   KEY+26(1),DETKEY+26 DOES LINE NUMBER MATCH?                      
         BNE   PCHA8               NO -- KEEP LOOKING                           
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE               DETAIL OLD KEY                               
         SPACE 1                                                                
         CLI   ETYPE,C'A'          AVAIL?                                       
         BE    PCHAX                                                            
         CLI   KEY+24,C'T'         OLD WAS NOT A PACKAGE WITH HEADER            
         BNE   PCHAX                                                            
         MVI   KEY+26,1                                                         
         GOTO1 HIGH                ANY MORE DETAILS                             
         CLC   KEYSAVE(26),KEY                                                  
         BE    PCHAX               YES, SO LEAVE PACKAGE HEADER                 
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+26,0            GET PACKAGE HEADER RECORD                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   PCHAX                                                            
         OI    KEY+27,X'80'        AND DELETE IT                                
         GOTO1 WRITE                                                            
         SPACE 1                                                                
PCHAX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE DELETES A PROPOSAL DETAIL KEY (NOT A RECORD).         *          
* ON ENTRY, DETAIL RECORD TO BE DELETED HAS JUST BEEN READ           *          
* IF THAT RECORD WAS PART OF A 'T' PACKAGE, AND IT'S THE LAST DETAIL *          
*    RECORD FOR THAT PACKAGE, IT DELETES THE PACKAGE HEADER ALSO.    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
DELPRAV  NTR1                                                                   
         OI    KEY+27,X'80'        TURN ON DELETED BIT                          
         GOTO1 WRITE               AND WRITE IT BACK TO DIRECTORY               
         SPACE 1                                                                
         CLI   ETYPE,C'P'          PROPOSAL?                                    
         BNE   DELPRAVX            NO                                           
         SPACE 1                                                                
         CLI   KEY+24,C'T'         IS IT A T PACKAGE                            
         BNE   DELPRAVX                                                         
         MVI   KEY+26,1                                                         
         GOTO1 HIGH                READ HIGH FOR OTHER DETAIL RECORDS           
         CLC   KEY(26),KEYSAVE                                                  
         BE    DELPRAVX            THERE ARE OTHERS, SO LEAVE HEADER            
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVI   KEY+26,0            GO FOR PACKAGE HEADER                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPRAVX                                                         
         OI    KEY+27,X'80'        TURN ON DELETED BIT                          
         GOTO1 WRITE                                                            
DELPRAVX B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE RESTORES A PROPOSAL DETAIL KEY (NOT A RECORD).        *          
* ON ENTRY, DETAIL RECORD TO BE RESTORED HAS JUST BEEN READ          *          
* IF THAT RECORD WAS PART OF A 'T' PACKAGE, AND THERE ARE NO OTHER   *          
*    DETAIL RECORDS FOR THAT PACKAGE, IT RESTORES THE                *          
*    PACKAGE HEADER ALSO.                                            *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
RESTPRAV NTR1                                                                   
         TM    KEY+27,X'80'        WAS IT DELETED                               
         BZ    RESTPRAX                                                         
         MVI   KEY+27,0            UNDELETE IT                                  
         GOTO1 WRITE               AND WRITE IT BACK TO DIRECTORY               
         SPACE 1                                                                
         CLI   ETYPE,C'P'          PROPOSAL?                                    
         BNE   RESTPRAX            NO                                           
         SPACE 1                                                                
         CLI   KEY+24,C'T'         IS IT A T PACKAGE                            
         BNE   RESTPRAX                                                         
         MVI   KEY+26,0                                                         
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 HIGH                READ HIGH FOR PACKAGE HEADER                 
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   RESTPRAX                                                         
         TM    KEY+27,X'80'        WAS IT DELETED                               
         BZ    RESTPRAX                                                         
         MVI   KEY+27,0            UNDELETE IT                                  
         GOTO1 WRITE                                                            
RESTPRAX B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CLEARS OUT A FIELD                                    *          
* ON ENTRY, R2 POINTS TO FIELD HEADER                                *          
*--------------------------------------------------------------------*          
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
*--------------------------------------------------------------------*          
* THIS ROUTINE CLEARS ALL THE UNPROTECTED RECORD FIELDS ON THE SCREEN*          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
CLEARREC NTR1                                                                   
         LA    R2,DETINVH                                                       
CR10     BAS   RE,CLEAR                                                         
         BAS   RE,NEXTUF                                                        
         CLI   0(R2),9             LAST FIELD ON SCREEN                         
         BNE   CR10                                                             
         B     XIT                                                              
         SPACE 3                                                                
NEXTUF   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    NEXTUF                                                           
         BR    RE                                                               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR                                                          
         SPACE 2                                                                
HYPHENS  DC    (L'DETTTL)C'-'                                                   
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* SPRANSIDD  (SRBLKD DSECT)                                                     
* DEDBLOCK  (DBLOCKD DSECT)                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD2D                                                       
         EJECT                                                                  
RAVLD    DSECT                                                                  
       ++INCLUDE REGENAVLN                                                      
         EJECT                                                                  
RPRPD    DSECT                                                                  
       ++INCLUDE REGENPRPN                                                      
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*        WORK AREA                                                              
*                                                                               
A0BELEM  DS    A                   A(BOOK HEADER ELEMENT)                       
*                                                                               
DINV     DS    CL3                 INVENTORY NUMBER                             
DATE     DS    CL3                 SELECTED DATE                                
DRTE     DS    CL24                RATES ($)  (UP TO SIX FOR AVAILS)            
DSEC     DS    CL1                 SECONDS LENGTH                               
DSAT     DS    CL1                 SATELLITE IF NON-0                           
DNUM     DS    CL1                 NUMBER OF SPOTS (0 IF DNC=T)                 
DNC      DS    CL1                 CODE FOR TYPE OF NUMBER IN # FIELD           
*                                     W=PER WEEK                                
*                                     X=PER PROGRAM                             
*                                     T=TOTAL FOR PACKAGE                       
DFRM     DS    XL12                CLASSES/LENGTHS                              
DBKS     DS    XL18                BOOKS (AVAILS ONLY)                          
DBKF     DS    CL6                 BOOK FILTERS (AVAILS ONLY)                   
         SPACE 1                                                                
DETKEY   DS    CL48                DETAIL KEY                                   
PSPT     DS    CL1                 TOTAL NUMBER OF SPOTS IN A PACKAGE           
NUMLENS  DS    XL1                 NUMBER OF LENGTHS                            
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091RESFM0F   05/01/02'                                      
         END                                                                    
