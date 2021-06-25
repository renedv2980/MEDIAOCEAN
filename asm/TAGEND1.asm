*          DATA SET TAGEND1    AT LEVEL 003 AS OF 08/01/12                      
*PHASE T702D1C                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T702D1 - RECORD MAINTENANCE'                                    
T702D1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D1,R6,RR=R3                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(APPLICATION SAVED STORAGE)              
         USING TWAHOLED,R7                                                      
         ST    R3,RELO                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 3                                                                
         CLI   MODE,SETFILE        SET ALTERNATE FILE                           
         BNE   MN20                                                             
         OI    DMINBTS,X'08'       OK TO READ DELETED                           
         LA    R1,CHKDKEYS         DETERMINE IF KEY IS FOR CHKDIR               
MN10     CLC   KEY(1),0(R1)                                                     
         BE    MN12                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),0             TEST END OF TABLE                            
         BNE   MN10                                                             
         B     MN15                NOT A CHKDIR KEY                             
         SPACE 1                                                                
MN12     MVC   SYSDIR,=CL8'CHKDIR' SET CHKDIR/CHKFIL                            
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE 1                                                                
MN15     CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XIT                                                              
         CLI   THISLSEL,CHASELQ    AND SELECTED FOR CHANGE                      
         BNE   XIT                                                              
         L     R1,SYSPARMS                                                      
         L     R1,0(R1)                                                         
         USING TIOBD,R1            R1 = A(TRANSLATOR I/O BLOCK)                 
         LA    RF,RECRHSH                                                       
         S     RF,ATWA                                                          
         STH   RF,TIOBLAST         FORCE GENCON TO THINK A FLD WAS I/P          
         B     XIT                                                              
         SPACE 3                                                                
MN20     CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MN40                                                             
         BAS   RE,DISPLAY                                                       
         SPACE 1                                                                
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XIT                                                              
         OC    RECLBL,RECLBL       AND IF MORE TO COME                          
         BZ    XIT                                                              
         B     MORE                GIVE MY OWN MESSAGE                          
         SPACE 3                                                                
MN40     CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                 GIVE MY OWN MESSAGE                          
         SPACE 3                                                                
         CLI   MODE,XRECPUT        AFTER CHANGING RECORD                        
         BNE   XIT                                                              
         CLI   IOOPT,C'Y'          IF SOMETHING CHANGED HANDLE PASSIVES         
         BNE   MN50                                                             
         CLI   ACTNUM,ACTSEL       ELSE IF ACTION IS SELECT                     
         BNE   MN60                                                             
         CLI   RECLBLH+5,0         AND IF NO MORE TO COME                       
         BE    XIT                 RETURN FOR NEXT RECORD                       
         B     MN60                ELSE GO DISPLAY NEXT PAGE                    
         SPACE 1                                                                
MN50     LH    R5,=Y(PTRBLK-TWAHOLED)                                           
         AR    R5,R7                                                            
         GOTO1 AADDPTRS,DMCB,(X'88',SVPTRBLK),(R5)    UPDATE POINTERS           
                                                                                
         BAS   RE,SETCURST         SET TO RESTART AT BEG. OF SCREEN             
         SPACE 1                                                                
MN60     BAS   RE,DISPLAY          (RE-)DISPLAY                                 
         SPACE 1                                                                
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BE    MORE                GIVE MY OWN MESSAGE                          
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 1                                                                
DKEY     NTR1                                                                   
         LA    R3,KEY              R3 = A(KEY)                                  
         USING TLDRD,R3                                                         
         GOTO1 HEXOUT,DMCB,TLDRDA,DUB,4,0  CONVERT DISK ADDRESS                 
         SPACE 1                                                                
         CLC   DUB,RECDA           IF DISK ADDRESS CHANGED                      
         BE    DK10                                                             
         MVC   RECDA(8),DUB        DISPLAY IT                                   
         OI    RECDAH+6,X'80'                                                   
         XC    WORKD(WORKLNQ),WORKD  AND RE-INITIALIZE STORAGE                  
         SPACE 1                                                                
DK10     BAS   RE,VALREM           VALIDATE REMAINING KEY FIELDS                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         XC    WORKD(WORKLNQ),WORKD  CLEAR STORAGE EACH TIME IN                 
         SPACE 1                                                                
         LA    R2,RECDAH           VALIDATE DISK ADDRESS                        
         GOTO1 ANY                                                              
         CLI   5(R2),8             CHECK FOR L'D/A                              
         BL    ERRINV                                                           
         BE    VK20                                                             
         CLI   8+8(R2),C'C'        EXTRA CHARACTER MUST BE 'C'HKFIL             
         BNE   ERRINV                                                           
         MVC   SYSDIR,=CL8'CHKDIR' SET FILE OVERRIDES                           
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE 1                                                                
VK20     LA    R3,KEY              R3 = A(KEY)                                  
         USING TLDRD,R3                                                         
         XC    KEY,KEY                                                          
         GOTO1 HEXIN,DMCB,8(R2),TLDRDA,8  INSURE D/A IS VALID HEX.              
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         OI    DMINBTS,X'08'       OK TO READ DELETED                           
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R4,AIO              R4 = A(RECORD)                               
         USING TLRCD,R4                                                         
         MVC   TLDRKEY,TLRCKEY     MOVE ACTIVE KEY TO 'KEY'                     
         SPACE 1                                                                
         BAS   RE,VALREM           VALIDATE REMAINING KEY FIELDS                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES ALL KEY FIELDS EXCEPT DISK ADDRESS             
         SPACE 1                                                                
VALREM   NTR1                                                                   
         BAS   RE,VALLBL           VALIDATE LABEL FIELD                         
         SPACE 1                                                                
         LA    R2,RECCMPH          VALIDATE COMPRESS OPTION                     
         CLI   5(R2),0                                                          
         BE    VREM20                                                           
         CLI   8(R2),C'Y'          VALID INPUT IS 'Y'ES                         
         BE    VREM20                                                           
         CLI   8(R2),C'N'                         'N'O                          
         BE    VREM20                                                           
         CLI   8(R2),C'D'                         'D'SECTS ONLY                 
         BE    VREM20                                                           
         CLI   8(R2),C'B'                         'B'AD DATA ONLY               
         BNE   ERRINV                                                           
         SPACE 1                                                                
VREM20   LA    R2,RECCMTH          VALIDATE COMMENTS ONLY OPTION                
         CLI   5(R2),0                                                          
         BE    VREMX                                                            
         CLI   8(R2),C'Y'          VALID INPUT IS 'Y'ES                         
         BE    VREMX                                                            
         CLI   8(R2),C'N'                         'N'O                          
         BNE   ERRINV                                                           
         SPACE 1                                                                
VREMX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LABEL FIELD                                             
         SPACE 1                                                                
VALLBL   NTR1                                                                   
         LA    R2,RECLBLH          R2 = A(FIELD)                                
         MVI   SEQUENCE,1                                                       
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VLBLX                                                            
         LA    R3,BLOCK            R3 = A(SCAN BLOCK)                           
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
         CLI   SCLEN1,3            ENSURE MINIMUM LENGTH FOR LABEL              
         BL    ERRINV                                                           
         CLC   =C'TA',SCDATA1      LABEL MUST BE FOR ELEMENT                    
         BE    VLBL10                                                           
         CLC   =C'TL',SCDATA1                 OR FOR RECORD                     
         BNE   ERRINV                                                           
         CLI   SCLEN1,4            RECORDS MUST HAVE AT LEAST 4 CHARS           
         BL    ERRINV                                                           
         CLI   4(R1),1             RECORDS CANNOT HAVE SEQUENCE NUMBER          
         BH    ERRINV                                                           
         SPACE 1                                                                
VLBL10   CLI   4(R1),2             MAY HAVE SEQUENCE NUMBER                     
         BL    VLBLX                                                            
         BH    ERRINV                                                           
         MVC   5(1,R2),SCLEN1      SAVE L'LABEL IN FIELD HEADER                 
         SPACE 1                                                                
         LA    R3,SCANNEXT         BUMP TO SEQUENCE NUMBER ENTRY                
         SPACE 1                                                                
         MVC   SEQUENCE,SCDATA1    SAVE CHARACTER                               
         CLI   SCLEN1,1            IF L'SEQUENCE NUMBER IS 1                    
         BNE   *+12                                                             
         CLI   SCDATA1,C'L'        THEN ALLOW 'L'AST                            
         BE    VLBLX                                                            
         TM    SCVAL1,X'80'        ELSE ENSURE NUMERIC                          
         BZ    ERRINV                                                           
         CLI   SCBIN1+3,0          DON'T ALLOW ZERO                             
         BE    ERRINV                                                           
         CLC   SCBIN1+2(2),=H'99'  NOR MORE THAN 99                             
         BH    ERRINV                                                           
         MVC   SEQUENCE,SCBIN1+3   SAVE BINARY SEQUENCE NUMBER                  
         SPACE 1                                                                
VLBLX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'21',RECLHSH),(X'10',999)  CLEAR/VAL SCRN          
         SPACE 1                                                                
         LA    R3,RECLHSH          R3 = A(FIRST FIELD ON SCREEN)                
         USING LINED,R3                                                         
         BAS   RE,SETSTART         SET STARTING POINTS IN PHASE & REC.          
         L     R4,ACURREL          R4 = A(START DATA)                           
         MVC   CURBYTES,0(R4)      SAVE SOME DATA FOR VER. PURPOSES             
         S     R4,AIO                                                           
         STH   R4,CURRCDSP         SAVE DISP. INTO RECORD OF START              
         XC    CURLABEL,CURLABEL   CLEAR CURRENT LABEL                          
         SPACE 1                                                                
DISP10   L     R2,ACURRPOS         R2 = A(LOCATION IN DSECT PHASE)              
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
DISP20   L     R4,ACURREL          SET R4 = A(DATA)                             
         AH    R4,SRFADDSP                                                      
         LA    R1,RECLAST                                                       
         CR    R3,R1               ENSURE NOT PAST END OF SCREEN                
         BNL   DISP80                                                           
         SPACE 1                                                                
         BAS   RE,FILTER           FILTER DISPLAY                               
         BNE   DISP60                                                           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   DISP40                                                           
         GOTO1 HEXOUT,DMCB,SRFADDSP,LINDISP,2,0  DISPLAY DISPLACEMENT           
         SPACE 1                                                                
DISP40   MVC   LINLBL,SRFADLBL     DISPLAY LABEL                                
         SPACE 1                                                                
         OC    CURLABEL,CURLABEL   IF HAVEN'T SAVED A LABEL YET                 
         BNZ   *+10                                                             
         MVC   CURLABEL,LINLBL     SAVE THIS ONE                                
         SPACE 1                                                                
         BAS   RE,DISDEF           DISPLAY DEFINITION                           
         SPACE 1                                                                
         BAS   RE,DISTYPE          DISPLAY TYPE                                 
         SPACE 1                                                                
         CLI   RECCMT,C'Y'         UNLESS COMMENTS ONLY REQUESTED               
         BE    DISP50                                                           
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   *+12                                                             
         BAS   RE,DISDATA          DISPLAY DATA                                 
         B     *+8                                                              
DISP50   BAS   RE,DISCMNT          ELSE DISPLAY COMMENT                         
         SPACE 1                                                                
         LA    R3,LINNEXT          BUMP TO NEXT LINE                            
         SPACE 1                                                                
DISP60   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BAS   RE,SVDATA           SAVE RELEVENT DATA                           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF NOT STARTING NEW DSECT                    
         BNE   DISP20              DISPLAY NEXT FIELD IN DSECT                  
         L     R4,ACURREL                                                       
         MVI   ELCODE,0                                                         
         C     R4,AIO              ELSE IF POINTING TO RECORD                   
         BNE   *+12                                                             
         BAS   RE,GETEL            BUMP TO FIRST ELEMENT                        
         B     *+8                                                              
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT ELEMENT IN RECORD          
         BNE   DISP90                                                           
         ST    R4,ACURREL          SAVE A(BEGINNING OF ELEMENT)                 
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH                              
         SPACE 1                                                                
         BAS   RE,LOOKUP           GET DSECT ENTRY FOR NEW CODE                 
         B     DISP10              LOOP                                         
         SPACE 1                                                                
DISP80   DS    0H                  REACHED END OF SCREEN                        
         XC    RECLBL,RECLBL       CLEAR LABEL                                  
         CLC   SRFADLBL,SPACES     IF CURRENT PHASE RECORD HAS NO LABEL         
         BH    *+12                                                             
         BAS   RE,NEXTSRF          GET NEXT                                     
         B     *-14                                                             
         MVC   RECLBL(8),SRFADLBL  SET START LABEL FOR NEXT TIME                
         OI    RECLBLH+4,X'20'     SET PREVIOUSLY VALIDATED                     
         L     R4,ACURREL          RESTORE A(ELEMENT)                           
         MVC   NXTBYTES,0(R4)      SAVE SOME DATA FOR VER. PURPOSES             
         S     R4,AIO                                                           
         STH   R4,NXTRCDSP         SAVE CURRENT DISP. INTO RECORD               
         B     DISPX               EXIT                                         
         SPACE 1                                                                
DISP90   DS    0H                  REACHED END OF RECORD                        
         XC    RECLBL,RECLBL       CLEAR LABEL                                  
         XC    NXTRCDSP,NXTRCDSP   CLEAR SAVED DISPLACEMENT                     
         SPACE 1                                                                
DISPX    OI    RECLBLH+6,X'80'     TRANSMIT NEW LABEL FIELD                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES                                      
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
         USING LINED,R3            R3 = A(CURRENT DISPLAY LINE)                 
         SPACE 1                                                                
DISDEF   DS    0H                                                               
         MVC   LINDEF(2),=C'DS'    DISPLAY DEFINITION                           
         CLI   SRFADDEF,SRFQDS                                                  
         BE    DDEFX                                                            
         MVC   LINDEF(2),=C'DC'                                                 
         CLI   SRFADDEF,SRFQDC                                                  
         BE    DDEFX                                                            
         MVC   LINDEF(3),=C'EQU'                                                
         CLI   SRFADDEF,SRFQEQU                                                 
         BE    DDEFX                                                            
         MVC   LINDEF(3),=C'ORG'                                                
         CLI   SRFADDEF,SRFQORG                                                 
         BE    DDEFX                                                            
         MVC   LINDEF(5),=C'DSECT'                                              
         CLI   SRFADDEF,SRFQDSCT                                                
         BE    DDEFX                                                            
         MVC   LINDEF(5),=C'???  '                                              
DDEFX    BR    RE                                                               
         SPACE 2                                                                
DISTYPE  DS    0H                                                               
         CLI   SRFADTYL,0          DISPLAY TYPE                                 
         BE    DTYPX                                                            
         ZIC   R1,SRFADTYL                                                      
         BCTR  R1,0                                                             
         CH    R1,=AL2(L'LINTYPE-1)                                             
         BNH   *+8                                                              
         LH    R1,=AL2(L'LINTYPE-1)                                             
         EX    R1,*+6                                                           
DTYPX    BR    RE                                                               
         MVC   LINTYPE(0),SRFADTY                                               
         SPACE 2                                                                
DISCMNT  DS    0H                  DISPLAY COMMENT                              
         ZIC   R1,SRFADTYL         BUMP PAST TYPE                               
         LA    RF,SRFADTY(R1)                                                   
         IC    R1,0(RF)            L'COMMENT                                    
         SH    R1,=H'1'                                                         
         BM    DCMTX                                                            
         CH    R1,=AL2(L'LINRHS-1)                                              
         BNH   *+8                                                              
         LH    R1,=AL2(L'LINRHS-1)                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINRHS(0),1(RF)     COMMENT IS 1 PAST L'COMMENT                  
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF THIS IS DSECT                             
         BNE   *+8                                                              
         OI    LINRHSH+1,X'08'     MAKE HIGH INTENSITY                          
DCMTX    BR    RE                                                               
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
         USING LINED,R3            R3 = A(CURRENT DISPLAY LINE)                 
*                                  R4 = A(DATA)                                 
         SPACE 1                                                                
DISDATA  NTR1                                                                   
         LH    RF,FLDLEN           RF = L'FIELD                                 
         LTR   RF,RF                                                            
         BZ    DDATX               DON'T BOTHER IF LENGTH IS ZERO               
         SPACE 1                                                                
         CLI   SRFADTY,C'C'        TEST FOR CHARACTER                           
         BE    *+14                                                             
         CLC   SRFADTY(2),=C'0C'                                                
         BNE   DDAT20                                                           
         BCTR  RF,0                                                             
         CH    RF,=AL2(L'LINRHS-1)                                              
         BNH   *+8                                                              
         LH    RF,=AL2(L'LINRHS-1)                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)       MOVE DATA TO TEMP AREA                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),SPACES      INSURE IT'S VALID CHARACTER DATA             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R4)                                                    
         BE    *+12                                                             
         AH    RF,=H'1'            IT'S NOT - RESTORE CORRECT LENGTH            
         B     DDAT20              DISPLAY AS HEX DATA                          
         EX    RF,*+8                                                           
         B     DDATX                                                            
         MVC   LINRHS(0),0(R4)     DISPLAY CHARACTER DATA                       
         SPACE 1                                                                
DDAT20   CH    RF,=AL2(L'LINRHS/2) INSURE NOT MORE THEN MAX DISPLAYABLE         
         BNH   *+8                                                              
         LH    RF,=AL2(L'LINRHS/2)                                              
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,(R4),LINRHS,(RF),0  DISPLAY DATA IN HEX.             
         SPACE 1                                                                
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BNE   DDAT30                                                           
         EDIT  (4,0(R4)),(12,LINRHS+9),2,FLOAT=-                                
         B     DDATX                                                            
         SPACE 1                                                                
DDAT30   CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BNE   DDAT40                                                           
         EDIT  (2,0(R4)),(12,LINRHS+9),FLOAT=-                                  
         B     DDATX                                                            
         SPACE 1                                                                
DDAT40   CLI   SRFADTY,C'X'        TEST FOR HEX                                 
         BNE   DDAT50                                                           
         CLC   FLDLEN,=H'6'        6 BYTE FIELD                                 
         BNE   DDAT50                                                           
         CLI   0(R4),0             AND DATA PRESENT                             
         BE    DDAT50                                                           
         CLC   SRFADLBL+4(4),=C'INV ' AND SUFFIX IS INV                         
         BE    *+14                                                             
         CLC   SRFADLBL+5(3),=C'INV '                                           
         BNE   DDAT50                                                           
         MVC   WORK(6),0(R4)       THIS MUST BE INVOICE NUMBER                  
         TM    WORK,X'80'          IF IT'S COMPLEMENTED                         
         BZ    *+10                                                             
         XC    WORK(6),HEXFFS      UNCOMPLEMENT                                 
         GOTO1 TINVCON,DMCB,WORK,LINRHS+15,DATCON  DISPLAY INVOICE NO.          
         SPACE 1                                                                
DDAT50   DS    0H                                                               
         SPACE 1                                                                
DDATX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHANGE A RECORD                                       
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVI   IOOPT,C'Y'          SET I'M DOING WRITE IN CASE NO CHGS          
         SPACE 1                                                                
         CLI   RECCMT,C'Y'         IGNORE CHANGES IF DISP COMMENTS ONLY         
         BE    BLDR90                                                           
         GOTO1 FLDVAL,DMCB,(X'40',RECLHSH),999  GET OUT IF NO CHANGES           
         BE    BLDR90                                                           
         GOTO1 ASAVPTRS,DMCB,SVPTRBLK  ELSE SAVE POINTERS                       
         SPACE 1                                                                
         BAS   RE,SETCURST         SET TO RESTART AT BEG. OF SCREEN             
         SPACE 1                                                                
         LA    R3,RECLHSH          R3 = A(FIRST FIELD ON SCREEN)                
         USING LINED,R3                                                         
         BAS   RE,SETSTART         SET STARTING POINTS IN PHASE & REC.          
         SPACE 1                                                                
BLDR10   L     R2,ACURRPOS         R2 = A(LOCATION IN DSECT PHASE)              
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
BLDR20   BAS   RE,FILTER           FILTER                                       
         BNE   BLDR60                                                           
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   BLDR50                                                           
         CLC   ACURREL,AIO         IF POINTING AT BEGINNING OF RECORD           
         BNE   BLDR30                                                           
         OC    SRFADDSP,SRFADDSP   IGNORE CHANGES TO RECORD CODE                
         BZ    BLDR50                                                           
         CLC   SRFADDSP,LKEY       IGNORE CHANGES TO RECORD LENGTH              
         BE    BLDR50                                                           
         B     *+14                                                             
BLDR30   CLC   SRFADDSP,=H'1'      ELSE IGNORE CHANGES TO EL. LENGTH            
         BE    BLDR50                                                           
         TM    LINRHSH+4,X'20'     TEST IF FIELD CHANGED                        
         BO    BLDR50                                                           
         L     R4,ACURREL          SET R4 = A(LOCATION IN RECORD)               
         AH    R4,SRFADDSP                                                      
         BAS   RE,VALFLD           VALIDATE/CHANGE FIELD                        
         MVI   IOOPT,C'N'          SET FOR GENCON TO UPDATE RECORD              
         SPACE 1                                                                
BLDR50   LA    R3,LINNEXT          BUMP TO NEXT LINE                            
         LA    R1,RECLAST                                                       
         CR    R3,R1               ENSURE NOT PAST END OF SCREEN                
         BNL   BLDR90                                                           
         SPACE 1                                                                
BLDR60   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BAS   RE,SVDATA           SAVE RELEVENT DATA                           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF NOT STARTING NEW DSECT                    
         BNE   BLDR20              DISPLAY NEXT FIELD IN DSECT                  
         L     R4,ACURREL                                                       
         MVI   ELCODE,0                                                         
         C     R4,AIO              ELSE IF POINTING TO RECORD                   
         BNE   *+12                                                             
         BAS   RE,GETEL            BUMP TO FIRST ELEMENT                        
         B     *+8                                                              
         BAS   RE,NEXTEL           ELSE BUMP TO NEXT ELEMENT IN RECORD          
         BNE   BLDR90                                                           
         ST    R4,ACURREL          SAVE A(BEGINNING OF ELEMENT)                 
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH                              
         SPACE 1                                                                
         BAS   RE,LOOKUP           GET DSECT ENTRY FOR NEW CODE                 
         B     BLDR10              LOOP                                         
         SPACE 1                                                                
BLDR90   CLI   IOOPT,C'N'          IF A CHANGE WAS MADE                         
         BNE   BLDRX                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,X'FF'        AND X'FF' ELEMENTS ARE ON RECORD             
         BAS   RE,GETEL                                                         
         BNE   BLDRX                                                            
         GOTO1 REMELEM             DELETE THEM                                  
         SPACE 1                                                                
BLDRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE A DATA FIELD                                 
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(LOCATION IN DSECT PHASE)              
         USING LINED,R3            R3 = A(SCREEN LINE)                          
*                                  R4 = A(LOCATION IN RECORD)                   
VALFLD   NTR1                                                                   
         CLC   SRFADLBL,SPACES     VALIDATE FLDS WITHOUT LABELS AS HEX          
         BE    VFLD40                                                           
         CLI   SRFADTY,C'C'        IF CHARACTER FIELD                           
         BNE   VFLD40                                                           
         CLC   LINRHSH+5(1),FLDLEN+1 INSURE NOT TOO LONG                        
         BH    ERRFLD                                                           
         OC    LINRHS,SPACES                                                    
         LH    R1,FLDLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VFLDX                                                            
         MVC   0(0,R4),LINRHS      MOVE TO RECORD                               
         SPACE 1                                                                
VFLD40   CLI   LINRHSH+5,0         HEX FIELD - IF NOTHING INPUT                 
         BNE   VFLD50                                                           
         LH    RF,FLDLEN           SET FIELD LENGTH                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     VFLDX                                                            
         XC    0(0,R4),0(R4)       CLEAR DATA IN RECORD                         
         SPACE 1                                                                
VFLD50   ZIC   RF,LINRHSH+5        ELSE ENSURE VALID LENGTH                     
         SRA   RF,1                                                             
         CH    RF,FLDLEN                                                        
         BH    ERRFLD                                                           
         IC    RF,LINRHSH+5                                                     
         GOTO1 HEXIN,DMCB,LINRHS,(R4),(RF)  MOVE IT TO RECORD                   
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFLD                                                           
         SPACE 1                                                                
VFLDX    B     XIT                                                              
         EJECT                                                                  
*              SET CURRENT RECORD AND PHASE POINTERS TO SAVED VALUES            
         SPACE 1                                                                
SETCURST DS    0H                                                               
         MVC   RECLBL,CURLABEL     SET TO RESTART AT CURRENT LABEL              
         MVI   RECLBLH+5,L'CURLABEL                                             
         OI    RECLBLH+4,X'20'                                                  
         OI    RECLBLH+6,X'80'                                                  
         MVC   NXTRCDSP,CURRCDSP   SET TO RESTART AT CURRENT REC LOC.           
         MVC   NXTBYTES,CURBYTES                                                
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE SETS STARTING POINTS IN DSECT PHASE AND RECORD           
         SPACE 1                                                                
SETSTART NTR1                                                                   
         L     R4,AIO              SET TO START AT BEGINNING OF RECORD          
         MVC   ELLEN,DATADISP+1    SET L'EL = DATADISP FOR KEY                  
         LA    R2,RECLBLH                                                       
         SPACE 1                                                                
         CLI   5(R2),0             IF NO LABEL FILTER                           
         BNE   *+12                                                             
         BAS   RE,LOOKUP           LOOK UP DSECT FOR KEY                        
         B     SSTX                DONE                                         
         SPACE 1                                                                
         CLC   =C'TL',8(R2)        IF LABEL IS FOR KEY                          
         BNE   SST20                                                            
         BAS   RE,LOOKUP           LOOK UP BASED ON RECORD CODE FIRST           
         L     RF,ACURRPOS         ENSURE LABEL IS CORRECT FOR KEY              
         CLC   8(4,R2),SRFADLBL-SRFADSEC(RF)                                    
         BNE   ERRINV                                                           
         SPACE 1                                                                
SST20    MVI   LABEL,C'Y'          SET LOOKING UP LABEL                         
         BAS   RE,LOOKUP           LOOK IT UP                                   
         MVI   LABEL,0                                                          
         SPACE 1                                                                
         OC    NXTRCDSP,NXTRCDSP   IF WE HAVE SAVED DISPLACEMENT                
         BZ    SST40                                                            
         TM    4(R2),X'20'         AND LABEL FIELD DIDN'T CHANGE                
         BZ    SST40                                                            
         AH    R4,NXTRCDSP         POINT TO SAVED LOCATION                      
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH (MUST BE ELEMENT)            
         CLC   NXTBYTES,0(R4)      VERIFY DATA HASN'T CHANGED                   
         BE    SSTX                                                             
         SPACE 1                                                                
SST40    L     R4,AIO              SET TO START AT BEGINNING OF RECORD          
         MVC   ELLEN,DATADISP+1    SET L'EL = DATADISP FOR KEY                  
         SPACE 1                                                                
         CLC   =C'TL',8(R2)        IF LABEL IS FOR KEY THEN DONE                
         BE    SSTX                                                             
         BAS   RE,GETELCD          ELSE GET ELEMENT CODE BASED ON LABEL         
         ZIC   R3,SEQUENCE         LOOK UP CORRECT ELEMENT                      
         BAS   RE,GETEL            BUMP TO APPROPRIATE ELEMENT                  
         BNE   ERRINV              IF NOT ON RECORD THEN ERROR                  
         B     *+12                                                             
SST50    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   SST60                                                            
         ST    R4,ACURREL          SAVE A(LAST ELEMENT FOUND)                   
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH                              
         BCT   R3,SST50            LOOP BASED ON SEQUENCE NUMBER                
         B     SSTX                                                             
         SPACE 1                                                                
SST60    CLI   SEQUENCE,C'L'       REACHED EOR - IF DON'T WANT 'L'AST           
         BNE   ERRINV              THEN ERROR                                   
         L     R4,ACURREL          ELSE RESTORE A(LAST ELEMENT FOUND)           
         SPACE 1                                                                
SSTX     ST    R4,ACURREL          RETURN STARTING ADDRESS                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP ELEMENT CODE BASED ON DSECT                     
         SPACE 1                                                                
GETELCD  NTR1                                                                   
         L     R2,ADSECT           R2 = A(DSECT ENTRY)                          
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
GETC10   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQEQU    BUMP UNTIL REACH FIRST EQUATE                
         BNE   GETC10                                                           
         SPACE 1                                                                
         GOTO1 HEXIN,DMCB,SRFADTY+2,ELCODE,2  ASSUME IT'S ELEMENT CODE          
         SPACE 1                                                                
         BAS   RE,CURRSRF          RESTORE POINTER TO CURRENT DSECT REC         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP CODE OR LABEL IN DSECT PHASE                    
         SPACE 1                                                                
*                                  IF LABEL = Y THEN USE LABEL FIELD            
*                                  ELSE R4 = A(CODE)                            
LOOKUP   NTR1                                                                   
         CLI   LABEL,C'Y'          IF PROCESSING CODE                           
         BE    LOOK10                                                           
         XC    RECSUB,RECSUB       INIT RECORD SUBCODE                          
         MVI   REC24ON,C'N'                                                     
         GOTO1 HEXOUT,DMCB,(R4),HALF,1,0  CONVERT CODE TO CHARACTER             
         CLI   0(R4),X'24'         IF X'24' RECORD                              
         BNE   LOOK10                                                           
         GOTO1 HEXOUT,DMCB,1(R4),RECSUB,1,0  CONVERT CODE TO CHARACTER          
                                                                                
LOOK10   MVI   OVERLAY,X'70'       LOAD FIRST PHASE                             
         BAS   RE,LOADPHAS                                                      
         L     R2,APHASE           R2 = A(CURRENT POSITION IN PHASE)            
         USING SRFADSEC,R2                                                      
         BAS   RE,FIRSTSRF         BUMP TO FIRST RECORD                         
         SPACE 1                                                                
LOOK20   BAS   RE,SVDATA           SAVE RELEVENT DATA FOR PHASE RECORD          
         SPACE 1                                                                
         CLI   LABEL,C'Y'          IF PROCESSING LABEL                          
         BNE   LOOK40                                                           
         ZIC   R1,RECLBLH+5        THEN MATCH ON IT                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRFADLBL(0),RECLBL                                               
         BNE   LOOK70                                                           
         B     LOOKX                                                            
         SPACE 1                                                                
LOOK40   CLI   SRFADDEF,SRFQEQU    IF THIS IS NOT EQUATE TYPE                   
         BNE   LOOK70              SKIP TO NEXT RECORD                          
                                                                                
         C     R4,AIO              IF POINTING TO BEG. OF RECORD                
         BNE   LOOK50                                                           
                                                                                
         CLC   SRFADLBL(2),=C'TL'  INSURE THIS IS RECORD DSECT                  
         BNE   LOOK70                                                           
         CLI   REC24ON,C'Y'        IF LOOKING FOR X'24' REC SUBCODE             
         BE    LOOK45              YES, DO SO                                   
         CLC   SRFADLBL+4(3),=C'CDQ'                                            
         BNE   LOOK70                                                           
         CLC   SRFADTY+2(2),HALF   MATCH ON CODE                                
         BNE   LOOK70                                                           
         CLI   0(R4),X'24'         IF X'24' RECORD                              
         BNE   LOOK48              NO,CONTINUE                                  
         MVI   REC24ON,C'Y'        LOOKING FOR X'24' REC SUBCODE                
         B     LOOK70              AND CONTINUE LOOKING                         
                                                                                
LOOK45   CLC   SRFADLBL+4(4),=C'SCDQ'   AND SUBCODE                             
         BNE   LOOK70                                                           
         CLC   SRFADTY+2(2),RECSUB      MATCH ON CODE                           
         BE    LOOK48                                                           
         MVI   REC24ON,C'N'                                                     
         B     LOOK70                                                           
                                                                                
LOOK48   MVC   ACURRPOS,ADSECT     SET TO START WITH DSECT                      
         B     LOOKX               FOUND IT - RETURN                            
                                                                                
LOOK50   CLC   SRFADLBL(2),=C'TA'  ELSE INSURE THIS IS ELEMENT DSECT            
         BNE   LOOK70                                                           
         CLC   SRFADLBL+4(3),=C'ELQ'                                            
         BNE   LOOK70                                                           
         CLC   SRFADTY+2(2),HALF   MATCH ON CODE                                
         BNE   LOOK70                                                           
         MVC   ACURRPOS,ADSECT     SET TO START WITH DSECT                      
         B     LOOKX               FOUND IT - RETURN                            
         SPACE 1                                                                
LOOK70   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         B     LOOK20              LOOP                                         
         SPACE 1                                                                
LOOKX    BAS   RE,CURRSRF          INSURE CORRECT PHASE IS LOADED               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PERFORMS DISPLAY FILTERING                               
         SPACE 1                                                                
FILTER   NTR1                                                                   
         CLI   RECCMP,C'D'         IF DSECTS ONLY REQUESTED                     
         BNE   *+12                                                             
         CLI   SRFADDEF,SRFQDSCT   DISPLAY DSECTS ONLY                          
         BNE   NO                                                               
                                                                                
         CLI   RECCMP,C'Y'         IF COMPRESSION REQUESTED                     
         BNE   FILT20                                                           
         CLI   SRFADDEF,SRFQEQU    SKIP EQUATES                                 
         BE    NO                                                               
         CLC   SRFADLBL,SPACES     SKIP IF NO LABEL                             
         BE    NO                                                               
                                                                                
FILT20   CLI   SRFADDEF,SRFQDSCT   ALWAYS DISPLAY DSECTS                        
         BE    FILTX                                                            
         CLI   SRFADDEF,SRFQDS     IF NOT DS                                    
         BE    FILT30                                                           
         CLI   SRFADDEF,SRFQDC     OR DC                                        
         BE    FILT30                                                           
         CLI   RECCMP,C'B'         THEN IF TEST BAD DATA REQUESTED              
         BE    NO                  SKIP                                         
         B     FILTX               ELSE GO DISPLAY LABEL                        
                                                                                
FILT30   BAS   RE,GETFLEN          GET L'FIELD - RETURNS IN FLDLEN              
                                                                                
         CLI   RECCMP,C'Y'         IF COMPRESSION REQUESTED                     
         BNE   *+14                                                             
         OC    FLDLEN,FLDLEN       SKIP IF THERE'S NO FIELD LENGTH              
         BZ    NO                                                               
                                                                                
         CLI   RECCMP,C'B'         TEST BAD DATA REQUESTED                      
         BNE   FILTX                                                            
         CLC   SRFADLBL,SPACES     IF NO LABEL DEFINED                          
         BNE   NO                                                               
         OC    FLDLEN,FLDLEN       AND THERE'S A FIELD LENGTH                   
         BZ    NO                                                               
         LH    R1,FLDLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       THEN SKIP IF NO DATA PRESENT                 
         BZ    NO                                                               
                                                                                
FILTX    B     YES                                                              
         EJECT                                                                  
*              ROUTINE RETURNS LENGTH OF CURRENT FIELD                          
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT FIELD ENTRY)                  
GETFLEN  NTR1                                                                   
         MVC   FLDLEN,=H'4'                                                     
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BE    GETFX                                                            
         MVC   FLDLEN,=H'2'                                                     
         CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BE    GETFX                                                            
         SPACE 1                                                                
         XC    FLDLEN,FLDLEN       RETURN LENGTH IN FLDLEN                      
         LH    R3,SRFADDSP         SAVE DISP. TO CURRENT FIELD                  
         SPACE 1                                                                
         BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   GETF20                                                           
         LH    R1,SRFADDSP         DISP. TO NEXT FIELD                          
         SR    R1,R3               LESS DISP. TO CURRENT FIELD                  
         STH   R1,FLDLEN           IS L'CURRENT FIELD                           
         B     GETFX                                                            
         SPACE 1                                                                
GETF20   BAS   RE,CURRSRF          RESTORE A(CURRENT ENTRY)                     
         ZIC   R1,SRFADTYL         ATTEMPT TO EXTRACT LENGTH FROM TYPE          
         SH    R1,=H'3'                                                         
         BM    GETF40                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SRFADTY+2   ASSUME TYPE IS CL???                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),=10C'0'     INSURE IT'S NUMERIC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SRFADTY+2                                                
         BNE   GETF40                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RF,DUB                                                           
         STH   RF,FLDLEN           SAVE IT                                      
         B     GETFX                                                            
         SPACE 1                                                                
GETF40   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE AGAIN           
         CLI   SRFADDEF,SRFQDSCT   IF IT'S A DSECT                              
         BE    GETF50                                                           
         CLI   SRFADDEF,SRFQEQU    OR IT'S AN EQUATE                            
         BNE   GETFX                                                            
         CLI   SRFADTYL,7          AND TYPE LENGTH IS 7 L'*-TA??D               
         BNE   GETFX                                                            
         MVC   WORK(2),=C'*-'      BUILD EOL TYPE FOR THIS EL.                  
         MVC   WORK+2(4),SRFADLBL                                               
         MVI   WORK+6,C'D'         NOW HAVE '*-TA??D'                           
         CLC   SRFADTY(7),WORK     IF IT MATCHES THEN REACHED END OF EL         
         BNE   GETFX                                                            
         SPACE 1                                                                
GETF50   BAS   RE,CURRSRF          RESTORE A(CURRENT ENTRY)                     
         ZIC   R1,ELLEN            L'ELEMENT                                    
         SH    R1,SRFADDSP         - DISP. TO THIS FIELD                        
         BM    *+8                                                              
         STH   R1,FLDLEN           IS L'THIS FIELD                              
         SPACE 1                                                                
GETFX    BAS   RE,CURRSRF          INSURE WE'RE RESTORED CURRENT PTR.           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS NEXT DSECT PHASE RECORD                             
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT ENTRY)                        
FIRSTSRF NTR1                                                                   
         B     NEXT10                                                           
         SPACE 1                                                                
NEXTSRF  NTR1                                                                   
         ZIC   R1,SRFADTYL                                                      
         LA    R2,SRFADTY(R1)      BUMP PAST L'TYPE, TYPE                       
         IC    R1,0(R2)                                                         
         LA    R2,1(R1,R2)         BUMP PAST L'COMMENT, COMMENT                 
         SPACE 1                                                                
NEXT10   CLC   SRFADDSP,=AL2(SRFQSPCL) IF THIS ISN'T SPECIAL RECORD             
         BNE   NEXTX                   THEN DONE                                
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQEOF     IF REACHED END OF FILE                       
         BE    ERRLABEL            THEN GIVE ERROR - INVALID LABEL              
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQMEM     IF START OF MEMBER                           
         BNE   *+12                                                             
         LA    R2,SRFMEMB+L'SRFMEMB  BUMP TO NEXT RECORD                        
         B     NEXT10                                                           
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQEOP     MUST BE END OF PHASE                         
         BE    *+6                                                              
         DC    H'0'                UNRECOGNIZED SPECIAL RECORD TYPE             
         ZIC   R1,OVERLAY                                                       
         LA    R1,1(R1)            BUMP OVERLAY NUMBER                          
         STC   R1,OVERLAY                                                       
         SPACE 1                                                                
         BAS   RE,LOADPHAS         LOAD NEXT DSECT PHASE                        
         L     R2,APHASE                                                        
         B     NEXT10              CONTINUE                                     
         SPACE 1                                                                
NEXTX    B     XITR2                                                            
         EJECT                                                                  
*              ROUTINE LOADS A DSECT PHASE                                      
         SPACE 1                                                                
*                                  OVERLAY = OVERLAY NUMBER                     
LOADPHAS NTR1                                                                   
         L     R1,=V(DUMMY)        SET A(WHERE TO LOAD DSECT PHASES)            
         A     R1,RELO                                                          
         ST    R1,APHASE                                                        
         MVC   DMCB+4(3),=X'D9015E'  SET TO LOAD T15E PHASE                     
         MVC   DMCB+7(1),OVERLAY     SET OVERLAY                                
         GOTO1 CALLOV,DMCB,APHASE,,0                                            
         OC    9(3,R1),9(R1)                                                    
         BZ    ERRPHASE                                                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE RESTORES SAVED POSITION IN DSECT PHASE                   
         SPACE 1                                                                
CURRSRF  NTR1                                                                   
         CLC   OVERLAY,ACURRPOS    TEST CORRECT PHASE IS LOADED                 
         BE    *+14                                                             
         MVC   OVERLAY,ACURRPOS    NO, SO LOAD IT                               
         BAS   RE,LOADPHAS                                                      
         SPACE 1                                                                
         L     R2,ACURRPOS         RESTORE A(DSECT RECORD)                      
         B     XITR2                                                            
         SPACE 3                                                                
*              ROUTINE SAVES VARIOUS DATA FOR A DSECT PHASE RECORD              
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
SVDATA   NTR1                                                                   
         ST    R2,ACURRPOS         SAVE A(CURRENT POSITION)                     
         MVC   ACURRPOS(1),OVERLAY SAVE CURRENT OVERLAY NUMBER IN HOB           
                                                                                
         CLI   SRFADDEF,SRFQDSCT   IF THIS IS DSECT RECORD                      
         BNE   SVDATAX                                                          
         MVC   ADSECT,ACURRPOS     SAVE ITS PHASE/ADDRESS                       
         MVI   REC24ON,C'N'        NEW DSECT, RESET X'24' REC MODE              
SVDATAX  B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
         USING LINED,R3                                                         
ERRFLD   LA    R2,LINRHSH                                                       
         B     ERRINV                                                           
ERRLABEL LA    R2,RECLBLH                                                       
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRPHASE MVI   ERROR,NOPHASE       PHASE NOT FOUND                              
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
MORE     MVI   MYMSGNO1,36         PARTIAL RECORD DISPLAYED                     
         OI    GENSTAT2,USGETTXT                                                
         L     R2,EFHACT                                                        
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
RELO     DS    F                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
CHKDKEYS DC    AL1(TLCKCDQ)        KEYS ON CHKDIR                               
         DC    AL1(TLCKHCDQ)                                                    
         DC    AL1(TLCKDCDQ)                                                    
         DC    AL1(TLCKLCDQ)                                                    
         DC    AL1(TLCKCCDQ)                                                    
         DC    AL1(TLCKECDQ)                                                    
         DC    AL1(TLCKYCDQ)                                                    
         DC    AL1(TLCKBCDQ)                                                    
         DC    AL1(TLDTCDQ)                                                     
         DC    AL1(TLW2CDQ)                                                     
         DC    AL1(0)                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SRFADDSECT                                                     
         EJECT                                                                  
*              DSECT TO COVER APPLICATION SAVED STORAGE                         
         SPACE 1                                                                
TWAHOLED DSECT                                                                  
SVPTRBLK DS    CL(L'TWAHOLE/2)     SAVED POINTER BLOCK                          
PTRBLK   DS    CL(L'TWAHOLE/2)     UPDATED POINTER BLOCK                        
         SPACE 3                                                                
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
LINED    DSECT                                                                  
LINLHSH  DS    CL8                                                              
LINLHS   DS    CL37                LHS                                          
         ORG   LINLHS                                                           
LINDISP  DS    CL4                 DISPLACEMENT                                 
         DS    CL1                                                              
LINLBL   DS    CL8                 LABEL                                        
         DS    CL1                                                              
LINDEF   DS    CL3                 DEFINITION                                   
         DS    CL1                                                              
LINTYPE  DS    CL19                TYPE                                         
         ORG                                                                    
LINRHSH  DS    CL8                                                              
LINRHS   DS    CL40                RHS                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD1D                                                       
         EJECT                                                                  
*              LOCAL SAVED STORAGE AT BOTTOM OF TWA0                            
         SPACE 1                                                                
WORKD    EQU   *                                                                
APHASE   DS    A                   A(DSECT PHASE)                               
ADSECT   DS    A                   A(CURRENT DSECT IN PHASE)                    
ACURRPOS DS    A                   A(CURRENT POSITION IN DSECT PHASE)           
ACURREL  DS    A                   A(CURRENT RECORD/ELEMENT)                    
FLDLEN   DS    H                   L'CURRENT FIELD                              
ELLEN    DS    XL1                 L'CURRENT ELEMENT                            
LABEL    DS    CL1                 LABEL INDICATOR FOR LOOKUP                   
SEQUENCE DS    XL1                 ELEMENT SEQUENCE NUMBER                      
WORKLNQ  EQU   *-WORKD                                                          
CURRCDSP DS    H                   DISP. TO CURR SAVED LOCATION IN REC          
NXTRCDSP DS    H                   DISP. TO NEXT SAVED LOCATION IN REC          
CURBYTES DS    XL8                 CURRENT VERIFICATION BYTES                   
NXTBYTES DS    XL8                 NEXT VERIFICATION BYTES                      
CURLABEL DS    CL8                 FIRST LABEL ON CURRENT SCREEN                
RECSUB   DS    H                   RECORD SUBCODE                               
REC24ON  DS    C                   X'24' RECORD MODE ON                         
         SPACE 3                                                                
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEND1   08/01/12'                                      
         END                                                                    
