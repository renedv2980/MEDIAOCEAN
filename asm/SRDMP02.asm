*          DATA SET SRDMP02    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T15D02A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T15D02 - $DUMP DSECT DISPLAY'                                   
T15D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T15D02,RR=R3                                                   
         L     RC,0(R1)            RC = A(COMMON STORAGE)                       
         USING WRKD,RC                                                          
         L     R9,4(R1)            R9 = A(SYSTEM STORAGE)                       
         USING SYSFACD,R9                                                       
         L     RA,8(R1)            RA = A(TWA)                                  
         USING T15DFFD,RA                                                       
         LH    R8,=Y(SORTBLCK-WRKD)                                             
         AR    R8,RC                                                            
         USING SORTBLCK,R8                                                      
         MVC   ASTART,12(R1)       SAVE A(DATA TO DISPLAY)                      
         ST    R3,RELO                                                          
         EJECT                                                                  
*              MAIN CONTROL                                                     
         SPACE 2                                                                
         BAS   RE,INITIAL          INITIALIZE                                   
         SPACE 1                                                                
         BAS   RE,VALARGS          VALIDATE ARGUMENTS                           
         SPACE 1                                                                
         BAS   RE,SETSTART         SET STARTING POINT IN DSECT PHASE            
         SPACE 1                                                                
         BAS   RE,DISPLAY          DISPLAY THE DATA                             
         BNE   *+14                                                             
         MVC   ACURRPOS,ADSECT     FINISHED - SET TO START AT DSECT             
         BAS   RE,CURRSRF                                                       
         SPACE 1                                                                
         BAS   RE,SETNEXT          SET ARGUMENTS FOR NEXT TIME IN               
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INITIAL  NTR1                                                                   
         CLC   =C'ND,L',SRVID+1    'FE' SCREEN USED?                            
         BNE   IN20                NOT 'FE' SCREEN                              
         MVC   REGHOLD(L'SRVP1),SRVP4   COPY THE PARAMETERS                     
         MVC   REGHOLD+16(L'SRVP1),SRVP1                                        
         MVC   REGHOLD+32(L'SRVP1),SRVP2                                        
         MVC   REGHOLD+48(L'SRVP1),SRVP3                                        
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(5),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,5          FOR LENGTH OF 3                              
         MVC   SRVP4,REGHOLD       COPY BACK PARAMETERS                         
         MVC   SRVP1,REGHOLD+16                                                 
         MVC   SRVP2,REGHOLD+32                                                 
         MVC   SRVP3,REGHOLD+48                                                 
         OI    SRVP4H+6,X'80'                                                   
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP2H+6,X'80'                                                   
         OI    SRVP3H+6,X'80'                                                   
*                                                                               
IN20     XC    SRVMSG,SRVMSG       CLEAR MESSAGE AREA                           
         OI    SRVMSGH+6,X'80'                                                  
         NI    SRVP1H+6,X'BF'      ENSURE CURSOR NOT ON OPTION 1                
*                                                                               
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVI   RECOREL,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ARGUMENTS                                               
         SPACE 1                                                                
VALARGS  NTR1                                                                   
         LA    R2,SRVP4H           R2 = A(FIELD)                                
         LA    R3,BLOCK            R3 = A(SCAN BLOCK)                           
         USING SCAND,R3                                                         
         GOTO1 VSCANNER,DMCB,(R2),(X'80',(R3))                                  
         CLI   4(R1),3             MUST HAVE DUMP#,SYS,LABEL                    
         BL    ERRARGS                                                          
         CLI   4(R1),4             MAY HAVE 'R'ECORD OR 'E'LEMENT IND.          
         BH    ERRARGS                                                          
         LA    R3,SCANNEXT         BUMP PAST DUMP# TO SYSTEM                    
         SPACE 1                                                                
         MVI   SYSTEM,0            DEFAULT SYSTEM IS FACPAK                     
         MVI   SYSCHAR,C' '                                                     
         CLI   SCLEN1,0            IF SOMETHING INPUT                           
         BE    *+12                                                             
         BAS   RE,VALSYS           VALIDATE SYSTEM                              
         BNE   ERRARGS                                                          
         SPACE 1                                                                
         LA    R3,SCANNEXT         BUMP PAST SYSTEM TO LABEL                    
         SPACE 1                                                                
         CLI   SCLEN1,8            ENSURE NOT MORE THAN MAX. LENGTH             
         BH    ERRARGS                                                          
         CLI   SCLEN1,0            TEST AUTO-LOOKUP REQUESTED                   
         BE    VARG40                                                           
         ZIC   R0,SCLEN1                                                        
         LA    RF,SCDATA1+7        ELSE CLEAR TRAILING DOTS                     
         CLI   0(RF),C'.'                                                       
         BH    *+14                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,*-14                                                          
VARG40   MVC   LABEL,SCDATA1       SAVE LABEL                                   
         MVC   LLABEL,SCLEN1       AND L'LABEL                                  
         MVC   DSPTOLBL,SCDISP1    AND DISPLACEMENT TO LABEL                    
         SPACE 1                                                                
         CLI   SYSCHAR,C'T'        IF SYSTEM IS TALENT                          
         BNE   VARG60                                                           
         CLC   =C'TL',LABEL        TEST FOR RECORD LABEL                        
         BNE   *+8                                                              
         MVI   RECOREL,C'R'        PRESET INDICATOR                             
         CLC   =C'TA',LABEL        ELSE TEST FOR ELEMENT LABEL                  
         BNE   *+8                                                              
         MVI   RECOREL,C'E'        PRESET INDICATOR                             
         SPACE 1                                                                
VARG60   CLI   4(R1),4             MAY HAVE 'R'ECORD OR 'E'LEMENT IND.          
         BNE   VARGX                                                            
         LA    R3,SCANNEXT         BUMP PAST SYSTEM TO INDICATOR                
         CLI   SCDATA1,C'R'        TEST FOR 'R'ECORD                            
         BE    *+12                                                             
         CLI   SCDATA1,C'E'        TEST FOR 'E'LEMENT                           
         BNE   ERRARGS                                                          
         MVC   RECOREL,SCDATA1                                                  
VARGX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SYSTEM                                                  
         SPACE 1                                                                
         USING SCAND,R3            R3 = A(SCAN BLOCK ENTRY FOR SYSTEM)          
VALSYS   NTR1                                                                   
         L     R4,VCOMFACS         SET TO GO TO GETFACT FOR A(SYSLIST)          
         USING COMFACSD,R4                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R4,0(R1)            R4 = A(FACTSD)                               
         USING FACTSD,R4                                                        
         MVC   WORK(1),SCDATA1     SAVE 1ST BYTE OF INPUT                       
         SPACE 1                                                                
         L     R3,FASYSLST         R3 = A(SYSLIST)                              
         LH    R4,0(R3)            SET FOR LOOP                                 
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SYSLSTD,R3                                                       
VSYS20   CLC   SYSLRPLT,WORK       SEARCH SYSLIST FOR SYSTEM NAME               
         BE    *+12                                                             
         BXLE  R3,R4,VSYS20                                                     
         B     NO                                                               
         SPACE 1                                                                
         MVC   SYSCHAR,SYSLRPLT    SAVE CHAR. SYSTEM CODE                       
         MVC   SYSTEM,SYSLNUM      SAVE BINARY SYSTEM NUMBER                    
         SPACE 1                                                                
         L     R4,VCOMFACS         SET TO GO TO HELLO FOR A(HELENFIL)           
         USING COMFACSD,R4                                                      
         GOTO1 CHELLO,DMCB,C'AFIL'                                              
         L     R4,0(R1)                                                         
         USING HELEND,R4                                                        
VSYS40   CLI   HELEND,X'FF'                                                     
         BE    VSYSX                                                            
         CLC   SYSCHAR,HELNAME     MATCH ON SYSTEM                              
         BE    *+12                                                             
         LA    R4,HELNEXT                                                       
         B     VSYS40                                                           
         MVC   DATADISP+1(1),HELEDIS  SAVE DISP. TO 1ST ELEMENT                 
         SPACE 1                                                                
VSYSX    B     YES                                                              
         EJECT                                                                  
*              ROUTINE SETS STARTING POINT IN DSECT PHASE                       
         SPACE 1                                                                
SETSTART NTR1                                                                   
         L     R4,ASTART           POINT TO DATA                                
         MVI   ELLEN,0             INITIALIZE REC/ELEMENT LENGTH                
         SPACE 1                                                                
         CLI   RECOREL,C'R'        IF POINTING TO RECORD                        
         BNE   *+10                                                             
         MVC   ELLEN,DATADISP+1    SET DUMMY LENGTH                             
         SPACE 1                                                                
         CLI   RECOREL,C'E'        ELSE IF POINTING TO ELEMENT                  
         BNE   *+10                                                             
         MVC   ELLEN,1(R4)         SET ELEMENT LENGTH                           
         SPACE 1                                                                
         BAS   RE,LOOKUP           LOOK UP STARTING LABEL                       
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   DMPL1(L'HEAD1),HEAD1  DISPLAY HEADING                            
         OI    DMPL1H+1,X'08'        MAKE HIGH INTENSITY                        
         SPACE 1                                                                
         LA    R3,DMPL2H           R3 = A(FIRST FIELD ON SCREEN)                
         USING LINED,R3                                                         
         SPACE 1                                                                
DISP10   L     R2,ACURRPOS         R2 = A(LOCATION IN DSECT PHASE)              
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
DISP20   L     R4,ASTART           SET R4 = A(DATA)                             
         AH    R4,SRFADDSP                                                      
         LA    R1,DMPLAST                                                       
         CR    R3,R1               ENSURE NOT PAST END OF SCREEN                
         BNL   NO                  RETURN CC NE - MORE TO COME                  
         SPACE 1                                                                
         BAS   RE,SVDATA           SAVE RELEVENT DATA                           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   DISP40                                                           
         GOTO1 VHEXOUT,DMCB,SRFADDSP,LINDISP,2,0 DISPLAY DISPLACEMENT           
         SPACE 1                                                                
DISP40   MVC   LINLBL,SRFADLBL     DISPLAY LABEL                                
         SPACE 1                                                                
         BAS   RE,DISDEF           DISPLAY DEFINITION                           
         SPACE 1                                                                
         BAS   RE,DISTYPE          DISPLAY TYPE                                 
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   *+12                                                             
         BAS   RE,DISDATA          DISPLAY DATA                                 
         B     *+8                                                              
         BAS   RE,DISCMNT          ELSE DISPLAY COMMENT                         
         SPACE 1                                                                
         LA    R3,LINNEXT          BUMP TO NEXT LINE                            
         SPACE 1                                                                
DISP60   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BNE   DISPX                                                            
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF NOT STARTING NEW DSECT                    
         BNE   DISP20              DISPLAY NEXT FIELD IN DSECT                  
         SPACE 1                                                                
         CLI   RECOREL,0           IF NOT POINTING TO REC OR EL, DONE           
         BE    DISPX                                                            
         L     R4,ASTART           ELSE POINT TO START OF DATA                  
         ZIC   R1,ELLEN            BUMP TO FIRST/NEXT ELEMENT                   
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    DISPX                                                            
         ST    R4,ASTART           SAVE A(BEGINNING OF ELEMENT)                 
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH                              
         MVI   RECOREL,C'E'        SET NOW POINTING TO ELEMENT                  
         SPACE 1                                                                
         A     R1,START            BUMP STARTING ADDRESS FOR DISPLAY            
         ST    R1,START                                                         
         SPACE 1                                                                
         BAS   RE,LOOKUP           GET DSECT ENTRY FOR NEW CODE                 
         B     DISP10              LOOP                                         
         SPACE 1                                                                
DISPX    B     YES                 RETURN CC EQ - DONE                          
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
         CH    R1,=AL2(L'LINDATA-1)                                             
         BNH   *+8                                                              
         LH    R1,=AL2(L'LINDATA-1)                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINDATA(0),1(RF)    COMMENT IS 1 PAST L'COMMENT                  
DCMTX    BR    RE                                                               
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES, CONT'D.                             
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
         USING LINED,R3            R3 = A(CURRENT DISPLAY LINE)                 
*                                  R4 = A(DATA)                                 
DISDATA  NTR1                                                                   
         BAS   RE,GETFLEN          GET L'FIELD                                  
         LH    RF,FIELDLEN         RF = L'FIELD                                 
         LTR   RF,RF                                                            
         BZ    DDATX               DON'T BOTHER IF LENGTH IS ZERO               
         SPACE 1                                                                
         CLI   SRFADTY,C'C'        TEST FOR CHARACTER                           
         BE    *+14                                                             
         CLC   SRFADTY(2),=C'0C'                                                
         BNE   DDAT20                                                           
         BCTR  RF,0                                                             
         CH    RF,=AL2(L'LINDATA-1) ENSURE LE MAX LENGTH OF FIELD               
         BNH   *+8                                                              
         LH    RF,=AL2(L'LINDATA-1)                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R4),DISPCHRS    TEST IF ALL DISPLAYABLE CHARS.               
         L     R2,ACURRPOS         (TRT CREAMS R2)                              
         BE    *+12                                                             
         AH    RF,=H'1'            IT'S NOT - RESTORE CORRECT LENGTH            
         B     DDAT20              DISPLAY AS HEX DATA                          
         EX    RF,*+8                                                           
         B     DDATX                                                            
         MVC   LINDATA(0),0(R4)    DISPLAY CHARACTER DATA                       
         SPACE 1                                                                
DDAT20   CH    RF,=AL2(L'LINDATA/2) ENSURE NOT GT MAX DISPLAYABLE               
         BNH   *+8                                                              
         LH    RF,=AL2(L'LINDATA/2)                                             
         SPACE 1                                                                
         GOTO1 VHEXOUT,DMCB,(R4),LINDATA,(RF),0 DISPLAY DATA IN HEX.            
         SPACE 1                                                                
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BNE   DDAT30                                                           
         LA    RE,LINDATA+9                                                     
         CLI   SYSTEM,0            IF SYSTEM IS FACPAK EDIT W/O DEC             
         BNE   DDAT25                                                           
         EDIT  (4,0(R4)),(12,(RE)),FLOAT=-                                      
         B     DDATX                                                            
DDAT25   EDIT  (4,0(R4)),(12,(RE)),2,FLOAT=-                                    
         B     DDATX                                                            
         SPACE 1                                                                
DDAT30   CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BNE   DDATX                                                            
         LA    RE,LINDATA+9                                                     
         EDIT  (2,0(R4)),(12,(RE)),FLOAT=-                                      
         SPACE 1                                                                
DDATX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP CODE OR LABEL IN DSECT PHASE                    
         SPACE 1                                                                
*                                  IF LLABEL NE 0 THEN USE LABEL FIELD          
*                                  ELSE R4 = A(CODE)                            
LOOKUP   NTR1                                                                   
         CLI   LLABEL,0            IF PROCESSING CODE                           
         BNE   LOOK10                                                           
         GOTO1 VHEXOUT,DMCB,(R4),HALF,1,0 CONVERT CODE TO CHARACTER             
         SPACE 1                                                                
LOOK10   ZIC   R1,SYSTEM           LOAD FIRST PHASE FOR SYSTEM                  
         SLA   R1,4                                                             
         BNZ   *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,OVERLAY                                                       
         BAS   RE,LOADPHAS                                                      
         L     R2,APHASE           R2 = A(CURRENT POSITION IN PHASE)            
         USING SRFADSEC,R2                                                      
         BAS   RE,FIRSTSRF         BUMP TO FIRST RECORD                         
         SPACE 1                                                                
LOOK20   BAS   RE,SVDATA           SAVE RELEVENT DATA FOR PHASE RECORD          
         SPACE 1                                                                
         CLI   LLABEL,0            IF PROCESSING LABEL                          
         BE    LOOK40                                                           
         ZIC   R1,LLABEL           MATCH ON L'INPUT                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRFADLBL(0),LABEL                                                
         BNE   LOOK70                                                           
         B     LOOKX                                                            
         SPACE 1                                                                
LOOK40   TM    LBLSTAT,LBLQREC     IF THIS IS EQUATE FOR A RECORD               
         BZ    *+12                                                             
         CLI   RECOREL,C'R'        AND WE'RE POINTING TO A RECORD               
         BE    LOOK50                                                           
         TM    LBLSTAT,LBLQEL      OR IF THIS IS EQUATE FOR AN ELEMENT          
         BZ    LOOK70                                                           
         CLI   RECOREL,C'E'        AND WE'RE POINTING TO AN ELEMENT             
         BNE   LOOK70                                                           
LOOK50   CLC   SRFADTY+2(2),HALF   MATCH ON CODE                                
         BNE   LOOK70                                                           
         MVC   ACURRPOS,ADSECT     SET TO START WITH DSECT                      
         B     LOOKX               FOUND IT - RETURN                            
         SPACE 1                                                                
LOOK70   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BNE   ERRLABEL            ERROR - INVALID LABEL                        
         B     LOOK20              LOOP                                         
         SPACE 1                                                                
LOOKX    BAS   RE,CURRSRF          INSURE CORRECT PHASE IS LOADED               
         MVI   LLABEL,0            CLEAR LABEL LENGTH FOR NEXT TIME IN          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RETURNS LENGTH OF CURRENT FIELD                          
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT FIELD ENTRY)                  
GETFLEN  NTR1                                                                   
         CLI   SRFADTYL,1          IF TYPE LENGTH IS ONE                        
         BNE   GETF10                                                           
         MVC   FIELDLEN,=H'4'                                                   
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BE    GETFX                                                            
         CLI   SRFADTY,C'A'        TEST FOR ADDRESS                             
         BE    GETFX                                                            
         CLI   SRFADTY,C'V'        TEST FOR EXTERNAL ADDRESS                    
         BE    GETFX                                                            
         MVC   FIELDLEN,=H'2'                                                   
         CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BE    GETFX                                                            
         MVC   FIELDLEN,=H'1'                                                   
         CLI   SRFADTY,C'C'        TEST FOR CHAR                                
         BE    GETFX                                                            
         CLI   SRFADTY,C'X'        TEST FOR HEX                                 
         BE    GETFX                                                            
         SPACE 1                                                                
GETF10   XC    FIELDLEN,FIELDLEN   RETURN LENGTH IN FIELDLEN                    
         LH    R3,SRFADDSP         SAVE DISP. TO CURRENT FIELD                  
         SPACE 1                                                                
         BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BNE   GETF20                                                           
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   GETF20                                                           
         LH    R1,SRFADDSP         DISP. TO NEXT FIELD                          
         SR    R1,R3               LESS DISP. TO CURRENT FIELD                  
         STH   R1,FIELDLEN         IS L'CURRENT FIELD                           
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
         STH   RF,FIELDLEN         SAVE IT                                      
         B     GETFX                                                            
         SPACE 1                                                                
GETF40   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE AGAIN           
         BNE   GETFX                                                            
         CLI   SRFADDEF,SRFQDSCT   IF IT'S A DSECT                              
         BE    GETF50                                                           
         CLI   SRFADDEF,SRFQEQU    OR IT'S AN EQUATE                            
         BNE   GETFX                                                            
         CLI   SRFADTYL,7          AND TYPE LENGTH IS 7 L'*-XX??D               
         BNE   GETFX                                                            
         MVC   WORK(2),=C'*-'      BUILD EOL TYPE FOR THIS EL.                  
         MVC   WORK+2(4),SRFADLBL                                               
         MVI   WORK+6,C'D'         NOW HAVE '*-XX??D'                           
         CLC   SRFADTY(7),WORK     IF IT MATCHES THEN REACHED END OF EL         
         BNE   GETFX                                                            
         SPACE 1                                                                
GETF50   BAS   RE,CURRSRF          RESTORE A(CURRENT ENTRY)                     
         ZIC   R1,ELLEN            L'ELEMENT                                    
         SH    R1,SRFADDSP         - DISP. TO THIS FIELD                        
         BM    *+8                                                              
         STH   R1,FIELDLEN         IS L'THIS FIELD                              
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
         BE    NEXTNO              RETURN CC NE                                 
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
NEXTX    B     YESR2               RETURN R2, CC EQ                             
         SPACE 1                                                                
NEXTNO   B     NO                  RETURN CC NE                                 
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
         GOTO1 VCALLOV,DMCB,APHASE,,0                                           
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
         EJECT                                                                  
*              ROUTINE SAVES VARIOUS DATA FOR A DSECT PHASE RECORD              
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
SVDATA   NTR1                                                                   
         ST    R2,ACURRPOS         SAVE A(CURRENT POSITION)                     
         MVC   ACURRPOS(1),OVERLAY SAVE CURRENT OVERLAY NUMBER IN HOB           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF THIS IS DSECT RECORD                      
         BNE   *+10                                                             
         MVC   ADSECT,ACURRPOS     SAVE ITS PHASE/ADDRESS                       
         SPACE 1                                                                
         NI    LBLSTAT,X'FF'-LBLQREC-LBLQEL  PRE-CLEAR STATUS BITS              
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQEQU    TEST THIS IS EQUATE TYPE                     
         BNE   SVDX                                                             
         CLI   SYSCHAR,C'A'        FOR ACC                                      
         BNE   *+14                                                             
         CLC   SRFADLBL+4(4),=C'TYPQ'  RECORD EQUATES END WITH 'TYPQ'           
         BE    SVD20                                                            
         CLI   SYSCHAR,C'T'        FOR TALENT                                   
         BNE   SVD30                                                            
         CLC   SRFADLBL(2),=C'TL'  RECORD EQUATES START WITH 'TL'               
         BNE   SVD30                                                            
         CLC   SRFADLBL+4(3),=C'CDQ'  AND END WITH 'CDQ'                        
         BNE   SVD30                                                            
SVD20    OI    LBLSTAT,LBLQREC     SET THIS IS EQUATE FOR RECORD                
         B     SVDX                                                             
         SPACE 1                                                                
SVD30    CLC   SRFADLBL+3(3),=C'ELQ'  IF LABEL ENDS WITH 'ELQ'                  
         BE    *+14                                                             
         CLC   SRFADLBL+4(3),=C'ELQ'                                            
         BNE   SVDX                                                             
         OI    LBLSTAT,LBLQEL      SET THIS IS EQUATE FOR ELEMENT               
         SPACE 1                                                                
SVDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS UP ARGUMENTS FOR NEXT TIME IN                       
         SPACE 1                                                                
SETNEXT  NTR1                                                                   
         L     R2,ACURRPOS         R2 = A(LAST LABEL DISPLAYED)                 
         USING SRFADSEC,R2                                                      
         C     R2,ADSECT           UNLESS POINTING TO DSECT                     
         BE    *+12                                                             
         BAS   RE,NEXTSRF          GET NEXT                                     
         BNE   *+14                                                             
         CLC   SRFADLBL,SPACES     IF DSECT PHASE RECORD HAS NO LABEL           
         BE    *-14                KEEP ON LOOKING                              
         SPACE 1                                                                
         ZIC   R1,DSPTOLBL                                                      
         LA    R3,SRVP4(R1)                                                     
         MVC   0(8,R3),SRFADLBL    SET START LABEL                              
         LA    RE,7(R3)                                                         
         CLI   0(RE),C' '          FILL UNUSED POSITIONS WITH DOTS              
         BH    *+12                                                             
         MVI   0(RE),C'.'                                                       
         BCT   RE,*-12                                                          
         SPACE 1                                                                
         CLI   RECOREL,0           IF RECORD/ELEMENT SWITCH SET                 
         BE    *+14                                                             
         MVI   8(R3),C','          DISPLAY IT, TOO                              
         MVC   9(1,R3),RECOREL                                                  
         SPACE 1                                                                
         GOTO1 VHEXOUT,DMCB,START,WORK,4,0  SET STARTING ADDRESS                
         MVC   SRVP1(6),WORK+2                                                  
         CLI   START,0                                                          
         BE    *+10                                                             
         MVC   SRVP1(8),WORK                                                    
         SPACE 1                                                                
         XC    SRVP2,SRVP2         ENSURE LAST TWO FIELDS CLEARED               
         XC    SRVP3,SRVP3                                                      
         OI    SRVP2H+6,X'40'      SET CURSOR TO FIRST OPEN FIELD               
         SPACE 1                                                                
         MVC   SRVMSG(L'MSG1),MSG1 DISPLAY MESSAGE                              
         SPACE 1                                                                
         CLC   ACURRPOS,ADSECT             IF NOT STARTING AT DSECT             
         BE    *+10                                                             
         MVC   SRVMSG+L'MSG1(L'MSG2),MSG2  INDICATE MORE TO COME                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS HELP ON LOWER PART OF SCREEN                    
         SPACE 1                                                                
HELPDIS  NTR1                                                                   
         LA    R2,DMPL2H           R2 = A(FIRST FIELD)                          
         LA    R3,HELP0            R3 = A(HELP LITERALS)                        
         SPACE 1                                                                
HDIS10   ZIC   R1,0(R3)            R1 = L'LITERAL                               
         LTR   R1,R1                                                            
         BZ    HDIS20                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8+10(0,R2),1(R3)    MOVE LITERAL TO SCREEN                       
         LA    R1,1(R1)                                                         
         SPACE 1                                                                
HDIS20   LA    R3,1(R1,R3)         BUMP TO NEXT LITERAL                         
         IC    R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT FIELD                           
         SPACE 1                                                                
         CLI   0(R3),X'FF'         TEST FOR END                                 
         BNE   HDIS10                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, ETC.                                              
         SPACE 2                                                                
ERRINV   MVC   SRVMSG(L'ERR1),ERR1                                              
         B     ERRXIT                                                           
ERRLABEL MVC   SRVMSG(L'ERR2),ERR2                                              
         OI    SRVP4H+6,X'C0'      SET CURSOR TO THIS FIELD                     
         B     ERRXIT                                                           
ERRARGS  MVC   SRVMSG(L'ERR3),ERR3                                              
         OI    SRVP4H+6,X'C0'      SET CURSOR TO THIS FIELD                     
         BAS   RE,HELPDIS          DISPLAY HELP                                 
         B     ERRXIT                                                           
ERRPHASE MVC   SRVMSG(L'ERR4),ERR4                                              
         GOTO1 VHEXOUT,DMCB,OVERLAY,SRVMSG+L'ERR4,1,0                           
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRXIT   L     RD,SAVERD                                                        
         B     XIT                                                              
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
YESR2    XR    RC,RC                                                            
NOR2     LTR   RC,RC                                                            
XITR2    XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
RELO     DS    F                                                                
SPACES   DC    (L'LINDATA)C' '                                                  
MSG1     DC    C'Dsect data displayed'                                          
MSG2     DC    C' - hit enter for next'                                         
ERR1     DC    C'Invalid input field'                                           
ERR2     DC    C'Label not found for requested system'                          
ERR3     DC    C'Invalid input - see below for usage instructions'              
ERR4     DC    C'Can''t find dsect phase T15E'                                  
         SPACE 1                                                                
HEAD1    DC    C'Disp Label    Def Type               Value'                    
         SPACE 1                                                                
*              TABLE OF DISPLAYABLE CHARACTERS                                  
         SPACE 1                                                                
DISPCHRS DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'006F6F6F6F6F6F6F6F6F6F006F00006F'  40-4F                    
         DC    XL16'006F6F6F6F6F6F6F6F6F6F0000006F6F'  50-5F                    
         DC    XL16'00006F6F6F6F6F6F6F6F6F00006F6F00'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F0000000000'  70-7F                    
         DC    XL16'6F000000000000000000006F6F6F6F6F'  80-8F                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  90-9F                    
         DC    XL16'6F6F00000000000000006F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  C0-CF                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  D0-DF                    
         DC    XL16'6F6F00000000000000006F6F6F6F6F6F'  E0-EF                    
         DC    XL16'000000000000000000006F6F6F6F6F6F'  F0-FF                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HELP LITERAL TABLE                                               
         SPACE 1                                                                
HELP0    DS    0C                                                               
         DC    AL1(L'HELP1)                                                     
HELP1    DC    C'Valid input is first parameter for $ND Dsect display'          
         DC    AL1(L'HELP2)                                                     
HELP2    DC    C'is of the form ''#,s,llllllll,i'', where:'                     
         DC    AL1(0)                                                           
         DC    AL1(L'HELP3)                                                     
HELP3    DC    C'    #        = dump number, as usual.'                         
         DC    AL1(0)                                                           
         DC    AL1(L'HELP4)                                                     
HELP4    DC    C'    s        = system.  Omit this argument if you are'         
         DC    AL1(L'HELP5)                                                     
HELP5    DC    C'                        requesting a Facpak dsect.'            
         DC    AL1(0)                                                           
         DC    AL1(L'HELP6)                                                     
HELP6    DC    C'    llllllll = 1 - 8 character label in dsect.'                
         DC    AL1(L'HELP7)                                                     
HELP7    DC    C'               For Acc and Talent, this argument is op*        
               tional'                                                          
         DC    AL1(L'HELP8)                                                     
HELP8    DC    C'               when pointing to a record or an element*        
               ,'                                                               
         DC    AL1(L'HELP9)                                                     
HELP9    DC    C'               as long as argument 4 is input.'                
         DC    AL1(0)                                                           
         DC    AL1(L'HELPA)                                                     
HELPA    DC    C'    i        = optional indicator.  Specify ''r''ecord*        
               '                                                                
         DC    AL1(L'HELPB)                                                     
HELPB    DC    C'               or ''e''lement (default).'                      
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SRDMPWORKD                                                     
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 1                                                                
         ORG   SORTBLCK                                                         
APHASE   DS    A                   A(DSECT PHASE)                               
ADSECT   DS    A                   A(CURRENT DSECT IN PHASE)                    
ACURRPOS DS    A                   A(CURRENT POSITION IN DSECT PHASE)           
ASTART   DS    A                   A(STARTING POSITION IN DUMP)                 
FIELDLEN DS    H                   L'CURRENT FIELD                              
DSPTOLBL DS    XL1                 DISPLACEMENT TO LABEL IN FIELD               
SYSCHAR  DS    CL1                 CHAR SYSTEM REQUESTED FOR DSECT              
SYSTEM   DS    XL1                 SYSTEM REQUESTED FOR DSECT                   
OVERLAY  DS    XL1                                                              
ELLEN    DS    XL1                 L'CURRENT ELEMENT                            
LABEL    DS    CL8                 LABEL                                        
LLABEL   DS    XL1                 L'LABEL                                      
DATADISP DS    H                   DISP. TO FIRST ELEMENT                       
ELCODE   DS    XL1                 ELEMENT CODE                                 
RECOREL  DS    CL1                 'R'ECORD OR 'E'LEMENT INDICATOR              
LBLSTAT  DS    XL1                 CURRENT DSECT RECORD STATUS                  
LBLQREC  EQU   X'80'               LABEL IS EQUATE FOR A RECORD                 
LBLQEL   EQU   X'40'               LABEL IS EQUATE FOR AN ELEMENT               
         EJECT                                                                  
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
LINTYPE  DS    CL18                TYPE                                         
         DS    CL1                                                              
         ORG                                                                    
LINDATA  DS    CL(L'DMPL1-L'LINLHS)  DATA                                       
LINNEXT  EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER 32-BYTE SCAN BLOCK                                
         SPACE 1                                                                
SCAND    DSECT                                                                  
SCLEN1   DS    XL1  L'FIELD (OR L'FIRST HALF OF DIVIDED FIELD).                 
SCLEN2   DS    XL1  L'SECOND HALF OF DIVIDED FIELD OR ZERO.                     
SCVAL1   DS    XL1  VALIDITY BITS (X'80'=NUMERIC X'40'=ALPHA X'20'=HEX)         
SCVAL2   DS    XL1  VALIDITY BITS FOR SECOND HALF OF DIVIDED FIELDS.            
SCDISP1  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF LHS                   
         ORG   *-1                                                              
SCBIN1   DS    F    BINARY VALUE OF VALID NUMERIC FIELDS.                       
SCDISP2  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF RHS                   
         ORG   *-1                                                              
SCBIN2   DS    F    BINARY VALUE OF SECOND HALF OF DIVIDED FIELDS.              
SCDATA1  DS    CL10 LEFT JUSTIFIED FIELD DATA PADDED WITH SPACES.               
SCDATA2  DS    CL10 DATA FOR SECOND HALF OF DIVIDED FIELDS.                     
SCANNEXT EQU   *  (NOTE - UNDIVIDED FIELDS MAY BE UP TO 20 CHARACTERS.)         
         EJECT                                                                  
       ++INCLUDE SRFADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         SPACE 3                                                                
* DSECT TO COVER FILE DEFINITION TABLE                                          
*                                                                               
HELEND   DSECT                                                                  
HELENL   DS    0CL13                                                            
HELFLEN  DS    CL1                 L'FILE NAME - 1                              
HELNAME  DS    CL8                 NAME                                         
HELMSIZE DS    CL2                 MAXIMUM RECORD SIZE                          
HELEDIS  DS    CL1                 DISPLACEMENT TO FIRST ELEMENT                
HELLDIS  DS    CL1                 DISPLACEMENT TO LENGTH                       
HELNEXT  EQU   *                                                                
         EJECT                                                                  
* SRDMPDSECT (INCLUDES DDCOMFACS, FADSECTS, DDFLDHEADER, SCREENS)               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE SRDMPDSECT                                                     
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRDMP02   05/01/02'                                      
         END                                                                    
