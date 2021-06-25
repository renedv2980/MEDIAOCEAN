*          DATA SET PRREQ05    AT LEVEL 041 AS OF 01/09/19                      
*PHASE T41205A                                                                  
*INCLUDE PPGETPO#                                                               
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
*  BPLA 01/15    FIX OFF= FILTER FOR THE P74 REQ                                
*                IT DID NOT SUPPORT 2 CHARACTER OFFICES                         
*                                                                               
*  BPLA 03/13    PO#=XXXX    FOR BILLING BY PO CLIENTS                          
*                                                                               
*  BPLA 06/11    UPDATE HELP FOR OPTIONS FIELD                                  
*                                                                               
*  BPLA 11/08    ADJSEP=Y OR N  TO OVERRIDE B2 PROFILE'S VALUE                  
*                GOES IN ROB OPTION FOR BILLING                                 
*                                                                               
*  BPLA 10/2007  WBF=XXX...  FOR BILLING OPTIONS                                
*                            REQUESTS FOR ONE WB FLIGHT                         
*                ACTION=YYYY YEAR OF SERVICE FILTER FOR GM                      
*                                                                               
*  BPLA 05/2007  BILLING OPTIONS BUG FIX  (FR, + ANY OTHER OPTION               
*                CAUSED ERROR)                                                  
*                                                                               
*  KWAN 07/2006  ADD EDIT FOR CLT/OFF OPTION FOR P74 (VAL74)                    
*                                                                               
*  SMYE 03/2006  ADD EDIT FOR "STEWARD" OPTION FOR P52 AND P77                  
*                  SEE VAL52 AND VAL77                                          
*                                                                               
*  BPLA   09/03  OPTION FOR P07 (UNBILLING) + PHASE CARD T41205A                
*                                                                               
*  BPLA   09/03  IREG FEATURE FOR DRAFT BILLING                                 
*                                                                               
*  BPLA  12/02   CHANGES FOR PRIOR MONTHS OPTIONS FOR BILLING                   
*                                                                               
* KWAN 04/09/01 VALIDATE ROUTINE FOR SCHEME CODE FIELD (GRP REC PURGES)         
*                                                                               
*  SMYE 12/00     ADD EDIT FOR VALID MEDIA FOR AGENCY FOR PCM                   
*                                                                               
*  KWAN 12/99     DISABLE COS2=Y OPTION FOR ESTIMATE CHANGE REPORT (EC)         
*                                                                               
*  KWAN 11/99     PLACE 52'S COS2=Y OPTION TO QOPT9 (CARD 2)                    
*                                                                               
*  KWAN 08/99     ADD COS2=Y TO OPTION FIELD FOR P52                            
*                                                                               
*  KWAN 3/99      ADD CODES FOR EST=NNN FOR 12'S OPTION FLD                     
*                                                                               
*  BPLA 3/99      "FR" (FRENCH) ONLY FOR CERTAIN REPORTS                        
*                 ADD "NOHELD" OPTION FOR AU (LIKE 19)                          
*                                                                               
*  SMYE 05/06/98  CHANGE "SFH=H" OPTION TO "NOHELD" OPTION                      
*                                                                               
*  SMYE 04/98     ID="N" AND UNA=Y (OR N) OPTIONS FOR 48                        
*                 VARIOUS HELP MESSAGES FOR OPTIONS FIELD                       
*                                                                               
*  SMYE 12/29/97  "SFH=H" OPTION FOR 18 AND 19                                  
*                                                                               
*  SMYE 11/13/97  "SFH=H" OPTION FOR 52 (AND EC - 01/07/98)                     
*                                                                               
*  BPLA  2/11/97  ERR=Y OPTION FOR B1 AND D1 (RETAIL ERRORS ONLY)               
*                                                                               
*  BPLA 6/11/93   AGENCY FILTER ROUTINE                                         
*                                                                               
*  BPLA 3/18/93   CHANGES FOR RFP  (FERN NOW 2 CHARS)                           
*                                                                               
*  BPLA 9/24/92  NEW OPTIONS FOR BILLING (B1/D1) FOR UPFRONT COMMISSION         
*                BILLING                                                        
*  BPLA 7/14/92  FIX NUMERIC CHECK BUG IN OVRVAL                                
*                                                                               
*  BPLA 3/31/92  NEW ROUTINE FOR OVERAGE                                        
*                                                                               
*  BPLA 6/26/91  NEW EDIT ROUTINE FOR BILLING TYPE                              
*                NEW EDIT ROUTINE FOR BILL NUMBER(S)                            
*                ALSO OPTIONS FOR MY (124)                                      
*  BPLA 10/1/90  ALLOW NEW OPTIONS FIELD FOR B1,D1 AND R1                       
*                                                                               
         TITLE 'PRREQ05  NEW REQUESTS  VALIDATE FIELDS PART-3'                  
         PRINT NOGEN                                                            
T41205   CSECT                                                                  
         NMOD1 000,T41205,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LA    R8,T41205+4095                R8=2ND BASE REGISTER               
         LA    R8,1(R8)                                                         
         USING T41205+4096,R8                                                   
         EJECT                                                                  
         L     R1,FLDHADR                    R1=A(FLD HDR TO BE VALED)          
         SR    RF,RF                                                            
         IC    RF,ROUTNUM                    RF=ROUTINE NUM REQD                
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO                       RF=A(ROUTINE REQD)                 
         BASR  RE,RF                         PASS CONTROL TO ROUTINE            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
OPTIONS  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   OPTX                                                             
*                                                                               
OPTGO    CLI   IFLD,C'?'                                                        
         BE    OPTHELP                                                          
         LA    RE,PRTREC+500                                                    
         LA    RF,470                                                           
         XCEF                                                                   
         LA    R5,PRTREC+500                                                    
         USING SCAND,R5                                                         
         L     R4,FLDHADR                                                       
         MVI   6(R4),X'80'         SET OFF HILITE AND TRANSMIT                  
         MVI   7(R4),0             SET OFF OUTPUT LENGTH                        
         MVC   PLIST+16(4),=X'0A001900'    OVERIDE LENGTHS TO 10 AND 25         
         GOTO1 SCANNER,PLIST,(25,(R4)),(10,PRTREC+500),0,0                      
         ZIC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    OPTX                                                             
         LR    R4,R1                                                            
         LA    R6,1                R6=SCANNER POSITION FOR ERROR RTN            
*                                                                               
*                                *UNIVERSAL OPTIONS                             
OPT10    DS    0H                                                               
         CLC   =C'FR',FLD1       *LANGUAGE TYPE                                 
         BNE   OPT15                                                            
*                                                                               
*                   FRENCH LANGUAGE OPTION ONLY FOR THESE REPORTS               
*                                                                               
         CLC   RNUM,=C'B1'         BILLING                                      
         BE    OPT10D                                                           
         CLC   RNUM,=C'D1'         DRAFT BILLING                                
         BE    OPT10D                                                           
         CLC   RNUM,=C'R1'         REBATE BILLING                               
         BE    OPT10D                                                           
         CLC   RNUM,=C'RD'         DRAFT REBATE BILLING                         
         BE    OPT10D                                                           
         CLC   RNUM,=C'52'         ESTIMATE                                     
         BE    OPT10D                                                           
         CLC   RNUM,=C'EC'         ESTIMATE CHANGE                              
         BE    OPT10D                                                           
         B     OPTINV                                                           
*                                                                               
OPT10D   CLI   FLD1LEN,2                                                        
         BNE   OPTINV                                                           
         OI    FIND,X'04'                                                       
         MVC   RLANG,FLD1                                                       
         MVI   FLD2LEN,X'FF'       SO I'LL IGNORE LATER                         
         CLI   DDS,1               IS IT DDS TERMINAL                           
         BE    OPT12                                                            
         BAS   RE,CHKCAN           OR CANDIAN AGENCY                            
         BNE   OPTINV                                                           
OPT12    BCTR  R4,0                                                             
         B     OPT200                                                           
*                                                                               
OPT15    DS    0H                                                               
         EJECT                                                                  
*                                                                               
OPT20    DS    0H                                                               
*                                                                               
OPT200   DS    0H                                                               
         LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R1,OPT10                                                         
*                                                                               
OPT220   CHI   R4,0                ANY MORE FIELDS TO VALIDATE                  
         BE    OPTX                                                             
*                                                                               
         LA    R5,PRTREC+500       RESET TO FIRST FIELD                         
         ZIC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    OPTX                                                             
         LA    R6,1                R6=SCANNER POSITION FOR ERROR RTN            
*                                                                               
         LA    R4,REQATBL           YES/GET ADDR OF VALROUTINE                  
OPT250   CLC   RNUM,0(R4)                                                       
         BNE   OPT300                                                           
         LR    R7,R1               PASS NUM OF SCANNER FLDS IN R7               
         L     R2,4(R4)                                                         
         A     R2,RELO                                                          
         BR    R2                                                               
*                                                                               
OPT300   LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   OPT250                                                           
         B     OPTHELP             NO MATCH ON RNUM                             
         SPACE 2                                                                
* THIS TABLE CONTAINS ADDRESSES OF REQUEST VALIDATION ROUTINES                  
*                                                                               
REQATBL  DS    0F                                                               
         DC    C'07',X'0000',A(VAL07)                                           
         DC    C'12',X'0000',A(VAL12)                                           
         DC    C'18',X'0000',A(VAL18)                                           
         DC    C'19',X'0000',A(VAL19)                                           
         DC    C'48',X'0000',A(VAL48)                                           
         DC    C'52',X'0000',A(VAL52)                                           
         DC    C'B1',X'0000',A(VALB1)                                           
         DC    C'D1',X'0000',A(VALB1)       SAME AS B1                          
         DC    C'EC',X'0000',A(VAL52)       SAME AS 52                          
         DC    C'MY',X'0000',A(MYVAL)                                           
         DC    C'AU',X'0000',A(VAL19)       SAME AS 19                          
         DC    C'77',X'0000',A(VAL77)                                           
         DC    C'74',X'0000',A(VAL74)                                           
         DC    C'GM',X'0000',A(VALGM)                                           
         DC    C'WB',X'0000',A(VALWB)                                           
         DC    C'98',X'0000',A(VAL98)                                           
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* INPUT: R5 POINTS TO SCANNER                                                   
*        R6 POSITION WITHIN SCANNER FOR ERROR ROUTINE                           
*        R7 CONTROLS LOOP IN SCANNER                                            
*                                                                               
         USING SCAND,R5                                                         
*                                                                               
*                                                                               
*                                                                               
VAL07    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL07N              IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
VAL07C   DS    0H                  VALIDATE ESTIMATE FILTER OPTION              
         CLC   =C'CORRECTION',FLD1                                              
         BNE   VAL07D                                                           
         CLI   FLD2LEN,0                                                        
         BNE   OPTINV                                                           
         MVI   RO5,C'D'        CREATE UNBILLING RECORDS                         
         OI    FIND,X'04'                                                       
         B     VAL07N                                                           
*                                                                               
VAL07D   B     OPTINV                                                           
*                                                                               
VAL07N   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL07                                                         
         B     OPTX                                                             
*                                                                               
*                                                                               
VAL12    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL12N              IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
VAL12C   DS    0H                  VALIDATE ESTIMATE FILTER OPTION              
         CLC   =C'EST',FLD1                                                     
         BNE   VAL12D                                                           
         CLI   FLD1LEN,3                                                        
         BNE   OPTINV                                                           
*                                                                               
         CLI   FLD2LEN,0                                                        
         BNH   OPTINV                                                           
         TM    FLD2VAL,X'80'       IS INPUT NUMERIC?                            
         BZ    OPTINV                                                           
         ICM   RE,15,FLD2B         BINARY ESTIMATE                              
         CHI   RE,0                                                             
         BL    OPTINV                                                           
         CHI   RE,999                                                           
         BH    OPTINV                                                           
*                                                                               
         CVD   RE,DUB                                                           
         UNPK  RESTEND,DUB                                                      
         OI    RESTEND+2,X'F0'                                                  
         OI    FIND,X'04'                                                       
         B     VAL12N                                                           
*                                                                               
VAL12D   B     OPTINV                                                           
*                                                                               
VAL12N   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL12                                                         
         B     OPTX                                                             
*                                                                               
*                                                                               
*                                                                               
VAL18    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL18N             IGNORE UNIVERSAL OPTIONS THIS TIME            
*                                                                               
VAL18C   DS    0H             TEST FOR NOHELD (SPECIAL FILE HANDLING)           
         CLC   =C'NOH',FLD1                                                     
         BNE   VAL18D                                                           
         CLI   FLD1LEN,6                                                        
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO8,C'H'            (REQ2,COL62)                                 
         B     VAL18N                                                           
*                                                                               
VAL18D   B     OPTINV                                                           
*                                                                               
VAL18N   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL18                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
VAL19    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL19N             IGNORE UNIVERSAL OPTIONS THIS TIME            
*                                                                               
VAL19C   DS    0H             TEST FOR NOHELD (SPECIAL FILE HANDLING)           
         CLC   =C'NOH',FLD1                                                     
         BNE   VAL19D                                                           
         CLI   FLD1LEN,6                                                        
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO9,C'H'            (REQ2,COL63)                                 
         B     VAL19N                                                           
*                                                                               
VAL19D   B     OPTINV                                                           
*                                                                               
VAL19N   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL19                                                         
         B     OPTX                                                             
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
VAL48    DS    0H                                                               
*****    CLI   FLD2LEN,X'FF'                                                    
*****    BE    VAL48X             IGNORE UNIVERSAL OPTIONS THIS TIME            
*                                                                               
         CLC   =C'ID',FLD1                                                      
         BNE   VAL48B                                                           
         CLI   FLD1LEN,2                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   RO1,C'G'            ONLY FOR LIST TYPE G                         
         BNE   OPTINV                                                           
         MVC   RO7,FLD2                                                         
         OI    FIND,X'04'                                                       
         B     VAL48X                                                           
*                                                                               
VAL48B   DS    0H                                                               
         CLC   =C'UNA',FLD1                                                     
         BNE   VAL48C                                                           
         CLI   FLD1LEN,3                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         CLI   RO1,C'G'            ONLY FOR LIST TYPE G & V                     
         BE    *+8                                                              
         CLI   RO1,C'V'                                                         
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BE    *+8                                                              
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   RO6,FLD2                                                         
         OI    FIND,X'04'                                                       
         B     VAL48X                                                           
*                                                                               
VAL48C   DS    0H                                                               
         CLC   =C'SC',FLD1                                                      
         BNE   VAL48D                                                           
         CLI   FLD1LEN,2                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,2                                                        
         BNE   OPTINV                                                           
         TM    FLD2VAL,X'80'       IS INPUT NUMERIC?                            
         BO    VAL48CXX                                                         
         TM    FLD2VAL,X'40'       IS INPUT ALPHA?                              
         BZ    OPTINV                                                           
*                                                                               
VAL48CXX MVC   RSCD,FLD2           STATE CODE FILTER (RCARD2)                   
         OI    FIND,X'04'                                                       
         B     VAL48X                                                           
*                                                                               
VAL48D   DS    0H                  FOR FUTURE USES                              
         B     OPTINV              THERE IS NO MORE OPTIONS                     
*                                                                               
VAL48X   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL48                                                         
         B     OPTX                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
VAL52    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL52XX            IGNORE UNIVERSAL OPTIONS THIS TIME            
         CLI   FLD1,C'D'          D=C'D'  (REQ1,COL65)                          
         BNE   VAL52C                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         BAS   RE,VONEFLD                                                       
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         B     VAL52XX                                                          
*                                                                               
VAL52C   DS    0H               TEST FOR NOHELD (SPECIAL FILE HANDLING)         
         CLC   =C'NOH',FLD1                                                     
         BNE   VAL52D                                                           
         CLI   FLD1LEN,6                                                        
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO8,C'H'            (REQ2,COL62)                                 
         B     VAL52XX                                                          
*                                                                               
VAL52D   DS    0H                  VALIDATE COST2 FACTOR OPTION                 
*                                                                               
         CLC   =C'COS2',FLD1                                                    
         BNE   VAL52E              GO VALIDATE NEXT OPTION FLD                  
         CLI   FLD1LEN,4                                                        
         BNE   OPTINV                                                           
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
*                                                                               
         CLC   RNUM,=C'EC'         COS2=Y DOESN'T WORK FOR EST CHANGE           
         BNE   VAL52D20                                                         
         MVC   FERN,=AL2(FLDINV)                                                
         B     EXITVAL                                                          
*                                                                               
VAL52D20 OI    FIND,X'08'                                                       
         MVI   RO9,C'O'            REQ9=O FOR OPEN                              
         B     VAL52XX                                                          
*                                                                               
*                                                                               
VAL52E   DS    0H                  CHECK FOR (STEW)ARD OPTION                   
*                                                                               
         CLC   =C'STEW',FLD1                                                    
         BNE   VAL52F              GO VALIDATE NEXT OPTION FLD                  
*NOGOOD  TM    RNAME,X'40'         "INCLUDE TEST BUYS" REQUEST ?                
*NOGOOD  BO    OPTINV              NO - STEWARD REQUEST NOT VALID               
         CLI   FLD1LEN,4                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         MVI   ROA,C'A'            QOPTA=A FOR "ALL INCLUDING STEWARD"          
         CLI   FLD2,C'Y'                                                        
         BE    VAL52EX             "A" IN ROA ALREADY SET                       
         CLI   FLD2,C'O'                                                        
         BNE   OPTINV                                                           
         MVI   ROA,C'O'         QOPTA=O FOR "ONLY STEWARD" (REQ2,COL64)         
*                                                                               
VAL52EX  DS    0H                  DONE WITH (STEW)ARD OPTION                   
         OI    FIND,X'08'                                                       
         B     VAL52XX                                                          
*                                                                               
VAL52F   DS    0H                  FOR FUTURE USE                               
*                                                                               
*                                                                               
VAL52X   DS    0H                  NO VALID OPTION FOUND                        
         B     OPTINV                                                           
*                                                                               
*                                                                               
VAL52XX  LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL52                                                         
         B     OPTX                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
VAL77    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL77XX             IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
         CLC   =C'STEW',FLD1       CHECK FOR (STEW)ARD OPTION                   
         BNE   VAL77F              GO VALIDATE NEXT OPTION FLD                  
         CLI   FLD1LEN,4                                                        
         BNE   OPTINV                                                           
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         MVI   RO9,C'A'            QOPT9=A FOR "ALL INCLUDING STEWARD"          
         CLI   FLD2,C'Y'                                                        
         BE    VAL77EX             IF "Y" SEND "A" IN REQUEST                   
         CLI   FLD2,C'O'                                                        
         BNE   OPTINV                                                           
         MVI   RO9,C'O'         QOPT9=O FOR "ONLY STEWARD" (REQ2,COL63)         
*                                                                               
VAL77EX  DS    0H                  DONE WITH (STEW)ARD OPTION                   
         OI    FIND,X'08'                                                       
         MVI   RO7,C'Y'            MAKE THIS TEST REQUEST FOR STEWARD           
         B     VAL77XX                                                          
*                                                                               
VAL77F   DS    0H                  FOR FUTURE USE                               
*                                                                               
*                                                                               
VAL77X   DS    0H                  NO VALID OPTION FOUND                        
         B     OPTINV                                                           
*                                                                               
*                                                                               
VAL77XX  LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL77                                                         
         B     OPTX                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VAL74    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL74XX             IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
         CLC   =C'CLT',FLD1        CLIENT FILTER?                               
         BE    VAL74OK                                                          
         CLC   =C'OFF',FLD1        OFFICE FILTER?                               
         BNE   OPTINV              NO, INVALID                                  
*                                                                               
*                                                                               
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(R3)                                                    
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC2,FLD2+1     EXTERNAL OFFICE CODE                          
*                                                                               
         MVC   OFCLMT,6(R3)                                                     
         MVC   OFCSECD,ASECBLK    A("SECRET BLOCK")                             
*                                                                               
         DROP  R1                                                               
*                                                                               
*        VALIDATE OFFICE CODE VIA OFFICER AND GET OFFICE NAME                   
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(1,ACOMFACS),                  X        
               (C'L',NAME)                                                      
*                                                                               
         CLI   0(R1),0             DID IT PASS SECURITY?                        
         BE    *+14                YES                                          
         MVC   FERN,=AL2(CAERROR) ACCESS ERROR                                  
         B     OFFINV                                                           
*                                                                               
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         LA    RF,FLD2+1           POINT TO OFFICE CODE SAVEAREA                
         MVI   FLD2+2,C' '         SET TO SPACE                                 
*                                                                               
         MVC   0(1,RF),OFCOFC      SAVE INTERNAL OFFICE CODE                    
*                                                                               
VAL74OK  OI    FIND,X'08'                                                       
         MVC   RPUB(3),FLD2                                                     
*                                                                               
VAL74XX  LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL74                                                         
         B     OPTX                                                             
         EJECT                                                                  
VAL98    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VAL98XX             IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
         CLC   =C'IDESK ONLY',FLD1 ADDED VIA IDESK ONLY                         
         BNE   OPTINV                                                           
         CLI   FLD1LEN,10                                                       
         BNE   OPTINV                                                           
         CLI   FLD2LEN,0                                                        
         BE    VAL98OK                                                          
         CLI   FLD2LEN,1                                                        
         BNE   OPTINV                                                           
         MVI   RO1-1,C'I'          QOPT9=A FOR "ALL INCLUDING STEWARD"          
         CLI   FLD2,C'Y'                                                        
         BE    VAL98OK             IF "Y" SEND "A" IN REQUEST                   
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVI   RO1-1,C' '          CLEAR IF N                                   
         OI    FIND,X'04'                                                       
         B     VAL98XX                                                          
*                                                                               
VAL98OK  MVI   RO1-1,C'I'         LETS THEM ONLY ENTER 'IDESK ONLY'             
         OI    FIND,X'04'                                                       
*                                                                               
VAL98XX  LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL98                                                         
         B     OPTX                                                             
         EJECT                                                                  
VALGM    DS    0H                                                               
         CLI   FLD2LEN,X'FF'                                                    
         BE    VALGMXX             IGNORE UNIVERSAL OPTIONS THIS TIME           
*                                                                               
         CLC   =C'ACTION',FLD1     YEAR OF SERVICE FILTER                       
         BNE   OPTINV                                                           
         TM    FLD2VAL,X'80'       MUST BE NUMERIC                              
         BZ    OPTINV                                                           
         CLI   FLD2LEN,4           MUST BE 4 DIGIT YEAR                         
         BNE   OPTINV                                                           
         OI    FIND,X'04'          IT IS VALID                                  
         MVI   RO1,C'Y'            TURN ON OPTION#1                             
         MVC   RPUB+1(2),FLD2+2    TAKING LAST 2 DIGITS OF THE YEAR             
*                                                                               
VALGMXX  LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VALGM                                                         
         B     OPTX                                                             
         EJECT                                                                  
         EJECT                                                                  
VALWB    DS    0H                                                               
*                                                                               
         CLC   =C'NOAOR',FLD1      EXCLUDE AOR INVOICES                         
         BNE   OPTINV                                                           
         OI    FIND,X'04'          IT IS VALID                                  
         MVI   RO6,C'X'            TURN ON OPTION#6                             
*                                                                               
         LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VALWB                                                         
         B     OPTX                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MYVAL    DS    0H                                                               
         CLC   =C'UNPOST',FLD1    UNPOST (ONLY DDS TERMINAL)                    
         BNE   MYV10              (UNPOST AND MARK MUTUALLY EXCLUSIVE)          
         CLI   RO4,C' '           MUST BE BLANK                                 
         BNE   OPTINV                                                           
         MVI   RO4,C'U'                                                         
         CLI   DDS,1                                                            
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         B     MYV100                                                           
*                                                                               
MYV10    DS    0H                                                               
         CLC   =C'MARK',FLD1           MARK                                     
         BNE   MYV20                                                            
         CLI   RO4,C' '                                                         
         BNE   OPTINV                 MUST BE BLANK                             
         MVI   RO4,C'M'                                                         
         CLI   DDS,1                                                            
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         B     MYV100                                                           
*                                                                               
*                                                                               
MYV20    DS    0H                                                               
         B     OPTINV                                                           
*                                                                               
MYV100   LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,MYVAL                                                         
         B     OPTX                                                             
*                                                                               
VALB1    DS    0H                                                               
         CLI   FLD2LEN,X'FF'     IGNORE UNIVERSAL OPTIONS THIS TIME             
         BE    VALB1X                                                           
*                                                                               
         CLC   =C'COM',FLD1                                                     
         BNE   VALB1B                                                           
         CLI   RO8,C' '                                                         
         BNE   OPTINV                                                           
         MVI   RO8,C'C'                                                         
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1B   CLC   =C'NET',FLD1                                                     
         BNE   VALB1D                                                           
         CLI   RO8,C' '                                                         
         BNE   OPTINV                                                           
         MVI   RO8,C'N'                                                         
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1D   CLC   =C'REG',FLD1                                                     
         BNE   VALB1E                                                           
         CLI   RO8,C' '                                                         
         BNE   OPTINV                                                           
         MVI   RO8,C'R'                                                         
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1E   CLC   =C'ERR',FLD1                                                     
         BNE   VALB1G                                                           
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
         CLC   RNUM(2),=C'D1'      ONLY FOR DRAFT BILLING                       
         BNE   OPTINV                                                           
         CLI   RO9,C' '                                                         
         BNE   OPTINV                                                           
         MVI   RO9,C'E'                                                         
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1G   DS    0H                                                               
         CLC   =C'PRIOR',FLD1                                                   
         BNE   VALB1M                                                           
         LA    R1,FLD2                                                          
         CLI   FLD2LEN,2                                                        
         BE    VALB1H                                                           
         CLI   FLD2LEN,3                                                        
         BE    VALB1I                                                           
         B     OPTINV                                                           
                                                                                
VALB1H   CLI   FLD2,C'1'                                                        
         BL    OPTINV                                                           
         CLI   FLD2,C'9'                                                        
         BH    OPTINV                                                           
         MVI   RCARD2+39,C'0'                                                   
         MVC   RCARD2+40(1),FLD2                                                
         LA    R1,1(R1)                                                         
         B     VALB1K                                                           
                                                                                
VALB1I   CLC   FLD2(2),=C'01'       MUST BE 01-12                               
         BL    OPTINV                                                           
         CLC   FLD2(2),=C'12'                                                   
         BH    OPTINV                                                           
         CLI   FLD2+1,C' '         DID THEY ENTER A SPACE                       
         BNE   VALB1I2             AFTER A ONE DIGIT MONTHS                     
         MVC   FLD2+1(1),FLD2                                                   
         MVI   FLD2,C'0'           CHANGE TO 0MTH                               
         B     VALB1I3                                                          
*                                                                               
VALB1I2  CLI   FLD2+1,C'0'         SECOND DIGIT MUST BE NUMERIC                 
         BL    OPTINV                                                           
*                                                                               
VALB1I3  MVC   RCARD2+39(2),FLD2                                                
         LA    R1,2(R1)                                                         
*                                                                               
VALB1K   CLI   0(R1),C'T'          TOGETHER?                                    
         BE    VALB1L                                                           
         CLI   0(R1),C'S'          SEPARATE?                                    
         BNE   OPTINV                                                           
         CLI   RCARD2+26,C'R'      SEE IF REVERSAL                              
         BE    OPTINV              NO SEPARATE PRIOR MONTHS ALLOWED             
*                                   - DOESN'T WORK                              
VALB1L   MVC   RCARD2+41(1),0(R1)                                               
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1M   DS    0H                                                               
         CLC   =C'IREG',FLD1                                                    
         BNE   VALB1P                                                           
         CLI   FLD2LEN,0                                                        
         BNE   OPTINV                                                           
         CLC   RNUM,=C'D1'      DRAFT                                           
         BE    VALB1M2                                                          
         CLC   RNUM,=C'RD'      OR REBATE DRAFT                                 
         BE    VALB1M2                                                          
         B     OPTINV                                                           
*                                                                               
VALB1M2  MVI   ROA,C'R'        PRODUCE REGISTER                                 
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1P   DS    0H                                                               
*                                                                               
VALB1R   DS    0H                                                               
         CLC   =C'WBF',FLD1    WB FLIGHT FILTER                                 
         BNE   VALB1V                                                           
         CLC   RCARD2+8(11),SPACES   SEE IF AREA USED ALREADY                   
         BH    WBFINV          IF SO, I CAN'T USE                               
         CLI   FLD2LEN,10      MUST BE 10 CHARACTERS                            
         BNE   OPTINV                                                           
         CLC   =C'ALL',RCLI                                                     
         BE    OPTINV                                                           
         CLI   RCLI,C'*'                                                        
         BE    OPTINV                                                           
         CLI   RCLI,C'&&'                                                       
         BE    OPTINV                                                           
         CLI   RCLI,C'$'                                                        
         BE    OPTINV                                                           
         OI    FIND,X'04'            VALID FLIGHT ID INPUT                      
*                                                                               
VALB1R5  DS    0H                                                               
*                                     PRECEDE WITH . SO BILLING                 
*                                     KNOWS IT'S A WB FLIGHT                    
         MVI   RCARD2+08,C'.'         CARD 2 COLUMN 09                          
         MVC   RCARD2+09(10),FLD2     CARD 2 COLUMN 10                          
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1V   DS    0H                                                               
         CLC   =C'ADJSEP',FLD1     OVERRIDING B2 PROFILE SEP ADJUSTMNT          
         BNE   VALB1PO                                                          
         CLI   FLD2,C'Y'                                                        
         BE    *+12                                                             
         CLI   FLD2,C'N'                                                        
         BNE   OPTINV                                                           
         MVC   ROB(1),FLD2                                                      
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
VALB1PO  DS    0H                                                               
         CLC   =C'PO#',FLD1    PO# FILTER                                       
         BNE   OPTINV                                                           
         TM    CLTPOOPT,X'80'  BILLING BY PO#                                   
         BZ    OPTINV                                                           
         CLC   RCARD2+45(25),SPACES   SEE IF AREA USED ALREADY                  
         BH    PO#INV          IF SO, I CAN'T USE                               
         CLI   FLD2LEN,01      MUST BE AT LEAST ONE                             
         BL    PO#INV                                                           
         CLI   FLD2LEN,25      MAX 25 CHARACTERS                                
         BH    PO#INV                                                           
         CLC   =C'ALL',RCLI                                                     
         BE    PO#INV                                                           
         CLI   RCLI,C'*'                                                        
         BE    PO#INV                                                           
         CLI   RCLI,C'&&'                                                       
         BE    PO#INV                                                           
         CLI   RCLI,C'$'                                                        
         BE    PO#INV                                                           
         OC    CLTPOEFF,CLTPOEFF     DO I HAVE AN EFFECTIVE MOS                 
         BZ    VALB1PO3                                                         
         CLC   RSTRD(4),CLTPOEFF     CHECK VS MOS                               
         BL    PO#INV                                                           
VALB1PO3 DS    0H                                                               
         CLI   CLTPOTYP,C'C'         CLIENT LEVEL PO#'S                         
         BE    VALB1PO7                                                         
         CLC   RPRO,=C'ALL'          DISALLOW ALL PRDS FOR P,E                  
         BE    PO#INV2                                                          
         CLI   CLTPOTYP,C'P'                                                    
         BE    VALB1PO7                                                         
         CLC   REST,=C'ALL'     DISALLOW ALL ESTS FOR E                         
         BE    PO#INV2                                                          
         CLC   REST,SPACES      DISALLOW MISSING EST FOR E                      
         BE    PO#INV2                                                          
         CLC   RESTEND,SPACES   DISALLOW A RANGE OR FILTER FOR E                
         BNE   PO#INV2                                                          
*                                                                               
VALB1PO7 DS    0H         FINALLY FIND PO# AND SAVE ITS SEQUENCE #              
         OC    FLD2(25),SPACES                                                  
         DS    0H                USE PPGETPO# TO GET PO#                        
         MVC   WORK+40(1),RMED                                                  
         MVC   WORK+41(2),RAGY                                                  
         MVC   WORK+43(3),RCLI                                                  
         MVC   WORK+46(3),RPRO                                                  
         CLI   CLTPOTYP,C'C'                                                    
         BNE   VALB1PO8                                                         
         MVC   WORK+46(5),=X'0000000000'  CLEAR PRD AND EST                     
         B     VALB1POX                                                         
VALB1PO8 DS    0H                                                               
         CLI   CLTPOTYP,C'P'                                                    
         BNE   VALB1PO9                                                         
         MVC   WORK+49(2),=X'0000'    CLEAR ESTIMATE                            
         B     VALB1POX                                                         
*                                                                               
VALB1PO9 DS    0H                     ESTIMATE LEVEL PO#                        
         CLI   REST,C'0'              ESTIMATE # MUST BE PRESENT                
         BL    PO#INV2                                                          
         PACK  DUB,REST(3)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   WORK+49(2),HALF                                                  
VALB1POX MVC   WORK+51(25),FLD2       PO#                                       
         GOTO1 =V(PPGETPO#),PLIST,(C'P',WORK+40),(CLTPOTYP,WORK+4),ACOMX        
               FACS,RR=RELO                                                     
         CLI   PLIST,X'FF'    PO# NOT FOUND                                     
         BE    PO#INV3                                                          
*                                                                               
         MVC   RCARD2+45(2),WORK+4   CARD 2 COLUMN 46                           
         OI    FIND,X'04'                                                       
         B     VALB1X                                                           
*                                                                               
*                                                                               
VALB1PX  B     EXITVAL                                                          
*                                                                               
*                                AND PUT JOB IN RPUB+27?                        
VALB1X   DS    0H                                                               
         LA    R5,47(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VALB1                                                         
         B     OPTX                                                             
*                                                                               
VONEFLD  NTR1                                                                   
         LA    R2,ONEFLD                                                        
VO5      CLC   1(2,R2),RNUM                                                     
         BE    VO10                                                             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   VO5                                                              
         DC    H'0'                NO MATCH                                     
VO10     ZIC   R1,0(R2)                                                         
         S     R1,=F'4'                                                         
         LR    R4,R2                                                            
VO12     CLC   FLD2(1),4(R4)                                                    
         BE    VO20                                                             
         LA    R4,1(R4)                                                         
         BCT   R1,VO12                                                          
         B     VONO                NO MATCH,INVALID                             
VO20     ZIC   R1,3(R2)                                                         
         BCTR  R1,0                MUST DECREMENT COLUMN NUMBER                 
         LA    R1,RNUM(R1)                                                      
         MVC   0(1,R1),4(R4)                                                    
         SR    R1,R1                                                            
         B     *+8                                                              
VONO     LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     OPTX                                                             
*                                                                               
* TABLE OF VALID VALUES                                                         
*                                                                               
* XL1 ENTRY LENGTH                                                              
* CL2 ENTRY REQUEST NUMBER                                                      
* XL1 ENTRY COLUMN                                                              
* CLN VALUE LIST                                                                
*                                                                               
ONEFLD   DS    0C                                                               
         DC    AL1(5),C'52',AL1(65),C'D'                                        
         DC    AL1(5),C'EC',AL1(65),C'D'                                        
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
OPTINV   DS    0H                  INVALID ENTRY                                
         MVC   FERN,=AL2(FE)                                                    
OFFINV   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(21),=C'*** ERROR FLD #   ***'                               
         LA    R5,TEMP+15                                                       
         EDIT  (R6),(2,0(R5)),ALIGN=LEFT,WRK=TEMP+61                            
         NI    FIND,X'FB'           SET OFF VALID BIT (X'04')                   
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR(60),TEMP                                                  
         FOUT  BVRHDRH                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         B     OPTX                                                             
         EJECT                                                                  
WBFINV   DS    0H                  INVALID ENTRY                                
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(33),=C'** OPTION DISALLOWED WITH ADID **'                   
         NI    FIND,X'FB'           SET OFF VALID BIT (X'04')                   
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR(60),TEMP                                                  
         FOUT  BVRHDRH                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         B     OPTX                                                             
         EJECT                                                                  
PO#INV   DS    0H                  INVALID PO# ENTRY                            
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(29),=C'** INVALID OR MULTIPLE PO# **'                       
         NI    FIND,X'FB'           SET OFF VALID BIT (X'04')                   
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR(60),TEMP                                                  
         FOUT  BVRHDRH                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         B     OPTX                                                             
*                                                                               
PO#INV2  DS    0H                  INVALID PO# ENTRY                            
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(28),=C'** PO# CAN''T BE VALIDATED **'                       
         NI    FIND,X'FB'           SET OFF VALID BIT (X'04')                   
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR(60),TEMP                                                  
         FOUT  BVRHDRH                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         B     OPTX                                                             
PO#INV3  DS    0H                  PO# RECORD NOT FOUND                         
         MVC   FERN,=AL2(RECNOF)                                                
         NI    FIND,X'FB'                                                       
         B     OPTX                                                             
*                                                                               
OPTHELP  DS    0H                    HELP SCREEN                                
         MVC   FERN,=AL2(FE)                                                    
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(3),=C'***'                                                  
         L     R4,FLDHADR                                                       
         CLC   RNUM,=C'12'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'18'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'19'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'48'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'52'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'EC'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'B1'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'D1'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'R1'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'RD'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'74'                                                      
         BE    HLP10                                                            
         CLC   RNUM,=C'GM'                                                      
         BE    HLP10                                                            
         XC    8(50,R4),8(R4)                                                   
         MVC   8(23,R4),=C' *** FOR FUTURE USE ***'   TEMP                      
         OI    6(R4),X'80'         TEMP                                         
         B     OPTX                TEMP                                         
*                                         CANADIAN                              
HLP10    MVC   8(10,R4),=C'  FR      '                                          
         OI    6(R4),X'88'                                                      
         CLC   RNUM,=C'07'    THESE REPORTS - NO FR OPTION                      
         BE    HLP11                                                            
         CLC   RNUM,=C'12'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'18'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'19'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'48'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'74'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'GM'                                                      
         BE    HLP11                                                            
         CLI   DDS,1                                                            
         BE    HLP12                                                            
         BAS   RE,CHKCAN                                                        
         BE    HLP12                                                            
HLP11    XC    8(10,R4),8(R4)                                                   
         B     *+8                                                              
HLP12    LA    R4,10(R4)                                                        
         LA    R5,HELPTBL                                                       
HLP15    CLC   RNUM,1(R5)                                                       
         BE    HLP20                                                            
         ZIC   R1,0(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BNE   HLP15                                                            
         B     OPTX                                                             
HLP20    ZIC   R1,0(R5)                                                         
         SH    R1,=H'4'            1 FOR EX AND 3 FOR OVERHEAD                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),3(R5)                                                    
OPTX     DS    0H                                                               
EXITVAL  XIT1                                                                   
         EJECT                                                                  
HELPTBL  DS    0F                                                               
*NOP*    DC    AL1(10),C'B1',C'  ERR=Y'                                         
         DC    AL1(10),C'D1',C'  ERR=Y'                                         
         DC    AL1(11),C'EC',C'  NOHELD'                                        
         DC    AL1(12),C'12',C'  EST=NNN'                                       
         DC    AL1(11),C'18',C'  NOHELD'                                        
         DC    AL1(11),C'19',C'  NOHELD'                                        
         DC    AL1(22),C'48',C'  ID=  UNA=Y/N  SC='                             
         DC    AL1(24),C'52',C'  D=D  NOHELD  COS2=Y'                           
         DC    AL1(17),C'74',C'  CLT=    OFF='                                  
         DC    AL1(11),C'AU',C'  NOHELD'                                        
         DC    AL1(16),C'GM',C'  ACTION=YYYY'                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
CHKCAN   NTR1             READ AGY HEADER TO SEE IF CANADIAN AGENCY             
         MVI   KRT1+3,X'01'                                                     
         XC    KRT1+4(L'KRT1-4),KRT1+4                                          
         CLI   KRT1+2,C'*'         SEE IF MEDIA * OR C                          
         BE    CHKCAN5                                                          
         CLI   KRT1+2,C'C'                                                      
         BE    CHKCAN5                                                          
         B     CHKCAN8                                                          
*                                                                               
CHKCAN5  MVI   KRT1+2,C'M'          TRY FOR MAGAZINES                           
*                                                                               
CHKCAN8  GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BNH   CHKNO                                                            
         LA    R2,PRTREC                                                        
         USING PAGYRECD,R2                                                      
         CLI   PAGYNAT,C'C'                                                     
         BNE   CHKNO                                                            
         B     CHKYES                                                           
CHKNO    LA    R1,1                                                             
         B     *+8                                                              
CHKYES   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXITVAL                                                          
         DROP  R2                                                               
*                                                                               
BILLTVAL NTR1                 X'02'= ALL                                        
*                             X'04'= REG/AOR/RET/FIN                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   BILLTVX        ALL OR MISSING                                    
         CLI   IFLDH+5,3                                                        
         BH    BILLTVE                                                          
         LA    R4,TYPTAB                                                        
BILLTV5  CLI   0(R4),X'FF'                                                      
         BE    BILLTVE                                                          
         CLC   IFLD(3),0(R4)                                                    
         BE    BILLTVO                                                          
         LA    R4,3(R4)                                                         
         B     BILLTV5                                                          
*                                                                               
BILLTVE  MVC   FERN,=AL2(FLDINV)                                                
         B     BILLTVX                                                          
BILLTVO  OI    FIND,X'04'                                                       
         MVC   RPUB(3),IFLD                                                     
*                                                                               
BILLTVX  B     EXITVAL                                                          
*                                                                               
TYPTAB   DC    C'REG'                                                           
         DC    C'AOR'                                                           
         DC    C'RET'                                                           
         DC    C'FIN'                                                           
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
BILLNUM  NTR1                 X'04'= NNNNNNN OR NNNNNN,NNNNNN                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   OPTX                                                             
         CLI   DDS,1              DDS ONLY                                      
         BNE   BILLINV                                                          
*                                                                               
         LA    R5,PRTREC+500                                                    
         USING SCAND,R5                                                         
         LA    R4,IFLDH                                                         
         MVC   PLIST+16(4),=X'0A001900'    OVERIDE LENGTHS TO 10 AND 25         
         GOTO1 SCANNER,PLIST,(R4),(10,PRTREC+500),0,0                           
         ZIC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    BILLX                                                            
*                                                                               
         CLC   RNUM,=C'07'                                                      
         BE    BILLN#07                                                         
*                                                                               
         CLI   FLD1LEN,6           1ST NUMBER                                   
         BNE   BILLINV                                                          
         TM    FLD1VAL,X'80'       NUMERIC                                      
         BNO   BILLINV                                                          
         MVC   RCARD2+20(6),FLD1                                                
*                                                                               
         LA    R5,47(R5)           BUMP TO 2ND SCANNER OUT FIELD                
         CLI   FLD1LEN,0           2ND NUMBER                                   
         BE    BILLOK                                                           
         CLI   FLD1LEN,6                                                        
         BNE   BILLINV                                                          
         TM    FLD1VAL,X'80'                                                    
         BNO   BILLINV                                                          
         CLC   FLD1(2),RCARD2+20                                                
         BL    BILLINV                                                          
         MVC   RCARD2+26(6),FLD1                                                
         B     BILLOK                                                           
*                                                                               
BILLN#07 DS    0H                                                               
         CHI   R1,1               ONLY ONE INVOICE ALLOWED                      
         BNE   BILLINV                                                          
         CLI   FLD1LEN,4          ONLY 1 INVOICE (LAST 4 DIGITS)                
         BNE   BILLINV                                                          
         TM    FLD1VAL,X'80'       NUMERIC                                      
         BNO   BILLINV                                                          
         ICM   R0,15,FLD1B         HAS TO BE POSITIVE                           
         BNP   BILLINV                                                          
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),FLD1                                                     
*        LA    R5,47(R5)           BUMP TO 2ND SCANNER OUT FIELD                
*        CLI   FLD1LEN,0           SHOULD BE NO 2ND NUMBER                      
*        BNE   BILLINV                                                          
         B     BILLOK                                                           
*                                                                               
BILLOK   OI    FIND,X'04'                                                       
         B     BILLX                                                            
*                                                                               
BILLINV  MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
BILLX    B     EXITVAL                                                          
*                                                                               
         EJECT                                                                  
OVRVAL   NTR1                 X'04'= N OR Y                                     
*                             X'08'= 0-200                                      
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   OVRVX                                                            
         CLI   IFLDH+5,1                                                        
         BNE   OVRV20                                                           
         CLI   IFLD,C'Y'                                                        
         BE    OVRV10                                                           
         CLI   IFLD,C'N'                                                        
         BNE   OVRV20                                                           
*                                                                               
OVRV10   OI    FIND,X'04'                                                       
         MVC   TEMP(1),IFLD                                                     
         B     OVRVO                                                            
*                                                                               
OVRV20   TM    4(R4),X'08'         TEST NUMERIC                                 
         BO    OVRV25                                                           
         MVC   FERN,=AL2(NUMINV)    NOT NUMERIC                                 
         B     OVRVX                                                            
*                                                                               
OVRV25   BCTR  R5,R0                                                            
         EX    R5,OVRPACK                                                       
         CP    DUB,=P'200'                                                      
         BNH   OVRV30                                                           
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     OVRVX                                                            
*                                                                               
OVRPACK  PACK  DUB,IFLD(0)           EXECUTED                                   
*                                                                               
OVRV30   DS    0H                                                               
         CVB   R0,DUB                                                           
         STC   R0,TEMP                                                          
         OI    FIND,X'08'                                                       
         B     OVRVO                                                            
*                                                                               
OVRVO    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),TEMP                                                     
*                                                                               
OVRVX    B     EXITVAL                                                          
         EJECT                                                                  
*        VALIDATE AGENCY FILTER  (AOR REPORTS)                                  
*                                                                               
*        X'02' = ALL                                                            
*        X'04' = VALID FILTER                                                   
*        X'08' = VALID AGENCY                                                   
*                                                                               
AGYFVAL  NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BNE   AGYFX               FILTER ALL OR MISSING                        
*                                                                               
AGYF3B4  DS    0H                                                               
         CLI   IFLDH+5,2                                                        
         BL    AGYFE1                                                           
         CLI   IFLDH+5,2           SEE IF 2 CHARACTERS                          
         BE    AGYF6               ASSUME IT IS AN AGENCY CODE                  
         CLI   IFLDH+5,6                                                        
         BH    AGYFE1              MAX IS -X-X-X                                
         ZIC   R1,IFLDH+5          SAVE INPUT LENGHT                            
         LA    R4,IFLD                                                          
AGYF3B6  LA    R5,3                FOR BCT                                      
         XC    TEMP(5),TEMP                                                     
         LA    R6,TEMP+2                                                        
AGYF3C   EQU   *                                                                
         CLI   0(R4),C'-'          SEE IF NEGATIVE FILTER                       
         BNE   AGYF3E              NO                                           
         MVI   TEMP,1                                                           
         LA    R4,1(R4)            PAST -                                       
         BCTR  R1,0                DECREMENT COUNTER                            
AGYF3E   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   AGYF3F                                                           
         CLI   TEMP,0                                                           
         BNE   AGYFE1              -* IS INVALID                                
         B     AGYF3G                                                           
*                                                                               
AGYF3F   CLI   0(R6),C'A'                                                       
         BL    AGYFE1                                                           
         CLI   0(R6),C'9'                                                       
         BH    AGYFE1                                                           
         CLI   TEMP,1            SEE IF NEGATIVE FILETR                         
         BNE   *+8                                                              
         NI    0(R6),X'BF'         SET OFF X'40'                                
AGYF3G   LA    R6,1(R6)                                                         
         MVI   TEMP,0            ZERO NEGATIVE INDICATOR                        
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R5,AGYF3C                                                        
         LTR   R1,R1                                                            
         BNZ   AGYFE1              SHOULD HAVE NO MORE INPUT                    
         MVI   FIND,X'04'          VALID FILTER                                 
         B     AGYFO                                                            
*                                                                               
AGYF6    DS    0H                                                               
         CLI   IFLD,C'A'           MUST BE ALPHA                                
         BL    AGYFE1                                                           
         CLI   IFLD+1,C'A'                                                      
         BL    AGYFE1                                                           
         CLI   IFLD,C'Z'                                                        
         BH    AGYFE1                                                           
         CLI   IFLD+1,C'Z'                                                      
         BH    AGYFE1                                                           
*                                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),IFLD                                                     
         MVI   FIND,X'08'           AGENCY CODE                                 
         B     AGYFX                                                            
*                                                                               
*                                                                               
AGYFO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),TEMP+2                                                   
*                                                                               
AGYFE1   MVI   FERN,FLDINV                                                      
AGYFX    DS    0H                                                               
         B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
FBCDVAL  NTR1                      FOREIGN BANK CODE VALIDATION                 
*                                                                               
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
         CLI   FIND,1                                                           
         BL    FBCDVXX             NO INPUT IS OK                               
         CHI   R5,2                                                             
         BNE   FBCDV30             IMPUT MUST BE EXACTLY 2 CHARS                
         TM    4(R4),X'08'                                                      
         BO    FBCDV50             INPUT MUST BE NUMERIC                        
         MVC   FERN,=AL2(NUMINV)                                                
         B     *+10                                                             
FBCDV30  MVC   FERN,=AL2(FLDINV)                                                
         B     FBCDVXX                                                          
*                                                                               
FBCDV50  DS    0H                                                               
*                                                                               
         SR    R7,R7                                                            
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         CLI   0(R7),C' '          SEE IF DATA ALREADY THERE                    
         BNE   FBCDV30                                                          
         CLI   1(R7),C' '          SEE IF DATA ALREADY THERE                    
         BNE   FBCDV30                                                          
         MVC   0(2,R7),8(R4)                                                    
         OI    FIND,X'04'          FIELD HAS BEEN VALIDATED                     
*                                                                               
FBCDVXX  B     EXITVAL                                                          
*                                                                               
*                                                                               
MEDVAL   NTR1                      VALIDATE COPY MEDIA FOR AGENCY               
*                                                                               
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
         CLI   FIND,1                                                           
         BL    MEDV40              MISSING INPUT                                
         CLC   RMED,8(R4)                                                       
         BNE   MEDV90              I/P MEDIA MUST DIFFER FROM O/P               
*                                                                               
MEDV30   MVC   FERN,=AL2(MEDINV)                                                
         B     MEDVXX                                                           
*                                                                               
MEDV40   MVC   FERN,=AL2(FLDMIS)                                                
         B     MEDVXX                                                           
*                                                                               
MEDV90   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),RAGY         AGENCY                                       
         MVC   KEY+2(1),8(R4)      MEDIA TO BE COPIED TO                        
         MVI   KEY+3,X'01'         AGENCY RECORD CODE                           
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   MEDV30              O/P MEDIA MUST BE FOUND                      
*                      **********  "RESTORE" AGENCY  **********                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),RAGY         AGENCY                                       
         MVC   KEY+2(1),RMED       MEDIA TO BE COPIED                           
         MVI   KEY+3,X'01'         AGENCY RECORD CODE                           
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                 AGENCY MUST BE FOUND                         
         DC    H'0'                                                             
*                                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
         OI    FIND,X'04'          FIELD HAS BEEN VALIDATED                     
*                                                                               
MEDVXX   B     EXITVAL                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SCHMCVAL NTR1                      VALIDATE SCHEME CODE FIELD                   
*                                                                               
         GOTO1 AINITV              SET R4=A(HDR) & R5=L'DATA                    
*                                                                               
         CLI   FIND,1                                                           
         BL    SCHMCV40            MISSING INPUT                                
*                                                                               
         CLI   RO1,C'D'            CLT GROUP?                                   
         BE    SCHMCV35                                                         
         CLI   RO1,C'E'            PRD GROUP?                                   
         BE    SCHMCV35                                                         
         CLI   RO1,C'F'            PUB GROUP?                                   
         BE    SCHMCV35                                                         
         MVC   FERN,=AL2(RTYPERR1)                                              
         B     EXITVAL                                                          
*                                                                               
SCHMCV35 TM    4(R4),X'04'         VALID ALPHA?                                 
         BO    *+14                                                             
         MVC   FERN,=AL2(ALPINV)   NOT VALID ALPHA INPUT                        
         B     EXITVAL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),RAGY         AGENCY                                       
         MVC   KEY+2(1),RMED       MEDIA                                        
         MVC   KEY+7(1),IFLD       SCHEME CODE TO BE LOOKED UP                  
*                                                                               
         CLI   RO1,C'D'            CLT GROUP?                                   
         BNE   *+8                                                              
         MVI   KEY+3,X'34'         CLT GROUP RECORD CODE                        
         CLI   RO1,C'E'            PRD GROUP?                                   
         BNE   *+14                                                             
         MVI   KEY+3,X'35'         PRD GROUP RECORD CODE                        
         MVC   KEY+4(3),RCLI       PRD GROUP REC NEEDS CLIENT CODE              
         CLI   RO1,C'F'            PUB GROUP?                                   
         BNE   *+8                                                              
         MVI   KEY+3,X'36'         PUB GROUP RECORD CODE                        
*                                                                               
         CLI   KEY+3,0                                                          
         BNE   *+6                                                              
         DC    H'0'                WRONG RECORD TYPE!                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(25),KEYSAVE     FOUND SCHEME CODE?                           
         BE    *+14                                                             
         MVC   FERN,=AL2(SCHMERR)  SCHEME CODE NOT ON FILE ERR MSG              
         B     EXITVAL                                                          
*                                                                               
SCHMCV37 LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD        SCHEME CODE INTO REQUEST CARD                
         OI    FIND,X'04'          FIELD HAS BEEN VALIDATED                     
         B     EXITVAL                                                          
*                                                                               
SCHMCV40 DS    0H                                                               
         CLI   RO1,C'D'            CLT GROUP?                                   
         BE    SCHMCV43                                                         
         CLI   RO1,C'E'            PRD GROUP?                                   
         BE    SCHMCV43                                                         
         CLI   RO1,C'F'            PUB GROUP?                                   
         BNE   EXITVAL                                                          
SCHMCV43 MVI   FIND,1              FAKE ERROR                                   
         MVC   FERN,=AL2(FLDMIS)                                                
         B     EXITVAL                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ROUTADRT DC    F'0'                   00                                        
         DC    A(OPTIONS)             01 - OPTIONS                              
         DC    A(BILLTVAL)            02 - BILLING TYPE                         
         DC    A(BILLNUM)             03 - BILL NUMBER(S)                       
         DC    A(OVRVAL)              04 - OVERAGE                              
         DC    A(AGYFVAL)             05 - AGENCY FILTER                        
         DC    A(FBCDVAL)             06 - FOREIGN BANK CODE                    
         DC    A(MEDVAL)              07 - MEDIA FOR AGENCY                     
         DC    A(SCHMCVAL)            08 - SCHEME CODE                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
WORK     DS    CL80                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DDSFMT   EQU   X'04'                                                            
*                                                                               
FLDMIS   EQU   001                                                              
ONEINV   EQU   002                                                              
FLDINV   EQU   002                                                              
NUMINV   EQU   003                 NUMERIC                                      
ALPINV   EQU   004                 NOT VALID ALPHA                              
MEDINV   EQU   013                                                              
DTEINV   EQU   020                                                              
SEDINV   EQU   020                                                              
RTYPERR1 EQU   048                 RECORD TYPE MUST BE "D,E,F"                  
RECNOF   EQU   053                 RECORD NOT FOUND                             
SCHMERR  EQU   059                 SCHEME CODE NOT ON FILE                      
SEDBIG   EQU   069                                                              
SEDSGE   EQU   080                                                              
PDNALL   EQU   166                                                              
ESNALL   EQU   167                                                              
NOGROSS  EQU   168                                                              
CAERROR  EQU   207                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
SCAND    DSECT                     COVER SCANNER LINES                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL25                                                             
         EJECT                                                                  
*                                                                               
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
         SPACE 2                                                                
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FLDIND                                                         
       ++INCLUDE PRREQFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PRREQ05   01/09/19'                                      
         END                                                                    
