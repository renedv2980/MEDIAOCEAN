*          DATA SET PRREQ05S   AT LEVEL 037 AS OF 05/01/02                      
*PHASE T41205A,+0                                                               
*   CHANGE LOG                                                                  
*   NEW PROGRAM 7/11/90                                                         
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
         LA    RF,320                                                           
         XCEF                                                                   
         LA    R5,PRTREC+500                                                    
         USING SCAND,R5                                                         
         L     R4,FLDHADR                                                       
         MVI   6(R4),X'80'         SET OFF HILITE AND TRANSMIT                  
         MVI   7(R4),0             SET OFF OUTPUT LENGTH                        
         GOTO1 SCANNER,PLIST,(R4),(10,PRTREC+500)                               
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
         LA    R5,32(R5)                                                        
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
VAL12N   LA    R5,32(R5)                                                        
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
*NOP*    CLI   FLD2LEN,1                                                        
*NOP*    BNE   OPTINV                                                           
*NOP*    CLI   FLD2,C'H'                                                        
*NOP*    BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO8,C'H'            (REQ2,COL62)                                 
         B     VAL18N                                                           
*                                                                               
VAL18D   B     OPTINV                                                           
*                                                                               
VAL18N   LA    R5,32(R5)                                                        
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
*NOP*    CLI   FLD2LEN,1                                                        
*NOP*    BNE   OPTINV                                                           
*NOP*    CLI   FLD2,C'H'                                                        
*NOP*    BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO9,C'H'            (REQ2,COL63)                                 
         B     VAL19N                                                           
*                                                                               
VAL19D   B     OPTINV                                                           
*                                                                               
VAL19N   LA    R5,32(R5)                                                        
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
         BNE   OPTINV                                                           
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
*****    B     VAL48X                                                           
*                                                                               
VAL48X   LA    R5,32(R5)                                                        
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
*NOP*    CLI   FLD2LEN,1                                                        
*NOP*    BNE   OPTINV                                                           
*NOP*    CLI   FLD2,C'H'                                                        
*NOP*    BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO8,C'H'            (REQ2,COL62)                                 
         B     VAL52XX                                                          
*                                                                               
*                                                                               
*                                                                               
VAL52D   DS    0H                  VALIDATE COST2 FACTOR OPTION                 
*                                                                               
         CLI   FLD1LEN,4                                                        
         BNE   VAL52E                                                           
         CLC   =C'COS2',FLD1                                                    
         BNE   VAL52E              GO VALIDATE NEXT OPTION FLD                  
         CLI   FLD2,C'Y'                                                        
         BNE   OPTINV                                                           
         OI    FIND,X'08'                                                       
         MVI   RO6,C'O'            REQ6=O FOR OPEN                              
         B     VAL52XX                                                          
*                                                                               
*                                                                               
*                                                                               
VAL52E   DS    0H                  FOR FUTURE USE                               
*                                                                               
*                                                                               
*                                                                               
VAL52X   B     OPTINV                                                           
*                                                                               
VAL52XX  LA    R5,32(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,VAL52                                                         
         B     OPTX                                                             
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
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
MYV100   LA    R5,32(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R7,MYVAL                                                         
         B     OPTX                                                             
*                                                                               
VALB1    DS    0H                                                               
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
         BNE   OPTINV                                                           
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
VALB1X   DS    0H                                                               
         LA    R5,32(R5)                                                        
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
         XC    TEMP(60),TEMP                                                    
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
         XC    8(50,R4),8(R4)                                                   
         MVC   8(23,R4),=C' *** FOR FUTURE USE ***'   TEMP                      
         OI    6(R4),X'80'         TEMP                                         
         B     OPTX                TEMP                                         
*                                         CANADIAN                              
HLP10    MVC   8(10,R4),=C'  FR      '                                          
         OI    6(R4),X'88'                                                      
         CLC   RNUM,=C'12'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'18'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'19'                                                      
         BE    HLP11                                                            
         CLC   RNUM,=C'48'                                                      
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
OPTX     XIT1                                                                   
         EJECT                                                                  
HELPTBL  DS    0F                                                               
*NOP*    DC    AL1(10),C'B1',C'  ERR=Y'                                         
         DC    AL1(10),C'D1',C'  ERR=Y'                                         
         DC    AL1(11),C'EC',C'  NOHELD'                                        
         DC    AL1(12),C'12',C'  EST=NNN'                                       
         DC    AL1(11),C'18',C'  NOHELD'                                        
         DC    AL1(11),C'19',C'  NOHELD'                                        
         DC    AL1(19),C'48',C'  ID=    UNA=Y/N'                                
         DC    AL1(15),C'52',C'D=    NOHELD'                                    
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
         XIT1                                                                   
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
BILLTVX  XIT1                                                                   
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
         GOTO1 SCANNER,PLIST,(R4),(10,PRTREC+500)                               
         ZIC   R1,PLIST+4                                                       
         LTR   R1,R1                                                            
         BZ    BILLX                                                            
         CLI   FLD1LEN,6           1ST NUMBER                                   
         BNE   BILLINV                                                          
         TM    FLD1VAL,X'80'       NUMERIC                                      
         BNO   BILLINV                                                          
         MVC   RCARD2+20(6),FLD1                                                
*                                                                               
         LA    R5,32(R5)           BUMP TO 2ND SCANNER OUT FIELD                
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
BILLOK   OI    FIND,X'04'                                                       
         B     BILLX                                                            
*                                                                               
BILLINV  MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
BILLX    XIT1                                                                   
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
OVRVX    XIT1                                                                   
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
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM                            
*                                                                               
ROUTADRT DC    F'0'                   00                                        
         DC    A(OPTIONS)             01 - OPTIONS                              
         DC    A(BILLTVAL)            02 - BILLING TYPE                         
         DC    A(BILLNUM)             03 - BILL NUMBER(S)                       
         DC    A(OVRVAL)              04 - OVERAGE                              
         DC    A(AGYFVAL)             05 - AGENCY FILTER                        
*                                                                               
         LTORG                                                                  
*                                                                               
DDSFMT   EQU   X'04'                                                            
ONEINV   EQU   2                                                                
FLDINV   EQU   2                                                                
NUMINV   EQU   3        NUMERIC                                                 
CONNOF   EQU   53       RECORD NOT FOUND                                        
DTEINV   EQU   20                                                               
PDNALL   EQU   166                                                              
ESNALL   EQU   167                                                              
NOGROSS  EQU   168                                                              
SEDSGE   EQU   80                                                               
SEDBIG   EQU   69                                                               
SEDINV   EQU   20                                                               
         EJECT                                                                  
*                                                                               
SCAND    DSECT                                                                  
*        DESCT TO COVER SCANNER LINES                                           
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
         EJECT                                                                  
*                                                                               
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
         SPACE 2                                                                
       ++INCLUDE FLDIND                                                         
       ++INCLUDE PRREQFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PRREQ05S  05/01/02'                                      
         END                                                                    
