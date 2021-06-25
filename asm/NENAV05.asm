*          DATA SET NENAV05    AT LEVEL 095 AS OF 11/18/20                      
*PHASE T31805B                                                                  
T31805   TITLE 'NENAV06 - STEWARD CABLE PROGRAM RECORD ADD PROGRAM'             
T31805   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV05**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
*  GET SECURITY AGENCY AND PASSWORD                                             
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         MVC   SECAGY,FATAGYSC     SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
*  READ PROGRAM RECORD REQUEST FROM TSAR AND PROCESS                            
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
         B     BDPRG040                                                         
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
BDPRG020 MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
*                                                                               
BDPRG040 BAS   RE,GETSTA            GET MARKET NUMBER                           
         CLI   ERROR,0                                                          
         BNE   BDPRG100                                                         
         BAS   RE,DOPROG            CREATE PROGRAM CODE/ROTATION                
         CLI   ERROR,0                                                          
         BNE   BDPRG100                                                         
*                                                                               
         BAS   RE,GETPROG           SEE IF RECORD ALREADY EXISTS                
         CLI   PGLSTLIN,0                                                       
         BE    BDPRG100                                                         
*                                                                               
         BAS   RE,ADDPROG                                                       
BDPRG100 BAS   RE,SENDATA                                                       
         B     BDPRG020                                                         
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*--BUILD A PROGRAM RECORD                                                       
*                                                                               
DOPROG NTR1                                                                     
         MVI   ERROR,0                                                          
*                                                                               
         L     R5,ANETBLK                                                       
         USING CPRGRECD,R5                                                      
*                                                                               
         MVC   PGRNAME,CPRPNAM                                                  
         OC    PGRNAME,SPACES                                                   
         MVC   PGRSTIME(4),CPRSTIM                                              
         MVI   PGRMIROR,0                                                       
         CLI   CPRMIR,X'40'                                                     
         BNH   *+10                                                             
         MVC   PGRMIROR,CPRMIR                                                  
*                                                                               
         MVC   PGRBOOK(2),=XL2'580E'                                            
* UNLESS THE AGENCY IS EXPLICITLY SET UP AS 1 BYTE DEMO                         
* PRECISION SET THE PROGRAM RECORD AS 2 BYTE DEC PRECISSION                     
         TM    SVAGYFL2,AGYFLAG2_BDP                                            
         BO    DOPRG050                                                         
         TM    SVAGYFL2,AGYFLAG2_2DP                                            
         BZ    *+10                                                             
DOPRG050 MVC   PGRBOOK(2),=XL2'5901'                                            
*                                                                               
*--CALCULATE PROGRAM CODE                                                       
         MVI   PGRCODE+3,X'FF'                                                  
         XC    PGRCODE(3),PGRCODE                                               
         LA    RE,CPRPNAM                                                       
         LA    RF,PGRCODE                                                       
         LA    R1,16                                                            
*                                                                               
*--FIRST AREA CHECKS FOR BYPASS NAMES                                           
DOPRG060 LA    R2,NAMETAB                                                       
DOPRG070 CLI   0(R2),X'FF'         END OF NAME TABLE                            
         BE    DOPRG100            YES CHECK FOR VOWELS                         
         ZIC   R6,8(R2)            MOVE IN LENGTH OF COMPARE                    
         EX    R6,NAMECOMP                                                      
         BNE   DOPRG080                                                         
         LA    R6,1(R6)                                                         
         AR    RE,R6               BUMP PROGRAM NAME TO NEXT WORD               
         SR    R1,R6               SUBTRACT LOOP COUNT BY WORD BUMP             
         LTR   R1,R1                                                            
         BZ    DOPRG260            END OF LITERAL EXIT ROUTINE                  
         BP    DOPRG060            CHECK NEXT WORD                              
         DC    H'0'                WENT PAST LITERAL                            
DOPRG080 LA    R2,9(R2)                                                         
         B     DOPRG070                                                         
*                                                                               
*--THIS AREA CHECKS FOR TO BYPASS VOWELS AND SPECIAL VALUES                     
DOPRG100 LA    R2,VOWELLST                                                      
DOPRG120 CLI   0(RE),X'40'         CHECK FOR WORD BREAK                         
         BNE   DOPRG140                                                         
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCTR  R1,0                SUBTRACT FROM COUNT                          
         LTR   R1,R1                                                            
         BZ    DOPRG260            END OF LITERAL EXIT ROUTINE                  
         B     DOPRG060            CHECK AGAINST WORD TABLE                     
*                                                                               
DOPRG140 CLI   0(RE),X'C1'         CHECK CHARACTER FOR ALPHA/NUM                
         BL    DOPRG180                                                         
         CLI   0(RE),X'F9'                                                      
         BH    DOPRG180                                                         
*                                                                               
DOPRG160 CLI   0(R2),X'FF'         END OF VOWEL TABLE                           
         BE    DOPRG200            MOVE LETTER TO CODE                          
         CLC   0(1,RE),0(R2)                                                    
         BE    DOPRG180            LETTER IS A VOWEL, BYPASS                    
         LA    R2,1(R2)                                                         
         B     DOPRG160                                                         
*                                                                               
DOPRG180 LA    RE,1(RE)                                                         
         BCT   R1,DOPRG100                                                      
         B     DOPRG260                                                         
*                                                                               
DOPRG200 CLI   0(RF),X'FF'                                                      
         BE    DOPRG260            CODE FIELD IS FILLED                         
         MVC   0(1,RF),0(RE)       MOVE NEXT LETTER OF CODE IN                  
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DOPRG100                                                      
*                                                                               
*--THE LITERAL PORTION OF THE PROGRAM CODE MUST BE                              
*--1 POSITIONS LONG. IF IT IS NOT AN ERROR CODE IS                              
*--AND THE PROGRAM BYPASSES THE UNITS UNDER THAT CODE.                          
DOPRG260 MVC   PGRCODE+3(3),=XL3'404040'                                        
         CLI   PGRCODE,X'00'                                                    
         BNE   DOPRG280                                                         
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXTRA(12),=CL12'PROGRAM NAME'                                  
         B     DOPRGX                                                           
         DROP  R6                                                               
*                                                                               
DOPRG280 MVC   PGRCODE+3(3),=XL3'F0F0F0'                                        
         CLI   PGRCODE+2,0                                                      
         BNE   DOPRG300                                                         
*        BNE   DOPRG285                                                         
         MVI   PGRCODE+2,X'F0'                                                  
         CLI   PGRCODE+1,0                                                      
         BNE   DOPRG300                                                         
*        BNE   DOPRG285                                                         
         MVI   PGRCODE+1,X'F0'                                                  
         SPACE 3                                                                
*                                                                               
* BELOW IS COMMENTED OUT SO WE CAN DO A SOFT LOOK UP FOR NEXT                   
* AVAILABLE PROGRAM WHEN WE HIT 999 THRESHOLD.  CODE IN GETPR090.               
*                                                                               
*&&DO                                                                           
*--NAMES THAT HAVE BEEN FILLED CONVERSION TABLE                                 
*--IF PROGRAM COUNT ALREADY UP TO 999 A CONVERSION IS NEEDED                    
DOPRG285 LA    RE,CONVTAB                                                       
DOPRG290 CLI   0(RE),X'FF'                                                      
         BE    DOPRG300                                                         
         CLC   PGRCODE(3),0(RE)                                                 
         BE    DOPRG295                                                         
         LA    RE,6(RE)                                                         
         B     DOPRG290                                                         
DOPRG295 MVC   PGRCODE(3),3(RE)                                                 
*&&                                                                             
*--CALCULATE ROTATION                                                           
DOPRG300 XC    PGRROT(2),PGRROT    CLEAR THE DAY FIELDS                         
         MVC   PGRROTE,CPRROT                                                   
         LA    R1,ROTTABLE                                                      
         LA    RE,CPRROT                                                        
DOPRG320 CLI   0(R1),X'FF'                                                      
         BE    DOPRG350                                                         
         CLI   0(RE),C'Y'                                                       
         BNE   DOPRG340                                                         
         OC    PGRROT,2(R1)                                                     
         NI    PGRROTN,X'F0'       CLEAR END DAT NUMBER                         
         OC    PGRROTN,5(R1)       END DAY NUMBER                               
         TM    PGRROTN,X'F0'                                                    
         BNZ   DOPRG340                                                         
         NI    PGRROTN,X'0F'       CLEAR START DAY NUMBER                       
         OC    PGRROTN,4(R1)       START DAY NUMBER                             
         MVC   PGDAYNO,5(R1)      START DAY NUMBER (NUMERIC)                    
         MVC   PGDAYHEX,3(R1)     START DAY NUMBER (HEX)                        
DOPRG340 LA    R1,6(R1)                                                         
         LA    RE,1(RE)                                                         
         B     DOPRG320                                                         
*                                                                               
DOPRG350 CLI   PGDAYNO,0                                                        
         BNE   DOPRG360                                                         
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXTRA(8),=CL8'ROTATION'                                        
         B     DOPRGX                                                           
         DROP  R6                                                               
         SPACE 2                                                                
*-CHECK START AND END FOR GREATER THEN 2400                                     
DOPRG360 CLC  CPRETIM,=XL2'0960'                                                
         BH   DPRENERR                                                          
*                                                                               
         CLC  CPRSTIM,=XL2'0960'                                                
         BH   DPRSTERR                                                          
*-CHECK START AND END FOR ZERO                                                  
         OC   CPRETIM,CPRETIM                                                   
         BZ   DPRENERR                                                          
*                                                                               
         OC   CPRSTIM,CPRSTIM                                                   
         BZ   DPRSTERR                                                          
*                                                                               
*-EXIT                                                                          
DOPRGX   B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  START TIME ERROR                                                             
DPRSTERR DS    0H                                                               
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXTRA(10),=CL10'START TIME'                                    
         B     DOPRGX                                                           
*                                                                               
*  END TIME ERROR                                                               
DPRENERR DS    0H                                                               
         LA    R6,WORK2                                                         
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXTRA(10),=CL10'END TIME'                                      
         B     DOPRGX                                                           
         DROP  R6                                                               
*                                                                               
NAMECOMP CLC   0(0,RE),0(R2)       FOR TEST ON NAME TABLE                       
         DROP  R5                                                               
*&&DO                                                                           
CONVTAB  DC    CL6'NCBNCD'                                                      
         DC    CL6'CNNCNX'                                                      
         DC    CL6'SPRSPJ'                                                      
         DC    CL6'CLSCLZ'                                                      
         DC    CL6'PRMPRB'                                                      
         DC    CL6'RSTRSF'                                                      
         DC    CL6'XGMXGX'                                                      
         DC    CL6'MLBMLX'                                                      
         DC    CL6'NFLNFY'                                                      
         DC    CL6'SPNSPH'                                                      
         DC    CL6'SMVSMX'                                                      
         DC    CL6'FXMFXG'                                                      
         DC    CL6'DSCDSZ'                                                      
         DC    CL6'NSCNSX'                                                      
         DC    CL6'FTRFTX'                                                      
         DC    CL6'SCCSCB'                                                      
         DC    CL6'PGTPGX'                                                      
         DC    CL6'CLLCLX'                                                      
         DC    CL6'FXXFXQ'                                                      
         DC    CL6'BBCBBZ'                                                      
         DC    CL6'MNFMNZ'                                                      
         DC    CL6'WKNWKZ'                                                      
         DC    CL6'TRSTRZ'                                                      
         DC    CL6'FRNFRA'                                                      
         DC    CL6'BRVBRB'                                                      
         DC    CL6'SCRSCT'                                                      
         DC    X'FF'                                                            
*&&                                                                             
NAMETAB  DC    CL8'THE     ',XL1'03'                                            
         DC    X'FF'                                                            
VOWELLST DC    CL5'AEIOU'                                                       
         DC    X'FF'                                                            
*                                                                               
ROTTABLE DC    CL2'MO',XL4'40401001'         MON                                
         DC    CL2'TU',XL4'20202002'         TUE                                
         DC    CL2'WE',XL4'10103003'         WED                                
         DC    CL2'TH',XL4'08084004'         THU                                
         DC    CL2'FR',XL4'04045005'         FRI                                
         DC    CL2'SA',XL4'02026006'         SAT                                
         DC    CL2'SU',XL4'01017007'         SUN                                
         DC    CL2'MF',XL4'7C401001'         MON-FRI                            
         DC    CL2'WK',XL4'7F401001'         MON-SUN                            
         DC    X'FF'                                                            
*                                                                               
CONSTLST DC    CL21'BCDFGHJKLMNPQRSTVWXYZ'                                      
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*--READ A PROGRAM RECORD THAT FITS THE UNITS TO BE ADDED.                       
*--IF FOUND FILL FIELD PGRCODE WITH THE PROGRAM CODE.                           
*--IF NONE FOUND PASS A X'FF' IN TWLSTLIN.                                      
GETPROG  NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING CPRGRECD,R5                                                      
         MVI   PGLSTLIN,0                                                       
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,PGMARKET                                                 
         MVC   NPGKPROG(3),PGRCODE                                              
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     GETPR040                                                         
*                                                                               
GETPR020 GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
GETPR040 CLC   KEY(8),KEYSAVE                                                   
         BNE   GETPR190                                                         
*                                                                               
*--FOR KEY TO QUALIFY LAST THEREE POSITIONS                                     
*--OF PROGRAM CODE MUST BE CHAR. NUMERIC                                        
         TM    KEY+8,X'F0'                                                      
         BNO   GETPR080                                                         
         TM    KEY+9,X'F0'                                                      
         BNO   GETPR080                                                         
         TM    KEY+10,X'F0'                                                     
         BNO   GETPR080                                                         
         MVC   PGRCODE+3(3),KEY+8   LINE COUNT NUMBER                           
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, ROTATION,TIME TO SEE IF                     
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
         L     R3,AIO2                                                          
         USING NPGRECD,R3                                                       
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         BAS   RE,TESTPGM          TEST PROGRAM RECORD                          
         JE    GETPR200            YES - QUALIFIES, USE IT                      
*                                                                               
GETPR080 CLC   NPGKPROG+3(3),=C'999'  NO - DOESN'T QUALIFY, TRY NEXT            
         BNE   GETPR020                                                         
*                                                                               
* PROGRAM CODE 999 ENCOUNTERED.  CHANGE 3RD CHAR TO ANOTHER LETTER              
* AND FIND AVAIALBLE PROGRAM CODE.  IF NONE AVAILABLE, SKIP IT AND TRY          
* NEXT CHAR.  IF ALL 3RD CHAR PROGRAM CODES EXIST, TRY 2ND CHAR.                
*                                                                               
GETPR090 LA    R6,CONSTLST         POSSIBLE CHARACTERS                          
GETPR095 CLI   0(R6),X'FF'                                                      
         JE    GETPR150            ALL 3RD CHAR EXISTS, TRY 2ND CHAR            
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
* START WITH 3RD CHAR.  LOOK FOR EACH LETTER AND READ THROUGH UNTIL             
* IT FINDS AN AVAILABLE ###.  IF IT HITS 999, THEN GET NEXT LETTER.             
*                                                                               
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,PGMARKET                                                 
         MVC   NPGKPROG(3),PGRCODE                                              
         MVC   NPGKPROG+2(1),0(R6) OVERWRITE 3RD CHAR                           
         MVI   NPGKPROG+3,C'0'                                                  
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(8),KEYSAVE      DOES IT EXIST FOR LETTER?                    
         BE    *+14                                                             
         MVC   PGRCODE+3(3),=C'000'   NO - CAN USE IT                           
         B     GETPR120                                                         
*                                                                               
GETPR100 GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ   YES - FIND FIRST AVAILABLE            
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+14                                                             
         MVC   PGRCODE+3(3),KEYSAVE+8     FOUND ONE - USE PREV #                
         J     GETPR120                                                         
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, ROTATION,TIME TO SEE IF                     
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
         L     R3,AIO2                                                          
         USING NPGRECD,R3                                                       
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         BAS   RE,TESTPGM          TEST PROGRAM RECORD                          
         JE    GETPR200            YES - QUALIFIES, USE IT                      
*                                                                               
GETPR115 CLC   NPGKPROG+3(3),=C'999'      DID WE REACH 999 FOR CHAR?            
         BNE   GETPR100                   NO - GET NEXT ONE                     
         AHI   R6,1                                                             
         J     GETPR095                                                         
*                                                                               
GETPR120 MVC   PGRCODE+2(1),0(R6)  SET 3RD CHARACTER                            
         J     GETPR190                                                         
*                                                                               
* ALL 3RD CHAR USED, NOW CHANGE 2ND AND 3RD CHARACTERS                          
*                                                                               
GETPR150 LA    R6,CONSTLST         POSSIBLE CHARACTERS                          
GETPR152 CLI   0(R6),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                NO MORE 2ND CHARACTERS LEFT                  
*                                                                               
         LA    R7,CONSTLST         NOW CYCLE THROUGH 3RD CHARACTERS             
GETPR154 CLI   0(R7),X'FF'                                                      
         JNE   *+12                                                             
         AHI   R6,1                NO MORE 3RD CHARACTERS LEFT, TRY             
         J     GETPR152            NEXT 2ND CHARACTER                           
*                                                                               
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,PGMARKET                                                 
         MVC   NPGKPROG(3),PGRCODE                                              
         MVC   NPGKPROG+1(1),0(R6) OVERWRITE 2ND CHAR                           
         MVC   NPGKPROG+2(1),0(R7) OVERWRITE 3RD CHAR                           
         MVI   NPGKPROG+3,C'0'                                                  
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(8),KEYSAVE      DOES IT EXIST FOR THIS LETTER COMBO?         
         BE    GETPR165                                                         
         MVC   PGRCODE+3(3),=C'000'   NO - CAN USE IT                           
         B     GETPR170                                                         
*                                                                               
GETPR160 GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ   YES - FIND FIRST AVAILABLE            
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+14                                                             
         MVC   PGRCODE+3(3),KEYSAVE+8     FOUND ONE - USE PREV #                
         J     GETPR170                                                         
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, ROTATION,TIME TO SEE IF                     
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
GETPR165 L     R3,AIO2                                                          
         USING NPGRECD,R3                                                       
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         BAS   RE,TESTPGM          TEST PROGRAM RECORD                          
         JE    GETPR200            YES - QUALIFIES, USE IT                      
*                                                                               
GETPR169 CLC   NPGKPROG+3(3),=C'999'      DID WE REACH 999 FOR CHAR?            
         BNE   GETPR160                   NO - GET NEXT ONE                     
         AHI   R7,1                TRY NEXT 3RD CHARACTER                       
         J     GETPR154                                                         
*                                                                               
GETPR170 MVC   PGRCODE+1(1),0(R6)  SET 2ND CHARACTER                            
         MVC   PGRCODE+2(1),0(R7)  SET 3RD CHARACTER                            
*                                                                               
GETPR190 MVI   PGLSTLIN,X'FF'                                                   
GETPR200 MVI   ERROR,0                                                          
GETPROGX J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*--IF RECORD DOES EXIST CHECK FIELDS TO SEE IF IT QUALIFIES                     
*--IF NOT EQUAL GET NEXT PROGRAM RECORD                                         
*                                                                               
TESTPGM  NTR1                                                                   
         L     R3,AIO2                                                          
         USING NPGRECD,R3                                                       
*                                                                               
* IN NOV/20, THEY STARTED ADDING 2021 UNITS.  UNFORTUNATELY, PROGRAM            
* RECORDS USED TO BE CREATED WITH A 2020 END DATE.  IT WAS CHANGED              
* TO 2021 BUT HISTORICAL PROGRAM RECORDS ARE STILL CAUSING ISSUES               
* THEREFORE THE FIX IS TO PRETEND THEY DON'T EXIST TO FORCE A NEW               
* PROGRAM RECORD TO BE CREATED.                                                 
*                                                                               
         CLC   NPGKEND,=X'F19F'    END DATE DEC31/20                            
         JNH   NO                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'5D',AIO),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NUBKD,R6                                                         
         CLC   NUBKBOOK(2),PGRBOOK                                              
         JNE   NO                                                               
*                                                                               
*--IF RECORD DOES EXIST CHECK NAME, ROTATION,TIME TO SEE IF                     
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'92',AIO),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
         CLC   PGRROT,NPGROT                                                    
         JNE   NO                                                               
*                                                                               
         OC    NPGNAME,SPACES       CHANGE BLANKS TO SPACES                     
         CLC   NPGNAME,PGRNAME                                                  
         JNE   NO                                                               
*                                                                               
         CLC   NPGTIME(4),CPRSTIM                                               
         JNE   NO                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE '),(X'03',AIO),0                    
         CLI   12(R1),0                                                         
         JNE   TPGM10                                                           
         L     R6,12(R1)                                                        
         USING NPGEL03,R6                                                       
*                                                                               
         CLC   NPGMIRCD,PGRMIROR   CHECK MIRROR CODE MATCH                      
         JNE   NO                                                               
         MVC   PGRCODE,NPGKPROG                                                 
         J     YES                 YES VALID PROGRAM RECORD FOUND               
*                                                                               
* IF NO ELEMENT MAKE SURE MIRROR CODE WAS NOT PASSED                            
TPGM10   OC    PGRMIROR,PGRMIROR      WAS MIRROR CODE PASSED                    
         JNZ   NO                     YES BYAPSS RECORD                         
         MVC   PGRCODE,NPGKPROG                                                 
         J     YES                    VALID PROGRAM RECORD FOUND                
         DROP  R3,R6                                                            
*                                                                               
*--ROUTINE WILL ADD A NEW PROGRAM RECORD.                                       
*--FIELD PGRCODE WILL HAVE THE NEW PROGRAM CODE.                                
*                                                                               
ADDPROG  NTR1                                                                   
         L     R3,AIO2                                                          
         XCEF  (R3),2000                                                        
         USING NPGRECD,R3                                                       
*                                                                               
         PACK  DUB(2),PGRCODE+3(3) CREATE A NEW PROGRAM LINE COUNT              
         AP    DUB(2),=PL1'1'                                                   
         UNPK  PGRCODE+3(3),DUB(2)                                              
         OI    PGRCODE+5,X'F0'                                                  
*                                                                               
*--BUILD NEW PROGRAM CODE                                                       
*                                                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,PGMARKET                                                 
         MVC   NPGKPROG,PGRCODE                                                 
         MVC   NPGKEND,=XL2'FF9F'  DEC31/27                                     
*                                                                               
         MVC   NPGRLEN,=XL2'0021'                                               
         MVC   NPGCNTL+5(2),QAGY                                                
*                                                                               
         MVC   NPGMAINL(2),=XL2'0108'                                           
         GOTO1 VDATCON,DMCB,(5,0),(3,NPGACTD)                                   
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORK(70),WORK                                                    
         LA    RE,WORK                                                          
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKFMS(3),=CL3'EVN'                                             
         MVC   NUBKBOOK(2),PGRBOOK                                              
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIO2,WORK,0                      
*                                                                               
*--BUILD 92 ELEMENT                                                             
         XC    WORK(100),WORK                                                   
         LA    RE,WORK                                                          
         USING NPGEL92,RE                                                       
*                                                                               
         MVC   NPGELEM(2),=XL2'9250'                                            
         MVI   NPGDAY,X'7F'                                                     
         MVC   NPGTIME,PGRSTIME                                                 
         MVC   NPGNAME,PGRNAME                                                  
         MVC   NPGROT,PGRROT                                                    
         MVC   NPGROTNO,PGRROTN                                                 
         MVI   NPGDAYNO,X'17'                                                   
         MVI   NPGUPLD,C'Y'                                                     
         OI    NPGSTAT,NPGSAEST                                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIO2,WORK,0                      
*                                                                               
*--BUILD 03 ELEMENT                                                             
         XC    WORK(100),WORK                                                   
         LA    RE,WORK                                                          
         USING NPGEL03,RE                                                       
*                                                                               
         MVC   NPGSPEL(2),=XL2'0328'                                            
         MVC   NPGMIRCD,PGRMIROR                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFILE '),AIO2,WORK,0                      
*--WRITE THE RECORD OUT                                                         
         GOTO1 AIOCALL,DMCB,SPT+FIL+ADDREC,AIO2                                 
         B     ADDPRX                                                           
*--EXIT                                                                         
ADDPRX   B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* READ STATION RECORD                                                           
*                                                                               
GETSTA   NTR1                                                                   
         CLC   CPRPNET,PGNETWK      SEE IF INFO ALREADY RETRIEVED               
         BE    GETSTX                                                           
*                                                                               
         L     R5,ANETBLK                                                       
         USING CPRGRECD,R5                                                      
         MVC   PGNETWK,CPRPNET                                                  
*                                                                               
         LA    R6,KEY                                                           
         USING STAREC,R6                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),CPRPNET                                              
         OC    STAKCALL(4),SPACES                                               
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,=XL3'F0F0F0'                                             
         PRINT GEN                                                              
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO1                                   
         PRINT NOGEN                                                            
         CLC   KEY(12),KEYSAVE                                                  
         BE    GETST100                                                         
         LA    R3,WORK2                                                         
         USING ERRDATA,R3                                                       
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXTRA(7),=CL7'STATION'                                         
         B     GETSTER                                                          
*                                                                               
GETST100 L     R6,AIO1                                                          
         PACK  DUB,SMKT(4)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,PGMARKET                                                    
*                                                                               
GETSTX   MVI   ERROR,0                                                          
GETSTER  B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDATA  NTR1                                                                   
*                                                                               
         L     R3,ANETBLK                                                       
         USING CPRGRECD,R3                                                      
*                                                                               
         LHI   R1,X'49'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   ERROR,0                                                          
         BE    SND040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,CPRSEQN                                                       
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERNUMBR                                                       
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERNUMBR,WORK),(X'FF',DMCB),(7,0)                
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXTRA                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         B     SNDEX                                                            
*                                                                               
SND040   LA    R4,CPRSEQN           SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,PGRCODE           PROGRAM CODE                                
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDEX    B     EXIT                                                             
         DROP  R6,R3                                                            
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
         SPACE 1                                                                
CALLTSR2 LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VTSAR,TSARBLK                                                    
         PRINT NOGEN                                                            
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
PGRCODE  DS    CL6                  PROGRAM CODE                                
PGRROTE  DS    CL7                                                              
PGRROT   DS    CL1                                                              
PGRROTN  DS    CL1                                                              
PGRNAME  DS    CL16                                                             
PGRSTIME DS    XL2                                                              
PGRETIME DS    XL2                                                              
PGRMIROR DS    CL1                                                              
PGDAYNO  DS    CL1                                                              
PGMARKET DS    XL2                                                              
PGNETWK  DS    CL4                                                              
PGDAYHEX DS    CL1                                                              
PGLSTLIN DS    CL1                                                              
ADDQ     EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
PGRBOOK  DS    CL2                                                              
*                                                                               
SECAGY   DS    CL2                                                              
SVPASSWD DS    CL2                                                              
*                                                                               
         DS    0D                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095NENAV05   11/18/20'                                      
         END                                                                    
