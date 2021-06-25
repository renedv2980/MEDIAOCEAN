*          DATA SET ACGEN6F    AT LEVEL 078 AS OF 01/07/15                      
*PHASE T00A6FC                                                                  
BAT6F    TITLE 'BATCH FACILITIES - OVERLAY 1'                                   
BAT6F    CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
ROUT     NMOD1 300,**ROU1**,R8,R7,R6,CLEAR=YES                                  
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         LH    R5,=Y(BSVALS-TWAD)                                               
         LA    R5,TWAD(R5)                                                      
         USING BSVALS,R5           R5=A(TWA SAVE AREA)                          
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     AMTVAL              VALIDATE AMOUNT (OLD BATCH OVERLAYS)         
         B     BLDDET              BUILD BATCH DETAILS LINES                    
         B     BLDPFK              BUILD PFKEY LINE                             
         B     TSTACS              TEST AUTHORISATION ACCESS                    
         B     TSTSEC              TEST ACCOUNT SECURITY                        
         B     TSTREC              LOOKUP RECORD & TEST AUTHORISATION           
         B     TSTACT              LOOKUP ACTION & TEST AUTHORISATION           
         B     TSTMIX              LOOKUP RECACT & TEST AUTHORISATION           
         B     TSTBMO              TEST BATCH MONTH(S) ARE OPEN                 
         B     VALOPT              VALIDATE OPTIONS                             
         B     VALAMT              VALIDATE CASH AMOUNT                         
         B     VALBTY              VALIDATE BATCH TYPE                          
         B     VALBMO              VALIDATE BATCH MONTH                         
         B     VALBNA              VALIDATE BATCH NAME                          
         B     VALBRF              VALIDATE BATCH REFERENCE                     
         B     VALCPJ              VALIDATE PRODUCTION ACCOUNT                  
         B     VALEFD              VALIDATE EFFECTIVE DATE                      
         B     VALITE              VALIDATE ITEM COUNT CONTROL                  
         B     VALCSH              VALIDATE CASH CONTROL                        
         B     VALIUP              VALIDATE INSTANT UPDATE                      
         B     VALOFF              VALIDATE AN OFFICE CODE                      
         B     VALULA              VALIDATE (UNIT/LEDGER)/ACCOUNT               
         B     VALNAR              VALIDATE & EXTRACT NARRATIVE                 
         B     VALGOB              CHECK GOBATCH & CALL JOBBER IF REQD          
         B     VALWRK              VALIDATE WORK CODE ESTIMATE AMOUNT           
         B     VALDAT              VALIDATE A SINGLE DATE                       
         B     VALPER              VALIDATE A DATE PERIOD                       
         B     VALMOS              VALIDATE MONTH OF SERVICE RANGE              
         B     GETBTY              GET BATCH TYPE VALUES                        
         B     GETLDG              GET LEDGER VALUES                            
         B     GETACC              GET ACCOUNT FOR OLD OVERLAYS                 
         B     GETACT              GET ACCOUNT FOR NEW OVERLAYS                 
         B     GETELS              GET ELEMENTS & EXTRACT DATA                  
         B     GETGEN              GET GENERAL MESSAGE                          
         B     GETWRK              GET WORK CODE DESCRIPTION                    
         B     GETEST              GET ESTIMATE VALUE(S)                        
         B     GETJOB              READ JOB & SAVE FORMAT TYPE                  
         B     MRGPRF              MERGE PROFILES FOR OLD OVERLAY               
         B     SETSEL              SET A(SELECT TABLE) FOR REC/ACT              
         B     GETPID              GET BATCHER NAME                             
         B     TSTINP              TEST FOR INPUT                               
         B     VALPID              VALIDATE PERSON-ID                           
         B     GETUID              GET USER-ID CODE                             
         B     TSTBTY              TEST BATCH TYPE/ACTION COMBO VALID           
         B     VALDST              VALIDATE DESTINATION ID                      
         B     FLDSEC              TEST FIELD SECURITY                          
         B     VALWEX              VALIDATE WRITE-OFF EXPENSE ACCOUNT           
         B     VALWAN              VALIDATE WRITE-OFF ANALYSIS ACCOUNT          
         B     MCHORD              MATCH ORDER TO INVOICE FOR PTAELS            
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AN AMOUNT FOR OLD BATCH OVERLAYS                           *         
***********************************************************************         
         SPACE 1                                                                
AMTVAL   LA    R0,=PL8'9999999999'                                              
         ST    R0,8(R1)                                                         
         MVC   8(1,R1),CULANG                                                   
         TMATB A2#SWAP             TEST SWAP COMMAS AND DECIMAL POINTS          
         BZ    *+8                                                              
         MVI   4(R1),C'S'                                                       
         GOTO1 VAMTVAL                                                          
         B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN OPTIONS FIELD                                *         
*                                                                     *         
* NTRY - FVAL MUST HAVE BEEN CALLED TO EXTRACT TWA FIELD INTO FVIHDR  *         
*        AND FVIFLD.                                                  *         
*        R1=A(OPTIONS VALIDATION TABLE)                               *         
*                                                                     *         
* EXIT - CC=EQUAL IF OPTIONS FIELD IS OK                              *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                       *         
***********************************************************************         
         SPACE 1                                                                
         USING VOWORKD,RC          RC=A(LOCAL W/S)                              
VALOPT   ST    R1,VOATAB                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALOPT34                                                         
         MVC   BCPARM+08(2),=C',='                                              
         MVC   BCPARM+10(2),BCCHARS                                             
         GOTO1 VSCANNER,BCPARM,('VORHSL',FVIHDR),VOAREA                         
         MVC   VOAREAN(1),4(R1)    SAVE NUMBER OF LINES INPUT                   
         CLI   VOAREAN,0           TEST FOR INVALID INPUT                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTX                                                          
         MVI   FVINDX,1            SET FIRST FIELD INDEX NUMBER                 
         LA    R3,VOAREA           R3=A(SCANNER TABLE)                          
         B     VALOPT04                                                         
*                                  BUMP TO NEXT BLOCK ENTRY                     
VALOPT02 SR    R1,R1                                                            
         IC    R1,FVINDX           BUMP MULTIPLE FIELD INDEX                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,VOAREAN        TEST ALL OPTIONS PROCESSED                   
         BH    VALOPT34                                                         
         STC   R1,FVINDX                                                        
         LA    R3,VOWDTH(R3)                                                    
*                                                                               
VALOPT04 CLI   0(R3),0             TEST VALID KEYWORD LENGTH                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALOPTX                                                          
         CLI   0(R3),OPTNAMLQ                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTX                                                          
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=KEYWORD LENGTH-1                          
         L     R2,VOATAB           SAVE A(OPTIONS VALIDATION TABLE)             
         USING OPTTABD,R2          R2=A(OPTIONS VALIDATION TABLE)               
*                                  SEARCH TABLE FOR KEYWORD                     
VALOPT06 CLI   OPTTABD,EOT         TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKINV) YES - INVALID KEYWORD                      
         B     VALOPTX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(OPTNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,OPTSHRT                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK+OPTNAMLQ(OPTSHTLQ),0(RE)                                  
VALOPT08 LA    RE,1(R1)                                                         
         CLM   RE,1,OPTMINKL       TEST L'INPUT LESS THAN MIN ALLOWED           
         BL    VALOPT14                                                         
         EX    R1,*+8                                                           
         BE    VALOPT10                                                         
         CLC   BCWORK(0),VOHDRL(R3) MATCH ON FULL OPTION NAME                   
         CLI   0(R3),OPTSHTLQ      TEST > SHORT KEYWORD                         
         BH    VALOPT14                                                         
         EX    R1,*+8                                                           
         BNE   VALOPT14                                                         
         CLC   BCWORK+OPTNAMLQ(0),VOHDRL(R3)                                    
VALOPT10 TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST THIS IS A DDS TERMINAL            
         BZ    VALOPT14                                                         
         MVC   BCWORK(2),OPTAUTH                                                
         NC    BCWORK(2),CUAUTH                                                 
         CLC   BCWORK(2),OPTAUTH                                                
         BNE   VALOPT14                                                         
*                                                                               
VALOPT12 CLI   OPTRECB,0           TEST VALID FOR ALL RECORDS                   
         BE    *+14                                                             
         CLC   CSREC,OPTRECB       NO - MATCH ON RECORD NUMBER                  
         BNE   VALOPT14                                                         
         CLI   OPTACTB,0           TEST VALID FOR ALL ACTIONS                   
         BE    *+14                                                             
         CLC   CSACT,OPTACTB       NO - MATCH ON ACTION NUMBER                  
         BNE   VALOPT14                                                         
         CLI   OPTSECN,0           TEST SECURITY NUMBER GIVEN                   
         BE    VALOPT16                                                         
         GOTO1 VSECRET,BCPARM,('SECPOPTP',ASECBLK),OPTSECN                      
         BE    VALOPT16                                                         
*                                                                               
VALOPT14 LA    R2,OPTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT06                                                         
*                                                                               
VALOPT16 SR    R0,R0               CHECK FOR DUPLICATED OPTION KEYWORD          
         LA    R1,1                SET LOW ORDER BIT ON                         
         SR    RE,RE                                                            
         IC    RE,OPTOPTN                                                       
         SLDL  R0,0(RE)            SHIFT BY UNIQUE OPTION NUMBER 1-63           
         STM   R0,R1,VODUB1                                                     
         MVC   VODUB2,VOMASK                                                    
         NC    VODUB2,VODUB1       AND NEW BIT POSITION WITH SAVED MASK         
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     VALOPTX                                                          
         OC    VOMASK,VODUB1       OR NEW BIT POSITION INTO SAVED MASK          
         MVI   4(R3),0             CLEAR SPECIAL CHARACTERS INDICATOR           
         CLI   VOLHSL(R3),C'<'     TEST LESS THAN CHARACTER                     
         BNE   *+8                                                              
         OI    4(R3),OPTLEQ                                                     
         CLI   VOLHSL(R3),C'>'     TEST GREATER THAN CHARACTER                  
         BNE   *+8                                                              
         OI    4(R3),OPTGEQ                                                     
         CLI   VOLHSL(R3),C'*'     TEST NOT EQUALS CHARACTER                    
         BNE   *+8                                                              
         OI    4(R3),OPTNEQ                                                     
         TM    OPTINDS2,OPTPLQ     TEST PLUS CHARACTER IS SPECIAL               
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'+'     TEST PLUS CHARACTER                          
         BNE   *+8                                                              
         OI    4(R3),OPTPLQ                                                     
         TM    OPTINDS2,OPTMIQ     TEST MINUS CHARACTER IS SPECIAL              
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'-'     TEST MINUS CHARACTER                         
         BNE   *+8                                                              
         OI    4(R3),OPTMIQ                                                     
         ICM   RE,1,4(R3)          TEST SPECIAL CHARACTER INPUT                 
         BZ    VALOPT18                                                         
         CLI   1(R3),1             TEST DATA INPUT AFTER SPECIAL                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTX                                                          
         EX    RE,*+8                                                           
         BNZ   VALOPT18                                                         
         TM    OPTINDS2,0          TEST SPECIAL CHARACTER ALLOWED               
         MVC   FVMSGNO,=AL2(FVFIGLE) NO - INVALID                               
         B     VALOPTX                                                          
VALOPT18 SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST DATA INPUT AFTER EQUALS SIGN            
         BNZ   VALOPT20                                                         
         CLI   OPTMINDL,0          NO - TEST IF THAT'S OK                       
         BE    VALOPT20                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         MVI   FVSUBX,1                                                         
         B     VALOPTX                                                          
*                                                                               
VALOPT20 MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         LTR   RF,RF               TEST ZERO LENGTH FIELD                       
         BZ    VALOPT22                                                         
         BCTR  RF,0                                                             
         LA    R1,VOLHSL(R3)                                                    
         TM    4(R3),FF-(OPTMIQ)                                                
         BZ    *+10                                                             
         BCTR  RF,0                DROP FIRST CHARACTER (UNLESS MINUS)          
         LA    R1,1(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),0(R1)                                                  
VALOPT22 XC    FVIHDR,FVIHDR                                                    
         LA    RE,L'FVIHDR+1(RF)                                                
         STC   RE,FVTLEN                                                        
         LR    R0,RF               R0=LENGTH OF FIELD-1                         
         GOTO1 AFVAL,0                                                          
*                                                                               
         CLC   FVILEN,OPTMINDL     TEST L'DATA IS VALID                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALOPTX                                                          
         CLC   FVILEN,OPTMAXDL                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTX                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,3,OPTIADDR                                                    
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION RTN                
         BZ    *+16                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         ICM   RF,7,AOVERVAL+1                                                  
         B     VALOPT26                                                         
*                                                                               
         TM    OPTINDS,OPTATAB                                                  
         BZ    VALOPT24                                                         
         A     RF,AOVERVAL                                                      
         B     VALOPT28                                                         
*                                                                               
VALOPT24 TM    OPTINDS,OPTARTN     TEST A(VALIDATION ROUTINE)                   
         BNZ   *+6                                                              
         DC    H'0'                NO - TABLE IS SCREWY                         
         A     RF,AOVERVAL                                                      
*                                                                               
VALOPT26 XC    BCWORK,BCWORK                                                    
         BASR  RE,RF               GO TO ROUTINE                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALOPT32                                                         
         MVI   FVFLAG,X'01'        INDICATE A USER ERROR MESSAGE                
         BNE   VALOPTX                                                          
*                                                                               
VALOPT28 SR    R0,R0                                                            
         IC    R0,0(RF)            R0=L'LHS OF TABLE                            
         SR    R1,R1                                                            
         IC    R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'TABLE                                   
         SR    RE,RE                                                            
         IC    RE,FVXLEN           RE=L'DATA-1                                  
VALOPT30 CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDINV) YES - INVALID DATA VALUE                   
         B     VALOPTX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT30                                                         
         AR    RF,R0               EXTRACT RHS OF TABLE INTO WORK               
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),0(RF)                                                  
*                                                                               
VALOPT32 BAS   RE,SETOPT           MOVE DATA TO OPTION AREA                     
         B     VALOPT02                                                         
*                                                                               
VALOPT34 MVI   FVINDX,0            RESET INDEX/SUB-INDEX                        
         MVI   FVSUBX,0                                                         
         L     R2,VOATAB           APPLY ANY DEFAULT OPTION VALUES              
         LA    R3,VOAREA                                                        
         XC    0(12,R3),0(R3)      TEST ALL REQUIRED OPTIONS WERE INPUT         
*                                  APPLY ANY DEFAULT OPTION VALUES              
VALOPT36 CLI   OPTTABD,EOT         TEST E-O-T                                   
         BE    VALOPTX                                                          
         TM    OPTINDS,OPTDFLTO+OPTDFLTI                                        
         BZ    VALOPT42                                                         
         CLI   OPTRECB,0           TEST RECORD/ACTION FILTERS                   
         BE    *+14                                                             
         CLC   CSREC,OPTRECB                                                    
         BNE   VALOPT42                                                         
         CLI   OPTACTB,0           AND ACTION IS VALID                          
         BE    *+14                                                             
         CLC   CSACT,OPTACTB                                                    
         BNE   VALOPT42                                                         
         SR    R0,R0               CHECK FOR OPTION KEYWORD USED                
         LA    R1,1                SET LOW ORDER BIT ON                         
         SR    RE,RE                                                            
         IC    RE,OPTOPTN                                                       
         SLDL  R0,0(RE)            SHIFT BY UNIQUE OPTION NUMBER 1-63           
         STM   R0,R1,VODUB1                                                     
         MVC   VODUB2,VOMASK                                                    
         NC    VODUB2,VODUB1       AND NEW BIT POSITION WITH SAVED MASK         
         BNZ   VALOPT42                                                         
         TM    OPTINDS,OPTDFLTO    AND THERE IS A DEFAULT VALUE (WHEW)          
         BZ    VALOPT38                                                         
         MVC   BCWORK(L'OPTDVAL),OPTDVAL                                        
         B     VALOPT40                                                         
*                                                                               
VALOPT38 MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    FVIHDR,FVIHDR                                                    
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'OPTDVAL),OPTDVAL                                        
         LA    RE,FVIFLD+L'OPTDVAL-1                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,FVIFLD                                                        
         SR    RE,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'FVIHDR+1(RE)                                                
         STC   RF,FVTLEN                                                        
         LR    R0,RE               R0=LENGTH OF FIELD-1                         
         GOTO1 AFVAL,0                                                          
         XC    BCWORK,BCWORK                                                    
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,1,OPTIADDR+1                                                  
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION ROUTINE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         ICM   RF,7,AOVERVAL+1                                                  
         BASR  RE,RF               CALL APPLICATION VALIDATION ROUTINE          
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    *+6                                                              
         DC    H'0'                DIE IF DEFAULT VALUE IS INVALID              
*                                                                               
VALOPT40 BAS   RE,SETOPT           SET DEFAULT OPTION VALUE                     
*                                                                               
VALOPT42 LA    R2,OPTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT36                                                         
*                                                                               
VALOPTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     ROUTX                                                            
         DROP  RC                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MOVE OPTION VALUE FROM WORK INTO OPTION/FORMAT AREA      *         
*                                                                     *         
* NTRY - R2=A(OPTION TABLE ENTRY)                                     *         
*        R3=A(SCANNER BLOCK ENTRY)                                    *         
*        SCWORK=OPTION VALUE TO BE SET                                *         
***********************************************************************         
         SPACE 1                                                                
SETOPT   SR    RF,RF                                                            
         ICM   RF,3,OPTOADDR                                                    
         A     RF,AOVEROUT                                                      
         SR    R1,R1                                                            
         IC    R1,OPTOUTDL         R1=L'DATA (EXCLUDING QUALIFIER)              
         BCTR  R1,0                                                             
         TM    OPTINDS2,OPTGEQ+OPTLEQ+OPTNEQ                                    
         BZ    *+14                                                             
         MVC   0(1,RF),4(R3)       MOVE DATA QUALIFIER                          
         LA    RF,1(RF)                                                         
         TM    OPTINDS,OPTBOOL     TEST IF A BIT VALUE                          
         BZ    *+12                                                             
         EX    R1,SETOPTOR         OR VALUE INTO OUTPUT AREA                    
         B     *+8                                                              
         EX    R1,SETOPTMV         MOVE VALUE TO OUTPUT AREA                    
         BR    RE                                                               
         SPACE 1                                                                
SETOPTOR OC    0(0,RF),BCWORK                                                   
SETOPTMV MVC   0(0,RF),BCWORK                                                   
         SPACE 1                                                                
VOWORKD  DSECT                     ** VALOPT S/R LOCAL W/S **                   
VODUB1   DS    D                                                                
VODUB2   DS    D                                                                
VOATAB   DS    A                   A(OPTIONS VALIDATION TABLE)                  
VOMASK   DS    XL8                 OPTION BIT MASK                              
VOMAXN   EQU   20                  MAXIMUM NUMBER OF SCANNER ENTRIES            
VOHDRL   EQU   12                  LENGTH OF HEADER                             
VOLHSL   EQU   VOHDRL+10           LENGTH OF LHS OF ENTRY                       
VORHSL   EQU   30                  LENGTH OF RHS OF ENTRY                       
VOWDTH   EQU   VOLHSL+VORHSL       WIDTH OF SCANNER ENTRY                       
VOAREAN  DS    XL1                 NUMBER OF SCANNER TABLE ENTRIES              
VOAREA   DS    (VOMAXN)XL(VOWDTH)  SCANNER TABLE                                
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD BATCH DETAILS SCREEN                                          *         
* NTRY - R1=NON-ZERO IF ITEM NUMBER REQUIRED                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BDWORKD,RC                                                       
BLDDET   STC   R1,BDFLAG                                                        
*                                                                               
         TM    BDFLAG,X'80'        TEST REPORT WRITER FORMAT                    
         BZ    BLDDET01                                                         
         MVC   BDTEXT1,=AL2(AS$BATD1)                                           
         MVC   BDTEXT2,=AL2(AS$BATD2)                                           
         L     RE,AREP                                                          
         LA    RF,REPH4-REPD(RE)                                                
         ST    RF,BDADET1H                                                      
         LA    RF,REPH5-REPD(RE)                                                
         ST    RF,BDADET2H                                                      
         B     BLDDET07                                                         
*                                                                               
BLDDET01 L     RF,AMIXNTRY                                                      
         OC    BDTEXT1,MIXDETL1-MIXTABD(RF)                                     
         BZ    BLDDETX                                                          
         OC    BDTEXT2,MIXDETL2-MIXTABD(RF)                                     
         BZ    BLDDETX                                                          
*                                                                               
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LSTTAB ENTRY)                           
         LA    R3,BASOLY1H         LOCATE PFKEY DISPLAY LINE IN TWA             
         LA    RF,OSVALS-1         RF=A(LAST POSSIBLE BYTE IN TWA)              
         SR    RE,RE                                                            
BLDDET02 TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    BLDDET04                                                         
         IC    RE,FVTLEN-FVIHDR(R3)                                             
         LA    R1,0(R3,RE)                                                      
         SH    R1,=Y(L'FVIHDR)     R1=A(EXTENDED FIELD HEADER)                  
         CLI   0(R1),DETFLDNO                                                   
         BE    BLDDET06                                                         
BLDDET04 ICM   RE,1,FVTLEN-FVIHDR(R3)                                           
         BZ    BLDDETX                                                          
         BXLE  R3,RE,BLDDET02                                                   
         B     BLDDETX                                                          
BLDDET06 ST    R3,BDADET1H         SAVE A(FIRST DETAIL LINE)                    
         IC    RE,FVTLEN-FVIHDR(R3)                                             
         AR    R3,RE                                                            
         ST    R3,BDADET2H         SAVE A(SECOND DETAIL LINE)                   
*                                                                               
BLDDET07 LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LSTTAB ENTRY)                           
         LA    R1,LSTBBTYP         SET BATCH TYPE VALUES                        
         TM    LSTBHDS2,BHDSDETL   TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         ICM   R1,8,=AL1(TYPIDETL) SET DETAIL FLAG                              
         GOTO1 AGETBTY                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,BDIN                                                          
*                                                                               
         MVI   0(R3),L'LSTBBREF+1  &1=BATCH REFERENCE                           
         MVC   1(L'LSTBBREF,R3),LSTBBREF                                        
         LA    R3,L'LSTBBREF+1(R3)                                              
*                                                                               
         MVI   0(R3),L'LSTBNAME+1  &2=BATCH NAME                                
         MVC   1(L'LSTBNAME,R3),LSTBNAME                                        
         LA    R3,L'LSTBNAME+1(R3)                                              
*                                                                               
         MVI   0(R3),10            &3=BATCH MONTH (MONTH/YEAR)                  
         MVC   1(9,R3),BCSPACES                                                 
         MVC   BDWORK(2),LSTBMOSP                                               
         MVI   BDWORK+2,1                                                       
         GOTO1 VDATCON,BDPARM,(1,BDWORK),(9,1(R3))                              
         LA    R3,10(R3)                                                        
*                                                                               
         MVI   0(R3),11            &4=BATCH EFFECTIVE DATE                      
         MVC   1(10,R3),BCSPACES                                                
         GOTO1 VDATCON,BDPARM,(2,LSTBEFDT),(17,1(R3))                           
         LA    R3,11(R3)                                                        
*                                                                               
         MVI   0(R3),4             &5=BATCH TYPE (NUMBER)                       
         CURED (B1,LSTBBTYP),(3,1(R3)),0,ALIGN=LEFT,DMCB=BDPARM                 
         LA    R3,4(R3)                                                         
*                                                                               
         MVI   0(R3),L'CSBTYPN+1   &6=BATCH TYPE (NAME)                         
         MVC   1(L'CSBTYPN,R3),CSBTYPN                                          
         LA    R3,L'CSBTYPN+1(R3)                                               
*                                                                               
         MVI   0(R3),L'LSTBREFN+1  &7=BATCH REFERENCE (YMBBBB)                  
         MVC   1(L'LSTBREFN,R3),LSTBREFN                                        
         LA    R3,L'LSTBREFN+1(R3)                                              
*                                                                               
         MVI   0(R3),0             SET END OF PARAMETERS                        
         LA    R1,BDPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,BDTEXT1                                                  
         MVC   GTMSYS,ASSYSO                                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,BDIN             SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         LA    R0,BDOUT                                                         
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         L     RF,BDADET1H                                                      
         TM    BDFLAG,X'80'                                                     
         BZ    *+14                                                             
         MVC   0(L'BASDET1,RF),BDOUT                                            
         B     *+14                                                             
         MVC   FVIFLD-FVIHDR(L'BASDET1,RF),BDOUT                                
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
*                                                                               
         LA    R3,BDIN                                                          
*                                                                               
         MVI   0(R3),14            &1=CASH CONTROL                              
         CURED LSTBCSHC,(13,1(R3)),CSCURTAM,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R3,14(R3)                                                        
*                                                                               
         OC    LSTBTDRS,LSTBTDRS   TEST TOTAL DEBITS (& CREDITS) SET            
         BZ    BLDDET08            NO - BATCH DID NOT ACCUMULATE                
         TM    CSBIND1,TYPICUMU                                                 
         BO    BLDDET10                                                         
         TM    CSBIND5,TYPIHRTX                                                 
         BO    BLDDET14                                                         
BLDDET08 MVI   0(R3),14            &2=CASH SO FAR                               
         CURED LSTBCSHA,(13,1(R3)),CSCURTAM,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R3,14(R3)                                                        
         B     BLDDET18                                                         
*                                                                               
BLDDET10 LA    R4,1(R3)            &2 FORMAT 'DR=AMOUNT/CR=AMOUNT'              
         MVC   0(L'BC@DR,R4),BC@DR                                              
         LA    R4,L'BC@DR-1(R4)                                                 
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'BCEQUAL,R4),BCEQUAL                                          
         LA    R4,1+L'BCEQUAL(R4)                                               
         CURED LSTBTDRS,(14,0(R4)),CSCURTDR,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R4,13(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'BCCOMMA,R4),BCCOMMA                                          
         MVC   1+L'BCCOMMA(L'BC@CR,R4),BC@CR                                    
         LA    R4,L'BC@CR+1(R4)                                                 
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'BCEQUAL,R4),BCEQUAL                                          
         LA    R4,1+L'BCEQUAL(R4)                                               
         CURED LSTBTCRS,(14,0(R4)),CSCURTCR,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R4,13(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
         SR    R4,R3                                                            
         STC   R4,0(R3)                                                         
         AR    R3,R4                                                            
         B     BLDDET18                                                         
*                                                                               
         USING BDTXTABD,R4                                                      
BLDDET14 LA    R4,BDTXTAB          SUBSTITUTE MESSAGE                           
         LA    R0,BDTXTABN                                                      
         CLC   LSTBBTYP,BDTXBTYP   TEST BATCH TYPE MATCH                        
         BE    *+12                                                             
         LA    R4,BDTXTABL(R4)                                                  
         BCT   R0,*-14                                                          
         MVC   BDTEXT2,BDTXTNOI    R4=A(SPECIFIC/DEFAULT EDIT RULES)            
         TM    BDFLAG,X'01'        TEST ITEM RECORD PASSED                      
         BZ    *+10                                                             
         MVC   BDTEXT2,BDTXTITE                                                 
         MVI   0(R3),15            &2 HOURS/UNITS/CASH                          
         CURED LSTBTCRS,(14,1(R3)),CSCURTCR,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R3,15(R3)                                                        
         MVI   0(R3),15            &3 TAX                                       
         CURED LSTBTDRS,(14,1(R3)),CSCURTDR,DMCB=BDPARM,ALIGN=LEFT              
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R3,15(R3)                                                        
         B     BLDDET20            SKIP CASH BALANCE                            
*                                                                               
BLDDET18 MVI   0(R3),14            &3=CASH BALANCE                              
         ZAP   BDPL6,LSTBCSHC                                                   
         SP    BDPL6,LSTBCSHA                                                   
         CURED BDPL6,(13,1(R3)),CSCURTAM,DMCB=BDPARM,ALIGN=LEFT                 
         ORG   *-2                                                              
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         LA    R3,14(R3)                                                        
*                                                                               
BLDDET20 MVI   0(R3),6             &4=ITEM CONTROL                              
         CURED (B2,LSTBITMC),(5,1(R3)),0,DMCB=BDPARM,ALIGN=LEFT                 
         LA    R3,6(R3)                                                         
*                                                                               
         MVI   0(R3),6             &5=ITEMS SO FAR                              
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         CURED (R0),(5,1(R3)),0,DMCB=BDPARM,ALIGN=LEFT                          
         LA    R3,6(R3)                                                         
*                                                                               
         MVI   0(R3),7             &6=ITEM BALANCE                              
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         MVC   BOHALF1,LSTBITMC                                                 
         SH    R0,BOHALF1                                                       
         CURED (R0),(6,1(R3)),0,DMCB=BDPARM,ALIGN=LEFT,MINUS=YES                
         LA    R3,7(R3)                                                         
*                                                                               
         TM    BDFLAG,X'01'        TEST ITEM RECORD PASSED                      
         BZ    BLDDET22                                                         
         LA    R2,BCITECUR                                                      
         MVI   0(R3),6             &7=ITEM NUMBER                               
         CURED LSTISEQ,(5,1(R3)),0,DMCB=BDPARM,ALIGN=LEFT                       
         LA    R3,6(R3)                                                         
*                                                                               
BLDDET22 MVI   0(R3),0             SET END OF PARAMETERS                        
         LA    R1,BDPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,BDTEXT2                                                  
         MVC   GTMSYS,ASSYSO                                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,BDIN             SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         LA    R0,BDOUT                                                         
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         L     RF,BDADET2H                                                      
         TM    BDFLAG,X'80'                                                     
         BZ    *+14                                                             
         MVC   0(L'BASDET1,RF),BDOUT                                            
         B     *+14                                                             
         MVC   FVIFLD-FVIHDR(L'BASDET1,RF),BDOUT                                
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
*                                                                               
BLDDETX  B     ROUTE                                                            
         DROP  R1,R2,R4,RC                                                      
*                                                                               
BDTXTAB  DS    0X                                                               
         DC    AL1(BT49)           TYPE 49 HOURS/TAX                            
         DC    AL2(AS$BATD4)                                                    
         DC    AL2(AS$BATD6)                                                    
*                                                                               
         DC    AL1(BT62)           TYPE 62 UNITS/TAX                            
         DC    AL2(AS$BATD5)                                                    
         DC    AL2(AS$BATD7)                                                    
*                                                                               
BDTXTABN EQU   (*-BDTXTAB)/BDTXTABL                                             
*                                                                               
         DC    AL1(0)              DEFAULT CASH/TAX                             
         DC    AL2(AS$BATD8)                                                    
         DC    AL2(AS$BATD9)                                                    
         SPACE 1                                                                
BDTXTABD DSECT                     ** DSECT COVERS BDTXTAB **                   
BDTXBTYP DS    XL1                 BATCH INPUT TYPE                             
BDTXTNOI DS    XL2                 TEXT MESSAGE WITH NO ITEM #                  
BDTXTITE DS    XL2                 TEXT MESSAGE WITH ITEM #                     
BDTXTABL EQU   *-BDTXTABD                                                       
         SPACE 1                                                                
BDWORKD  DSECT                     ** BLDDET S/R LOCAL W/S **                   
BDADET1H DS    A                   A(FIRST DETAIL LINE HEADER)                  
BDADET2H DS    A                   A(SECOND DETAIL LINE HEADER)                 
BDTEXT1  DS    XL2                 TEXT NUMBER FOR FIRST DETAIL LINE            
BDTEXT2  DS    XL2                 TEXT NUMBER FOR SECOND DETAIL LINE           
BDPARM   DS    6F                                                               
BDFLAG   DS    XL1                                                              
BDPL6    DS    PL6                                                              
BDWORK   DS    XL64                                                             
BDIN     DS    XL256                                                            
BDOUT    DS    XL256                                                            
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PFKEY LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BPWORKD,RC                                                       
BLDPFK   MVC   BPTWAI3,TWAINDS3    SAVE TWA INDICATOR BYTE 3                    
         TM    CSINDSL2,CSIPFAPP   TEST PFKEYS CONTROLLEDE BY APP               
         BNZ   BLDPFKX                                                          
BLDPFK00 LA    R2,BASOLY1H         LOCATE PFKEY DISPLAY LINE IN TWA             
         LA    RF,OSVALS-1         RF=A(LAST POSSIBLE BYTE IN TWA)              
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,OSSAVE-1                                                      
         SR    RE,RE                                                            
BLDPFK02 TM    FVATRB-FVIHDR(R2),FVAXTND                                        
         BZ    BLDPFK04                                                         
         IC    RE,FVTLEN-FVIHDR(R2)                                             
         LA    R1,0(R2,RE)                                                      
         SH    R1,=Y(L'FVIHDR)     R1=A(EXTENDED FIELD HEADER)                  
         CLI   0(R1),PFKFLDNO                                                   
         BE    BLDPFK06                                                         
BLDPFK04 ICM   RE,1,FVTLEN-FVIHDR(R2)                                           
         BZ    BLDPFKX                                                          
         BXLE  R2,RE,BLDPFK02                                                   
         B     BLDPFKX                                                          
*                                                                               
BLDPFK06 ICM   RE,1,FVTLEN-FVIHDR(R2)                                           
         SH    RE,=Y(L'FVIHDR+L'FVIHDR+1)                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R2),L'FVIHDR(R2)                                      
         OI    FVATRB-FVIHDR(R2),FVAHIGH                                        
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         TM    TWAINDS2,TWAIDPFK   TEST PFKEYS ARE DISABLED                     
         BNZ   BLDPFKX                                                          
         LR    R4,RE               SAVE LENGTH OF PFKEY LINE IN R4              
*                                                                               
         MVC   BPRECACT,CSREC                                                   
         CLI   BPRECACT+1,0                                                     
         BNE   *+12                                                             
         MVI   BPRECACT+0,FF                                                    
         MVI   BPRECACT+1,FF                                                    
*&&DO                                                                           
         CLI   CSBTYP,BT10         BATCH TYPES MATCH?                           
         BNE   BLDPFK07                                                         
         CLI   CSREC,RECITE        TEST ITEM                                    
         BNE   BLDPFK07                                                         
         CLI   CSACT,ACTINP        TEST INPUT                                   
         BNE   BLDPFK07                                                         
         LA    RF,CSLSTCUR         TEST GENERATED BATCH                         
         TM    LSTBHDS1-LSTTABD(RF),BHDSGENE                                    
         BNO   BLDPFK07                                                         
         MVI   BPRECACT+1,ACTCHA     USE 'CHANGE' PF KEYS                       
*&&                                                                             
BLDPFK07 L     R3,APFKTAB          LOCATE PFKEY SUB-TABLE                       
         USING PFKTABD,R3          R3=A(PFKEY TABLE)                            
         SR    R1,R1                                                            
BLDPFK08 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    BLDPFKX                                                          
         CLC   PFKKEY,BPRECACT     TEST CURRENT RECORD MATCHES                  
         BE    *+14                                                             
         ICM   R1,3,PFKLEN         BUMP TO NEXT TABLE HEADER                    
         AR    R3,R1                                                            
         B     BLDPFK08                                                         
*                                                                               
         LA    R3,PFKHEADL(R3)     BUMP TO FIRST DATA ENTRY                     
         LA    R2,L'FVIHDR(R2)     BUMP TO PFKEY FIELD                          
         ST    R2,BCFULL           SAVE A(OUTPUT FIELD)                         
         MVI   0(R2),C' '                                                       
         MVI   BOELEM,C' '         CLEAR WORK AREA TO SPACES                    
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         LA    R2,BOELEM           POINT TO WORK AREA                           
         MVI   BCBYTE1,0           INITIALISE FLAG BYTE                         
         MVI   BCBYTE2,0           INITIALISE LAST RECORD BYTE                  
*                                                                               
BLDPFK10 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    BLDPFK40                                                         
         CLC   PFKKEY,CSNEXTPF     TEST AUTO PF KEY                             
         BE    BLDPFK38                                                         
         TM    PFKINDS1,PFKIALTP   TEST ALTPFS KEY                              
         BNZ   BLDPFK22                                                         
         TM    PFKINDS1,PFKIKAPA   TEST FOR KNOWN APPLICATION ACTION            
         BZ    BLDPFK12                                                         
         CLI   PFKSUBA,0           TEST SUB-ACTION NUMBER                       
         BE    BLDPFK22                                                         
         CLC   PFKSUBA,CSSUBACT    MATCH ON SUB-ACTION                          
         BNE   BLDPFK38                                                         
         B     BLDPFK22                                                         
*                                                                               
*                                                                               
BLDPFK12 TM    PFKINDS1,PFKIAPPL   TEST FOR APPLICATION USE                     
         BZ    BLDPFK14                                                         
         CLC   CSBTYP,PFKBTYP      MATCH ON BATCH INPUT TYPE                    
         BNE   BLDPFK38                                                         
         CLC   PFKSPROG,CSSPROG    MATCH ON OVERLAY SUB-PROGRAM                 
         BNE   BLDPFK38                                                         
         MVC   BCHALF,CSAPFMSK     TEST APPLICATION PFKEY MASK                  
         NC    BCHALF,PFKOMASK                                                  
         CLC   BCHALF,PFKOMASK                                                  
         BNE   BLDPFK38                                                         
*                                                                               
         CLI   PFKBTYP,34          TYPE 34 (O&M ONLY, TEMPORARY)                
         BNE   BLDPF12C                                                         
         CLI   PFKNUMB,9           PF9                                          
         BNE   BLDPF12C                                                         
*                                                                               
         XC    BCPARM(24),BCPARM                                                
         MVI   BPSECBYT,4                                                       
         GOTO1 VSECRET,BCPARM,('SECPOPTP',ASECBLK),BPSECBYT                     
         CLI   BCPARM,SECPYES                                                   
         BNE   BLDPFK38            NOT AVAILABLE FOR THIS ID                    
*                                                                               
BLDPF12C CLI   PFKCTRY,0           ALL COUNTRIES VALID?                         
         BE    BLDPFK22            YES                                          
         TM    PFKCTRY,CTRYNOT     'NOT' COUNTRY?                               
         BNZ   BLDPFK13            YES                                          
         CLC   PFKCTRY,CUCTRY      NO, COUNTRIES MATCH?                         
         BNE   BLDPFK38            NO                                           
         B     BLDPFK22            NO                                           
*                                                                               
BLDPFK13 MVC   BCWORK(L'PFKCTRY),PFKCTRY                                        
         NI    BCWORK,FF-CTRYNOT                                                
         CLC   CUCTRY,BCWORK                                                    
         BE    BLDPFK38                                                         
         B     BLDPFK22                                                         
*                                                                               
BLDPFK14 CLI   PFKRECN,0           TEST RECORD/ACTION SET                       
         BNE   *+12                                                             
         CLI   PFKACTN,0                                                        
         BE    BLDPFK38                                                         
         TM    PFKINDS1,PFKIQUIT   TEST QUIT PFKEY                              
         BZ    BLDPFK15                                                         
         TM    CSINDSL2,CSIDPFQN   TEST QUIT/NEXT DISABLED                      
         BNZ   BLDPFK38                                                         
         CLI   TWASESNL,0          TEST NESTED                                  
         BE    BLDPFK38                                                         
         TM    PFKINDS1,PFKITSEC   TEST BATCH TYPE SECURITY                     
         BZ    BLDPFK22                                                         
         CLI   CSBTYP,0            TEST BATCH TYPE RESOLVED                     
         BE    BLDPFK38                                                         
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,PFKACTN     TEST ACTION VALID                            
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BE    BLDPFK22                                                         
         B     BLDPFK38                                                         
*                                                                               
BLDPFK15 TM    PFKINDS1,PFKINEXT   TEST NEXT PFKEY                              
         BZ    BLDPFK16                                                         
         TM    CSINDSL2,CSIDPFQN   TEST QUIT/NEXT DISABLED                      
         BNZ   BLDPFK38                                                         
         SR    RF,RF                                                            
         ICM   RF,1,TWASESNL                                                    
         BZ    BLDPFK38                                                         
         SLL   RF,1                                                             
         LA    RF,TWASESRA-L'TWASESRA(RF)                                       
         CLI   L'CSREC(RF),ACTLST  TEST PREVIOUS LEVEL IS A LIST                
         BNE   BLDPFK38                                                         
         B     BLDPFK22                                                         
*                                                                               
BLDPFK16 TM    BPTWAI3,TWAIPFKD    TEST ACTION OR SCROLL PFKEYS                 
         BZ    *+16                                                             
         TM    PFKINDS1,PFKISCRL                                                
         BZ    BLDPFK38                                                         
         B     BLDPFK22                                                         
         TM    PFKINDS1,PFKIACTN                                                
         BZ    BLDPFK38                                                         
*                                                                               
         LA    RE,TWASESRA         RE=A(SESSION RECORD/ACTION TABLE)            
         SR    RF,RF                                                            
         ICM   RF,1,TWASESNL       RF=NUMBER OF ENTERED SESSIONS                
         BZ    BLDPFK18                                                         
         CLC   PFKRECA,0(RE)       TEST RECORD/ACTION ALREADY USED              
         BE    BLDPFK38                                                         
         LA    RE,L'TWASESRA(RE)   BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,*-14                                                          
*                                                                               
BLDPFK18 GOTO1 ATSTACS,PFKRECA     TEST RECORD/ACTION AUTHORISATION             
         BNE   BLDPFK38                                                         
         TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BZ    BLDPFK22                                                         
         LA    RF,CSLSTCUR                                                      
         CLC   PFKRTYPE,LSTTRTYP-LSTTABD(RF)                                    
         BE    BLDPFK20                                                         
         SR    RE,RE                                                            
         IC    RE,PFKRTYPE                                                      
         MH    RE,=Y(LSTTABL)                                                   
         LA    RF,BCBATCUR-LSTTABL(RE)                                          
         CLC   PFKRTYPE,LSTTRTYP-LSTTABD(RF)                                    
         BNE   BLDPFK38                                                         
BLDPFK20 MVC   BCHALF,LSTTMASK-LSTTABD(RF)                                      
         NC    BCHALF,PFKRMASK                                                  
         CLC   BCHALF,PFKRMASK                                                  
         BNE   BLDPFK38                                                         
*                                                                               
BLDPFK22 CLI   BCBYTE1,1           TEST PF PREFIX HAS BEEN OUTPUT               
         BNE   *+12                                                             
         LA    R2,1(R2)            LEAVE A SPACE BETWEEN EXPRESSIONS            
         B     BLDPFK24                                                         
         MVI   BCBYTE1,1           OUTPUT PF PREFIX ON FIRST                    
         MVC   0(L'UC@PFKEY,R2),UC@PFKEY                                        
         LA    R2,L'UC@PFKEY(R2)                                                
*                                                                               
BLDPFK24 SR    R1,R1               OUTPUT PFKEY NUMBER                          
         ICM   R1,1,PFKNUMB                                                     
         CVD   R1,BCDUB                                                         
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         UNPK  0(2,R2),BCDUB                                                    
         CLI   0(R2),C'0'          TEST LEADING ZERO                            
         BE    *+12                                                             
         LA    R2,2(R2)                                                         
         B     *+14                                                             
         MVC   0(2,R2),1(R2)       SUPPRESS LEADING ZERO                        
         LA    R2,1(R2)                                                         
         MVC   0(L'BCEQUAL,R2),BCEQUAL                                          
         TM    PFKINDS1,PFKISCRL   TEST SCROLL PFKEY                            
         BNZ   BLDPFK34                                                         
         TM    PFKINDS1,PFKIALTP   TEST ALTERNATE PFKEY                         
         BNZ   BLDPFK30                                                         
         TM    PFKINDS1,PFKIAPPL                                                
         BNZ   BLDPFK34                                                         
         TM    PFKINDS1,PFKIQUIT+PFKINEXT                                       
         BNZ   BLDPFK30                                                         
         TM    PFKINDS1,PFKIKAPA                                                
         BNZ   BLDPFK30                                                         
*                                                                               
         CLC   PFKRECN,BCBYTE2     TEST SAME RECORD AS PREVIOUS                 
         BE    BLDPFK28                                                         
         MVC   BCBYTE2,PFKRECN     SET LAST DISPLAYED RECORD TYPE               
         L     RF,ARECTAB          LOCATE RECORD TABLE ENTRY                    
         USING RECTABD,RF                                                       
BLDPFK26 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RECNUMB,PFKRECN     MATCH ON RECORD NUMBER                       
         BE    *+12                                                             
         LA    RF,RECTABL(RF)      BUMP TO NEXT TABLE ENTRY                     
         B     BLDPFK26                                                         
         SR    RE,RE               OUTPUT RECORD NAME                           
         ICM   RE,3,RECNAMEL                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   1(RECNAMLQ,R2),0(RE)                                             
         LA    R2,RECNAMLQ(R2)                                                  
*                                                                               
BLDPFK28 CLI   0(R2),C' '          OUTPUT SLASH BETWEEN RECORD & ACTION         
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'BCSLASH,R2),BCSLASH                                          
         LA    R2,L'BCSLASH(R2)                                                 
*                                                                               
BLDPFK30 L     RF,AACTTAB          LOCATE ACTION TABLE ENTRY                    
         USING ACTTABD,RF                                                       
BLDPFK32 CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,PFKACTN     MATCH ON ACTION NUMBER                       
         BE    *+12                                                             
         LA    RF,ACTTABL(RF)      BUMP TO NEXT TABLE ENTRY                     
         B     BLDPFK32                                                         
         SR    RE,RE               OUTPUT ACTION NAME                           
         ICM   RE,3,ACTNAMEL                                                    
         LA    RE,TWAD(RE)                                                      
         B     BLDPFK36                                                         
*                                                                               
BLDPFK34 SR    RE,RE               OUTPUT SCROLL EXPRESSION                     
         ICM   RE,3,PFKSCRLL                                                    
         LA    RE,TWAD(RE)                                                      
         TM    PFKINDS1,PFKIAPPL                                                
         BNZ   BLDPFK36                                                         
         TM    PFKINDS2,PFKIMAXN+PFKINPFX                                       
         BNZ   BLDPFK36                                                         
         MVI   1(R2),C'+'          NO - OUTPUT DIRECTION                        
         TM    PFKINDS2,PFKIUPDN                                                
         BZ    *+8                                                              
         MVI   1(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
BLDPFK36 MVC   1(PFKSCRLQ,R2),0(RE)                                             
         LA    R2,PFKSCRLQ(R2)                                                  
         TM    PFKINDS2,PFKILONG   TEST LONG PFKEY DESCRIPTION                  
         BZ    *+14                                                             
         MVC   1+PFKSCRLQ(PFKSCRLQ,R2),PFKSCRLQ(RE)                             
         LA    R2,PFKSCRLQ(R2)                                                  
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
BLDPFK38 LA    R3,PFKDATAL(R3)     BUMP TO NEXT DATA ENTRY                      
         B     BLDPFK10                                                         
*                                                                               
BLDPFK40 BCTR  R2,0                POINT TO LAST NON-BLANK CHARACTER            
         LA    R1,BOELEM                                                        
         SR    R2,R1                                                            
         BM    BLDPFK44                                                         
         CR    R2,R4                                                            
         BNH   BLDPFK42                                                         
         AR    R2,R1                                                            
         BCTR  R2,0                LOCATE LAST BLANK                            
         CLI   0(R2),C' '                                                       
         BNE   *-6                                                              
         B     BLDPFK40                                                         
*                                                                               
BLDPFK42 L     R1,BCFULL           MOVE BUILT LINE TO TWA FIELD                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),BOELEM                                                   
         B     BLDPFKX                                                          
*                                                                               
BLDPFK44 TM    BPTWAI3,TWAIPFKD    TEST ALTERNATE PFKEYS REQUESTED              
         BZ    BLDPFKX                                                          
         XI    BPTWAI3,TWAIPFKD    YES - REVERT TO ACTION PFKEYS                
         B     BLDPFK00            AND TRY AGAIN                                
*                                                                               
BLDPFKX  B     ROUTE                                                            
         DROP  R3,RF                                                            
*                                                                               
         SPACE 1                                                                
BPWORKD  DSECT                     ** BLDPFK S/R LOCAL W/S **                   
BPRECACT DS    XL2                                                              
BPTWAI3  DS    XL1                                                              
BPSECBYT DS    XL1                                                              
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST AUTHORISATION FOR A RECORD OR RECORD/ACTION COMBO   *         
***********************************************************************         
         SPACE 1                                                                
TSTACS   LR    RF,R1                                                            
         OC    TWALEN,TWALEN       TEST NEW SECURITY IN USE                     
         BZ    ROUTE                                                            
         CLI   L'CSREC(RF),0       TEST ACTION SPECIFIED                        
         BE    TSTACS02                                                         
         GOTO1 VSECRET,BCPARM,('SECPRACT',ASECBLK),(0(RF),1(RF))                
         B     TSTACSX                                                          
*                                                                               
TSTACS02 GOTO1 VSECRET,BCPARM,('SECPRCD',ASECBLK),(RF)                          
*                                                                               
TSTACSX  BNE   ROUTH                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A CASH AMOUNT OR NUMBER                         *         
*                                                                     *         
* NTRY - P1  BYTE 0     X'80' ON  OUTPUT IS PACKED DECIMAL            *         
*                             OFF OUTPUT IS BINARY                    *         
*                       X'40' ON  VALUE MAY BE SIGNED                 *         
*                       X'0F' NUMBER OF DECIMAL PLACES TO USE         *         
*            BYTE 1-3   A(INPUT)                                      *         
*        P2  BYTE 0     L'OUTPUT OR ZERO                              *         
*            BYTE 1-3   A(OUTPUT)                                     *         
*                                                                     *         
* EXIT -     CC LOW   - FIELD NOT INPUT                               *         
*            CC EQUAL - FIELD INPUT AND CORRECT                       *         
*            CC HIGH  - FIELD INPUT AND INVALID (FVMSGNO SET)         *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   LR    R2,R1               R2=A(PARAMETER LIST)                         
         MVC   BCBYTE1,0(R2)       SAVE FORMAT/D.P.                             
         NI    BCBYTE1,X'0F'       PRESERVE NUMBER OF DECIMAL PLACES            
         MVC   BCBYTE2,0(R2)       SAVE FORMAT/D.P.                             
         NI    BCBYTE2,X'8F'       REMOVE SPECIAL BITS                          
         CLI   4(R2),0             TEST L'OUTPUT IS SET                         
         BNE   VALAMT02                                                         
         MVI   4(R2),4             BINARY - DEFAULT IS 4 BYTES                  
         TM    0(R2),X'80'         TEST PACKED OUTPUT                           
         BZ    VALAMT02                                                         
         MVI   4(R2),6             PACKED - DEFAULT IS SIX BYTES                
*                                                                               
VALAMT02 L     R1,4(R2)                                                         
         IC    RF,4(R2)                                                         
         BCTR  RF,0                                                             
         TM    0(R2),X'80'         TEST PACKED FORMAT                           
         BNZ   VALAMT04                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR BINARY OUTPUT VALUE                    
         B     VALAMT06                                                         
*                                                                               
VALAMT04 SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     VALAMT06                                                         
         ZAP   0(0,R1),BCPZERO     CLEAR PACKED OUTPUT VALUE                    
*                                                                               
VALAMT06 SR    R1,R1                                                            
         ICM   R1,7,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,FVIHDR                                                        
         CR    R1,R0               IF INPUT PASSED IN FVIHDR                    
         BE    VALAMT07            DON'T CALL FVAL                              
         GOTO1 AFVAL               VALIDATE THE INPUT                           
         BNE   ROUTX                                                            
*                                                                               
VALAMT07 IC    RF,BCBYTE2                                                       
         CLI   BCBYTE2,X'00'       TEST BINARY INTEGER                          
         BNE   *+8                                                              
         LA    RF,C'N'                                                          
         CLI   BCBYTE2,X'80'       TEST PACKED INTEGER                          
         BNE   *+8                                                              
         LA    RF,C'0'                                                          
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         GOTO1 VCASHVAL,BCPARM,((RF),FVIFLD),(R0)                               
         CLI   0(R1),0                                                          
         BNE   VALAMTN                                                          
         TM    0(R2),X'80'         TEST PACKED OUTPUT                           
         BZ    VALAMT10                                                         
*                                                                               
         TM    0(R2),X'40'         TEST OUTPUT MAY BE NEGATIVE                  
         BNZ   *+14                                                             
         CP    4(8,R1),BCPZERO     TEST NOT NEGATIVE                            
         BL    VALAMTN                                                          
         ZAP   BCDUB,4(8,R1)       EXTRACT PACKED RETURN VALUE                  
         SR    R1,R1                                                            
         IC    R1,4(R2)                                                         
         LCR   R1,R1                                                            
         AH    R1,=Y(L'BCDUB-1)                                                 
         BM    VALAMT08                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    BCDUB(0),BCDUB      ENSURE WILL PACK INTO NN BYTES               
         BNZ   VALAMTN                                                          
*                                                                               
VALAMT08 L     R1,4(R2)                                                         
         IC    RF,4(R2)                                                         
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R1),BCDUB       RETURN PACKED NUMBER TO CALLER               
         L     RF,0(R2)                                                         
         BAS   RE,CLRFLD2                                                       
         LA    R0,1(R1)            AFTER CLRFLD R1=FIELD X'LENGTH               
         LA    R3,L'FVIHDR(RF)     R3=A(OUTPUT FIELD)                           
         CURED BCDUB,((R0),(R3)),0,DMCB=BCPARM,ALIGN=LEFT                       
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),BCBYTE1                         
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         B     ROUTE                                                            
*                                                                               
VALAMT10 MVC   BCFULL,4(R1)        EXTRACT BINARY RETURN VALUE                  
         SR    R1,R1                                                            
         IC    R1,4(R2)                                                         
         LCR   R1,R1                                                            
         TM    0(R2),X'40'         TEST VALUE MAY BE SIGNED                     
         BZ    VALAMT12                                                         
         TM    BCFULL,X'80'        TEST NEGATIVE VALUE                          
         BZ    VALAMT12                                                         
         AH    R1,=Y(L'BCFULL-1)                                                
         BM    VALAMT16                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BCFULL(0),BCEFFS                                                 
         BNE   VALAMTN                                                          
         LA    RE,BCFULL+1(R1)     RE=A(FIRST SIGNIFICANT BYTE)                 
         TM    0(RE),X'80'         TEST SIGN BIT ON                             
         BZ    VALAMTN                                                          
         B     VALAMT16                                                         
*                                                                               
VALAMT12 AH    R1,=Y(L'BCFULL-1)                                                
         BM    VALAMT14                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    BCFULL(0),BCFULL    ENSURE WILL FIT INTO N BYTES                 
         BNZ   VALAMTN                                                          
*                                                                               
VALAMT14 LR    RE,R1                                                            
         AH    RE,=H'1'                                                         
         TM    0(R2),X'40'         TEST VALUE MAY BE SIGNED                     
         BZ    VALAMT16                                                         
         LA    RE,BCFULL(RE)       RE=A(FIRST SIGNIFICANT BYTE)                 
         TM    0(RE),X'80'         TEST HIGH ORDER BIT ON                       
         BNZ   VALAMTN                                                          
*                                                                               
VALAMT16 AH    R1,=H'1'                                                         
         L     RE,4(R2)            RE=A(OUTPUT)                                 
         LA    R1,BCFULL(R1)                                                    
         IC    RF,4(R2)            RF=L'OUTPUT                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       RETURN BINARY NUMBER TO CALLER               
         L     RF,0(R2)                                                         
         BAS   RE,CLRFLD2                                                       
         LA    R0,1(R1)            AFTER CLRFLD R1=FIELD X'LENGTH               
         LA    R3,L'FVIHDR(RF)     R3=A(OUTPUT FIELD)                           
         CURED BCFULL,((R0),(R3)),0,DMCB=BCPARM,ALIGN=LEFT                      
         ORG   *-2                                                              
         MVC   CURPCDEC-CURPARMD(L'CURPCDEC,R1),BCBYTE1                         
*&&UK*&& OI    CURPEDT1-CURPARMD(R1),CURPMINY                                   
*&&US*&& OI    CURPEDT2-CURPARMD(R1),CURPFLON                                   
         BASR  RE,RF                                                            
         B     ROUTE                                                            
         SPACE 1                                                                
VALAMTN  MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH TYPE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALBTY   XC    CSBTYP,CSBTYP                                                    
         XC    CSBTYPN,CSBTYPN                                                  
         XC    CSBINDS,CSBINDS                                                  
         XC    CSBINDS2,CSBINDS2                                                
         MVI   CSBITS,0                                                         
         MVI   CSBITO,0                                                         
         XC    CSCURTAM,CSCURTAM                                                
         XC    CSCURTDR,CSCURTDR                                                
         XC    CSCURTCR,CSCURTCR                                                
         MVI   CSCURULE,0                                                       
                                                                                
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(5),8(R1)    SAVE BATCH TYPE INPUT                        
         XR    R3,R3               GET LENGTH OF FIELD                          
         IC    R3,0(R1)            R3=TOTAL LENGTH                              
         SHI   R3,9                LESS HEADER                                  
         TM    1(R1),X'02'         TEST EXTENDED HEADER                         
         BNO   *+8                                                              
         SHI   R3,8                LESS EXTENDED HEADER                         
         EX    R3,*+8              CLEAR FIELD                                  
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
                                                                                
         MVI   BOBYTE1,0                                                        
         LA    R0,5                                                             
         LA    R2,8(R1)                                                         
         LA    R3,BOWORK1                                                       
         XR    RF,RF                                                            
VALBTY01 CLC   0(2,R3),=C',D'      LOOK FOR DETAIL OPTION                       
         BNE   *+12                                                             
         OI    BOBYTE1,TYPIDETL    SET FILTER FOR DETAIL                        
         B     VALBTY02                                                         
         CLI   0(R3),C' '                                                       
         BNH   VALBTY02                                                         
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VALBTY01                                                      
*                                                                               
VALBTY02 STC   RF,5(R1)                                                         
         GOTO1 AFVAL                                                            
         MVC   8(5,R1),BOWORK1     RESTORE ORIGINAL INPUT                       
         BNE   ROUTX                                                            
         TM    FVIIND,FVINUM       TEST INPUT IS NUMERIC                        
         BZ    VALBTYER                                                         
         OC    BCFULL(3),BCFULL    AND ONE BYTE LONG                            
         BNZ   VALBTYER                                                         
                                                                                
VALBTY04 L     R2,ATYPTAB                                                       
         USING TYPTABD,R2          R2=A(BATCH TYPE TABLE)                       
*                                                                               
VALBTY06 CLI   TYPNUM,EOT                                                       
         BE    VALBTYER                                                         
         CLC   TYPNUM,BCFULL+3                                                  
         BNE   VALBTY08                                                         
         MVC   BOBYTE2,TYPIND8                                                  
         NI    BOBYTE2,TYPIDETL                                                 
         CLC   BOBYTE1,BOBYTE2     TEST DETAIL ENTRY                            
         BNE   VALBTY08                                                         
         CLI   TYPCTRY,CTRYALL     TEST ALL COUNTRIES                           
         BE    VALBTY10                                                         
         CLC   TYPCTRY,CUCTRY      TEST FOR SINGLE VALID COUNTRY                
         BE    VALBTY10                                                         
         MVC   BCWORK(L'TYPCTRY),TYPCTRY                                        
         NI    BCWORK,FF-CTRYNOT                                                
         TM    TYPCTRY,CTRYNOT     TEST TYPE EXCLUDED FOR A COUNTRY             
         BNO   VALBTY08                                                         
         CLC   CUCTRY,BCWORK       TEST THIS IS THE EXCLUDED COUNTRY            
         BNE   VALBTY10                                                         
*                                                                               
VALBTY08 LA    R2,TYPTABL(R2)                                                   
         B     VALBTY06                                                         
*                                                                               
VALBTY10 TM    TYPIND2,TYPIDDS     TEST DDS ONLY BATCH TYPE                     
         BNO   VALBTY12                                                         
         TM    CUSTAT,CUSDDS       TEST CONNECTED USER IS DDS                   
         BNO   VALBTYER                                                         
*                                                                               
VALBTY12 B     GETBTY06                                                         
*                                                                               
VALBTYER MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ROUTH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET BATCH TYPE VALUES IN WORKING STORAGE                 *         
*                                                                     *         
* NTRY - R1=A(BATCH TYPE)   HOB=X'10' FOR DETAIL FILTER               *         
***********************************************************************         
         SPACE 1                                                                
GETBTY   L     R2,ATYPTAB                                                       
         USING TYPTABD,R2          R2=A(BATCH TYPE TABLE)                       
         STCM  R1,8,BOBYTE1                                                     
*                                                                               
GETBTY02 CLI   TYPNUM,EOT          TEST END OF TABLE                            
         BE    ROUTH                                                            
         CLC   TYPNUM,0(R1)        MATCH ON BATCH TYPE                          
         BNE   GETBTY04                                                         
         MVC   BOBYTE2,TYPIND8                                                  
         NI    BOBYTE2,TYPIDETL                                                 
         CLC   BOBYTE2,BOBYTE1     TEST SAME  ENTRY                             
         BNE   GETBTY04                                                         
         CLI   TYPCTRY,CTRYALL     TEST ALL COUNTRIES                           
         BE    GETBTY06                                                         
         CLC   TYPCTRY,CUCTRY      TEST FOR SINGLE VALID COUNTRY                
         BE    GETBTY06                                                         
         MVC   BCWORK(L'TYPCTRY),TYPCTRY                                        
         NI    BCWORK,FF-CTRYNOT                                                
         TM    TYPCTRY,CTRYNOT     TEST TYPE EXCLUDED FOR A COUNTRY             
         BNO   GETBTY04                                                         
         CLC   CUCTRY,BCWORK       TEST THIS IS THE EXCLUDED COUNTRY            
         BNE   GETBTY06                                                         
GETBTY04 LA    R2,TYPTABL(R2)                                                   
         B     GETBTY02                                                         
*                                                                               
GETBTY06 MVC   CSBTYP,TYPNUM       SET NUMBER                                   
         MVC   CSBTYPN(L'TYPNAME),TYPNAME                                       
         GOTO1 VDICTAT,BCPARM,C'SL  ',CSBTYPN,0                                 
         MVC   CSBINDS,TYPINDS     INDICATORS                                   
         MVC   CSBINDS2,TYPIND5                                                 
         MVC   CSBTYP2,TYPNUM2     SECOND BATCH TYPE                            
         MVC   CSBGRUP,TYPGRUP     GROUP                                        
         MVC   CSBITS,TYPSCRN      SCREEN                                       
         MVC   CSBITO,TYPOLAY      OVERLAY                                      
         MVC   CSAPFMSK,TYPPFMSK   APPLICATION PFKEY MASK                       
         ZAP   CSBMAXAM,=P'200000000'                                           
         TM    CSBIND1,TYPIBIL                                                  
         BZ    *+10                                                             
         ZAP   CSBMAXAM,=P'9999999999'                                          
         MVC   BOBYTE1,TYPIND3                                                  
         NI    BOBYTE1,TYPIMAXI                                                 
         SR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         MH    R1,=H'50'                                                        
         STCM  R1,3,CSBMAXIT       MAXIMUM NUMBER OF ITEMS ALLOWED              
         TM    TYPIND5,TYPIASIR    IF 'AUTO REVERSE ALL' SET                    
         BZ    *+10                                                             
         MVC   CSBMAXIT,=H'1000'   ALLOW 1000 ITEMS                             
         MVC   BOBYTE1,TYPIND4                                                  
         NI    BOBYTE1,TYPIMAXU                                                 
         SR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         MH    R1,=H'50'                                                        
         STCM  R1,3,CSBMAXIU       MAXIMUM NUMBER OF ITEMS FOR UPDATE           
         MVC   BCWORK(L'BATTYP),BCSPACES                                        
         CURED CSBTYP,(3,BCWORK),0,ALIGN=LEFT,DMCB=BCPARM                       
         LA    RF,BCWORK                                                        
         AR    RF,R0                                                            
         TM    CSBIND8,TYPIDETL                                                 
         BNO   *+10                                                             
         MVC   0(2,RF),=C',D'                                                   
         MVC   BCWORK+4(L'CSBTYPN),CSBTYPN                                      
         XC    CSCURTAM,CSCURTAM                                                
         XC    CSCURTDR,CSCURTDR                                                
         XC    CSCURTCR,CSCURTCR                                                
         MVI   CSCURTAM+(CURTDECP-CURTABD),2                                    
         MVI   CSCURTDR+(CURTDECP-CURTABD),2                                    
         MVI   CSCURTCR+(CURTDECP-CURTABD),2                                    
         CLI   TYPDRUL,0                                                        
         BE    GETBTY08                                                         
         TM    TYPDRUL,TYPIHOUR                                                 
         BZ    *+12                                                             
         MVI   CSCURTDR+(CURTDECP-CURTABD),2                                    
         B     GETBTY08                                                         
         TM    TYPDRUL,TYPIUNIT                                                 
         BZ    *+12                                                             
         MVI   CSCURTDR+(CURTDECP-CURTABD),0                                    
         B     GETBTY08                                                         
         TM    TYPDRUL,TYPIINTG                                                 
         BZ    *+12                                                             
         MVI   CSCURTDR+(CURTDECP-CURTABD),0                                    
         B     GETBTY08                                                         
         MVC   BOBYTE1,TYPDRUL                                                  
         NI    BOBYTE1,FF-(TYPIDP03)                                            
         CLI   BOBYTE1,0                                                        
         BE    GETBTY08                                                         
         MVC   CSCURTDR+(CURTDECP-CURTABD)(L'CURTDECP),BOBYTE1                  
GETBTY08 CLI   TYPCRUL,0                                                        
         BE    GETBTY10                                                         
         TM    TYPCRUL,TYPIHOUR                                                 
         BZ    *+12                                                             
         MVI   CSCURTCR+(CURTDECP-CURTABD),2                                    
         B     GETBTY10                                                         
         TM    TYPCRUL,TYPIUNIT                                                 
         BZ    *+12                                                             
         MVI   CSCURTCR+(CURTDECP-CURTABD),0                                    
         B     GETBTY10                                                         
         TM    TYPCRUL,TYPIINTG                                                 
         BZ    *+12                                                             
         MVI   CSCURTCR+(CURTDECP-CURTABD),0                                    
         B     GETBTY10                                                         
         MVC   BOBYTE1,TYPCRUL                                                  
         NI    BOBYTE1,FF-(TYPIDP03)                                            
         CLI   BOBYTE1,0                                                        
         BE    GETBTY10                                                         
         MVC   CSCURTCR+(CURTDECP-CURTABD)(L'CURTDECP),BOBYTE1                  
GETBTY10 LA    R1,CSCURTDR                                                      
         TM    TYPIND5,TYPIHRTX                                                 
         BZ    *+8                                                              
         LA    R1,CSCURTCR                                                      
         MVC   CSCURTAM,0(R1)                                                   
*&&UK                                                                           
         TM    BCCPYST6,CPYSFTXR   TEST FT RATES PERMITTED                      
         BZ    *+8                                                              
         OI    CSCURULE,CSCUXRFT   SET FT RATES VALID (BUT SEE BELOW)           
         TM    BCCPYST6,CPYSFBIL   TEST USING FC BILLING                        
         BZ    *+8                                                              
         OI    CSCURULE,CSCUFBIL   SET USING FC BILLING                         
         TM    BCCPYST6,CPYSFMCR   TEST USING FC MEDIA SUPPLIERS                
         BZ    *+8                                                              
         OI    CSCURULE,CSCUFMCR   SET USING FC MEDIA SUPPLIERS                 
         TM    BCCPYST6,CPYSFOCR   TEST USING FC ACCOUNTING SUPPLIERS           
         BZ    *+8                                                              
         OI    CSCURULE,CSCUFOCR   SET USING FC ACCOUNTING SUPPLIERS            
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,=AL1(ACTFTR)                                             
         BE    *+8                                                              
         NI    CSCURULE,FF-(CSCUXRFT)                                           
         GOTO1 ATSTBTY,=AL1(ACTDFR)                                             
         BNE   *+8                                                              
         OI    CSCURULE,CSCUXRDF                                                
         GOTO1 ATSTBTY,=AL1(ACTIPR)                                             
         BNE   *+8                                                              
         OI    CSCURULE,CSCUXRIP                                                
         STCM  R0,7,FVOMTYP        RESTORE MESSGAE TYPE/NUMBER                  
*&&                                                                             
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
         SPACE 1                                                                
VALBMO   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
         LA    R2,BCWORK                                                        
         USING PERVALD,R2                                                       
         MVC   BCWORK(1),CULANG    SET LANGUAGE                                 
         GOTO1 VPERVAL,BCPARM,(FVILEN,FVIFLD),(BCWORK,BCWORK)                   
         TM    4(R1),X'01'         TEST DATE INVALID                            
         BNZ   *+14                                                             
         CLC   PVALNMNS,=H'1'      TEST SINGLE MONTH ONLY                       
         BE    ROUTE                                                            
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     ROUTH                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALBNA   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         XC    LSTBNAME,LSTBNAME                                                
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
         MVC   LSTBNAME,FVIFLD                                                  
         B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALBRF   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(BATCH MONTH TABLE ENTRY)                
         XC    LSTBBREF,LSTBBREF                                                
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
         CLI   0(R1),C'A'                                                       
         BL    ROUTH                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         MVC   LSTBBREF,FVIFLD                                                  
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A PRODUCTION ACCOUNT AT ANY LEVEL.              *         
* IF THE PRODUCTION ACCOUNT IS ENTERED IN SEPARATE FIELDS, EACH MUST  *         
* VALIDATED IN HIERARCHICAL SEQUENCE.                                 *         
*                                                                     *         
* NTRY - R1=A(INPUT FIELD) OR ZERO (MEANS USE FVIHDR/FVIFLD)          *         
*        ONE OR MORE OF THE FOLLOWING BITS ON IN BOFLAG1, INDICATE    *         
*        WHICH LEVEL(S) OF ACCOUNT TO VALIDATE:                       *         
*        ACIPRCLI - VALIDATE CLIENT                                   *         
*        ACIPRPRO - VALIDATE PRODUCT                                  *         
*        ACIPRJOB - VALIDATE JOB                                      *         
***********************************************************************         
         SPACE 1                                                                
VALCPJ   L     RF,AFVAL                                                         
         LTR   R1,R1                                                            
         BZ    *+10                                                             
         BASR  RE,RF                                                            
         BNE   ROUTX                                                            
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2          BUILD VIRGIN KEY                             
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         LA    R1,FVIFLD                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVC   BCWORK(L'PSCLICOD+L'PSPROCOD+L'PSJOBCOD),BCSPACES                
*                                                                               
         TM    BOFLAG1,ACIPRCLI    TEST VALIDATE CLIENT                         
         BZ    VALCPJ02                                                         
         IC    RE,BCCLILEN         EXTRACT CLIENT CODE                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),0(R1)                                                  
         LA    R1,1(RE,R1)                                                      
         CLC   BCWORK(L'PSCLICOD),BCSPACES                                      
         BE    VALCPJEC            ERROR - CLIENT NOT GIVEN                     
*                                                                               
VALCPJ02 TM    BOFLAG1,ACIPRPRO    TEST VALIDATE PRODUCT                        
         BZ    VALCPJ04                                                         
         IC    RE,BCPROLEN         EXTRACT PRODUCT CODE                         
         IC    RF,BCCLILEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK+L'PSCLICOD(0),0(R1)                                       
         LA    R1,1(RE,R1)                                                      
         CLC   BCWORK+L'PSCLICOD(L'PSPROCOD),BCSPACES                           
         BE    VALCPJEP            ERROR - PRODUCT NOT GIVEN                    
*                                                                               
VALCPJ04 TM    BOFLAG1,ACIPRJOB    TEST VALIDATE JOB                            
         BZ    VALCPJ06                                                         
         IC    RE,BCJOBLEN         EXTRACT JOB CODE                             
         IC    RF,BCPROLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK+L'PSCLICOD+L'PSPROCOD(0),0(R1)                            
         CLC   BCWORK+L'PSCLICOD+L'PSPROCOD(L'PSJOBCOD),BCSPACES                
         BE    VALCPJEJ            ERROR - JOB NOT GIVEN                        
*                                                                               
VALCPJ06 LA    R3,ACTKACT                                                       
         MVC   0(L'PSCLICOD,R3),PSCLICOD                                        
         TM    BOFLAG1,ACIPRCLI    TEST VALIDATE CLIENT                         
         BZ    VALCPJ10                                                         
         MVC   0(L'PSCLICOD,R3),BCWORK                                          
         XC    PSCLIPPR(L'PSCLIPPR+L'PSPROPPR+L'PSJOBPPR),PSCLIPPR              
         GOTO1 AGETACT,0           READ CLIENT & TEST SECURITY                  
         BNE   ROUTH                                                            
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BCPARM,(RF),PSCLIPPR                                     
         MVC   PSCLICOD,0(R3)                                                   
         MVC   PSCLIDA,IODA                                                     
         MVC   PSCOMPPR,PSCLIPPR                                                
*                                                                               
VALCPJ10 OC    PSCLICOD,PSCLICOD   TEST FOR VALID CLIENT CODE                   
         BZ    VALCPJEC                                                         
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         AR    R3,RE               POINT TO PRODUCT IN KEY                      
         MVC   0(L'PSPROCOD,R3),PSPROCOD                                        
         TM    BOFLAG1,ACIPRPRO    TEST VALIDATE PRODUCT                        
         BZ    VALCPJ14                                                         
         MVC   0(L'PSPROCOD,R3),BCWORK+L'PSCLICOD                               
         XC    PSPROPPR(L'PSPROPPR+L'PSJOBPPR),PSPROPPR                         
         GOTO1 AGETACT,0           READ PRODUCT & TEST SECURITY                 
         BNE   ROUTH                                                            
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BCPARM,(RF),PSPROPPR                                     
         MVC   PSPROCOD,0(R3)                                                   
         MVC   PSPRODA,IODA                                                     
*                                                                               
VALCPJ14 TM    BOFLAG1,ACIPRPRO+ACIPRJOB                                        
         BZ    VALCPJ18                                                         
         OC    PSPROCOD,PSPROCOD   TEST VALID PRODUCT CODE                      
         BZ    VALCPJEP                                                         
         TM    BOFLAG1,ACIPRJOB    TEST VALIDATE JOB                            
         BZ    VALCPJ18                                                         
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         SR    RE,RF                                                            
         AR    R3,RE               POINT TO JOB IN KEY                          
         MVC   0(L'PSJOBCOD,R3),BCWORK+L'PSCLICOD+L'PSPROCOD                    
         XC    PSJOBPPR,PSJOBPPR                                                
         GOTO1 AGETACT,0           READ JOB & TEST SECURITY                     
         BNE   ROUTH                                                            
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BCPARM,(RF),PSJOBPPR                                     
         MVC   PSJOBCOD,0(R3)                                                   
         MVC   PSJOBDA,IODA                                                     
*                                                                               
VALCPJ18 GOTO1 AMRGPRF             ESTABLISH COMPOSITE PROFILE                  
*                                                                               
VALCPJX  B     ROUTE                                                            
         SPACE 1                                                                
VALCPJEC LH    RE,=Y(LC@CLI-TWAD)  MISSING KEY FIELD - CLIENT                   
         B     VALCPJEX                                                         
*                                                                               
VALCPJEP LH    RE,=Y(LC@PRO-TWAD)  MISSING KEY FIELD - PRODUCT                  
         B     VALCPJEX                                                         
*                                                                               
VALCPJEJ LH    RE,=Y(LC@JOB-TWAD)  MISSING KEY FIELD - JOB                      
*                                                                               
VALCPJEX LA    RE,TWAD(RE)         RE=A(KEY COMPONENT NAME)                     
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'LC@CLI),0(RE)                                           
         MVC   FVMSGNO,=AL2(AE$NOKEY)                                           
         B     ROUTH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE EFFECTIVE DATE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALEFD   LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(BATCH MONTH TABLE ENTRY)                
         MVC   LSTBEFDT,BCTODAYC   DEFAULT EFFECTIVE DATE IS TODAY              
         GOTO1 AFVAL                                                            
         BH    ROUTX               HIGH - ERROR OR NO INPUT, FVMINL>0           
         BL    ROUTE               LOW - NOT INPUT, FVMINL=0                    
         MVC   BCWORK(1),CULANG    SET LANGUAGE                                 
         OI    BCWORK,X'60'        SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,BCPARM,(FVILEN,FVIFLD),(BCWORK,BCWORK)                   
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ROUTH                                                            
         BAS   RE,CLRFLD                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT  RF=A(FIELD HEADER)                     
         LA    RF,L'FVIHDR(RF)                                                  
         LA    R2,BCWORK                                                        
         USING PERVALD,R2                                                       
         GOTO1 VDATCON,BCPARM,(1,PVALPSTA),(17,(RF))                            
         CLC   PVALPSTA,BCTODAYP   IF SET, MUST BE GREATER THAN TODAY           
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DNGRT)                                           
         B     ROUTH                                                            
         GOTO1 VDATCON,BCPARM,(3,BCTODAYB),(0,BCWORK)                           
         GOTO1 VADDAY,BCPARM,(C'M',BCWORK),BCWORK+6,12   ADD 12 MONTHS          
         GOTO1 VPERVERT,BCPARM,PVALESTA,BCWORK+6                                
         SR    RE,RE                                                            
         ICM   RE,3,BCPARM+14                                                   
         BCTR  RE,0                SUB 1 FOR PERVERT # MONTH INCLUSIVE          
         CH    RE,=H'12'           > 12 MONTHS?                                 
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DTFIF)                                           
         B     ROUTH                                                            
         MVC   LSTBEFDT,PVALCSTA   EXTRACT COMPRESSED EFFECTIVE DATE            
         B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A SINGLE DATE                                              *         
* NTRY - R1=A(FIELD HEADER OF SINGLE DATE FIELD)                      *         
* EXIT - BCWORK+00(2)=COMPRESSED DATE                                 *         
*        BCWORK+02(3)=PWOS DATE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
         MVC   BCWORK(1),CULANG    SET LANGUAGE                                 
         OI    BCWORK,X'60'        SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,BCPARM,(FVILEN,FVIFLD),(BCWORK,BCWORK)                   
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   VALDATER                                                         
         BAS   RE,CLRFLD                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT  RF=A(FIELD HEADER)                     
         LA    RF,L'FVIHDR(RF)                                                  
         LA    R2,BCWORK                                                        
         USING PERVALD,R2                                                       
         GOTO1 VDATCON,BCPARM,(1,PVALPSTA),(17,(RF))                            
         ICM   R0,7,PVALPSTA                                                    
         MVC   BCWORK(L'PVALCSTA),PVALCSTA  EXTRACT COMPRESSED DATE             
         STCM  R0,7,BCWORK+L'PVALCSTA                                           
         B     ROUTE                                                            
*                                                                               
VALDATER MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     ROUTH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE PERIOD                                              *         
* NTRY - P1=A(PERIOD FIELD HEADER)                                    *         
*        P2=A(OUTPUT PERIOD) OR 0 IF P1=A(OUTPUT FIELD)               *         
* EXIT - BCWORK CONTAINS PERVAL OUTPUT BLOCK                          *         
***********************************************************************         
         SPACE 1                                                                
VALPER   LR    R2,R1               R2=A(CALLING PARAMETER LIST)                 
         L     R1,0(R1)                                                         
         LA    R1,0(R1)                                                         
         LA    R0,FVIHDR                                                        
         CR    R1,R0                                                            
         BE    VALPER01                                                         
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
*                                                                               
VALPER01 GOTO1 VPERVAL,BCPARM,(FVILEN,FVIFLD),(CULANG,BCWORK)                   
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     ROUTH                                                            
         LA    R3,BCWORK           REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BO    VALPER04            YES - JUST TAKE PERVAL END DATE              
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VALPER02            DON'T SET END DATE                           
         LA    RF,PVALCPER                                                      
         LA    R1,L'PVALCPER-1                                                  
         B     VALPERX             SHOW DDMMMYY-DDMMMYY                         
*                                                                               
VALPER02 LA    RF,PVALCPER         TAKE PERVAL START DATE                       
         LA    R1,L'PVALCPER-1(RF)                                              
         CLI   0(R1),C'-'                                                       
         BE    *+10                                                             
         BCT   R1,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF                                                            
         MVC   PVALBEND,BCEFFS                                                  
         MVC   PVALCEND,BCEFFS                                                  
         MVC   PVALEEND,BCEFFS                                                  
         MVC   PVALPEND,BCEFFS                                                  
         B     VALPERX             SHOW DDMMMYY-                                
*                                                                               
VALPER04 LA    RF,PVALCPER+(L'PVALCPER-1)                                       
         LR    R1,RF                                                            
         CLI   0(RF),C'-'                                                       
         BE    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF               SHOW -DDMMMYY                                
         XC    PVALBSTA,PVALBSTA                                                
         XC    PVALCSTA,PVALCSTA                                                
         XC    PVALESTA,PVALESTA                                                
         XC    PVALPSTA,PVALPSTA                                                
*                                                                               
VALPERX  ICM   RE,15,4(R2)         SET R2=A(OUTPUT)                             
         BNZ   *+16                                                             
         L     RE,0(R2)                                                         
         OI    FVOIND-FVIHDR(RE),FVOXMT                                         
         LA    RE,L'FVIHDR(RE)                                                  
         EX    R1,*+8              DISPLAY THE PERIOD INTERPRETED               
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOS RANGE FILTER                                           *         
*                                                                     *         
* NTRY - R1=A(INPUT FIELD HEADER)                                     *         
* EXIT - BCFULL=START & END MONTH OF SERVICE VALUES                   *         
***********************************************************************         
         SPACE 1                                                                
VALMOS   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    BCFULL(2),BCFULL                                                 
         MVC   BCFULL+2(2),BCEFFS                                               
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
         GOTO1 VPERVAL,BCPARM,(FVILEN,FVIFLD),(CULANG,BCWORK)                   
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     ROUTH                                                            
         BAS   RE,CLRFLD                                                        
         LA    R3,L'FVIHDR(RF)                                                  
         LA    R2,BCWORK                                                        
         USING PERVALD,R2                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BO    VALMOS02            YES - JUST TAKE PERVAL END DATE              
         MVC   BCFULL(2),PVALPSTA                                               
         GOTO1 VDATCON,BCPARM,(1,PVALPSTA),(9,(R3))                             
         LA    R3,7(R3)                                                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
VALMOS02 MVI   0(R3),C'-'                                                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    ROUTE               DON'T SET END DATE                           
         GOTO1 VDATCON,BCPARM,(1,PVALPEND),(9,1(R3))                            
         MVC   BCFULL+2(2),PVALPEND                                             
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ITEM COUNT CONTROL TOTAL                                   *         
***********************************************************************         
         SPACE 1                                                                
VALITE   LR    R2,R1                                                            
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(BATCH MONTH TABLE ENTRY)                
         GOTO1 AVALAMT,BCDMCB,(X'00',(R2)),(L'LSTBITMC,LSTBITMC)                
         BNE   ROUTX                                                            
         CLC   LSTBITMC,CSBMAXIT                                                
         BNH   ROUTE                                                            
         MVC   FVMSGNO,=AL2(AE$BATMX)                                           
         B     ROUTH                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CASH CONTROL TOTAL                                         *         
***********************************************************************         
         SPACE 1                                                                
VALCSH   LR    R2,R1                                                            
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(BATCH MONTH TABLE ENTRY)                
         IC    RF,CSCURTAM+(CURTDECP-CURTABD)                                   
         LA    RF,X'C0'(RF)                                                     
         GOTO1 AVALAMT,BCDMCB,((RF),(R2)),(L'LSTBCSHC,LSTBCSHC)                 
         B     ROUTX                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INSTANT UPDATE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALIUP   NI    CSINDSG1,FF-CSINDIUP                                             
         LA    R2,BC@NO                                                         
         GOTO1 AFVAL                                                            
         BNE   VALIUP04                                                         
         IC    R1,FVXLEN                                                        
         LA    R2,BC@NO                                                         
         EX    R1,*+8                                                           
         BE    VALIUP04                                                         
         CLC   FVIFLD(0),0(R2)                                                  
         LA    R2,BC@YES                                                        
         EX    R1,*+8                                                           
         BE    VALIUP02                                                         
         CLC   FVIFLD(0),0(R2)                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ROUTH                                                            
VALIUP02 OI    CSINDSG1,CSINDIUP                                                
VALIUP04 BAS   RE,CLRFLD                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         MVC   L'FVIHDR(L'BC@YES,RF),0(R2)                                      
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE AN ACCOUNT                                                 *         
*                                                                     *         
* NTRY - P1 BYTE 0    X'FF' ALLOW OVERRIDE UNIT/LEDGER                *         
*           BYTE 1-3  A(INPUT FIELD)                                  *         
*        P2 BYTE 0    N/D                                             *         
*           BYTE 1-3  A(DEFAULT LEDGER) OR ZERO                       *         
*        P3 BYTE 0    N/D                                             *         
*           BYTE 1-3  A(LIST OF VALID LEDGERS) OR ZERO                *         
***********************************************************************         
         SPACE 1                                                                
VALULA   LM    R2,R4,0(R1)                                                      
         GOTO1 AFVAL,(R2)                                                       
         BNE   ROUTX                                                            
         LA    R1,IOKEY                                                         
         USING ACTRECD,R1          R1=A(KEY FOR ACCOUNT RECORD)                 
         MVC   ACTKEY,BCSPACES     BUILD KEY FOR ACCOUNT RECORD                 
         MVC   ACTKUNT(ACTKEND-L'ACTKCPY),FVIFLD                                
         LTR   R3,R3               TEST DEFAULT UNIT/LEDGER PASSED              
         BZ    VALULA04                                                         
         MVC   ACTKUNT(L'LDGTUL),0(R3)                                          
         MVC   ACTKACT,FVIFLD      ASSUME USER ONLY GIVES ACCOUNT               
         CLI   FVIFLD,C'*'         TEST OVERRIDE CHARACTER                      
         BNE   VALULA04                                                         
         CLM   R2,8,BCEFFS         TEST ALLOWING OVERRIDE UNIT/LEDGER           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ROUTH                                                            
         MVC   ACTKUNT(ACTKEND-L'ACTKCPY),FVIFLD+1                              
*                                                                               
VALULA04 GOTO1 AGETACT,(R4)        PASS A(LEDGER LIST OR ZERO)                  
         B     ROUTX                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A LEDGER RECORD AND BUILD LDGTAB (SEE LDGTABD)       *         
*                                                                     *         
* NTRY - IOKEY=KEY OF A LEDGER OR LOWER LEVEL RECORD                  *         
*                                                                     *         
* EXIT - ACALDG POINTS TO LEDGER TABLE ENTRY IF VALID LEDGER          *         
***********************************************************************         
         SPACE 1                                                                
         USING GLWORKD,RC                                                       
GETLDG   LA    R0,LDGTMAXN         R0=MAXIMUM N'LEDGER TABLE ENTRIES            
         LA    R2,BCLDGTAB                                                      
         USING LDGTABD,R2          R2=A(LEDGER TABLE)                           
GETLDG1  OC    LDGTUL,LDGTUL       TEST FREE SLOT                               
         BZ    GETLDG2                                                          
         CLC   LDGTUL,IOKEY+(LDGKUNT-LDGKEY)                                    
         BE    GETLDGX                                                          
         LA    R2,LDGTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,GETLDG1          DO FOR NUMBER OF ENTRIES                     
         SH    R2,=Y(LDGTABL)      USE LAST ENTRY IF TABLE FULL                 
*                                                                               
GETLDG2  MVC   GLKEYSAV,IOKEY      SAVE CALLER'S KEY                            
         MVC   IOKEY,BCSPACES      TAKE LEDGER PORTION & READ                   
         MVC   IOKEY(LDGKEND),GLKEYSAV                                          
         LA    R1,GLIOAREA                                                      
         ST    R1,IOADDR                                                        
         GOTO1 AIO,IOREAD+IOACCMST                                              
         MVC   IOKEY,GLKEYSAV      RESTORE CALLER'S KEY                         
         BNE   GETLDGN                                                          
*                                                                               
         LA    R1,GLIOAREA         PROCESS LEDGER RECORD & BUILD ENTRY          
         MVC   LDGTUL,LDGKUNT-LDGKEY(R1)                                        
         AH    R1,=Y(LDGRFST-LDGRECD)                                           
         SR    R0,R0                                                            
GETLDG3  CLI   0(R1),0             TEST EOR                                     
         BE    GETLDGX                                                          
*                                                                               
         USING LDGELD,R1                                                        
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   GETLDG4                                                          
         MVC   LDGTTYPE,LDGTYPE                                                 
         MVC   LDGTLIKE,LDGLIKE                                                 
         MVC   LDGTOFFP,LDGOPOS                                                 
         MVC   LDGTCLOS,LDGCLOS                                                 
         CLI   LDGLN,LDGLNQ                                                     
         BL    GETLDG9                                                          
         PACK  LDGTDDL,LDGDPOS     EXTRACT DEPARTMENT VALUES                    
         NI    LDGDLEN,X'0F'                                                    
         OC    LDGTDDL,LDGDLEN                                                  
         B     GETLDG9                                                          
*                                                                               
         USING ACLELD,R1                                                        
GETLDG4  CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BNE   GETLDG5                                                          
         MVC   LDGTLVA,ACLVALS                                                  
         MVC   LDGTLVB,ACLVALS+(L'ACLVALS*1)                                    
         MVC   LDGTLVC,ACLVALS+(L'ACLVALS*2)                                    
         MVC   LDGTLVD,ACLVALS+(L'ACLVALS*3)                                    
         B     GETLDG9                                                          
*                                                                               
         USING RSTELD,R1                                                        
GETLDG5  CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   GETLDG6                                                          
         MVC   LDGTSEC,RSTSECY+1                                                
         B     GETLDG9                                                          
*                                                                               
         USING FFTELD,R1                                                        
GETLDG6  CLI   FFTEL,FFTELQ        TEST FREE FOR TEXT ELEMENT                   
         BNE   GETLDG7                                                          
         CLI   FFTTYPE,FFTTVATC    TEST VAT CODE                                
         BNE   GETLDG7                                                          
         MVC   LDGTVATC,FFTDATA    SET LEDGER DEFAULT VAT TYPE                  
         B     GETLDG9                                                          
*                                                                               
GETLDG7  DS    0H                                                               
*                                                                               
GETLDG9  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETLDG3                                                          
*                                                                               
GETLDGX  STCM  R2,15,ACALDG        SET A(LEDGER TABLE ENTRY)                    
         MVC   FVXTRA,BCSPACES                                                  
         B     ROUTE                                                            
*                                                                               
GETLDGN  MVC   FVXTRA(L'LDGTUL),IOKEY+(ACTKUNT-ACTKEY)                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     ROUTH                                                            
         DROP  R1,R2,RC                                                         
         SPACE 1                                                                
GLWORKD  DSECT                     ** GETLDG S/R LOCAL W/S **                   
GLKEYSAV DS    XL(L'IOKEY)                                                      
GLIOAREA DS    2000X                                                            
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT TEST LEDGER AND ACCOUNT SECURITY                        *         
* NTRY - P1=A(ACCOUNT KEY)                                            *         
*        P2=A(PRODUCTION PROFILE AREA) OR 0                           *         
* EXIT - ACCTNUM, ACCTNAME, ACCTSTAT, ACCTCOST SET AND SAVED IN       *         
*        TABLE IF OK, OR SET BUT NOT SAVED IN TABLE IF ERROR, IN      *         
*        WHICH CASE FVMSGNO IS SET ACCORDINGLY                        *         
***********************************************************************         
         SPACE 1                                                                
         USING GAWORKD,RC                                                       
GETACC   LM    R2,R3,0(R1)                                                      
         STCM  R3,8,BCFLAG         SAVE CALLING FLAG                            
         MVC   GASAVULA,ACTKULA-ACTRECD(R2)                                     
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 AGETLDG                                                          
         BNE   ROUTX                                                            
         OC    BCCPYGLM,BCCPYGLM   IF USING NEW GL, CHECK MOA IF G              
         BZ    GETACC00                                                         
         CLI   ACTKUNT-ACTRECD(R2),C'G'                                         
         BNE   GETACC00                                                         
         LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4                                                       
         CLC   LSTBMOSP,BCCPYGLM   MOA CAN'T BE = OR > THEN GL                  
         BNL   GETACCIG                                                         
         CLI   LSTBBTYP,55         IF BATCH TYPE 55 OR 53                       
         BE    *+12                SUBTRACT A MONTH                             
         CLI   LSTBBTYP,53                                                      
         BNE   GETACC00                                                         
         MVC   BCWORK(L'BCCPYGLM),BCCPYGLM                                      
         MVI   BCWORK+2,X'01'                                                   
         GOTO1 VDATCON,BOPARM,(X'31',BCWORK),(1,BCWORK),(4,0)                   
         CLC   LSTBMOSP,BCWORK     THEN DO THE COMPARE                          
         BNL   GETACCIG                                                         
         DROP  R4                                                               
*                                                                               
GETACC00 ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4          R4=A(LEDGER TABLE ENTRY)                     
         TM    BCFLAG,X'80'        TEST HIGH LEVEL PROFILES REQUIRED            
         BZ    GETACC02                                                         
*                                                                               
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKACT-ACTRECD),0(R2)                                     
         SR    R1,R1                                                            
         IC    R1,LDGTLVA                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+(ACTKACT-ACTRECD)(0),ACTKACT-ACTRECD(R2)                   
         GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BNE   GETACCIA                                                         
         L     RF,AIO1                                                          
         AH    RF,=Y(ACCORFST)                                                  
         GOTO1 AGETELS,BCPARM,(RF),PSCLIPPR                                     
*                                                                               
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKACT-ACTRECD),0(R2)                                     
         IC    R1,LDGTLVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+(ACTKACT-ACTRECD)(0),ACTKACT-ACTRECD(R2)                   
         GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BNE   GETACCIA                                                         
         L     RF,AIO1                                                          
         AH    RF,=Y(ACCORFST)                                                  
         GOTO1 AGETELS,BCPARM,(RF),PSPROPPR                                     
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
*                                                                               
GETACC02 GOTO1 AIO,IOREAD+IOACCFIL+IO1                                          
         BL    ROUTL                                                            
         BH    GETACCIA                                                         
         L     R2,AIO1                                                          
         USING ACTRECD,R2          R2=A(ACCOUNT RECORD)                         
         GOTO1 AGETELS,BCPARM,ACTRECD+ACCORFST,(R3)                             
         TM    ACBSTAT,ACBSPROV    PROVISIONAL?                                 
         BO    GETACCIP            YES, CAN'T POST                              
*                                                                               
GETACC04 TM    ACTRSTAT,ACTSDRFT                                                
         BNZ   GETACCID                                                         
*                                                                               
         L     RF,AOFFBLK                                                       
         NI    OFFACTRL-OFFALD(RF),FF-(OFFACCNV)                                
         GOTO1 ATSTSEC                                                          
         BNE   ROUTX                                                            
*                                                                               
GETACCX  B     ROUTE                                                            
*                                                                               
GETACCIG MVC   FVMSGNO,=AL2(AE$NOGEN)                                           
         B     GETACCNX                                                         
*                                                                               
GETACCIP MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     GETACCNX                                                         
*                                                                               
GETACCIA MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     GETACCNX                                                         
*                                                                               
GETACCID MVC   FVMSGNO,=AL2(AE$ACCNA)                                           
*                                                                               
GETACCNX MVC   FVXTRA(L'GASAVULA),GASAVULA                                      
         B     ROUTH                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AN ACCOUNT - NEW FILE                                *         
*                                                                     *         
* NTRY - IOKEY=ACCOUNT RECORD KEY                                     *         
*        R1=A(LIST OF VALID LEDGERS OR ZERO)                          *         
***********************************************************************         
         SPACE 1                                                                
GETACT   LA    R2,IOKEY            R2=A(ACCOUNT RECORD KEY)                     
         USING ACTRECD,R2                                                       
         MVC   GASAVULA,ACTKULA                                                 
         MVC   ACTKCPY,CUABIN                                                   
*                                                                               
         LTR   R0,R1               TEST/SAVE A(LEDGER LIST PASSED)              
         BZ    GETACT06                                                         
         SR    RE,RE               RE=NUMBER OF ENTRIES IN TABLE                
GETACT02 CLI   0(R1),EOT           TEST E-O-T                                   
         BE    GETACT04                                                         
         LA    RE,1(RE)                                                         
         CLC   0(L'LDGTUL,R1),ACTKUNT                                           
         BE    GETACT06                                                         
         LA    R1,L'LDGTUL(R1)                                                  
         B     GETACT02                                                         
*                                                                               
GETACT04 BCT   RE,GETACTIL         LIST MUST CONTAIN ONE ENTRY ONLY             
         LR    R1,R0               RESTORE A(LIST UNIT/LEDGER)                  
         CLC   ACTKUNT(L'LDGTUL),BCSPACES                                       
         BH    GETACTIL                                                         
         MVC   ACTKUNT(L'LDGTUL),0(R1)                                          
*                                                                               
GETACT06 GOTO1 AGETLDG             CHECK LEDGER IS VALID                        
         BNE   ROUTX                                                            
         OC    BCCPYGLM,BCCPYGLM   IF USING NEW GL, CHECKMOA IF G               
         BZ    GETACT08                                                         
         CLI   ACTKUNT,C'G'                                                     
         BNE   GETACT08                                                         
         LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4                                                       
         CLC   LSTBMOSP,BCCPYGLM   MOA CAN'T BE = OR > THEN GL                  
         BNL   GETACTIG                                                         
         DROP  R4                                                               
*                                                                               
GETACT08 GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BL    ROUTL                                                            
         BH    GETACTIA                                                         
         L     R2,AIO1             R2=A(ACCOUNT RECORD)                         
         GOTO1 AGETELS,BCPARM,ACTRFST,0                                         
         TM    ACBSTAT,ACBSPROV    PROVISIONAL?                                 
         BO    GETACTIP            YES, CAN'T POST                              
*                                                                               
         TM    ACTRSTAT,ACTSDRFT                                                
         BO    GETACTID                                                         
*                                                                               
         L     RF,AOFFBLK                                                       
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
         GOTO1 ATSTSEC             TEST ACCOUNT SECURITY                        
         B     ROUTX                                                            
         SPACE 1                                                                
GETACTIG MVC   FVMSGNO,=AL2(AE$NOGEN)                                           
         B     GETACTIX                                                         
*                                                                               
GETACTIA MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     GETACTIX                                                         
*                                                                               
GETACTIP MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     GETACTIX                                                         
*                                                                               
GETACTID MVC   FVMSGNO,=AL2(AE$ACCNA)                                           
*                                                                               
GETACTIX MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'GASAVULA),GASAVULA                                      
         B     ROUTH                                                            
*                                                                               
GETACTIL MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'LDGTUL),GASAVULA                                        
         B     ROUTH                                                            
*                                                                               
         DROP  R2,RC                                                            
         SPACE 1                                                                
GAWORKD  DSECT                     ** GETACC/GETACT LOCAL W/S **                
GASAVULA DS    CL(L'ACTKULA)       SAVED UNIT/LEDGER/ACCOUNT                    
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST ACCOUNT SECURITY                                               *         
***********************************************************************         
         SPACE 1                                                                
TSTSEC   ICM   R2,15,ACALDG                                                     
         USING LDGTABD,R2          R2=A(LEDGER TABLE ENTRY)                     
*&&US*&& TM    CSBSECL,CPYBSSEC                                                 
*&&US*&& BZ    TSTSEC02                                                         
         CLC   LDGTSEC,TWAAUTH+1   TEST SECURITY LEVEL                          
         BH    TSTSECSL                                                         
         CLC   ACSECY,TWAAUTH+1                                                 
         BH    TSTSECSL                                                         
*                                                                               
TSTSEC02 TM    BCCPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    ROUTE                                                            
         CLC   TWAACCS,BCSPACES    TEST ANY LIMIT ACCESS                        
         BNH   ROUTE                                                            
*&&US*&& TM    CSBSECL,CPYBSOFF    TEST OVERRIDE OFFICE SECURITY                
*&&US*&& BZ    ROUTE                                                            
         CLC   BCCPYPRD,ACCODE+(ACTKUNT-ACTRECD)                                
         BNE   TSTSEC04                                                         
         OC    PSPROPPR,PSPROPPR   TEST PRODUCT PROFILE RESOLVED                
         BNZ   TSTSEC06                                                         
         B     ROUTE                                                            
*                                                                               
TSTSEC04 CLI   LDGTOFFP,LDGONONE   TEST NO OFFICE IN THIS LEDGER                
         BE    ROUTE                                                            
         CLI   LDGTOFFP,LDGOTRAN   TEST OFFICE IN TRANSACTIONS                  
         BE    ROUTE                                                            
*                                                                               
TSTSEC06 L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO1                                                     
         MVC   OFFAOPOS,LDGTOFFP                                                
         MVC   OFFAOFFC,ACOFFC                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    ROUTE                                                            
         SPACE 1                                                                
TSTSECSL MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(ACTKEND-L'ACTKCPY),IOKEY+(ACTKUNT-ACTRECD)                
         B     ROUTH                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT VALUES FROM AN ACCOUNT RECORD INTO AWS           *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST AS FOLLOWS):-                            *         
*        P1=A(FIRST ELEMENT ON ACCOUNT RECORD)                        *         
*        P2=A(WHERE TO PUT THE PROFILE ELEMENT IF FOUND)              *         
*        IOKEY=KEY OF ACCOUNT RECORD                                  *         
*        IODA=RECORD DISK ADDRESS IF FILE IS I/S D/A PAIR             *         
***********************************************************************         
         SPACE 1                                                                
GETELS   SR    R3,R3               CLEAR R3                                     
         ICM   R3,7,5(R1)          INSERT JUST THE ADDRESS                      
         L     R1,0(R1)            R1=A(FIRST ELEMENT ON RECORD)                
         XC    ACVALS(ACVALSL),ACVALS                                           
         SR    R0,R0                                                            
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING ACTRECD,R2                                                       
         CLC   ACTKACT,BCSPACES                                                 
         BE    GETELS06                                                         
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),BCSPACES                        
         BNE   GETELS06                                                         
         MVC   ACCODE,ACTKEY       SET ACCOUNT CODE                             
         OI    ACINDS1,ACIACTHI    SET HIGH LEVEL ACCOUNT                       
         MVC   ACDA,IODA           SET ACCOUNT DISK ADDRESS OR ZERO             
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),ACTKUNT                      
         BNE   GETELS06                                                         
*                                                                               
         SR    RE,RE               SET PRODUCTION ACCOUNT VALUES                
         IC    RE,BCJOBLEN         RE=L'CLIENT+PRODUCT+JOB                      
         SR    RF,RF                                                            
         IC    RF,BCPROLEN         RF=L'CLIENT+PRODUCT                          
         SR    RE,RF               RE=L'JOB                                     
         LA    RF,ACTKACT(RF)      RF=A(JOB IN KEY)                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES    TEST JOB CODE PRESENT                        
         BE    GETELS02                                                         
         OI    ACINDS1,ACIPRJOB    SET PRODUCTION JOB                           
         B     GETELS06                                                         
*                                                                               
GETELS02 IC    R0,BCCLILEN         R0=L'CLIENT                                  
         IC    RE,BCPROLEN         RE=L'CLIENT+PRODUCT                          
         SR    RE,R0               RE=L'PRODUCT                                 
         SR    RF,RE               RF=A(PRODUCT IN KEY)                         
         BCTR  RE,0                                                             
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES    TEST PRODUCT CODE PRESENT                    
         BE    GETELS04                                                         
         OI    ACINDS1,ACIPRPRO    SET PRODUCTION PRODUCT                       
         B     GETELS06                                                         
*                                                                               
GETELS04 OI    ACINDS1,ACIPRCLI    SET PRODUCTION CLIENT                        
*                                                                               
GETELS06 CLI   0(R1),0             TEST EOR                                     
         BE    GETELS32                                                         
         CLI   0(R1),NAMELQ        TEST NAME ELEMENT                            
         BE    GETELS10                                                         
         CLI   0(R1),RSTELQ        TEST STATUS ELEMENT                          
         BE    GETELS12                                                         
         CLI   0(R1),ABLELQ        TEST BALANCE ELEMENT                         
         BE    GETELS14                                                         
         CLI   0(R1),PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BE    GETELS16                                                         
         CLI   0(R1),RATETAXQ      TEST (TAX) RATE ELEMENT                      
         BE    GETELS18                                                         
         CLI   0(R1),SNMELQ        TEST SHORT NAME ELEMENT                      
         BE    GETELS20                                                         
         CLI   0(R1),SPAELQ        TEST SPECIAL POSTING A/C ELEMENT             
         BE    GETELS22                                                         
         CLI   0(R1),ASTELQ        TEST ACCOUNT STATUS ELEMENT                  
         BE    GETELS24                                                         
         CLI   0(R1),FFTELQ        TEST FREE FORM TEXT ELEMENT                  
         BE    GETELS26                                                         
*                                                                               
GETELS08 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETELS06                                                         
*                                                                               
         USING NAMELD,R1                                                        
GETELS10 SR    RE,RE               EXTRACT NAME                                 
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   ACNAME,BCSPACES                                                  
         EX    RE,*+8                                                           
         B     GETELS08                                                         
         MVC   ACNAME(0),NAMEREC                                                
*                                                                               
         USING RSTELD,R1                                                        
GETELS12 MVC   ACSTAT1,RSTSTAT1    STATUS BYTES 1 & 3 ALWAYS PRESENT            
         MVC   ACSTAT3,RSTSTAT3                                                 
         CLI   RSTLN,RSTLN2Q                                                    
         BL    *+16                                                             
         MVC   ACSTAT2,RSTSTAT2    STATUS BYTES 2 & 4 PRESENT                   
         MVC   ACSTAT4,RSTSTAT4                                                 
         MVC   ACCOFF,RSTOFFC      G/L OFFICE                                   
         CLI   RSTLN,RSTLN3Q                                                    
         BL    *+10                                                             
         MVC   ACSTAT5,RSTSTAT5    STATUS BYTE 5 PRESENT                        
         MVC   ACCOST,RSTCOSTG                                                  
         MVC   ACSECY,RSTSECY+1                                                 
         MVC   ACFLT1,RSTFILT1                                                  
         MVC   ACFLT2,RSTFILT2                                                  
         MVC   ACFLT3,RSTFILT3                                                  
         MVC   ACFLT4,RSTFILT4                                                  
         MVC   ACFLT5,RSTFILT5                                                  
*                                                                               
         TM    ACSTAT1,RSTSGPEI    SET BATCH STATUS                             
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSPERS                                                 
         TM    ACSTAT1,RSTSACIC                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSCLSE                                                 
         TM    ACSTAT1,RSTSACIL                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSLOCK                                                 
         TM    ACSTAT1,RSTSEADD                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSDEPT                                                 
*&&US                                                                           
         TM    ACSTAT1,RSTSVB2C                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSVEND                                                 
         TM    ACSTAT5,RSTSPROV                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSPROV                                                 
*&&                                                                             
*&&UK                                                                           
         TM    ACSTAT1,RSTSIVAT                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSIVAT                                                 
*&&                                                                             
         B     GETELS08                                                         
*                                                                               
GETELS14 OI    ACBSTAT,ACBSABAL    SET BALANCE ELEMENT PRESENT                  
         NI    ACINDS1,FF-ACIACTHI CLEAR HIGH LEVEL ACCOUNT                     
         OI    ACINDS1,ACIACTLO    SET LOW LEVEL ACCOUNT                        
         B     GETELS08                                                         
*                                                                               
GETELS16 STCM  R1,15,ACAPPR        SAVE A(PROFILE ELEMENT)                      
         B     GETELS08                                                         
*                                                                               
         USING RATELD,R1                                                        
GETELS18 MVC   ACTAXR,RATRATE      EXTRACT (TAX) RATE                           
         B     GETELS08                                                         
*                                                                               
         USING SNMELD,R1                                                        
GETELS20 MVC   ACNAMESH,SNMNAME    EXTRACT SHORT NAME                           
         B     GETELS08                                                         
*                                                                               
GETELS22 OC    ACASPA,ACASPA       TEST A(FIRST SPAEL SET)                      
         BNZ   *+8                                                              
         STCM  R1,15,ACASPA                                                     
         B     GETELS08                                                         
*                                                                               
         USING ASTELD,R1                                                        
GETELS24 MVC   ACCURCOD,ASTCUR     EXTRACT ACCOUNT CURRENCY CODE                
         MVC   ACKSVTYP,ASTKSVTY   EXTRACT KSV TYPE FOR GERMANY                 
         B     GETELS08                                                         
*                                                                               
         USING FFTELD,R1                                                        
GETELS26 CLI   FFTTYPE,FFTTVATC    TEST VAT CODE ELEMENT                        
         BNE   *+10                                                             
         MVC   ACVATCOD,FFTDATA                                                 
         B     GETELS08                                                         
*                                                                               
GETELS32 ICM   RE,15,ACALDG                                                     
         USING LDGTABD,RE          RE=A(LEDGER TABLE ENTRY)                     
         CLC   LDGTUL,BCCPYPRD     TEST PRODUCTION LEDGER                       
         BNE   GETELS34                                                         
         LTR   R3,R3               TEST OUTPUT PROFILE AREA PASSED              
         BZ    GETELS34                                                         
         XC    0(L'PSJOBPPR,R3),0(R3)                                           
         ICM   RF,15,ACAPPR        TEST RECORD HAS A PROFILE ELEMENT            
         BNZ   *+16                                                             
         MVI   PPREL-PPRELD(R3),PPRELQ                                          
         MVI   PPRLN-PPRELD(R3),PPRLN1Q                                         
         B     GETELS34                                                         
         MVC   0(L'PSJOBPPR,R3),0(RF)                                           
         LA    R0,PSCLIPPR                                                      
         CR    R0,R3                                                            
         BNE   *+14                                                             
         XC    PSPROVLS(PSPROVLL+PSJOBVLL),PSPROVLS                             
         B     GETELS34                                                         
         LA    R0,PSPROPPR                                                      
         CR    R0,R3                                                            
         BNE   GETELS34                                                         
         XC    PSJOBVLS(PSJOBVLL),PSJOBVLS                                      
*                                                                               
GETELS34 MVC   ACOFFC,BCSPACES                                                  
         CLC   BCCPYPRD,ACCODE+(ACTKUNT-ACTRECD)                                
         BNE   GETELS36                                                         
         OC    PSPROPPR,PSPROPPR   TEST PRODUCT PROFILE RESOLVED                
         BZ    GETELSX                                                          
         MVC   ACOFFC,PSJOBPPR+(PPRGAOFF-PPRELD)                                
         CLC   ACOFFC,BCSPACES                                                  
         BH    GETELSX                                                          
         MVC   ACOFFC,PSPROPPR+(PPRGAOFF-PPRELD)                                
         CLC   ACOFFC,BCSPACES                                                  
         BH    GETELSX                                                          
         MVC   ACOFFC,PSCLIPPR+(PPRGAOFF-PPRELD)                                
         B     GETELSX                                                          
*                                                                               
GETELS36 CLI   LDGTOFFP,LDGONONE   TEST ANY OFFICE IN THIS LEDGER               
         BE    GETELSX                                                          
         CLI   LDGTOFFP,LDGOTRAN   TEST OFFICE IN TRANSACTIONS                  
         BE    GETELSX                                                          
         CLI   LDGTOFFP,LDGOFLT1   TEST OFFICE IN FILTERS                       
         BNL   GETELS38                                                         
         MVC   BCWORK(1),LDGTOFFP                                               
         NI    BCWORK,FF-LDGOKEY2                                               
         CLI   BCWORK,LDGOKEY                                                   
         BH    GETELSX                                                          
         SR    R1,R1                                                            
         IC    R1,BCWORK                                                        
         LA    R1,ACCODE+(ACTKACT-ACTRECD-1)(R1)                                
         MVC   ACOFFC+0(1),0(R1)                                                
         TM    LDGTOFFP,LDGOKEY2   TEST 2 CHARACTER OFFICE IN KEY               
         BZ    *+10                                                             
         MVC   ACOFFC+1(1),1(R1)                                                
         B     GETELSX                                                          
*                                                                               
GETELS38 PACK  BCDUB,LDGTOFFP      OFFICE IN FILTERS                            
         CVB   R1,BCDUB            VALUE IS OF FORM X'F1'-X'F4'                 
         LA    R1,ACFLTS-1(R1)                                                  
         MVC   ACOFFC(1),0(R1)                                                  
*                                                                               
GETELSX  B     ROUTE                                                            
         DROP  R1,R2,RE                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A TEXT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
GETGEN   LA    R1,BCWORK                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,FVMSGNO                                                  
         LA    R0,BASMSG                                                        
         CLI   FVOMTYP,GTMTXT                                                   
         BNE   *+8                                                              
         LA    R0,BOWORK1                                                       
         STCM  R0,7,GTAOUT                                                      
         OI    GT1INDS,GT1OWRK                                                  
         MVI   GTMAXL,L'BASMSG                                                  
         MVC   GTMTYP,FVOMTYP                                                   
         GOTO1 VGETTXT                                                          
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET PRODUCTION WORK CODE DESCRIPTION INTO BOWORK1                   *         
*                                                                     *         
* NTRY - R1=A(WORK CODE)                                              *         
* EXIT - BOWORK1=WORK CODE DESCRIPTION                                *         
*        IOAREA2=WORK CODE RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
GETWRK   MVC   FVXTRA,BCSPACES                                                  
         XC    BOWORK1,BOWORK1     CLEAR OUTPUT AREA                            
         CLC   =C'99',0(R1)                                                     
         BE    ROUTE                                                            
         CLC   =C'**',0(R1)                                                     
         BE    ROUTE                                                            
         MVC   FVXTRA(L'WCOKWRK),0(R1)                                          
         LA    R2,IOKEY                                                         
         USING WCORECD,R2                                                       
         MVC   WCOKEY,BCSPACES     BUILD KEY TO READ                            
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   WCOKWRK,0(R1)                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BNE   GETWRKN                                                          
*                                                                               
         L     R2,AIO2             R2=A(WORK CODE RECORD)                       
         LA    R1,WCORFST                                                       
         USING WCOELD,R1           LOCATE WORK CODE ELEMENT                     
         SR    R0,R0                                                            
GETWRK02 CLI   WCOEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   WCOEL,WCOELQ        TEST WORK CODE ELEMENT                       
         BE    *+14                                                             
         IC    R0,WCOLN                                                         
         AR    R1,R0                                                            
         B     GETWRK02                                                         
         MVC   BOWORK1(WCOLNQ-(WCODESC-WCOELD)),WCODESC                         
         MVC   FVXTRA,BCSPACES                                                  
         B     ROUTE                                                            
*                                                                               
GETWRKN  TM    IOERR,IOERNF        TEST RECORD NOT FOUND                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     ROUTH                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE AN OFFICE CODE                                             *         
*                                                                     *         
* NTRY - P1=AL1(NON-ZERO FOR DEPARTMENT READ),AL3(OFFICE CODE)        *         
* EXIT - CC=EQUAL IF OFFICE VALID                                     *         
*        CC=NOT EQUAL IF OFFICE INVALID WITH FVMSGNO SET              *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         SR    R0,R0                                                            
         IC    R0,0(R1)            OPTION TO READ DEPARTMENT                    
         L     R1,0(R1)                                                         
         MVC   ACOFFC,0(R1)        SET OFFICE CODE                              
         TM    BCCPYST4,CPYSOFF2   2 BYTE OFFICE COMPANY?                       
         BZ    VALOFF1A            NO                                           
         CLI   ACOFFC+1,C' '       MUST HAVE 2 NON-SPACE CHARS                  
         BNH   ROUTH                                                            
         B     VALOFF02                                                         
*                                                                               
VALOFF1A CLI   ACOFFC+1,C' '       IF NOT CONVERTED - 2 BYTES IS BAD            
         BH    ROUTH                                                            
         B     VALOFF05                                                         
*                                                                               
VALOFF02 LA    R2,IOKEY            READ NEW OFFICE RECORD                       
         USING OFFRECD,R2                                                       
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,ACOFFC                                                   
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    VALOFF04                                                         
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACOFFC),ACOFFC                                          
         MVC   FVMSGNO,=AL2(AE$DSKER)                                           
         BL    ROUTL               LOW - DISK ERROR                             
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         B     ROUTH               HIGH - LOGICAL ERROR                         
*                                                                               
VALOFF04 TM    OFFKSTA,OFFSLIST                                                 
         BZ    VALOFF05                                                         
         MVC   FVMSGNO,=AL2(AE$NOLST)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACOFFC),ACOFFC                                          
         B     ROUTH                                                            
*                                                                               
VALOFF05 TM    CSBSECL,CPYBSOFF    TEST OFFICE SECURITY INHIBITED               
         BZ    VALOFF06                                                         
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,ACOFFC                                                  
         MVI   OFFAACT,OFFAPST                                                  
         GOTO1 VOFFAL                                                           
         BE    VALOFF06            OFFICE IS VALID FOR POSTING                  
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACOFFC),ACOFFC                                          
         B     ROUTH                                                            
*                                                                               
VALOFF06 LTR   R0,R0               TEST READ DEPARTMENT/OFFICE RECORD           
         BZ    VALOFFY                                                          
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT(L'ACOFFC),ACOFFC                                         
         GOTO1 AIO,IOREAD+IOACCFIL+IO2                                          
         BE    VALOFFY             EQUAL - GOOD EXIT                            
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(ACTKEND-L'ACTKCPY),IOKEYSAV+(ACTKUNT-ACTRECD)             
         MVC   FVMSGNO,=AL2(AE$DSKER)                                           
         BL    ROUTL               LOW - DISK ERROR                             
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         B     ROUTH               HIGH - LOGICAL ERROR                         
*                                                                               
VALOFFY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTE                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* MERGE CLIENT, PRODUCT & JOB PROFILES INTO COMPOSITE PROFILE         *         
***********************************************************************         
         SPACE 1                                                                
MRGPRF   MVC   PSCOMPPR,PSCLIPPR                                                
         LA    R0,2                ONCE FOR PROD AND ONCE FOR JOB               
         LA    RE,PSPROPPR         PRODUCT LEVEL                                
         USING PPRELD,RE                                                        
MRGPRF2  OC    PPRELD(L'PSCOMPPR),PPRELD                                        
         BZ    MRGPRF6                                                          
MRGPRF4  OC    PPRGRUP,PPRGRUP                                                  
         BZ    *+10                                                             
         MVC   PSCOMPPR+(PPRGRUP-PPRELD)(L'PPRGRUP),PPRGRUP                     
         OC    PPRRECV,PPRRECV                                                  
         BZ    *+10                                                             
         MVC   PSCOMPPR+(PPRRECV-PPRELD)(L'PPRRECV),PPRRECV                     
         OC    PPRCOST,PPRCOST                                                  
         BZ    *+10                                                             
         MVC   PSCOMPPR+(PPRCOST-PPRELD)(L'PPRCOST),PPRCOST                     
         OC    PPRBTYPE,PPRBTYPE                                                
         BZ    *+10                                                             
         MVC   PSCOMPPR+(PPRBTYPE-PPRELD)(L'PPRBTYPE),PPRBTYPE                  
         CLC   PPRUFORA,BCSPACES                                                
         BNH   *+10                                                             
         MVC   PSCOMPPR+(PPRUFORA-PPRELD)(L'PPRUFORA),PPRUFORA                  
         CLC   PPRGAOFF,BCSPACES                                                
         BNH   *+10                                                             
         MVC   PSCOMPPR+(PPRGAOFF-PPRELD)(L'PPRGAOFF),PPRGAOFF                  
MRGPRF6  LA    RE,PSJOBPPR         JOB LEVEL                                    
         BCT   R0,MRGPRF2                                                       
MRGPRFX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* STRING TOGETHER NARRATIVE FIELD(S) AND MOVE TO CALLER'S OUTPUT AREA *         
*                                                                     *         
* NTRY - P1=N'FIELDS OR ZERO,AL3(FIRST NARRATIVE FIELD HEADER)        *         
*      - P2=A(DESTINATION OF MERGED NARRATIVE OR ZERO)                *         
*                                                                     *         
* EXIT - R6=L'MERGED NARRATIVE IF OLD STYLE CALL (P2=NON-ZERO)        *         
*            OR                                                       *         
*        BOELEM=NARRATIVE STRING IF NEW STYLE CALL (P2=ZERO) &        *         
*        BOHALF1=LENGTH OF NARRATIVE STRING                           *         
***********************************************************************         
         SPACE 1                                                                
VALNAR   LM    R2,R3,0(R1)         R2=A(TWA FLD HEADER), R3=A(OUTPUT)           
         MVI   BCFLAG,0            SET OLD STYLE CALL                           
         LTR   R3,R3               TEST OLD STYLE CALL                          
         BNZ   *+12                                                             
         MVI   BCFLAG,1            SET NEW STYLE CALL                           
         LA    R3,BOELEM                                                        
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BNZ   *+8                                                              
         LA    R0,10               R0=MAXIMUM NUMBER OF LINES                   
         SR    R4,R4                                                            
*                                                                               
VALNAR02 CLI   FVTLEN-FVIHDR(R2),0           TEST END OF SCREEN                 
         BE    VALNAR08                                                         
         CLI   FVTLEN-FVIHDR(R2),L'FVIHDR+1  TEST ONE BYTE TAB FIELD            
         BE    VALNAR08                                                         
         TM    FVATRB-FVIHDR(R2),FVAPROT     TEST PROTECTED FIELD               
         BNZ   VALNAR06                                                         
*                                                                               
         GOTO1 AFVAL,(R2)          VALIDATE INPUT FIELD                         
         BNE   VALNAR04                                                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN           RF=L'INPUT-1                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD      MOVE NARRATIVE TO OUTPUT AREA                
         LA    R3,1(RF,R3)         POINT TO NEXT OUTPUT POSITION                
         MVI   0(R3),C' '                                                       
         AHI   R3,1                                                             
         LA    R4,2(RF,R4)         INCREMENT TOTAL OUTPUT LENGTH                
*                                                                               
VALNAR04 BCT   R0,VALNAR06         DO FOR NUMBER OF INPUT FIELDS                
         B     VALNAR08                                                         
*                                                                               
VALNAR06 SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R2)                                             
         AR    R2,RF               BUMP TO NEXT FIELD                           
         B     VALNAR02                                                         
*                                                                               
VALNAR08 LTR   R4,R4               TEST ANY NARRATIVE FOUND                     
         BNZ   *+12                                                             
         MVI   0(R3),C' '          NO - SET A SINGLE BLANK                      
         LHI   R4,2                                                             
         BCTR  R4,0                                                             
         CLI   BCFLAG,0                                                         
         BE    VALNAROX                                                         
         STH   R4,BOHALF1          SET LENGTH OF OUTPUT STRING                  
         B     ROUTE                                                            
*                                                                               
VALNAROX L     RE,4(RD)                                                         
         ST    R4,44(RE)           SET LENGTH TO RETURN IN R6                   
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CALL GETOPT, READ JOB RECORD AND SAVE FORMAT TYPE.                  *         
* NTRY - R1 POINTS TO PARAMETER LIST AS FOLLOWS:-                     *         
*        P1    B0   = X'80' MEANS A(WORK CODE) PASSED IN P2           *         
*                   = X'40' MEANS NEW BILLING EXTENSION REQUIRED      *         
*                     (ADDRESS OF WHICH IS RETURNED IN P2)            *         
*              B1-3 = A(JOB KEY)                                      *         
*        P2    B0-3 = A(WORK CODE)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING GJWORKD,RC          RC=A(LOCAL W/S)                              
GETJOB   ST    R1,GJAPARM          SAVE A(PARAMETER LIST)                       
         MVC   GJPARM(GJPARML),0(R1) EXTRACT PARAMETER LIST                     
         L     R1,GJPAJOB                                                       
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKEND),0(R1)                                             
         L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2         R2=A(GETOPT BLOCK)                           
         MVC   GOADM,VDMGR                                                      
         MVC   GOSELCUL,0(R1)                                                   
         MVC   GOAEXT,AGOXBLK      SET A(BLOCK EXTENSION)                       
         LA    R1,ACTKACT-ACTRECD(R1)                                           
         MVI   ACJOBFRM,ACJOBOLD   INITIALIZE JOB FORMAT TO OLD AND             
         NI    ACOPSTAT,X'FF'-(ACOXJOB+ACOMCSE) NOT XJOB                        
*                                                                               
         MVC   GOSELCLI,BCSPACES                                                
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R1)                                                
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVC   GOSELPRO,BCSPACES                                                
         LA    RE,1(RF)                                                         
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R1)                                                
         CLC   GOSELPRO,BCSPACES                                                
         BNE   *+10                                                             
         XC    GOSELPRO,GOSELPRO                                                
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVC   GOSELJOB,BCSPACES                                                
         IC    RE,BCJOBLEN                                                      
         IC    RF,BCPROLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R1)                                                
         CLC   GOSELJOB,BCSPACES                                                
         BNE   *+10                                                             
         XC    GOSELJOB,GOSELJOB                                                
*                                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO1                                            
         BNE   ROUTX                                                            
         MVC   GOAJOB,AIO1         PASS ADDRESS TO GETOPT                       
*                                                                               
         TM    GJPINDS,GJPIWORK    TEST WORK CODE PASSED                        
         BZ    *+14                                                             
         L     RF,GJPAWRK                                                       
         MVC   GOSELWC,0(RF)                                                    
*                                                                               
         TM    GJPINDS,GJPIBXTN    TEST BILLING EXTENSION REQUIRED              
         BZ    *+16                                                             
         LA    RF,GJBEXT                                                        
         ST    RF,GOABEXT                                                       
         ST    RF,GJPAXTN                                                       
*                                                                               
         GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
*                                                                               
         L     R1,AIO1                                                          
         LA    R1,ACCORFST(R1)                                                  
         SR    R0,R0                                                            
         USING JOBELD,R1                                                        
GETJOB2  CLI   JOBEL,0                                                          
         BE    GETJOBX                                                          
         CLI   JOBEL,JOBELQ                                                     
         BE    *+14                                                             
         IC    R0,JOBLN                                                         
         AR    R1,R0                                                            
         B     GETJOB2                                                          
*                                                                               
         TM    JOBSTA1,JOBSNEST    TEST USING NEW ESTIMATING SYSTEM             
         BZ    GETJOB4                                                          
         MVI   ACJOBFRM,ACJOBNEW   SET NEW ESTIMATING FLAG                      
*                                                                               
GETJOB4  TM    JOBSTA1,JOBSXJOB    TEST/SET EXPENSE JOB INDICATOR               
         BZ    GETJOB6                                                          
         OI    ACOPSTAT,ACOXJOB                                                 
*                                                                               
GETJOB6  TM    JOBSTA1,JOBSMCSE    TEST USING MCS                               
         BZ    GETJOBX                                                          
         OI    ACOPSTAT,ACOMCSE                                                 
         MVI   ACJOBFRM,ACJOBMCS                                                
*                                                                               
GETJOBX  L     R1,GJAPARM          RETURN (UPDATED) PARAMETER LIST              
         MVC   0(GJPARML,R1),GJPARM                                             
         B     ROUTE                                                            
         DROP  R1,R2,RC                                                         
         SPACE 1                                                                
GJWORKD  DSECT                     ** GETJOB S/R LOCAL W/S **                   
GJAPARM  DS    A                   A(PARAMETER LIST)                            
GJPARM   DS    0X                  SAVED PARAMETER LIST                         
GJPINDS  DS    0XL1                INDICATORS                                   
GJPIWORK EQU   X'80'               WORK CODE PASSED IN PLIST                    
GJPIBXTN EQU   X'40'               NEW BILLING EXTENSION REQUIRED               
GJPAJOB  DS    A                   A(JOB KEY)                                   
GJPAWRK  DS    A                   A(WORK CODE)                                 
GJPAXTN  EQU   GJPAWRK             A(NEW BILLING EXTENSION BLOCK)               
GJPARML  EQU   *-GJPARM                                                         
GJBEXT   DS    XL(L'GOBBLOCK)      GOBLOCK EXTENSION                            
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK GOBATCH OPTION AND IF REQUIRED, CALL JOBBER                   *         
***********************************************************************         
         SPACE 1                                                                
VALGOB   MVI   ACJOBEST,ACJOBNON   INDICATE NO CHECKS NEEDED                    
         L     R3,AJOBBLK                                                       
         USING JOBLOCKD,R3         R3=A(JOBBER BLOCK)                           
         L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2         R2=A(GETOPT BLOCK)                           
         CLI   ACJOBFRM,ACJOBNEW   IS THIS A NEW FORMAT JOB?                    
         BE    *+12                                                             
         CLI   ACJOBFRM,ACJOBMCS   IS THIS A MCS FORMAT JOB?                    
         BNE   VALGOB4                                                          
         CLI   GONEEDES,GOOPTNQ    DO WE NEED AN ESTIMATE TO BILL?              
         BE    VALGOB6                                                          
         CLI   GOBATCH,GOOPTNQ     IS AMOUNT/EST CHECK REQUIRED?                
         BE    VALGOB6                                                          
         GOTO1 AGETEST             GET ESTIMATE VALUES                          
*                                                                               
         CLI   GOBATCH,GOBAPPRQ    JUST CHECK APPROVED?                         
         BE    VALGOB2                                                          
         MVI   ACJOBEST,ACJOBZER                                                
         CLI   GOBATCH,GOBZEROQ    INDICATE CHECK FOR ZERO AMOUNT               
         BE    VALGOB6                                                          
*                                                                               
         MVI   ACJOBEST,ACJOBNEW   INDICATE NEW METHOD FOR CHECK                
         CLI   GOBATCH,GOBCASHQ    CHECK AMOUNT ONLY?                           
         BE    VALGOB6                                                          
*                                                                               
         CLI   GOBATCH,GOBMAXA     COMPARE AGAINST MAX?                         
         BNE   VALGOB0             NO                                           
         MVI   ACJOBEST,ACJOBMAX                                                
         B     VALGOB1                                                          
*                                                                               
VALGOB0  CLI   GOBATCH,GOBTOTL                                                  
         BNE   VALGOB1                                                          
         MVI   ACJOBEST,ACJOBTOT   INDICATE TOTAL CHECKING                      
         CLI   CSBITO,08           ONLY VALID FOR 8 OR 10                       
         BE    VALGOB1                                                          
         CLI   CSBITO,1                                                         
         BE    VALGOB1                                                          
         CLI   CSBITO,61                                                        
         BE    VALGOB1                                                          
         MVI   ACJOBEST,ACJOBNON                                                
*                                                                               
VALGOB1  CLI   GONEEDAE,GOOPTYQ    WE NEED APPROVAL TO BILL?                    
         BNE   VALGOB6                                                          
*                                                                               
VALGOB2  MVC   FVMSGNO,=AL2(AE$CBENA)                                           
         CLI   JBHIAPP,0           IS ESTIMATE APPROVED?                        
         BE    VALGOB8             NO, ERROR                                    
         B     VALGOB6             YES, DO REST OF CHECKING                     
*                                                                               
VALGOB4  TM    BCCPYST1,CPYSCIVE   CHECK AMOUNT AGAINST ESTIMATE?               
         BZ    VALGOB6                                                          
         GOTO1 AGETEST             GET ESTIMATE VALUES                          
         MVI   ACJOBEST,ACJOBOLD   INDICATE OLD METHOD                          
*                                                                               
VALGOB6  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALGOB8  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET ESTIMATE VALUE(S) FROM JOBBER                                   *         
***********************************************************************         
         SPACE 1                                                                
GETEST   GOTO1 VJOBCOL,BCPARM,('ESTPARM',ESTLIST),BOELEM,ACOM                   
         CLI   4(R1),0             TEST FOR ERRORS                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,AJOBBLK                                                       
         USING JOBLOCKD,R2         R2=A(JOBBER BLOCK)                           
         MVC   JBAJOB,AIO1                                                      
         LA    R0,BOELEM                                                        
         ST    R0,JBACOLS                                                       
         MVC   JBACOM,ACOM                                                      
         MVC   JBAGOBLK,AGOPBLK                                                 
         MVC   JBAIO,AIO3                                                       
         MVC   JBAKEY,AIO1                                                      
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
         MVI   JBGETDFT,C'Y'       INCLUDE DRAFTS                               
*                                                                               
         MVI   JBSELWRK,JBNOZERO   SKIP OVER ZERO WORKCODES                     
*                                                                               
         MVI   JBSELFUN,JBGETDE                                                 
         LA    RE,ESTLIST                                                       
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 VJOBBER,BCPARM,AJOBBLK                                           
         CLI   JBERROR,0           TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
GETESTX  B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 1                                                                
*&&UK                                                                           
ESTPARM  EQU   255                                                              
ESTLIST  DC    AL2(AC#CE,0)                                                     
*&&                                                                             
*&&US                                                                           
ESTPARM  EQU   0                                                                
ESTLIST  DC    AL1(20,0,0,0,0,12,0,0),C'CE,MAXCE,ACT'                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORK CODE CURRENT ESTIMATE AMOUNT                          *         
***********************************************************************         
         SPACE 1                                                                
VALWRK   L     R1,0(R1)            GET WORKCODE BUCKETS                         
         L     R2,AJOBBLK                                                       
         USING JOBLOCKD,R2         R2=A(JOBBER BLOCK)                           
         L     R3,ACOLTAB                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    VALWRK20            YES                                          
         USING JBCOLD,R3           R3=A(COLUMN OUTPUT TABLE)                    
         CLI   ACJOBEST,ACJOBTOT   DOING TOTAL CHECK?                           
         BNE   VALWRK00            NO                                           
         MVC   FVMSGNO,=AL2(AE$AEETO)                                           
         ZAP   BCDUB,JBCOLVAL+12(6)  COMPARE TO MAX % OVER EST                  
         AP    BCDUB,L'JBCOLWC(L'JBCOLVAL,R1)                                   
         CP    BCDUB,JBCOLVAL+6(6)                                              
         BH    VALWRKX                                                          
         L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2                                                      
         CP    GOOVRAMT,=P'0'      ZERO EQUAL BLANK                             
         BE    VALWRKX                                                          
         ZAP   BCDUB,JBCOLVAL+12(6) COMPARE TO MAX AMT OVR EST                  
         AP    BCDUB,L'JBCOLWC(L'JBCOLVAL,R1)                                   
         SP    BCDUB,JBCOLVAL      LESS CURRENT ESTIMATE                        
         CP    BCDUB,GOOVRAMT                                                   
         B     VALWRKX             EXIT WITH RETURN CODE                        
         DROP  R2                                                               
*                                                                               
         USING JOBLOCKD,R2         R2=A(JOBBER BLOCK)                           
VALWRK00 LH    R0,JBNROWS                                                       
*                                                                               
VALWRK02 CLI   JBCOLTYP,JBCOLTWC   TEST WORK CODE ENTRY                         
         BNE   VALWRK06                                                         
         CLC   JBCOLWC,0(R1)       TEST WORK CODE MATCHES                       
         BNE   VALWRK06                                                         
         CLI   ACJOBEST,ACJOBZER   TEST CHECK ZERO AMOUNT                       
         BE    VALWRK04                                                         
         CLI   ACJOBEST,ACJOBMAX   TEST AGAINST MAXIMUM ESTIMATE                
         BE    *+14                                                             
         CP    L'JBCOLWC(L'JBCOLVAL,R1),JBCOLVAL                                
         B     VALWRKX             EXIT WITH RETURN CODE                        
*                                                                               
         ZAP   BCDUB,JBCOLVAL+12(6)  GET THE ACTUALS                            
         AP    BCDUB,L'JBCOLWC(L'JBCOLVAL,R1)                                   
         CP    BCDUB,JBCOLVAL+6(6)                                              
         B     VALWRKX             EXIT WITH RETURN CODE                        
*                                                                               
VALWRK04 CP    JBCOLVAL,=P'0'      TEST AMOUNT GREATER THAN ZERO                
         BNH   ROUTH               NO - ERROR                                   
         B     ROUTE                                                            
*                                                                               
VALWRK06 AH    R3,JBLCOL           BUMP TO NEXT COLUMN ENTRY                    
         BCT   R0,VALWRK02                                                      
         B     ROUTH               SET RETURN CODE TO HIGH                      
         DROP  R3                                                               
*                                                                               
         USING MJETABD,R3                                                       
VALWRK20 CLI   ACJOBEST,ACJOBTOT   CHECKING TOTALS                              
         BNE   VALWRK22                                                         
         MVC   FVMSGNO,=AL2(AE$AEETO)                                           
         ZAP   BCDUB,MJETVAL+12(6) COMPARE TO MAX % OVR EST                     
         AP    BCDUB,L'MJETWCD(L'MJETVAL,R1)                                    
         CP    BCDUB,MJETVAL+6(6)                                               
         BH    VALWRKX                                                          
         L     R2,AGOPBLK                                                       
         USING GOBLOCKD,R2                                                      
         CP    GOOVRAMT,=P'0'      ZERO EQUAL BLANK                             
         BE    VALWRKX                                                          
         ZAP   BCDUB,MJETVAL+12(6) COMPARE TO MAX AMT OVR EST                   
         AP    BCDUB,L'MJETWCD(L'MJETVAL,R1)                                    
         SP    BCDUB,MJETVAL       LESS CURRENT ESTIMATE                        
         CP    BCDUB,GOOVRAMT                                                   
         B     VALWRKX             EXIT WITH RETURN CODE                        
*                                                                               
VALWRK22 CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    ROUTH               YES                                          
         CLI   MJETTYP,MJETTWQ     WORKCODE ENTRY?                              
         BNE   VALWRK26            NO                                           
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   VALWRK26                                                         
         CLC   MJETWCD,0(R1)       WORKCODE MATCHES?                            
         BNE   VALWRK26            NO                                           
         CLI   ACJOBEST,ACJOBZER   TEST CHECK ZERO AMOUNT                       
         BE    VALWRK24                                                         
         CLI   ACJOBEST,ACJOBMAX   TEST AGAINST MAXIMUM ESTIMATE                
         BE    *+14                                                             
         CP    L'MJETWCD(L'MJETVAL,R1),MJETVAL                                  
         B     VALWRKX             EXIT WITH RETURN CODE                        
*                                                                               
         ZAP   BCDUB,MJETVAL+12(6)  GET THE ACTUALS                             
         AP    BCDUB,L'MJETWCD(L'MJETVAL,R1)                                    
         CP    BCDUB,MJETVAL+6(6)                                               
         B     VALWRKX             EXIT WITH RETURN CODE                        
*                                                                               
VALWRK24 CP    MJETVAL,=P'0'       TEST AMOUNT GREATER THAN ZERO                
         BNH   ROUTH               NO - ERROR                                   
         B     ROUTE                                                            
*                                                                               
                                                                                
VALWRK26 XR    R0,R0               BUMP TO NEXT COLUMN ENTRY                    
         IC    R0,MJETLEN                                                       
         AR    R3,R0                                                            
         B     VALWRK22                                                         
*                                                                               
VALWRKX  B     ROUTX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LOOKUP RECORD TABLE & TEST AUTHORISATION                            *         
*                                                                     *         
* NTRY - R1=A(RECORD NUMBER)                                          *         
***********************************************************************         
         SPACE 1                                                                
TSTREC   L     R2,ARECTAB                                                       
         USING RECTABD,R2          LOCATE RECTAB ENTRY                          
         MVC   BCWORK(L'RECNUMB),0(R1)                                          
         MVI   BCWORK+L'RECNUMB,0  SET 'ALL' ACTION VALUE                       
         LA    R1,BCWORK                                                        
*                                                                               
TSTREC2  CLI   RECTABD,EOT         TEST END OF TABLE                            
         BE    ROUTH                                                            
         CLC   RECNUMB,BCWORK      MATCH ON RECORD NUMBER                       
         BNE   TSTREC4                                                          
         TM    RECINDS1,RECIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    TSTREC4                                                          
         GOTO1 ATSTACS             TEST RECORD AUTHORISATION                    
         BNE   TSTREC4                                                          
*                                                                               
         CLI   RECCTRY,0           ANY COUNTRY SET?                             
         BE    TSTREC3                                                          
         CLC   RECCTRY,CUCTRY      MATCH ON COUNTRY?                            
         BE    TSTREC3                                                          
*                                                                               
         TM    RECCTRY,CTRYNOT     VALID FOR ALL BUT THIS COUNTRY?              
         BZ    TSTREC4                                                          
*                                                                               
         MVC   BCBYTE1,RECCTRY                                                  
         NI    BCBYTE1,FF-(CTRYNOT)                                             
         CLC   BCBYTE1,CUCTRY                                                   
         BE    TSTREC4                                                          
*                                                                               
TSTREC3  ST    R2,ARECNTRY         SAVE A(RECORD TABLE ENTRY)                   
         SR    RE,RE                                                            
         ICM   RE,3,RECNAMEL       EXTRACT LOWER CASE RECORD WORD               
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(RECNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,RECNAMEU       EXTRACT UPPER CASE RECORD WORD               
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK+RECNAMLQ(RECNAMLQ),0(RE)                                  
         B     ROUTE                                                            
*                                                                               
TSTREC4  LA    R2,RECTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     TSTREC2                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOOKUP ACTION TABLE                                                 *         
*                                                                     *         
* NTRY - R1=A(ACTION NUMBER)                                          *         
***********************************************************************         
         SPACE 1                                                                
TSTACT   L     R2,AACTTAB                                                       
         USING ACTTABD,R2          LOCATE ACTTAB ENTRY                          
*                                                                               
TSTACT2  CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BE    ROUTH                                                            
         CLC   ACTNUMB,0(R1)       MATCH ACTION NUMBER                          
         BNE   TSTACT4                                                          
         TM    ACTINDS1,ACTIDDS    TEST DDS ONLY ACTION TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    TSTACT4                                                          
         TM    ACTINDS1,ACTIUPD    IS THIS AN UPDATIVE ACTION?                  
         BZ    TSTACT3             NO, CONTINUE                                 
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    TSTACT3                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(360)                                                
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    ROUTH               YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(358)                                                
         MVC   FVXTRA(L'FACUPD),FACUPD                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    ROUTH               YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(358)   CONNECTED TO READ ONLY SYSTEM                
         MVC   FVXTRA,BCSPACES                                                  
         B     ROUTH                                                            
*                                                                               
TSTACT3  ST    R2,AACTNTRY         SAVE A(ACTION TABLE ENTRY)                   
         SR    RE,RE                                                            
         ICM   RE,3,ACTNAMEL       EXTRACT LOWER CASE RECORD WORD               
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(ACTNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,ACTNAMEU       EXTRACT UPPER CASE ACTION WORD               
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK+ACTNAMLQ(ACTNAMLQ),0(RE)                                  
         B     ROUTE                                                            
*                                                                               
TSTACT4  LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     TSTACT2                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOOKUP MIX TABLE & TEST AUTHORISATION                               *         
*                                                                     *         
* NTRY - R1=A(RECORD/ACTION COMBO)                                    *         
***********************************************************************         
         SPACE 1                                                                
TSTMIX   L     R2,AMIXTAB                                                       
         USING MIXTABD,R2          LOCATE MIXTAB ENTRY                          
         XR    R0,R0                                                            
         ICM   R0,1,BCMIXLEN       GET LENGTH OF ENTRIES                        
         BNZ   *+8                                                              
         LA    R0,MIXTABL                                                       
TSTMIX02 CLI   MIXTABD,EOT         TEST END OF TABLE                            
         BE    ROUTH                                                            
         CLC   MIXKEY,0(R1)        MATCH RECORD                                 
         BNE   TSTMIX04                                                         
         GOTO1 ATSTACS             TEST AUTHORISATION                           
         BNE   ROUTH                                                            
         TM    MIXINDS1,MIXIDDS    TEST DDS ONLY COMBO                          
         BZ    TSTMIX06                                                         
         TM    CUSTAT,CUSDDS       YES - TEST IF A DDS TERMINAL                 
         BNZ   TSTMIX06                                                         
TSTMIX04 AR    R2,R0               BUMP TO NEXT TABLE ENTRY                     
         B     TSTMIX02                                                         
*                                                                               
TSTMIX06 ST    R2,AMIXNTRY         SAVE A(COMBO TABLE ENTRY)                    
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOOKUP SELECT TABLE FOR RECORD/ACTION AND SET AOVERSEL              *         
*                                                                     *         
* NTRY - R1=A(RECORD/ACTION COMBO)                                    *         
***********************************************************************         
         SPACE 1                                                                
SETSEL   L     R2,ASELTAB          RESOLVE RECORD/ACTION SELECT TABLE           
         SR    RE,RE                                                            
         XC    AOVERSEL,AOVERSEL                                                
         USING SELTABD,R2                                                       
SETSEL02 CLI   SELTABD,EOT         TEST EOT                                     
         BE    ROUTE                                                            
         CLC   SELKEY,0(R1)        MATCH ON NEW RECORD/ACTION                   
         BE    *+14                                                             
         ICM   RE,3,SELLEN         NO - BUMP TO NEXT TABLE ENTRY                
         AR    R2,RE                                                            
         B     SETSEL02                                                         
         LA    R2,SELHEADL(R2)     BUMP TO FIRST ENTRY                          
         STCM  R2,15,AOVERSEL      SET A(SUB-TABLE)                             
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BATCHER PUBLIC-ID INTO BCWORK                        *         
*                                                                     *         
* NTRY - R1=A(PASSWORD NUMBER)                                        *         
* EXIT - BCWORK=PUBLIC-ID                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING GPWORKD,RC                                                       
GETPID   MVC   GPIOKEY,IOKEY                                                    
         MVI   BCWORK,C'*'                                                      
         MVC   BCWORK+1(L'SAPALPID-1),BCWORK                                    
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           BUILD KEY OF RECORD                          
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUASEC                                                   
         MVC   SA0KNUM,0(R1)                                                    
         GOTO1 AIO,IOREAD+IOCTFILE+IO3                                          
         BNE   GETPIDX                                                          
         L     R2,AIO3                                                          
         LA    R2,SA0DATA                                                       
         USING SAPALD,R2                                                        
         SR    R0,R0                                                            
GETPID2  CLI   SAPALEL,0           TEST EOR                                     
         BE    GETPIDX                                                          
         CLI   SAPALEL,SAPALELQ    TEST PERSON PERSONAL-ID ELEMENT              
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R2,R0                                                            
         B     GETPID2                                                          
         MVC   BCWORK(L'SAPALPID),SAPALPID                                      
GETPIDX  MVC   IOKEY,GPIOKEY                                                    
         B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 1                                                                
GPWORKD  DSECT                     ** GETPID S/R LOCAL W/S **                   
GPIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A PERSONAL-ID                                   *         
*                                                                     *         
* NTRY - R1=A(PERSON-ID)                                              *         
* EXIT - BCWORK=PASSWORD NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING VPWORKD,RC                                                       
VALPID   MVC   VPIOKEY,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          BUILD KEY OF RECORD                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,CUASEC                                                   
         MVC   SAPEPID,0(R1)                                                    
         GOTO1 AIO,IOHI+IOCTFILE+IO3                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,VPIOKEY                                                    
         L     R2,AIO3                                                          
         CLC   SAPEREC(SAPEDEF-SAPEREC),IOKEYSAV                                
         BNE   VALPIDER                                                         
*                                                                               
         LA    R2,SAPEDATA                                                      
         USING SAPWDD,R2                                                        
         SR    R0,R0                                                            
VALPID2  CLI   SAPWDEL,0           TEST EOR                                     
         BE    VALPIDER                                                         
         CLI   SAPWDEL,SAPWDELQ    TEST PASSWORD ELEMENT                        
         BE    *+14                                                             
         IC    R0,SAPWDLN                                                       
         AR    R2,R0                                                            
         B     VALPID2                                                          
         MVC   BCWORK(L'SAPWDNUM),SAPWDNUM                                      
         B     ROUTE                                                            
*                                                                               
VALPIDER MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         B     ROUTH                                                            
         DROP  R2                                                               
         SPACE 1                                                                
VPWORKD  DSECT                     ** VALPID S/R LOCAL W/S **                   
VPIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR INPUT BETWEEN TWO POINTS IN TWA                 *         
*                                                                     *         
* NTRY - P1=A(START POINT)                                            *         
*        P2=A(END POINT OR ZERO)                                      *         
* EXIT - CC=EQUAL IF INPUT, NOT EQUAL IF NO INPUT                     *         
***********************************************************************         
         SPACE 1                                                                
TSTINP   ICM   RF,15,4(R1)                                                      
         L     R1,0(R1)                                                         
         BNZ   *+8                                                              
         LA    RF,4095(R1)                                                      
         SR    RE,RE                                                            
TSTINP02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    ROUTH                                                            
         TM    FVATRB-FVIHDR(R1),FVAMODF                                        
         BNZ   ROUTE                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVITHIS                                        
         BNZ   ROUTE                                                            
         BXLE  R1,RE,TSTINP02                                                   
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* TEST BATCH MONTH IS OPEN FOR INPUT                                  *         
*                                                                     *         
* NTRY - R1=A(INPUT FIELD HEADER OF MOS FIELD)                        *         
*        R1=0 TO VALIDATE LSTBMOSP FOR BATCH/CLOSE ETC.               *         
*        R1=1 IF CALLED FROM BATCH/UPDATE                             *         
***********************************************************************         
         SPACE 1                                                                
TSTBMO   LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(BATCH MONTH TABLE ENTRY)                
         CHI   R1,1                                                             
         BH    TSTBMO02                                                         
         MVI   BCBYTE1,1           CONVERT LSTBMOSP TO PRINTABLE                
         BNE   *+8                                                              
         MVI   BCBYTE1,FF                                                       
         MVC   BCFULL(L'LSTBMOSP),LSTBMOSP                                      
         MVI   BCFULL+L'LSTBMOSP,1                                              
         MVC   BCDUB,BCSPACES                                                   
         GOTO1 VDATCON,BCPARM,(1,BCFULL),(9,BCDUB)                              
         LA    RF,BCDUB                                                         
         LA    R0,L'BCDUB                                                       
         B     TSTBMO04                                                         
*                                                                               
TSTBMO02 MVI   BCBYTE1,0           VALIDATE USER INPUT                          
         GOTO1 AFVAL                                                            
         BNE   ROUTH                                                            
         LA    RF,FVIHDR                                                        
         SR    R0,R0                                                            
*                                                                               
TSTBMO04 MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
*                                                                               
         LA    R2,BCWORK                                                        
         USING BMONVALD,R2                                                      
         GOTO1 VBMONVAL,BCPARM,((R0),(RF)),(CSBTYP,ACOM),              X        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    TSTBMO06                                                         
         MVC   FVMSGNO,BMOMSG                                                   
         TM    BMOERR,BMOELOKQ+BMOERNGQ  TEST LOCKED/OUT OF RANGE               
         BZ    ROUTH                                                            
         CLI   BCBYTE1,FF          ERROR IF BATCH/UPDATE                        
         BE    ROUTH                                                            
*                                                                               
         CLC   LSTBEFDT,BCTODAYC   TEST EFFECTIVE TODAY                         
         BE    *+12                                                             
         MVI   BMOERR,BMOEOKQ      NO - RESET ERROR AND SKIP LOCK TESTS         
         B     TSTBMO06                                                         
         CLI   BMOERR,BMOELOKQ     TEST LOCKED MONTH IS THE ONLY ERROR          
         BNE   TSTBMO06                                                         
         TM    CSBIND1,TYPIXLOK    TEST BATCH TYPE IGNORES MOS LOCK             
         BZ    TSTBMO06                                                         
         MVI   BMOERR,BMOEOKQ      SET MONTH IS ALLOWED                         
*                                                                               
TSTBMO06 CLI   BCBYTE1,0           DON'T DISPLAY IF LSTBMOSP VALIDATION         
         BNE   TSTBMO08                                                         
         MVC   BCDUB,BMOMOSP                                                    
         MVI   BCDUB+L'BMOMOSP,X'01'                                            
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         GOTO1 VDATCON,BCPARM,(1,BCDUB),(9,(RF))                                
*                                                                               
TSTBMO08 CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BNE   ROUTH                                                            
         MVC   LSTBMOSP,BMOMOSP                                                 
         MVC   LSTBMOSC,BMOMOSC                                                 
*                                                                               
         MVC   CSBSECL,BOBYTE1     BMONVAL RETURNS BATCH SECURITY               
*                                                                               
TSTBMOX  B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BATCH USER-ID CODE INTO BCWORK                       *         
*                                                                     *         
* NTRY - R1=A(USER-ID NUMBER)                                         *         
* EXIT - BCWORK=USER-ID CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING GUWORKD,RC                                                       
GETUID   MVC   GUIOKEY,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R1)                                                    
         L     R2,AIO3                                                          
         CLC   IOKEY(L'CTIKEY),0(R2)                                            
         BE    GETUID02                                                         
         GOTO1 AIO,IOREAD+IOCTFILE+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
GETUID02 LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
         USING CTDSCD,R1                                                        
GETUID04 CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    RE,CTDSCLEN                                                      
         AR    R1,RE                                                            
         B     GETUID04                                                         
         MVC   BCWORK(L'CTDSC),CTDSC                                            
GETUIDX  MVC   IOKEY,GUIOKEY                                                    
         B     ROUTE                                                            
         DROP  R1,R2                                                            
         SPACE 1                                                                
GUWORKD  DSECT                     ** GETUID S/R LOCAL W/S **                   
GUIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST VALIDITY OF BATCH TYPE AND ACTION COMBINATION       *         
*                                                                     *         
* NTRY - R1=A(ACTION NUMBER), CSBTYP IS BATCH INPUT TYPE              *         
* EXIT - CC=EQUAL IF TYPE/ACTION VALID, ELSE NOT EQUAL                *         
***********************************************************************         
         SPACE 1                                                                
TSTBTY   LR    R0,R1                                                            
         SR    RF,RF                                                            
         IC    RF,CSBTYP                                                        
         LA    RF,64(RF)           ADD ON BASE NUMBER FOR RECORD TYPE           
         GOTO1 VSECRET,BCPARM,('SECPRACT',ASECBLK),((RF),(R0))                  
         BNE   TSTBTY02                                                         
*                                                                               
         LR    R1,R0                                                            
         CLI   0(R1),ACTOPN                                                     
         BE    *+8                                                              
         CLI   0(R1),ACTCOP                                                     
         BE    *+8                                                              
         CLI   0(R1),ACTREV                                                     
         BE    *+8                                                              
         CLI   0(R1),ACTGEN                                                     
         BNE   ROUTE                                                            
         TM    CSBIND8,TYPITIME    TEST TIME INPUT TYPE                         
         BZ    ROUTE                                                            
         TM    BCCPYST7,CPYSTMSY   TEST TMS=Y ON COMPANY RECORD                 
         BZ    ROUTE                                                            
*                                                                               
TSTBTY02 MVI   FVOMTYP,GTMERR                                                   
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR AND TRANSMIT AN INPUT FIELD ADDRESSED BY FVADDR               *         
***********************************************************************         
         SPACE 1                                                                
CLRFLD   L     RF,FVADDR                                                        
CLRFLD2  OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         SR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(RF)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(RF),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,RF),L'FVIHDR(RF)                                      
CLRFLDX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DESTINATION FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(DESTINATION TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE BCWORK(2) CONTAINS ID NUMBER      *         
***********************************************************************         
         SPACE 1                                                                
         USING VDWORKD,RC                                                       
VALDST   ST    R1,VDAFLD                                                        
         MVC   BCWORK(L'CUUSER),CUUSER                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R1,IOKEY                                                         
         USING CTIKEY,R1           BUILD ID RECORD KEY                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         LA    R0,VDREC                                                         
         ST    R0,IOADDR                                                        
         GOTO1 AIO,IORD+IOCTFILE                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T READ USER ID RECORD                    
*                                                                               
         L     R1,VDAFLD                                                        
         GOTO1 AFVAL               VALIDATE DESTINATION FIELD                   
         BNE   VALDSTX                                                          
*                                                                               
         GOTO1 VGETIDS,VDPARM,(C'D',IOADDR),0,VDMGR                             
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          RE=NUMBER OF DESTINATION ID'S                
         BZ    VALDST04                                                         
         L     RF,4(R1)            RF=A(LIST OF VALID DESTINATIONS)             
VALDST02 CLC   FVIFLD(10),0(RF)    MATCH INPUT TO DESTINATION LIST              
         BE    VALDST06                                                         
         LA    RF,12(RF)                                                        
         BCT   RE,VALDST02                                                      
VALDST04 MVC   FVMSGNO,=AL2(FVFEDST)                                            
         B     ROUTH                                                            
*                                                                               
VALDST06 MVC   BCWORK(L'CUUSER),10(RF)                                          
*                                                                               
VALDSTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     ROUTE                                                            
         DROP  R1,RC                                                            
         SPACE 1                                                                
VDWORKD  DSECT                     ** VALDEST S/R LOCAL W/S **                  
VDAFLD   DS    A                   SAVED A(DESTINATION FIELD HEADER)            
VDPARM   DS    6F                  GETIDS PARAMETER LIST                        
VDREC    DS    XL1000              OUTPUT TYPE RECORD I/O AREA                  
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST SECURITY FOR A FIELD                                           *         
*                                                                     *         
* NTRY - R1=A(FIELD SECURITY NUMBER)                                  *         
* EXIT - CC=EQUAL IF FIELD VALID FOR READ AND WRITE                   *         
*        CC=HIGH IF FIELD VALID FOR READ ONLY                         *         
*        CC=LOW IF FIELD INVALID FOR READ AND WRITE                   *         
***********************************************************************         
         SPACE 1                                                                
FLDSEC   CLI   0(R1),0                                                          
         BE    ROUTX                                                            
         LR    RF,R1                                                            
         GOTO1 VSECRET,BCPARM,('SECPFLDP',ASECBLK),(RF)                         
         B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A WRITE-OFF EXPENSE ACCOUNT                     *         
*                                                                     *         
* NTRY - P1    B0   = X'80' MEANS FIELD IS REQUIRED                   *         
*                     X'40' TIME EXPENSE ACCOUNT                      *         
*                     X'20' OTHER EXPENSE ACCOUNT (DEFAULT)           *         
*              B1-3 = A(INPUT FIELD HEADER) OR ZERO IF FVAL HAS BEEN  *         
*                     CALLED BY CALLING ROUTINE                       *         
* EXIT - CC=LOW IF ACCOUNT IS NOT PRESENT                             *         
*        CC=EQUAL IF ACCOUNT OK                                       *         
*        CC=HIGH IF FIELD/ACCOUNT IS IN ERROR                         *         
***********************************************************************         
         SPACE 1                                                                
         USING VEWORKD,RC                                                       
VALWEX   MVC   VEPARM(VEPARML),0(R1)                                            
         L     RE,AIO1             SAVE CALLERS IO1                             
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         LA    R0,VESAVIO1                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&UK                                                                           
         LA    RF,X'40'                                                         
         TM    VEPINDS,VEPIREQD    TEST ACCOUNT REQUIRED                        
         BZ    *+8                                                              
         LA    RF,X'80'(RF)                                                     
         SR    R0,R0                                                            
         ICM   R0,7,VEPAFLD        TEST DUTCH TAX TYPE / CALL FVAL              
         GOTO1 ATSTTAX,VEOPARM,((RF),(R0))                                      
         BH    VALWEXH                                                          
*&&                                                                             
*&&US                                                                           
         TM    VEPINDS,VEPIREQD    TEST ACCOUNT REQUIRED                        
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         SR    R1,R1                                                            
         ICM   R1,7,VEPAFLD                                                     
         BZ    VALWEX02                                                         
         GOTO1 AFVAL                                                            
         BNE   ROUTX                                                            
*&&                                                                             
*                                                                               
VALWEX02 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'S'        DEFAULT IS EXPENSE LEDGER                    
         MVI   ACTKLDG,C'E'                                                     
*&&US                                                                           
         TM    VEPINDS,VEPITIME    IF TIME DEFAULT IS SI                        
         BZ    *+8                                                              
         MVI   ACTKLDG,C'I'                                                     
*&&                                                                             
         CLI   FVIFLD,C'*'         TEST OVERRIDE UNIT/LEDGER                    
         BE    VALWEX04                                                         
         CLI   FVILEN,L'ACTKACT    TEST INPUT LONGER THAN ACCOUNT CODE          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALWEXH                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         B     VALWEX06                                                         
*                                                                               
VALWEX04 CLI   FVILEN,ACTKACT-ACTRECD                                           
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALWEXH                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FVIFLD+1                                              
*                                                                               
VALWEX06 LA    R1,VEULTIME                                                      
         TM    VEPINDS,VEPITIME                                                 
         BNZ   *+8                                                              
         LA    R1,VEULOTHR                                                      
         GOTO1 AGETACT,(R1)                                                     
         BNE   VALWEXH                                                          
         TM    ACBSTAT,ACBSABAL    TEST BALANCE ELEMENT PRESENT                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     VALWEXH                                                          
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     VALWEXH                                                          
         TM    BCCPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    VALWEXE                                                          
         CLC   ACOFFC,BCSPACES     TEST ACCOUNT HAS AN OFFICE                   
         BNH   VALWEXE                                                          
*&&US*&& TM    CSBSECL,CPYBSOFF    TEST OVERRIDE OFFICE SECURITY                
*&&US*&& BZ    VALWEXE                                                          
         OC    CSOFFICE,CSOFFICE   TEST GLOBAL OFFICE SET                       
         BNZ   *+14                                                             
         MVC   CSOFFICE,ACOFFC     NO - SET GLOBAL OFFICE                       
         B     VALWEXE                                                          
         CLC   CSOFFICE,ACOFFC     TEST ACCOUNT/GLOBAL OFFICE MATCH             
         BE    VALWEXE                                                          
         MVC   FVMSGNO,=AL2(AE$MIXOF)                                           
         B     VALWEXH                                                          
*                                                                               
VALWEXL  MVI   BCDUB,0             SET CC LOW                                   
         B     VALWEXX                                                          
VALWEXH  MVI   BCDUB,2             SET CC HIGH                                  
         B     VALWEXX                                                          
VALWEXE  MVI   BCDUB,1             SET CC EQUAL                                 
*                                                                               
VALWEXX  LA    RE,VESAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ROUTCC                                                           
         SPACE 1                                                                
*&&UK                                                                           
VEULTIME DC    C'SE',C'GB',C'GP',C'SK',C'SB',C'SL',C'SI',C'SQ',AL1(EOT)         
VEULOTHR DC    C'SE',C'GB',C'GP',C'SK',C'SB',C'SL',C'SI',C'SQ',AL1(EOT)         
*&&                                                                             
*&&US                                                                           
VEULTIME DC    C'SE',C'SB',C'SI',AL1(EOT)                                       
VEULOTHR DC    C'SE',C'SB',C'SI',AL1(EOT)                                       
*&&                                                                             
         SPACE 1                                                                
VEWORKD  DSECT                     ** VALWEX LOCAL W/S **                       
VEPARM   DS    0A                                                               
VEPINDS  DS    XL1                                                              
VEPIREQD EQU   X'80'               FIELD IS REQUIRED                            
VEPITIME EQU   X'40'               TIME EXPENSE ACCOUNT                         
VEPIOTHR EQU   X'20'               OTHER EXPENSE ACCOUNT                        
VEPAFLD  DS    AL3                 A(INPUT FIELD HEADER)                        
VEPARML  EQU   *-VEPARM                                                         
VEOPARM  DS    6A                                                               
VESAVIO1 DS    XL2048              SAVED AREA FOR IO1                           
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A WRITE-OFF ANALYSIS ACCOUNT                    *         
*                                                                     *         
* NTR1 - P1    B0   = X'80' MEANS FIELD IS REQUIRED                   *         
*                     X'40' TIME WRITE-OFF ACCOUNT                    *         
*                     X'20' OTHER WRITE-OFF ACCOUNT (DEFAULT)         *         
*              B1-3 = A(INPUT FIELD HEADER) OR ZERO IF FVAL HAS BEEN  *         
*                     CALLED BY CALLING ROUTINE                       *         
* EXIT - CC=LOW IF ACCOUNT IS NOT PRESENT                             *         
*        CC=EQUAL IF ACCOUNT OK                                       *         
*        CC=HIGH IF FIELD/ACCOUNT IS IN ERROR                         *         
***********************************************************************         
         SPACE 1                                                                
         USING VAWORKD,RC                                                       
VALWAN   MVC   VAPARM(VAPARML),0(R1)                                            
         L     RE,AIO1             SAVE CALLERS IO1                             
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         LA    R0,VASAVIO1                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    VAPINDS,VAPIREQD    TEST ACCOUNT REQUIRED                        
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         SR    R1,R1                                                            
         ICM   R1,7,VAPAFLD                                                     
         BZ    VALWAN02                                                         
         GOTO1 AFVAL               VALIDATE INPUT FIELD                         
         BNE   VALWANH                                                          
*                                                                               
VALWAN02 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'S'        DEFAULT IS EXPENSE LEDGER                    
         MVI   ACTKLDG,C'E'                                                     
         CLI   FVIFLD,C'*'         TEST OVERRIDE UNIT/LEDGER                    
         BE    VALWAN04                                                         
         CLI   FVILEN,L'ACTKACT    TEST INPUT LONGER THAN ACCOUNT CODE          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALWANH                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         B     VALWAN06                                                         
*                                                                               
VALWAN04 CLI   FVILEN,ACTKACT-ACTRECD                                           
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALWANH                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FVIFLD+1                                              
*                                                                               
VALWAN06 LA    R1,VAULTIME                                                      
         TM    VAPINDS,VAPITIME                                                 
         BNZ   *+8                                                              
         LA    R1,VAULOTHR                                                      
         GOTO1 AGETACT,(R1)                                                     
         BNE   VALWANH                                                          
         TM    ACBSTAT,ACBSABAL    TEST BALANCE ELEMENT PRESENT                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     VALWANH                                                          
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     VALWANH                                                          
         TM    BCCPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    VALWANE                                                          
         CLC   ACOFFC,BCSPACES     TEST ACCOUNT HAS AN OFFICE                   
         BNH   VALWANE                                                          
*&&US*&& TM    CSBSECL,CPYBSOFF    TEST OVERRIDE OFFICE SECURITY                
*&&US*&& BZ    VALWANE                                                          
         OC    CSOFFICE,CSOFFICE   TEST GLOBAL OFFICE SET                       
         BNZ   *+14                                                             
         MVC   CSOFFICE,ACOFFC     NO - SET GLOBAL OFFICE                       
         B     VALWANE                                                          
         CLC   CSOFFICE,ACOFFC     TEST ACCOUNT/GLOBAL OFFICE MATCH             
         BE    VALWANE                                                          
         MVC   FVMSGNO,=AL2(AE$MIXOF)                                           
         B     VALWANH                                                          
*                                                                               
VALWANL  MVI   BCDUB,0             SET CC LOW                                   
         B     VALWANX                                                          
VALWANH  MVI   BCDUB,2             SET CC HIGH                                  
         B     VALWANX                                                          
VALWANE  MVI   BCDUB,1             SET CC EQUAL                                 
*                                                                               
VALWANX  LA    RE,VASAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ROUTCC                                                           
         SPACE 1                                                                
VAULTIME DC    C'SE',AL1(EOT)                                                   
VAULOTHR DC    C'SE',AL1(EOT)                                                   
         SPACE 1                                                                
VAWORKD  DSECT                     ** VALWAN LOCAL W/S **                       
VAPARM   DS    0A                                                               
VAPINDS  DS    XL1                                                              
VAPIREQD EQU   X'80'               FIELD IS REQUIRED                            
VAPITIME EQU   X'40'               TIME WRITE-OFF ACCOUNT                       
VAPIOTHR EQU   X'20'               OTHER WRITE-OFF ACCOUNT                      
VAPAFLD  DS    AL3                 A(INPUT FIELD HEADER)                        
VAPARML  EQU   *-VAPARM                                                         
VASAVIO1 DS    XL2048              SAVED AREA FOR IO1                           
BAT6F    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TO TEST TRANSACTION ORDER FOR PTAELS, NOT BNDELS                    *         
* NTRY: P1 BYTE 0 = C'T'                                              *         
*             1-3 = A(1ST ELEMENT OF TRANSACTION ORDER RECORD)        *         
* EXIT:        CC = EQUAL IF PTAELS USED                              *         
*       P1 BYTE 0 = C'P' IF PENDING ACTIVITY FOUND                    *         
*                 = C'U' IF UPDATED ACTIVITY FOUND ONLY               *         
*                                                                     *         
* TO PROCESS TRANSACTION ORDER RECORD FOR AMOUNT INVOICED TO DATE     *         
* NTRY: P1 BYTE 0 = C'I'                                              *         
*             1-3 = A(1ST ELEMENT OF TRANSACTION ORDER RECORD)        *         
*       P2        = A(OUTPUT AREA FOR PROCESSED PTAELS)               *         
* EXIT: P1        = A(END OF OUTPUT AREA)                             *         
*                                                                     *         
* TO MATCH INVOICE RECORD FOR PARTLY MATCHED AMOUNT                   *         
* NTRY: P1 BYTE 0 = C'M'                                              *         
*             1-3 = A(1ST ELEMENT OF INVOICE PTAELS)                  *         
*       P2        = A(PL8 AMOUNT TO BE MATCHED)                       *         
* EXIT: P1        = A(NEW END OF PTAELS LIST)                         *         
***********************************************************************         
         SPACE 1                                                                
         USING MOWORKD,RC                                                       
MCHORD   MVC   MOPARMS,0(R1)                                                    
         ST    R1,MOAR1                                                         
         CLI   MOACT,MOATST        TEST SUB-ACTION                              
         BE    MCHTST                                                           
         CLI   MOACT,MOAINV                                                     
         BE    MCHINV                                                           
         CLI   MOACT,MOAMCH                                                     
         BE    MCHMCH                                                           
         DC    H'0'                                                             
*                                                                               
MCHTST   L     R3,MOPA1            * TEST PTAELS USED ON RECORD *               
         LA    R3,0(R3)                                                         
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
MTST02   CLI   PTAEL,0                                                          
         BE    MTST10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   MTST08                                                           
         CP    PTANET,BCPZERO      TEST FOR ACTIVITY                            
         BNE   MTST04                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   MTST08                                                           
         CP    PTARCOM,BCPZERO                                                  
         BE    MTST08                                                           
MTST04   TM    PTASTAT1,PTASPEND   TEST PENDING/UPDATED                         
         BZ    *+12                                                             
         OI    MOINDS,MOIPEN                                                    
         B     MTST08                                                           
         OI    MOINDS,MOIUPD                                                    
MTST08   IC    RF,PTALN                                                         
         BXH   R3,RF,MTST02                                                     
         DC    H'0'                                                             
         DROP  R3                                                               
MTST10   L     R1,MOAR1                                                         
         MVI   0(R1),0                                                          
         TM    MOINDS,MOIPEN+MOIUPD                                             
         BZ    ROUTL               CC=NOT EQUAL FOR PTAELS NOT USED             
         MVI   0(R1),C'P'          TEST PENDING ACTIVITY FOUND                  
         TM    MOINDS,MOIPEN                                                    
         BO    ROUTE                                                            
         MVI   0(R1),C'U'          UPDATED ACTIVITY ONLY FOUND                  
         B     ROUTE                                                            
         SPACE 1                                                                
MCHINV   L     R1,MOPA1            * PROCESS FOR IVOICE TOTAL TO DATE *         
         LA    R1,0(R1)                                                         
         USING OAMELD,R1           FIND INVOICED TO DATE TOTAL                  
         XR    RF,RF                                                            
         CLI   OAMEL,OAMELQ                                                     
         BE    *+12                                                             
         IC    RF,OAMLN                                                         
         BXH   R1,RF,*-12                                                       
         ZAP   MOINVTOT,OAMIVAL                                                 
         BNZ   *+8                                                              
         OI    MOINDS,MOIINVTD                                                  
         ZAP   MOINV,BCPZERO                                                    
         DROP  R1                                                               
*                                                                               
         L     R3,MOPA1                                                         
         LA    R3,0(R3)                                                         
         USING PTAELD,R3                                                        
         L     R4,MOPA2                                                         
         XR    R0,R0                                                            
*                                                                               
MINV02   CLI   PTAEL,0             TEST EOR                                     
         BE    MCHINVX                                                          
         IC    R0,PTALN                                                         
         CLI   PTAEL,PTAELQ        FIND NON-PENDING ALLOCATION PTAELS           
         BNE   MINV08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   MINV08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    MINV08                                                           
*                                                                               
         ZAP   MONET,PTANET                                                     
         TM    MOINDS,MOIINVTD     TEST ALREADY PAST TOTAL                      
         BO    MINV04                                                           
         AP    MOINV,MONET         NO - UPDATE RUNNING TOTAL                    
         CP    MOINV,MOINVTOT      TEST PAST INVOICED TO DATE TOTAL             
         BNH   MINV08                                                           
         ZAP   MONET,MOINV         YES - SET NET FOR REMAINDER OF PTAEL         
         SP    MONET,MOINVTOT                                                   
         OI    MOINDS,MOIINVTD                                                  
*                                                                               
MINV04   GOTO1 MCHPTA,MODMCB,PTAELD,(R4)                                        
         AR    R4,R0               BUMP R4 TO NEXT ELEMENT                      
*                                                                               
MINV08   AR    R3,R0               BUMP R3 TO NEXT ELEMENT                      
         B     MINV02                                                           
*                                                                               
MCHINVX  MVI   0(R4),0             SAVE A(END OF ELEMENT BLOCK)                 
         L     R1,MOAR1                                                         
         ST    R4,0(R1)                                                         
         B     ROUTE                                                            
         SPACE 1                                                                
MCHMCH   L     R1,MOPA2                                                         
         ZAP   MOMATTOT,0(8,R1)                                                 
         ZAP   MOMAT,BCPZERO                                                    
         L     R3,MOPA1                                                         
         LA    R3,0(R3)                                                         
         USING PTAELD,R3                                                        
         XR    R0,R0                                                            
MMCH02   CLI   PTAEL,0             TEST EOR                                     
         BE    MMCH10                                                           
         IC    R0,PTALN                                                         
         CLI   PTAEL,PTAELQ        FIND NON-PENDING ALLOCATION PTAELS           
         BNE   MMCH08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   MMCH08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    MMCH08                                                           
*                                                                               
         AP    MOMAT,PTANET        TEST MATCHED TOTAL WILL GO OVER MAX          
         CP    MOMAT,MOMATTOT                                                   
         BNH   MMCH04                                                           
         ZAP   MONET,PTANET        YES - REDUCE AMOUNT FOR THIS PTAEL           
         SP    MONET,MOMAT                                                      
         AP    MONET,MOMATTOT                                                   
         ZAP   MOMAT,MOMATTOT                                                   
         MVC   MOPTA,PTAELD                                                     
         GOTO1 MCHPTA,MODMCB,MOPTA,PTAELD                                       
*                                                                               
MMCH04   CP    MOMAT,MOMATTOT      TEST MATCHED FOR TOTAL                       
         BNE   MMCH08                                                           
         AR    R3,R0                                                            
         B     MMCH10                                                           
*                                                                               
MMCH08   AR    R3,R0                                                            
         B     MMCH02                                                           
*                                                                               
MMCH10   MVI   0(R3),0             SET E-O-R                                    
         L     R1,MOAR1                                                         
         ST    R3,0(R1)                                                         
         B     ROUTE                                                            
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO COPY PTA ELEMENT                                         *         
*                                                                     *         
* NTRY: MONET = NET AMOUNT FOR NEW PTAEL                              *         
*          P1 = A(OLD PTA ELEMENT)                                    *         
*          P2 = A(NEW PTA ELEMENT)                                    *         
***********************************************************************         
         SPACE 1                                                                
MCHPTA   NTR1  ,                   * MATCH MONET INTO INV.PTAELD *              
         LM    R3,R4,0(R1)                                                      
OLD      USING PTAELD,R3                                                        
NEW      USING PTAELD,R4                                                        
         IC    RF,OLD.PTALN        COPY ELEMENT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEW.PTAELD(0),OLD.PTAELD                                         
*                                                                               
         CP    NEW.PTANET,MONET    TEST AMOUNTS THE SAME                        
         BE    MCHPTAX                                                          
         ZAP   NEW.PTANET,MONET    NO - SET COMMISSION                          
         GOTO1 MCHAMT,MODMCB,OLD.PTARCOM,NEW.PTARCOM                            
*                                                                               
         CP    OLD.PTANETF,BCPZERO TEST FOREIGN CURRENCY                        
         BE    MCHPTAX                                                          
         GOTO1 (RF),(R1),OLD.PTANETF,NEW.PTANETF                                
         GOTO1 (RF),(R1),OLD.PTARFCOM,NEW.PTARFCOM                              
*                                                                               
MCHPTAX  B     ROUTX                                                            
         SPACE 1                                                                
MCHAMT   LM    R1,R2,0(R1)         * ROUTINE TO PORTION AMOUNT *                
         ZAP   MOPL61(12),0(6,R1)  MOPL61(12) = OLD AMOUNT                      
         MP    MOPL61(12),NEW.PTANET           *NEW.PTANET                      
         SRP   MOPL61(12),2,0                  *100                             
         DP    MOPL61(12),OLD.PTANET           /OLD.PTANET                      
         SRP   MOPL61,64-2,5                   /100                             
         ZAP   0(6,R2),MOPL61      SET NEW AMOUNT                               
         BR    RE                                                               
         SPACE 1                                                                
         DROP  OLD,NEW                                                          
         DROP  RC                                                               
         SPACE 1                                                                
MOWORKD  DSECT                     ** MCHORD S/R LOCAL W/S **                   
MOPARMS  DS    0XL8                                                             
MOACT    DS    0CL1                                                             
MOATST   EQU   C'T'                TEST FOR PTAELS                              
MOAINV   EQU   C'I'                COPY PTAELS FOR INVOICE                      
MOAMCH   EQU   C'M'                PROCESS PTAELS FOR PART MATCH                
MOPA1    DS    A                   ADDRESS 1                                    
MOPA2    DS    A                   ADDRESS 2                                    
         ORG   MOPARMS+L'MOPARMS                                                
MOAR1    DS    A                   A(CALLERS R1)                                
MODMCB   DS    6A                                                               
MOINVTOT DS    PL8                 INVOICE TOTAL TO DATE                        
MOINV    DS    PL8                 RUNNING INVOICE TOTAL TO DATE                
MOMATTOT DS    PL8                 TOTAL TO BE MATCHED                          
MOMAT    DS    PL8                 RUNNING TOTAL MATCHED SO FAR                 
MONET    DS    PL8                 NET ON PTA ELEMENT TO BE MATCHED             
MOPL61   DS    PL6                                                              
MOPL62   DS    PL6                                                              
MOINDS   DS    XL1                 INDICATOR BYTE                               
MOIINVTD EQU   X'80'               INVOICED TO DATE TOTAL REACHED               
MOIPEN   EQU   X'40'               PENDING ACTIVITY FOUND                       
MOIUPD   EQU   X'20'               UPDATED ACTIVITY FOUND                       
MOPTA    DS    XL100                                                            
MOWORKL  EQU   *-MOWORKD                                                        
*                                                                               
BAT6F    CSECT                                                                  
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGOBBLOCK                                                                    
         PRINT OFF                                                              
GOXXBLKD DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCUREDITD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCUREDITD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078ACGEN6F   01/07/15'                                      
         END                                                                    
