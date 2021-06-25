*          DATA SET ACREQ02    AT LEVEL 043 AS OF 01/15/21                      
*PHASE T60402B                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE ACWKSCAN                                                               
         TITLE 'ACREQ02 - REQUEST - DISPLAY/UPDATE REQUEST FILE'                
*--------------------------------------------------------------------*          
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* JSHA 041 23JAN20 SPEC-41535 Check Delivery Flag                               
* GHOA 042 29OCT20 SPEC-51025 1099 NEW REQUIREMENTS FOR 2020                    
* GHOA 043 14JAN21 SPEC-53117 STACK RUN,  CLEAR CHECK DELIVERY FLAG             
*--------------------------------------------------------------------*          
T60402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60402,RA,R7,R2,RR=R5                                   
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         L     R9,0(R1)                                                         
         USING GWS,R9              R9=A(GLOBAL WORKING STORAGE)                 
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         ST    R5,RELO                                                          
         MVI   DMIN,X'20'          HANDLING 80-BYTE HDR REQUESTS                
         L     RF,=V(ACWKSCAN)                                                  
         A     RF,RELO                                                          
         ST    RF,VCWKSCAN                                                      
*                                                                               
         ICM   R0,14,=X'D9000A'                                                 
         LA    R5,PHASES                                                        
         LA    R4,VGETOPT                                                       
*                                                                               
INIT03   ICM   R0,1,0(R5)          LOAD JOBBER, ETC                             
         GOTO1 CALLOV,DMCB,0,(R0),0                                             
         MVC   0(4,R4),0(R1)                                                    
         LA    R5,1(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   INIT03                                                           
         EJECT                                                                  
         MVC   FERN,=AL2(FF)                                                    
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         OC    OUTSTAT,OUTSTAT     STACKED/SOON REQS                            
         BZ    REQIO10                                                          
         CLI   REQACTN,C'N'        NEW REQUEST                                  
         BE    CHKIT                                                            
         B     EXIT                                                             
*                                                                               
REQIO10  CLI   PFKEY,8                                                          
         BNE   *+10                                                             
         MVC   REQINCR,LASTREAD                                                 
         CLI   STATUS,3                                                         
         BNE   *+16                                                             
         CLI   REQOPTN,C'T'                                                     
         BNE   ENQREQ                                                           
         B     TOTREQ                                                           
         CLI   REQACTN,C'A'                                                     
         BE    CHKIT                                                            
         CLI   REQACTN,C'D'                                                     
         BE    CANREQ                                                           
         CLI   REQACTN,C'N'                                                     
         BE    CHKIT                                                            
         DC    H'0'                                                             
*                                                                               
REQIOERR MVC   FERN,=AL2(0)                                                     
CLEARADR XC    LADR,LADR                                                        
         B     EXIT                                                             
SAVEADR  MVC   LADR,ADR                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ANY GLOBAL REQUEST POST VALIDATION GOES HERE                       *          
**********************************************************************          
*                                                                               
         USING PRGTABD,R4                                                       
CHKIT    DS    0H                                                               
         L     R4,APRGTAB          PROGRAM/ACTION TABLE                         
CHKIT10  CLI   0(R4),X'FF'         IF NOT FOUND-ALLOW ACCESS                    
         BE    CHKIT20                                                          
         CLC   PRGID,ACQPROG       MATCH ON PROGRAM                             
         BE    *+12                                                             
         LA    R4,PRGTBLNQ(R4)                                                  
         B     CHKIT10                                                          
*                                                                               
         TM    PRGRSTAT,PRGLMACC   ARE WE LIMITING ACCESS?                      
         BNO   CHKIT20                                                          
*                                                                               
         LR    RE,R8               RE=A(REQUEST CARD)                           
         SR    RF,RF                                                            
         IC    RF,PRGLMFLD                                                      
         AR    RE,RF               BUMP TO FIELD TO CHECK IF LIVE RUN           
         CLC   0(L'PRGLMVAL,RE),PRGLMVAL                                        
         BNE   CHKIT20             IF NOT EQUAL-ALLOW ACCESS                    
         GOTO1 ACHKACC                                                          
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                CHECK FOR AN ERROR                           
*                                                                               
CHKIT20  CLI   LREQMAP,126         CARD REQUEST                                 
         BE    CHKREQX             BYPASS VALIDATION                            
*                                                                               
CHKREQ   L     R4,AREQTBL          R4=A(REQTBL ENTRY)                           
         USING HDRD,R4                                                          
         AH    R4,REQNDX                                                        
         SR    RF,RF               RF=FURTHER VAL ROUTINE NUM                   
         IC    RF,HDRXROUT                                                      
         LTR   RF,RF                                                            
         BZ    CHKREQ40            NO VAL REQUIRED                              
         SLA   RF,2                                                             
*                                                                               
         L     RE,=A(VROUTS)                                                    
         A     RE,RELO                                                          
         L     RF,0(RF,RE)         RF=A(FURTHER VAL ROUTINE)                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLC   FERN,=AL2(FF)                                                    
         BE    CHKREQ40            ALL FIELDS OK                                
         DROP  R4                                                               
*                                                                               
CHKREQ10 LA    R0,MAXFLD           SEARCH REQ MAP TABLE                         
         LA    R1,LREQMAP                                                       
CHKREQ20 CLI   0(R1),LREQMAPX                                                   
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ30                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ20                                                      
*                                                                               
CHKREQ2  OC    FADR,FADR           EXIT IF A(FIELD) ALREADY SET                 
         BNZ   EXIT                                                             
         LA    R1,LREQMAP          POSN TO 1ST FLD                              
CHKREQ30 MVC   HALF,1(R1)                                                       
         LR    R6,R3                                                            
         AH    R6,HALF                                                          
         ST    R6,FADR             POSN CURSOR TO ROUTNUM FLD                   
         B     EXIT                                                             
*                                                                               
CHKREQ40 BAS   RE,VR53             SPECIAL LOGIC FOR SOON A27/A29               
         CLC   FERN,=AL2(FF)                                                    
         BNE   CHKREQ10            CHECK FOR AN ERROR                           
*                                                                               
         CLI   REQACTN,C'N'        IF NEW REQUEST                               
         BNE   CHKREQX                                                          
         MVC   FERN,=AL2(FF)       NO ERROR                                     
         CLC   ACQUNT(2),=C'SJ'    IF UNIT/LEDGER IS SJ                         
         BNE   CHKREQX                                                          
         LA    R6,RPTLST           AND REPORT IS IN TABLE                       
         LA    R0,NRPTS                                                         
         CLC   ACQPROG,0(R6)                                                    
         BE    *+16                FOUND IT                                     
         LA    R6,2(R6)                                                         
         BCT   R0,*-14                                                          
         B     CHKREQX                                                          
         CLC   ACQACT(ACQESTOR-ACQACT),SPACES INSURE 'FILTERABLE' INPUT         
         BNE   CHKREQX                                                          
         CLC   ACQMOSST(8),SPACES                                               
         BNE   CHKREQX                                                          
         CLC   ACQOFFFL,SPACES                                                  
         BNE   CHKREQX                                                          
         MVC   FERN,=AL2(MORESPEC) REQUEST MUST BE MORE SPECIFIC                
         B     CHKREQ2             CURSOR TO FIRST FIELD                        
*                                                                               
RPTLST   DC    C'1415212227283159616777'                                        
NRPTS    EQU   (*-RPTLST)/2                                                     
*                                                                               
CHKREQX  DS    0H                                                               
         MVC   FERN,=AL2(FF)                                                    
         CLI   REQACTN,C'N'                                                     
         BE    NEWREQ                                                           
         B     AMDREQ                                                           
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #01                            *         
***********************************************************************         
*                                                                               
VR01     NTR1                      * ENTERED FROM OTHER ROUTINES *              
         CLC   ACQPROG,=C'92'      IF RUNNING 92-CHK QOPT1                      
         BNE   VR01#30             ONLY ALLOWED HRS RPT IF SEC IS ON            
*                                                                               
         LA    R1,OPT1TAB          SALARY ONLY RPT TABLE                        
VR01#10  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    VR01#30             YES-NOT AN SAL ONLY RPT - CONTINUE           
         CLC   ACQOPT1,0(R1)       IF OPT 1 MATCH AN ENTRY IN TABLE             
         BE    VR01#20             CHECK SECURITY                               
         LA    R1,1(R1)                                                         
         B     VR01#10                                                          
*                                                                               
VR01#20  TM    CMPSTAT,CMPNSWT+CMPNSRD  IS EITHER READ/WRITE ON?                
         BNZ   VR01#30                  NO - SKIP IT                            
         MVI   ROUTNUM,11               CURSOR TO QOPT1                         
         MVC   FERN,=AL2(SECLOCK)       SECURITY LOCKOUT                        
         B     VR01X                                                            
*                                                                               
VR01#30  MVI   ROUTNUM,8           MAX DURATION = 12 MONTHS                     
         CLC   ACQSTART,SPACES                                                  
         BE    VR01X               START NOT INPUT                              
         CLC   ACQEND,SPACES                                                    
         BE    VR01X                                                            
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BO    VR01X               YES, BYPASS VALIDATION FOR NOW               
*                                                                               
         MVC   TEMP(L'ACQSTART),ACQSTART                                        
         MVC   TEMP+4(2),=C'01'                                                 
         GOTO1 ADDAY,PLIST,(C'Y',TEMP),TEMP+6,1                                 
         GOTO1 ADDAY,PLIST,(C'D',TEMP+6),TEMP+12,-1                             
*                                                                               
         CLC   TEMP+12(4),ACQEND                                                
         BNL   *+10                                                             
         MVC   FERN,=AL2(INVDATER)    DATE RANGE GREATER THAN 1 YEAR            
*                                                                               
VR01X    B     EXIT                                                             
*                                                                               
OPT1TAB  DS    0C                    RPTS THAT SHOW SALARY ONLY                 
         DC    C'7'                                                             
         DC    C'8'                                                             
         DC    C'9'                                                             
         DC    C'A'                                                             
         DC    C'D'                                                             
         DC    C'E'                                                             
         DC    C'H'                                                             
         DC    C'I'                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #02                            *         
***********************************************************************         
*                                                                               
VR02     NTR1                                                                   
         CLC   ACQPROG,=C'55'      IF REQUEST 55                                
         BNE   VR02#70                                                          
         TM    OUTSTAT,SOONQ       FOR SOON REQUESTS ACCT OR CLI REQ            
         BZ    VR02#05                                                          
         MVI   ROUTNUM,3                                                        
         CLC   ACQACT,SPACES                                                    
         BNE   VR02#05                                                          
         CLC   ACQSEL,SPACES                                                    
         BNE   VR02#05                                                          
         MVC   FERN,=AL2(ACCLIMIS)                                              
         B     VR02X                                                            
*                                                                               
VR02#05  CLI   ACQOPT8,X'40'       ANY CHECK DELIVERY ENTERED?                  
         BH    VR02#07                                                          
         TM    OUTSTAT,RUNQ                                                     
         BO    VR02#07                                                          
         MVI   ACQOPT8,C'1'        IF NOT, SET DEFAULT TO 1                     
*                                                                               
VR02#07  CLC   ACQDTSTR(12),SPACES ANY DATES IN HERE?                           
         BNH   VR02#30                                                          
*                                                                               
         LA    R1,PAYTBL           VERIFY THAT ONLY RUNNING PAYABLES            
VR02#10  CLI   0(R1),X'FF'                                                      
         BNE   VR02#20                                                          
         MVI   ROUTNUM,126         SPECIAL DATES ONLY FOR PAYABLES              
         MVC   FERN,=AL2(INVPYLDG) ERROR - WRONG LEDGER                         
         B     VR02X                                                            
VR02#20  CLC   ACQUNT(2),0(R1)                                                  
         BE    VR02#30                                                          
         LA    R1,2(R1)                                                         
         B     VR02#10                                                          
*                                                                               
VR02#30  CLI   ACQEND,C' '         DO WE HAVE AN END DATE?                      
         BE    VR02#40             NO, SKIP THIS                                
         MVI   ROUTNUM,8                                                        
         MVC   DUB(6),ACQEND                                                    
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(0,WORK)                                     
         GOTO1 DATECHK,DMCB,F'-7',F'3'                                          
*                                                                               
VR02#40  CLC   TWAAGY,=C'BO'       BOCLARO SPECIAL OUTPUT TYPE                  
         BNE   VR02#70                                                          
         LA    R0,4                                                             
         LA    RF,BO55TBL                                                       
*                                                                               
VR02#50  CLC   ACQLDG(2),0(RF)                                                  
         BNE   *+14                                                             
         CLC   ACQACTF1,2(RF)                                                   
         BE    VR02#60                                                          
         LA    RF,9(RF)                                                         
         BCT   R0,VR02#50                                                       
         B     VR02#90                                                          
*                                                                               
VR02#60  MVC   REQOUT(6),3(RF)                                                  
         B     VR02#90                                                          
*                                                                               
*              1ST BYTE = LEDGER                                                
*              2ND BYTE = ACCT CODE                                             
*              3RD BYTE = FILTER 1                                              
*              4-9 BYTE = OUTPUT TYPE                                           
*                                                                               
BO55TBL  DC    C'ST CLTO56'                                                     
         DC    C'SR CLTO57'                                                     
         DC    C'UN CLTO58'                                                     
         DC    C'UNICLTO59'                                                     
*                                                                               
PAYTBL   DC    C'SP'                                                            
         DC    C'SS'                                                            
         DC    C'SU'                                                            
         DC    C'SN'                                                            
         DC    C'ST'                                                            
         DC    C'SQ'                                                            
         DC    X'FF'                                                            
*                                                                               
VR02#70  TM    OUTSTAT,RUNQ                                                     
         BZ    VR02#90                                                          
         CLC   ACQACT(37),SPACES                                                
         BNE   VR02#80                                                          
         CLC   ACQSEL(16),SPACES                                                
         BNE   VR02#80                                                          
         CLC   ACQCARD2(38),SPACES                                              
         BNE   VR02#80                                                          
         CLC   ACQOFFFL(26),SPACES                                              
         BE    VR02#280                                                         
*                                                                               
VR02#80  MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(ONLYLEDG) ONLY LEDGER FIELD IS VALID                   
         B     VR02X                                                            
*                                                                               
VR02#90  CLC   ACQEND,SPACES       ENDDATE REQUIRED ON CHECK SCREEN             
         BNE   *+18                                                             
         MVI   ROUTNUM,8                                                        
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR02X                                                            
*                                                                               
         TM    OUTSTAT,STCKQ       VALIDATE STACKED                             
         BZ    VR02#100                                                         
         CLC   ACQACT,SPACES       STACKED REQUESTS REQUIRE ACCOUNT             
         BNE   *+18                                                             
         MVI   ROUTNUM,3                                                        
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR02X                                                            
         MVC   KEY,SPACES          STACKED REQUEST MUST HAVE LOW LEVEL          
         MVC   KEY(15),ACQCPY      ACCOUNT                                      
         GOTO1 AIOREAD                                                          
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'32'        MUST HAVE A BALANCE ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    VR02#100                                                         
         MVI   ROUTNUM,3           HIGH LEVEL ACCOUNT NOT VALID INPUT           
         MVC   FERN,=AL2(WRNGLVL)  ERROR - WRONG LEVEL ACCOUNT                  
         B     VR02X                                                            
*                                                                               
VR02#100 CLC   ACQBILGP,SPACES     IF THERE'S EST NUM                           
         BE    VR02#110                                                         
         CLC   ACQSEL,SPACES       CLIENT REQUIRED                              
         BNE   VR02#110                                                         
         MVC   FERN,=AL2(FLDMIS)                                                
         MVI   ROUTNUM,94                                                       
         B     VR02X                                                            
*                                                                               
VR02#110 MVI   ROUTNUM,8                                                        
         CLI   ACQLDG,C'U'         NETWORK                                      
         BE    VR02#150                                                         
         CLI   ACQLDG,C'S'                                                      
         BE    *+12                                                             
         CLI   ACQLDG,C'T'         CANADIAN SPOT                                
         BNE   VR02#150                                                         
         CLI   ACQACT,C'N'         NETWORK USES CALENDAR MTHS                   
         BE    VR02#150                                                         
         CLI   ACQACT,C'X'                                                      
         BE    VR02#150                                                         
         CLC   ACQEND,SPACES                                                    
         BNE   VR02#120                                                         
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR02X                                                            
*                                  FOR LEDGER S OR T                            
*                                  IF END DATE IS YYMM GO TO                    
*                                  GET BROADCAST END DATE                       
VR02#120 DS    0H                                                               
         CLC   ACQEND+4(2),SPACES                                               
         BNE   VR02#160                                                         
*                                                                               
         MVC   TEMP(4),ACQEND                                                   
         MVC   TEMP+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),PLIST,(1,TEMP),TEMP+6,GETDAY,ADDAY,RR=RB            
         CLI   PLIST,X'FF'         INVALID                                      
         BNE   VR02#140                                                         
         MVC   FERN,=AL2(INVDATEF) INVALID DATE FORMAT                          
         B     VR02X                                                            
*                                                                               
VR02#140 MVC   ACQEND(6),TEMP+12   END OF BROADCAST MONTH                       
         B     VR02#160                                                         
*                                                                               
VR02#150 CLC   ACQEND,SPACES       IF LEDGER IS NOT S                           
         BE    VR02#160            THEN END DATE IS OPT,YYMMDD                  
         CLC   ACQEND+4(2),SPACES                                               
         BNE   VR02#160                                                         
         MVC   TEMP(4),ACQEND                                                   
         MVC   TEMP+4(2),=C'01'                                                 
         GOTO1 ADDAY,PLIST,(C'M',TEMP),(X'80',TEMP+6),0                         
         MVC   ACQEND,TEMP+6                                                    
*                                                                               
VR02#160 DS    0H                  MOA VALID FOR ANY U/L                        
         XC    ACQCHKSQ(4),ACQCHKSQ                                             
         LA    RF,ACQCHKSQ                                                      
         CLI   ACQACT+1,C' '       PAYEE                                        
         BE    *+8                                                              
         OI    0(RF),X'80'                                                      
         CLI   ACQACT,C' '         MEDIA                                        
         BE    *+8                                                              
         OI    0(RF),X'40'                                                      
         CLI   ACQACTF1,C' '       FILTER 1                                     
         BE    *+8                                                              
         OI    0(RF),X'20'                                                      
         CLI   ACQACTF2,C' '       FILTER 2                                     
         BE    *+8                                                              
         OI    0(RF),X'10'                                                      
         CLI   ACQSEL,C'+'         LIST RECORD                                  
         BNE   *+12                                                             
         OI    0(RF),X'04'                                                      
         B     VR02#170                                                         
         CLI   ACQSEL,C'-'         EXCLUDE LIST RECORD                          
         BNE   *+12                                                             
         OI    0(RF),X'01'                                                      
         B     VR02#170                                                         
         TM    ACQSEL,X'40'        EXCLUDE CLIENT                               
         BO    *+12                                                             
         OI    0(RF),X'02'                                                      
         B     VR02#170                                                         
         CLI   ACQSEL,C' '         CLIENT                                       
         BE    *+8                                                              
         OI    0(RF),X'08'                                                      
*                                                                               
VR02#170 LA    RF,1(RF)            BUMP TO NEXT BYTE                            
         CLI   ACQOPT2,C'*'      ALL OFFICES SAME AS NO OFFICE                  
         BE    VR02#180                                                         
         CLI   ACQOPT2,C' '      OFFICE                                         
         BE    *+8                                                              
         OI    0(RF),X'80'                                                      
*                                                                               
VR02#180 CLI   ACQOPT1,C' '        URGENT                                       
         BE    *+8                                                              
         OI    0(RF),X'40'                                                      
         CLI   ACQOPT3,C' '      APPROVED                                       
         BE    *+8                                                              
         OI    0(RF),X'20'                                                      
         XC    ACQCHKSQ,=X'FFFF'   COMPLEMENT FOR ASCENDING SORT                
         MVC   ACQCHKDT,=X'FFFF'                                                
         CLI   ACQEND,C' '         END DATE                                     
         BE    VR02#190                                                         
         GOTO1 DATCON,PLIST,(0,ACQEND),(2,ACQCHKDT)                             
*                                                                               
VR02#190 DS    0H                                                               
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    VR02#200            DO NOT NEED OFFICE CHECK                     
         CLI   ACQOFFFL,C' '       IF OFFICE FILTER                             
         BNE   *+12                                                             
VR02#200 CLI   ACQSEL,C' '         OR CLIENT FILTER                             
         BE    VR02#280                                                         
*                                                                               
         XC    KEY,KEY             CHECK AUTHORIZATION - CHARECD                
         MVI   KEY,X'10'                                                        
         MVC   KEY+1(3),ACQCPY     CMPY/UNIT/LEDGER                             
         MVC   LKEY,KEY                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=CL8'ACCOUNT',KEY,AIO1                       
         L     R6,AIO1                                                          
         CLC   KEY(4),0(R6)                                                     
         BNE   VR02#280                                                         
*                                                                               
VR02#210 L     R6,AIO1                                                          
         MVI   ELCODE,X'54'                                                     
         BAS   RE,GETEL                                                         
         BE    VR02#230                                                         
*                                                                               
VR02#220 BAS   RE,NEXTEL                                                        
         BE    VR02#230                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,=CL8'ACCOUNT',AIO1,AIO1                      
         L     R6,AIO1             READ NEXT REC FOR ID INFO                    
         CLC   LKEY(4),0(R6)                                                    
         BE    VR02#210                                                         
         B     VR02#280                                                         
*                                                                               
VR02#230 DS    0H                                                               
         USING OCNELD,R6                                                        
         OC    OCNFILT,OCNFILT     IS THERE A FILTER                            
         BZ    VR02#220            NO - TRY NEXT                                
         CLI   OCNFILT,C'*'        SEE IF BY OFFICE                             
         BE    VR02#240            YES                                          
         CLC   OCNOFFID,TWAUSRID   IF IDS ARE DIFFERENT                         
         BNE   VR02#220            THEN TRY NEXT                                
         CLI   ACQSEL,C' '         NO INPUT IS OK                               
         BE    VR02#220                                                         
         CLI   ACQSEL,C'+'         MUST BE A CLT FILTER-NO LISTS                
         BE    VR02#270            INVALID FIELD                                
         CLI   ACQSEL,C'-'                                                      
         BE    VR02#270                                                         
         TM    ACQSEL,X'40'        EXCLUDING A CLIENT IS NOT VALID              
         BZ    VR02#270                                                         
         CLC   OCNFILT,ACQSEL      THEN CLIENTS MUST MATCH                      
         BNE   VR02#270                                                         
         B     VR02#280                                                         
*                                                                               
VR02#240 TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    VR02#220            SKIP OFFICE FILTER CHECK                     
         CLC   ACQOFFFL(1),OCNFILT+1                                            
         BNE   VR02#260            CHK OFFICE MATCH                             
         CLC   OCNOFFID,TWAUSRID   IDS MUST MATCH                               
         BE    VR02#280                                                         
*                                                                               
VR02#250 MVI   ROUTNUM,123         OFFICE INVALID                               
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR02X                                                            
*                                                                               
VR02#260 CLC   OCNOFFID,TWAUSRID   SEE IF IDS MATCH                             
         BNE   VR02#220                                                         
         CLC   ACQOFFFL(1),OCNFILT+1      OFFICE CODES MUST MATCH               
         BNE   VR02#250            ERROR                                        
         B     VR02#280                                                         
*                                                                               
VR02#270 MVI   ROUTNUM,26          CLIENT SELECT INVALID                        
         CLC   ACQPROG,=C'55'      IF REQUEST 55                                
         BNE   *+8                                                              
         MVI   ROUTNUM,94          CLIENT SELECT INVALID                        
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR02X                                                            
*                                                                               
VR02#280 CLC   ACQPROG,=C'55'      IF REQUEST 55-CHECK DATE TYPE                
         BNE   VR02X                                                            
         CLI   ACQOPT5,C' '        ANYTHING IN QOPT5?                           
         BE    VR02#320            CHECK IF PROD LIST WAS ENTERED               
         CLI   ACQOPT5,C'T'        TRANSACTION DATE-LEAVE AS IS                 
         BE    VR02#320            CHECK IF PROD LIST WAS ENTERED               
         CLI   ACQOPT5,C'A'        ACTIVITY DATE DATE TYPE?                     
         BNE   VR02#290                                                         
         MVC   ACQACTND,ACQEND     FILL IN ACTIVITY END DATE                    
         MVC   ACQEND,SPACES       CLEAR TRANSACTION END DATE                   
         BE    VR02#320            CHECK IF PROD LIST WAS ENTERED               
VR02#290 CLI   ACQOPT5,C'D'        DUE DATE DATE TYPE                           
         BNE   VR02#320            CHECK IF PROD LIST WAS ENTERED               
         LA    RE,ACQTYP1                                                       
         LA    R0,4                4 FILTERS CURRENTLY AVAILABLE                
VR02#300 CLI   ACQTYP1-ACQCARD3(RE),X'40'  ANYTHING DEFINED IN FILTERS?         
         BNH   VR02#310                                                         
         LA    RE,L'ACQTYP1+L'ACQFLT1(RE)       BUMP TO NEXT FILTER FLD         
         BCT   R0,VR02#300                                                      
         MVC   FERN,=AL2(INVREQ)   TOO MANY FILTERS-CANT ADD DUE DATES          
         B     VR02X                                                            
*                                                                               
VR02#310 MVI   ACQTYP1-ACQCARD3(RE),ACQDATE     MARK AS DATE                    
         MVI   ACQDTTYP-ACQCARD3(RE),ACQDTDUE   SHOW IT'S A DUE DATE            
         MVC   ACQDTEND-ACQCARD3(L'ACQDTEND,RE),ACQEND                          
         MVC   ACQEND,SPACES       CLEAR TRANSACTION END DATE                   
*                                                                               
VR02#320 CLC   ACQSRTAR,SPACES                  ANY DATA                        
         BNH   VR02X                                                            
         LA    RE,ACQTYP1                                                       
         LA    R0,4                4 FILTERS CURRENTLY AVAILABLE                
VR02#330 CLI   ACQTYP1-ACQCARD3(RE),X'40'  ANYTHING DEFINED IN FILTERS?         
         BNH   VR02#340                                                         
         LA    RE,L'ACQTYP1+L'ACQFLT1(RE)       BUMP TO NEXT FILTER FLD         
         BCT   R0,VR02#330                                                      
         MVC   FERN,=AL2(INVREQ)   TOO MANY FILTERS-CANT ADD DUE DATES          
         B     VR02X                                                            
*                                                                               
VR02#340 MVI   ACQTYP1-ACQCARD3(RE),ACQANAL            MARK AS ANALYSIS         
         MVC   ACQFLT1-ACQCARD3(L'ACQSRTAR,RE),ACQSRTAR MOVE IN PRD LST         
         MVC   ACQSRTAR,SPACES                          CLEAR SRT AREA          
*                                                                               
VR02X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #03                            *         
***********************************************************************         
*                                                                               
VR03     NTR1                                                                   
         MVI   ROUTNUM,18                                                       
         CLI   ACQOPT2,C'Y'      IF RUN BEFORE CHECKS                           
         BNE   VR03#10                                                          
         CLI   ACQOPT1,C'U'        THEN MUST BE UNDISPURSED ONLY                
         BE    VR03#10                                                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR03X                                                            
*                                                                               
VR03#10  MVI   ROUTNUM,07                                                       
         CLI   ACQOPT1,C' '        IF DISPURSED ONLY                            
         BNE   VR03#20                                                          
         CLC   ACQSTART+4(2),=C'  ' NO DAY FOR DISPURSED                        
         BE    VR03#20                                                          
         MVC   FERN,=AL2(INVDATEF) INVALID DATE FORMAT                          
         B     VR03X                                                            
*                                                                               
VR03#20  MVI   ROUTNUM,35                                                       
         CLC   ACQSTART+4(2),SPACES IF START = YMD THEN INCREMENT               
         BE    VR03#30                                                          
         CLC   ACQSRTAR(2),SPACES  MUST BE SPECIFIED                            
         BNE   VR03#30                                                          
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR03X                                                            
*                                                                               
VR03#30  MVI   ROUTNUM,11                                                       
         CLC   ACQSRTAR(2),SPACES  OR INCREMENT                                 
         BNE   VR03#40                                                          
         B     VR03#50                                                          
*                                                                               
VR03#40  CLI   ACQOPT1,C'U'        THEN MUST BE UNDISPURSED ONLY                
         BE    VR03#50                                                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR03X                                                            
*                                                                               
VR03#50  DS    0H                                                               
         MVI   ROUTNUM,7                                                        
         CLI   ACQOPT1,C'U'                                                     
         BE    VR03#60                                                          
         CLC   ACQSTART(4),SPACES    NOT UNDISPURSED - REQUIRE START            
         BNE   VR03#60                                                          
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR03X                                                            
*                                                                               
VR03#60  MVI   ROUTNUM,37                                                       
         CLI   ACQOPT6,C'Y'                                                     
         BNE   VR03#70             OPT6 FOR UNDISP ONLY                         
         CLI   ACQOPT1,C'U'                                                     
         BE    VR03#70                                                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR03X                                                            
*                                                                               
VR03#70  MVI   ROUTNUM,8                                                        
         CLC   ACQEND,SPACES       END DATE                                     
         BE    VR03X                                                            
         CLI   ACQOPT1,C'U'        IF UNDISBURSED                               
         BNE   VR03#80                                                          
         CLC   ACQEND+4(2),SPACES                                               
         BNE   VR03X                  MUST BE YYMMDD                            
         B     VR03#90                                                          
*                                                                               
VR03#80  CLC   ACQEND+4(2),SPACES ELSE IF DISBURSED                             
         BE    VR03X               MUST BE YYMM                                 
*                                                                               
VR03#90  MVC   FERN,=AL2(INVDATEF) INVALID DATE FORMAT                          
*                                                                               
VR03X    TM    RFPSTAT,RFPINUSE    IF USING RFP AND UNDISBURSED REPORT          
         BZ    EXIT                THEN IF DATE ERROR ALLOW IT ANYWAY           
         CLI   ACQOPT1,C'U'        UNDISPURSED                                  
         BNE   EXIT                                                             
         CLC   FERN,=AL2(INVDATEF)                                              
         BNE   EXIT                                                             
         MVC   FERN,=AL2(FF)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #04                            *         
*              USED FOR A21, A22, A27 AND A28                         *         
***********************************************************************         
*                                                                               
VR04     NTR1                                                                   
         MVI   ROUTNUM,30                                                       
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BO    VR04#50             YES, BYPASS VALIDATION FOR NOW               
         GOTO1 LOCKBILL,DMCB,ACQSTART CHECK FOR MOS LOCKOUT                     
         BNE   VR04X                                                            
         CLC   ACQSTART,SPACES                                                  
         BE    VR04#50                                                          
         MVC   TEMP(6),TODAY                                                    
         MVC   PLIST+8(4),=F'31'                                                
         GOTO1 ADDAY,PLIST,TEMP,TEMP+6                                          
         CLC   ACQSTART,TEMP+6                                                  
         BNH   VR04#30             WITHIN +31 DAYS                              
*                                                                               
VR04#20  MVC   FERN,=AL2(INVDATEF) INVALID DATE FORMAT                          
         B     VR04X                                                            
*                                                                               
VR04#30  MVC   PLIST+8(4),=F'-31'                                               
         GOTO1 (RF),(R1)                                                        
         CLC   ACQSTART,TEMP+6                                                  
         BL    VR04#20             NOT WITHIN -31 DAYS                          
*                                                                               
VR04#50  CLC   ACQPROG,=C'27'                                                   
         BE    VR04#60                                                          
         CLC   ACQPROG,=C'28'                                                   
         BE    VR04#60                                                          
         CLI   ACQOPT3,C'Y'        IF OPTION 3=Y                                
         BNE   VR04X                                                            
         CLC   ACQACT+7(5),SPACES  THEN JOB MUST BE PRESENT                     
         BNE   VR04X                                                            
         MVI   ROUTNUM,15          CURSOR TO JOB FIELD                          
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR04X                                                            
*                                                                               
VR04#60  CLC   ACQAPPL+4(2),=C'40'                                              
         BE    VR04#70                                                          
         CLC   ACQAPPL+4(2),=C'50'                                              
         BNE   VR04X                                                            
*                                                                               
VR04#70  MVI   ROUTNUM,134                                                      
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR04X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #05                            *         
***********************************************************************         
*                                                                               
VR05     NTR1                                                                   
         CLC   ACQOPT1(7),SPACES   NO OPTIONS FILLED IN?                        
         BH    *+8                                                              
         MVI   ACQOPT5,C'Y'        DEFAULT BATCH SUMMARY                        
*                                                                               
         CLC   ACQMOSST,SPACES     IF THERE IS AN MOA RANGE ST/END              
         BH    VR05B                  DATES AREN'T REQUIRED                     
         CLC   ACQMOSND,SPACES                                                  
         BNH   VR05C                                                            
*                                                                               
VR05B    CLC   ACQSTART(L'ACQSTART+L'ACQEND),SPACES                             
         BNH   VR05X               START / END DATE OK                          
VR05C    MVI   ROUTNUM,7           POINT TO START DATE                          
         CLC   ACQSTART,SPACES                                                  
         BE    VR05ERR                                                          
         MVI   ROUTNUM,8           POINT TO END DATE                            
         CLC   ACQEND,SPACES                                                    
         BNE   VR05X                                                            
*                                                                               
VR05ERR  MVC   FERN,=AL2(FLDMIS)   EXIT WITH AN ERROR                           
*                                                                               
VR05X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #06                            *         
***********************************************************************         
*                                                                               
VR06     NTR1                                                                   
         CLC   ACQPROG,=C'25'         IF RUNNING 25 CHECK                       
         BNE   VR0610                                                           
         CLC   =C'SOON,',BVROUT       IF THIS IS A SOON REQUEST                 
         BNE   VR0605                                                           
         CLI   ACQOPT2,C'D'           CANT DO A LIVE RUN FOR SOON               
         BE    *+18                                                             
         MVI   ROUTNUM,18             CURSOR TO OPTION 2                        
         MVC   FERN,=AL2(DRFTONLY)    DRAFT ONLY FOR SOON REQUEST               
         B     VR06X                                                            
         MVC   ACQPROG,=C'DU'         CHANGE TO DRAFT UPDATE                    
*                                                                               
         MVC   DUB(6),ACQSTART                                                  
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(0,WORK)                                     
         GOTO1 (RF),DMCB,(5,0),(0,WORK+6)      TODAY'S DATE                     
         LHI   R4,-4                                                            
*                                                                               
         GOTO1 PERVERT,DMCB,WORK+6,WORK        COMPARE TO REQUEST DATE          
*                                                                               
         CH    R4,14(R1)              CHECK IF TOO MANY MONTHS AGO              
         BNH   VR0605                                                           
*                                                                               
         MVI   ROUTNUM,7                       POINT TO START DATE              
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,=CL60'EA#9999 3 months max for all SOON requests'         
         B     VR06X                                                            
*                                                                               
VR0605   CLI   ACQOPT1,C'C'        IS THIS A CORRECTION RUN?                    
         BNE   VR0610              NO                                           
         OC    COMPGLM,COMPGLM                                                  
         BNZ   VR0608                                                           
         MVI   ROUTNUM,159                                                      
         MVC   FERN,=AL2(NEEDGLO)                                               
         B     VR06X                                                            
*                                                                               
VR0608   MVC   DUB(4),ACQSTART                                                  
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(1,WORK)                                     
         CLC   WORK(L'COMPGLM),COMPGLM                                          
         BNL   VR0610                                                           
         MVC   WORK(L'COMPGLM),COMPGLM                                          
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+6)                                  
         MVC   BVRHDR,=CL60'EA#9999 Correction Runs are valid for mmm/y*        
               y later'                                                         
         MVC   BVRHDR+38(6),WORK+6                                              
         MVC   FERN,=AL2(FE)                                                    
         MVI   ROUTNUM,159          POINT TO OPTION #1                          
         B     VR06X                                                            
*                                                                               
VR0610   CLI   ACQUNT,C'S'                                                      
         BE    VR0620                                                           
         CLI   ACQUNT,C'A'                                                      
         BE    VR0620                                                           
         CLI   ACQUNT,C'T'                                                      
         BE    VR0620                                                           
         MVI   ROUTNUM,1                                                        
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR06X                                                            
*                                                                               
VR0620   CLC   ACQSTART,SPACES                                                  
         BE    VR06X                                                            
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BO    VR06X               YES, BYPASS VALIDATION FOR NOW               
         MVC   DUB(6),ACQSTART                                                  
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(0,WORK)                                     
                                                                                
         MVC   DMCB,=F'-3'                                                      
         GOTO1 DATECHK,DMCB,,,F'2'                                              
         MVI   ROUTNUM,7           CURSOR TO START DATE                         
*                                                                               
VR06X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #07                            *         
***********************************************************************         
*                                                                               
VR07     NTR1                                                                   
         MVI   ROUTNUM,35                                                       
         CLI   ACQEND,C' '                                                      
         BE    VR07#20                                                          
         CLC   ACQSRTAR(2),SPACES   IF ENDD=YMD THEN INCREMENT REQ              
         BNE   VR07#30                                                          
*                                                                               
VR07#10  MVC   FERN,=AL2(FLDMIS)                                                
         B     VR07X                                                            
*                                                                               
VR07#20  MVI   ROUTNUM,8           CURSOR TO ENDD                               
         CLC   ACQSRTAR(2),SPACES                                               
         BE    VR07#30                                                          
         CLI   ACQEND,C' '         IF INCREMENT THEN ENDD REQUIRED              
         BE    VR07#10             TO MISSING FLD ERROR                         
*                                                                               
VR07#30  CLI   ACQOPT2,C'I'        IF OPT2=I                                    
         BNE   *+12                                                             
         CLI   ACQOPT3,C'M'        THEN OPT3 CAN'T BE = M.                      
         BE    VR07#40                                                          
         CLI   ACQOPT2,C'C'        IF OPT2=C                                    
         BNE   VR07X                                                            
         CLI   ACQOPT3,C' '        THEN OPT3 MUST BE BLANK.                     
         BE    VR07X                                                            
*                                                                               
VR07#40  MVI   ROUTNUM,19          CURSOR TO OPT3                               
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR07X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #08  (USED BY A98 ONLY)        *         
***********************************************************************         
*                                                                               
VR08     NTR1                                                                   
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(4),ACQMOSND                                                 
         MVC   TEMP+4(2),=C'15'                                                 
         GOTO1 ADDAY,PLIST,TEMP,TEMP+6,30                                       
         MVC   TEMP(6),TEMP+6                                                   
         CLC   TEMP(6),TODAY       COMPARE TODAY VS 15 OF END MTH+1             
         BNH   VR08#10                                                          
         MVI   ROUTNUM,36          CURSOR TO END                                
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR08X                                                            
*                                                                               
VR08#10  DS    0H                                                               
         CLI   ACQSEL,C' '         CHK FOR LIST                                 
         BNE   VR08#30                                                          
         CLI   ACQLDG,C' '         LEDGER REQUIRED                              
         BE    VR08#20                                                          
         CLI   ACQOPT2,C'C'        CLOSED JOBS ONLY FOR SJ OR T                 
         BNE   VR08#50                                                          
         CLC   ACQUNT(2),=C'SJ'                                                 
         BE    VR08#50                                                          
         CLI   ACQUNT,C'T'                                                      
         BE    VR08#50                                                          
         MVI   ROUTNUM,18          CURSOR TO OPTION 2                           
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR08X                                                            
*                                                                               
VR08#20  MVI   ROUTNUM,2           CURSOR TO LEDGER                             
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR08X                                                            
*                                                                               
VR08#30  CLI   ACQSEL,C'-'         NO 'ALL BUT' FOR REQ98                       
         BNE   VR08#40                                                          
         MVI   ROUTNUM,57          CURSOR TO UNIT/LIST                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR08X                                                            
*                                                                               
VR08#40  CLI   ACQLDG,C' '         LEDGER NOT ALLOWED IF LIST INPUT             
         BE    VR08#50                                                          
         MVI   ROUTNUM,2           CURSOR TO LEDGER                             
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR08#50  CLI   ACQOPT2,C'C'        IS THIS A CLOSE?                             
         BNE   VR08#70             NO, MAKE SURE THIS IS A DRAFT                
         CLI   ACQSEL,C' '         YES, IS THIS A LIST?                         
         BNE   VR08#80             YES                                          
         CLC   ACQUNT(2),=C'SJ'    NO, IS IT UNIT SJ?                           
         BE    VR08X               YES                                          
*                                                                               
VR08#60  MVI   ROUTNUM,18          CURSOR TO OPTION 2                           
         MVC   FERN,=AL2(INVINPT)  NOT VALID FOR OTHER LEDGERS                  
         B     VR08X                                                            
*                                                                               
VR08#70  CLI   ACQOPT1,C'D'        IS THIS A DRAFT RUN?                         
         BE    VR08X               YES, DONE                                    
         B     VR08#60             NO, ERROR THEN                               
*                                                                               
VR08#80  CLC   LASTLED,=C'SJ'      IS SJ THE ONLY LIST ENTRY                    
         BNE   VR08#60             NO, PRINT ERROR                              
*                                                                               
VR08X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #09                            *         
***********************************************************************         
*                                                                               
VR09     NTR1                                                                   
         CLI   ACQOPT2,C'T'        IF OPT2=T                                    
         BNE   VR09X                                                            
         MVI   ROUTNUM,115                                                      
         CLC   ACQSTART,SPACES                                                  
         BNE   VR09#10             START AGING NOT ALLOWED                      
         MVI   ROUTNUM,118                                                      
         CLC   ACQEND,SPACES                                                    
         BNE   VR09#10             AND END AGING NOT ALLOWED                    
         MVI   ROUTNUM,37                                                       
         CLI   ACQOPT6,C' '                                                     
         BE    VR09X               AND OPT6 NOT ALLOWED                         
*                                                                               
VR09#10  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR09X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #08                            *         
***********************************************************************         
*                                                                               
VR10     NTR1                                                                   
         MVI   ROUTNUM,11          CURSOR TO OPT 1                              
         CLI   ACQOPT1,C'P'                                                     
         BNE   VR10X                                                            
         CLI   ACQOPT2,C'D'                                                     
         BE    VR10#10                                                          
         MVI   ROUTNUM,31          CURSOR TO OPT4                               
         CLI   ACQOPT4,C'Y'                                                     
         BNE   VR10X                                                            
*                                                                               
VR10#10  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR10X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #11                            *         
***********************************************************************         
*                                                                               
VR11     NTR1                                                                   
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR11#05                                                          
         CLI   ACQOPT1,C'L'        CANT DO A LIVE RUN FOR SOON                  
         BNE   VR11#05                                                          
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR11X                                                            
*                                                                               
VR11#05  CLC   ACQEND,SPACES       END DATE CANT BE > TODAY                     
         BE    VR11#10                                                          
         CLC   ACQEND(4),TODAY                                                  
         BNH   VR11#10                                                          
         MVI   ROUTNUM,X'08'       CURSOR TO END DATE                           
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,=CL60'EA#9999 End Date Cannot Be > Than Today'            
         B     VR11X                                                            
*                                                                               
VR11#10  CLI   ACQOPT6,C' '        OPT1 MUST BE = 'L' IF OPT6 INPUT             
         BE    VR11#15                                                          
         CLI   ACQOPT1,C'L'                                                     
         BE    VR11#15                                                          
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(INVINPT)                                               
         CLI   ACQOPT1,C' '                                                     
         BNE   VR11X                                                            
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR11X                                                            
*                                                                               
VR11#15  BAS   RE,VR01             CHECK THE RANGE NOW                          
*                                                                               
VR11X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #12                            *         
***********************************************************************         
*                                                                               
VR12     NTR1                                                                   
         CLI   ACQOPT1,C'Y'        ONLY ALLOW SR BREAKOUT BY CLIENT             
         BNE   VR12#10                                                          
         CLC   ACQSEL,SPACES       DID THEY ENTER A CLIENT?                     
         BH    VR12#10                                                          
         MVI   ROUTNUM,26                                                       
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,CLIRSRBO     CLIENT NEEDED FOR SR BREAKOUT                
         B     VR12X                                                            
*                                                                               
VR12#10  CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR12#40                                                          
         MVI   ACQOPT5,C'N'        QOPT5=Y IS TO RUN OFF FILE                   
*                                                                               
VR12#20  DS    0H                                                               
         CLC   ACQSEL,SPACES       ONLY ALLOW SOON WITH CLIENTS                 
         BH    VR12#30                                                          
         MVI   ROUTNUM,26          POINT TO CLIENT FIELD                        
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,CLIRSOON     CLIENT NEEDED FOR SOONS                      
         B     VR12X                                                            
*                                                                               
VR12#30  CLI   ACQOPT4,C'Y'        LOCK LEDGER FOR "LIVE" SOON REQUEST          
         BNE   VR12#40                                                          
         GOTO1 CALLOV,DMCB,0,X'D9000A66',0                                      
         L     RF,0(R1)                                                         
         ST    RF,ASETLOCK         SAVE SETLOCK ADDRESS                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'T',KEY),ACOMFACS    TEST THE LOCK               
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
         MVC   FERN,=AL2(LDGLOCK)                                               
         B     VR12X                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'L',KEY),ACOMFACS           SETLOCK              
*                                                                               
VR12#40  MVI   ACQTYP1,ACQDATE     MARK AS DATE                                 
         MVI   ACQDTTYP,ACQDTMOS   SHOW IT'S A MEDIA MOS                        
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BNO   VR12#45             YES, BYPASS VALIDATION FOR NOW               
         MVC   ACQDTEND(4),ACQEND                                               
         MVC   ACQEND,SPACES                                                    
         B     VR12X                                                            
*                                                                               
VR12#45  GOTO1 DATCON,DMCB,(5,0),(0,TEMP)                                       
         GOTO1 ADDAY,PLIST,(C'M',TEMP),TEMP+6,-11                               
         CLC   ACQEND,SPACES                                                    
         BH    *+14                                                             
         MVC   ACQEND,TEMP                                                      
         B     VR12#60                                                          
         CLC   ACQEND,TEMP+6       FOR REQUEST OVER A YEAR OLD                  
         BNL   VR12#50             FORCE THEM TO READ OFF THE FILE              
         MVI   ACQOPT1,C'Y'                                                     
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR12#50                                                          
         MVI   ROUTNUM,36          POINT TO END MONTH                           
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,DTEGTRY                                                   
         B     VR12X                                                            
*                                                                               
VR12#50  MVC   ACQDTEND(4),ACQEND                                               
         MVC   TEMP(4),ACQEND                                                   
         MVC   TEMP+4(2),=C'01'                                                 
         GOTO1 ADDAY,PLIST,(C'M',TEMP),TEMP+6,-11                               
VR12#60  MVC   ACQDTSTR(4),TEMP+6                                               
         MVC   ACQEND,SPACES       CLEAR TRANSACTION END DATE                   
*                                                                               
VR12X    B     EXIT                                                             
*                                                                               
DTEGTRY  DC    CL60'EA#9999 Sooning cannot be > than 1 Year From Today'         
CLIRSOON DC    CL60'EA#9999 Must have a CLIENT/CLIENT LIST for SOONS'           
CLIRSRBO DC    CL60'EA#9999 Must have a ClIENT/CLIENT LIST for SR Optn'         
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #13                            *         
***********************************************************************         
*                                                                               
VR13     NTR1                                                                   
         CLC   REQOUT(4),=C'DOWN'                                               
         BNE   VR13X                                                            
         MVI   ACQOPT7,C'Y'        QOPT7=Y DOWNLOAD                             
*                                                                               
         CLI   ACQOPT2,C'N'                                                     
         BNE   VR13X                                                            
         MVI   ROUTNUM,18                                                       
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR13X                                                            
*                                                                               
VR13X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #14                            *         
***********************************************************************         
*                                                                               
VR14     NTR1                                                                   
         CLI   ACQSTART,C' '                                                    
         BE    VR14X                                                            
         CLI   ACQEND,C' '         IF START INPUT - END IS REQUIRED             
         BNE   VR14X                                                            
         MVC   FERN,=AL2(FLDMIS)                                                
         MVI   ROUTNUM,X'08'       CURSOR TO END DATE                           
*                                                                               
VR14X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #15                            *         
***********************************************************************         
*                                                                               
VR15     NTR1                                                                   
         CLC   REQOUT(4),=CL4'JW99' JWNY                                        
         BE    *+14                                                             
         CLC   REQOUT(4),=CL4'1099' IF USER ASKING FOR FORMS                    
         BNE   VR15#20                                                          
         CLI   ACQOPT1,C'Y'        INSURE APPROPRIATE OPTION REQUESTED          
         BE    VR15#20                                                          
         MVI   ROUTNUM,98          CURSOR TO LIVE FORMS                         
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR15S                                                            
*                                                                               
VR15#20  CLC   REQOUT(4),=C'DOWN'                                               
         BNE   VR15#30                                                          
         CLI   ACQOPT1,C'N'                                                     
         BE    *+18                                                             
         MVI   ROUTNUM,98                                                       
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR15S                                                            
*                                                                               
VR15#30  CLI   ACQOPT1,C'Y'        IF LIVE FORMS = 'Y'                          
         BNE   VR15S               THEN,                                        
         CLI   ACQAPPL,C'N'                                                     
         BNE   VR15#40                                                          
         CLC   REQOUT(5),=C'1099N'  ONLY ACCEPT 1099N FOR 1099-NEC              
         BE    VR15S                                                            
         MVI   ROUTNUM,0           CURSOR TO LIVE FORMS                         
         MVC   FERN,=AL2(INVINPT)                                               
         LA    R1,BVROUTH          POINT DIRECTLY TO BAD FIELD.                 
         ST    R1,FADR                                                          
         B     VR15S                                                            
*                                                                               
VR15#40  CLC   REQOUT(5),=C'1099 '  1099 FOR 1099-MISC                          
         BE    VR15S                                                            
         CLC   REQOUT(4),=C'JW99'                                               
         BE    VR15S                                                            
VR15#45  MVI   ROUTNUM,0                                                        
         MVC   FERN,=AL2(OUT1099)  OUTPUT TYPE MUST BE 1099.                    
         LA    R1,BVROUTH          POINT DIRECTLY TO BAD FIELD.                 
         ST    R1,FADR                                                          
*                                                                               
VR15S    CLI   ACQOPT3,C'C'                                                     
         JE    VR15T                                                            
         CLI   ACQOPT3,C'R'                                                     
         JNE   VR15X                                                            
*                                                                               
VR15T    CLC   ACQSTART(2),=X'FCF0'                                             
         JL    VR15U                                                            
         CLC   ACQEND(2),=X'FCF0'                                               
         JNL   VR15X                                                            
VR15U    MVI   ROUTNUM,19               CURSOR TO QOPT3                         
         MVC   FERN,=AL2(INVINPT)  EXIT WITH AN ERROR                           
*                                                                               
VR15X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #16                            *         
***********************************************************************         
*                                                                               
VR16     NTR1                                                                   
         MVI   ROUTNUM,25          POINT TO PERSON FIELD                        
         CLC   ACQACT,SPACES       IF ACCOUNT SPACES                            
         BNE   VR16#10                                                          
         CLC   ACQFLT1,SPACES      AND PERSON SPACES                            
         BE    VR16#20             CHECK FOR SOON                               
         B     VR16X                                                            
*                                                                               
VR16#10  CLC   ACQFLT1,SPACES                                                   
         BE    VR16X                                                            
         MVC   FERN,=AL2(INVINPT)  EXIT WITH AN ERROR                           
         B     VR16X                                                            
*                                                                               
VR16#20  CLC   =C'SOON,',BVROUT    SOON MUST HAVE ACCT OR PERSON                
         BNE   VR16X                                                            
         MVC   FERN,=AL2(ACCPERS)                                               
*                                                                               
VR16X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #17                            *         
***********************************************************************         
*                                                                               
VR17     NTR1                      FOR REQ B2                                   
         SR    R4,R4               ADD THE NUMBER OF DATA TYPES                 
         LA    R5,ACQSRTAR                                                      
         LA    R6,7                FOR BCT                                      
*                                                                               
VR17#10  CLI   0(R5),0                                                          
         BE    *+8                 CHUNKS X DATA TYPES CAN'T                    
         LA    R4,1(R4)            EXCEED 9 COLUMNS                             
         LA    R5,1(R5)                                                         
         BCT   R6,VR17#10                                                       
         LTR   R4,R4                                                            
         BZ    VR17#40                                                          
         SR    RF,RF               NEED TO COUNT CHUNKS                         
         LA    R5,ACQOPT2          OPTION 2                                     
         LA    R6,3                                                             
*                                                                               
VR17#20  CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,VR17#20                                                       
         LTR   RF,RF               SEE IF ANY CHUNKS SPECIFIED                  
         BZ    VR17#50                                                          
         SR    R6,R6                                                            
         MR    R6,R4                                                            
         C     RF,=F'9'            MAX OF 9 COLUMNS                             
         BNH   VR17X               CHUNKS X DATA TYPES                          
*                                                                               
VR17#30  MVI   ROUTNUM,63          CURSOR TO DATA TYPES                         
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR17X                                                            
*                                                                               
VR17#40  MVI   ROUTNUM,63          AT LEAST ONE DATA TYPE REQUIRED              
         B     *+8                                                              
*                                                                               
VR17#50  MVI   ROUTNUM,62          AT LEAST ONE CHUNK REQUIRED                  
         MVC   FERN,=AL2(FLDMIS)                                                
*                                                                               
VR17X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #18                            *         
***********************************************************************         
*                                                                               
VR18     NTR1                                                                   
         CLI   ACQMEDFL,C' '       MEDIA FILTER ONLY FOR U/L = SJ               
         BE    VR18X                                                            
         CLC   ACQUNT(2),=C'SJ'                                                 
         BE    VR18X                                                            
         MVI   ROUTNUM,21          CURSOR TO MEDIA FILTER                       
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR18X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #19                            *         
***********************************************************************         
*                                                                               
VR19     NTR1                                                                   
         MVC   ACQSTART+4(2),=C'01' SET DAY TO 01                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #20                            *         
***********************************************************************         
*                                                                               
VR20     NTR1                                                                   
         CLC   ACQSEL,SPACES                                                    
         BE    VR20X               IF EXCEPTION LIST SPECIFIED                  
         CLI   ACQOPT2,C' '        OPT 2 CAN'T BE                               
         BE    VR20X                                                            
         MVI   ROUTNUM,18          CURSOR TO OPTION 2                           
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR20X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #21                            *         
***********************************************************************         
*                                                                               
VR21     NTR1                                                                   
         CLC   ACQSTART+4(2),SPACES IF START DATE IS YYMM                       
         BNE   VR21#10                                                          
         CLC   ACQEND+4(2),SPACES   AND END DATE IS YYMM                        
         BE    VR21#20              THEN THEY'RE BOTH VALID                     
         B     VR21#30                                                          
*                                                                               
VR21#10  CLC   ACQEND+4(2),SPACES   IF START DATE IS YYMMDD                     
         BNE   VR21#20              AND END DATE IS YYMMDD, THEN VALID          
         B     VR21#30                                                          
*                                                                               
VR21#20  CLC   ACQPROG,=C'CI'                                                   
         BE    *+12                                                             
         CLI   ACQOPT2,C' '         IF REPORT FORMAT ENTERED                    
         BE    VR21#40                                                          
         MVC   ACQSRTAR(2),=C'P='   MOVE IN 'P=' REPORT NUMBER                  
         MVC   ACQSRTAR+2(2),ACQPROG                                            
         MVC   ACQSRTAR+4(1),ACQOPT2 AND FORMAT                                 
         CLC   ACQPROG,=C'CI'                                                   
         BNE   *+8                                                              
         MVI   ACQSRTAR+4,C'1'     CI ALWAYS GETS A 1                           
         B     VR21#40                                                          
*                                                                               
VR21#30  MVI   ROUTNUM,7           CURSOR TO START DATE                         
         MVC   FERN,=AL2(INVDATEF) INVALID FORMAT                               
         B     VR21X                                                            
*                                                                               
VR21#40  CLC   ACQPROG,=C'FI'      IF ANY FILTER ENTERED THEN                   
         BE    *+14                THEY MUST INPUT U/L (FI&M2 REQS)             
         CLC   ACQPROG,=C'M2'                                                   
         BNE   VR21X                                                            
*                                                                               
         BAS   RE,VR56             CHECK DATE RANGE                             
*                                                                               
         CLC   ACQACTF1(6),SPACES  TEST ALL FILTERS                             
         BE    VR21X                                                            
         CLI   ACQUNT,C' '                                                      
         BNE   VR21#50                                                          
         MVI   ROUTNUM,1           CURSOR TO UNIT FIELD                         
         B     VR21#60                                                          
*                                                                               
VR21#50  CLI   ACQLDG,C' '                                                      
         BNE   VR21X                                                            
         MVI   ROUTNUM,2           CURSOR TO LEDGER FIELD                       
*                                                                               
VR21#60  MVC   FERN,=AL2(FLDMIS)                                                
*                                                                               
VR21X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #22                            *         
***********************************************************************         
*                                                                               
VR22     NTR1                                                                   
         CLI   ACQSTART,C' '                                                    
         BE    VR22#20                                                          
         MVI   ROUTNUM,X'07'       CURSOR TO START DATE                         
         CLI   ACQOPT2,C'T'        ONLY OPT2=T CAN HAVE START DATE              
         BE    VR22#20                                                          
         CLI   ACQOPT2,C'H'        AND OPT2=H                                   
         BE    VR22#20                                                          
         CLI   ACQOPT2,C'U'        AND OPT2=H                                   
         BE    VR22#20                                                          
         CLI   ACQOPT2,C'C'        AND OPT2=C                                   
         BNE   VR22#70                                                          
         CLI   ACQSTART+4,C' '     BUT OPT2=C IS ONLY YYMM                      
         BE    VR22#20                                                          
         MVC   FERN,=AL2(INVDATEF) INVALID FORMAT                               
         B     VR22X                                                            
*                                                                               
VR22#20  CLI   ACQEND,C' '         END DATE                                     
         BE    VR22X                                                            
         CLI   ACQOPT2,C'T'        ONLY OPT2=T CAN HAVE YYMMDD                  
         BE    VR22#30                                                          
         CLI   ACQOPT2,C'H'        OR OPT2=H                                    
         BE    VR22#30                                                          
         CLI   ACQOPT2,C'U'        OR OPT2=U                                    
         BNE   VR22#40                                                          
*                                                                               
VR22#30  CLI   ACQEND+4,C' '                                                    
         BE    VR22#50                                                          
         B     VR22X                                                            
*                                                                               
VR22#40  CLI   ACQOPT2,C'P'        ONLY OPT2=P CAN HAVE YYMM                    
         BE    *+12                                                             
         CLI   ACQOPT2,C'C'        ALSO OPT2=C                                  
         BNE   VR22#60                                                          
         CLI   ACQEND+4,C' '                                                    
         BE    VR22X                                                            
*                                                                               
VR22#50  MVI   ROUTNUM,X'08'                                                    
         MVC   FERN,=AL2(INVDATEF) INVALID FORMAT                               
         B     VR22X                                                            
*                                                                               
VR22#60  MVI   ROUTNUM,X'08'       CURSOR TO END DATE                           
*                                                                               
VR22#70  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR22X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #23                            *         
***********************************************************************         
*                                                                               
VR23     NTR1                                                                   
         MVC   FERN,=AL2(INVINPT)                                               
         CLI   ACQOPT4,C' '      IF OPTION 4 NOT BLANK                          
         BE    VR23#10                                                          
         CLC   ACQSTART+4(2),=C'  ' MUST BE YYMMDD                              
         BE    VR23#30                                                          
         CLI   ACQOPT4,C'D'      IF OPTION 4 = D                                
         BNE   VR23#20                                                          
         MVC   FERN,=AL2(FLDMIS)                                                
         CLC   ACQSEL(2),=C'  '    MUST HAVE DAILY UNITS                        
         BE    VR23#40                                                          
         B     VR23#20                                                          
*                                                                               
VR23#10  CLC   ACQSTART+4(2),SPACES     MUST HAVE YYMM ONLY                     
         BNE   VR23#30                                                          
         CLC   ACQSEL(2),SPACES         AND CANNOT HAVE DAILY UNITS             
         BNE   VR23#40                                                          
*                                                                               
VR23#20  MVC   FERN,=AL2(FF)       ALL OK                                       
         B     EXIT                                                             
*                                                                               
VR23#30  MVI   ROUTNUM,X'07'       CURSOR TO START DATE                         
         B     EXIT                                                             
*                                                                               
VR23#40  MVI   ROUTNUM,X'51'       CURSOR TO DAILY UNITS                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #24                            *         
***********************************************************************         
*                                                                               
VR24     NTR1                                                                   
         CLI   ACQREVOP,C' '       IF USING REVERSAL OPTION THEN                
         BNH   VR24#10             OPTION #1 = 'B' NOT ALLOWED                  
         CLI   ACQOPT1,C'B'                                                     
         BNE   VR24#10                                                          
         MVI   ROUTNUM,11          CURSOR TO OPT1 FIELD                         
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR24X                                                            
*                                                                               
VR24#10  CLC   BVROUT(4),=C'SOON'  FOR SOON REQUESTS ONLY                       
         BNE   VR24#20                                                          
         CLC   ACQACT+6(6),SPACES  IF JOB NOT INPUT THEN DATES MUST             
         BH    VR24#20             BE WITHIN 4 MONTH RANGE                      
         MVC   TEMP(4),ACQSTART                                                 
         MVC   TEMP+4(2),=C'01'                                                 
         MVC   TEMP+6(4),ACQEND                                                 
         MVC   TEMP+10(2),=C'01'                                                
         GOTO1 PERVERT,DMCB,TEMP,TEMP+6                                         
         SR    R1,R1                                                            
         ICM   R1,3,DMCB+14        GET #MOS INCLUSIVE                           
         C     R1,=F'4'                                                         
         BNH   VR24#20                                                          
         MVI   ROUTNUM,7                                                        
         MVC   FERN,=AL2(1812)     INVALID DATE RANGE                           
         B     VR24X                                                            
*                                                                               
VR24#20  CLI   ACQOPT2,C' '        OPTS 3-5 REQUIRE OPT2                        
         BNE   VR24X                                                            
         CLC   ACQTTYPE,=C'   '    TRANSACTION TYPE REQUIRE OPT2                
         BE    VR24#30                                                          
         MVC   FERN,=AL2(FLDMIS)                                                
         MVI   ROUTNUM,X'12'       CURSOR TO OPT2                               
         B     VR24X                                                            
*                                                                               
VR24#30  CLI   ACQSEL,C' '         AND REGULAR REPORT MUST HAVE                 
         BNE   VR24#40             LIST RECORD                                  
         MVC   FERN,=AL2(FLDMIS)                                                
         MVI   ROUTNUM,X'3A'       CURSOR TO LIST                               
         B     VR24X                                                            
*                                                                               
VR24#40  CLI   ACQOPT3,C' '                                                     
         BE    *+12                                                             
         MVI   ROUTNUM,X'13'                                                    
         B     VR24#50                                                          
         CLI   ACQOPT4,C' '                                                     
         BE    *+12                                                             
         MVI   ROUTNUM,X'1F'                                                    
         B     VR24#50                                                          
         CLI   ACQOPT5,C' '                                                     
         BE    VR24X                                                            
         MVI   ROUTNUM,X'20'                                                    
*                                                                               
VR24#50  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR24X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #25                            *         
***********************************************************************         
*                                                                               
VR25     NTR1                                                                   
         CLI   ACQOPT2,C'Y'        IF OUTPUT TAPE IS DESIRED                    
         BNE   VR25X                                                            
         CLC   ACQACT,SPACES       ACCOUNT REQUIRED                             
         BNH   VR25X                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACQCPY      MOVE IN BANK ACCOUNT KEY                     
         GOTO1 AIOREAD             AND REREAD ACCOUNT                           
         BE    *+6                                                              
         DC    H'0'                COULDN'T REREAD ACCOUNT                      
*                                                                               
         USING RSTELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         TM    RSTSTAT,RSTSOFBR   OUTFILE=YES                                   
         BO    VR25X                                                            
         MVC   FERN,=AL2(ACCNFLAG) ACCOUNT NOT FLAGGED FOR O/P FILE             
         MVI   ROUTNUM,X'12'       CURSOR TO OPTION 2                           
*                                                                               
VR25X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #26                            *         
***********************************************************************         
*                                                                               
VR26     NTR1                                                                   
         CLC   ACQPROG,=C'9A'      IF RUNNING 9A-CHK QOPT3                      
         BNE   VR26#10                                                          
         CLI   ACQOPT3,C'Y'        IF OPTION 1 IS 2 OR 3 OR 4                   
         BNE   VR26#10                                                          
         TM    CMPSTAT,CMPNSWT+CMPNSRD  IS EITHER READ/WRITE ON?                
         BNZ   VR26#10                  NO - SKIP IT                            
         MVI   ROUTNUM,19               CURSOR TO QOPT3                         
         MVC   FERN,=AL2(SECLOCK)       SECURITY LOCKOUT                        
         B     VR26X                                                            
*                                                                               
VR26#10  CLI   ACQSRTAR,0          IF NO BUDGET TYPE                            
         BNE   VR26#20                                                          
         CLI   ACQOPT1,C'2'        IF OPTION 1 IS 2 OR 3 OR 4                   
         BL    VR26#20                                                          
         CLI   ACQOPT1,C'4'                                                     
         BNH   *+12                                                             
         CLI   ACQOPT1,C'9'        OR 9                                         
         BNE   VR26#20                                                          
         MVI   ROUTNUM,64          CURSOR TO BUDGET TYPE                        
         MVC   FERN,=AL2(FLDMIS)   BUDGET IS REQUIRED                           
         B     VR26X                                                            
*                                                                               
VR26#20  BAS   RE,VR01             VALIDATE DATE RANGE                          
*                                                                               
VR26X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #27                            *         
*              USED FOR A23,A24, A29 AND A30                          *         
***********************************************************************         
*                                                                               
VR27     NTR1                                                                   
         MVI   ROUTNUM,56                 REV. BILL DATE                        
         GOTO1 LOCKBILL,DMCB,ACQSTART     CHECK FOR MOS LOCKOUT                 
         BNE   VR27X                                                            
         CLC   ACQSTART,SPACES                                                  
         BE    VR27#03                                                          
         MVC   TEMP(6),TODAY                                                    
         MVC   PLIST+8(4),=F'31'                                                
         GOTO1 ADDAY,PLIST,TEMP,TEMP+6                                          
         CLC   ACQSTART,TEMP+6                                                  
         BNH   VR27#02             WITHIN +31 DAYS                              
*                                                                               
VR27#01  MVC   FERN,=AL2(INVDATEF) INVALID DATE FORMAT                          
         B     VR27X                                                            
*                                                                               
VR27#02  MVC   PLIST+8(4),=F'-31'                                               
         GOTO1 (RF),(R1)                                                        
         CLC   ACQSTART,TEMP+6                                                  
         BL    VR27#01             NOT WITHIN -31 DAYS                          
*                                                                               
VR27#03  CLC   ACQPROG,=C'29'                                                   
         BE    VR27#10                                                          
         CLC   ACQPROG,=C'30'                                                   
         BNE   VR27#40                                                          
*                                                                               
VR27#10  CLC   ACQAPPL+4(2),=C'40'                                              
         BE    VR27#20                                                          
         CLC   ACQAPPL+4(2),=C'50'                                              
         BNE   VR27#30                                                          
*                                                                               
VR27#20  MVI   ROUTNUM,134                                                      
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR27X                                                            
*                                                                               
         USING GRBRECD,R5                                                       
VR27#30  LA    R5,KEY                                                           
         XC    GRBKEY,GRBKEY       GROUP BILLING RECORD                         
         MVI   GRBKTYP,GRBKTYPQ    X'2C'                                        
         MVI   GRBKSUB,GRBKSUBQ    X'3A'                                        
         MVC   GRBKCPY,ACQCPY      COMPANY                                      
         MVC   GRBKCLI,SPACES                                                   
         MVC   GRBKCLI(3),ACQACT      CLIENT                                    
         MVC   GRBKBILN,ACQSEL     BILL NUMBER                                  
         GOTO1 DATCON,DMCB,(0,ACQEND),(1,DUB)                                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,DUB                                                         
         LNR   R1,R1               GET 2'S COMPLEMENT                           
         STCM  R1,7,GRBKBILD                                                    
*                                                                               
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(0,DMREAD),ACCOUNT,KEY,(R6)                         
         CLI   DMCB+8,0                                                         
         BNE   VR27X               NO RECORD, JUST EXIT                         
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'B4'        GROUP BILLING DATA ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   VR27X                                                            
*                                                                               
         USING BDAELD,R6                                                        
         MVC   ACQAPPL+4(1),BDAFORM  FORMAT FIELD                               
         MVC   ACQAPPL+5(1),BDALVL  LEVEL FIELD                                 
         MVC   ACQAPPL+6(4),=X'FFFFFFFF'                                        
         XC    TEMP(3),TEMP                                                     
         OC    BDAFDTE,BDAFDTE                                                  
         BZ    VR27X               NO FROM/TO DATES ALLOWED                     
         MVC   TEMP(2),BDAFDTE                                                  
         MVI   TEMP+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,TEMP),(2,ACQAPPL+6)                               
         OC    BDATDTE,BDATDTE                                                  
         BZ    VR27X                                                            
         MVC   TEMP(2),BDATDTE                                                  
         MVI   TEMP+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,TEMP),(2,ACQAPPL+8)                               
         B     VR27X                                                            
*                                                                               
VR27#40  CLI   ACQOPT7,C' '      IF OPTION 7 IS NOT USED                        
         BNE   VR27X                                                            
         CLC   ACQACT+7(5),SPACES  THEN JOB MUST BE PRESENT                     
         BNE   VR27X                                                            
         MVI   ROUTNUM,15          CURSOR TO JOB FIELD                          
         MVC   FERN,=AL2(FLDMIS)                                                
*                                                                               
VR27X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #28                            *         
***********************************************************************         
*                                                                               
VR28     NTR1                                                                   
         MVI   ROUTNUM,87          NUMBER FIELD                                 
         CLI   ACQCPY,X'F5'        IF COMPANY IS BBDO                           
         BNE   VR28X                                                            
         CLI   ACQSEL,C' '         THEN NUMBER REQUIRED                         
         BNE   VR28X                                                            
         MVC   FERN,=AL2(FLDMIS)   MISSING INPUT                                
*                                                                               
VR28X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #29                            *         
***********************************************************************         
*                                                                               
VR29     NTR1                                                                   
         CLC   ACQSEL,SPACES       ONLY ALLOW SOON WITH CLIENTS                 
         BH    VR29#10                                                          
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR29#10                                                          
         MVI   ROUTNUM,26          POINT TO CLIENT FIELD                        
         MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,CLIRSOON     CLIENT NEEDED FOR SOONS                      
         B     VR29X                                                            
*                                                                               
VR29#10  MVI   ACQTYP1,ACQDATE     MARK AS DATE                                 
         MVI   ACQDTTYP,ACQDTMOS   SHOW IT'S A MEDIA MOS                        
         MVC   ACQDTEND(4),ACQEND                                               
         MVC   ACQDTSTR(4),ACQSTART                                             
         MVC   ACQEND,SPACES       CLEAR TRANSACTION DATES                      
         MVC   ACQSTART,SPACES                                                  
*                                                                               
VR29X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #30                            *         
***********************************************************************         
*                                                                               
VR30     NTR1                                                                   
         CLI   ACQOPT3,C' '        IF OPTION INPUT, THEN NO DATES               
         BE    VR30#10                                                          
         MVI   ROUTNUM,7           START DATE                                   
         CLC   ACQSTART,SPACES                                                  
         BNE   VR30#70                                                          
         MVI   ROUTNUM,8           END DATE                                     
         CLC   ACQEND,SPACES                                                    
         BNE   VR30#70                                                          
         B     VR30#20                                                          
*                                                                               
VR30#10  MVI   ROUTNUM,7           ELSE DATES REQUIRED                          
         CLC   ACQSTART,SPACES                                                  
         BE    VR30#60                                                          
         MVI   ROUTNUM,8                                                        
         CLC   ACQEND,SPACES                                                    
         BE    VR30#60                                                          
*                                                                               
VR30#20  MVI   ROUTNUM,91          TRANSMISSION MONTH                           
         CLC   ACQSEL(2),SPACES    IF TRANSMISSION MONTH INPUT                  
         BE    VR30#30                                                          
         CLI   ACQOPT3,C' '        IF OPT3 IS USED NO DATES ALLOWED             
         BNE   VR30#70                                                          
         MVI   ROUTNUM,92          TRANSMISSION YEAR                            
         CLC   ACQSEL+3(2),SPACES  SO MUST TRANSMISSION YEAR                    
         BE    VR30#60                                                          
         B     VR30X                                                            
*                                                                               
VR30#30  MVI   ROUTNUM,92          TRANSMISSION YEAR                            
         CLC   ACQSEL+3(2),SPACES  IF TRANSMISSION YEAR INPUT                   
         BE    VR30#40             SO MUST TRANSMISSION MONTH                   
         CLI   ACQOPT3,C' '        IF OPT3 IS USED NO DATES ALLOWED             
         BNE   VR30#70                                                          
         B     VR30#50                                                          
*                                                                               
VR30#40  CLI   ACQOPT1,C'Y'        IF OUTPUT TAPE REQUESTED                     
         BNE   VR30X               REQUIRE MM/YY                                
         MVI   ROUTNUM,11          OPTION 1                                     
         CLI   ACQOPT3,C' '        IF OPT3 IS USED OPT1 NOT ALLOWED             
         BNE   VR30#70                                                          
         B     VR30#50                                                          
*                                                                               
VR30#50  MVI   ROUTNUM,91          TRANSMISSION MONTH                           
*                                                                               
VR30#60  MVC   FERN,=AL2(FLDMIS)   MISSING INPUT                                
         B     VR30X                                                            
*                                                                               
VR30#70  MVC   FERN,=AL2(NOTCOMP)  INCOMPATIBLE INPUT FIELDS                    
*                                                                               
VR30X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #32                            *         
***********************************************************************         
*                                                                               
VR32     NTR1                                                                   
         CLC   ACQFLT5,SPACES      ANY AMOUNT?                                  
         BE    VR32A               NO                                           
         CLC   ACQFLT6,SPACES      YES, HOURS ALSO?                             
         BE    VR32A               NO                                           
         MVI   ROUTNUM,175         YES, HOURS OR AMOUNT, NOT BOTH               
         MVC   FERN,=AL2(HRSAAMT)                                               
         B     VR32X                                                            
*                                                                               
VR32A    CLI   ACQTYP8,C' '        ARE WE REVERSING?                            
         BE    VR32C               NO                                           
         MVI   ACQOPT2,C'Y'        YES, THEN SET OPTION 2                       
         B     VR32D                                                            
*                                                                               
VR32C    CLI   ACQOPT3,C'Y'        IS THIS A LIVE RUN?                          
         BNE   VR32D               NO, SKIP THE REFERENCE UPDATE                
         MVC   KEY,SPACES          READ THE COMPANY RECORD                      
         MVC   KEY(1),ACQCPY                                                    
         GOTO1 AIOREAD                                                          
         BE    *+6                                                              
         DC    H'0'                COULDN'T READ COMPANY                        
         USING CPXELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,CPXELQ       FIND THE EXTRA COMPANY ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                COULDN'T FIND ELEMENT                        
*                                                                               
         L     RF,CPXTXRQ#                                                      
         AHI   RF,1                BUMP REFERENCE BY 1                          
         STCM  RF,15,CPXTXRQ#      AND STORE IT BACK                            
         CVD   RF,DUB                                                           
*                                                                               
         UNPK  ACQFLT8(6),DUB+6(2)                                              
         OI    ACQFLT8+5,X'F0'                                                  
         MVI   ACQTYP8,C'8'                                                     
*                                                                               
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,(R6),(R6)                             
         CLI   DMCB+8,0                                                         
         BE    VR32D                                                            
         MVC   FERN,=AL2(CANTADD)  UNSUCCESSFUL ADD                             
         B     VR32X                                                            
*                                                                               
VR32D    CLC   ACQUNT(2),=C'1N'    IS FROM ACCOUNT 1N?                          
         BNE   VR32E               NO                                           
         CLI   ACQOPT1,C' '        YES, FROM TIME MUST BE N OR BLANK            
         BE    VR32E                                                            
         CLI   ACQOPT1,C'N'                                                     
         BE    VR32E                                                            
         MVI   ROUTNUM,173                                                      
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR32X                                                            
*                                                                               
VR32E    CLC   ACQFLT4(2),=C'1N'   IS TO ACCOUNT 1N?                            
         BNE   VR32F                                                            
         CLI   ACQOPT4,C' '        YES, TO TIME MUST BE N OR BLANK              
         BE    VR32X                                                            
         CLI   ACQOPT4,C'N'                                                     
         BE    VR32X                                                            
VR32E1   MVI   ROUTNUM,179                                                      
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR32X                                                            
*                                                                               
VR32F    CLI   ACQOPT4,C'B'        IF TRANSFERRING AS B TIME                    
         BNE   VR32X               WORKCODE CANNOT BE UNBILLABLE                
         CLC   ACQFLT3(2),SPACES   DO WE HAVE A WORKCODE?                       
         B     VR32I                                                            
*&&DO                                                                           
         BE    VR32I               NO                                           
         LA    RE,GOBDATA          CLEAR GOBLOCK                                
         L     RF,=A(L'GOBLOCK)                                                 
         XCEFL                                                                  
         LA    R6,GOBDATA                                                       
         USING GOBLOCKD,R6                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(1),ACQCPY     SET ACCOUNT DETAIL                        
         MVC   GOSELCUL+1(1),ACQFLT4                                            
         MVC   GOSELCUL+2(1),ACQFLT4+1                                          
         MVC   GOSELCLI(3),ACQFLT4+2                                            
         OC    GOSELCLI,SPACES                                                  
         MVC   GOSELPRO(3),ACQFLT4+5                                            
         OC    GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,ACQFLT4+8                                               
         OC    GOSELJOB,SPACES                                                  
         GOTO1 VGETOPT,DMCB,GOBDATA                                             
*                                                                               
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         LA    R0,6                                                             
VR32G    CLC   ACQFLT3(2),0(R1)                                                 
         BE    VR32H                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,VR32G                                                         
         B     VR32I                                                            
*                                                                               
VR32H    MVC   FERN,=AL2(INVWKCD)                                               
         MVI   ROUTNUM,177                                                      
*&&                                                                             
VR32I    CLC   ACQFLT4(2),=C'SJ'   IS TO ACCOUNT SJ?                            
         BNE   VR32X               NO                                           
         MVC   KEY,SPACES          YES, READ IT                                 
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(14),ACQFLT4                                                
         GOTO1 AIOREAD                                                          
         BE    *+6                                                              
         DC    H'0'                COULDN'T READ ACCOUNT                        
         USING JOBEL,R6                                                         
         L     R6,AIO1                                                          
         MVI   ELCODE,JOBELQ       LOOK FOR JOBEL                               
         BAS   RE,GETEL                                                         
         BNE   VR32E1              MUST NOT BE A JOB RECORD                     
         TM    JOBSTA1,JOBSXJOB    CAN'T HAVE X-JOB AND B TIME                  
         BO    VR32E1                                                           
*                                                                               
VR32X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #33                            *         
***********************************************************************         
*                                                                               
VR33     NTR1                                                                   
         MVC   FERN,=AL2(FF)                                                    
         BAS   RE,VR01             INSURE PERIOD NOT GT 1 YEAR                  
         CLC   FERN,=AL2(FF)                                                    
         BNE   VR33X                                                            
         MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         MVI   ROUTNUM,1           UNIT                                         
         CLI   ACQOPT1,C'G'        IF OPT1=G                                    
         BNE   VR33#10                                                          
         CLI   ACQUNT,C' '         AND UNIT INPUT                               
         BE    VR33#10                                                          
         CLI   ACQUNT,C'S'         IT MUST BE 'S'                               
         BNE   VR33X                                                            
*                                                                               
VR33#10  MVC   FERN,=AL2(INPNALLW) INPUT NOT ALLOWED                            
         MVI   ROUTNUM,100         FROM BUDGET                                  
         CLI   ACQSRTAR,0          IF IT'S NOT BLANK                            
         BE    VR33#20                                                          
         CLI   ACQOPT1,C'T'        THEN CAN'T HAVE OPT1='T'                     
         BE    VR33X                                                            
         CLI   ACQOPT1,C'M'        THEN CAN'T HAVE OPT1='T'                     
         BE    VR33X                                                            
         B     VR33#30                                                          
*                                                                               
VR33#20  MVC   FERN,=AL2(FLDMIS)   MISSING FIELD                                
         CLI   ACQOPT1,C'T'        ALL OPTIONS BUT 'T' & M REQUIRE  '           
         BE    VR33#30             FROM FIELD                                   
         CLI   ACQOPT1,C'M'                                                     
         BE    VR33#30                                                          
         B     VR33X                                                            
*                                                                               
VR33#30  CLI   ACQOPT1,C'D'        IF OPT1='D'                                  
         BNE   VR33#40                                                          
         MVC   FERN,=AL2(INPNALLW) INPUT NOT ALLOWED                            
         MVI   ROUTNUM,101                                                      
         CLI   ACQSRTAR+1,0        CAN'T HAVE 'TO' BUDGET                       
         BNE   VR33X                                                            
         MVI   ROUTNUM,19                                                       
         CLI   ACQOPT3,C' '      CAN'T HAVE OPTION 3                            
         BNE   VR33X                                                            
         B     VR33#50                                                          
*                                                                               
VR33#40  MVC   FERN,=AL2(FLDMIS)   MISSING FIELD                                
         MVI   ROUTNUM,101         TO BUDGET                                    
         CLI   ACQSRTAR+1,0        OTHER OPTIONS MUST HAVE TO BUDGET            
         BE    VR33X                                                            
         CLI   ACQOPT1,C'A'        FOR OPTION 1 = 'A'                           
         BNE   VR33#50                                                          
         MVI   ROUTNUM,104         FROM CONTRA U/L REQUIRED                     
         CLC   ACQCNTRA(2),SPACES                                               
         BE    VR33X                                                            
         MVI   ROUTNUM,107         AND TO CONTRA U/L REQUIRED                   
         CLC   ACQSEL+2(2),SPACES                                               
         BE    VR33X                                                            
         B     VR33#60                                                          
*                                                                               
VR33#50  MVC   FERN,=AL2(INPNALLW) INPUT NOT ALLOWED FOR OTHERS                 
         MVI   ROUTNUM,104         FROM CONTRA U/L                              
         CLC   ACQCNTRA(2),SPACES                                               
         BNE   VR33X                                                            
         MVI   ROUTNUM,107         AND TO CONTRA U/L                            
         CLC   ACQSEL+2(2),SPACES                                               
         BNE   VR33X                                                            
*                                                                               
VR33#60  MVC   FERN,=AL2(FF)       NO ERROR                                     
*                                                                               
VR33X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #34                            *         
***********************************************************************         
*                                                                               
VR34     NTR1                                                                   
         CLI   ACQOPT2,C' '        CX - IF OPTION 2 ENTERED                     
         BE    VR34X                                                            
         CLI   ACQSRTAR,C' '       THEN SO MUST AGENCY FIELD                    
         BNE   VR34X                                                            
         MVI   ROUTNUM,95          CURSOR TO AGENCY FILTER                      
         MVC   FERN,=AL2(FLDMIS)   MISSING INPUT                                
*                                                                               
VR34X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #35                            *         
***********************************************************************         
*                                                                               
VR35     NTR1                                                                   
         CLI   ACQOPT3,C' '        IF OPTION 3 ENTERED                          
         BE    VR35X                                                            
         CLI   ACQACT,C' '         ACCOUNT MUST BE INPUT                        
         BE    VR35#10                                                          
         MVC   KEY,SPACES          BUILD ACCOUNT KEY                            
         MVC   KEY(15),ACQCPY                                                   
         GOTO1 AIOREAD             AND RE-READ ACCOUNT                          
         BE    *+6                                                              
         DC    H'0'                COULDN'T RE-READ ACCOUNT                     
         L     R6,AIO1                                                          
         MVI   ELCODE,X'32'        MUST FIND BALANCE ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    VR35X                                                            
*                                                                               
VR35#10  MVC   FERN,=AL2(WRNGLVL)  WRONG LEVEL ACCOUNT                          
         MVI   ROUTNUM,19          CURSOR TO OPTION 3                           
*                                                                               
VR35X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #37                            *         
***********************************************************************         
*                                                                               
VR37     NTR1                                                                   
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR37X                                                            
         CLI   ACQOPT1,C'Y'        CAN'T DO IF GENERATING A TAPE                
         BNE   VR37X                                                            
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR37X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #38                            *         
***********************************************************************         
*                                                                               
VR38     NTR1                                                                   
         CLI   ACQMEDGP,C' '       IF THERE'S MEDIA GROUP                       
         BE    *+12                                                             
         MVI   ROUTNUM,10          MOVE CURSOR TO IT                            
         B     VR38#10                                                          
         CLI   ACQMEDFL,C' '       ELSE IF THERE'S MEDIA FILTER                 
         BE    VR38X                                                            
         MVI   ROUTNUM,21          MOVE CURSOR TO IT                            
*                                                                               
VR38#10  MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   ACQUNT(2),LREQJOB   SET U/L = SJ                                 
         MVC   KEY+1(2),ACQUNT                                                  
         GOTO1 AIOREAD             AND READ                                     
         BNE   VR38#20             ERROR IF CAN'T READ                          
         CLI   ACQOPT2,C'E'      IF OPT2 = E                                    
         BNE   VR38X                                                            
         MVI   ROUTNUM,18          ERROR - MOVE CURSOR TO IT                    
*                                                                               
VR38#20  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR38X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #30                            *         
***********************************************************************         
*                                                                               
VR39     NTR1                                                                   
         CLC   BVROUT(4),=C'SOON'  IF THIS IS A SOON REQUEST                    
         BNE   VR39X                                                            
         CLI   ACQOPT1,C'N'        IT CAN'T BE A LIVE RUN                       
         BE    VR39#10                                                          
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(DRFTONLY) DRAFT ONLY FOR SOON REQUEST                  
         B     VR39X                                                            
*                                                                               
VR39#10  CLC   ACQAPPL+8(4),SPACES   THEN REFERENCE IS REQUIRED                 
         BNE   VR39X                                                            
         MVI   ROUTNUM,46          SET CURSOR TO REFERENCE FIELD                
         MVC   FERN,=AL2(FLDMIS)   AND REQUIRE INPUT                            
*                                                                               
VR39X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #40                            *         
***********************************************************************         
*                                                                               
VR40     NTR1                                                                   
         MVI   ROUTNUM,11          MOVE CURSOR TO OPT1                          
         CLI   ACQOPT1,C'N'        IF OPT1 IS N                                 
         BNE   VR40X                                                            
         CLC   ACQOPT4(3),SPACES AND ATTRIBUTES(OPT4-6) ARE USED                
         BE    VR40X                                                            
         MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
*                                                                               
VR40X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #41                            *         
***********************************************************************         
*                                                                               
VR41     NTR1                                                                   
         DC    H'0'                NOT USED, DUMP IF ENTERED                    
VR41X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #42                            *         
***********************************************************************         
*                                                                               
VR42     NTR1                                                                   
         CLI   ACQOPT7,C'Y'        IF MATERIALS ARE SELECTED, REQUEST           
         BE    VR4202              NEEDS TO BE DOWNLOAD                         
         CLI   ACQOPT2,C'M'                                                     
         BNE   VR4202              - WILL GIVE INVALID OPTION ERROR             
         MVI   ROUTNUM,18                                                       
         MVC   FERN,=AL2(AE$OP2DN)                                              
         B     VR42X                                                            
*                                                                               
VR4202   BAS   RE,XVCOMDL                                                       
         BAS   RE,VR01             REQUEST PERIOD LESS THAN ONE YEAR?           
*                                                                               
VR42X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #43                            *         
***********************************************************************         
*                                                                               
VR43     NTR1                                                                   
         CLC   ACQPROG,=C'CF'      COST OF FINANCE                              
         BNE   VR4310                                                           
         CLC   REQOUT(4),=C'DOWN'  IF THEY ARE DOWNLOADING THEY CANT            
         BNE   VR4310              CHOOSE OPT3=Y OR OPT4=S                      
         MVI   ROUTNUM,19                                                       
         CLI   ACQOPT3,C'Y'        Y=PRINT DETAIL REPORT (NONE IF DWN)          
         BE    *+16                                                             
         MVI   ROUTNUM,31                                                       
         CLI   ACQOPT4,C'S'        S=SUPPRESS AGING COLUMNS (SAME)              
         BNE   VR4310                                                           
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR43X                                                            
*                                                                               
VR4310   MVI   ROUTNUM,11          POINT TO OPTION1                             
         CLI   ACQOPT2,C'C'        IF OPTION2 = C THEN FORCE OPT1               
         BNE   VR43X                                                            
         CLI   ACQOPT1,C' '                                                     
         BNE   VR43X                                                            
         MVC   FERN,=AL2(FLDMIS)                                                
*                                                                               
VR43X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #44                            *         
***********************************************************************         
*                                                                               
VR44     NTR1                                                                   
         MVI   ROUTNUM,140         POINT TO OFFICE FIELD                        
         CLC   ACQACT(3),SPACES    IF CLIENT ENTERED THEN OFFICE                
         BE    VR44X                                                            
         CLC   ACQOFFFL,SPACES     CANT BE ENTERED.                             
         BE    VR44X                                                            
         MVC   FERN,=AL2(INVINPT)  EXIT WITH AN ERROR                           
*                                                                               
VR44X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #45                            *         
***********************************************************************         
*                                                                               
VR45     NTR1                                                                   
         CLC   ACQPROG,=C'BX'      IF RUNNING BX OR BF CHECK                    
         BE    *+14                   OUTPUT TYPE                               
         CLC   ACQPROG,=C'BF'                                                   
         BNE   VR4510                                                           
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   VR4510                                                           
         CLI   ACQOPT1,C'Y'        CANT DO A LIVE RUN FOR SOON                  
         BNE   VR4510                                                           
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR45X                                                            
*                                                                               
VR4510   CLC   ACQACT(3),SPACES    IF ALL CLIENTS THEN FORCE SOMETHING          
         BNE   VR45X                                                            
         CLC   ACQOFFFL,SPACES     EITHER OFFICE                                
         BNE   VR45X                                                            
         CLC   ACQACTF1(6),SPACES  OR A FILTER                                  
         BNE   VR45X                                                            
         CLC   ACQSTART,SPACES     OR A START DATE                              
         BNE   VR45X                                                            
         CLC   ACQEND,SPACES       OR AN END DATE                               
         BNE   VR45X                                                            
         CLC   ACQMOSST,SPACES     OR A MOA RANGE                               
         BNE   VR45X                                                            
         CLC   ACQMOSND,SPACES                                                  
         BNE   VR45X                                                            
         MVI   ROUTNUM,7           POINT TO START DATE                          
         MVC   FERN,=AL2(FLDMIS)   EXIT WITH AN ERROR                           
*                                                                               
VR45X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #46                            *         
***********************************************************************         
*                                                                               
VR46     NTR1                      AUTO HOLD REPORT                             
         CLI   ACQSTART,C' '       DO WE HAVE A START DATE ?                    
         BE    VR46#20             NO, LOOK FOR END                             
         MVI   ROUTNUM,X'07'       YES, CURSOR TO START DATE                    
*                                                                               
VR46#10  CLI   ACQOPT1,C'A'        OPTION 1 MUST BE SET                         
         BE    VR46X                                                            
         CLI   ACQOPT1,C'M'                                                     
         BE    VR46X                                                            
         CLI   ACQOPT1,C'U'                                                     
         BE    VR46X                                                            
         CLI   ACQOPT1,C'B'                                                     
         BE    VR46X                                                            
         MVC   FERN,=AL2(INVOPT1)                                               
         B     VR46X                                                            
*                                                                               
VR46#20  MVI   ROUTNUM,X'08'       SET CURSOR AT END DATE                       
         CLI   ACQEND,C' '         DO WE HAVE AN END DATE ?                     
         BNE   VR46#10             YES, SEE IF WE HAVE AN OPTION                
*                                                                               
VR46X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #47                            *         
***********************************************************************         
*                                                                               
VR47     NTR1                                                                   
         CLC   BVROUT(4),=C'SOON'  FOR SOON REQUESTS ONLY                       
         BNE   VR47X                                                            
         CLI   ACQOPT1,C'D'        IF OPTION 1=D (DRAFT)                        
         BNE   VR47X                                                            
         CLC   ACQACT,SPACES       THEN ACCOUNT IS REQUIRED                     
         BNE   VR47X                                                            
         MVI   ROUTNUM,3           SET CURSOR TO ACCOUNT FIELD                  
         MVC   FERN,=AL2(FLDMIS)   AND REQUIRE INPUT                            
*                                                                               
VR47X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #48                            *         
***********************************************************************         
*                                                                               
VR48     NTR1                      IF BOTH START/END DATES INPUT                
         CLC   ACQSTART,SPACES     THEN INSURE RANGE NOT WITHIN                 
         BNH   VR48#30             CALENDAR YEAR                                
         CLC   ACQEND,SPACES                                                    
         BNH   VR48#30                                                          
*                                                                               
         USING CASRECD,R6          R6=A(CALENDAR RECORD)                        
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,ACQCPY                                                   
         GOTO1 DATCON,DMCB,(0,ACQEND),(1,TEMP)                                  
         MVC   CASPEDTE,TEMP        CALENDAR END DATE                           
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'ACCDIR ',KEY,AIO1                     
         B     VR48#20                                                          
*                                                                               
VR48#10  GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=C'ACCDIR ',KEY,AIO1                     
VR48#20  L     R6,AIO1                                                          
         CLC   0(CASPEDTE-CASKEY,R6),KEY     SAME KEY UPTO END DATE?            
         BE    *+12                                                             
         BAS   RE,VR01             CHECK THAT DATE IS NOT > YR                  
         B     VR48#30                                                          
*                                                                               
         CLC   CASPOFC,SPACES          ARE WE AT DEFAULT CAL RECORD             
         BNE   VR48#10                     IF NOT - READ AGAIN                  
         GOTO1 DATCON,DMCB,(0,ACQSTART),(1,TEMP+3)                              
         CLC   TEMP+3(3),CASPSDTE                                               
         BL    VR48#10                                                          
         CLC   TEMP(3),CASPEDTE                                                 
         BH    VR48#10                                                          
         DROP  R6                                                               
*                                                                               
VR48#30  CLC   ACQSEL(4),SPACES    IF TS#RANGE NOT INPUT THE BOTH               
         BH    VR48#50             START AND END DATES ARE REQUIRED             
         CLC   ACQSTART,SPACES                                                  
         BH    VR48#40                                                          
         OC    ACQSTART(4),ACQSTART IS THIS A SOFTDATE?                         
         BZ    VR48#40                                                          
         MVI   ROUTNUM,X'07'       SET CURSOR AT START DATE                     
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR48X                                                            
VR48#40  CLC   ACQEND,SPACES                                                    
         BH    VR48#50                                                          
         OC    ACQEND(4),ACQEND    IS THIS A SOFTDATE?                          
         BZ    VR48#50                                                          
         MVI   ROUTNUM,X'08'       SET CURSOR AT END DATE                       
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR48X                                                            
*                                                                               
VR48#50  CLC   ACQSTART,SPACES     IF START DATE ENTERED THEN ENDDATE           
         BNH   VR48#60             REQUIRED                                     
         CLC   ACQEND,SPACES                                                    
         BH    VR48#60                                                          
         OC    ACQEND(4),ACQEND    IS THIS A SOFTDATE?                          
         BZ    VR48#60                                                          
         MVI   ROUTNUM,X'08'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR48X                                                            
*                                                                               
VR48#60  CLC   ACQEND,SPACES       IF END DATE ENTERED THEN START DATE          
         BNH   VR48X               REQUIRED                                     
         CLC   ACQSTART,SPACES                                                  
         BH    VR48X                                                            
         OC    ACQSTART(4),ACQSTART IS THIS A SOFTDATE?                         
         BZ    VR48X                                                            
         MVI   ROUTNUM,X'07'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
*                                                                               
VR48X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #49                            *         
***********************************************************************         
*                                                                               
VR49     NTR1                      VALIDATE MOA RANGE                           
         CLC   ACQMOSST(8),SPACES                                               
         BNH   VR49A                                                            
         MVI   BYTE,X'FF'                                                       
         MVC   TEMP(4),ACQMOSST                                                 
         MVC   TEMP+4(2),=C'01'                                                 
         MVC   TEMP+6(4),ACQMOSND                                               
         MVC   TEMP+10(2),=C'01'                                                
         GOTO1 PERVERT,DMCB,TEMP,TEMP+6                                         
         SR    R1,R1                                                            
         ICM   R1,3,DMCB+14        GET #MOS INCLUSIVE                           
         C     R1,=F'12'                                                        
         BNH   VR49A                                                            
         MVI   ROUTNUM,X'56'       MOS FIELD                                    
         MVC   FERN,=AL2(1813)     INVALID DATE RANGE > 12 MOS                  
         B     VR49X                                                            
*                                                                               
VR49A    DS    0H                  VALIDATE CALENDAR MONTHS                     
         CLI   ACQTYP1,ACQDATE                                                  
         BNE   VR49B                                                            
         CLI   ACQDTTYP,ACQDTPER   CALENDAR PERIOD                              
         BNE   VR49B                                                            
         CLC   ACQDTSTR(12),SPACES                                              
         BNH   VR49B                                                            
         MVI   BYTE,X'FF'                                                       
         MVC   TEMP(4),ACQDTSTR                                                 
         MVC   TEMP+4(2),=C'01'                                                 
         MVC   TEMP+6(4),ACQDTEND                                               
         MVC   TEMP+10(2),=C'01'                                                
         GOTO1 PERVERT,DMCB,TEMP,TEMP+6                                         
         SR    R1,R1                                                            
         ICM   R1,3,DMCB+14        GET #MOS INCLUSIVE                           
         C     R1,=F'12'                                                        
         BNH   VR49B                                                            
         MVI   ROUTNUM,X'99'       CALENDAR FIELD                               
         MVC   FERN,=AL2(1813)     INVALID DATE RANGE > 12 MOS                  
         B     VR49X                                                            
*                                                                               
VR49B    DS    0H                                                               
         CLI   BYTE,0                                                           
         BNE   VR49X                                                            
         MVI   ROUTNUM,X'56'       SET CURSOR TO MOA FIELD                      
         MVC   FERN,=AL2(FLDMIS)   AND REQUIRE INPUT                            
*                                                                               
VR49X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #50                            *         
***********************************************************************         
*                                                                               
VR50     NTR1                                                                   
         MVC   FERN,=AL2(FF)                                                    
         CLI   ACQOPT7,C'I'        IF OPT7=I SKIP PERIOD CHECK                  
         BE    *+8                                                              
         BAS   RE,VR01             INSURE PERIOD NOT GT 1 YEAR                  
         CLC   FERN,=AL2(FF)                                                    
         BNE   VR50X                                                            
*                                                                               
         CLI   ACQOPT1,C' '                                                     
         BE    VR50#10                                                          
         CLI   ACQOPT1,C'2'                                                     
         BE    VR50#10                                                          
         CLI   ACQOPT2,C'Y'                                                     
         BNE   VR50#10                                                          
         MVI   ROUTNUM,X'12'       CURSOR TO OPT2                               
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR50X                                                            
*                                                                               
VR50#10  CLI   ACQOPT7,C'I'        IF OPT7=I THEN OPT1 MUST BE BLANK            
         BNE   VR50X               OR A '3'                                     
         CLI   ACQOPT1,C' '                                                     
         BE    VR50X                                                            
         CLI   ACQOPT1,C'3'                                                     
         BE    VR50X                                                            
         MVI   ROUTNUM,X'48'       CURSOR TO OPT7                               
         MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR50X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #51                            *         
***********************************************************************         
*                                                                               
VR51     NTR1                                                                   
         MVC   FERN,=AL2(FF)                                                    
         CLC   ACQACT,SPACES       IF CLIENT ALL, OR BLANK                      
         BNE   VR51X                                                            
         CLC   ACQBILGP,SPACES     BILLING GROUP IS REQUIRED                    
         BNE   VR51X                                                            
         MVC   FERN,=AL2(MORESPEC)                                              
         MVI   ROUTNUM,X'6'        CURSOR TO BILL GROUP                         
*                                                                               
VR51X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #52                            *         
***********************************************************************         
*                                                                               
VR52     NTR1                                                                   
         CLC   BVROUT(4),=C'SOON'  FOR SOON REQUESTS ONLY                       
         BE    VR52X                                                            
         MVI   ROUTNUM,0                                                        
         MVC   FERN,=AL2(OUTSOON)  OUTPUT TYPE MUST BE SOON                     
         LA    R1,BVROUTH          POINT DIRECTLY TO BAD FIELD.                 
         ST    R1,FADR                                                          
*                                                                               
VR52X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #53                            *         
***********************************************************************         
*                                                                               
VR53     NTR1                                                                   
         CLC   BVROUT(4),=C'SOON'  FOR SOON REQUESTS ONLY                       
         BNE   VR53X                                                            
         TM    COMPSTAA,CPYSADBL   TEST AGENCY ON NEW SOON BILLING              
         BNO   VR5301              NO,                                          
         CLC   ACQPROG,=C'21'      JUST TEST FOR LOCKED FOR NOW                 
         BE    VR5302                                                           
         CLC   ACQPROG,=C'22'                                                   
         BE    VR53X                                                            
         CLC   ACQPROG,=C'23'                                                   
         BE    VR5302                                                           
         CLC   ACQPROG,=C'24'                                                   
         BE    VR53X                                                            
*                                                                               
VR5301   CLC   ACQPROG,=C'27'      VALID FOR AC27 AND AC29 ONLY                 
         BE    VR5302                                                           
*                                                                               
         CLC   ACQPROG,=C'29'      THIS PART JUST FOR A29 ONLY                  
         BNE   VR53X                                                            
*                                                                               
         MVI   ROUTNUM,19                                                       
         CLI   ACQOPT3,C'Y'        IS OPTION FOR REALLOCATE ON?                 
         BNE   VR5302              NO                                           
         MVC   FERN,=AL2(INVINPT)                                               
         B     VR53X                                                            
*                                                                               
VR5302   LA    R1,BVROUTH          POINT DIRECTLY TO BAD FIELD.                 
         ST    R1,FADR                                                          
         MVI   ROUTNUM,0                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A66',0                                      
         L     RF,0(R1)                                                         
         ST    RF,ASETLOCK         SAVE SETLOCK ADDRESS                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'T',KEY),ACOMFACS    TEST THE LOCK               
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
*                                                                               
VR5304   MVC   FERN,=AL2(LDGLOCK)                                               
         B     VR53X                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB                                                 
         GOTO1 AIOREAD             READ THE LEDGER RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACCRECD,R5                                                       
         L     R5,AIO1                                                          
         LR    R6,R5                                                            
         MVI   ELCODE,LOKELQ       LOOK FOR LOCK ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   VR5306              NOT THERE, OK SETLOCK WILL ADD IT            
*                                                                               
         USING LOKELD,R6                                                        
         TM    LOKSTAT,LOKSLOCK    IS IT ALREADY LOCKED?                        
         BO    VR5304              YES, ERROR                                   
*                                                                               
VR5306   CLC   ACQPROG,=C'21'      FOR A21 AND A23, JUST TEST                   
         BE    VR53X               DON'T LOCK                                   
         CLC   ACQPROG,=C'23'                                                   
         BE    VR53X                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB                                                 
         MVC   KEY+3(3),ACQACT                                                  
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACCRECD,R5                                                       
         L     R5,AIO1                                                          
         LR    R6,R5                                                            
         MVI   ELCODE,LOKELQ       LOOK FOR LOCK ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   VR5308              NOT THERE, ADD IT                            
*                                                                               
         USING LOKELD,R6                                                        
         TM    LOKSTAT,LOKSLOCK    IS IT ALREADY LOCKED?                        
         BZ    VR5310              NO, LOCK IT                                  
         MVC   FERN,=AL2(CLILOCK)  YES, ERROR                                   
         B     VR53X                                                            
*                                                                               
         USING LOKELD,R6                                                        
VR5308   MVI   LOKEL,LOKELQ        ADD CLIENT LOCK HERE                         
         MVI   LOKLN,LOKLNQ                                                     
         MVI   LOKSTAT,0                                                        
         MVI   LOKELD+LOKLNQ,0                                                  
         LA    R1,LOKELD+LOKLNQ+1                                               
         LA    R0,ACCRECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,ACCRECD+ACCORLEN   SET NEW LENGTH                           
*                                                                               
VR5310   MVI   LOKSTAT,LOKSLOCK                                                 
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   LOKDATE,FADATEB                                                  
         MVC   LOKTIME,FATIME                                                   
         MVC   LOKLUID,FASYM                                                    
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',ACCRECD,ACCRECD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ASETLOCK,DMCB,(C'L',KEY),ACOMFACS     SET THE LOCK               
*                                                                               
VR53X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #54                            *         
***********************************************************************         
*                                                                               
VR54     NTR1                                                                   
         MVC   FERN,=AL2(FF)                                                    
         BAS   RE,VR01             ENSURE PERIOD NOT GT 1 YEAR                  
         CLC   FERN,=AL2(FF)                                                    
         BNE   VR54X                                                            
*                                                                               
         CLI   ACQOPT4,C' '        IF THERE IS SOMETHING IN OPT4                
         BE    VR5410              THERE MUST DOWN IN REQOUT                    
         CLC   REQOUT(4),=C'DOWN'                                               
         BE    VR5410                                                           
         MVI   ROUTNUM,X'1F'       POINT CURSOR TO DOWNLOAD FIELD               
         MVC   FERN,=AL2(INVDWNRQ)        INVALID DOWNLOAD REQ                  
         B     VR54X                                                            
*                                                                               
VR5410   CLC   ACQAPPL+6(6),SPACES        ANY GROUP CODE                        
         BE    VR54X                                                            
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,ACQCPY             COMPANY CODE                          
         MVC   ACTKUNT(2),=C'FR'          ALWAYS 'FR'                           
         MVC   ACTKACT(6),ACQAPPL+6       GROUP CODE                            
         GOTO1 AIOREAD                    READ ACCOUNT RECORD                   
         BE    VR54X                                                            
         MVI   ROUTNUM,X'33'              POINT CURSOR TO GROUP FIELD           
         MVC   FERN,=AL2(INVGRPCD)        LEDGER NOT ON FILE                    
*                                                                               
VR54X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* POST VALIDATION ROUTINE #55 (USED BY AWI/AWC)                       *         
***********************************************************************         
         SPACE 1                                                                
VR55     NTR1                                                                   
         CLC   ACQPROG,=C'WC'      VALID FOR ACWI AND ACWC ONLY                 
         BE    VR5530                                                           
*                                                                               
         CLC   ACQSTART(L'ACQSTART+L'ACQEND),SPACES                             
         BE    VR5510              STR/END DATE NOT VALID W/MOA FORMAT          
*                                                                               
         CLI   ACQOPT1,C'3'        ONLY FORMAT '3' ALLOWS ST/END DATES          
         BE    VR5510              AND NO MOA DATES                             
*                                                                               
         MVI   ROUTNUM,7           SET CURSOR TO START DATE FIELD               
         CLC   ACQSTART,SPACES     CHECK IF START DATE ENTERED                  
         BNE   *+8                                                              
         MVI   ROUTNUM,8           SET CURSOR TO END DATE FIELD                 
         MVC   FERN,=AL2(INPNALLW) FIELD NOT ALLOWED                            
         B     VR55X                                                            
*                                                                               
VR5510   CLI   ACQOPT1,C'4'        MOA BY CLIENT FORMAT                         
         BE    *+12                                                             
         CLI   ACQOPT1,C'5'        MOA BY OFFICE FORMAT                         
         BNE   VR5520                                                           
*                                                                               
         CLC   ACQMOSST(L'ACQMOSST+L'ACQMOSND),SPACES                           
         BNE   VR5520              NO                                           
         MVI   ROUTNUM,X'56'       SET CURSOR TO MOA FIELD                      
         MVC   FERN,=AL2(FLDMIS)   AND REQUIRED INPUT                           
         B     VR55X                                                            
*                                                                               
* VALIDATE GROUP CODE                                                           
*                                                                               
VR5520   CLI   ACQOPT1,C'3'        ONLY FORMAT '3' ALLOWS ST/END DATES          
         BNE   VR5530              AND NO MOA DATES                             
         CLC   ACQMOSST(L'ACQMOSST+L'ACQMOSND),SPACES                           
         BE    VR5530              NO                                           
         MVI   ROUTNUM,X'56'       SET CURSOR TO MOA FIELD                      
         MVC   FERN,=AL2(INPNALLW) FIELD NOT ALLOWED                            
         B     VR55X                                                            
*                                                                               
VR5530   CLC   ACQAPPL,SPACES      WAS ANY GROUP ENTERED?                       
         BE    VR55X               NO - EXIT                                    
         CLC   ACQOPT8,SPACES      GROUP WAS ENTERED - LEDGER MUST BE!          
         BNE   *+18                                                             
         MVI   ROUTNUM,X'32'       POINT CURSOR TO LEDGER FIELD                 
         MVC   FERN,=AL2(MISGRLDG) MISSING GROUP LEDGER                         
         B     VR55X                                                            
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,ACQCPY              COMPANY CODE                         
         MVI   ACTKUNT,C'F'                UNIT ALWAYS 'F'                      
         MVC   ACTKLDG,ACQOPT8                                                  
         MVC   ACTKACT(L'ACQAPPL),ACQAPPL  GROUP CODE                           
         GOTO1 AIOREAD                     READ ACCOUNT RECORD                  
         BE    VR55X                                                            
         MVI   ROUTNUM,X'33'       POINT CURSOR TO GROUP FIELD                  
         MVC   FERN,=AL2(INVGRPCD) LEDGER NOT ON FILE                           
*                                                                               
VR55X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #56                            *         
***********************************************************************         
*                                                                               
VR56     NTR1                                                                   
         MVI   ROUTNUM,8           MAX DURATION = 24 MONTHS                     
         CLC   ACQSTART,SPACES                                                  
         BE    VR56X               START NOT INPUT                              
         CLC   ACQEND,SPACES                                                    
         BE    VR56X                                                            
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BO    VR56X               YES, BYPASS VALIDATION FOR NOW               
*                                                                               
         MVC   TEMP(L'ACQSTART),ACQSTART                                        
         MVC   TEMP+4(2),=C'01'                                                 
         GOTO1 ADDAY,PLIST,(C'Y',TEMP),TEMP+6,2                                 
         GOTO1 ADDAY,PLIST,(C'D',TEMP+6),TEMP+12,-1                             
*                                                                               
         CLC   TEMP+12(4),ACQEND                                                
         BNL   *+10                                                             
         MVC   FERN,=AL2(INVDTE2Y)    DATE RANGE GREATER THAN 2 YEARS           
*                                                                               
VR56X    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DATE RANGE CHECK AGAINST TODAY                         *         
***********************************************************************         
*                                                                               
*        P1=F'DAYS BACK, INCLUSIVE'                                             
*        P2=F'DAYS AHEAD, INCLUSIVE'                                            
*        WORK(6) = REQUEST DATE                                                 
*                                                                               
DATECHK  NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)    TODAY'S DATE                     
*                                                                               
         GOTO1 PERVERT,DMCB,WORK+6,WORK        COMPARE TO REQUEST DATE          
*                                                                               
         CH    R4,16(R1)                                                        
         BH    DATECHNO            TOO LONG AGO                                 
*                                                                               
         CH    R5,16(R1)                                                        
         BL    DATECHNO            TOO FAR INTO THE FUTURE                      
         B     EXIT                                                             
*                                                                               
DATECHNO MVC   FERN,=AL2(FE)                                                    
         MVC   BVRHDR,=CL60'EA#9999 Date Is Outside Allowable Range'            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHECK MOS BILL DATE LOCKOUT                            *         
***********************************************************************         
*                                                                               
*        P1 = A(DATE FIELD - YYMMDD)                                            
*                                                                               
         USING OFFALD,R5                                                        
         USING PROFKD,R4                                                        
LOCKBILL NTR1                                                                   
         TM    RFPSTAT,RFPINUSE    RFP IN USE?                                  
         BO    LOCKBOK             YES, BYPASS VALIDATION                       
         L     R5,AOFFBLK                                                       
         L     R6,0(R1)                                                         
         LA    R4,PROFDATA         READ LK PROFILE RECORD                       
         XC    PROFDATA,PROFDATA                                                
         XC    LKDATA,LKDATA                                                    
         MVI   PROFKSYS,C'A'         ACCOUNT SYSTEM                             
         MVC   PROFKPGM+1(2),=C'LK' LK PROGRAM                                  
         MVC   PROFKAGY,TWAAGY     ALPHA ID                                     
         MVC   PROFKUNL,ACQUNT     UNIT                                         
*                                                                               
         CLC   ACQOFFFL,SPACES     YES, REQUEST BY OFFICE ?                     
         BNH   *+10                NO                                           
         MVC   SOFFICE,ACQOFFFL    YES, USE THAT ONE                            
         CLC   SOFFICE,SPACES      DO WE HAVE A CLIENT/REQUEST OFFICE ?         
         BE    LOCKB4              NO                                           
         TM    OFFACST4,X'01'      YES, ARE WE ON NEW OFFICES ?                 
         BZ    LOCKB2              NO                                           
         MVI   PROFKAST,C'+'       NEW OFFICE INDICATOR FOR GETPROF             
         MVC   PROFKOFC,SOFFICE                                                 
         B     LOCKB4                                                           
*                                                                               
LOCKB2   CLC   SOFFICE,SPACES      DO WE HAVE A CLIENT/REQUEST OFFICE ?         
         BE    LOCKB4              NO                                           
         MVI   PROFKAST,C'*'       OLD OFFICE INDICATOR FOR GETPROF             
         MVC   PROFKOFF,SOFFICE                                                 
*                                                                               
LOCKB4   GOTO1 GETPROF,DMCB,PROFKEY,LKDATA,DATAMGR                              
         OC    LKDATA,LKDATA       DID WE FIND ANY PROFILE ?                    
         BZ    LOCKBOK             NO, NO NEED TO CHECK                         
         MVC   WORK+3(2),LKDATA    CONVERT PROFILE DATE                         
         MVI   WORK+5,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK+3),(0,WORK+6)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         CLC   0(6,R6),SPACES      YES, DO WE HAVE A DATE ?                     
         BNE   LOCKB6              YES, USE THAT DATE                           
         GOTO1 DATCON,DMCB,(5,0(R6)),(1,FULL) USE TODAY'S DATE                  
         B     LOCKB8                                                           
*                                                                               
LOCKB6   GOTO1 DATCON,DMCB,(0,0(R6)),(1,FULL)                                   
*                                                                               
LOCKB8   CLC   WORK(2),FULL        COMPARE YEAR/MONTH PACKED                    
         BL    LOCKBOK                                                          
         MVC   FERN,=AL2(BILLOCK)  MOS BILLING DATE LOCK                        
         LTR   RB,RB               RETURN BAD CC                                
         B     LOCKBX                                                           
*                                                                               
LOCKBOK  CR    RB,RB               RETURN CC EQUAL                              
LOCKBX   B     EXIT                                                             
         DROP  R5,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              SEARCH REQUEST CHAIN AND COUNT/DISPLAY                 *         
***********************************************************************         
*                                                                               
ENQREQ   LA    R4,ENQFRSTH                                                      
         ST    R4,DISPADR          R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         LA    R6,ENQENDH                                                       
         TWAXC (R4),(R6),PROT=Y    CLEAR SCREEN                                 
         XC    SKIPCTR(08),SKIPCTR SET COUNTERS                                 
         MVC   DISPMAX,=H'14'                                                   
*                                                                               
         XC    ADR,ADR                                                          
         LA    R5,DMRDIR                                                        
         CLI   REQOPTN,C'N'                                                     
         BNE   ENQR1                                                            
         CLC   DISPFLDS(2),DISPMAX WAS THERE PREVIOUS                           
         BL    ENQRE2              NO                                           
         MVC   REQINCR,=H'2'       SET TO SKIP 1                                
         LH    R6,DISPFLDS+2       SEQ OF 1ST = LAST + 1                        
         AH    R6,DISPFLDS                                                      
         STH   R6,DISPFLDS+2                                                    
         LH    R6,DISPFLDS         SET ADR TO A(LAST)                           
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   ADR,0(R6)                                                        
         B     ENQR3                                                            
*                                                                               
ENQR1    MVC   DISPFLDS+2(2),REQINCR         SEQ OF 1ST = INPUT VALUE           
         CLI   REQNUM,255                                                       
         BNE   *+12                                                             
         LA    R5,DMRSEQ                                                        
         B     ENQR3                                                            
         MVC   ADR,=X'000000FF'                                                 
         MVC   ADR(2),LREQCARD                                                  
         B     ENQR3                                                            
*                                                                               
ENQR2    LA    R5,DMRSEQ                                                        
         CLI   REQNUM,255                                                       
         BE    ENQR3                                                            
         MVC   ADR,REQLINK                                                      
         LA    R5,DMRDIR                                                        
*                                                                               
ENQR3    L     R0,AIO2                                                          
         LH    R1,=Y(L'REQREC)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,(DMIN,(R5)),REQUEST,ADR,AIO2,(R6)                   
         LA    R0,REQREC                                                        
         LH    R1,=Y(L'REQREC)                                                  
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REQ RECORD IO2 -> REQREC                
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    ENQR4                                                            
         TM    DMCB+8,X'80'                                                     
         BO    ENQREOF                                                          
         B     REQIOERR                                                         
*                                                                               
ENQR4    CLI   REQNUM,255          FILTER OUT CANCELLED FROM ALL OPTION         
         BNE   ENQR4A                                                           
         CLC   ACQPROG,=X'FFFF'                                                 
         BE    ENQR2                                                            
         CLC   ACQPROG,=C'99'                                                   
         BE    ENQR2                                                            
         TM    REQFLAG,X'01'       FILTER OUT UNLINKED FROM ALL OPTION          
         BZ    ENQR2                                                            
         B     ENQR4B                                                           
*                                                                               
ENQR4A   DS    0H                                                               
*        CLC   ACQPROG,=C'55'      DETERMINE IF AUTHORIZED FOR CHECKS           
*        BNE   *+12                                                             
*        BAS   RE,CHKAUTH                                                       
*        BNE   ENQR2                                                            
*                                                                               
         CLC   ACQPROG,LREQCARD    FILTER OUT ANY SUNDRY REQUESTS               
         BE    *+14                                                             
         CLC   ACQPROG,=C'99'                                                   
         BNE   ENQR2                                                            
*                                                                               
ENQR4B   CLI   LREQCARD+9,X'00'                                                 
         BE    ENQR4C                                                           
         CLC   ACQCPY,LREQCARD+9   FILTER ON COMPANY CODE                       
         BNE   ENQR2                                                            
*                                                                               
ENQR4C   OC    LREQOHDR(4),LREQOHDR                                             
         BZ    ENQR5                                                            
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE CODE                        
         BNE   ENQR2                                                            
*                                                                               
ENQR5    TM    REQFLTR,X'01'       REQUESTOR FILTER REQUIRED                    
         BZ    ENQR5A              NO                                           
         CLC   ACQESTOR,LREQOHDR+92 YES TEST REQUESTOR NAME                     
         BNE   ENQR2               IGNORE IF DIFFERENT                          
*                                                                               
ENQR5A   OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    ENQR5B                                                           
         CLC   REQOUT,LREQOHDR+4   FILTER ON OUTPUT TYPE                        
         BNE   ENQR2                                                            
*                                                                               
ENQR5B   OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    ENQR5C                                                           
         CLC   REQDEST,LREQOHDR+11 FILTER ON DESTINATION ID                     
         BNE   ENQR2                                                            
*                                                                               
ENQR5C   DS    0H                                                               
*                                                                               
ENQR6    LH    R4,REQINCR          INGNORE (REQINCR-1) REQS                     
         BCTR  R4,0                                                             
         CH    R4,SKIPCTR                                                       
         BE    ENQR7                                                            
         LH    R4,SKIPCTR                                                       
         LA    R4,1(R4)                                                         
         STH   R4,SKIPCTR                                                       
         B     ENQR2                                                            
*                                                                               
ENQR7    LH    R1,READCTR          UPDATE REQ READ COUNTER                      
         LA    R1,1(R1)                                                         
         STH   R1,READCTR                                                       
*                                                                               
         LA    R0,REQREC                                                        
         LH    R1,=Y(L'REQREC)                                                  
         LR    RF,R1                                                            
         LA    RE,TEMP                                                          
         MVCL  RE,R0                                                            
*                                                                               
         CLC   ACQPROG,=C'99'                                                   
         BNE   *+16                                                             
         LH    R1,CANCCTR          UPDATE CANCELLED COUNTER                     
         LA    R1,1(R1)                                                         
         STH   R1,CANCCTR                                                       
         CLI   REQOPTN,C'L'        DISPLAY LAST OPTION                          
         BE    ENQR2               YES DO NOT DISPLAY                           
*                                                                               
ENQR8    BAS   RE,ENQDISP          DISPLAY REQUEST                              
         CLC   DISPCTR,DISPMAX     END OF SCREEN                                
         BL    ENQR2               NO BACK FOR NEXT                             
         B     ENQRE3                                                           
*                                                                               
ENQREOF  OC    READCTR,READCTR                                                  
         BZ    ENQRE2              NO REQUESTS FOUND                            
         CLI   REQOPTN,C'L'                                                     
         BE    ENQRE1                                                           
         OC    DISPCTR,DISPCTR                                                  
         BZ    ENQRE2              NO REQUESTS DISPLAYED                        
         B     ENQRE3                                                           
*                                                                               
ENQRE1   LA    R0,TEMP                                                          
         LH    R1,=Y(L'REQREC)                                                  
         LR    RF,R1                                                            
         LA    RE,REQREC                                                        
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,ENQDISP          DISPLAY THE LAST REQ                         
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(24),=C'Request total = nnn live'                            
         MVC   TEMP+24(33),=C' - last request num nnn displayed'                
         LH    R6,READCTR                                                       
         SH    R6,CANCCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+16(3),DUB                                                   
         LH    R6,READCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+44(3),DUB                                                   
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
*                                                                               
ENQRE2   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(18),=C'Requests not found'                                  
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
*                                                                               
ENQRE3   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'Requests nnn thru nnn displayed'                     
         MVC   TEMP+31(25),=C' - change cancel status ?'                        
         LH    R6,DISPFLDS+2       GET FIRST                                    
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+09(3),DUB                                                   
         AH    R6,DISPFLDS         GET LAST = FIRST+TOTAL-1                     
         BCTR  R6,0                                                             
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+18(3),DUB                                                   
         MVI   STATUS,1            SET STATUS FOR INPUT                         
*                                                                               
ENQRX    DS    0H                                                               
         MVC   BVRHDR,TEMP         SET HDR MSG                                  
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
         LH    R1,READCTR          NUMBER OF RECORDS READ                       
         LA    R1,1(R1)            PLUS 1                                       
         AH    R1,SKIPCTR          PLUS NUMBER SKIPPED                          
         STH   R1,LASTREAD                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FORMAT REQUEST DATA INTO DISPLAY LINE                  *         
***********************************************************************         
*                                                                               
ENQDISP  NTR1                                                                   
         L     R4,AIO1             READ USER ID REC                             
         MVC   SVCPLOGO,SPACES                                                  
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,REQORIG                                                  
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         LA    RE,CTIDATA                                                       
*                                                                               
ENQD10   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CTDSCELQ      X'02' - DESCRIPTION ELEMENT                  
         BE    ENQD20                                                           
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     ENQD10                                                           
*                                                                               
         USING CTDSCD,RE                                                        
ENQD20   SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AHI   R1,-2               SUBTRACT OVERHEAD                            
         CHI   R1,L'SVCPLOGO                                                    
         BNH   *+8                                                              
         LA    R1,L'SVCPLOGO                                                    
         BCTR  R1,0                                                             
         MVC   SVCPLOGO(0),CTDSC   MOVE IN DESC.                                
         EX    R1,*-6                                                           
         DROP  RE                                                               
*                                                                               
         OC    FLTID,FLTID         ANY FILTERING ON ID?                         
         BZ    *+14                                                             
         CLC   SVCPLOGO,FLTID      SAME ID?                                     
         BNE   ENQDISPX                                                         
*                                                                               
         USING DISPLD,R4                                                        
         L     R4,DISPADR          R4=A(NEXT SCR DISP LINE)                     
         XC    DLINE,DLINE         INITIALIZE DISPLAY LINE(S)                   
         OC    ACQCARD2,ACQCARD2                                                
         BZ    *+10                                                             
         XC    DLINE2,DLINE2                                                    
         MVI   DLCANC,C' '                                                      
         MVC   DLNUM,ACQPROG                                                    
         CLC   ACQPROG,=C'99'                                                   
         BNE   ENQD30                                                           
         MVI   DLCANC,C'C'                                                      
         MVC   DUB(1),REQNUMB                                                   
         BRAS  RE,GETREQID                                                      
         MVC   DLNUM,DUB+1                                                      
*                                                                               
ENQD30   CLI   REQNUM,255          DONT DISPLAY REQ ID FOR SPECIFICS            
         BE    *+10                                                             
         MVC   DLNUM,SPACES                                                     
         OC    ACQREPSQ,ACQREPSQ                                                
         BZ    *+10                                                             
         MVC   DLSEQ,ACQREPSQ                                                   
         OC    ACQXJOB,ACQXJOB                                                  
         BZ    *+10                                                             
         MVC   DLXJOB,ACQXJOB                                                   
         OC    ACQCOMNT(6),ACQCOMNT                                             
         BZ    *+10                                                             
         MVC   DLCOMMNT(6),ACQCOMNT                                             
         OC    ACQOPT8,ACQOPT8                                                  
         BZ    *+10                                                             
         MVC   DLOPT8,ACQOPT8                                                   
         OC    ACQACTST(6),ACQACTST                                             
         BZ    *+10                                                             
         MVC   DLACTSTR(6),ACQACTST                                             
         OC    ACQACTND(6),ACQACTND                                             
         BZ    *+10                                                             
         MVC   DLACTEND(6),ACQACTND                                             
*                                                                               
         CLC   ACQMOSST,SPACES                                                  
         BNH   ENQD40                                                           
         MVC   WORK(L'ACQMOSST),ACQMOSST        CONVERT INTO Y2K                
         MVC   WORK+4(2),=C'01'                 ADD DAY                         
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+8)                              
         MVC   DLMOSTRT,WORK+8                                                  
ENQD40   CLC   ACQMOSND,SPACES                                                  
         BNH   ENQD50                                                           
         MVC   WORK(L'ACQMOSND),ACQMOSND        CONVERT INTO Y2K                
         MVC   WORK+4(2),=C'01'                 ADD DAY                         
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+8)                              
         MVC   DLMOSEND,WORK+8                                                  
*                                                                               
ENQD50   OC    ACQDTSTR,ACQDTSTR                                                
         BZ    *+10                                                             
         MVC   DLCALSTR(6),ACQDTSTR                                             
         OC    ACQDTEND,ACQDTEND                                                
         BZ    *+10                                                             
         MVC   DLCALEND(6),ACQDTEND                                             
         OC    ACQCNTRA,ACQCNTRA                                                
         BZ    *+10                                                             
         MVC   DLCONTRA,ACQCNTRA                                                
         OC    ACQCNTRA+14(4),ACQCNTRA+14                                       
         BZ    *+10                                                             
         MVC   DLCONTRA+14(4),ACQCNTRA+14                                       
         OC    ACQOFFFL(2),ACQOFFFL                                             
         BZ    *+10                                                             
         MVC   DLOFFICE(2),ACQOFFFL                                             
         OC    ACQSRTAR,ACQSRTAR   IF SORT AREA IS PRESENT                      
         BZ    ENQD90                                                           
         MVC   DLSRTA,ACQSRTAR     DISPLAY IT                                   
         LA    R6,DLSRTA                                                        
         LA    R0,7                                                             
ENQD60   CLI   0(R6),0                                                          
         BE    ENQD70                                                           
         CLI   0(R6),C' '                                                       
         BE    ENQD70                                                           
         CLI   0(R6),C'='          ALLOW 'P=NN' IN APG REPORTS                  
         BE    ENQD70                                                           
         CLI   0(R6),C'A'         CHK FOR BINARY FIELD                          
         BL    ENQD80              YES                                          
         CLI   0(R6),C'9'         CHK FOR BINARY FIELD                          
         BH    ENQD80              YES                                          
ENQD70   LA    R6,1(R6)                                                         
         BCT   R0,ENQD60                                                        
         B     ENQD90                                                           
*                                                                               
ENQD80   MVC   DLSRTA,SPACES       SET TO SPACES SO SCREEN                      
*                                  WON'T GET SCREWED UP                         
ENQD90   GOTO1 HEXOUT,PLIST,ACQCPY,DLCMPY,1,0                                   
         CLI   PLIST+19,0                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID HEX IN ACQCPY                        
         MVC   DLUNIT,ACQUNT                                                    
         MVC   DLLDGR,ACQLDG                                                    
         MVC   DLACNO,ACQACT                                                    
         MVC   DLLGRP,ACQTRNF                                                   
         MVC   DLBGRP,ACQBILGP                                                  
         MVC   DLFTR1,ACQACTF1                                                  
         MVC   DLFTR2,ACQACTF2                                                  
         MVC   DLFTR3,ACQACTF3                                                  
         MVC   DLFTR4,ACQACTF4                                                  
         MVC   DLFTR5,ACQACTF5                                                  
         MVC   DLMEDIA,ACQMEDFL                                                 
         MVC   DLBILTYP,ACQBILTY                                                
*                                                                               
         MVC   DLSTRD,SPACES                                                    
         CLC   ACQSTART,SPACES                                                  
         BNH   ENQD100                                                          
         MVC   WORK(6),ACQSTART                                                 
         CLC   WORK+4(2),SPACES    IS THERE ANY DAY?                            
         BH    *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+6)                              
         LA    R1,6                ASSUME LENGTH IS 6 BYTES                     
         CLC   ACQSTART+4(2),SPACES                                             
         BH    *+8                 DON'T INCLUDE DAY IF NOT VALID               
         LA    R1,4                                                             
         AHI   R1,-1                                                            
         MVC   DLSTRD(0),WORK+6                                                 
         EX    R1,*-6                                                           
ENQD100  MVC   DLENDD,SPACES                                                    
         CLC   ACQEND,SPACES                                                    
         BNH   ENQD110                                                          
         MVC   WORK(6),ACQEND                                                   
         CLC   WORK+4(2),SPACES    IS THERE ANY DAY?                            
         BH    *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+6)                              
         LA    R1,6                ASSUME LENGTH IS 6 BYTES                     
         CLC   ACQEND+4(2),SPACES                                               
         BH    *+8                 DON'T INCLUDE DAY IF NOT VALID               
         LA    R1,4                                                             
         AHI   R1,-1                                                            
         MVC   DLENDD(0),WORK+6                                                 
         EX    R1,*-6                                                           
*                                                                               
ENQD110  MVC   DLSORT,ACQSORT                                                   
         MVC   DLSELECT,ACQSEL                                                  
         MVC   DLTTYPE,ACQTTYPE                                                 
         MVC   DLOPTNS,ACQOPT1                                                  
         MVC   DLNAME,ACQESTOR                                                  
*                                                                               
         LA    R1,BVROPTH                                                       
         CLI   5(R1),0                                                          
         BE    *+16                                                             
         MVC   DLNAME,SPACES                                                    
         MVC   DLNAME(L'SVCPLOGO),SVCPLOGO  INSTEAD OF NAME PUT ID              
*                                                                               
         MVC   DLREV,ACQREVOP                                                   
         MVC   DLSTAT,ACQTRNST                                                  
         OC    DLINE,SPACES                                                     
         MVC   DLPTR,=C'->'                                                     
         NI    DLCANCH+1,X'DF'     UNPROTECT CANCEL FIELD                       
*                                                                               
         OI    DLPTRH+6,X'80'      TRANSMIT ALL                                 
         OI    DLCANCH+6,X'80'                                                  
         OI    DLINEH+6,X'80'                                                   
         OC    ACQCARD2,ACQCARD2                                                
         BZ    *+14                                                             
         OC    DLINE2,SPACES                                                    
         OI    DLINE2H+6,X'80'                                                  
         CLI   STATUS,3                                                         
         BNE   ENQDISPX            GET OUT NOW IF NOT DISPLAY                   
*                                                                               
         LA    R4,DLNEXT           BUMP TO NEXT LINE                            
         OC    DLINE,SPACES                                                     
         CLC   DLINE,SPACES  DO WE ALREADY HAVE SOMETHING ON NEXT LINE          
         BE    ENQD120                                                          
         OI    DLCANCH+1,X'20'     YES - PROTECT CANCEL FIELD ON IT             
         LA    R4,DLNEXT           AND BUMP TO NEXT LINE                        
         LH    R6,DISPMAX          DECREASE MAX ALLOWED BY ONE EXTRA            
         BCTR  R6,0                                                             
         STH   R6,DISPMAX                                                       
*                                                                               
ENQD120  ST    R4,DISPADR                                                       
         LH    R6,DISPCTR          UPDATE DISPLAY COUNTER                       
         LA    R6,1(R6)                                                         
         CHI   R6,1                                                             
         BNE   ENQD130                                                          
         CLC   REQINCR,=X'0001'                                                 
         BNE   ENQD130                                                          
         MVC   DISPFLDS+2(2),READCTR                                            
*                                                                               
ENQD130  STH   R6,DISPCTR                                                       
         STH   R6,DISPFLDS                                                      
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   0(4,R6),ADR         SAVE DISK ADR                                
*                                                                               
ENQDISPX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHECK IF ID AUTHORIZED TO CANCEL/AMEND CHECKS          *         
***********************************************************************         
*                                                                               
*CHKAUTH  NTR1                                                                  
*         USING CHARECD,RF                                                      
*         LA    RF,KEY                                                          
*         XC    CHAKEY,CHAKEY                                                   
*         MVI   CHAKTYP,CHAKTYPQ    X'10' CHECK AUTHORIZATION REC               
*         MVC   CHAKCULA,ACQCPY                                                 
*         MVC   LKEY,KEY                                                        
*         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'ACCOUNT',KEY,AIO1                      
*         CLI   DMCB+8,0                                                        
*         BNE   CHKNO                                                           
*                                                                               
*CHK100   L     R6,AIO1             MUST HAVE X'54' ELEM FOR THIS ID TO         
*         AH    R6,DATADISP         CANCEL CHECK REQUEST                        
*CHK200   CLI   0(R6),X'54'                                                     
*         BE    CHK400                                                          
*         CLI   0(R6),0                                                         
**        BE    CHK320              IF END OF THIS REC GET NEXT REC             
*CHK300   SR    R1,R1                                                           
*         IC    R1,1(R6)                                                        
*         AR    R6,R1                                                           
*         B     CHK200                                                          
*CHK320   GOTO1 DATAMGR,DMCB,DMRSEQ,=CL8'ACCOUNT',AIO1,AIO1                     
*         L     R6,AIO1             READ NEXT REC FOR ID INFO                   
*         CLC   LKEY(4),0(R6)                                                   
*         BE    CHK100              CHECK THROUGH NEXT RECORD                   
*         B     CHKNO               ELSE ERROR                                  
*         USING OCNELD,R6                                                       
*CHK400   CLC   OCNOFFID,TWAUSRID                                               
*         BNE   CHK300                                                          
*         TM    OCNSTAT,OCNSAUTH    AUTHORIZATION REQUIRED                      
*         BZ    CHKYES                                                          
*         TM    TWAAUTH,X'01'                                                   
*         BO    CHKYES                                                          
**                                                                              
*CHKNO    LTR   RB,RB               CC=NOT EQUAL                                
*         B     EXIT                                                            
*CHKYES   CR    RB,RB               CC=EQUAL                                    
*         B     EXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              CHANGE REQUEST CANCEL STATUS                           *         
***********************************************************************         
*                                                                               
CANREQ   LA    R4,ENQFRSTH         R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         ST    R4,DISPADR                                                       
         LH    R5,DISPFLDS         R5=NUM OF TWA LINES                          
         LA    R6,DISPFLDS+4       R6=A(DISK ADR)                               
         LTR   R5,R5                                                            
         BNZ   CANR1                                                            
         DC    H'0'                                                             
*                                                                               
CANR1    TM    DLCANCH+4,X'C0'    ANY INPUT IN CANCEL FIELD                     
         BZ    CANR6                                                            
         MVC   ADR,0(R6)                                                        
*                                                                               
         L     R0,AIO2                                                          
         LH    R1,=Y(L'REQREC)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMIN,DMRDIR),REQUEST,ADR,AIO2                      
         LA    R0,REQREC                                                        
         LH    R1,=Y(L'REQREC)                                                  
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REQ RECORD IO2 -> REQREC                
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
*                                                                               
         CLI   DLCANC,C'U'                                                      
         BE    CANR1A                                                           
         CLI   DLCANC,0                                                         
         BE    CANR1A                                                           
         CLI   DLCANC,C' '                                                      
         BNE   CANR1B                                                           
CANR1A   CLC   ACQPROG,=C'99'      UNCANCELL REQUIRED                           
         BNE   CANR6                                                            
         MVC   DUB(1),REQNUMB                                                   
         BRAS  RE,GETREQID                                                      
         MVC   ACQPROG,DUB+1                                                    
         MVI   DLCANC,C' '                                                      
         B     CANR4                                                            
*                                                                               
CANR1B   CLI   DLCANC,C'A'                                                      
         BE    CANR3                                                            
         CLI   DLCANC,C'C'                                                      
         BNE   CANR1C                                                           
         CLC   ACQPROG,=C'99'      CANCELL REQUIRED                             
         BE    CANR6                                                            
         MVC   ACQPROG,=C'99'                                                   
         B     CANR4                                                            
*                                                                               
CANR1C   MVC   FERN,=AL2(INVINPT)  INVALID CODE                                 
         ST    R4,FADR                                                          
         B     EXIT                                                             
*                                                                               
CANR3    CLI   DDS,1               * PATCH TO BNE TO ALLOW AMEND *              
         B     CANR1C                                                           
         CLC   ACQPROG,=C'99'      CAN ONLY AMEND ACTIVE REQ                    
         BE    CANR1C                                                           
*                                                                               
CANR3B   MVC   ACQUNT(14),DLUNIT   OK TO CHANGE U/L/ACCOUNT                     
         MVC   ACQSTART(13),DLSTRD              START/END DATES/SORT            
         MVC   ACQOPT1(7),DLOPTNS               OPTIONS                         
         MVC   ACQESTOR(L'DLNAME),DLNAME        REQUESTOR                       
         MVC   ACQREVOP,DLREV                                                   
         MVC   ACQMOSST(8),DLMOSTRT             MOS                             
         MVC   ACQCNTRA(14),DLCONTRA            CONTRA U/L/ACCOUNT              
         MVC   ACQACTST(6),DLACTSTR             ACTIVITY START DATE             
         MVC   ACQACTND(6),DLACTEND             ACTIVITY END DATE               
         MVC   ACQOPT8,DLOPT8                   OPTION #8                       
         MVC   ACQREPSQ,DLSEQ                   SEQUENCE                        
         MVC   ACQCOMNT(6),DLCOMMNT             COMMENT CODE                    
*                                                                               
CANR4    GOTO1 DATAMGR,DMCB,(DMIN,DMWRT),REQUEST,ADR,REQREC                     
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
         BAS   RE,ENQDISP                                                       
         B     CANR6                                                            
*                                                                               
CANR5    MVC   FERN,=AL2(0)        DISK ERROR ON UPDATE                         
         ST    R4,FADR                                                          
         B     EXIT                                                             
*                                                                               
CANR6    LA    R4,DLNEXT           BUMP TO NEXT LINE                            
         TM    DLCANCH+1,X'20'                                                  
         BZ    *+8                                                              
         LA    R4,DLNEXT           SKIP ANOTHER IF CANCEL FIELD PROT.           
         ST    R4,DISPADR                                                       
         LA    R6,4(R6)            BUMP TO NEXT DISK ADR                        
         BCT   R5,CANR1                                                         
*                                                                               
CANRX    LA    R0,TEMP             CLEAR TEMP AREA                              
         LH    R1,=Y(L'TEMP)                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TEMP(29),=C'Request cancel status amended'                       
         B     ENQRX                                                            
         EJECT                                                                  
***********************************************************************         
*              DISPLAY TOTAL REQUEST COUNTS                           *         
***********************************************************************         
*                                                                               
TOTREQ   XC    TOTCTR(256),TOTCTR                                               
         XC    TOTCTR+256(256),TOTCTR+256                                       
         XC    ADR,ADR                                                          
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(DMIN,DMRSEQ),REQUEST,ADR,(R6),AIO1                 
*                                                                               
TOTR1    GOTO1 DATAMGR,DMCB,(DMIN,DMRSEQ),REQUEST,ADR,(R6),AIO1                 
         CLI   DMCB+8,0                                                         
         BE    TOTR2                                                            
         TM    DMCB+8,X'80'                                                     
         BO    TOTR3                                                            
         B     REQIOERR                                                         
*                                                                               
TOTR2    CLC   ACQPROG-ACQD(L'ACQPROG,R6),=X'FFFF'   IGNORE DUMMYS              
         BE    TOTR1                                                            
         CLC   ACQPROG-ACQD(L'ACQPROG,R6),=C'99'     IGNORE CANCELLED           
         BE    TOTR1                                                            
*                                                                               
         CLI   LREQOHDR+35,X'00'                                                
         BE    *+14                                                             
         CLC   ACQCPY-ACQD(L'ACQCPY,R6),LREQOHDR+35  FILTER ON COMPANY          
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR(4),LREQOHDR                                             
         BZ    *+14                                                             
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE                             
         BNE   TOTR1                                                            
*                                                                               
         TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   ACQESTOR-ACQD(L'ACQESTOR,R6),LREQOHDR+92                         
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    *+14                                                             
         CLC   REQOUT,LREQOHDR+4   FILTER ON OUTPUT TYPE                        
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    *+14                                                             
         CLC   REQDEST,LREQOHDR+11 FILTER ON DESTINATION                        
         BNE   TOTR1                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,REQNUMB                                                       
         SLL   RE,1                                                             
         LA    RF,TOTCTR(RE)       POINT TO COUNTER AND BUMP                    
         LH    R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,0(RF)                                                         
         B     TOTR1                                                            
*                                                                               
TOTR3    SR    R5,R5               DISPLAY COUNTERS ON MENU SCREEN              
         LA    R6,TOTCTR                                                        
TOTR4    CH    R5,=H'256'                                                       
         BE    TOTRX                                                            
         STC   R5,DUB                                                           
         BRAS  RE,GETREQID         SEARCH REQTBL FOR BINARY REQ NUM             
         CLI   DUB+3,0                                                          
         BE    TOTR7               NOT IN REQTBL                                
*                                                                               
         LA    R1,3                3 ENTRIES PER SCREEN FIELD                   
         ZAP   HALF,=P'0'                                                       
         LA    RE,BVRFRSTH                                                      
         SR    RF,RF                                                            
TOTR5    CP    HALF,=P'54'         SEARCH SCREEN FOR ALPHA REQ ID               
         BH    TOTR7               END OF SCREEN                                
         CLC   8(2,RE),DUB+1                                                    
         BNE   TOTR6                                                            
         LH    R0,0(R6)            MOVE COUNT TO SCREEN FIELD                   
         CVD   R0,DUB                                                           
         UNPK  10(4,RE),DUB+5(3)                                                
         OI    13(RE),X'F0'                                                     
         CLI   10(RE),C'0'                                                      
         BNE   *+8                                                              
         MVI   10(RE),C'='                                                      
         LTR   R0,R0                                                            
         BNZ   TOTR7                                                            
         MVC   11(3,RE),=C'...'                                                 
         B     TOTR7                                                            
TOTR6    LA    RE,27(RE)           BUMP SCREEN FIELD                            
         AP    HALF,=P'1'                                                       
         BCT   R1,TOTR5                                                         
         LA    RE,6(RE)            NEW SCREEN LINE                              
         LA    R1,3                                                             
         B     TOTR5                                                            
*                                                                               
TOTR7    LA    R5,1(R5)            BUMP REQNUM                                  
         LA    R6,2(R6)            BUMP TABLE                                   
         B     TOTR4                                                            
*                                                                               
TOTRX    XC    TEMP(60),TEMP                                                    
         MVC   TEMP(30),=C'Total request counts displayed'                      
         LA    R1,BVRNAMEH                                                      
         OC    NEXTAD,NEXTAD       ANY MORE TO COME                             
         BZ    *+14                                                             
         MVC   TEMP+31(20),=C'- hit enter for next'                             
         LA    R1,BVRDESTH                                                      
         ST    R1,FADR                                                          
         B     ENQRX                                                            
         EJECT                                                                  
***********************************************************************         
*              ADD NEW REQUEST TO REQUEST FILE                        *         
***********************************************************************         
*                                                                               
NEWREQ   DS    0H                                                               
*&&UK                                                                           
         MVC   RQHFLG1,RTNTYP       SET SECURITY DATA TYPE                      
         MVC   RQHSECD(L'PIDN),PIDN       SET SECURITY DATA                     
*&&                                                                             
         LA    R0,NEWTBLQ                                                       
         LA    R1,NEWTBL                                                        
NEWREQ0  CLC   ACQPROG,0(R1)                                                    
         BE    NEWREQ1                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,NEWREQ0                                                       
         B     NEWREQ3                                                          
*                                                                               
NEWREQ1  MVC   FERN,=AL2(NOTREQ)                                                
         B     CLEARADR                                                         
*                                                                               
NEWREQ3  DS    0H                                                               
         GOTO1 ASETREQ             SET REMAINING REQUEST RECORD DETAILS         
         OC    OUTSTAT,OUTSTAT                                                  
         BNZ   SAVEADR                                                          
         CLC   =C'SOON,',BVROUT    IF THIS IS A SOON REQUEST                    
         BNE   NEWREQ6                                                          
         CLC   ACQPROG,=C'21'                                                   
         BE    NEWREQ4                                                          
         CLC   ACQPROG,=C'23'                                                   
         BE    NEWREQ4                                                          
         CLC   ACQPROG,=C'27'                                                   
         BE    NEWREQ4                                                          
         CLC   ACQPROG,=C'29'                                                   
         BE    NEWREQ4                                                          
         CLC   ACQPROG,=C'55'                                                   
         BNE   NEWREQ5                                                          
*                                                                               
NEWREQ4  L     R4,DATAMGR          CHECK TO SEE IF FACWK HAS AN                 
         GOTO1 VCWKSCAN,DMCB,(R4)    AVAILABLE INDEX                            
         CLI   0(R1),0             IF ZERO, FILE IS NOT FULL                    
         BE    NEWREQ5                                                          
*        MVC   BVRHDR,SPACES                                                    
*        MVC   BVRHDR(31),=CL31'** File Full - Stop call DDS **'                
         MVC   FERN,=AL2(ENDOFILE)                                              
         B     CLEARADR                                                         
*                                                                               
NEWREQ5  GOTO1 =A(GENSOON),DMCB,(RC),RR=RELO GO PROCESS IT                      
         B     CLEARADR                      AND EXIT                           
*                                                                               
NEWREQ6  TM    RFPSTAT,RFPINUSE    ADDING REQUEST TO GROUP                      
         BZ    NEWREQ7                                                          
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPRADD                                                
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    CLEARADR                                                         
         MVC   BVRHDR,SPACES                                                    
         MVC   BVRHDR(60),=CL60'Request Cannot Be Added to a Submitted X        
               Group'                                                           
         MVC   FERN,=AL2(FE)                                                    
         CLI   QRFPMODE,QRFPNORO                                                
         BNE   CLEARADR                                                         
         MVC   BVRHDR(60),=CL60'Request Cannot Be Added - maximum # of X        
               requests is 50'                                                  
         B     CLEARADR                                                         
*                                                                               
NEWREQ7  DS    0H                                                               
         MVC   ADR,=X'AAAAAAAA'    SET TO ADD NEW REQUEST RECORD                
         GOTO1 DATAMGR,DMCB,(DMIN,DMADD),REQUEST,ADR,REQREC,,L'REQREC           
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
*                                                                               
NEWTBL   DC    C'RLRPILIPXLXPPLPPVLVP'  LIST OF RPTS NOT REQUESTABLE            
NEWTBLQ  EQU   (*-NEWTBL)/2                                                     
*                                                                               
* READ LAST REQUEST RECORD AND UPDATE IT WITH AMENDED DATA                      
*                                                                               
AMDREQ   MVC   ADR,LADR                                                         
         OC    ADR,ADR                                                          
         BZ    AMDERR                                                           
         GOTO1 DATAMGR,DMCB,(DMIN,DMRDIR),REQUEST,ADR,TEMP,,L'REQREC            
         CLI   DMCB+8,0                                                         
         BNE   AMDERR                                                           
         MVC   REQREC(80),TEMP     KEEP HEADER FROM READ                        
         GOTO1 ASETREQ                                                          
         CLC   REQREC(80),TEMP     IF HEADER HAS CHANGED                        
         BNE   AMDERR              THEN CANNOT AMEND THIS REQUEST               
         GOTO1 DATAMGR,DMCB,(DMIN,DMWRT),REQUEST,ADR,REQREC,,L'REQREC           
         CLI   DMCB+8,0                                                         
         BNE   AMDERR                                                           
         B     SAVEADR                                                          
*                                                                               
AMDERR   MVC   BVRHDR,SPACES                                                    
         MVC   FERN,=AL2(NOAMEND)                                               
         B     CLEARADR                                                         
*                                                                               
**********************************************************************          
* COMMON DOWNLOAD CHECK                                              *          
**********************************************************************          
XVCOMDL  CLI   ACQOPT7,C'Y'        COMMON DOWNLOAD CHECK FOR OUTTYPE            
         BNER  RE                                                               
         MVC   REQOUT(4),=C'DOWN'                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
DATADISP DC    H'49'                                                            
REQUEST  DC    CL8'ACCREQ'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
ACCOUNT  DC    CL8'ACCOUNT'                                                     
*                                                                               
PHASES   DS    0X                                                               
         DC    AL1(QGETOPT)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATION ROUTINE ADDRESS TABLE                       *         
***********************************************************************         
*                                                                               
VROUTS   DC    A(0)                                                             
         DC    A(VR01)                                                          
         DC    A(VR02)                                                          
         DC    A(VR03)                                                          
         DC    A(VR04)                                                          
         DC    A(VR05)                                                          
         DC    A(VR06)                                                          
         DC    A(VR07)                                                          
         DC    A(VR08)                                                          
         DC    A(VR09)                                                          
         DC    A(VR10)                                                          
         DC    A(VR11)                                                          
         DC    A(VR12)                                                          
         DC    A(VR13)                                                          
         DC    A(VR14)                                                          
         DC    A(VR15)                                                          
         DC    A(VR16)                                                          
         DC    A(VR17)                                                          
         DC    A(VR18)                                                          
         DC    A(VR19)                                                          
         DC    A(VR20)                                                          
         DC    A(VR21)                                                          
         DC    A(VR22)                                                          
         DC    A(VR23)                                                          
         DC    A(VR24)                                                          
         DC    A(VR25)                                                          
         DC    A(VR26)                                                          
         DC    A(VR27)                                                          
         DC    A(VR28)                                                          
         DC    A(VR29)                                                          
         DC    A(VR30)                                                          
         DC    A(VR31)                                                          
         DC    A(VR32)                                                          
         DC    A(VR33)                                                          
         DC    A(VR34)                                                          
         DC    A(VR35)                                                          
         DC    A(VR36)                                                          
         DC    A(VR37)                                                          
         DC    A(VR38)                                                          
         DC    A(VR39)             SPARE                                        
         DC    A(VR40)                                                          
         DC    A(VR41)             SPARE                                        
         DC    A(VR42)                                                          
         DC    A(VR43)                                                          
         DC    A(VR44)                                                          
         DC    A(VR45)                                                          
         DC    A(VR46)                                                          
         DC    A(VR47)                                                          
         DC    A(VR48)                                                          
         DC    A(VR49)                                                          
         DC    A(VR50)                                                          
         DC    A(VR51)                                                          
         DC    A(VR52)                                                          
         DC    A(VR53)             BRANCHED TO FROM CHKIT ROUTINE               
         DC    A(VR54)                                                          
         DC    A(VR55)                                                          
         DC    A(VR56)                                                          
         EJECT                                                                  
***********************************************************************         
* GENERATE SOON REQUEST                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING SPOOK,R6                                                         
GENSOON  DS    0D                                                               
         NMOD1 0,GENSOON,RR=R5                                                  
         L     RC,0(R1)                                                         
         CLC   CMPY,ACQCPY         TRAP BAD REQUESTS ON THE WAY OUT             
         BE    *+6                                                              
         DC    H'0'                HEXCOMP IS CREAMED - CHECK FOR MORE          
         XC    TEMP(SPOOKXL),TEMP  BUILD SPOOK BLOCK                            
         LA    R6,TEMP                                                          
         MVC   SPOOKXT,=C'XT='     SET EXTRA LENGTH                             
         MVC   SPOOKUID,USRID      CONNECT ID                                   
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
         MVC   SPOOKAGX,CMPY       HEXCOMP                                      
         MVC   SPOOKDID,BVROUT+5   USER INITIALS (ID)                           
         TM    OUTSTAT,RUNQ                                                     
         BZ    *+10                                                             
         MVC   SPOOKDID,BVROUT+4                                                
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
         MVC   SPOOKSYS,=C'AC'     ACCOUNT SYSTEM                               
         MVC   SPOOKEOD,ACQPROG                                                 
         MVC   SPOOKJCL,ACQPROG                                                 
         MVI   SPOOKWEN,2          SET SOON STATUS                              
         CLC   ACQPROG,=C'55'      SET HIGH PRIORITY FOR CHECKS                 
         BNE   *+8                                                              
         MVI   SPOOKPR2,7                                                       
*                                                                               
         CLC   ACQPROG,=C'AA'      TEST FOR UPDATIVE                            
         BNE   *+8                                                              
         MVI   SPOOKWEN,5          SET UPDATIVE SOON                            
*                                                                               
         CLC   ACQPROG,=C'21'      PROD BILLing                                 
         BE    GENS10                                                           
         CLC   ACQPROG,=C'22'                                                   
         BE    GENS10                                                           
         CLC   ACQPROG,=C'23'                                                   
         BE    GENS10                                                           
         CLC   ACQPROG,=C'24'                                                   
         BNE   GENS20                                                           
*                                                                               
GENS10   TM    COMPSTAA,CPYSADBL   TEST USES NEW BILLING                        
         BNO   GENS20                                                           
*                                                                               
         OI    SPOOKIND,SPOOKINB   SET USES NEW BILLING                         
* CALL OVERLAY 6 TO DETERMINE IF JOB IS BILLABLE                                
*  LOCK JOB AND ASSIGN BILL NUMBER                                              
         MVI   OLAYNUM,6                                                        
         MVI   ROUTNUM,0                                                        
         GOTO1 AOLAY                                                            
         GOTO1 ASETREQ             RESET REQUEST                                
         CLC   FERN,=AL2(FF)       TEST OK                                      
         BE    GENS15              YES, CONTINUE                                
         CLI   ROUTNUM,0                                                        
         BNE   GENSXIT                                                          
         MVI   ROUTNUM,15          CURSOR TO JOB FIELD                          
         B     GENSXIT             NO, ERROR MESSAGE HAS BEEN SET               
*                                                                               
GENS15   CLC   ACQPROG,=C'21'                                                   
         BE    GENS30                                                           
         CLC   ACQPROG,=C'23'                                                   
         BE    GENS30                                                           
*                                                                               
GENS20   CLC   ACQPROG,=C'27'                                                   
         BE    GENS30                                                           
         CLC   ACQPROG,=C'29'                                                   
         BNE   GENS31                                                           
*                                                                               
GENS30   MVI   SPOOKWEN,5          SET UPDATIVE SOON                            
         OI    SPOOKSTA,X'02'      SET REPORT STATUS(INVISIBLE)                 
GENS31   L     R4,APARM                                                         
         L     R4,16(R4)           A(COMFACS)                                   
         GOTO1 REQTWA,DMCB,(5,(R3)),REQREC+54,DATAMGR,(R4),(R6)                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DUMP IF ERROR                                
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(L'SOONMSG1),SOONMSG1                                      
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   BVRHDR+7(3),2(RE)                                                
         SR    RF,RF                                                            
         ICM   RF,3,6(RE)                                                       
         LA    R4,BVRHDR+L'SOONMSG1                                             
         EDIT  (RF),(5,(R4)),ALIGN=LEFT,WRK=TEMP+60                             
         AR    R4,R0                                                            
         MVC   0(L'SOONMSG2,R4),SOONMSG2                                        
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
GENSXIT  XIT1                                                                   
*                                                                               
SOONMSG1 DC    C'Report XXX,'                                                   
SOONMSG2 DC    C' will be processed soon'                                       
*                                                                               
         LTORG                                                                  
         DROP  R2,R7,RA,RB                                                      
         EJECT                                                                  
***********************************************************************         
*              CONVERT BINARY REQNUM IN DUB(1) TO ALPHA IN DUB+1(2)   *         
***********************************************************************         
*                                                                               
GETREQID NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB                                                           
         STC   R0,DUB                                                           
         UNPK  DUB+1(2),DUB+6(2)                                                
         OI    DUB+2,X'F0'                                                      
         MVI   DUB+3,0             SET NOT FOUND FLAG AND NUMBER VALUE          
*                                                                               
         L     R6,AREQTBL                                                       
         USING HDRD,R6                                                          
GETRID1  OC    HDRLEN,HDRLEN         SEARCH REQTBL FOR BINARY REQ NUM           
         BZ    GETRIDX                                                          
         CLC   DUB(1),HDRNUM                                                    
         BE    GETRID2                                                          
         SR    R1,R1                                                            
         ICM   R1,3,HDRLEN                                                      
         AR    R6,R1                                                            
         B     GETRID1                                                          
*                                                                               
GETRID2  ST    R6,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,HDRLEN       POINT TO LAST TWO BYTES OF ENTRY               
         AR    R6,R1                                                            
         SH    R6,=H'2'                                                         
         MVC   DUB+1(2),0(R6)      RETURN REQ ALPHA ID                          
         DROP  R6                                                               
*                                                                               
GETRIDX  J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #31                            *         
***********************************************************************         
*                                                                               
VR31     NMOD1 0,**VR31**                                                       
         CLI   ACQXJOB,C'O'        INPUT TO XJOB FIELD ONLY ALLOWED             
         BNE   VR31A               WHEN OPT1=J OR B                             
         CLI   ACQOPT1,C'J'                                                     
         BE    VR31A                                                            
         CLI   ACQOPT1,C'B'                                                     
         BE    VR31A                                                            
         MVI   ROUTNUM,11          CURSOR TO OPTION 1                           
         MVC   FERN,=AL2(INVINPT)                                               
         CLI   ACQOPT1,C' '                                                     
         BNE   *+10                                                             
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VR31X                                                            
*                                                                               
VR31A    CLI   ACQSORT,C'T'        IF SORTING ON TAX-ID                         
         BNE   VR31X                                                            
         CLC   ACQUNT(2),=C'2C'    MUST BE U/L=2C                               
         BE    VR31X                                                            
         CLC   ACQUNT(2),=C'SV'    OR BE U/L=SV                                 
         BE    VR31X                                                            
         MVI   ROUTNUM,9           CURSOR TO SORT OPTION                        
         MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
*                                                                               
VR31X    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              POST VALIDATION ROUTINE #36                            *         
***********************************************************************         
*                                                                               
VR36     NMOD1 0,**VR36**                                                       
         MVI   ROUTNUM,2           MOVE CURSOR TO LEDGER                        
         CLI   ACQLDG,C'J'                                                      
         BE    VR36#10                                                          
         CLI   ACQLDG,C'K'                                                      
         BNE   VR36#20                                                          
*                                                                               
VR36#10  MVI   ROUTNUM,18          MOVE CURSOR TO OPT2                          
         CLI   ACQOPT2,C'P'        IF OPT2=P                                    
         BNE   VR36X                                                            
         CLI   ACQLDG,C'J'         THEN LEDGER MUST BE J                        
         BE    *+10                                                             
*                                                                               
VR36#20  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
VR36X    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
ASETLOCK DS    A                   A(SETLOCK)                                   
DISPADR  DS    F                   A(NEXT DISP LINE ON SCR)                     
DISPMAX  DS    H                   N'LINES AVAILABLE FOR DISPLAY                
SKIPCTR  DS    H                   NUM OF RECS SKIPPED                          
READCTR  DS    H                   NUM OF RECS READ                             
CANCCTR  DS    H                   NUM OF RECS CANCELLED                        
DISPCTR  DS    H                   NUM OF RECS DISPLAYED                        
TOTCTR   DS    256H                                                             
LKDATA   DS    CL16                LK PROFILE HOLD AREA                         
PROFDATA DS    CL16                DATA TO PASS TO GETPROF                      
WORK     DS    CL60                                                             
VCWKSCAN DS    V                   V(ACWKSCAN)                                  
VGETOPT  DS    A                   V(GETOPT)                                    
GOBDATA  DS    XL(GOLNQ)                                                        
LWSX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*              DISPLAY LINE DSECT                                     *         
***********************************************************************         
*                                                                               
DISPLD   DSECT                                                                  
DLPTRH   DS    CL8                                                              
DLPTR    DS    CL2                                                              
DLCANCH  DS    CL8                                                              
DLCANC   DS    CL1                                                              
DLINEH   DS    CL8                                                              
DLINE    DS    0CL73                                                            
DLNUM    DS    CL2                                                              
         DS    CL1                                                              
DLCMPY   DS    CL2                                                              
DLUNIT   DS    CL1                                                              
DLLDGR   DS    CL1                                                              
DLACNO   DS    CL12                                                             
DLLGRP   DS    CL3                                                              
DLBGRP   DS    CL3                                                              
DLFTR1   DS    CL1                                                              
DLFTR2   DS    CL1                                                              
DLFTR3   DS    CL1                                                              
DLFTR4   DS    CL1                                                              
DLFTR5   DS    CL1                                                              
DLMEDIA  DS    CL1                                                              
DLBILTYP DS    CL1                                                              
DLSTRD   DS    CL6                                                              
DLENDD   DS    CL6                                                              
DLSORT   DS    CL1                                                              
         DS    CL1                                                              
DLSELECT DS    CL6                                                              
DLTTYPE  DS    CL3                                                              
DLOPTNS  DS    CL7                                                              
DLNAME   DS    CL9                                                              
DLREV    DS    CL1                                                              
DLSTAT   DS    CL1                                                              
DLNEXT   EQU   *                   A(NEXT LINE ON SCREEN)                       
         ORG   *+DLINEH-DISPLD     SECOND REQUEST CARD STARTS HERE              
DLINE2H  DS    CL8                                                              
DLINE2   DS    0CL73                                                            
         DS    CL1                                                              
DLSEQ    DS    CL1                                                              
DLXJOB   DS    CL1                                                              
DLSRTA   DS    CL7                                                              
DLMOSTRT DS    CL4                                                              
DLMOSEND DS    CL4                                                              
DLCONTRA DS    CL14                                                             
         DS    CL4                 USED ABOVE FOR CONTRA                        
DLACTSTR DS    CL6                                                              
DLACTEND DS    CL6                                                              
DLOPT8   DS    CL1                                                              
         DS    CL3                                                              
DLOFFICE DS    CL2                                                              
DLCOMMNT DS    CL6                                                              
DLCALSTR DS    CL6                                                              
DLCALEND DS    CL6                                                              
         EJECT                                                                  
***********************************************************************         
*              GETPROFS KEY DSECT                                     *         
***********************************************************************         
*                                                                               
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OLD TALENT GUARANTEE RECORD TYPE.                    *         
* COPIED FROM ACGENBOTH.                                              *         
* NO EQUIVALENT DSECT IN ACGENFILE.                                   *         
* REMOVE WHEN GUARANTEE ROUTINE IS REMOVED.                           *         
***********************************************************************         
ACGUD    DSECT                                                                  
ACGUKEY  DS    0CL42                                                            
ACGUCODE DS    CL1       B         RECORD TYPE X'24'                            
ACGUEQU  EQU   X'24'                                                            
ACGUAGNC DS    CL1       B         SIGN-ON CODE (IE. DPS X'7B')                 
ACGUCOMP DS    CL1       B         COMPANY CODE(IE. WEC X'F6')                  
ACGUEMP  DS    CL3       C         EMPLOYER CODE                                
ACGUSSN  DS    CL9       C         PERFORMER S/S                                
ACGUUL   DS    CL2       C         COMMERCIAL UNIT/LEDGER                       
ACGUCLI  DS    CL3       C         CLIENT CODE                                  
ACGUSEQ  DS    CL1       B         SEQUENCE NUMBER                              
         DS    CL21                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACREQWORK                                                      
GOBLOCKD DSECT                                                                  
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
GOLNQ    EQU   *-GOBLOCKD                                                       
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ACREQ02   01/15/21'                                      
         END                                                                    
