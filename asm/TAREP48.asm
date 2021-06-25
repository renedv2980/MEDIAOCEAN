*          DATA SET TAREP48    AT LEVEL 102 AS OF 02/04/10                      
*PHASE T70348A,*                                                                
         TITLE 'T70348 - QUESTIONNARE REPORT'                                   
T70348   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70348,R8,R7                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         LA    R6,BUFF                                                          
         LA    R6,8(R6)                                                         
         USING QUESD,R6            R6=A(LOCAL W/S)                              
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,PRINTREP       RECOGNIZE ONLY MODE PROCESS REPORT           
         BNE   XIT                                                              
         BAS   RE,PREP             PRINT REPORT                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         BAS   RE,INIT                                                          
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         BAS   RE,FLUSHWSP         FLUSH TABLES FROM LAST COMML.                
*                                                                               
         BAS   RE,FLUSHPGM                                                      
*                                                                               
         BAS   RE,FLUSHCLA                                                      
*                                                                               
         BAS   RE,FLSHRUSE                                                      
*                                                                               
         BAS   RE,FLSHRWSP                                                      
*                                                                               
         BAS   RE,PRTREP           PRINT REPORT                                 
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              INITIAL                                                          
*                                                                               
INIT     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R2,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R2,SPECS                                                         
         LA    R2,HOOK             SET A(HEADLINE HOOK)                         
         ST    R2,HEADHOOK                                                      
         DROP  R5                                                               
*                                                                               
         L     RE,TWADCONS         SAVE ADDRESS OF TWADCON ROUTINES             
         USING TWADCOND,RE                                                      
         MVC   BINSRCH,TBINSRCH                                                 
         DROP  RE                                                               
*                                                                               
         USING BIND,R3                                                          
         L     R3,=A(BINLIST)                                                   
         XC    BININ,BININ         RESET BINSRCH TABLE                          
         DROP  R3                                                               
*                                                                               
         BAS   RE,CLRTABS          CLEAR COML LEVEL TABLES                      
         ZAP   COMMCNT,=P'0'       CLEAR COMMERCIAL COUNT                       
         MVC   WSPLAST,=C'000'                                                  
         MVC   CLALAST,=C'   1'                                                 
         MVI   LSTWSPCD,X'70'      FIRST WSP CODE                               
         CLI   TIFMED,C'R'         IF REQUESTING RADIO                          
         BNE   *+8                                                              
         MVI   LSTWSPCD,X'C0'      FIRST RADIO WSP CODE                         
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCOCDQ      READ COMMERCIAL RECORDS                      
         MVI   TISUBRD,TLINHCDQ    SUBREAD INV RECORDS                          
         OI    TIQFLAG2,TIQFHCK+TIQFHCA   & CHECKS * CAST                       
*                                                                               
         MVC   TIFCOM,MYCOM                                                     
         MVC   TIFCID,MYCID                                                     
*                                                                               
         OI    TIFINSTN,TAINSCIN+TAINSCAN  IGNORE CANCELLED INVOICES            
         OI    TIFPDSN,TAPDSCAN            IGNORE CANADIAN INVOICES             
**IRV    OI    TIFPDPN,TAPDPCRD+TAPDPBNP   IGNORE CREDIT + BNP                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS RECORDS FROM SYSIO                                       
*                                                                               
IOHOOK   NTR1                                                                   
         L     R4,TIAREC           R4=A(FILE RECORD)                            
         USING TLRCD,R4                                                         
*                                                                               
         CLI   TLRCCD,TLCOCDQ      IF COMMERCIAL RECORD                         
         BNE   IOHK20                                                           
         BAS   RE,PROCCOM          PROCESS COMMERCIAL RECORD                    
         B     IOHKX                                                            
*                                                                               
IOHK20   BAS   RE,PROCRECD         PROCESS ALL OTHER RECORDS                    
*                                                                               
IOHKX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS COMMERCIAL RECORDS                            
*                                                                               
         USING TLCOD,R4            R4=A(COMMERCIAL RECORD)                      
PROCCOM  NTR1                                                                   
         CLI   TICTYPE,CTYADD      IGNORE ADDENDUMS                             
         BE    PROCC10                                                          
         CLI   TICTYPE,CTYMUS      IGNORE MUSIC                                 
         BE    PROCC10                                                          
         CLI   TICTYPE,CTYSOAP     IGNORE SOAPS                                 
         BE    PROCC10                                                          
         BAS   RE,TSTFILTS         TEST FILTERS                                 
         BE    PROCC20                                                          
*                                                                               
PROCC10  MVI   TIMODE,PROCNOIN     DON'T PASS INVOICES                          
         B     PROCCX                                                           
*                                                                               
PROCC20  DS    0H                                                               
         BAS   RE,FLUSHWSP         FLUSH TABLES FROM LAST COMML.                
         BAS   RE,FLUSHPGM                                                      
         BAS   RE,FLUSHCLA                                                      
         BAS   RE,FLSHRUSE                                                      
         BAS   RE,FLSHRWSP                                                      
*                                                                               
         BAS   RE,CLRTABS          CLEAR COML LEVEL TABLES                      
         MVI   COUNTED,C'N'                                                     
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'COMML REC',TIAREC,0                              
         BAS   RE,RESET            RESET TABLE FOR NEW COMMERCIAL               
         MVC   SVMED,TIMED         SAVE THE MEDIA                               
PROCCX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              TEST SCREEN FILTERS                                              
*                                                                               
TSTFILTS NTR1                                                                   
         L     R4,TIAREC           GET TACO ELEMENT                             
         MVI   ELCODE,TACOELQ                                                   
         USING TACOD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   TFNO                                                             
         CLC   TACOPDTE,TIQPSTR    IF LAST PAY DATE BEFORE REQ START            
         BL    TFNO                THEN SKIP COMML                              
*                                                                               
         CLI   QUCLNMN,0           IS THERE A MIN LENGTH                        
         BE    TF10                                                             
         CLC   TACOSEC,QUCLNMN                                                  
         BL    TFNO                                                             
*                                                                               
TF10     CLI   QUCLNMX,0           IS THERE A MAX LENGTH                        
         BE    TF20                                                             
         CLC   TACOSEC,QUCLNMX                                                  
         BH    TFNO                                                             
*                                                                               
TF20     TM    QUOPTS,QUCLA        IF REQUESTING CLA OPTION                     
         BZ    TFYES                                                            
         LA    R3,KEY                                                           
         USING TLUHD,R3            BUILD PARTIAL KEY FOR RECORD                 
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TICOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,=C'CLA'     USE CODE                                     
         SPACE 1                                                                
         GOTO1 HIGH                GET DIRECTORY RECORD                         
         B     TF35                                                             
         SPACE 1                                                                
TF34     GOTO1 SEQ                 GET NEXT DIRECTORY RECORD                    
         SPACE 1                                                                
TF35     CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE  DID WE FIND REC FOR USE          
         BNE   TFNOCLA                                                          
         SPACE 1                                                                
TF36     GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   TF34                                                             
         SPACE                                                                  
TF37     CLC   QUPEND,TAUHSTRT     IF PERIOD END IS BETWEEN                     
         BL    TF34                                                             
         CLC   QUPEND,TAUHEND      ELEMENT'S START AND END DATES                
         BNH   TFYESCLA                                                         
         SPACE                                                                  
         CLC   QUPSTA,TAUHSTRT     OR IF PERIOD START IS BETWEEN                
         BL    TF34                                                             
         CLC   QUPSTA,TAUHEND      ELEMENT'S START AND END DATES                
         BH    TF34                                                             
*                                                                               
TFYESCLA MVC   KEY,TIKEY           RESET SYSIO'S READ SEQ                       
         GOTO1 HIGH                                                             
TFYES    B     YES                                                              
*                                                                               
TFNOCLA  MVC   KEY,TIKEY           RESET SYSIO'S READ SEQ                       
         GOTO1 HIGH                                                             
TFNO     B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO PROCESS RECORDS                                       
*                                                                               
PROCRECD NTR1                                                                   
         GOTO1 MYTRACE,DMCB,=C'PROCR RECORD',TIAREC,0                           
         GOTO1 USEVAL,DMCB,(X'10',TIUSE),TIUTYPE                                
         L     R4,TIAREC           R4 = A(RECORD)                               
*                                                                               
PROCR02  CLI   0(R4),TLHCCDQ       IF THIS IS A HISTORY COMMENT                 
         BE    PROCR05                                                          
         CLI   0(R4),TLINCDQ       OR IF THIS IS AN INVOICE RECORD              
         BNE   PROCR07                                                          
         OI    TIQFLAG2,TIQFHCA    WANT CAST                                    
*                                                                               
PROCR05  MVI   TIMODE,PROCNOCK     MUST SET NOT TO GET CKS HERE                 
*                                                                               
         USING RECD,R2                                                          
PROCR07  LA    R2,RECORD           R2=A(RECORD)                                 
         USING TABLED,R3                                                        
         L     R3,=A(TABLE)        R3 = A(QUESTION TABLE)                       
*                                                                               
PROCR10  CLI   0(R3),0             END OF TABLE                                 
         BE    PROCRX                                                           
         CLC   TABRCD,0(R4)        IS THIS QUESTION FOR THIS RECORD             
         BNE   PROCR20             TYPE                                         
         CLC   SVMED,TABMED        IS THIS THE CORRECT MEDIA                    
         BNE   PROCR20                                                          
         CLI   TABRCD,TLCACDQ      IS THIS QUESTION FOR CAST                    
         BE    PROCR30                                                          
         CLI   TABRCD,TLCKCDQ      OR CHECKS                                    
         BNE   PROCR15                                                          
         BAS   RE,TSTDATE                                                       
         BNE   PROCRX                                                           
         B     PROCR30             YES - LOOK AT EACH ONE                       
*                                                                               
PROCR15  CLI   TABUSED,C'Y'        ELSE IF THIS QUESTION WAS ALREADY            
         BNE   PROCR30                  ANSWERED - GET NEXT QUESTION            
*                                                                               
PROCR20  ZIC   R1,TABLEN                                                        
         AR    R3,R1               ELSE CHECK NEXT TABLE ENTRY                  
         B     PROCR10                                                          
*                                                                               
PROCR30  BAS   RE,CLRREC                                                        
         MVI   ADDIN,C'Y'                                                       
*                                                                               
         LH    RF,TABIRTN          RF=DISPLACEMENT TO INPUT ROUTINE             
         LTR   RF,RF               IS THERE A ROUTINE                           
         BZ    PROCR20                                                          
         AR    RF,RB                                                            
         BASR  RE,RF               *** GO TO INPUT ROUTINE ***                  
         BNE   PROCR20                                                          
*                                                                               
         CLI   ADDIN,C'N'          ADD THIS ENTRY TO BINSRCH TABLE              
         BE    PROCR20             NO                                           
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
         MVI   TABUSED,C'Y'        SET THIS QUESTION USED                       
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
*                                                                               
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         B     PROCR20             TRY NEXT TABLE ENTRY                         
*                                                                               
PROCRX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*              BINSRCH RECORD GENERATION ROUTINES                               
*                                                                               
RESET    NTR1                                                                   
         NI    TIQFLAG2,X'FF'-TIQFHCA  DON'T WANT CAST                          
         USING TABLED,R3                                                        
         L     R3,=A(TABLE)        R3 = A(QUESTION TABLE)                       
*                                                                               
RS10     CLI   0(R3),0             END OF TABLE                                 
         BE    RSX                                                              
         MVI   TABUSED,C'N'        SET THIS QUESTION TO NOT USED YET            
         ZIC   R1,TABLEN                                                        
         AR    R3,R1               BUMP TO NEXT TABLE ENTRY                     
         B     RS10                                                             
*                                                                               
RSX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        CHECK TO SEE IF CHECK IS IN CORRECT BILL DATE                          
*                                                                               
TSTDATE  NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TABYD,R4                                                         
         MVI   ELCODE,TABYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   TDNO                                                             
*                                                                               
         OC    TIQPSTR(6),TIQPSTR  POSSIBLE FILTERS                             
         BZ    TDYES                                                            
         CLC   TABYDATE,TIQPSTR    START TEST                                   
         BL    TDNO                                                             
         OC    TIQPEND,TIQPEND     END TEST                                     
         BZ    TDYES                                                            
         CLC   TABYDATE,TIQPEND                                                 
         BH    TDNO                                                             
*                                                                               
TDYES    B     YES                                                              
*                                                                               
TDNO     B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              BINSRCH RECORD GENERATION ROUTINES                               
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
SESSALL  NTR1                                                                   
*                                                                               
SA10     LA    RF,INCLUSES         RF=A(TABLE) OF USES TO INCLUDE               
         LA    RE,NINCLUDE         RE=N'USES IN TABLE                           
*                                                                               
         CLC   TGUSEQU,0(RF)                                                    
         BE    SAYES               MATCH - USE IT                               
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
*                                                                               
         TM    TGUSSTAT,SESSION    IF THIS IS NOT A SESSION                     
         BNO   SANO                SKIP IT                                      
*                                                                               
         LA    RF,EXCLUSES         RF=A(TABLE) OF USES TO EXCLUDE               
         LA    RE,NEXCLUDE         RE=N'USES IN TABLE                           
*                                                                               
         CLC   TGUSEQU,0(RF)                                                    
         BE    SANO                MATCH - SKIP IT                              
         LA    RF,1(RF)                                                         
         BCT   RE,*-14             ELSE USE IT                                  
*                                                                               
SAYES    B     YES                                                              
*                                                                               
SANO     B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
*        REUSE                                                                  
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
REUALL   NTR1                                                                   
         TM    TGUSSTAT,SESSION    IF THIS IS A SESSION                         
         BO    RENO                SKIP IT                                      
*                                                                               
         LA    RF,EXCLURE          RF=A(TABLE) OF USES TO EXCLUDE               
         LA    RE,NEXCLURE         RE=N'USES IN TABLE                           
*                                                                               
         CLC   TGUSEQU,0(RF)                                                    
         BE    RENO                MATCH - SKIP IT                              
         LA    RF,1(RF)                                                         
         BCT   RE,*-14             ELSE USE IT                                  
*                                                                               
REYES    B     YES                                                              
*                                                                               
RENO     B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        REUSE APPLIED TO SESSION                                               
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
APPLSES  NTR1                                                                   
         CLI   TABUSED,C'Y'        EVEN THOUGH DOING CHECKS                     
         BE    ASNO                ONLY WANT 1 PER COMML                        
         BAS   RE,REUALL                                                        
         BNE   ASNO                                                             
         BAS   RE,GETPDEL                                                       
         USING TAPDD,R4                                                         
         CLI   TAPDACDE,APPLSESS                                                
         BE    YES                                                              
*                                                                               
ASNO     B     NO                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        TV HOLDING FEE COMMERCIAL                                              
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
HLDFEE   NTR1                                                                   
         CLI   TABUSED,C'Y'        EVEN THOUGH DOING CHECKS                     
         BE    NO                  ONLY WANT 1 PER COMML                        
         TM    TGUSSTA2,HLDTYPE    IS THIS A HOLDING FEE                        
         BNO   NO                                                               
         B     YES                                                              
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*        REUSE APPLIED TO HOLDING FEE                                           
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
APPLHF   NTR1                                                                   
         CLI   TABUSED,C'Y'        EVEN THOUGH DOING CHECKS                     
         BE    AHNO                ONLY WANT 1 PER COMML                        
         BAS   RE,REUALL           IS THIS A REUSE                              
         BNE   AHNO                                                             
         BAS   RE,GETPDEL                                                       
         BNE   AHNO                                                             
         USING TAPDD,R4                                                         
         CLI   TAPDACDE,APPLHLD                                                 
         BE    YES                                                              
*                                                                               
AHNO     B     NO                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        FOREIGN USE COMMERCIALS                                                
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FOREIGN  NTR1                                                                   
         LA    RF,INCFOR           RF=A(TABLE) OF USES TO INCLUDE               
         LA    RE,NINCFOR          RE=N'USES IN TABLE                           
*                                                                               
         CLC   TGUSEQU,0(RF)                                                    
         BE    FOYES               MATCH - USE IT                               
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
         B     NO                                                               
*                                                                               
FOYES    B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        GENERAL CHECK ROUTINE                                                  
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
GENCHK   NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCKCAT                                              
         BAS   RE,GENUNI                                                        
         BE    YES                                                              
         B     NO                                                               
         DROP  R3,R4                                                            
         SPACE 2                                                                
*                                                                               
*        GENERAL UNION ROUTINE                                                  
*        R4    A(RECORD)                                                        
GENUNI   NTR1                                                                   
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GUNO                                                             
         USING TACAD,R4                                                         
         MVC   TGONOF,TACAONOF                                                  
         GOTO1 UNIVAL,DMCB,TACAUN                                               
*        CLI   TGUNEQU,AFT         AFT IS OK FOR RADIO & TV/CABLE               
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         BO    GUYES               MATCH - USE IT                               
         CLI   SVMED,C'R'                                                       
         BE    GUNO                                                             
         CLI   SVMED,C'T'                                                       
         BE    GU10                                                             
         CLI   SVMED,C'C'                                                       
         BNE   GUNO                                                             
*                                                                               
*U10     CLI   TGUNEQU,SAG         SAG IS FOR TV/CABLE                          
GU10     GOTO1 UNITEST,DMCB,TGUNEQUS,SAG,0,0,0                                  
         BZ    GUNO                                                             
*                                                                               
GUYES    B     YES                                                              
*                                                                               
GUNO     B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        $ PAID TO SCALE PERFORMERS                                             
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
SCALE    NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   SCNO                                                             
         USING TAPDD,R4                                                         
         BAS   RE,GETPDEL                                                       
         BNE   SCNO                                                             
         L     R1,TAPDGRS                                                       
         ST    R1,FULL                                                          
*                                                                               
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
*                                                                               
         OC    TAPDOV1,TAPDOV1     GET OVERSCALE AMOUNT                         
         BZ    SC10                                                             
         ZIC   R1,RECCDE           IF OVERSCALE INC REC CODE                    
         LA    R1,1(R1)                                                         
         STC   R1,RECCDE                                                        
*                                                                               
SC10     L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    RECACC1,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         BAS   RE,ACCTOT                                                        
*                                                                               
         B     YES                                                              
*                                                                               
SCNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        $ PAID TO SCALE/OVERSCALE PERFORMERS BY CAT TYPE                       
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
CATTYP   NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   CTNO                                                             
         BAS   RE,SESSALL          IS THIS A SESSION                            
         BNE   CTNO                                                             
         USING TAPDD,R4                                                         
         BAS   RE,GETPDEL                                                       
         BNE   CTNO                                                             
         L     R1,TAPDGRS                                                       
         ST    R1,MYFULL                                                        
*                                                                               
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
         MVC   RECDAT1(L'TGCACDE),TGCACDE     SET CATEGORY TYPE                 
         MVC   RECDAT2(3),TGONOF                                                
*                                                                               
         MVI   MYBYTE2,C' '                                                     
         LA    R5,RECACC1          POINT TO SCALE ACCUM                         
         OC    TAPDOV1,TAPDOV1     GET OVERSCALE AMOUNT                         
         BZ    CT10                                                             
         LA    R5,RECACC2                                                       
         MVI   MYBYTE2,C'O'                                                     
*                                                                               
CT10     L     R1,MYFULL                                                        
         CVD   R1,DUB                                                           
         AP    0(8,R5),DUB                                                      
*                                                                               
         L     R1,MYFULL           ACCUMULATE TOTAL                             
         CVD   R1,DUB                                                           
         AP    RECACC3,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         MVI   ADDIN,C'N'                                                       
         MVC   MYBYTE,RECCDE       SAVE RECORD CODE                             
         BAS   RE,CLRREC                                                        
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,MYBYTE       RESTORE RECORD CODE                          
         OI    RECCDE,X'0F'        AND SET CODE FOR TOTAL                       
*                                                                               
         LA    R5,RECACC1          POINT TO SCALE ACCUM                         
         CLI   MYBYTE2,C'O'                                                     
         BNE   CT20                                                             
         LA    R5,RECACC2          OR OVERSCALE ACCUM                           
*                                                                               
CT20     L     R1,MYFULL                                                        
         CVD   R1,DUB                                                           
         AP    0(8,R5),DUB                                                      
*                                                                               
         L     R1,MYFULL           ACCUMULATE TOTAL                             
         CVD   R1,DUB                                                           
         AP    RECACC3,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         B     YES                                                              
*                                                                               
CTNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        $ PAID TO SCALE/OVERSCALE PERFORMERS BY USE TYPE                       
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
USETYP   NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   UTNO                                                             
         USING TAPDD,R4                                                         
         BAS   RE,GETPDEL                                                       
         BNE   UTNO                                                             
         CLC   TAPDUSE,=C'MUS'     IGNORE MUSIC PAYMENTS                        
         BE    UTNO                                                             
         L     R1,TAPDGRS                                                       
         ST    R1,MYFULL                                                        
*                                                                               
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
         MVC   RECDAT1(L'TGUSCDE),TGUSCDE     SET USE TYPE                      
         MVC   RECUNAME,TGUSNAME                                                
*                                                                               
         MVI   MYBYTE2,C' '                                                     
         LA    R5,RECACC1          POINT TO SCALE ACCUM                         
         OC    TAPDOV1,TAPDOV1     GET OVERSCALE AMOUNT                         
         BZ    UT10                                                             
         LA    R5,RECACC2                                                       
         MVI   MYBYTE2,C'O'                                                     
*                                                                               
UT10     L     R1,MYFULL                                                        
         CVD   R1,DUB                                                           
         AP    0(8,R5),DUB                                                      
*                                                                               
         L     R1,MYFULL           ACCUMULATE TOTAL                             
         CVD   R1,DUB                                                           
         AP    RECACC3,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         MVI   ADDIN,C'N'                                                       
         MVC   MYBYTE,RECCDE       SAVE RECORD CODE                             
         BAS   RE,CLRREC                                                        
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,MYBYTE       RESTORE RECORD CODE                          
         OI    RECCDE,X'0F'        AND SET CODE FOR TOTAL                       
*                                                                               
         LA    R5,RECACC1          POINT TO SCALE ACCUM                         
         CLI   MYBYTE2,C'O'                                                     
         BNE   UT20                                                             
         LA    R5,RECACC2          OR OVERSCALE ACCUM                           
*                                                                               
UT20     L     R1,MYFULL                                                        
         CVD   R1,DUB                                                           
         AP    0(8,R5),DUB                                                      
*                                                                               
         L     R1,MYFULL           ACCUMULATE TOTAL                             
         CVD   R1,DUB                                                           
         AP    RECACC3,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         B     YES                                                              
*                                                                               
UTNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        AVERAGE NUMBER OF CAST IN COMMERCIAL BROKEN DOWN BY CAT                
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
AVGCOM   NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
         BAS   RE,GENUNI           IS THIS CAST MEMBER THE RIGHT UNION          
         BNE   AVNO                                                             
         TM    TLCASORT,X'20'      IF MEMBER IS AN EXTRA                        
         BO    AVNO                                                             
         CLC   TLCACAT,=C'ZZ '     OR CAT = ZZ                                  
         BE    AVNO                                                             
         CLC   TLCACAT,=C'ZZZ'     OR CAT = ZZZ                                 
         BE    AVNO                                                             
         CLC   TLCACAT,=C'GEN'     OR CAT = GEN                                 
         BE    AVNO                IGNORE                                       
         MVI   ADDIN,C'N'                                                       
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
         MVC   RECDAT1(L'TLCACAT),TLCACAT     CATEGORY                          
         MVC   RECDAT2(3),=C'ON '                                               
         TM    TLCASORT,X'08'      IS CAST OFF CAMERA                           
         BNO   *+10                                                             
         MVC   RECDAT2(3),=C'OFF'                                               
         AP    RECACC1,=P'1'                                                    
         AP    RECCNT,=P'1'                                                     
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         MVC   MYBYTE,RECCDE       SAVE RECORD CODE                             
         BAS   RE,CLRREC                                                        
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,MYBYTE       RESTORE RECORD CODE                          
         OI    RECCDE,X'02'        AND SET CODE FOR TOTAL                       
         AP    RECACC1,=P'1'                                                    
         AP    RECCNT,=P'1'                                                     
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         CLI   COUNTED,C'Y'                                                     
         BE    AVYES                                                            
         MVI   COUNTED,C'Y'                                                     
         AP    COMMCNT,=P'1'       ADD TO COMMERCIAL COUNT                      
         GOTO1 MYTRACE,DMCB,=C'COUNT REC',TIAREC,0                              
AVYES    B     YES                                                              
*                                                                               
AVNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF CYCLES OF TV PROGRAM USE BY CLASS TYPE                    
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
CLATYP   NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ      GO FIND TACO ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   CLNO                                                             
         USING TACOD,R4                                                         
         MVC   MYCID2,TACOCID                                                   
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   CLNO                                                             
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         BNE   CLNO                                                             
         CLC   TGUSCDE,=C'CLA'     CLASS A CODE                                 
         BE    CL10                                                             
         CLC   TGUSCDE,=C'LOC'     CLASS B & C CODE                             
         BNE   CLNO                                                             
*                                                                               
CL10     L     R1,=A(USECTAB)      USE BY CLASS TYPE TABLE                      
         USING USECTABD,R1                                                      
*                                                                               
CL20     CLI   0(R1),X'FF'         TABLE TOO SMALL                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    UCCYC,UCCYC         LAST ENTRY                                   
         BNZ   CL30                                                             
         MVC   UCTYPE,TGUSCDE      SET USE TYPE                                 
         MVC   CCTYPE,TGUSTYCD     SET CLASS TYPE                               
         MVC   UCCYC,TAPDCYCS      SET CYCLE DATES                              
         B     CL50                                                             
*                                                                               
CL30     CLC   UCTYPE,TGUSCDE      USE TYPE                                     
         BNE   CL40                                                             
         CLC   CCTYPE,TGUSTYCD     CLASS TYPPE                                  
         BNE   CL40                                                             
         CLC   UCCYC,TAPDCYCS      DO CYCLE DATES MATCH                         
         BE    CL50                                                             
*                                                                               
CL40     LA    R1,USECTBLN(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     CL20                                                             
*                                                                               
CL50     DS    0H                                                               
*                                                                               
CLNO     B     NO                                                               
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF CYCLES OF TV DEALER USE BY CLASS TYPE                     
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
DLRTYP   NTR1                                                                   
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   DLNO                                                             
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         BNE   DLNO                                                             
         CLC   TGUSCDE,=C'DLR'     DEALER                                       
         BNE   DLNO                                                             
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,TABCDE       AND CODE                                     
         MVC   RECDAT1(L'TGUSCDE),TGUSCDE     SET USE TYPE                      
         MVC   RECDAT2(L'TGUSTYCD),TGUSTYCD                                     
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
         B     YES                                                              
*                                                                               
DLNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF TV SPOT USE BY MAJOR AREA AND UNIT GROUP                  
*        KEEP INFO IN TABLE UNTIL READ ALL INVOICES                             
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
MAJOR    NTR1                                                                   
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   MAJNO                                                            
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         BNE   MAJNO                                                            
         CLC   TGUSCDE,=C'WSP'     WILDSPOT                                     
         BNE   MAJNO                                                            
         L     R1,=A(WSPTAB)       WSP TABLE                                    
         USING WSPTABD,R1                                                       
*                                                                               
MAJ10    CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    WTCYC,WTCYC         LAST ENTRY                                   
         BZ    MAJ20                                                            
         CLC   WTCYC,TAPDCYCS      DO CYCLE DATES MATCH                         
         BE    MAJ20                                                            
         LA    R1,WSPTABLN(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     MAJ10                                                            
*                                                                               
MAJ20    MVC   WTCYC,TAPDCYCS      SET CYCLE DATES                              
         OC    WTMAJ,TAPDMAJ       OR IN MAJORS                                 
         CLC   WTUNIT,TAPDUNIT     CHECK NUMBER OF UNITS                        
         BH    MAJNO                                                            
         MVC   WTUNIT,TAPDUNIT     SET TO HIGHER NUMBER                         
*                                                                               
MAJNO    B     NO                                                               
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF TV CLASS A USES                                           
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
CLAUSES  NTR1                                                                   
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   CLAUNO                                                           
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         BNE   CLAUNO                                                           
         CLC   TGUSCDE,=C'CLA'     CLASS A CODE                                 
         BNE   CLAUNO                                                           
         L     R1,=A(CLATAB)       CLA TABLE                                    
         USING CLATABD,R1                                                       
*                                                                               
CLAU10   CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    CLCYC,CLCYC         LAST ENTRY                                   
         BNZ   CLAU15                                                           
         MVC   CLCYC,TAPDCYCS      SET CYCLE DATES                              
         B     CLAU20                                                           
*                                                                               
CLAU15   CLC   CLCYC,TAPDCYCS      DO CYCLE DATES MATCH                         
         BE    CLAU20                                                           
         LA    R1,CLATABLN(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     CLAU10                                                           
*                                                                               
CLAU20   LH    R3,TAPDSTUS         ENSURE WE HAVE MAX N'USES FOR CYCLE          
         AH    R3,TAPDUSES                                                      
         LTR   R3,R3                                                            
         BZ    *+6                                                              
         BCTR  R3,0                                                             
         CH    R3,CLUSES           IF IT'S HIGHER THAN CURRENT                  
         BNH   *+8                                                              
         STH   R3,CLUSES           SET NEW N'USES                               
*                                                                               
         L     R3,TAPDGRS          ACCUMULATE GROSS                             
         A     R3,CLGROSS                                                       
         ST    R3,CLGROSS                                                       
*                                                                               
CLAUNO   B     NO                                                               
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF CYCLES  & DOLLARS PAID TO SCALE & OVERSCALE               
*        BY USE TYPE                                                            
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
RUSETYP  NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   RUNO                                                             
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   RUNO                                                             
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         BNE   RUNO                                                             
         CLC   TAPDUSE,=C'MUS'     IGNORE MUSIC PAYMENTS                        
         BE    RUNO                                                             
         CLI   TGUSTYCD,C'U'       UPGRADES ARE PART OF NATIVE TYPE             
         BNE   *+14                                                             
         MVC   TGUSTYCD(1),TGUSTYCD+1                                           
         MVI   TGUSTYCD+1,C' '                                                  
         L     R1,=A(RUSETAB)      RADIO USE TABLE                              
         USING RUSETABD,R1                                                      
*                                                                               
RU10     CLI   0(R1),X'FF'         TABLE TOO SMALL                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    RUCODE,RUCODE       LAST ENTRY                                   
         BNZ   RU15                                                             
         MVC   RUCODE,TAPDUSE      SET USE CODE                                 
         MVC   RUTYPE,TGUSTYCD     SET USE TYPE                                 
         MVC   RUCYC,TAPDCYCS      SET CYCLE DATES                              
         B     RU16                                                             
*                                                                               
RU15     CLC   RUCODE,TAPDUSE      DOES ENTRY MATCH                             
         BNE   RU17                                                             
         CLC   RUTYPE,TGUSTYCD                                                  
         BNE   RU17                                                             
         CLC   RUCYC,TAPDCYCS                                                   
         BNE   RU17                                                             
*                                                                               
RU16     LA    R5,RUSCALE          ADD GROSS TO APPROPRIATE ACCUM.              
         OC    TAPDOV1,TAPDOV1                                                  
         BZ    *+8                                                              
         LA    R5,RUOVSC                                                        
         L     R0,0(R5)                                                         
         A     R0,TAPDGRS                                                       
         ST    R0,0(R5)                                                         
         B     RU20                                                             
*                                                                               
RU17     LA    R1,RUSETBLN(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RU10                                                             
*                                                                               
RU20     DS    0H                                                               
RUNO     B     NO                                                               
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF RADIO SPOT USE BY MAJOR AREA AND UNIT GROUP               
*        KEEP INFO IN TABLE UNTIL READ ALL INVOICES                             
*                                                                               
RMAJOR   NTR1                                                                   
         BAS   RE,GETPDEL          GO FIND TAPD ELEMENT                         
         BNE   RMAJNO                                                           
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         CLC   TGUSCDE,=C'WSP'     WILDSPOT                                     
         BNE   RMAJNO                                                           
         L     R1,=A(RWSPTAB)      RADIO WSP TABLE                              
         USING RWSPTABD,R1                                                      
*                                                                               
RMAJ10   CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    RWTCYC,RWTCYC       LAST ENTRY                                   
         BZ    RMAJ20                                                           
         CLC   RWTCYC,TAPDCYCS     DO CYCLE DATES MATCH                         
         BE    RMAJ20                                                           
         LA    R1,RWSPTBLN(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RMAJ10                                                           
*                                                                               
RMAJ20   MVC   RWTCYC,TAPDCYCS     SET CYCLE DATES                              
         MVC   RWTWKS,TGUSWKS          CYCLE WEEKS                              
         OC    RWTMAJ,TAPDMAJ      OR IN MAJORS                                 
         CLC   RWTUNIT,TAPDUNIT    CHECK NUMBER OF UNITS                        
         BH    RMAJNO                                                           
         MVC   RWTUNIT,TAPDUNIT    SET TO HIGHER NUMBER                         
*                                                                               
RMAJNO   B     NO                                                               
         DROP  R1,R4                                                            
         EJECT                                                                  
*                                                                               
*        TOTAL $ FOR MADE FOR CABLE SESSIONS                                    
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
CABSESS  NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   CABNO                                                            
         BAS   RE,SESSALL          IS THIS A SESSION                            
         BNE   CABNO                                                            
         USING TAPDD,R4                                                         
         BAS   RE,GETPDEL                                                       
         BNE   CABNO                                                            
*                                                                               
         L     R1,TAPDGRS                                                       
         CVD   R1,DUB                                                           
         AP    RECACC1,DUB                                                      
         B     YES                                                              
*                                                                               
CABNO    B     NO                                                               
         DROP  R2,R3,R4                                                         
         SPACE 2                                                                
*                                                                               
*        TOTAL COMMERCIALS FOR CABLE SESSIONS                                   
*                                                                               
CABCOMM  NTR1                                                                   
         CLI   COUNTED,C'Y'                                                     
         BE    CBCNO                                                            
         MVI   COUNTED,C'Y'                                                     
         B     YES                                                              
*                                                                               
CBCNO    B     NO                                                               
         EJECT                                                                  
*                                                                               
*        TOTAL $ FOR MADE FOR CABLE REUSE                                       
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
         USING RECD,R2                                                          
CABREUS  NTR1                                                                   
         BAS   RE,GENCHK           GENERAL CHECK ROUTINE                        
         BNE   CRNO                                                             
         BAS   RE,REUALL           IS THIS A REUSE                              
         BNE   CRNO                                                             
         USING TAPDD,R4                                                         
         BAS   RE,GETPDEL                                                       
         BNE   CRNO                                                             
*                                                                               
         L     R1,TAPDGRS                                                       
         CVD   R1,DUB                                                           
         AP    RECACC1,DUB                                                      
         B     YES                                                              
*                                                                               
CRNO     B     NO                                                               
         DROP  R2,R3,R4                                                         
         SPACE 2                                                                
*                                                                               
*        TOTAL COMMERCIALS FOR CABLE REUSE                                      
*                                                                               
CABCOM2  NTR1                                                                   
         CLI   COUNTED2,C'Y'                                                    
         BE    C2NO                                                             
         MVI   COUNTED2,C'Y'                                                    
         B     YES                                                              
*                                                                               
C2NO     B     NO                                                               
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF TV SPOT USE BY MAJOR AREA AND UNIT GROUP                  
*        LOOP THRU WSP TABLE AND ADD TO BINSRCH TABLE AS NEEDED                 
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FLUSHWSP NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
*                                                                               
         L     R4,=A(WSPTAB)       WSP TABLE                                    
         USING WSPTABD,R4                                                       
FWSP10   CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    FWSPX                                                            
         OC    WTCYC,WTCYC         LAST ENTRY                                   
         BZ    FWSPX                                                            
*                                                                               
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         BAS   RE,GETRCDE          GET RECORD CODE IN MYBYTE2                   
         MVC   RECCDE,MYBYTE2      AND CODE                                     
         BAS   RE,SETMAX           SET MAXIMUM IN FULL                          
         LH    R1,WTUNIT                                                        
         BAS   RE,GETENDS                                                       
         MVC   RECDAT1(3),WORK                                                  
         MVC   RECDAT2(3),WORK+3                                                
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         ZIC   R1,RECCDE                                                        
         BAS   RE,CLRREC                                                        
         LA    R1,1(R1)            INC REC CODE FOR SUBTOTAL                    
         STC   R1,RECCDE                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         MVC   MYBYTE,RECCDE       SAVE RECORD CODE                             
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVC   RECCDE,MYBYTE       RESTORE RECORD CODE                          
         OI    RECCDE,X'0F'        AND SET CODE FOR TOTAL                       
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         LA    R4,WSPTABLN(R4)     BUMP TO NEXT ENTRY                           
         B     FWSP10                                                           
*                                                                               
FWSPX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF CYCLES OF TV PROGRAM USE BY CLASS TYPE                    
*        LOOP THRU USE CLA TABLE AND ADD TO BINSRCH TABLE AS NEEDED             
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FLUSHPGM NTR1                                                                   
         LA    R2,RECORD                                                        
         USING RECD,R2             R2=A(RECORD)                                 
*                                                                               
         L     R4,=A(USECTAB)      USE BY CLASS TYPE TABLE                      
         USING USECTABD,R4                                                      
FPGM10   CLI   0(R4),X'FF'         PHYSICAL END OF TABLE                        
         BE    FPGMX                                                            
         OC    UCTYPE,UCTYPE       LOGICAL END OF TABLE                         
         BZ    FPGMX                                                            
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVI   RECCDE,X'55'        SET CODE                                     
         MVC   RECDAT1(L'UCTYPE),UCTYPE                                         
         MVC   RECDAT2(L'CCTYPE),CCTYPE                                         
         ZAP   RECCNT,=P'1'                                                     
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         BAS   RE,CLRREC           NOW SET UP FOR TOTAL                         
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVI   RECCDE,X'55'        SET CODE                                     
         OI    RECCDE,X'0F'        SET TOTAL INDICATOR                          
         ZAP   RECCNT,=P'1'                                                     
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         LA    R4,USECTBLN(R4)                                                  
         B     FPGM10                                                           
*                                                                               
FPGMX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF TV CLASS A USES                                           
*        LOOP THRU CLA TABLE AND ADD TO BINSRCH TABLE AS NEEDED                 
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FLUSHCLA NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
*                                                                               
         L     R4,=A(CLATAB)       CLA TABLE                                    
         USING CLATABD,R4                                                       
FCLA10   CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    FCLAX                                                            
         OC    CLCYC,CLCYC         LAST ENTRY                                   
         BZ    FCLAX                                                            
*                                                                               
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVI   RECCDE,X'80'        SET CODE                                     
         ZAP   RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         ZAP   RECACC1,=P'1'       INCREMENT CUMULATIVE COUNT                   
         L     RE,CLGROSS          ACCUMULATE GROSS                             
         CVD   RE,DUB                                                           
         AP    RECACC2,DUB                                                      
*                                                                               
         LH    RF,CLUSES                                                        
         CH    RF,=H'105'          IF MORE THAN MAX N'USES                      
         BNH   FCLA20                                                           
         SH    RF,=H'104'          SET TO ADD IN OVERAGE TO MAX RECORD          
         CVD   RF,DUB                                                           
         ZAP   RECACC1,DUB                                                      
         LH    RF,=H'105'          LOOP MAX N'TIMES                             
FCLA20   EDIT  (RF),(4,RECDAT1)                                                 
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
         ZAP   RECCNT,=P'0'        CLEAR INDIV. COUNT AFTER 1ST TIME            
         ZAP   RECACC2,=P'0'       CLEAR GROSS ACCUMULATOR                      
         ZAP   RECACC1,=P'1'       INCREMENT CUMULATIVE COUNT                   
         BCT   RF,FCLA20                                                        
*                                                                               
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVI   RECCDE,X'80'        SET CODE                                     
         OI    RECCDE,X'0F'        AND SET CODE FOR TOTAL                       
         ZAP   RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         L     RE,CLGROSS          ACCUMULATE GROSS                             
         CVD   RE,DUB                                                           
         AP    RECACC2,DUB                                                      
         LH    RF,CLUSES                                                        
         CVD   RF,DUB                                                           
         ZAP   RECACC1,DUB                                                      
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         LA    R4,CLATABLN(R4)     BUMP TO NEXT ENTRY                           
         B     FCLA10                                                           
*                                                                               
FCLAX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*              FIGURE OUT CORRECT RECORD CODE                                   
*                                                                               
         USING WSPTABD,R4                                                       
GETRCDE  NTR1                                                                   
         CLI   SVMED,C'R'          IF THIS IS RADIO                             
         BE    GRC10                                                            
         MVI   MYBYTE2,X'70'       NO MAJORS                                    
         CLI   WTMAJ,0                                                          
         BE    GRX                                                              
         MVI   MYBYTE2,X'72'       NY ONLY                                      
         CLI   WTMAJ,X'80'                                                      
         BE    GRX                                                              
         MVI   MYBYTE2,X'74'       LA OR CHI ONLY                               
         CLI   WTMAJ,X'40'                                                      
         BE    GRX                                                              
         CLI   WTMAJ,X'20'                                                      
         BE    GRX                                                              
         MVI   MYBYTE2,X'78'       ALL THREE                                    
         CLI   WTMAJ,X'E0'                                                      
         BE    GRX                                                              
         MVI   MYBYTE2,X'76'       ANY TWO                                      
         B     GRX                                                              
*                                                                               
         USING RWSPTABD,R4                                                      
GRC10    MVI   MYBYTE2,X'C0'       NO MAJORS                                    
         CLI   RWTMAJ,0                                                         
         BE    GRX                                                              
         MVI   MYBYTE2,X'C2'       NY ONLY                                      
         CLI   RWTMAJ,X'80'                                                     
         BE    GRX                                                              
         MVI   MYBYTE2,X'C4'       LA OR CHI ONLY                               
         CLI   RWTMAJ,X'40'                                                     
         BE    GRX                                                              
         CLI   RWTMAJ,X'20'                                                     
         BE    GRX                                                              
         MVI   MYBYTE2,X'C8'       ALL THREE                                    
         CLI   RWTMAJ,X'E0'                                                     
         BE    GRX                                                              
         MVI   MYBYTE2,X'C6'       ANY TWO                                      
*                                                                               
GRX      B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
*              SET MAXIMUM UNITS/RECORD TYPE                                    
*                                                                               
SETMAX   NTR1                                                                   
         MVC   FULL,=F'76'         NO MAJORS - RADIO                            
         CLI   MYBYTE2,X'C0'                                                    
         BE    SMX                                                              
         MVC   FULL,=F'126'                                                     
         CLI   MYBYTE2,X'70'       NO MAJORS - TV                               
         BE    SMX                                                              
         CLI   MYBYTE2,X'C2'       NY - RADIO                                   
         BE    SMX                                                              
         CLI   MYBYTE2,X'C4'       CHI OR LA - RADIO                            
         BE    SMX                                                              
         MVC   FULL,=F'176'                                                     
         CLI   MYBYTE2,X'72'       NY - TV                                      
         BE    SMX                                                              
         CLI   MYBYTE2,X'74'       CHI OR LA - TV                               
         BE    SMX                                                              
         CLI   MYBYTE2,X'C6'       ANY TWO - RADIO                              
         BE    SMX                                                              
         MVC   FULL,=F'226'                                                     
         CLI   MYBYTE2,X'76'       ANY TWO - TV                                 
         BE    SMX                                                              
         CLI   MYBYTE2,X'C6'       ALL THREE - RADIO                            
         BE    SMX                                                              
         MVC   FULL,=F'376'        ALL THREE - TV                               
*                                                                               
SMX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*              FIGURE OUT CORRECT ENDS                                          
*              R1  - N'UNITS                                                    
*                                                                               
GETENDS  NTR1                                                                   
*                                                                               
         MVC   WORK(6),=C'000000'  SET TO 0 ENDS                                
         LTR   R1,R1               IS THIS 0 UNITS                              
         BNZ   GE05                                                             
         B     GEX                                                              
*                                                                               
GE05     LA    R3,5                WE'LL BUMP BY 5 UNITS                        
*                                                                               
GE10     CR    R1,R3                                                            
         BNH   GE20                                                             
         LA    R3,5(R3)            WE'LL BUMP BY 5 UNITS                        
         C     R3,FULL             MAXIMUM                                      
         BNH   GE10                                                             
         EDIT  FULL,(3,WORK),ZERO=NOBLANK,FILL=0                                
         MVC   WORK+3(3),=C'&&UP'                                               
         B     GEX                                                              
*                                                                               
GE20     DS    0H                                                               
         EDIT  (R3),(3,WORK+20),ZERO=NOBLANK,FILL=0                             
         SH    R3,=H'4'                                                         
         EDIT  (R3),(3,WORK),ZERO=NOBLANK,FILL=0                                
         MVC   WORK+3(3),WORK+20                                                
*                                                                               
GEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF RADIO SPOT USE BY MAJOR AREA AND UNIT GROUP               
*        LOOP THRU RADIO WSP TABLE AND ADD TO BINSRCH TABLE AS NEEDED           
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FLSHRWSP NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
*                                                                               
         L     R4,=A(RWSPTAB)      RADIO WSP TABLE                              
         USING RWSPTABD,R4                                                      
FRWSP10  CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    FRWSPX                                                           
         OC    RWTCYC,RWTCYC       LAST ENTRY                                   
         BZ    FRWSPX                                                           
*                                                                               
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         BAS   RE,GETRCDE          GET RECORD CODE BASED ON MAJORS              
         MVC   RECCDE,MYBYTE2      SET RECORD CODE                              
*                                                                               
         BAS   RE,SETMAX           SET MAXIMUM IN FULL                          
         LH    R1,RWTUNIT          GET RANGE BASED ON N'UNITS IN CYCLE          
         BAS   RE,GETENDS                                                       
         MVC   RECDAT1(3),WORK                                                  
         MVC   RECDAT2(3),WORK+3                                                
*                                                                               
         CLI   RWTWKS,13           INCREMENT COUNT BASED ON N'CYC WKS           
         BNE   *+14                                                             
         AP    RECCNT,=P'1'                                                     
         B     *+10                                                             
         AP    RECACC1,=P'1'                                                    
*                                                                               
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         ZIC   R1,RECCDE                                                        
         LA    R1,1(R1)            INC REC CODE FOR SUBTOTAL                    
         STC   R1,RECCDE                                                        
         XC    RECDAT1,RECDAT1                                                  
         XC    RECDAT2,RECDAT2                                                  
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         OI    RECCDE,X'0F'        SET CODE FOR TOTAL                           
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         LA    R4,RWSPTBLN(R4)     BUMP TO NEXT ENTRY                           
         B     FRWSP10                                                          
*                                                                               
FRWSPX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        BREAKDOWN OF RADIO SPOT USE BY SCALE/OVERSCALE -CYCLE                  
*        LOOP THRU RADIO USE TABLE AND ADD TO BINSRCH TABLE AS NEEDED           
*                                                                               
         USING TABLED,R3           R3=A(TABLE ENTRY)                            
FLSHRUSE NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
*                                                                               
         L     R4,=A(RUSETAB)      RADIO USE TABLE                              
         USING RUSETABD,R4                                                      
FRUSE10  CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    FRUSEX                                                           
         OC    RUCODE,RUCODE       LAST ENTRY                                   
         BZ    FRUSEX                                                           
*                                                                               
         BAS   RE,CLRREC                                                        
         MVC   RECMED,SVMED        SET MEDIA                                    
         MVI   RECCDE,X'A0'        AND CODE                                     
*                                                                               
         MVC   RECDAT1(3),RUCODE   SET CODE                                     
         MVC   RECDAT2(5),RUTYPE       TYPE                                     
         ZAP   RECCNT,=P'1'        ADD ONE TO CYCLE COUNT                       
*                                                                               
         L     R0,RUSCALE                                                       
         CVD   R0,DUB                                                           
         ZAP   RECACC1,DUB                                                      
         L     RE,RUOVSC                                                        
         CVD   RE,DUB                                                           
         ZAP   RECACC2,DUB                                                      
         AR    R0,RE                                                            
         CVD   R0,DUB                                                           
         ZAP   RECACC3,DUB                                                      
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
*                                                                               
         MVI   RECCDE,X'A2'        SET TOTAL RECORD FOR THIS QUES.              
         XC    RECDAT1,RECDAT1                                                  
         XC    RECDAT2,RECDAT2                                                  
         BAS   RE,BINADD                                                        
*                                                                               
         LA    R4,RUSETBLN(R4)     BUMP TO NEXT ENTRY                           
         B     FRUSE10                                                          
*                                                                               
FRUSEX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*              ACCUMULATE TOTAL RECORD                                          
*                                                                               
         USING TABLED,R3                                                        
ACCTOT   NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
         MVI   ADDIN,C'N'                                                       
         MVC   MYBYTE,RECCDE       SAVE RECORD CODE                             
         BAS   RE,CLRREC                                                        
         MVC   RECMED,TABMED       SET MEDIA                                    
         MVC   RECCDE,MYBYTE       RESTORE RECORD CODE                          
         OI    RECCDE,X'0F'        AND SET CODE FOR TOTAL                       
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    RECACC1,DUB                                                      
*                                                                               
         AP    RECCNT,=P'1'        INCREMENT RECORD COUNT                       
         BAS   RE,BINADD           ** ADD RECORD TO BINSRCH TABLE **            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*              GET TAPD ELEMENT                                                 
*                                                                               
GETPDEL  NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GO FIND TAPD ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*                                                                               
*              ROUTINE TO ADD A RECORD TO BINSRCH TABLE                         
*                                                                               
         USING RECD,R2                                                          
         USING BIND,R3                                                          
         USING TABLED,R4           R4=A(TABLE ENTRY)                            
BINADD   NTR1                                                                   
         LA    R2,RECORD           R2=A(NEW RECORD)                             
         L     R3,=A(BINLIST)      R3=A(BINSRCH PARAMETERS)                     
         MVC   DMCB+8(16),BININ                                                 
         LA    RF,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R2)),(RF)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    XIT                 NOT FOUND - ADDED                            
*                                                                               
         L     RE,DMCB             RE=A(RECORD FOUND)                           
         ZIC   R1,BINFRST          DISP. TO FIRST BUCKET                        
         AR    RE,R1               RE=A(BUCKETS OF RECORD FOUND)                
         LA    RF,RECORD(R1)       RF=A(BUCKETS OF NEW RECORD)                  
         IC    R1,BINNUMB          R1=NUMBER OF BUCKETS                         
         TM    BINSTAT,X'80'                                                    
         BO    BINADD2                                                          
         AP    0(8,RE),0(8,RF)     DATA IS PACKED - ADD NEW TO OLD              
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,*-14                                                          
         B     XIT                                                              
*                                                                               
BINADD2  L     R0,0(RE)            DATA IS BINARY - ADD NEW TO OLD              
         A     R0,0(RF)                                                         
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,BINADD2                                                       
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*              ROUTINE TO PRINT REPORT                                          
*                                                                               
         USING RECD,R2                                                          
         USING BIND,R3                                                          
         USING TABLED,R4           R4=A(TABLE ENTRY)                            
PRTREP   NTR1                                                                   
         L     R5,ASPOOLD          R5=A(SPOOL DSECT)                            
         USING SPOOLD,R5                                                        
         BAS   RE,NEWPAGE          FORCE TO NEW PAGE                            
         MVI   FORCECLR,C'Y'       FORCE CLEARING OF ALL LINES                  
         BAS   RE,PRNTIT                                                        
         ZAP   MYRUNTOT,=P'0'      INIT RUNNING TOTAL FOR CLA                   
*                                                                               
         L     R3,=A(BINLIST)                                                   
         LA    R2,BINTABLE         R2=A(BINSRCH RECORD)                         
         ICM   R0,15,BININ         R0=N'RECORDS IN TABLE                        
         BZ    PRX                 NOTHING TO PRINT                             
*                                                                               
PR10     L     R4,=A(TABLE)        R4 = A(QUESTION TABLE)                       
*                                                                               
PR20     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                CAN'T FIND RECORD TYPE IN TABLE              
*                                                                               
         CLC   TABCDE,RECCDE       SAME RECORD CODE                             
         BE    PR30                                                             
         ZIC   R1,TABLEN                                                        
         AR    R4,R1               TRY NEXT TABLE ENTRY                         
         B     PR20                                                             
*                                                                               
PR30     CLC   MEDIA,TABMED        IF MEDIA CHANGED                             
         BE    PR40                                                             
         BAS   RE,NEWPAGE          FORCE TO NEW PAGE                            
         BAS   RE,PRNTIT           THEN PRINT IT NOW                            
         MVC   MEDIA,TABMED        SAVE NEW MEDIA                               
         MVI   LASTCDE,0           AND RESET CODE                               
*                                                                               
PR40     CLC   RECCDE,LASTCDE      IF CODE CHANGED                              
         BE    PR60                                                             
         MVC   LASTCDE,RECCDE      SAVE THIS CODE                               
         BAS   RE,PRNTIT           AND PRINT A BLANK LINE                       
*                                                                               
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRINTD,R3                                                        
         CLC   LASTQUE,TABCHAR     IS THIS A NEW QUESTION                       
         BE    PR45                                                             
         MVC   LASTQUE,TABCHAR                                                  
*                                                                               
         CLI   TABNPG,C'Y'         START A NEW PAGE                             
         BNE   *+8                                                              
         BAS   RE,NEWPAGE                                                       
         MVC   RCSUBPRG,TABSPROG   SET SPROG                                    
*                                                                               
*&&DO                                                                           
         MVC   PQUES,=C'QUESTION'                                               
         MVC   PCHAR,TABCHAR       MOVE OUT QUESTION NUMBER                     
         MVI   PPERIOD,C'.'                                                     
         LA    R3,132(R3)          BUMP TO NEXT PRINT LINE                      
         MVC   PQUES(13),=C'-------------'                                      
         MVI   SPACING,2           SET DOUBLE SPACE BETWEEN QUESTIONS           
         BAS   RE,PRNTIT                                                        
*                                                                               
*&&                                                                             
PR45     DS    0H                                                               
         ZIC   RF,TABQLEN          QUESTION LENGTH                              
         LTR   RF,RF                                                            
         BZ    PR60                                                             
         GOTO1 CHOPPER,DMCB,((RF),TABQUEST),(50,PQUEST),(C'P',4)                
         CLC   =C'ALL USES',PQUEST                                              
         BNE   PR60                                                             
         ZAP   RECACC1,=P'0'                                                    
         B     PR60                                                             
*                                                                               
PR60     DS    0H                                                               
         CLI   TABPDAT,C'N'        PRINT THE KEYS                               
         BE    PR65                                                             
         LA    R3,P                RESET POINTER                                
         MVC   PDAT1,RECDAT1       KEY DATA 1 TO DISPLAY LINE                   
         MVC   PDAT2,RECDAT2       KEY DATA 2 TO DISPLAY LINE                   
         OC    RECUNAME,RECUNAME                                                
         BZ    *+10                                                             
         MVC   PDATUSEN,RECUNAME   PUT USE NAME INSTEAD OF USE CODE             
*                                                                               
PR65     LA    R3,P                R3=A(PRINT LINE)                             
         LH    RF,TABORTN          RF=DISPLACEMENT TO OUTPUT ROUTINE            
         AR    RF,RB                                                            
         BASR  RE,RF               *** GO TO OUTPUT ROUTINE ***                 
*                                                                               
PR70     DS    0H                                                               
         BAS   RE,PRNTIT                                                        
         LA    R2,RECNEXT          BUMP TO NEXT RECORD                          
         BCT   R0,PR10             AND PROCESS                                  
*                                                                               
         BAS   RE,PRNTIT           PRINT WHAT'S LEFT                            
*                                                                               
PRX      B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*              BINSRCH RECORD OUTPUT ROUTINES                                   
*                                                                               
         USING RECD,R2             R2=A(RECORD)                                 
         USING PRINTD,R3           R3=A(PRINT LINE)                             
*                                                                               
BINOUT1  NTR1                      FOR A COUNTER                                
         LA    R4,PCNT                                                          
         BAS   RE,NUMOUT1                                                       
         B     XIT                                                              
*                                                                               
BINOUT2  NTR1                      FOR AN ACCUMULATOR                           
         BAS   RE,BINOUT1                                                       
         ZAP   DUB,RECACC1                                                      
         LA    R4,PACC1                                                         
         BAS   RE,ACCOUT                                                        
         B     XIT                                                              
*                                                                               
BINOUT2C NTR1                      FOR 2 COUNTERS                               
         BAS   RE,BINOUT1                                                       
         LA    R4,PACC1                                                         
         BAS   RE,NUMOUT2                                                       
         B     XIT                                                              
*                                                                               
BINOUT3  NTR1                      FOR 2 COUNTERS AND 1 ACCUMULATOR             
         BAS   RE,BINOUT2C                                                      
         ZAP   DUB,RECACC2                                                      
         LA    R4,PACC2                                                         
         BAS   RE,ACCOUT                                                        
         B     XIT                                                              
*                                                                               
BINOUT4  NTR1                      FOR 3 ACCUMULATORS                           
         ZAP   DUB,RECACC1                                                      
         LA    R4,PACC1                                                         
         BAS   RE,ACCOUT                                                        
*                                                                               
         ZAP   DUB,RECACC2                                                      
         LA    R4,PACC2                                                         
         BAS   RE,ACCOUT                                                        
*                                                                               
         ZAP   DUB,RECACC3                                                      
         LA    R4,PACC3                                                         
         BAS   RE,ACCOUT                                                        
         B     XIT                                                              
*                                                                               
BINOUT5  NTR1                      CALCULATE AVERAGE                            
         OC    RECDAT1,RECDAT1                                                  
         BNZ   BINOUT5D                                                         
         EDIT  COMMCNT,(10,PCNT),COMMAS=YES,ZERO=NOBLANK   N'COMMLS             
*                                                                               
BINOUT5D LA    R4,PACC1            EDIT RECACC1 IN PCNT                         
         BAS   RE,NUMOUT2                                                       
*                                                                               
         CP    COMMCNT,=P'0'       DON'T DIVIDE BY ZERO                         
         BE    BINOUT5X                                                         
         ZAP   TEMP,RECCNT         N'PERFORMERS                                 
         MP    TEMP,=P'10000'                                                   
         DP    TEMP,COMMCNT        DIVIDED BY N'COMMERCIALS                     
         SRP   TEMPQUO,64-2,5      SHIFT/ROUND QUOTIENT                         
         EDIT  TEMPQUO,(14,PACC2),2,ZERO=NOBLANK                                
*                                                                               
BINOUT5X B     XIT                                                              
*                                                                               
*                                                                               
BINOUT6  NTR1                      FOR 3 ACCUMULATORS                           
         ZAP   DUB,RECACC1                                                      
         LA    R4,PACC1                                                         
         BAS   RE,ACCOUT                                                        
*                                                                               
         ZAP   DUB,RECACC2                                                      
         LA    R4,PACC2                                                         
         BAS   RE,ACCOUT                                                        
*                                                                               
         ZAP   DUB,RECACC3                                                      
         LA    R4,PACC3                                                         
         BAS   RE,ACCOUT                                                        
*                                                                               
         LA    R4,PTOTAL                                                        
         BAS   RE,NUMOUT1                                                       
         B     XIT                                                              
*                                                                               
BINOUT7  NTR1                      FOR AN ACCUMULATOR                           
         ZAP   DUB,RECACC1                                                      
         LA    R4,PACC1                                                         
         BAS   RE,ACCOUT                                                        
         B     XIT                                                              
*                                                                               
BINOUT8  NTR1                      FOR AN ACCUMULATOR                           
         ZAP   DUB,RECACC1                                                      
         LA    R4,PCNT                                                          
         BAS   RE,ACCOUT                                                        
         B     XIT                                                              
*                                                                               
BINOUT9  NTR1                      FOR AN ACCUMULATOR                           
         LA    R4,PCNT                                                          
         BAS   RE,ACCOUT2                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CLAOUT   NTR1                                                                   
         USING PRINTD,R3                                                        
         MVC   PDAT1,=C'CLASS'                                                  
         CLC   PDAT2,=C'     '                                                  
         BNH   CLA10                                                            
         MVC   PDAT2,RECDAT2                                                    
         CLC   =C'BNY',PDAT2                                                    
         BNE   *+10                                                             
         MVC   PDAT2(4),=C'B+NY'                                                
         BAS   RE,BINOUT1          EDIT 1 COUNTER                               
         B     CLAX                                                             
*                                                                               
CLA10    MVI   PDAT2,C'A'                                                       
         BAS   RE,BINOUT1          EDIT 1 COUNTER                               
*                                                                               
CLAX     B     XIT                                                              
*                                                                               
DLROUT   NTR1                                                                   
         MVC   PDAT1,=C'TYPE '                                                  
         MVC   PDAT2,RECDAT2                                                    
         CLC   =C'ANY',PDAT2                                                    
         BNE   *+10                                                             
         MVC   PDAT2(4),=C'A+NY'                                                
         CLC   =C'BNY',PDAT2                                                    
         BNE   *+10                                                             
         MVC   PDAT2(4),=C'B+NY'                                                
         BAS   RE,BINOUT1          EDIT 1 COUNTER                               
*                                                                               
DLRX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*         MAJORS                                                                
*                                                                               
WSPOUT1  NTR1                                                                   
         PACK  DUB,WSPLAST                                                      
         CVB   R5,DUB                                                           
         CLC   WSPLAST,RECDAT1     HAVE WE PRINTED EVERYTHING UP TO NOW         
         BE    WO20                                                             
         CLC   WSPLAST,=C'000'     START FROM 0                                 
         BNE   WO10                                                             
         MVC   PQUES(3),=C'000'                                                 
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),=C'000'                                               
         CLI   SVMED,C'R'                                                       
         BNE   WO01                                                             
         BAS   RE,PRTZERO2                                                      
         B     *+8                                                              
*                                                                               
WO01     BAS   RE,PRTZERO                                                       
         LA    R5,1                                                             
         MVC   WSPLAST,=C'001'                                                  
*                                                                               
WO05     CLC   WSPLAST,RECDAT1     HAVE WE PRINTED EVERYTHING UP TO NOW         
         BE    WO20                                                             
*                                                                               
WO10     BAS   RE,EDPR5                                                         
         MVC   PQUES(3),WORK                                                    
         AH    R5,=H'4'                                                         
         BAS   RE,EDPR5                                                         
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),WORK                                                  
         CLI   SVMED,C'R'                                                       
         BNE   WO15                                                             
         BAS   RE,PRTZERO2                                                      
         B     *+8                                                              
*                                                                               
WO15     BAS   RE,PRTZERO                                                       
         LA    R5,1(R5)            BUMP BY 5 TOTAL                              
         BAS   RE,EDPR5                                                         
         MVC   WSPLAST,WORK                                                     
         B     WO05                                                             
*                                                                               
WO20     MVC   PQUES(3),RECDAT1                                                 
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),RECDAT2                                               
         BAS   RE,BINOUT1          EDIT 1 COUNTER                               
         CLI   SVMED,C'R'                                                       
         BNE   *+8                                                              
         BAS   RE,RWSPOUT1         EDIT 2 COUNTERS                              
         CLC   =C'000',RECDAT1     IS THIS THE FIRST ONE                        
         BNE   WO30                                                             
         LA    R5,1(R5)            BUMP BY 1 TOTAL                              
         B     *+8                                                              
*                                                                               
WO30     LA    R5,5(R5)            BUMP BY 5 TOTAL                              
         BAS   RE,EDPR5                                                         
         MVC   WSPLAST,WORK                                                     
         MVC   LSTWSPCD,RECCDE     SET THE REC CODE                             
         LA    R2,RECNEXT          LOOK AHEAD TO NEXT CODE                      
         CLC   LSTWSPCD,RECCDE     HAS THE REC CODE CHANGED                     
         BE    WSPX                                                             
         BAS   RE,PRNTIT                                                        
         BAS   RE,WSPEND           FINISH PRINTING OUT REST OF UNITS            
         MVC   WSPLAST,=C'000'     START FROM THE BEGINING                      
*                                                                               
WSPX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FINISH PRINTING OUT THIS MAJORS UNITS                                  
*                                                                               
WSPEND   NTR1                                                                   
         MVC   MYBYTE2,LSTWSPCD                                                 
         BAS   RE,SETMAX           GET MAX NUM TO  PRINT IN FULL                
         EDIT  FULL,(3,WORK),ZERO=NOBLANK,FILL=0                                
         MVC   MAXEND,WORK                                                      
         PACK  DUB,WSPLAST                                                      
         CVB   R5,DUB                                                           
         LTR   R5,R5                                                            
         BZ    WE01                                                             
         BAS   RE,EDPR5                                                         
         MVC   WSPLAST,WORK                                                     
*                                                                               
WE01     CLC   WSPLAST,MAXEND      HAVE WE PRINTED EVERYTHING UP TO NOW         
         BE    WE20                                                             
         BH    WEX                                                              
         CLC   WSPLAST,=C'000'     START FROM 0                                 
         BNE   WE10                                                             
         MVC   PQUES(3),=C'000'                                                 
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),=C'000'                                               
         CLI   SVMED,C'R'                                                       
         BNE   WE02                                                             
         BAS   RE,PRTZERO2                                                      
         B     *+8                                                              
*                                                                               
WE02     BAS   RE,PRTZERO                                                       
         LA    R5,1                                                             
         BAS   RE,EDPR5                                                         
         MVC   WSPLAST,WORK                                                     
*                                                                               
WE05     CLC   WSPLAST,MAXEND      HAVE WE PRINTED EVERYTHING                   
         BE    WE20                                                             
*                                                                               
WE10     BAS   RE,EDPR5                                                         
         MVC   PQUES(3),WORK                                                    
         AH    R5,=H'4'                                                         
         BAS   RE,EDPR5                                                         
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),WORK                                                  
         CLI   SVMED,C'R'                                                       
         BNE   WE15                                                             
         BAS   RE,PRTZERO2                                                      
         B     *+8                                                              
*                                                                               
WE15     BAS   RE,PRTZERO                                                       
         LA    R5,1(R5)            BUMP BY 5                                    
         BAS   RE,EDPR5                                                         
         MVC   WSPLAST,WORK                                                     
         B     WE05                                                             
*                                                                               
WE20     MVC   PQUES(3),WSPLAST                                                 
         MVI   PQUES+4,C'-'                                                     
         MVC   PQUES+6(3),=C'&&UP'                                              
         CLI   SVMED,C'R'                                                       
         BNE   WE30                                                             
         BAS   RE,PRTZERO2                                                      
         B     *+8                                                              
*                                                                               
WE30     BAS   RE,PRTZERO                                                       
*                                                                               
WEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*         CLASS A USES                                                          
*                                                                               
         USING RECD,R2                                                          
         USING TABLED,R4                                                        
         USING PRINTD,R3                                                        
CLAUOUT  NTR1                                                                   
         MVC   WORK(4),CLALAST                                                  
         OC    WORK(4),=C'0000'                                                 
         PACK  DUB,WORK(4)                                                      
         CVB   R5,DUB                                                           
*                                                                               
CU10     CLC   CLALAST,RECDAT1     HAVE WE PRINTED EVERYTHING UP TO NOW         
         BE    CU20                                                             
         MVC   PQUES(3),=C'USE'                                                 
         MVC   PQUES+6(4),CLALAST                                               
         BAS   RE,PRTZERO                                                       
         LA    R5,1(R5)                                                         
         EDIT  (R5),(4,CLALAST)                                                 
         B     CU10                                                             
*                                                                               
CU20     CH    R5,=H'105'          IF REACHED LAST ONE THEN GO TO END           
         BNL   CU30                                                             
         MVC   PQUES(3),=C'USE'                                                 
         MVC   PQUES+6(4),RECDAT1                                               
         BAS   RE,BINOUT3          EDIT 3 FIELDS                                
         AP    MYRUNTOT,RECACC2    ACCUMULATE RUNNING TOTAL                     
         CP    MYRUNTOT,RECACC2                                                 
         BE    CU25                DON'T BOTHER PRINTING IT IF 1ST LINE         
         ZAP   DUB,MYRUNTOT                                                     
         LR    R0,R4               SAVE R4 IN R0                                
         LA    R4,PACC3                                                         
         BAS   RE,ACCOUT           PRINT RUNNING TOTAL                          
         LR    R4,R0               RESTORE R4                                   
*                                                                               
CU25     LA    R5,1(R5)            BUMP BY 1 TOTAL                              
         EDIT  (R5),(4,CLALAST)                                                 
*                                                                               
         LA    R2,RECNEXT          LOOK AHEAD TO NEXT CODE                      
         CLC   TABCDE,RECCDE       HAS THE REC CODE CHANGED                     
         BE    CUX                                                              
         BAS   RE,PRNTIT                                                        
CU30     BAS   RE,CLAEND           FINISH PRINTING OUT REST OF USES             
*                                                                               
CUX      B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*        FINISH PRINTING OUT CLASS A USES                                       
*                                                                               
         USING RECD,R2                                                          
         USING PRINTD,R3                                                        
*                                  R5=CLALAST                                   
CLAEND   NTR1                                                                   
         CLC   RECDAT1(4),=C' 105' IF WE HAVE LAST RECORD THEN PRINT IT         
         BNE   CE10                                                             
         MVC   PQUES(3),=C'USE'                                                 
         MVC   PQUES+6(4),RECDAT1                                               
         MVC   PQUES+11(4),=C'&& UP'                                            
         ZAP   RECACC1,=P'0'                                                    
         BAS   RE,BINOUT3          EDIT 2 COUNTERS AND 1 ACCUM.                 
         B     CEX                                                              
*                                                                               
CE10     MVC   PQUES(3),=C'USE'                                                 
         MVC   PQUES+6(4),CLALAST                                               
         CH    R5,=H'105'                                                       
         BNE   *+10                                                             
         MVC   PQUES+11(4),=C'&& UP'                                            
         BAS   RE,PRTZERO                                                       
         LA    R5,1(R5)            BUMP BY 1                                    
         EDIT  (R5),(4,CLALAST)                                                 
         CH    R5,=H'105'                                                       
         BNH   CE10                                                             
*                                                                               
CEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        INITIALIZE RECORD                                                      
*                                                                               
CLRREC   NTR1                                                                   
         USING RECD,R2             R2=A(RECORD)                                 
         LA    R2,RECORD                                                        
         XC    RECORD,RECORD       RE-INITIALIZE RECORD                         
         ZAP   RECCNT,=P'0'                                                     
         ZAP   RECACC1,=P'0'                                                    
         ZAP   RECACC2,=P'0'                                                    
         ZAP   RECACC3,=P'0'                                                    
         B     XIT                                                              
*                                                                               
*        PRINT OUT ZERO                                                         
*                                                                               
PRTZERO  NTR1                                                                   
         USING PRINTD,R3                                                        
         LA    RF,0                                                             
         LA    R4,PCNT                                                          
         BAS   RE,NUMOUT                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        PRINT OUT ZERO                                                         
*                                                                               
PRTZERO2 NTR1                                                                   
         LA    RF,0                                                             
         LA    R4,PCNT                                                          
         BAS   RE,NUMOUT                                                        
*                                                                               
         LA    RF,0                                                             
         LA    R4,PACC1+9                                                       
         BAS   RE,NUMOUT                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        PRINT OUT 2ND COUNTER                                                  
*                                                                               
         USING RECD,R2                                                          
         USING PRINTD,R3                                                        
RWSPOUT1 NTR1                                                                   
         LA    R4,PACC1+5                                                       
         BAS   RE,NUMOUT2                                                       
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
RWSPOUT2 NTR1                      FOR 2 COUNTERS                               
         BAS   RE,BINOUT1                                                       
         BAS   RE,RWSPOUT1                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
EDPR5    DS    0H                  EDIT NUMBERS                                 
         EDIT  (R5),(3,WORK),ZERO=NOBLANK,FILL=0                                
         BR    RE                                                               
*                                                                               
NUMOUT   DS    0H                  EDIT COUNTERS                                
         EDIT  (RF),(10,0(R4)),COMMAS=YES,ZERO=NOBLANK                          
         BR    RE                                                               
*                                                                               
         USING RECD,R2                                                          
NUMOUT1  DS    0H                  EDIT COUNTERS                                
         EDIT  RECCNT,(10,0(R4)),COMMAS=YES,ZERO=NOBLANK                        
         BR    RE                                                               
*                                                                               
NUMOUT2  DS    0H                  EDIT NUMBERS                                 
         EDIT  RECACC1,(14,0(R4)),COMMAS=YES,ZERO=NOBLANK                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              BINSRCH RECORD OUTPUT ROUTINES  (CONT'D)                         
*                                                                               
ACCOUT   DS    0H                  EDIT ACCUMULATORS                            
         EDIT  (P8,DUB),(14,0(R4)),2,COMMAS=YES,ZERO=NOBLANK,FLOAT=-            
         BR    RE                                                               
*                                                                               
         USING RECD,R2                                                          
ACCOUT2  DS    0H                  EDIT ACCUMULATORS                            
         EDIT  RECCNT,(14,0(R4)),COMMAS=YES,ZERO=NOBLANK                        
         BR    RE                                                               
         EJECT                                                                  
*              CLEAR COML LEVEL TABLES                                          
         SPACE 1                                                                
CLRTABS  NTR1                                                                   
         L     RE,=A(WSPTAB)                                                    
         LH    RF,=Y(L'WSPTAB)                                                  
         XCEFL                                                                  
         L     RE,=A(USECTAB)                                                   
         LH    RF,=Y(L'USECTAB)                                                 
         XCEFL                                                                  
         L     RE,=A(CLATAB)                                                    
         LH    RF,=Y(L'CLATAB)                                                  
         XCEFL                                                                  
         L     RE,=A(RWSPTAB)                                                   
         LH    RF,=Y(L'RWSPTAB)                                                 
         XCEFL                                                                  
         L     RE,=A(RUSETAB)                                                   
         LH    RF,=Y(L'RUSETAB)                                                 
         XCEFL                                                                  
         B     XIT                                                              
*                                                                               
*              ROUTINE TO PRINT A LINE                                          
*                                                                               
PRNTIT   NTR1                                                                   
         L     R5,ASPOOLD          R5=A(SPOOL DSECT)                            
         USING SPOOLD,R5                                                        
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
*              ROUTINE TO PRINT A LINE                                          
*                                                                               
SETPLINE NTR1                                                                   
         L     R5,ASPOOLD          R5=A(SPOOL DSECT)                            
         USING SPOOLD,R5                                                        
         LA    R3,P                R3=A(PRINT LINE)                             
         XIT1  REGS=(R3)                                                        
         DROP  R5                                                               
*                                                                               
*              ROUTINE TO SKIP TO NEW PAGE                                      
*                                                                               
NEWPAGE  NTR1                                                                   
         L     R5,ASPOOLD          R5=A(SPOOL DSECT)                            
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
*              ROUTINE TO TRACE RECORDS                                         
*                                                                               
MYTRACE  NTR1                                                                   
         TM    QUOPTS,QUTRACE      TEST TRACE ENABLED                           
         BZ    MYTRACEX                                                         
         LM    R2,R4,0(R1)         R2=A(LITERAL),R3=A(IOAREA),R4=A(LEN)         
         ZIC   RF,0(R1)            RF=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),(R4),(R2),(RF)                                   
*                                                                               
MYTRACEX B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R5,ASPOOLD          R5=A(SPOOL DSECT)                            
         USING SPOOLD,R5                                                        
         MVC   H2+7(10),TGMENAME   MEDIA NAME                                   
         MVC   H3+107(17),QUPERIOD PERIOD                                       
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
*                                                                               
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 ERREX                                                            
*                                                                               
YES      SR    RC,RC               SET CONDITION CODES                          
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*              USES TO EXCLUDE FOR SESSIONS                                     
*                                                                               
EXCLUSES DC    AL1(URRS,USRS,URRR,UADC,UTAG,UAUD,UCAU,UNBS,UCNL,UPPF)           
         DC    AL1(USCN)                                                        
NEXCLUDE EQU   *-EXCLUSES                                                       
*                                                                               
*              USES TO INCLUDE FOR SESSIONS                                     
*                                                                               
INCLUSES DC    AL1(ULFT,USLF)                                                   
NINCLUDE EQU   *-INCLUSES                                                       
*                                                                               
*              USES TO EXCLUDE FOR REUSE                                        
*                                                                               
EXCLURE  DC    AL1(ULFT,USLF)                                                   
NEXCLURE EQU   *-EXCLURE                                                        
*                                                                               
*              USES TO EXCLUDE FOR REUSE                                        
*                                                                               
INCFOR   DC    AL1(UFGN,UFGS,UFGR)                                              
NINCFOR  EQU   *-INCFOR                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18                   
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H2,2,C'MEDIA'                                                    
         SSPEC H3,100,C'PERIOD'                                                 
*                                                                               
         SSPEC H2,50,C'TALENT QUESTIONNAIRE'                                    
         SSPEC H3,50,C'--------------------'                                    
*                                                                               
         SPROG 1                                                                
         SSPEC H5,2,C'BREAKDOWN OF TOTAL TV DOLLARS PAID TO SCALE'              
         SSPEC H6,2,C'AND OVERSCALE PERFORMERS BY USE TYPE'                     
         SSPEC H7,2,C'-------------------------------------------'              
*                                                                               
         SPROG 2                                                                
         SSPEC H5,2,C'TOTAL TV SESSION DOLLARS PAID BY CAST TYPE'               
         SSPEC H6,2,C'------------------------------------------'               
*                                                                               
         SPROG 1,2,12,13                                                        
         SSPEC M1,85,C'SCALE         OVERSCALE       TOTAL'                     
         SSPEC M2,85,C'-----         ---------       -----'                     
*                                                                               
         SPROG 3                                                                
         SSPEC H5,2,C'BREAKDOWN OF AVERAGE NUM OF CAST IN COMMERCIAL'           
         SSPEC H5,48,C' BY CAT'                                                 
         SSPEC H6,2,C'----------------------------------------------'           
         SSPEC H6,48,C'-------'                                                 
*                                                                               
         SSPEC M1,71,C'TOT COMM    TOT CAST COUNT  AVERAGE COUNT'               
         SSPEC M2,71,C'--------    --------------  -------------'               
*                                                                               
         SPROG 4                                                                
         SSPEC H5,2,C'BREAKDOWN OF CYCLES OF TV PROGRAM USE'                    
         SSPEC H5,39,C' BY CLASS TYPE'                                          
         SSPEC H6,2,C'-------------------------------------'                    
         SSPEC H6,39,C'--------------'                                          
*                                                                               
         SPROG 5                                                                
         SSPEC H5,2,C'BREAKDOWN OF CYCLES OF TV DEALER USE'                     
         SSPEC H5,38,C' BY CLASS TYPE'                                          
         SSPEC H6,2,C'------------------------------------'                     
         SSPEC H6,38,C'--------------'                                          
*                                                                               
         SPROG 6,7,8,9,10                                                       
         SSPEC H5,2,C'BREAKDOWN OF TV SPOT USE BY MAJOR AREA AND '              
         SSPEC H5,44,C' UNIT GROUP'                                             
         SSPEC H6,2,C'------------------------------------------ '              
         SSPEC H6,44,C'-----------'                                             
*                                                                               
         SPROG 14,15,16,17,18                                                   
         SSPEC H5,2,C'BREAKDOWN OF RADIO SPOT USE BY MAJOR AREA AND '           
         SSPEC H5,47,C' UNIT GROUP'                                             
         SSPEC H6,2,C'--------------------------------------------- '           
         SSPEC H6,47,C'-----------'                                             
*                                                                               
         SPROG 6,14                                                             
         SSPEC H8,2,C'NO MAJOR CITIES - TABLE A'                                
         SSPEC H9,2,C'-------------------------'                                
*                                                                               
         SPROG 7,15                                                             
         SSPEC H8,2,C'NEW YORK - TABLE B'                                       
         SSPEC H9,2,C'------------------'                                       
*                                                                               
         SPROG 8,16                                                             
         SSPEC H8,2,C'CHICAGO OR L.A. - TABLE C'                                
         SSPEC H9,2,C'-------------------------'                                
*                                                                               
         SPROG 9,17                                                             
         SSPEC H8,2,C'ANY TWO OF N.Y., CHICAGO OR L.A. - TABLE D'               
         SSPEC H9,2,C'------------------------------------------'               
*                                                                               
         SPROG 10,18                                                            
         SSPEC H8,2,C'ALL THREE OF N.Y., CHICAGO AND L.A. - TABLE E'            
         SSPEC H9,2,C'---------------------------------------------'            
*                                                                               
         SPROG 6,7,8,9,10                                                       
         SSPEC M1,2,C'GROUP OF UNITS'                                           
         SSPEC M2,2,C'--------------'                                           
         SSPEC M1,69,C'NO. OF CYCLES'                                           
         SSPEC M2,69,C'-------------'                                           
*                                                                               
         SPROG 14,15,16,17,18                                                   
         SSPEC M1,2,C'GROUP OF UNITS'                                           
         SSPEC M2,2,C'--------------'                                           
         SSPEC M1,69,C'13 WEEK CYCLES'                                          
         SSPEC M2,69,C'--------------'                                          
         SSPEC M1,90,C'8 WEEK CYCLES'                                           
         SSPEC M2,90,C'-------------'                                           
*                                                                               
         SPROG 11                                                               
         SSPEC H8,2,C'BREAKDOWN OF TV CLASS A USES'                             
         SSPEC H9,2,C'-----------------------------'                            
         SSPEC H8,73,C'CYCLES=    CYCLES>=        $=     '                      
         SSPEC H9,73,C'-------    --------   ------------'                      
         SSPEC H8,111,C'$CUMULATIVE'                                            
         SSPEC H9,111,C'-----------'                                            
*                                                                               
         SPROG 12                                                               
         SSPEC H5,2,C'BREAKDOWN OF TOTAL RADIO DOLLARS PAID TO SCALE'           
         SSPEC H6,2,C'AND OVERSCALE PERFORMERS BY USE TYPE'                     
         SSPEC H7,2,C'----------------------------------------------'           
*                                                                               
         SSPEC M1,123,C'TOT CYCLES'                                             
         SSPEC M2,123,C'----------'                                             
*                                                                               
         SPROG 13                                                               
         SSPEC H5,2,C'TOTAL RADIO SESSION DOLLARS PAID BY CAST TYPE'            
         SSPEC H6,2,C'---------------------------------------------'            
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              TABLE TO COVER BINSRCH PARAMETERS                                
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**BINL**'                                                    
BINLIST  DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(RECLNQ)         RECORD LENGTH                                
         DC    AL4(RECLKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(NRECS)          MAX. NUMBER OF RECORDS                       
         DC    AL1(NRECACCS)       NUMBER OF BUCKETS                            
         DC    AL1(RECDSP)         DISP TO BUCKETS                              
         DC    X'00'               X'80'=BINARY BUCKETS                         
         DC    AL1(0)                                                           
         DC    (RECLNQ*NRECS)X'00' TABLE                                        
NRECS    EQU   3000                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                  RADIO WSP TABLE                              
         DC    CL8'RWSP TAB'                                                    
RWSPTAB  DS    CL(20*RWSPTBLN)                                                  
RWSPTABX DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0D                  WSP TABLE                                    
         DC    CL8'*WSP TAB'                                                    
WSPTAB   DS    CL(12*WSPTABLN)                                                  
WSPTABX  DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*              CLASS A USES TABLE                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*CLA TAB'                                                    
CLATAB   DS    CL(12*CLATABLN)                                                  
CLATABX  DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*              USE BY CLASS TYPE TABLE                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*USE CLA'                                                    
USECTAB  DS    CL(40*USECTBLN)                                                  
USECTABX DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*        RADIO USE TYPE TABLE                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'RUSE TAB'                                                    
RUSETAB  DS    CL(80*RUSETBLN)                                                  
RUSETABX DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*              QUESTION TABLE                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'**TABLE*'                                                    
TABLE    DS    0F                                                               
*                                                                               
T10      DC    AL1(T10X-T10,TLINCDQ)                                            
         DC    C'T',X'10',AL2(SESSALL-T70348,BINOUT1-T70348)                    
         DC    CL3'  1',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T10X-(*+1))                                                  
         DC    C'TOTAL TV SESSION COMMERCIALS'                                  
T10X     EQU   *                                                                
*                                                                               
T15      DC    AL1(T15X-T15,TLINCDQ)                                            
         DC    C'T',X'11',AL2(REUALL-T70348,BINOUT1-T70348)                     
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T15X-(*+1))                                                  
         DC    C'TOTAL TV REUSE COMMERCIALS'                                    
T15X     EQU   *                                                                
*                                                                               
T20      DC    AL1(T20X-T20,TLCKCDQ)                                            
         DC    C'T',X'12',AL2(APPLSES-T70348,BINOUT1-T70348)                    
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T20X-(*+1))                                                  
         DC    C'TOTAL TV REUSE COMMERCIALS APPLIED TO SESSION'                 
T20X     EQU   *                                                                
*                                                                               
T25      DC    AL1(T25X-T25,TLINCDQ)                                            
         DC    C'T',X'13',AL2(HLDFEE-T70348,BINOUT1-T70348)                     
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T25X-(*+1))                                                  
         DC    C'TOTAL TV HOLDING FEE COMMERCIALS'                              
T25X     EQU   *                                                                
*                                                                               
T30      DC    AL1(T30X-T30,TLCKCDQ)                                            
         DC    C'T',X'14',AL2(APPLHF-T70348,BINOUT1-T70348)                     
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T30X-(*+1))                                                  
         DC    C'TOTAL TV REUSE COMMERCIALS - HOLDING FEE APPLIED'              
T30X     EQU   *                                                                
*                                                                               
T35      DC    AL1(T35X-T35,TLINCDQ)                                            
         DC    C'T',X'15',AL2(FOREIGN-T70348,BINOUT1-T70348)                    
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T35X-(*+1))                                                  
         DC    C'TOTAL TV FOREIGN USE COMMERCIALS'                              
T35X     EQU   *                                                                
*                                                                               
T40      DC    AL1(T40X-T40,TLCKCDQ)                                            
         DC    C'T',X'20',AL2(SCALE-T70348,BINOUT7-T70348)                      
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T40X-(*+1))                                                  
         DC    C'TOTAL TV DOLLARS PAID TO SCALE PERFORMERS'                     
T40X     EQU   *                                                                
*                                                                               
T41      DC    AL1(T41X-T41,TLCKCDQ)                                            
         DC    C'T',X'21',AL2(0,BINOUT7-T70348)                                 
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T41X-(*+1))                                                  
         DC    C'TOTAL TV DOLLARS PAID TO OVERSCALE PERFORMERS'                 
T41X     EQU   *                                                                
*                                                                               
T4F      DC    AL1(T4FX-T4F,0)                                                  
         DC    C'T',X'2F',AL2(0,BINOUT7-T70348)                                 
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T4FX-(*+1))                                                  
         DC    C'TOTAL TV DOLLARS PAID TO ALL PERFORMERS'                       
T4FX     EQU   *                                                                
*                                                                               
T50      DC    AL1(T50X-T50,TLCKCDQ)                                            
         DC    C'T',X'30',AL2(USETYP-T70348,BINOUT4-T70348)                     
         DC    CL3'  4',CL1'N',CL1'Y',AL1(1,0)                                  
         DC    AL1(T50X-(*+1))                                                  
         DC    AL1(0)                                                           
T50X     EQU   *                                                                
*                                                                               
T5F      DC    AL1(T5FX-T5F,0)                                                  
         DC    C'T',X'3F',AL2(0,BINOUT4-T70348)                                 
         DC    CL3'  4',CL1'N',CL1'N',AL1(1,0)                                  
         DC    AL1(T5FX-(*+1))                                                  
         DC    C'TOTALS'                                                        
T5FX     EQU   *                                                                
*                                                                               
T65      DC    AL1(T65X-T65,TLCKCDQ)                                            
         DC    C'T',X'45',AL2(CATTYP-T70348,BINOUT4-T70348)                     
         DC    CL3'  5',CL1'N',CL1'Y',AL1(2,0)                                  
         DC    AL1(T65X-(*+1))                                                  
         DC    AL1(0)                                                           
T65X     EQU   *                                                                
*                                                                               
T6F      DC    AL1(T6FX-T6F,0)                                                  
         DC    C'T',X'4F',AL2(0,BINOUT4-T70348)                                 
         DC    CL3'  5',CL1'N',CL1'N',AL1(2,0)                                  
         DC    AL1(T6FX-(*+1))                                                  
         DC    C'TOTAL TV SESSION DOLLARS'                                      
T6FX     EQU   *                                                                
*                                                                               
T70      DC    AL1(T70X-T70,TLCACDQ)                                            
         DC    C'T',X'50',AL2(AVGCOM-T70348,BINOUT5-T70348)                     
         DC    CL3'  7',CL1'N',CL1'Y',AL1(3,0)                                  
         DC    AL1(T70X-(*+1))                                                  
         DC    AL1(0)                                                           
T70X     EQU   *                                                                
*                                                                               
T72      DC    AL1(T72X-T72,TLCACDQ)                                            
         DC    C'T',X'52',AL2(0,BINOUT5-T70348)                                 
         DC    CL3'  7',CL1'N',CL1'N',AL1(3,0)                                  
         DC    AL1(T72X-(*+1))                                                  
         DC    C'TOTAL AVERAGE CAST SIZE'                                       
T72X     EQU   *                                                                
*                                                                               
T75      DC    AL1(T75X-T75,TLINCDQ)                                            
         DC    C'T',X'55',AL2(CLATYP-T70348,CLAOUT-T70348)                      
         DC    CL3'  8',CL1'N',CL1'Y',AL1(4,0)                                  
         DC    AL1(T75X-(*+1))                                                  
         DC    AL1(0)                                                           
T75X     EQU   *                                                                
*                                                                               
T7F      DC    AL1(T7FX-T7F,TLINCDQ)                                            
         DC    C'T',X'5F',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'  8',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T7FX-(*+1))                                                  
         DC    C'TOTAL PROGRAM CYCLES'                                          
T7FX     EQU   *                                                                
*                                                                               
T80      DC    AL1(T80X-T80,TLINCDQ)                                            
         DC    C'T',X'60',AL2(DLRTYP-T70348,DLROUT-T70348)                      
         DC    CL3'  9',CL1'N',CL1'Y',AL1(5,0)                                  
         DC    AL1(T80X-(*+1))                                                  
         DC    AL1(0)                                                           
T80X     EQU   *                                                                
*                                                                               
T8F      DC    AL1(T8FX-T8F,TLINCDQ)                                            
         DC    C'T',X'6F',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'  9',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(T8FX-(*+1))                                                  
         DC    C'TOTAL DEALER CYCLES'                                           
T8FX     EQU   *                                                                
*                                                                               
T90      DC    AL1(T90X-T90,TLINCDQ)                                            
         DC    C'T',X'70',AL2(MAJOR-T70348,WSPOUT1-T70348)                      
         DC    CL3'10A',CL1'N',CL1'Y',AL1(6),CL1'N'                             
         DC    AL1(T90X-(*+1))                                                  
         DC    AL1(0)                                                           
T90X     EQU   *                                                                
*                                                                               
T91      DC    AL1(T91X-T91,TLINCDQ)                                            
         DC    C'T',X'71',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'10A',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T91X-(*+1))                                                  
         DC    C'TOTAL NO MAJORS'                                               
T91X     EQU   *                                                                
*                                                                               
T92      DC    AL1(T92X-T92,TLINCDQ)                                            
         DC    C'T',X'72',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3'10B',CL1'N',CL1'Y',AL1(7),CL1'N'                             
         DC    AL1(T92X-(*+1))                                                  
         DC    AL1(0)                                                           
T92X     EQU   *                                                                
*                                                                               
T93      DC    AL1(T93X-T93,TLINCDQ)                                            
         DC    C'T',X'73',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'10B',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T93X-(*+1))                                                  
         DC    C'TOTAL NEW YORK'                                                
T93X     EQU   *                                                                
*                                                                               
T94      DC    AL1(T94X-T94,TLINCDQ)                                            
         DC    C'T',X'74',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3'10C',CL1'N',CL1'Y',AL1(8),CL1'N'                             
         DC    AL1(T94X-(*+1))                                                  
         DC    AL1(0)                                                           
T94X     EQU   *                                                                
*                                                                               
T95      DC    AL1(T95X-T95,TLINCDQ)                                            
         DC    C'T',X'75',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'10C',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T95X-(*+1))                                                  
         DC    C'TOTAL CHICAGO OR LA'                                           
T95X     EQU   *                                                                
*                                                                               
T96      DC    AL1(T96X-T96,TLINCDQ)                                            
         DC    C'T',X'76',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3'10D',CL1'N',CL1'Y',AL1(9),CL1'N'                             
         DC    AL1(T96X-(*+1))                                                  
         DC    AL1(0)                                                           
T96X     EQU   *                                                                
*                                                                               
T97      DC    AL1(T97X-T97,TLINCDQ)                                            
         DC    C'T',X'77',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'10D',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T97X-(*+1))                                                  
         DC    C'TOTAL ANY TWO MAJORS'                                          
T97X     EQU   *                                                                
*                                                                               
T98      DC    AL1(T98X-T98,TLINCDQ)                                            
         DC    C'T',X'78',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3'10E',CL1'N',CL1'Y',AL1(10),CL1'N'                            
         DC    AL1(T98X-(*+1))                                                  
         DC    AL1(0)                                                           
T98X     EQU   *                                                                
*                                                                               
T99      DC    AL1(T99X-T99,TLINCDQ)                                            
         DC    C'T',X'79',AL2(0,BINOUT1-T70348)                                 
         DC    CL3'10E',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T99X-(*+1))                                                  
         DC    C'TOTAL N.Y., CHICAGO AND L.A.'                                  
T99X     EQU   *                                                                
*                                                                               
T9F      DC    AL1(T9FX-T9F,TLINCDQ)                                            
         DC    C'T',X'7F',AL2(0,BINOUT1-T70348)                                 
         DC    CL3' 10',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(T9FX-(*+1))                                                  
         DC    C'GRAND TOTAL - ALL CYCLES'                                      
T9FX     EQU   *                                                                
*                                                                               
TA0      DC    AL1(TA0X-TA0,TLINCDQ)                                            
         DC    C'T',X'80',AL2(CLAUSES-T70348,CLAUOUT-T70348)                    
         DC    CL3' 11',CL1'N',CL1'Y',AL1(11),CL1'N'                            
         DC    AL1(TA0X-(*+1))                                                  
         DC    AL1(0)                                                           
TA0X     EQU   *                                                                
*                                                                               
TAF      DC    AL1(TAFX-TAF,TLINCDQ)                                            
         DC    C'T',X'8F',AL2(0,BINOUT3-T70348)                                 
         DC    CL3' 11',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(TAFX-(*+1))                                                  
         DC    C'ALL USES'                                                      
TAFX     EQU   *                                                                
*                                                                               
R10      DC    AL1(R10X-R10,TLINCDQ)                                            
         DC    C'R',X'90',AL2(SESSALL-T70348,BINOUT1-T70348)                    
         DC    CL3'  1',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R10X-(*+1))                                                  
         DC    C'TOTAL RADIO SESSION COMMERCIALS'                               
R10X     EQU   *                                                                
*                                                                               
R15      DC    AL1(R15X-R15,TLINCDQ)                                            
         DC    C'R',X'91',AL2(REUALL-T70348,BINOUT1-T70348)                     
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R15X-(*+1))                                                  
         DC    C'TOTAL RADIO REUSE COMMERCIALS'                                 
R15X     EQU   *                                                                
*                                                                               
R20      DC    AL1(R20X-R20,TLCKCDQ)                                            
         DC    C'R',X'92',AL2(APPLSES-T70348,BINOUT1-T70348)                    
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R20X-(*+1))                                                  
         DC    C'TOTAL RADIO REUSE COMMERCIALS APPLIED TO SESSION'              
R20X     EQU   *                                                                
*                                                                               
R25      DC    AL1(R25X-R25,TLINCDQ)                                            
         DC    C'R',X'93',AL2(FOREIGN-T70348,BINOUT1-T70348)                    
         DC    CL3'  2',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R25X-(*+1))                                                  
         DC    C'TOTAL RADIO FOREIGN USE COMMERCIALS'                           
R25X     EQU   *                                                                
*                                                                               
R30      DC    AL1(R30X-R30,TLCKCDQ)                                            
         DC    C'R',X'94',AL2(SCALE-T70348,BINOUT7-T70348)                      
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R30X-(*+1))                                                  
         DC    C'TOTAL RADIO DOLLARS PAID TO SCALE PERFORMERS'                  
R30X     EQU   *                                                                
*                                                                               
R31      DC    AL1(R31X-R31,TLCKCDQ)                                            
         DC    C'R',X'95',AL2(0,BINOUT7-T70348)                                 
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R31X-(*+1))                                                  
         DC    C'TOTAL RADIO DOLLARS PAID TO OVERSCALE PERFORMERS'              
R31X     EQU   *                                                                
*                                                                               
R3F      DC    AL1(R3FX-R3F,0)                                                  
         DC    C'R',X'9F',AL2(0,BINOUT7-T70348)                                 
         DC    CL3'  3',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(R3FX-(*+1))                                                  
         DC    C'TOTAL RADIO DOLLARS PAID TO ALL PERFORMERS'                    
R3FX     EQU   *                                                                
*                                                                               
R40      DC    AL1(R40X-R40,TLCKCDQ)                                            
         DC    C'R',X'A0',AL2(RUSETYP-T70348,BINOUT6-T70348)                    
         DC    CL3'  4',CL1'N',CL1'Y',AL1(12,0)                                 
         DC    AL1(R40X-(*+1))                                                  
         DC    AL1(0)                                                           
R40X     EQU   *                                                                
*                                                                               
R41      DC    AL1(R41X-R41,TLINCDQ)                                            
         DC    C'R',X'A2',AL2(0,BINOUT6-T70348)                                 
         DC    CL3' 4A',CL1'N',CL1'N',AL1(12,0)                                 
         DC    AL1(R41X-(*+1))                                                  
         DC    C'TOTALS'                                                        
R41X     EQU   *                                                                
*                                                                               
R45      DC    AL1(R45X-R45,TLCKCDQ)                                            
         DC    C'R',X'A5',AL2(CATTYP-T70348,BINOUT4-T70348)                     
         DC    CL3'  5',CL1'N',CL1'Y',AL1(13,0)                                 
         DC    AL1(R45X-(*+1))                                                  
         DC    AL1(0)                                                           
R45X     EQU   *                                                                
*                                                                               
R4F      DC    AL1(R4FX-R4F,0)                                                  
         DC    C'R',X'AF',AL2(0,BINOUT4-T70348)                                 
         DC    CL3'  5',CL1'N',CL1'N',AL1(13,0)                                 
         DC    AL1(R4FX-(*+1))                                                  
         DC    C'TOTAL RADIO SESSION DOLLARS'                                   
R4FX     EQU   *                                                                
*                                                                               
R50      DC    AL1(R50X-R50,TLCACDQ)                                            
         DC    C'R',X'B0',AL2(AVGCOM-T70348,BINOUT5-T70348)                     
         DC    CL3'  6',CL1'N',CL1'Y',AL1(3,0)                                  
         DC    AL1(R50X-(*+1))                                                  
         DC    AL1(0)                                                           
R50X     EQU   *                                                                
*                                                                               
R52      DC    AL1(R52X-R52,TLCACDQ)                                            
         DC    C'R',X'B2',AL2(0,BINOUT5-T70348)                                 
         DC    CL3'  6',CL1'N',CL1'N',AL1(3,0)                                  
         DC    AL1(R52X-(*+1))                                                  
         DC    C'TOTAL AVERAGE CAST SIZE'                                       
R52X     EQU   *                                                                
*                                                                               
R60      DC    AL1(R60X-R60,TLINCDQ)                                            
         DC    C'R',X'C0',AL2(RMAJOR-T70348,WSPOUT1-T70348)                     
         DC    CL3' 7A',CL1'N',CL1'Y',AL1(14),CL1'N'                            
         DC    AL1(R60X-(*+1))                                                  
         DC    AL1(0)                                                           
R60X     EQU   *                                                                
*                                                                               
R61      DC    AL1(R61X-R61,TLINCDQ)                                            
         DC    C'R',X'C1',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3' 7A',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R61X-(*+1))                                                  
         DC    C'TOTAL NO MAJORS'                                               
R61X     EQU   *                                                                
*                                                                               
R62      DC    AL1(R62X-R62,TLINCDQ)                                            
         DC    C'R',X'C2',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3' 7B',CL1'N',CL1'Y',AL1(15),CL1'N'                            
         DC    AL1(R62X-(*+1))                                                  
         DC    AL1(0)                                                           
R62X     EQU   *                                                                
*                                                                               
R63      DC    AL1(R63X-R63,TLINCDQ)                                            
         DC    C'R',X'C3',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3' 7B',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R63X-(*+1))                                                  
         DC    C'TOTAL NEW YORK'                                                
R63X     EQU   *                                                                
*                                                                               
R64      DC    AL1(R64X-R64,TLINCDQ)                                            
         DC    C'R',X'C4',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3' 7C',CL1'N',CL1'Y',AL1(16),CL1'N'                            
         DC    AL1(R64X-(*+1))                                                  
         DC    AL1(0)                                                           
R64X     EQU   *                                                                
*                                                                               
R65      DC    AL1(R65X-R65,TLINCDQ)                                            
         DC    C'R',X'C5',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3' 7C',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R65X-(*+1))                                                  
         DC    C'TOTAL CHICAGO OR LA'                                           
R65X     EQU   *                                                                
*                                                                               
R66      DC    AL1(R66X-R66,TLINCDQ)                                            
         DC    C'R',X'C6',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3' 7D',CL1'N',CL1'Y',AL1(17),CL1'N'                            
         DC    AL1(R66X-(*+1))                                                  
         DC    AL1(0)                                                           
R66X     EQU   *                                                                
*                                                                               
R67      DC    AL1(R67X-R67,TLINCDQ)                                            
         DC    C'R',X'C7',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3' 7D',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R67X-(*+1))                                                  
         DC    C'TOTAL ANY TWO MAJORS'                                          
R67X     EQU   *                                                                
*                                                                               
R68      DC    AL1(R68X-R68,TLINCDQ)                                            
         DC    C'R',X'C8',AL2(0,WSPOUT1-T70348)                                 
         DC    CL3' 7E',CL1'N',CL1'Y',AL1(18),CL1'N'                            
         DC    AL1(R68X-(*+1))                                                  
         DC    AL1(0)                                                           
R68X     EQU   *                                                                
*                                                                               
R69      DC    AL1(R69X-R69,TLINCDQ)                                            
         DC    C'R',X'C9',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3' 7E',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R69X-(*+1))                                                  
         DC    C'TOTAL N.Y., CHICAGO AND L.A.'                                  
R69X     EQU   *                                                                
*                                                                               
R6F      DC    AL1(R6FX-R6F,TLINCDQ)                                            
         DC    C'R',X'CF',AL2(0,RWSPOUT2-T70348)                                
         DC    CL3'  7',CL1'N',CL1'N',AL1(0),CL1'N'                             
         DC    AL1(R6FX-(*+1))                                                  
         DC    C'GRAND TOTAL - ALL CYCLES'                                      
R6FX     EQU   *                                                                
*                                                                               
*                                                                               
C10      DC    AL1(C10X-C10,TLCKCDQ)                                            
         DC    C'C',X'D0',AL2(CABSESS-T70348,BINOUT8-T70348)                    
         DC    CL3'  1',CL1'N',CL1'Y',AL1(0,0)                                  
         DC    AL1(C10X-(*+1))                                                  
         DC    C'TOTAL DOLLARS FOR MADE-FOR-CABLE SESSIONS'                     
C10X     EQU   *                                                                
*                                                                               
C11      DC    AL1(C11X-C11,TLINCDQ)                                            
         DC    C'C',X'D1',AL2(CABCOMM-T70348,BINOUT9-T70348)                    
         DC    CL3' 1A',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(C11X-(*+1))                                                  
         DC    C'TOTAL COMMERCIALS FOR MADE-FOR-CABLE SESSIONS'                 
C11X     EQU   *                                                                
*                                                                               
C12      DC    AL1(C12X-C12,TLCKCDQ)                                            
         DC    C'C',X'D2',AL2(CABREUS-T70348,BINOUT8-T70348)                    
         DC    CL3' 1B',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(C12X-(*+1))                                                  
         DC    C'TOTAL DOLLARS FOR MADE-FOR-CABLE REUSE'                        
C12X     EQU   *                                                                
*                                                                               
C13      DC    AL1(C13X-C13,TLCKCDQ)                                            
         DC    C'C',X'D3',AL2(CABCOM2-T70348,BINOUT9-T70348)                    
         DC    CL3' 1C',CL1'N',CL1'N',AL1(0,0)                                  
         DC    AL1(C13X-(*+1))                                                  
         DC    C'TOTAL COMMERCIALS FOR MADE-FOR-CABLE REUSE'                    
C13X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE TAREPQUESD                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC7D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDTWADCOND                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102TAREP48   02/04/10'                                      
         END                                                                    
