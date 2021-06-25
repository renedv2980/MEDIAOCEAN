*          DATA SET DDPAPTQA   AT LEVEL 004 AS OF 01/13/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTQAA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE STXITER                                                                
         TITLE 'PANAPT: REPORT ON LOAD MODULES FOR FQA LOAD LIBRARY'            
PAPTQA   CSECT                                                                  
*                                                                               
* THE INPUT TO THIS PROGRAM IS A FILE PRODUCED BY AN ICETOOL STEP.              
* SEE THE INVOKING JCL FOR DETAILS.                                             
*                                                                               
* THE ASSUMPTION IS THAT ICETOOL HAS ALREADY FILTERED OUT RECORDS               
* WHICH AREN'T OF A STATUS WE NEED. WE ALSO ASSUME THAT WE'RE ONLY              
* SEEING MOVE REQUESTS WITH A "FIRST RUN DATE" (I.E., DATE TO QA), AND          
* WHICH HAVE MEMBERS OF AN APPROPRIATE LIBCODE (I.E., LIBCODES WHICH            
* INVOLVE LOAD MODULES).                                                        
*                                                                               
* (THERE'S A DFSORT SUBTLETY AT PLAY HERE: EVEN IF THERE ARE NO INPUT           
*  RECORDS PRODUCED BY THE PRIOR ICETOOL STEP, WE STILL GET AN INPUT            
*  "HEADER" RECORD BEGINNING WITH "USERID=", BECAUSE DFSORT'S "HEADER3"         
*  AND "TRAILER3" IN THE "SECTIONS" OPERATOR GENERATE HEADER/TRAILER            
*  RECORDS UNCONDITIONALLY.)                                                    
*                                                                               
* THE INPUT RECORDS MUST BE GROUPED TOGETHER BY USERID. THIS ALLOWS US          
* TO DYNAMICALLY ALLOCATE/DEALLOCATE EACH PERSONAL PAN LIBRARY ONLY             
* ONCE FOR EACH COMPONENT MEMBERS.                                              
*                                                                               
* UPON EXIT:                                                                    
*   RC=0: THERE IS AT LEAST ONE PHASE TO PROCESS FOR FQALIB                     
*   RC=4: SUCCESSFUL COMPLETION. NOTHING FOUND TO FQALIB                        
*   RC=8: ERROR FOUND FOR AT LEAST ONE INPUT RECORD (SEE SYSPRINT)              
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PAPTQA,=V(REGSAVE),R9                                          
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         OPEN  MRFILE              INPUT FILE                                   
         OPEN  (OUTFILE,OUTPUT)                                                 
*                                                                               
GETPARM  DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'                                     
         CLC   =C'/*',CARD         ANY MORE PARAMETER CARDS?                    
         BE    GETREC              NO                                           
*                                                                               
         MVC   P(L'CARD),CARD      PRINT THE PARAMETER CARD                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    GETPARM             YES: IGNORE                                  
*                                                                               
         CLC   =C'ENV=DDS ',CARD   ENVIRONMENT                                  
         BE    GETPARM                                                          
         CLC   =C'ENV=SBX ',CARD                                                
         BNE   *+14                                                             
         MVC   ENV,=C'SBX'                                                      
         B     GETPARM                                                          
*                                                                               
         CLC   =C'DDSIO=',CARD     DDSIO=XXXXXX                                 
         BNE   *+18                                                             
         L     RF,=V(DDSIO)        RF = A(NAME OF DDSIO LOAD MODULE)            
         MVC   0(8,RF),CARD+6                                                   
         B     GETPARM                                                          
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=X                                     
         BNE   *+20                                                             
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7                                      
         B     GETPARM                                                          
*                                                                               
         J     *+2                 INVALID PARAMETER CARD!                      
*                                                                               
GETREC   DS    0H                                                               
         GET   MRFILE,MRLEN        READ ONE INPUT RECORD                        
         BAS   RE,TRACEREC         PRINT IT IN THE TRACE                        
*                                                                               
         CLC   =C'USERID=',GRPHDR  SECTION (USERID) HEADER RECORD?              
         BNE   CHKTRAIL            NO: IT'S SOMETHING ELSE                      
         CLC   GRPUSRID,SPACES     IS THIS AN EMPTY "HEADER3" RECORD?           
         BE    CLOSE               YES: JUST CLOSE OUTPUT FILE AND EXIT         
*                                                                               
* DYNAMICALLY ALLOCATE THE USER'S PERSONAL PAN LIBRARY.                         
*                                                                               
         MVC   ORIGDSN,SPACES      BUILD PANLIB DSN FROM OWNER                  
         MVC   ORIGDSN(4),=C'PAN.' STANDARD PERSONAL PANLIB HLQ                 
         LA    RF,ORIGDSN+4        POINT RF TO NEXT CHARACTER                   
         CLC   ENV,=C'SBX'                                                      
         BNE   *+14                                                             
         MVC   ORIGDSN(15),=C'PANAPT.SBX.PAN.' SBX PERSONAL PANLIB HLQS         
         LA    RF,ORIGDSN+15       POINT RF TO NEXT CHARACTER                   
*                                                                               
         LA    RE,GRPUSRID         USERID (JUST AFTER "USERID=")                
         MVC   0(1,RF),0(RE)       NEXT QUALIFIER IS MR OWNER'S USERID          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVC   0(8,RF),=C'.LIBRARY' LAST QUALIFIER IS '.LIBRARY'                
*                                                                               
*                                  44-CHAR DSN GIVEN, AND DON'T ABEND           
         GOTO1 =V(DYNALLOC),DMCB,(X'FF',=C'PANDD1  '),(X'45',ORIGDSN)           
         OC    DMCB+12(4),DMCB+12  SUCCESSFUL DYNAMIC ALLOCATION?               
         IF (NZ)                   IF NOT:                                      
           MVC   P(42),=C'*** ERROR: PANVALET LIBRARY DOES NOT EXIST'           
           GOTO1 =V(PRINTER)                                                    
           MVC   RETCODE,=F'8'       SET RC=8                                   
           DO  UNTIL=(CLC,=C'END USERID',EQ,GRPTRLR)                            
             GET   MRFILE,MRLEN    READ AND TRACE ALL REMAINING...              
             BAS   RE,TRACEREC     ...RECORDS FOR THIS USERID                   
           ENDDO ,                                                              
         ENDIF ,                                                                
         B     GETREC              GET NEXT INPUT FILE RECORD                   
*                                                                               
CHKTRAIL DS    0H                                                               
         CLC   =C'END USERID',GRPTRLR SECTION (USERID) TRAILER RECORD?          
         BNE   CHK02REC            NO: IT'S SOMETHING ELSE                      
*                                                                               
*                                  YES: CLOSE AND UNALLOCATE PANDD1             
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
         B     GETREC              GET NEXT INPUT FILE RECORD                   
*                                                                               
CHK02REC DS    0H                                                               
         CLC   MRTYPE,=C'02'       MEMBER RECORD?                               
         JNE   *+2                 NO: WHAT IS THIS RECORD ?!?                  
*&&DO                                                                           
         CLC   =C'RW ',MRNAME_RW   IS MEMBER IN A COPY FOR REWORK MR?           
         BNE   *+12                                                             
         CLI   MRNAME_DELIMITER,C':'                                            
         BE    GETREC              YES: SKIP THIS MEMBER                        
*&&                                                                             
         MVC   TESTNAME,SPACES                                                  
         MVC   BASENAME,SPACES                                                  
         MVC   BKUPNAME,SPACES                                                  
         MVC   TEMPNAME,SPACES                                                  
         MVI   XXGSW,C'N'          POSIT:  NOT APG/DPG/RRG/GEN                  
         MVI   DICTSW,C'N'         *DICT NOT YET FOUND                          
*                                                                               
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'DIR',MRFRMEM,CARD                 
         IF (CLI,DMCB+8,NE,0)      PAN BOOK FOUND?                              
           MVC   P(36),=C'*** ERROR: PANVALET MEMBER NOT FOUND'                 
           GOTO1 =V(PRINTER)                                                    
           MVC   RETCODE,=F'8'     SET RC=8                                     
           B     PUTREC            PUT AS MUCH OF THE RECORD AS WE HAVE         
         ENDIF ,                                                                
*                                                                               
* READ PAN MEMBER CONTENTS AND EXTRACT LOAD MODULE NAME                         
*                                                                               
GETPAN   DS    0H                                                               
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'PAN',MRFRMEM,CARD                 
         CLI   DMCB+8,0            GET A RECORD FROM PAN MEMBER                 
         JNE   *+2                 READ FAILED: IMPOSSIBLE                      
*                                                                               
         IF (CLC,=C'$*',EQ,CARD)   EOF REACHED ON PAN MEMBER?                   
           MVC   P(36),=C'*** ERROR: COULD NOT DERIVE LOAD MODULE NAME'         
           GOTO1 =V(PRINTER)                                                    
           MVC   RETCODE,=F'8'     SET RC=8                                     
           B     PUTREC            PUT AS MUCH OF THE RECORD AS WE HAVE         
         ENDIF ,                                                                
*                                                                               
         CLI   XXGSW,C'Y'          XXG IDENTIFIED?                              
         BE    XXGPHASE            YES: FIND PHASE CARD                         
*                                                                               
         CLC   =C'*PHASE ',CARD    IS IT A "PHASE" CARD?                        
         BE    PROG                YES: GENERATE PROG CARDS                     
*                                                                               
         CLC   =C'*DICT ',CARD     IS IT A "*DICT" CARD?                        
         BNE   CHKDONE             NO: SEE IF WE ARE DONE                       
         MVI   DICTSW,C'Y'         YES: SET "*DICT FOUND"                       
         LA    R3,CARD+6           ADVANCE PAST *DICT                           
         B     PROG10              GENERATE CONTROL CARDS                       
*                                                                               
CHKDONE  DS    0H                                                               
         CLI   DICTSW,C'Y'         WERE WE DOING *DICT CARDS?                   
         BNE   CHKAPG              NO: CHECK FOR *APG                           
         MVI   DICTSW,C'N'         YES: RESET SWITCH                            
         B     GETREC              GET NEXT INPUT FILE RECORD                   
*                                                                               
CHKAPG   DS    0H                                                               
         CLC   =C'*APG ',CARD      IS IT APG?                                   
         BNE   CHKDPG              NO: CHECK FOR DPG                            
         MVC   XXGADDR,=A(APGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKDPG   DS    0H                                                               
         CLC   =C'*DPG ',CARD      IS IT DPG?                                   
         BNE   CHKRRG              NO: CHECK FOR RRG                            
         MVC   XXGADDR,=A(DPGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKRRG   DS    0H                                                               
         CLC   =C'*RRG ',CARD      IS IT RRG?                                   
         BNE   CHKGEN              NO: CHECK FOR SCRGEN                         
         MVC   XXGADDR,=A(RRGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKGEN   DS    0H                                                               
         CLC   =C'*GEN ',CARD      IS IT SCRGEN?                                
         BNE   GETPAN              NO: GET NEXT RECORD                          
         MVC   XXGADDR,=A(GENRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR SCREEN CARD NEXT                    
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
XXGPHASE DS    0H                  SCAN FOR "PHASE"                             
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    GETPAN              YES: SKIP IT                                 
*                                                                               
         LA    R3,CARD                                                          
         L     RF,XXGADDR          ADDR OF TYPE-SPECIFIC RTN                    
         BR    RF                                                               
         EJECT                                                                  
GENRTN   DS    0H                                                               
         CLI   CARD,C'S'           SCREEN CARD?                                 
         BNE   GETPAN              GET NEXT RECORD                              
*                                                                               
         MVC   TESTNAME(4),CARD+4  START OF BASE NAME (SSPP)                    
         MVC   TESTNAME+4(2),CARD+1  BRING IN OVERLAY # (OO)                    
         MVC   BASENAME,TESTNAME   COPY BASE NAME                               
         MVC   BKUPNAME,TESTNAME   COPY BASE NAME                               
         MVI   BKUPNAME+6,C'S'     CREATE "SAVE" NAME                           
         MVC   TEMPNAME,TESTNAME   COPY BASE NAME                               
         MVI   TEMPNAME+6,C'@'     CREATE "TEMP" NAME                           
         CLI   CARD+3,C' '         TEST SUFFIX DEFAULTED?                       
         BNE   *+8                 NO: USE IT AS GIVEN                          
         MVI   CARD+3,C'A'         SUPPLY DEFAULT                               
         MVC   TESTNAME+6(1),CARD+3  MOVE IN SUFFIX CHAR                        
         B     CHKDUP              ADD LOAD MODULE NAME FIELDS TO REC.          
*                                                                               
APGRTN   DS    0H                                                               
         CLC   =C'PHASE    ',CARD  APG PHASE RECORD?                            
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME(2),=CL2'AC'                                             
         MVC   TESTNAME+2(6),CARD+9 CONSTRUCT PHASE NAME                        
         B     OTHRNM              BUILD OTHER NAMES                            
*                                                                               
DPGRTN   DS    0H                                                               
         CLC   =C'         PHASE ',CARD  DPG PHASE RECORD?                      
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME,CARD+15    RETRIEVE PHASE NAME                          
         B     OTHRNM              BUILD OTHER NAMES                            
*                                                                               
RRGRTN   DS    0H                                                               
         CLC   =C'PHASE    ',CARD  RRG PHASE RECORD?                            
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME(4),=CL4'RERG'    CONSTRUCT PHASE NAME                   
         MVC   TESTNAME+4(3),CARD+9                                             
*                                                                               
*                                  BUILD OTHER NAMES                            
*                                                                               
OTHRNM   DS    0H                  BUILD BASE AND BKUP NAMES                    
         LA    R3,TESTNAME         SEEK BLANK                                   
         SR    R1,R1               CLEAR FOR TRT                                
         TRT   TESTNAME+1(8),TRTBLANK                                           
         SR    R1,R3                                                            
         STH   R1,HALF                                                          
         BCTR  R1,0                OMIT SUFFIX LETTER                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BASENAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKUPNAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPNAME(0),TESTNAME                                             
         LH    R1,HALF                                                          
         LA    RF,BKUPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'S'          CREATE "SAVE" NAME                           
         LA    RF,TEMPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'@'          CREATE "TEMP" NAME                           
         B     CHKDUP                                                           
*                                                                               
PROG     DS    0H                                                               
         LA    R3,CARD+7           ADVANCE PAST *PHASE                          
*                                                                               
PROG10   DS    0H                  FIND START OF NAME                           
         CLI   0(R3),C' '                                                       
         BNE   *+12                FOUND FIRST NONBLANK                         
         LA    R3,1(R3)            ADVANCE POINTER                              
         B     PROG10              LOOP                                         
*                                  START OF NAME                                
         LA    R4,1(R3)                                                         
PROG22   DS    0H                                                               
         CLI   0(R4),C' '          FIND END OF NAME                             
         BE    PROG30                                                           
         CLI   0(R4),C','          FIND END OF NAME                             
         BE    PROG30                                                           
         LA    R4,1(R4)                                                         
         B     PROG22                                                           
PROG30   DS    0H                                                               
         LR    R1,R4               KEEP R4                                      
         SR    R1,R3               LENGTH OF NAME                               
         STH   R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TESTNAME(0),0(R3)                                                
*                                                                               
         LH    R1,HALF                                                          
         BCTR  R1,0                OMIT SUFFIX LETTER                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BASENAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKUPNAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPNAME(0),TESTNAME                                             
         LH    R1,HALF                                                          
         LA    RF,BKUPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'S'          CREATE "SAVE" NAME                           
         LA    RF,TEMPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'@'          CREATE "TEMP" NAME                           
         EJECT                                                                  
CHKDUP   DS    0H                                                               
*&&DO                                                                           
*                                                                               
* MAKE SURE THERE ARE NO DUPLICATE PHASE NAMES.                                 
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(1,BASENAME),BASETAB,BASECNT,8,8,      +        
               BASEMAX                                                          
         OC    DMCB(4),DMCB        TABLE IS FULL?                               
         JZ    *+2                 YES: INCREASE BASEMAX                        
         IF (CLC,BASECNT,EQ,DMCB+8) IF BASENAME *WAS* FOUND IN TABLE:           
           MVC   P(36),=C'*** ERROR: DUPLICATE BASE PHASE NAME'                 
           GOTO1 =V(PRINTER)                                                    
           MVC   RETCODE,=F'8'     SET RC=8                                     
           B     PUTREC            PUT THE DUPLICATE RECORD ANYWAY              
         ENDIF ,                                                                
         MVC   BASECNT,DMCB+8      SAVE THE CURRENT RECORD COUNT                
*&&                                                                             
*                                                                               
PUTREC   DS    0H                                                               
*                                                                               
* CREATE OUTPUT RECORDS FOR INPUT TO REXX UTILITY.                              
*                                                                               
         MVC   OUTRDW(2),=AL2(OUTRECLQ)  VARIABLE RECORD LENGTH                 
         MVC   OUTSTAMP,MRSTAMP    MOVE VERSION STAMP                           
         MVC   OUTUID,MRADDID      OWNER USERID                                 
         MVC   OUTMRNUM,MRNUM      MOVE REQUEST NUMBER                          
         MVC   OUTSTAT,MRSTAT      MOVE REQUEST STATUS                          
         MVC   OUTLEVEL,MRCLSN     CURRENT LEVEL (SHORT NAME)                   
         MVC   OUTMTYP,MRMTYP      MOVE TYPE                                    
         MVC   OUTJIRA,MRJIRA      JIRA ISSUE# (SPEC TICKET)                    
         MVC   OUTQADAT,MRRUNDA    DATE TO QA                                   
         EDIT  MRCMOVC,OUTCMOVC,ZERO=NOBLANK,FILL=0   COMPL. MOVE COUNT         
         MVC   OUTMEMBR,MRFRMEM    PANVALET "FROM" MEMBER NAME                  
*                                   ("TO" MBRNAME ALWAYS = "FROM" NAME)         
         MVC   OUTTOUDT,MRTODATA   MEMBER "TO" USER DATA                        
         MVC   OUTLIBC,MRLIBC      LIBCODE/SUBCODE                              
         MVC   OUTASNFL,MRASSNFL   ASSIGNMENT FLAG                              
         MVC   OUTVERFL,MRVERFL    VERIFY FLAG                                  
         MVC   OUTCDFLG,MRCDFLG    CONCURRENT DEVELOPMENT FLAG                  
         MVC   OUTTSTNM,TESTNAME   TEST MEMBER NAME                             
         MVC   OUTBASNM,BASENAME   DERIVED BASE NAME                            
         MVC   OUTBAKNM,BKUPNAME   DERIVED BKUP NAME                            
         MVC   OUTTMPNM,TEMPNAME   DERIVED TEMP NAME                            
         MVC   OUTDESCR,MRDESCR    MOVE REQUEST DESCRIPTION                     
         MVC   OUTORIG#,MRNAME_ORIG_MR#  REWORK: ORIGINAL MR#                   
         MVC   OUTESTOP,MRESTOP    REWORK: EARLY STOP LEVEL                     
*                                                                               
         PUT   OUTFILE,OUTREC                                                   
         MVI   PUTFLAG,C'Y'        REMEMBER THAT WE PUT SOMETHING               
*                                                                               
         MVC   P(14),=C'OUTPUT RECORD:'                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OUTRDW         RDW                                          
         SHI   R1,4                MINUS L'RDW                                  
         CHI   R1,L'P              DOES RECORD OVERFLOW THE PRINT LINE?         
         BNH   *+8                                                              
         LHI   R1,L'P              YES: USE L'P                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),OUTSTAMP       PRINT THE OUTPUT RECORD                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   DICTSW,C'Y'         WAS THIS A *DICT CARD?                       
         BE    GETPAN              YES: THERE MAY BE MORE *PHASE RECS           
         B     GETREC              NO: GET NEXT INPUT FILE RECORD               
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE (OUTFILE)                                                        
*                                                                               
         OC    RETCODE,RETCODE     RETURN CODE ALREADY SET?                     
         BNZ   XBASE                                                            
         CLI   PUTFLAG,C'Y'        NO: ANY RECORDS PUT TO OUTFILE?              
         BE    XBASE                                                            
         MVC   RETCODE,=F'4'       NO: SET RC=4                                 
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
TRACEREC NTR1  ,                                                                
*                                                                               
         MVC   P(13),=C'INPUT RECORD:'                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MRLEN          RDW                                          
         SHI   R1,4                MINUS L'RDW                                  
         CHI   R1,L'P              DOES RECORD OVERFLOW THE PRINT LINE?         
         BNH   *+8                                                              
         LHI   R1,L'P              YES: USE L'P                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MRTYPE         PRINT THE INPUT RECORD                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XIT1  ,                                                                
         EJECT                                                                  
*************************************************************                   
*                                                                               
*   WORK AREAS                                                                  
*                                                                               
*************************************************************                   
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
DUB      DS    D                                                                
WORK     DS    CL17                                                             
*                                                                               
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
XXGADDR  DS    A                                                                
*                                                                               
HALF     DS    H                                                                
*                                                                               
XXGSW    DS    C                                                                
DICTSW   DS    C                                                                
PUTFLAG  DC    C'N'                'Y' IF WE PUT ANYTHING TO OUTFILE            
ENV      DC    CL3'DDS'            DEFAULT TO LIVE ENVIRONMENT                  
*                                                                               
TRTBLANK DC    256X'00'                                                         
         ORG   TRTBLANK+C' '                                                    
         DC    X'FF'                                                            
         ORG   TRTBLANK+C','                                                    
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
DUMPLIST DS    0F                  FOR STXITER                                  
         DC    A(PAPTQA),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
                                                                                
CARD     DS    CL80                                                             
*                                                                               
TESTNAME DS    CL8                 STGE PHASE NAME                              
         DC    C' '                 (DELIMIT PREVIOUS FIELD)                    
BKUPNAME DS    CL8                 BACKUP PHASE NAME                            
BASENAME DS    CL8                 PRODUCTION PHASE NAME                        
TEMPNAME DS    CL8                 TEMPORARY PHASE NAME (FOR RENAMES)           
*                                                                               
ORIGDSN  DS    CL44                PERSONAL PANLIB DSN                          
*                                                                               
MRFILE   DCB   DDNAME=MRFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,LRECL=OUTRECLQ,RECFM=VB,MACRF=PM         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSODSPAC                                                         
         DC    C'N'                FORCE DSPACE=N                               
         ORG                                                                    
         SPACE 3                                                                
         EJECT                                                                  
* INPUT FILE RECORD LAYOUT                                                      
*                                                                               
* THE INPUT FILE IS BUILT BY THE ICETOOL STEP THAT CALLS THIS PROGRAM.          
* THE RECORDS ARE GROUPED BY USERID, WITH SPECIAL HEADER AND TRAILER            
* RECORDS AROUND EACH GROUP.                                                    
*                                                                               
* EACH DATA RECORD STARTS WITH THE "02" MEMBER RECORD, AS DEFINED BY            
* COPYBOOK APAMMMBR.                                                            
*                                                                               
         PRINT GEN                                                              
         APAMMMBR ,                                                             
         PRINT NOGEN                                                            
         ORG   MRTYPE                                                           
GRPHDR   DS    C'USERID='          SECTION START                                
GRPUSRID DS    CL8                 MOVE REQUEST OWNER                           
         ORG   MRTYPE                                                           
GRPTRLR  DS    C'END USERID'       SECTION END                                  
         ORG   ,                                                                
*                                                                               
* AFTER THE "02" DATA FIELDS, ADDITIONAL FIELDS ARE PRESENT.                    
* THEY IMMEDIATELY FOLLOW THE END OF THE "02" MEMBER RECORD. ICETOOL            
* EXTRACTS THESE FIELDS FROM THE "01" RECORD ASSOCIATED WITH THE MOVE           
* REQUEST.                                                                      
*                                                                               
MRCMOVC  DS    XL2                 COMPLETED MOVE COUNT                         
MRSTAT   DS    CL6                 MOVE REQUEST STATUS                          
MRCLSN   DS    CL4                 CURRENT LEVEL (SHORT NAME)                   
MRMTYP   DS    CL1                 MOVE TYPE                                    
MRJIRA   DS    CL16                JIRA ISSUE # (FROM SERVICE REQUEST)          
MRADDID  DS    CL8                 OWNER USERID                                 
MRRUNDA  DS    CL8                 FIRST RUN DATE (DATE TO QA)                  
MRDESCR  DS    CL55                MOVE REQUEST DESCRIPTION                     
MRNAME   DS    CL16                MOVE REQUEST NAME (FOR REWORKS)              
         ORG   MRNAME                                                           
MRNAME_RW         DS C'RW '        COPY FOR REWORK INDICATOR                    
MRNAME_ORIG_MR#   DS CL6           COPY FOR REWORK: ORIGINAL MR #               
MRNAME_DELIMITER  DS C':'          COPY FOR REWORK: FIXED DELIMITER             
MRNAME_REWORK_MR# DS CL6           COPY FOR REWORK: ORIGINAL MR #               
MRESTOP  DS    CL4                 COPY FOR REWORK: EARLY STOP LEVEL            
*                                                                               
         EJECT                                                                  
* OUTPUT FILE IS PIPE-DELIMITED, VARIABLE-LENGTH RECORDS                        
*                                                                               
OUTREC   DS    0D                                                               
OUTRDW   DC    F'0'                RDW                                          
OUTSTAMP DS    CL4                 MOVE VERSION STAMP                           
         DC    C'|'                                                             
OUTUID   DS    CL8                 OWNER USERID                                 
         DC    C'|'                                                             
OUTMRNUM DS    CL6                 MOVE REQUEST NUMBER                          
         DC    C'|'                                                             
OUTSTAT  DS    CL6                 MOVE REQUEST STATUS                          
         DC    C'|'                                                             
OUTLEVEL DS    CL4                 MOVE REQUEST LEVEL (SHORT NAME)              
         DC    C'|'                                                             
OUTMTYP  DS    CL1                 MOVE TYPE                                    
         DC    C'|'                                                             
OUTQADAT DS    CL8                 FIRST RUN DATE (DATE TO QA)                  
         DC    C'|'                                                             
OUTJIRA  DS    CL16                JIRA ISSUE# (FROM SPEC TICKET)               
         DC    C'|'                                                             
OUTCMOVC DS    CL2                 COMPLETED MOVE COUNT                         
         DC    C'|'                                                             
OUTMEMBR DS    CL10                PANVALET "FROM" MEMBER NAME                  
         DC    C'|'                                                             
OUTTOUDT DS    CL8                 MEMBER "TO" USER DATA                        
         DC    C'|'                                                             
OUTLIBC  DS    0CL7                LIBCODE/SUBCODE                              
OUTLIB   DS    CL4                 LIBCODE                                      
OUTSUBC  DS    CL3                 SUBCODE                                      
         DC    C'|'                                                             
OUTASNFL DS    CL1                 ASSIGNMENT FLAG                              
         DC    C'|'                                                             
OUTVERFL DS    CL1                 VERIFY FLAG                                  
         DC    C'|'                                                             
OUTCDFLG DS    CL1                 CONCURRENT DEVELOPMENT FLAG                  
         DC    C'|'                                                             
OUTTSTNM DS    CL8                 TEST MEMBER NAME                             
         DC    C'|'                                                             
OUTBASNM DS    CL8                 DERIVED BASE NAME                            
         DC    C'|'                                                             
OUTBAKNM DS    CL8                 DERIVED BKUP NAME                            
         DC    C'|'                                                             
OUTTMPNM DS    CL8                 DERIVED TEMP NAME                            
         DC    C'|'                                                             
OUTDESCR DS    CL55                MR DESCRIPTION                               
         DC    C'|'                                                             
OUTORIG# DS    CL6                 REWORK ORIGINAL MOVE REQUEST NUMBER          
         DC    C'|'                                                             
OUTESTOP DS    CL4                 EARLY STOP LEVEL (SHORT NAME)                
         DC    C'|'                                                             
OUTRECLQ EQU   *-OUTREC                                                         
         EJECT                                                                  
*&&DO                                                                           
BASECNT  DC    A(0)                # OF BASE PHASES ENTERED IN PHASETAB         
         DS    0D                                                               
         DC    C'*BASETAB'                                                      
BASETAB  DC    (BASEMAX)CL8' '     TABLE OF BASE PHASE NAMES                    
BASEMAX  EQU   500                                                              
         EJECT                                                                  
*&&                                                                             
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDPAPTQA  01/13/21'                                      
         END                                                                    
