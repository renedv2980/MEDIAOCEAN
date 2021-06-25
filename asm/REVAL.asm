*          DATA SET REVAL      AT LEVEL 029 AS OF 03/12/12                      
*PHASE T00AA6B,*                                                                
*              'REVAL - GENERIC VALIDATOR'                                      
         TITLE 'REVAL - GENERIC VALIDATOR'                                      
REVAL    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 VALWRKX-VALWRKD,**REVAL*,RA,RR=RE,CLEAR=YES                      
         USING VALWRKD,RC                                                       
         ST    RE,RELO                                                          
         L     RE,4(RD)            SAVE BACKWARD POINTER FOR HOOK CALLS         
M001     L     RE,4(RE)                                                         
         C     RB,16(RE)           GO BACK UNTIL RF NEQ A(REVAL)                
         BH    M001B               (TO CATER FOR RECURSIVE REVAL CALLS)         
         LR    RF,RB               LOOK FOR ANY CALL FROM WITHIN                
         AH    RF,=H'8192'         2 BASE REGS OF REVAL                         
         C     RF,16(RE)                                                        
         BH    M001                                                             
*                                                                               
M001B    MVC   CALLRD,8(RE)        AND THEN SAVE THE FORWARD POINTER            
*                                                                               
M002     SR    RE,RE               CHECK PARAMETERS                             
         CLI   VLPERR-VLPARMS(R1),VLPBADQ                                       
         BNE   *+6                                                              
         DC    H'0'                RECALLED AFTER A BAD CALL                    
*                                                                               
         ICM   RE,7,VLPAERR+1-VLPARMS(R1)                                       
         BNZ   *+8                                                              
         LA    RE,ERRNUM           RETURN ERROR MESSAGE ADDRESS IF NONE         
*                                                                               
         ST    RE,VLPAERR-VLPARMS(R1)                                           
*                                                                               
         LA    RE,UPPER            RETURN A(UPPER CASE XLATE TABLE)             
         ST    RE,VLAUPPER-VLPARMS(R1)                                          
*                                                                               
         ST    R1,APARMS                                                        
         MVC   VLPARMS(VLPARML),0(R1)                                           
*                                                                               
         TM    VLPTYPE,VLPVALQ+VLPTRAQ+VLPTABQ+VLPHLPQ                          
         BNZ   *+6                                                              
         DC    H'0'                NO FUNCTION REQUESTED                        
*                                                                               
         OC    VLPAHOOK,VLPAHOOK   IF A HOOK ADDRESS PASSED                     
         BZ    *+12                FORCE TO TABLE CALL ONLY                     
         NI    VLPTYPE,X'FF'-VLPVALQ-VLPTRAQ                                    
         OI    VLPTYPE,VLPTABQ                                                  
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
*        DETERMINE TABLE FOR SYSTEM                                             
*                                                                               
         L     RF,=A(SYSTAB)       POINT TO SYSTEM TABLE                        
         A     RF,RELO             RE-LOCATE ADDRESS                            
         USING SYSTABD,RF          ESTABLISH TABLE ENTRY                        
*                                                                               
         CLC   VLSYSTEM,SYSSYS     MATCH SYSTEM                                 
         BE    *+18                                                             
         LA    RF,SYSENTL(RF)      BUMP TO NEXT TABLE ENTRY                     
         CLI   SYSSYS,0            CONTINUE IF EOT NOT REACHED                  
         BNE   *-18                                                             
         DC    H'0'                                                             
*                                                                               
         ST    RF,ASYSTAB          SAVE TABLE ADDRESS                           
*                                                                               
*        TRANSLATE INCOMING FILTER VALUES TO FILTER MASKS                       
*                                                                               
         L     RE,SYSMASKA         TRANSLATE VALUES TO MASKS VIA TABLE          
*                                  TAB CONTAINS DISPS TO VAL/MASK               
         A     RE,RELO             RE-LOCATE ADDRESS                            
         DROP  RF                                                               
*                                                                               
         USING MASKD,RE                                                         
         XC    VLMASKS,VLMASKS     CLEAR MASKS                                  
         XC    VLMASKS1,VLMASKS1   CLEAR MASKS                                  
*                                                                               
M012     CLI   0(RE),MASKEOT       LOOP FOR A VALUE                             
         BE    M015                                                             
         LH    R1,MASKVDSP         HALFWORD DISP TO VALUE BYTE                  
         AR    R1,RC               IN VALWRKD                                   
         CLI   0(R1),0             IF NULL VALUE                                
         BNE   *+10                                                             
         SR    R2,R2               NO FILTERING                                 
         B     M014                                                             
*                                                                               
         CLI   0(R1),X'FF'         IF VALUE = FF                                
         BNE   *+12                                                             
         L     R2,=F'-1'           SET MASK TO ALL FF'S, IE PASS                
         B     M014                ONLY ENTRIES WITH MASKS OF FF'S              
*                                                                               
         LA    R2,1                                                             
         SLL   R2,31               R2 = MASK FOR 1ST VALUE IN MASKVALS          
         LA    R0,L'MASKVALS                                                    
         LA    RF,MASKVALS                                                      
M013     CLC   0(1,R1),0(RF)                                                    
         BE    M014                                                             
         LA    RF,1(RF)                                                         
         SRL   R2,1                SHIFT 1 BIT PER VALUE                        
         BCT   R0,M013                                                          
         SR    R2,R2               UNDEFINED FILTER, PASS EVERYTHING            
M014     ST    R2,FULL             NOW MOVE MASK VALUE IN                       
         LH    R1,MASKMDSP         HALFWORD DISP TO MASK BYTE(S)                
         AR    R1,RC               IN VALWRKD                                   
         ZIC   RF,MASKMLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FULL                                                     
         LA    RE,MASKLEN(RE)                                                   
         B     M012                                                             
*                                                                               
M015     MVC   CALLTYPE,VLPTYPE    SAVE CALL TYPE                               
         NI    CALLTYPE,VLPVALQ+VLPTRAQ+VLPTABQ+VLPHLPQ                         
         L     R1,APARMS           RETURN MASK VALUES                           
         MVC   VLMASKS-VLPARMS(L'VLMASKS,R1),VLMASKS                            
         MVC   VLMASKS1-VLPARMS(L'VLMASKS1,R1),VLMASKS1                         
         SR    RF,RF               GET TYPE ICODE INTO RF                       
         ICM   RE,7,VLPATYP+1                                                   
         BZ    M018                NO ADDRESS MEANS NO TYPE                     
         ICM   RF,3,0(RE)                                                       
         BNZ   M018                                                             
         ICM   RF,3,=AL2(ALONEQ)   A(0) MEANS STANDALONE QUALIFIER              
*                                                                               
M018     L     R2,ASYSTAB          LOOK UP TYPE IN TYPTAB                       
         L     R2,SYSTYPEA-SYSTABD(R2)                                          
         A     R2,RELO             RE-LOCATE ADDRESS                            
         USING TYTYPD,R2                                                        
         ICM   R6,15,VLPATAB       R6 = USER AREA FOR TABLE                     
         USING VLTABD,R6                                                        
         LA    R6,0(R6)                                                         
         TM    CALLTYPE,VLPHLPQ                                                 
         BO    M020                IF NOT HELP CALL                             
         XC    VLTCNTL,VLTCNTL     CLEAR CONTROL WORDS AND FIRST ENTRY          
         XC    VLTAB(VLTABLQ),VLTAB                                             
         MVC   VLTSHORT(L'VLTSHORT+L'VLTFULL),SPACES                            
         MVI   VLTCELEN+3,VLTABLQ  SET STANDARD ENTRY SIZE                      
         MVI   VLTCAEOT,1          SET NO (HIGH) LIMIT TO TABLE SIZE            
         ZIC   RE,VLPTLEN          MAX TABLE SIZE IN 256-BYTE UNITS             
         LTR   RE,RE                                                            
         BZ    M020                                                             
         SLA   RE,8                CONVERT TO BYTES                             
         AR    RE,R6               PLUS A(TABLE)                                
         S     RE,VLTCELEN         MINUS 1 ENTRY'S WIDTH                        
         ST    RE,VLTCAEOT         GIVES LIMIT FOR BUILDING                     
*                                                                               
M020     CLC   TYTICODE,=AL2(TYTEOT) SEARCH FOR TYPE IN TYPTAB                  
         BE    EBAD                NOT FOUND                                    
         CLM   RF,3,TYTICODE                                                    
         BE    M025                                                             
         LA    R2,TYTYPL(R2)                                                    
         B     M020                                                             
*                                                                               
M025     TM    CALLTYPE,VLPHLPQ    IF CALL WAS JUST FOR HELP                    
         BO    M105                RETURN HELP NUMBER IN TABLE AREA             
*                                                                               
M028     ST    R2,ATYP             SAVE ADDRESS OF ENTRY FOR LATER              
         ICM   RF,7,TYT1ADDR                                                    
         TM    TYTIND1,TYT1TABQ                                                 
         BO    M040                SKIP FOR TABLE LOOK-UP                       
*                                                                               
M030     L     R1,APARMS           ITS A ROUTINE - PASS CALLERS PARMS           
         TM    TYTIND1,TYT1SYSQ                                                 
         BO    *+12                EITHER IN SYSFACS                            
         A     RF,RELO             OR IN THIS MODULE                            
         B     *+16                                                             
         SRL   RF,16               SYSFACS DISPLACEMENT                         
         A     RF,VLASFACS                                                      
         L     RF,0(RF)                                                         
*                                                                               
         BASR  RE,RF               CALL ROUTINE TO HANDLE IT                    
         MVC   VLPARMS,0(R1)       RESAVE CALLERS PARMS                         
         TM    VLPTYPE,VLPHLPQ                                                  
         BO    M105                HELP NUMBER REQUESTED/RETURNED               
         OC    VLPAHOOK,VLPAHOOK                                                
         BNZ   EXIT                HOOK-STYLE ALREADY DEALT WITH                
         TM    VLPERR,VLPERRQ                                                   
         BNZ   M032                                                             
         TM    CALLTYPE,VLPTABQ    NO ERRORS                                    
         BO    M070                TABLE CALL GOES TO STAGE 2                   
         LA    R2,VLTAB            OTHER CALLS GO TO HAVE CONTROL               
         ST    R2,VLTCATAB         WORDS TIDIED UP (ASSUME 1-ENTRY TAB)         
         A     R2,VLTCELEN         R2 = A(NEXT TAB ENTRY)                       
         B     M068                                                             
*                                                                               
M032     SR    RE,RE               ERRORS                                       
         L     RE,VLPAERR                                                       
         MVC   ERRNUM,0(RE)        SAVE ERROR NUMBER RETURNED                   
         LA    R2,VLTAB                                                         
         TM    CALLTYPE,VLPTRAQ                                                 
         BO    M068                IF TRANSLATE CREATE DUMMY TAB ENTRY          
         TM    CALLTYPE,VLPTABQ                                                 
         BNO   M100                THAT'S ALL IF TABLE NOT REQUIRED             
         OC    VLTCENUM,VLTCENUM   IF WE HAVE A TABLE WITH ENTRIES              
         BZ    M100                                                             
         B     M070                DO STAGE 2 FILTER PASS                       
*                                                                               
M040     TM    TYTIND1,TYT1ADRQ    TABLE LOOK-UP - WHERE'S THE TABLE            
         BO    M045                IN THIS MODULE                               
*                                                                               
*                                  ELSE ITS IN A CORE-RES PHASE                 
*                                                                               
         L     RF,VLACFACS         A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(RF)  A(CALLOV)                               
*                                                                               
         ICM   R0,14,=X'D9000A'    FORMAT CORE-RESIDENT ID                      
         ICM   R0,1,TYT1PHAS       DESIRED MODULE ID                            
*                                                                               
         GOTO1 (RF),DUB,0,(R0)     GET MODULE ADDRESS                           
*                                                                               
         L     RF,0(R1)            GET RETURNED PHASE ADDRESS                   
*                                                                               
         ZIC   R3,TYT1TNUM         TABLE NUMBER IN TYPTAB                       
         BCTR  R3,0                INDEX TO NTH 2BYTE DISPLACEMENT              
         SLL   R3,1                IN THE SPECIFIED PHASE                       
         AR    R3,RF                                                            
         LH    R3,0(R3)            PICK UP DISPLACEMENT                         
         AR    R3,RF               ADD PHASE ADDRESS FOR TABLE ADDRESS          
         B     M048                                                             
         DROP  R2                                                               
*                                                                               
M045     LR    R3,RF               COME HERE IF TABLE IS IN REVAL               
         A     R3,RELO                                                          
         USING VLGLBD,R3                                                        
*                                                                               
M048     LA    R2,VLTAB            R2 = TABLE ENTRY POINTER                     
         USING VLTAB,R2            SAVE EXTRA LENGTH FOR VLTEXTRA               
         MVC   EXTRALH+1(1),VLGLENX AS HALFWORD                                 
         LA    R3,L'VLGLENX(R3)    BUMP PAST FOR TABLE PROPER                   
         USING VLGLB,R3                                                         
         LH    RF,EXTRALH          IF THERE IS EXTRA TABLE WIDTH                
         LTR   RF,RF               REDO CONTROL WORDS                           
         BZ    M049                                                             
         LA    RF,VLTABLQ(RF)                                                   
         ST    RF,VLTCELEN         SAVE TABLE ENTRY WIDTH                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    VLTAB(0),VLTAB      CLEAR A TABLE ENTRY                          
         MVI   VLTCAEOT,1          SET NO (HIGH) LIMIT TO TABLE SIZE            
         ZIC   RF,VLPTLEN          MAX TABLE SIZE IN 256-BYTE UNITS             
         LTR   RF,RF                                                            
         BZ    M049                                                             
         SLA   RF,8                CONVERT TO BYTES                             
         AR    RF,R6               PLUS A(TABLE)                                
         S     RF,VLTCELEN         MINUS 1 ENTRY'S WIDTH                        
         ST    RF,VLTCAEOT         GIVES LIMIT FOR BUILDING                     
*                                                                               
M049     ICM   RF,15,VLPAVAL       PICK UP A(EXTERNAL/INTERNAL VALUE)           
         BNZ   M050                                                             
         TM    CALLTYPE,VLPVALQ+VLPTRAQ                                         
         BZ    M052                                                             
         B     EBAD                MUST BE ONE UNLESS ONLY TABLE CALL           
M050     MVC   VALUE,0(RF)         SAVE VALUE IN W/S                            
         CLI   VLPVLEN,0           CHECK VALUE LENGTH                           
         BNE   M051                                                             
         CLI   CALLTYPE,VLPTABQ                                                 
         BE    *+12                (TABLE WITH STARTING ICODE GIVEN)            
         TM    CALLTYPE,VLPTRAQ                                                 
         BNO   M052                                                             
         MVI   VLPVLEN,2           IF NONE GIVEN ICODE DEFAULTS TO 2            
*                                                                               
M051     CLI   CALLTYPE,VLPTRAQ    IF SIMPLE TRANSLATE REQUIRED                 
         BE    *+12                OR SIMPLE TABLE WITH START ICODE             
         CLI   CALLTYPE,VLPTABQ    COMPARE BEFORE FILTERING                     
         BNE   M052                                                             
         SR    R1,R1                                                            
         ICM   R1,1,VLPVLEN                                                     
         BZ    M052                LEN CLEARED WHEN START ICODE REACHED         
         LA    RE,VLGICODE+L'VLGICODE                                           
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   VALUE(0),0(RE)                                                   
         BNE   M067                                                             
         CLI   CALLTYPE,VLPTABQ    CLEAR LENGTH AS START POINT REACHED          
         BNE   M052                                                             
         MVI   VLPVLEN,0                                                        
*                                                                               
M052     DS    0H                  APPLY FILTER MASK1                           
         MVC   WORK(L'VLGFLT1M),VLFLT1M                                         
         NC    WORK(L'VLGFLT1M),VLGFLT1M                                        
         CLC   WORK(L'VLGFLT1M),VLFLT1M                                         
         BNE   M067                                                             
         TM    VLGIND,VLGDDSQ      DDS-ONLY FILTER                              
         BNO   M053                                                             
         TM    VLSTAT,VLSDDSQ                                                   
         BNO   M067                                                             
*                                                                               
M053     LA    R1,VLGLANGN         FIND LANGUAGE ENTRY                          
         AH    R1,EXTRALH          VARIABLE EXTRA SUBENTRY IN FRONT             
         USING VLGLANGN,R1                                                      
         ZIC   R0,VLGLEN                                                        
         AR    R0,R3               R0 = END OF ENTRY                            
         SR    RE,RE                                                            
*                                                                               
M054     CR    R1,R0               LOOP FOR A LANGUAGE SUBENTRY                 
         BL    M055                                                             
         TM    CALLTYPE,VLPTRAQ    END OF ENTRY - NO LANGUAGE MATCH             
         BNO   M067                IF TRANSLATE USE ENGLISH VERSION             
         DROP  R1                                                               
         LA    R1,VLGLB+VLGLEN1Q   (ASSUMED FIRST SUBENTRY)                     
         AH    R1,EXTRALH                                                       
         USING VLGLANGN,R1                                                      
         B     M056                                                             
M055     CLC   VLLANG,VLGLANG      LOOK FOR LANGUAGE MATCH                      
****     BE    M056                FOUND ONE                                    
         B     M056 ***********************                                     
         IC    RE,VLGLANGL         ELSE BUMP SUBENTRY                           
         AR    R1,RE                                                            
         B     M054                                                             
M056     C     R2,VLTCAEOT         FOUND IT - HAVE WE ROOM                      
         BNH   M059                YES                                          
         L     RE,APARMS                                                        
         OI    VLPERR-VLPARMS(RE),VLPTOOQ ELSE RETURN WARNING                   
         OC    VLTCATAB,VLTCATAB                                                
         BNZ   M068                IF WE HAVE REQUIRED ENTRY STOP               
         CLI   CALLTYPE,VLPTABQ                                                 
         BE    M068                ALSO IF ITS JUST A TABLE CALL                
         S     R2,VLTCELEN         ELSE DROP LAST ONE                           
         XC    DUPICODE,DUPICODE   CLEAR ICODE FOR DUPLICATE CHECKS             
*                                                                               
M059     DS    0H                  BUILD A TABLE ENTRY                          
         MVC   VLTICODE,VLGICODE                                                
         MVC   VLTIND,VLGIND                                                    
         OI    VLTIND,VLTGLOBQ     FORCE GLOBAL INDICATOR                       
         MVC   VLTHELP,VLGHELP                                                  
         MVC   VLTMINI,VLGMINI                                                  
         MVC   VLTMINO,VLGMINO                                                  
         MVC   VLTSHORT,VLGSHORT                                                
         ZIC   RE,VLGLANGL                                                      
         LA    RF,(VLGFULL-VLGLANGN)                                            
         SR    RE,RF                                                            
         STC   RE,VLTLFULL         SAVE FULL NAME LENGTH                        
         MVC   VLTFULL,SPACES      MOVE IN FULL NAME SPACE-PADDED               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VLTFULL(0),VLGFULL                                               
         CLC   VLTSHORT,SPACES     DEFAULT TO 1ST CHARS OF FULL IF              
         BH    M059A               THERE IS NO SHORT NAME                       
         CLI   VLTMINO,L'VLTSHORT  AND MIN OUT LENGTH IS SHORT ENOUGH           
         BH    M059A                                                            
         IC    RE,VLTMINO                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VLTSHORT(0),VLTFULL                                              
         DROP  R1                                                               
*                                                                               
M059A    ICM   RE,1,EXTRALH+1      APPEND EXTRA CODES IF REQUIRED               
         BZ    M060                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VLTEXTRA(0),VLGEXTRA                                             
*                                                                               
M060     OC    DUPICODE,DUPICODE   CHECK FOR DUPLICATE HANDLING                 
         BZ    M061                                                             
         TM    VLPTYPE,VLPDUPQ                                                  
         BO    *+14                DUPLICATES REQUIRED                          
         CLC   VLTICODE,DUPICODE                                                
         BE    *+14                SAME AS LAST                                 
         XC    DUPICODE,DUPICODE   CLEAR IF NOT OR DUPS REQUIRED                
         B     M061                                                             
         OC    VLPAHOOK,VLPAHOOK   IF THE HOOK IS IN USE                        
         BNZ   M067                SKIP TO NEXT TABLE ENTRY                     
*                                                                               
M061     MVI   CLCOK,C'N'          PRESET COMPARE SWITCH TO MISMATCH            
         OC    VLTCATAB,VLTCATAB   SKIP VALIDATION/TRANSLATION                  
         BNZ   M063F               IF ALREADY DONE                              
         TM    CALLTYPE,VLPVALQ    VALIDATION                                   
         BNO   M063                                                             
         TR    VALUE,UPPER         UPPER CASE                                   
         LA    R1,L'VLTSHORT       COMPARE FOR FULL LEN OF SHORT IF ANY         
         LA    RF,VLTSHORT-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         BCT   R1,*-12                                                          
         B     M062                NO SHORT FORM                                
         CLM   R1,1,VLPVLEN        INPUT LENGTH MUST MATCH EXACTLY              
         BNE   M062                                                             
         BCTR  R1,0                                                             
         MVC   VALUE2,VLTSHORT                                                  
         TR    VALUE2,UPPER                                                     
         EX    R1,VALUECLC                                                      
         BE    M063E               OK                                           
*                                                                               
M062     IC    R1,VLPVLEN                                                       
         CLM   R1,1,VLTMINI        CHECK FOR LESS THAN MINIMUM                  
         BL    M067                                                             
         SH    R1,=H'1'                                                         
         BM    M063E               VLTMINI=0 MEANS THIS IS DEFAULT NTRY         
         MVC   VALUE2,VLTFULL                                                   
         TR    VALUE2,UPPER                                                     
         EX    R1,VALUECLC                                                      
         BE    M063E                                                            
         B     M067                                                             
VALUECLC CLC   VALUE(0),VALUE2                                                  
*                                                                               
M063     TM    CALLTYPE,VLPTRAQ    TRANSLATE                                    
         BNO   M063F               IF NOT ITS JUST TABLE BUILDING               
         ZIC   R1,VLPVLEN                                                       
         LA    RE,VLGICODE+L'VLGICODE                                           
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   VALUE(0),0(RE)                                                   
         BNE   M067                                                             
*                                                                               
M063E    MVI   CLCOK,C'Y'          SET COMPARE OK                               
*                                                                               
M063F    DS    0H                  APPLY FILTER MASKS HERE                      
         MVC   WORK(L'VLGFLT2M),VLFLT2M                                         
         NC    WORK(L'VLGFLT2M),VLGFLT2M                                        
         CLC   WORK(L'VLGFLT2M),VLFLT2M                                         
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(VLEFLT2) INVALID FOR TYPE 2 MASK                     
         B     M063H                                                            
*                                                                               
         MVC   WORK(L'VLGFLT3M),VLFLT3M                                         
         NC    WORK(L'VLGFLT3M),VLGFLT3M                                        
         CLC   WORK(L'VLGFLT3M),VLFLT3M                                         
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(VLEFLT3) INVALID FOR TYPE 3 MASK                     
         B     M063H                                                            
*                                                                               
         MVC   WORK(L'VLGFLT4M),VLFLT4M                                         
         NC    WORK(L'VLGFLT4M),VLGFLT4M                                        
         CLC   WORK(L'VLGFLT4M),VLFLT4M                                         
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(VLEFLT4) INVALID FOR TYPE 4 MASK                     
         B     M063H                                                            
*                                                                               
         MVC   WORK(L'VLGFLT5M),VLFLT5M                                         
         NC    WORK(L'VLGFLT5M),VLGFLT5M                                        
         CLC   WORK(L'VLGFLT5M),VLFLT5M                                         
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(VLEFLT5) INVALID FOR TYPE 5 MASK                     
         B     M063H                                                            
*                                                                               
         MVC   WORK(L'VLGFLT6M),VLFLT6M                                         
         NC    WORK(L'VLGFLT6M),VLGFLT6M                                        
         CLC   WORK(L'VLGFLT6M),VLFLT6M                                         
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(VLEFLT6) INVALID FOR TYPE 6 MASK                     
         B     M063H                                                            
*                                  NO FILTER PROBLEM                            
         XC    ERRNUM,ERRNUM                                                    
         TM    CALLTYPE,VLPVALQ+VLPTRAQ TEST VALIDATING OR TRANSLATING          
         BNZ   M064                                                             
         B     M066                NO, JUST TABLE BUILDING                      
*                                                                               
M063H    DS    0H                                                               
         B     M067                                                             
*                                                                               
M064     CLI   CLCOK,C'Y'          SUCCESSFUL VALIDATION/TRANSLATION            
         BNE   M066                                                             
         OC    DUPICODE,DUPICODE                                                
         BZ    M065                IGNORE AN EARLIER SYNONYM                    
         XC    DUPICODE,DUPICODE   BY SHIFTING THIS ENTRY OVER IT               
         LR    R1,R2                                                            
         L     RE,VLTCELEN                                                      
         SR    R2,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
M065     ST    R2,VLTCATAB         STORE ADDR OF THE ENTRY IN CONTROL           
         TM    CALLTYPE,VLPTABQ    SKIP OUT IF TABLE NOT RQRD                   
         BO    M066                                                             
         A     R2,VLTCELEN         INDICATING ENTRY PRESENT                     
         B     M068                                                             
*                                                                               
M066     TM    CALLTYPE,VLPTABQ    IF TABLE IS RQRD                             
         BNO   M067                                                             
         OC    DUPICODE,DUPICODE   DONT KEEP IT IF ITS A DUP                    
         BNZ   M067                                                             
         MVC   DUPICODE,VLTICODE   ELSE SAVE LAST ICODE                         
         A     R2,VLTCELEN         AND BUMP TABLE POINTER (IE KEEP IT)          
         OC    VLPAHOOK,VLPAHOOK                                                
         BZ    M067                                                             
         ST    R3,ALASTGLB         IF THERE'S A HOOK SAVE GLBTAB POINTR         
         B     M068                AND SKIP OUT                                 
*                                                                               
M067     ZIC   R0,VLGLEN           BUMP VLGLBTAB                                
         AR    R3,R0                                                            
         CLI   VLGLB,0                                                          
         BNE   M051                REPEAT FOR NEXT TABLE ENTRY                  
*                                                                               
M068     LA    RF,VLTAB-VLTABD(R6) AT EOT TIDY UP CONTROL WORDS                 
         CLI   CALLTYPE,VLPTABQ                                                 
         BNE   *+8                                                              
         ST    RF,VLTCATAB         START OF TABLE ADDRESS                       
         BCTR  R2,0                                                             
         ST    R2,VLTCAEOT         END OF TABLE MINUS 1                         
         LA    R2,1(R2)                                                         
         SR    R2,RF                                                            
         SRDL  R2,32                                                            
         D     R2,VLTCELEN                                                      
         ST    R3,VLTCENUM         NUMBER OF ENTRIES IN TABLE                   
         LTR   R3,R3                                                            
         BZ    *+14                NO ENTRIES = ERROR                           
         OC    VLTCATAB,VLTCATAB                                                
         BNZ   M070                NO START ADDRESS = ERROR                     
         CLI   CALLTYPE,VLPTRAQ                                                 
         BE    M069                                                             
M068E    OC    ERRNUM,ERRNUM       SET ERROR IF NOT PROVISIONALLY SET           
         BNZ   *+10                                                             
         MVC   ERRNUM,=AL2(VLENOTV)                                             
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ SET INVALID ERROR CODE                
         LTR   R3,R3                                                            
         BNZ   M070                CARRY ON IF WE HAVE TABLE ENTRIES            
         B     M100                EXIT NOW IF NO TABLE ENTRIES                 
         DROP  R2                                                               
*                                                                               
M069     L     R2,ATYP             IF TRANSLATE FAILS                           
         USING TYTYPD,R2           AND IT WASNT DECLARED PARENTLESS             
*MN      OC    TYTICODE,TYTICODE   RETURN '*?*' = UNKNOWN                       
*MN      BZ    M069A                                                            
         CLC   TYTICODE,NULLS      RETURN '*?*' = UNKNOWN                       
         BE    M069A                                                            
         XC    VLTAB(VLTABLQ),VLTAB                                             
         MVI   VLTMINO,3                                                        
         MVI   VLTLFULL,3                                                       
         MVC   VLTSHORT(L'VLTSHORT+L'VLTFULL),SPACES                            
         MVC   VLTSHORT(3),=C'*?*'                                              
         MVC   VLTFULL(3),=C'*?*'                                               
         MVI   VLTCENUM+3,1                                                     
         B     M068E                                                            
*                                                                               
M069A    ICM   RE,15,VLPAVAL       IF PARENTLESS SEE IF TYPTAB TELLS US         
         L     R2,ASYSTAB                                                       
         L     R2,SYSTYPEA-SYSTABD(R2) WHAT THE PARENT SHOULD BE                
         A     R2,RELO                                                          
*                                                                               
M069B    CLC   TYTICODE,=AL2(TYTEOT) SEARCH FOR VALUE IN TYPTAB                 
         BE    M068E               NOT FOUND                                    
         CLC   TYTICODE,0(RE)                                                   
         BE    *+12                                                             
         LA    R2,TYTYPL(R2)                                                    
         B     M069B                                                            
         SR    RF,RF               FOUND                                        
         ICM   RF,3,TYTPARNT       IS PARENT SPECIFIED                          
         BNZ   M018                YES - TRY IT                                 
         B     M068E               NO - ERROR                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        SECONDARY FILTERING (GLOBALS SPECIFIC TO A MEDIABASE MEMBER) *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
M070     TM    VLPTYPE,VLPGLBQ     CHECK FOR SECONDARY  FILTERING               
         BO    M090                GLOBAL VALUES ASKED FOR                      
*                                                                               
*        THIS TEST NEEDS TO BE MADE MORE GENERAL                                
*                                                                               
         L     R2,ATYP             PICK UP TYPTAB ENTRY                         
         USING TYTYPD,R2                                                        
         CLI   TYTIND2,0                                                        
         BE    M090                NO SECONDARY FILTERS                         
         SR    RF,RF                                                            
         ICM   RF,7,TYT2ADDR       GET ROUTINE ADDRESS                          
         TM    TYTIND2,TYT2SYSQ                                                 
         BO    *+12                EITHER IN SYSFACS                            
         A     RF,RELO             OR IN THIS MODULE                            
         B     *+12                                                             
         SRL   RF,16               SYSFACS DISPLACEMENT                         
         A     RF,VLASFACS         RF = ROUTINE ADDRESS                         
         LA    R1,VLPARMS          R1 = A(PARMS)                                
         ICM   R6,15,VLPATAB                                                    
         LA    R6,0(R6)                                                         
         USING VLTABD,R6                                                        
         L     R0,VLTCENUM         R0 = COUNT OF ENTRIES                        
         L     R3,VLTCELEN         R3 = LENGTH OF AN ENTRY                      
         LA    R2,VLTAB            R2 = A(VLTAB ENTRY)                          
         USING VLTAB,R2                                                         
         XC    SAVICODE,SAVICODE   SAVE VALID ICODE IF ANY                      
         TM    CALLTYPE,VLPTRAQ+VLPVALQ                                         
         BZ    M072                NOT APPLICABLE IF NOT XLATE/VALIDATE         
         ICM   RE,15,VLTCATAB                                                   
         BZ    M072                NO VALID ONE                                 
         MVC   SAVICODE,VLTICODE-VLTAB(RE)                                      
*                                                                               
M072     DS    0H                  CALL ROUTINE PER VLTAB ENTRY                 
         MVI   VLPERR,0            CLEAR ERROR EACH TIME                        
         ST    R2,VLPATAB          PASS TABLE ENTRY ADDRESS                     
         BASR  RE,RF               CALL FILTER ROUTINE                          
         TM    VLPERR,VLPERRQ      CHECK FOR ERRORS                             
         BZ    M076                NONE SO BUMP                                 
         SH    R0,=H'1'            REDUCE COUNT BY 1                            
         BZ    M080                NONE LEFT                                    
         LR    R4,R2               INVALID SO REMOVE IT                         
         LR    RE,R0                                                            
         BCTR  R3,0                TABLE ENTRY SIZE MINUS 1                     
         LA    R5,1(R3,R4)         R5 = FROM ADDRESS                            
         EX    R3,M072MOVE         MOVE THIS ENTRY FORWARD                      
         LA    R4,1(R3,R4)         BUMP TO ADDRESS                              
         BCT   RE,*-12                                                          
         LA    R3,1(R3)            RESTORE ENTRY SIZE                           
         B     M072                R2 = A(NEXT ONE) NOW                         
M072MOVE MVC   0(0,R4),0(R5)       MOVE NEXT ENTRY INTO THIS ONE                
*                                                                               
M076     AR    R2,R3               IF VALID JUST BUMP TO NEXT                   
         BCT   R0,M072                                                          
*                                                                               
M080     MVI   VLPERR,0            NO MORE TO CHECK SO TIDY UP                  
         BCTR  R2,0                                                             
         C     R2,VLTCAEOT                                                      
         BE    M090                NO CHANGE                                    
         ST    R2,VLTCAEOT         ELSE STORE NEW EOT - 1                       
         LA    R2,1(R2)                                                         
         LA    RF,VLTAB-VLTABD(R6)                                              
         SR    R2,RF                                                            
         SRL   R2,32                                                            
         D     R2,VLTCELEN                                                      
         ST    R3,VLTCENUM         AND NEW NUMBER OF ENTRIES                    
         OC    SAVICODE,SAVICODE   AND RESET POINTER TO VALID ENTRY             
         BZ    M084                                                             
         XC    VLTCATAB,VLTCATAB                                                
         LR    R2,RF               POINT TO FIRST ENTRY                         
         LTR   R0,R3               COUNT OF ENTRIES                             
         BZ    M084                                                             
M082     CLC   SAVICODE,VLTICODE   LOOK FOR ENTRY FOR SAVED ICODE               
         BE    M083                                                             
         A     R2,VLTCELEN                                                      
         BCT   R0,M082                                                          
         B     M084                                                             
M083     ST    R2,VLTCATAB                                                      
*                                                                               
M084     OC    VLTCENUM,VLTCENUM   CHECK FOR ERRORS TO REPORT                   
         BZ    *+14                NO ENTRIES                                   
         OC    VLTCATAB,VLTCATAB                                                
         BNZ   M090                OR NO VALID ONE FOR VALIDATE/XLATE           
         L     R1,APARMS                                                        
         OC    VLPAHOOK,VLPAHOOK                                                
         BNZ   M095                A HOOK MEANS BUMP TO NEXT IN GLBTAB          
         OI    VLPERR-VLPARMS(R1),VLPINVQ ELSE REPORT AN ERROR                  
         B     M100                                                             
*                                                                               
M090     L     R1,APARMS           CHECK FOR HOOK CALL                          
         OC    VLPAHOOK,VLPAHOOK                                                
         BZ    M100                IF NOT EXIT                                  
         TM    VLPERR-VLPARMS(R1),VLPERRQ CHECK FOR ERRORS                      
         BNZ   M095                IF SO BUMP TO NEXT                           
         LA    RE,M092             ELSE CALL HOOK                               
         NTR1  ,                   SAVE MY REGISTERS                            
         L     RF,VLPAHOOK                                                      
         L     RE,CALLRD                                                        
         LM    R0,RC,20(RE)        RESTORE CALLERS REGISTERS                    
         BASR  RE,RF                                                            
         XIT1  ,                   RESTORE MY REGISTERS                         
M092     TM    VLPHIND-VLPARMS(R1),VLPHENDQ                                     
         BO    EXIT                STOP HERE IF REQUESTED                       
*                                                                               
M095     NI    VLPERR-VLPARMS(R1),X'FF'-VLPERRQ CLEAR ERROR CODE                
         XC    ERRNUM,ERRNUM       AND NUMBER                                   
         L     R3,ALASTGLB         BUMP TO NEXT IN GLOBAL TABLE                 
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    EXIT                NO MORE ENTRIES                              
         LA    R2,VLTAB-VLTABD(R6) POINT TO FIRST VLTAB ENTRY                   
         B     M051                DEAL WITH NEXT GLBTAB ENTRY                  
*                                                                               
M100     ICM   R3,15,VLTCENUM      RETURN HELP NUMBER IF WE HAVE NO             
         BNZ   M110                TABLE                                        
         L     R2,ATYP             POINT TO TYPTAB ENTRY                        
         USING TYTYPD,R2                                                        
*                                                                               
M105     DS    0H                                                               
         L     RF,VLACFACS         XSORT ADDRESS IN COMFACS                     
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         GOTO1 (RF)                                                             
*                                                                               
         L     RE,VLPATAB          HELP NUMBER RETURNED                         
         MVC   0(L'TYTHELP,RE),TYTHELP                                          
*                                                                               
         L     RF,VLACFACS         XSORT ADDRESS IN COMFACS                     
         L     RF,CPROTON-COMFACSD(RF)                                          
         GOTO1 (RF)                                                             
*                                                                               
*MN      OC    TYTHELP,TYTHELP                                                  
*MN      BZ    M120                NO HELP NUMBER                               
         CLC   TYTHELP,NULLS                                                    
         BE    M120                NO HELP NUMBER                               
*                                                                               
         L     R1,APARMS                                                        
         OI    0(R1),VLPHLPQ       SET INDICATOR THAT HELP GIVEN                
         B     M120                                                             
*                                                                               
M110     TM    VLPTYPE,VLPSRTQ     SORT TABLE IF REQUIRED                       
         BNO   M120                                                             
         CLI   CALLTYPE,VLPTABQ                                                 
         BNE   M120                ONLY IF TABLE CALL                           
         CH    R3,=H'1'                                                         
         BNH   M120                AND MORE THAN 1 ENTRY                        
         LA    R2,VLTAB-VLTABD(R6) P1 = A(RECORDS) / P2 = #RECS                 
         L     R4,VLTCELEN         P3 = RECORD LENGTH                           
         LA    R5,L'VLTFULL        P4 = KEY LENGTH                              
         LA    R6,VLTFULL-VLTAB    P5 = KEY DISPLACEMENT                        
         STM   R2,R6,TEMP                                                       
         L     RF,VLACFACS         XSORT ADDRESS IN COMFACS                     
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),TEMP                                                        
         B     M120                                                             
*                                                                               
EBAD     L     R1,APARMS           BAD CALL (FEATURE NOT AVAILABLE?)            
         MVI   VLPERR-VLPARMS(R1),VLPBADQ                                       
         MVC   ERRNUM,=AL2(VLENOTAV)                                            
*                                                                               
M120     DS    0H                                                               
         L     R1,APARMS           EXITS TO CALLER                              
         CLI   VLPERR-VLPARMS(R1),0                                             
         BE    EXIT                IF THERE WAS AN ERROR                        
         OC    ERRNUM,ERRNUM                                                    
         BZ    EXIT                AND WE HAVE AN ERROR NUMBER                  
         L     RE,VLPAERR                                                       
         MVC   0(2,RE),ERRNUM      RETURN IT TO CALLER                          
EXIT     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATPROC - PROCESS DATE (START OR END)                               *         
*                                                                     *         
* ON ENTRY R1 = APARMS = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS           *         
***********************************************************************         
         SPACE 1                                                                
DATPROC  NTR1  ,                                                                
         TM    CALLTYPE,VLPVALQ    VALIDATE                                     
         BNO   DTPROC1                                                          
         L     RF,VLACFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         LA    R4,WORK                                                          
         USING PERVALD,R4                                                       
         XC    PVALOUTB,PVALOUTB                                                
         MVC   PVALCSTA,VLTODAYC                                                
         ST    R4,TEMP+4                                                        
         MVC   TEMP+4(1),VLLANG                                                 
         OI    TEMP+4,PVINSGLO+PVINSGLS SINGLE DATE IN/OUT                      
         OC    VLTODAYC,VLTODAYC                                                
         BZ    *+8                                                              
         OI    TEMP+4,PVINTOD      TODAY'S DATE PASSED TO AVOID SVC             
         L     RE,VLPAVAL                                                       
         MVC   VALUE,0(RE)         UPPER CASE                                   
         TR    VALUE,UPPER                                                      
         GOTO1 (RF),TEMP,(VLPVLEN,VALUE)                                        
         CLI   4(R1),4             SINGLE DATE RETURNED?                        
         BNE   DTPROCER                                                         
         MVC   VLTICODE,PVALCSTA                                                
         B     DTPROC2                                                          
*                                                                               
DTPROC1  TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   DTPROC3                                                          
         L     RE,VLPAVAL                                                       
         MVC   VLTICODE,0(RE)      MOVE DATE INTO VLTICODE                      
*                                                                               
DTPROC2  L     RF,VLACFACS         USE DATCON TO GET STANDARD FORM              
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),TEMP,(X'82',VLTICODE),(17,VLTFULL)                          
         IC    RF,4(R1)            LENGTH IN P2 B0                              
         STC   RF,VLTMINI                                                       
         STC   RF,VLTMINO                                                       
         STC   RF,VLTLFULL                                                      
         MVI   VLTIND,VLTGLOBQ                                                  
         B     EXIT                                                             
*                                                                               
DTPROC3  DS    0H                  IF TABLE CALL RETURN HELP PANEL              
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
         B     EXIT                                                             
*                                                                               
DTPROCER L     RE,VLPAERR          INVALID DATE                                 
         MVC   0(2,RE),=AL2(VLENOTV)                                            
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DAYPROC - PROCESS DAY EXPRESSION (EG MON, MTW, M-F)                 *         
*                                                                     *         
* ON ENTRY R1 = APARMS = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS           *         
***********************************************************************         
         SPACE 1                                                                
DAYPROC  NTR1  ,                                                                
         TM    CALLTYPE,VLPVALQ    VALIDATE CALL                                
         BNO   DYPROC1                                                          
         L     RF,VLPAVAL                                                       
         MVC   VALUE,0(RF)                                                      
         TR    VALUE,UPPER         UPPER CASE                                   
         L     RF,VLACFACS         USE DEJAVU                                   
         L     RF,CDEJAVU-COMFACSD(RF)                                          
         GOTO1 (RF),TEMP,(VLPVLEN,VALUE),(X'10',VLTICODE+1),WORK                
         CLI   VLTICODE+1,0                                                     
         BNE   DYPROC2             IF OK TRANSLATE IT TO STANDARD FORM          
         B     DYPROCER            INVALID                                      
*                                                                               
DYPROC1  TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   DYPROC3                                                          
         L     RE,VLPAVAL          MOVE ICODE INTO TABLE                        
         CLI   VLPVLEN,1                                                        
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVC   VLTICODE+1(1),0(RE)                                              
*                                                                               
DYPROC2  TM    VLTICODE+1,X'80'    VALIDATE AND TRANSLATE                       
         BO    DYPROCER            INVALID FORM AS X'80' NOT USED               
         L     RF,VLACFACS         USE DEJAVU TO GIVE STANDARD FORM             
         L     RF,CDEJAVU-COMFACSD(RF)                                          
         GOTO1 (RF),TEMP,VLTICODE+1,(X'20',VLTFULL),0                           
         CLC   VLTFULL,SPACES                                                   
         BE    DYPROCER                                                         
         MVI   VLTIND,VLTGLOBQ+VLTBYTEQ                                         
         LA    RE,L'VLTFULL                                                     
         LA    RF,VLTFULL-1(RE)                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-12                                                          
         STC   RE,VLTMINI                                                       
         STC   RE,VLTMINO                                                       
         STC   RE,VLTLFULL                                                      
         B     EXIT                                                             
*                                                                               
DYPROC3  DS    0H                  IF TABLE CALL RETURN HELP PANEL              
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
         B     EXIT                                                             
*                                                                               
DYPROCER L     RE,VLPAERR          INVALID DAY EXPRESSION                       
         MVC   0(2,RE),=AL2(VLENOTV)                                            
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FLTPROC - PROCESS FILTER EXPRESSION                                 *         
*                                                                     *         
* ON ENTRY R1 = APPARM = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS           *         
*                                                                     *         
* NOTE- **NON-STANDARD CODE LENGTH** -                                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
FLTPROC  NTR1  ,                                                                
*                                                                               
         OI    VLTIND,VLTNSLNQ     SET NON-STANDARD LENGTH INDICATOR            
*                                                                               
         TM    CALLTYPE,VLPVALQ    VALIDATE CALL                                
         BNO   FLTPVALN                                                         
*                                                                               
*                                                                               
         L     R3,VLPAVAL          POINT TO EXTERNAL VALUE                      
         LA    RF,VLTEXTRA+1       POINT TO OUTPUT AREA                         
         LA    R4,0(R3)            POINT TO FIRST CHARACTER IN INPUT            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,VLPVLEN        GET EXTERNAL VALUE LENGTH                    
         BZ    FLTPDONE            NO DATA TO ANALYZE                           
*                                                                               
FLTPLOOP DS    0H                                                               
*                                                                               
         CLI   0(R4),C'-'          IF NEGATIVE FILTER                           
         BNE   FLTPLP20                                                         
*                                                                               
         BCT   R0,*+8                 DECREMENT INPUT COUNTER                   
         B     FLTPDONE                  NO MORE DATA                           
*                                                                               
         LA    R4,1(R4)               BUMP TO NEXT INPUT BYTE                   
         MVC   0(1,RF),0(R4)          SAVE INPUT BYTE                           
         NI    0(RF),X'FF'-X'40'      FORCE NEXT TO BE LOWERCASE                
*                                                                               
         B     FLTPCONT                                                         
*                                                                               
FLTPLP20 DS    0H                                                               
*                                                                               
         MVC   0(1,RF),0(R4)       SAVE INPUT BYTE                              
*                                                                               
FLTPCONT DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP OUTPUT POINTER                          
         LA    R4,1(R4)            BUMP TO NEXT INPUT BYTE                      
         BCT   R0,FLTPLOOP         DECREMENT INPUT LENGTH COUNTER               
*                                                                               
FLTPDONE DS    0H                  NO MORE INPUT                                
*                                                                               
         LA    RE,VLTEXTRA+1       POINT TO START OF INTERNAL VALUE             
         SR    RF,RE               LENGTH OF INTERNAL VALUE                     
         STC   RF,VLTEXTRA                                                      
*                                                                               
FLTPVALX DS    0H                  NO MORE INPUT                                
*                                                                               
         B     FLTPTRA1            GO TRANSLATE FILTER                          
*                                                                               
FLTPVALN DS    0H                  NO MORE INPUT                                
*                                                                               
FLTPTRA  DS    0H                  NO MORE INPUT                                
*                                                                               
         TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   FLTPTRAN                                                         
*                                                                               
         SR    RF,RF               MOVE INTERNAL FORM INTO TABLE                
         ICM   RF,1,VLPVLEN        INTERNAL FORM LENGTH                         
         BZ    FLTPTRAX            NOTHING TO TRANSLATE                         
         STC   RF,VLTEXTRA         SAVE INTERNAL LENGTH                         
*                                                                               
         L     RE,VLPAVAL          POINT TO INTERNAL FORM                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VLTEXTRA+1(0),0(RE) MOVE TO TABLE                                
*                                                                               
FLTPTRA1 DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,VLTEXTRA       LENGTH OF INTERNAL FORMAT                    
         BZ    FLTPTRAX            NOTHING TO TRANSLATE                         
*                                                                               
         LA    R4,VLTFULL          START OF TRANSLATION AREA                    
         LA    RF,VLTEXTRA+1       START OF INTERNAL FORMAT                     
*                                                                               
FLTPTRAL DS    0H                                                               
*                                                                               
         TM    0(RF),X'40'         IF LOWERCASE FILTER                          
         BO    FLTPTRA2                                                         
*                                                                               
         MVI   0(R4),C'-'             INDICATE NEGATIVE FILTER                  
         LA    R4,1(R4)               BUMP TO NEXT OUTPUT POSITION              
         MVC   0(1,R4),0(RF)          MOVE FILTER VALUE TO OUTPUT               
         OI    0(R4),X'40'            FORCE UPPERCASE                           
         B     FLTPTRAC                                                         
*                                                                               
FLTPTRA2 DS    0H                                                               
*                                                                               
         MVC   0(1,R4),0(RF)       MOVE FILTER VALUE TO OUTPUT                  
*                                                                               
FLTPTRAC DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP POINTERS                                
         LA    R4,1(R4)                                                         
         BCT   R0,FLTPTRAL                                                      
*                                                                               
FLTPTRAD DS    0H                                                               
*                                                                               
         LA    RE,VLTFULL          CALCULATE TRANSLATE LENGTH                   
         SR    R4,RE                                                            
         STC   R4,VLTLFULL                                                      
*                                                                               
         STC   R4,VLTMINI                                                       
         STC   R4,VLTMINO                                                       
*                                                                               
         LA    RF,8                ADJUST TABLE CONTROL INFO                    
         L     RE,VLTCELEN         FILTERS HAVE MAX LENGTH 8                    
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCELEN                                                      
         L     RE,VLTCAEOT                                                      
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCAEOT                                                      
*                                                                               
FLTPTRAX DS    0H                                                               
*                                                                               
         B     FLTPROCX                                                         
*                                                                               
FLTPTRAN DS    0H                  IF TABLE CALL RETURN HELP PANEL              
*                                                                               
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
*                                                                               
FLTPROCX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TEXT ELEMS                                       *         
*                                                                     *         
* NTRY -  PARM+0    =  A(FIRST SCREEN FIELD)                          *         
*         PARM+4    =  A(TXTCTL)                                      *         
* EXIT -  TXTCTL BLOCK COMPLETED                                      *         
*         SCREEN FIELDS FILLED IN                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* MTICPROC - HANDLE MASTER TYPE ICODE                                 *         
*                                                                     *         
* ON ENTRY R1 = APPARM = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  BUILDS A DUMMY TABLE ENTRY AND RETURNS VALID CC            *         
*        ONLY NECESSARY IN CASE ROUTINE PROCESSING ASKS TO            *         
*        VALIDATE/TRANSLATE THIS ICODE. IT REALLY IS A SPECIAL CASE   *         
*        IT IS USED TO STORE A KEYWORD'S MASTER TYPE IN THE           *         
*        FILTER AREA ON-LINE.                                         *         
***********************************************************************         
         SPACE 1                                                                
MTICPROC NTR1  ,                                                                
*                                                                               
         XC    VLTICODE,VLTICODE   NO ICODE                                     
         XC    VLTFULL,VLTFULL     NO EXPANSION                                 
*                                                                               
         LA    RF,0                LENGTH OF RETURNED DATA                      
         STC   RF,VLTLFULL                                                      
         STC   RF,VLTMINI                                                       
         STC   RF,VLTMINO                                                       
*                                                                               
MTICPRCX DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NUMPROC - PROCESS NUMERIC EXPRESSION - MAX 7 DIGITS                 *         
*                                                                     *         
* ON ENTRY R1 = APPARM = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS           *         
*        FIRST  BYTE OF VLTEXTRA IS LENGTH OF BINARY REPRESENTATION   *         
*        NEXT POSITIONS ARE BINARY REPRESENTATION                     *         
***********************************************************************         
         SPACE 1                                                                
NUMPROC  NTR1  ,                                                                
*                                                                               
         OI    VLTIND,VLTNSLNQ     SET NON-STANDARD LENGTH INDICATOR            
*                                                                               
         TM    CALLTYPE,VLPVALQ    VALIDATE CALL                                
         BNO   NMPTRA                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,VLPVLEN        GET INPUT LENGTH                             
         BZ    NMPE1               NO INPUT                                     
         CH    RF,=H'7'            MUST BE AT MOST 7 DIGITS                     
         BH    NMPE3                                                            
*                                                                               
         L     RE,VLPAVAL          POINT TO VALUE                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VALUE(0),0(RE)      MOVE INPUT TO WORKAREA                       
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         NC    VALUE(0),=C'0000000'    KILL NUMERIC PORTION                     
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   VALUE(0),=C'0000000'    MAKE SURE ZONES ARE 'F'                  
         BNE   NMPE2               NOT NUMERIC                                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         PACK NUMBER                                  
*                                                                               
         CVB   RE,DUB              CVB                                          
         STCM  RE,15,VLTEXTRA+1    RETURN IN VLTEXTRA                           
         MVI   VLTEXTRA,4          LENGTH OF EXTRA DATA IS 4                    
*                                                                               
         B     NMPTRA1                                                          
*                                                                               
NMPTRA   TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   NMPHLP                                                           
*                                                                               
         L     RE,VLPAVAL          MOVE INPUT INTO TABLE                        
         MVC   VLTEXTRA+1(4),0(RE)  INPUT IS FULLWORD NUMBER                    
         MVI   VLTEXTRA,4          LENGTH OF EXTRA DATA IS 4                    
*                                                                               
NMPTRA1  DS    0H                                                               
*                                                                               
         XC    VLTICODE,VLTICODE   NO ICODE                                     
*                                                                               
         ICM   RF,15,VLTEXTRA+1    GET BINARY FORM                              
*                                                                               
         CVD   RF,DUB              CVD                                          
*                                                                               
         MVC   VALUE(8),=X'4020202020202020' SET EDIT PATTERN                   
*                                                                               
         LA    R1,VALUE+8          SET DEFAULT MARK ADDRESS                     
*                                                                               
         EDMK  VALUE(8),DUB+4                                                   
*                                                                               
         LA    RF,VALUE                                                         
         LR    R0,R1               COPY FIRST NON-BLANK POINTER                 
         SR    R0,RF               GET DISPLACEMENT TO 1ST DIGIT                
*                                                                               
         LA    RE,8                                                             
         SR    RE,R0               LENGTH OF NON-ZERO PORTION                   
         BNZ   *+8                 NO DATA                                      
*                                                                               
         LA    RE,1                DEFAULT TO 1 DIGIT                           
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VLTFULL(0),0(R1)    RETURN EXPANSION                             
*                                                                               
         LA    RF,1(RE)            LENGTH OF RETURNED DATA                      
         STC   RF,VLTLFULL                                                      
         STC   RF,VLTMINI                                                       
         STC   RF,VLTMINO                                                       
*                                                                               
         ZIC   RF,VLTEXTRA         ADJUST TABLE CONTROL INFO                    
         L     RE,VLTCELEN                                                      
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCELEN                                                      
         L     RE,VLTCAEOT                                                      
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCAEOT                                                      
*                                                                               
         B     NUMPROCX                                                         
*                                                                               
NMPHLP   DS    0H                  IF TABLE CALL RETURN HELP PANEL              
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
         B     NUMPROCX                                                         
*                                                                               
NMPE1    L     RE,VLPAERR          NO DATA                                      
         MVC   0(2,RE),=AL2(VLENONE)                                            
         B     NMPER                                                            
*                                                                               
NMPE2    L     RE,VLPAERR          NOT NUMERIC                                  
         MVC   0(2,RE),=AL2(VLENOT#)                                            
         B     NMPER                                                            
*                                                                               
NMPE3    L     RE,VLPAERR          NO DATA                                      
         MVC   0(2,RE),=AL2(VLELONG)                                            
         B     NMPER                                                            
*                                                                               
NMPER    DS    0H                                                               
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
*                                                                               
NUMPROCX DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PERPROC - PROCESS PERIOD (RANGE OF DATES OR SINGLE DATE)                      
*                                                                     *         
* ON ENTRY R1 = APARMS = A(REVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS           *         
*                                                                     *         
* NOTE- **NON-STANDARD CODE LENGTH** - 3 (SINGLE DATE), 3 (RANGE)     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PERPROC  NTR1  ,                                                                
*                                                                               
         OI    VLTIND,VLTNSLNQ     SET NON-STANDARD LENGTH INDICATOR            
*                                                                               
         TM    CALLTYPE,VLPVALQ    VALIDATE                                     
         BNO   PERPRC20                                                         
*                                                                               
         L     RF,VLACFACS         USE PERVAL FOR VALIDATION                    
         L     RF,CPERVAL-COMFACSD(RF)                                          
*                                                                               
         LA    R4,WORK                                                          
         USING PERVALD,R4                                                       
*                                                                               
         XC    PVALOUTB,PVALOUTB                                                
         MVC   PVALCSTA,VLTODAYC                                                
         ST    R4,TEMP+4                                                        
         MVC   TEMP+4(1),VLLANG                                                 
*                                                                               
         OC    VLTODAYC,VLTODAYC                                                
         BZ    *+8                                                              
         OI    TEMP+4,PVINTOD      TODAY'S DATE PASSED TO AVOID SVC             
*                                                                               
         L     RE,VLPAVAL                                                       
         MVC   VALUE,0(RE)         UPPER CASE                                   
         TR    VALUE,UPPER                                                      
*                                                                               
         OI    TEMP+4,PVINSGLS     TREAT SINGLE AS SINGLE                       
         GOTO1 (RF),TEMP,(VLPVLEN,VALUE)                                        
         TM    4(R1),X'03'         TEST ERROR                                   
         BNZ   PERPRCER                                                         
*                                                                               
         MVI   VLTEXTRA,3          LENGTH OF SINGLE DATE                        
*                                                                               
         MVC   VLTEXTRA+1(3),PVALBSTA                                           
         MVI   VLPVLEN,3           SET FOR REDISPLAY                            
*                                                                               
         CLI   4(R1),PVRCONE       SINGLE DATE FOUND?                           
         BE    PERPRC18                                                         
*                                                                               
         MVI   VLTEXTRA,6          LENGTH OF 2 DATES                            
         MVC   VLTEXTRA+1(6),PVALBSTA  COPY BOTH DATES                          
         MVI   VLPVLEN,6                                                        
*                                                                               
PERPRC18 DS    0H                                                               
*                                                                               
         B     PERPRC30                                                         
*                                                                               
PERPRC20 TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   PERPRC40                                                         
*                                                                               
         L     RE,VLPAVAL                                                       
         MVC   VLTEXTRA+1(6),0(RE)   MOVE DATE(S) TO VLTEXTRA+1                 
         MVC   VLTEXTRA(1),VLPVLEN   AND LENGTH                                 
*                                                                               
PERPRC30 DS    0H                                                               
*                                                                               
         MVC   DTWRK(6),VLTEXTRA+1  COPY DATE(S)                                
*                                                                               
         LA    R0,17               SET FOR STANDARD DAY MONTH YEAR              
*                                                                               
         L     RF,VLACFACS         USE DATCON TO GET STANDARD FORM              
         L     RF,CDATCON-COMFACSD(RF)                                          
*                                                                               
         GOTO1 (RF),TEMP,(X'83',DTWRK),((R0),WORK)                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)            LENGTH IN P2 B0                            
         BZ    PERPRCER                                                         
*                                                                               
         STC   RE,VLTLFULL                                                      
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VLTFULL(0),WORK     MOVE TO OUTPUT AREA                          
*                                                                               
         CLI   VLPVLEN,6           TEST FOR 2 DATES                             
         BNE   PERPRC46                                                         
*                                                                               
         LA    R0,17               SET FOR STANDARD DAY MONTH YEAR              
*                                                                               
         L     RF,VLACFACS         USE DATCON TO GET STANDARD FORM              
         L     RF,CDATCON-COMFACSD(RF)                                          
*                                                                               
         GOTO1 (RF),TEMP,(X'83',DTWRK+3),((R0),WORK)                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)          LENGTH IN P2 B0                              
         BZ    PERPRCER                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,VLTLFULL         GET PREVIOUS LENGTH                          
*                                                                               
         LA    R1,VLTFULL(RF)      POINT TO CURRENT END                         
         MVI   0(R1),C'-'          SET DASH                                     
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),WORK        MOVE TO OUTPUT AREA                          
*                                                                               
         LA    RE,2(RF,RE)         CALCULATE NEW LENGTH                         
         STC   RE,VLTLFULL                                                      
*                                                                               
PERPRC46 DS    0H                                                               
*                                                                               
         MVC   VLTMINI,VLTLFULL    ADJUST LENGTHS                               
         MVC   VLTMINO,VLTLFULL                                                 
         OI    VLTIND,VLTGLOBQ                                                  
*                                                                               
         LA    RF,6                ADJUST TABLE CONTROL INFO                    
         L     RE,VLTCELEN         MAX PERIOD ENTRY LENGTH                      
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCELEN                                                      
         L     RE,VLTCAEOT                                                      
         LA    RE,1(RF,RE)                                                      
         ST    RE,VLTCAEOT                                                      
         B     EXIT                                                             
*                                                                               
PERPRC40 DS    0H                  IF TABLE CALL RETURN HELP PANEL              
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
         B     EXIT                                                             
*                                                                               
PERPRCER L     RE,VLPAERR          INVALID PERIOD                               
         MVC   0(2,RE),=AL2(VLENOTV)                                            
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TXTPROC - PROCESS TEXT QUALIFIER                                              
*                                                                               
* ON ENTRY R1 = APARMS = A(REVAL CALLER'S VLPARMS)                              
* ON EXIT  VLPERR/VLPAERR ARE SET IN REVAL CALLER'S VLPARMS                     
* NOTE - MAX TEXT LENGTH IS 32                                                  
*      - LENGTH IS NON-STANDARD                                                 
*      - VALUE IS SET IN BOTH VLTFULL AND VLTEXTRA                              
***********************************************************************         
         SPACE 1                                                                
TXTPROC  NTR1  ,                                                                
         OI    VLTIND,VLTNSLNQ     SET NON-STANDARD LENGTH IND                  
*                                                                               
         TM    CALLTYPE,VLPVALQ    VALIDATE                                     
         BNO   TXTP04                                                           
         B     TXTP06                                                           
*                                                                               
TXTP04   DS    0H                                                               
         TM    CALLTYPE,VLPTRAQ    TRANSLATE CALL                               
         BNO   TXTP08                                                           
*                                                                               
TXTP06   DS    0H                  VAL AND TRANS SHARE SAME CODE                
         CLI   VLPVLEN,32          MAX LENGTH IS 32                             
         BNH   *+12                                                             
         LA    RF,=AL2(VLELONG)                                                 
         B     TXTPERR                                                          
*                                                                               
         ZIC   RF,VLPVLEN                                                       
         STC   RF,VLTMINI          MINIMUM INPUT                                
         STC   RF,VLTMINO          MINIMUM OUTPUT                               
         STC   RF,VLTLFULL         LENGTH                                       
         STC   RF,VLTEXTRA         ALSO IN VLTEXTRA                             
         L     RE,VLPAVAL          ADDRESS OF VALUE                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VLTFULL(0),0(RE)    SET VALUE                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VLTEXTRA+1(0),0(RE)  ALSO IN VLTEXTRA+1                          
         B     EXIT                                                             
*                                                                               
TXTP08   DS    0H                  IF TABLE CALL RETURN HELP PANEL              
         MVI   VLPTYPE-VLPARMS(R1),VLPHLPQ                                      
         B     EXIT                                                             
*                                                                               
TXTPERR  L     RE,VLPAERR                                                       
         MVC   0(2,RE),0(RF)                                                    
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS TABLES AND EQUATES                                    *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
ALONEQ   EQU   X'FEFE'                                                          
*                                                                               
NULLS    DS    XL12'000000000000000000000000'                                   
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE TABLE FOR CONVERTING TO UPPERCASE                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
UPPER    DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614040404040404040E06B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C9404040404040' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E9404040404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' B0-BF                     
         DC    XL16'4AC1C2C3C4C5C6C7C8C9404040404040' C0-CF                     
         DC    XL16'5AD1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                     
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        SYSTEM TABLE GIVING SYSTEM SPECIFIC TABLE ADDRESSES          *         
*                                           (SYSTABD)                 *         
***********************************************************************         
         SPACE 2                                                                
SYSTAB   DS    0D                                                               
         DC    AL1(VLSYSREQ)       REP   SYSTEM                                 
         DC    XL3'00'             SPARE                                        
         DC    A(RETYPTAB)         A(PARENTLESS ITEMS TABLE)                    
         DC    A(REMASKS)          A(SYSTEM MASKS)                              
         DC    A(REERRTAB)         A(SYSTEM MASK TYPES)                         
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REP                                                          *         
*                                                                     *         
*        TABLE FOR VALUE-MASK CONVERSION - SEE MASKD                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
REMASKS  DS    0C                                                               
*                                                                               
         DC    AL2(VLFLT1V-VALWRKD,VLFLT1M-VALWRKD) DATABASE                    
         DC    AL1(L'VLFLT1M),X'0102030405060708090A0B0C0D0E0F10'               
*                                                                               
         DC    AL2(VLFLT2V-VALWRKD,VLFLT2M-VALWRKD) MEDIA TYPE                  
         DC    AL1(L'VLFLT2M),X'0102030405060708090A0B0C0D0E0F10'               
*                                                                               
         DC    AL2(VLFLT3V-VALWRKD,VLFLT3M-VALWRKD) RECORD TYPE                 
         DC    AL1(L'VLFLT3M),X'0102030405060708090A0B0C0D0E0F10'               
*                                                                               
         DC    AL2(VLFLT4V-VALWRKD,VLFLT4M-VALWRKD) FUNCTION TYPE               
         DC    AL1(L'VLFLT4M),X'0102030405060708090A0B0C0D0E0F10'               
*                                                                               
         DC    AL2(VLFLT5V-VALWRKD,VLFLT5M-VALWRKD) MASK 5                      
         DC    AL1(L'VLFLT5M),X'01020304050607080000000000000000'               
*                                                                               
         DC    AL2(VLFLT6V-VALWRKD,VLFLT6M-VALWRKD) MASK 6                      
         DC    AL1(L'VLFLT6M),X'01020304050607080000000000000000'               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT ERROR CODE TRANSLATION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
REERRTAB DS    0H                                                               
*                                                                               
         DC    AL2(VLENOTV,2)                 NOT VALID                         
         DC    AL2(VLENOTAV,2)                FEATURE NOT AVAILABLE             
         DC    AL2(VLELONG,2)                 ENTRY TOO LONG                    
         DC    AL2(VLENONE,2)                 NO DATA                           
         DC    AL2(VLENOT#,2)                 NOT NUMERIC                       
         DC    AL2(VLEFLT1,2)                 NOT VALID FOR FILTER 1            
         DC    AL2(VLEFLT2,2)                 NOT VALID FOR FILTER 2            
         DC    AL2(VLEFLT3,2)                 NOT VALID FOR FILTER 3            
         DC    AL2(VLEFLT4,2)                 NOT VALID FOR FILTER 4            
         DC    AL2(VLEFLT5,2)                 NOT VALID FOR FILTER 5            
         DC    AL2(VLEFLT6,2)                 NOT VALID FOR FILTER 6            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REP                                                          *         
*                                                                     *         
* TYPTAB - GLOBAL TABLE OF TYPES - SEE TYTYPD                         *         
* PARENTLESS SET AND STANDALONES FIRST, THEN IN ALPHA NAME SEQUENCE   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RETYPTAB DS    0F                                                               
*                                  MASTER = PARENTLESS SET                      
         DC    AL2(0,0,0)                                                       
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(REMSTRTB)                             
         DC    AL1(0),AL3(0)                                                    
*                                     ADVERTISER CITY                           
         DC    AL2(RKQADVCT,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(TXTPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - AS-AT-DATE                        
         DC    AL2(RKQOPAAD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - ACTIVITY DATES                    
         DC    AL2(RKQOPATD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - CONTRACT CREATION                 
         DC    AL2(RKQOPCCD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     AGENCY  OPTIONS                           
         DC    AL2(RKQAGOPT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(AGOPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENUS                                
         DC    AL2(RKQHLPMN,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - STATION BUDGET TYPES          
         DC    AL2(RKQHMBST,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - COMPANY BUDGET TYPES          
         DC    AL2(RKQHMBCO,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - OFFICE  BUDGET TYPES          
         DC    AL2(RKQHMBOF,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - SAL     BUDGET TYPES          
         DC    AL2(RKQHMBSP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - SAL/STA BUDGET TYPES          
         DC    AL2(RKQHMBSS,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - CROSS-FILE GROUPS             
         DC    AL2(RKQHMXFL,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - STATION OPTIONS               
         DC    AL2(RKQHMSOP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - STATION TYPES                 
         DC    AL2(RKQHMSTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER COL FILT/FMTS          
         DC    AL2(RKQHMCFW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER OPTIONS                
         DC    AL2(RKQHMOPW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER KEYWORDS               
         DC    AL2(RKQHMKYW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     BACKBILLING OPTIONS                       
         DC    AL2(RKQBKBOP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(BKOPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     BUDGET TYPES                              
         DC    AL2(RKQBUDTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(BDTPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     ACCESSIBLE REPS                           
         DC    AL2(RKQXFLRP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(XFILTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     STATION AFFILIATIONS                      
         DC    AL2(RKQSTNAF,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(STAFTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     STATION OPTIONS                           
         DC    AL2(RKQSTOPT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(STOPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     STATION TYPES                             
         DC    AL2(RKQSTTPS,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(STTYTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     MASTER TYPE INTERNAL CODE                 
         DC    AL2(RKQMTIC,0,0)                                                 
         DC    AL1(TYT1ADRQ),AL3(MTICPROC)                                      
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT KEYWORDS                              
         DC    AL2(RKQWRKYW,0,0)                                                
         DC    AL1(TYT1TABQ),AL1(RETAB2Q,01,00)                                 
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT OPTIONS                               
         DC    AL2(RKQWROPT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(WOPTTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT COLUMN FILTERS/FORMATS                
         DC    AL2(RKQWRCFL,0,0)                                                
         DC    AL1(TYT1TABQ),AL1(RETAB2Q,02,00)                                 
         DC    AL1(0),AL3(0)                                                    
*                                     YTD START DATE                            
         DC    AL2(RKQOPYTD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                                                               
*                                     DATES - AS-AT-DATE COLUMN FILTER          
         DC    AL2(RKQCFAAD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
         DC    AL2(TYTEOT)                                                      
*                                     DATES - ACTIVITY COLUMN FILTER            
         DC    AL2(RKQCFATD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
         DC    AL2(TYTEOT)                                                      
*                                     DATES - AS-AT-DATE COLUMN FILTER          
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REP                                                          *         
*                                                                     *         
* REMSTRTB - GLOBAL TABLE OF MASTER TYPES (PARENTLESS) -SEE VLGLOBTABD*         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
REMSTRTB DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRAS                                    
*                                  COLUMN FILTERS/FORMATS                       
MSCFL    DC    AL1(MSCFLX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(RKQWRCFL,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSCFLL1  DC    AL1(MSCFLX1-*,LANGEUK,3,6),C'    Filter'                         
MSCFLX1  DS    0C                                                               
MSCFLX   DS    0C                                                               
*                                                                               
MSHLP    DC    AL1(MSHLPX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(RKQHLPMN,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSHLPL1  DC    AL1(MSHLPX1-*,LANGEUK,4,4),C'    Menu'                           
MSHLPX1  DS    0C                                                               
MSHLPX   DS    0C                                                               
*                                                                               
MSOPT    DC    AL1(MSOPTX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(RKQWROPT,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSOPTL1  DC    AL1(MSOPTX1-*,LANGEUK,3,6),C'    Option'                         
MSOPTX1  DS    0C                                                               
MSOPTX   DS    0C                                                               
*                                                                               
         DC    AL1(0)                                                           
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE'                                           
REVAL1   DS    0D                                                               
         DC    AL2(WRITTAB-REVAL1)  01     WRITER REPORTABLE FIELDS             
         DC    AL2(0)               02                                          
         DC    AL2(0)               03                                          
         DC    AL2(WOPTTAB-REVAL1)  04     WRITER OPTIONS                       
         DC    AL2(HLPMTAB-REVAL1)  05     HELP MENUS                           
         DC    AL2(STOPTAB-REVAL1)  06     STATION OPTIONS                      
         DC    AL2(STTYTAB-REVAL1)  07     STATION TYPES                        
         DC    AL2(AGOPTAB-REVAL1)  08     AGENCY OPTIONS                       
         DC    AL2(STAFTAB-REVAL1)  09     STATION AFFILIATIONS                 
         DC    AL2(BDTPTAB-REVAL1)  10     BUDGET TYPES                         
         DC    AL2(XFILTAB-REVAL1)  11     CROSS-FILE TABLE                     
         DC    AL2(0)               12                                          
         TITLE 'REVAL1 - TABLE PHASE - 01 - WRITER REPORTABLE FIELDS'           
***********************************************************************         
*                                                                     *         
* (01)   WRITER REPORTABLE FIELDS                                     *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN REGLOBEQUS  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
WRITTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRA BYTES OF DATA PER ENTRY             
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
*              ALL ENTRIES MUST BE MAX 16 CHARACTERS                            
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 04 - WRITER OPTIONS'                     
***********************************************************************         
*        WRITER OPTIONS (04)                                                    
***********************************************************************         
         SPACE 1                                                                
WOPTTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*              ALLAGY FILE LIST                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPAAG   DC    AL1(WOPAAGX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPAAG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNLST)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(0)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPAAGL1 DC    AL1(WOPAAGX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLAGY'                FULL NAME                               
WOPAAGX1 DS    0C                                                               
WOPAAGX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ALLSTA FILE LIST                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPAST   DC    AL1(WOPASTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPAST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNLST)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPASTL1 DC    AL1(WOPASTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLSTA'                FULL NAME                               
WOPASTX1 DS    0C                                                               
WOPASTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ALTERNATE CALENDAR                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPALT   DC    AL1(WOPALTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPALT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPALTL1 DC    AL1(WOPALTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'AC  '                SHORT NAME-USE START OF FULL            
         DC    C'ALTCAL'                FULL NAME                               
WOPALTX1 DS    0C                                                               
WOPALTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ALL DOWNLOAD IS ALPHA                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPALP   DC    AL1(WOPALPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPALF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPALPL1 DC    AL1(WOPALPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLALPH'               FULL NAME                               
WOPALPX1 DS    0C                                                               
WOPALPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              Activity dates                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPATD   DC    AL1(WOPATDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPATD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPATDL1 DC    AL1(WOPATDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'ATD '                SHORT NAME-USE START OF FULL            
         DC    C'ACTIVITY='              FULL NAME                              
WOPATDX1 DS    0C                                                               
WOPATDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              AS-AT-DATE                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPAAD   DC    AL1(WOPAADX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPAAD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPAADL1 DC    AL1(WOPAADX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'AAD '                SHORT NAME-USE START OF FULL            
         DC    C'ASATDATE='              FULL NAME                              
WOPAADX1 DS    0C                                                               
WOPAADX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              BACKBILLING OPTIONS                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBKB   DC    AL1(WOPBKBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBKB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBKBL1 DC    AL1(WOPBKBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BKB '                SHORT NAME-USE START OF FULL            
         DC    C'BACKBILL=+'            FULL NAME                               
WOPBKBX1 DS    0C                                                               
WOPBKBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              BACKOUT DROPPED DETAILS FROM TOTALS                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBCK   DC    AL1(WOPBCKX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBCK)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBCKL1 DC    AL1(WOPBCKX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Backout'               FULL NAME                               
WOPBCKX1 DS    0C                                                               
WOPBCKX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              CONTRACT CREATION DATES                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCCD   DC    AL1(WOPCCDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCCD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCCDL1 DC    AL1(WOPCCDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(9)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'CCD '                SHORT NAME-USE START OF FULL            
         DC    C'CONCREATE='             FULL NAME                              
WOPCCDX1 DS    0C                                                               
WOPCCDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        INCLUDE BOXES ON REPORT (BOX = Y/N)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBOX   DC    AL1(WOPBOXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBOX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBOXL1 DC    AL1(WOPBOXX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BOX='                  FULL NAME                               
WOPBOXX1 DS    0C                                                               
WOPBOXX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STABUD  - BUDGET TYPE - STATION                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBST   DC    AL1(WOPBSTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBTY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBSTL1 DC    AL1(WOPBSTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BST '                SHORT NAME-USE START OF FULL            
         DC    C'STABUD=+'              FULL NAME                               
WOPBSTX1 DS    0C                                                               
WOPBSTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        COMBUD  - BUDGET TYPE - COMPANY                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBCO   DC    AL1(WOPBCOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBCO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBTY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBCOL1 DC    AL1(WOPBCOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BCO '                SHORT NAME-USE START OF FULL            
         DC    C'COMBUD=+'              FULL NAME                               
WOPBCOX1 DS    0C                                                               
WOPBCOX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        OFFBUD  - BUDGET TYPE - OFFICE                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBOF   DC    AL1(WOPBOFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBOF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBTY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBOFL1 DC    AL1(WOPBOFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BOF '                SHORT NAME-USE START OF FULL            
         DC    C'OFFBUD=+'              FULL NAME                               
WOPBOFX1 DS    0C                                                               
WOPBOFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SALBUD  - BUDGET TYPE - SALESPERSON                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBSP   DC    AL1(WOPBSPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBTY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBSPL1 DC    AL1(WOPBSPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BSP '                SHORT NAME-USE START OF FULL            
         DC    C'SALBUD=+'              FULL NAME                               
WOPBSPX1 DS    0C                                                               
WOPBSPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SASTBUD  - BUDGET TYPE - SALESPERSON/STATION                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBSS   DC    AL1(WOPBSSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBSS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBTY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBSSL1 DC    AL1(WOPBSSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BSS '                SHORT NAME-USE START OF FULL            
         DC    C'SASTBUD=+'             FULL NAME                               
WOPBSSX1 DS    0C                                                               
WOPBSSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BTYPE1   - BUYCODE                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBCD   DC    AL1(WOPBCDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBCD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBCDL1 DC    AL1(WOPBCDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BCD '                SHORT NAME-USE START OF FULL            
         DC    C'BCODE='                FULL NAME                               
WOPBCDX1 DS    0C                                                               
WOPBCDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BTYPE1   - BUY TYPE 1                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBT1   DC    AL1(WOPBT1X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBT1)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBT1L1 DC    AL1(WOPBT1X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BT1 '                SHORT NAME-USE START OF FULL            
         DC    C'BTYPE1='               FULL NAME                               
WOPBT1X1 DS    0C                                                               
WOPBT1X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BTYPE2   - BUY TYPE 2                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBT2   DC    AL1(WOPBT2X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBT2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBT2L1 DC    AL1(WOPBT2X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BT2 '                SHORT NAME-USE START OF FULL            
         DC    C'BTYPE2='               FULL NAME                               
WOPBT2X1 DS    0C                                                               
WOPBT2X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BUYCOM - REPORT ONLY COMPLETE CONTRACTS                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBCM   DC    AL1(WOPBCMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBCM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBCML1 DC    AL1(WOPBCMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BUYCOM'                FULL NAME                               
WOPBCMX1 DS    0C                                                               
WOPBCMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BUYINC - REPORT ONLY INCOMPLETE CONTRACTS                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBIN   DC    AL1(WOPBINX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBIN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBINL1 DC    AL1(WOPBINX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BUYINC'                FULL NAME                               
WOPBINX1 DS    0C                                                               
WOPBINX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BUYLOSS - REPORT ONLY LOST CONTRACTS                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBLS   DC    AL1(WOPBLSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBLS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBLSL1 DC    AL1(WOPBLSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BUYLOSS'               FULL NAME                               
WOPBLSX1 DS    0C                                                               
WOPBLSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BUYPND - REPORT ONLY PENDING CONTRACTS                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBPD   DC    AL1(WOPBPDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPBPD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBPDL1 DC    AL1(WOPBPDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BUYPND'                FULL NAME                               
WOPBPDX1 DS    0C                                                               
WOPBPDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XBUYCOM - EXCLUDE COMPLETE CONTRACTS                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXCM   DC    AL1(WOPXCMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXCM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXCML1 DC    AL1(WOPXCMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XBUYCOM'               FULL NAME                               
WOPXCMX1 DS    0C                                                               
WOPXCMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XBUYINC - EXCLUDE INCOMPLETE CONTRACTS                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXIN   DC    AL1(WOPXINX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXIN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXINL1 DC    AL1(WOPXINX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XBUYINC'               FULL NAME                               
WOPXINX1 DS    0C                                                               
WOPXINX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XBUYLOSS - EXCLUDE LOST CONTRACTS                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXLS   DC    AL1(WOPXLSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXLS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXLSL1 DC    AL1(WOPXLSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XBUYLOSS'              FULL NAME                               
WOPXLSX1 DS    0C                                                               
WOPXLSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XBUYPND - EXCLUDE PENDING CONTRACTS                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXPD   DC    AL1(WOPXPDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXPD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXPDL1 DC    AL1(WOPXPDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XBUYPND'               FULL NAME                               
WOPXPDX1 DS    0C                                                               
WOPXPDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CATEGORY FILTER CATEGORY =                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCAT   DC    AL1(WOPCATX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCAT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCATL1 DC    AL1(WOPCATX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'CTG '                SHORT NAME-USE START OF FULL            
         DC    C'CATEGORY='             FULL NAME                               
WOPCATX1 DS    0C                                                               
WOPCATX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CLASS FILTER CL=                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCL    DC    AL1(WOPCLX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCLS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCLL1 DC     AL1(WOPCLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'CLS '                SHORT NAME-USE START OF FULL            
         DC    C'CLASS='                FULL NAME                               
WOPCLX1 DS     0C                                                               
WOPCLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        COMBOS                                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCMB   DC    AL1(WOPCMBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCMB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCMBL1 DC    AL1(WOPCMBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'COMBOS='               FULL NAME                               
WOPCMBX1 DS    0C                                                               
WOPCMBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CONFIRMED DOLLARS                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCF    DC    AL1(WOPCFX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCF)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCFL1 DC     AL1(WOPCFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(17)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'CF  '                SHORT NAME-USE START OF FULL            
         DC    C'CONFIRMED'             FULL NAME                               
WOPCFX1 DS     0C                                                               
WOPCFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        UNCONFIRMED DOLLARS                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPUC    DC    AL1(WOPUCX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPUC)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(RKMFLCON)            FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPUCL1 DC     AL1(WOPUCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(19)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'UC  '                SHORT NAME-USE START OF FULL            
         DC    C'UNCONFIRMED'           FULL NAME                               
WOPUCX1 DS     0C                                                               
WOPUCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CONTRACT NUMBER =                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCON   DC    AL1(WOPCONX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCON)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCONL1 DC    AL1(WOPCONX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONTRACT='             FULL NAME                               
WOPCONX1 DS    0C                                                               
WOPCONX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        CONTRACT TYPE FILTER CTP=,CONTYPE                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCT    DC    AL1(WOPCTX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCTY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCTL1 DC     AL1(WOPCTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'CTP '                SHORT NAME-USE START OF FULL            
         DC    C'CONTYPE='              FULL NAME                               
WOPCTX1 DS     0C                                                               
WOPCTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        CURRENT DOLLARS                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCDL   DC    AL1(WOPCDLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCDL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCDLL1 DC    AL1(WOPCDLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(04)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CUR$'                  FULL NAME                               
WOPCDLX1 DS    0C                                                               
WOPCDLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        CROSS FILE REPORTING - LIST OF REPS TO BE INCLUDED                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXFL   DC    AL1(WOPXFLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXFLL1 DC    AL1(WOPXFLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'XFL '                SHORT NAME-USE START OF FULL            
         DC    C'XFILES=+'              FULL NAME                               
WOPXFLX1 DS    0C                                                               
WOPXFLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DON'T WRAP ANY FIELD TO NEXT LINE (CUT)                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCUT   DC    AL1(WOPCUTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCUT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCUTL1 DC    AL1(WOPCUTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CUT'                   FULL NAME                               
WOPCUTX1 DS    0C                                                               
WOPCUTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ALL DETAILS (ALLDET)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTA   DC    AL1(WOPDTAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDTA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT+RKMMNDWN)   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDTAL1 DC    AL1(WOPDTAX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLDET'                FULL NAME                               
WOPDTAX1 DS    0C                                                               
WOPDTAX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DOWN LOAD TO TAPE (DOWNTAPE)                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTP   DC    AL1(WOPDTPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    XL1'60'                  NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDTPL1 DC    AL1(WOPDTPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNTAPE'              FULL NAME                               
WOPDTPX1 DS    0C                                                               
WOPDTPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DOWN LOAD HEADLINES (DOWNHEAD)                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDHD   DC    AL1(WOPDHDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDHD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDHDL1 DC    AL1(WOPDHDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNHEAD'              FULL NAME                               
WOPDHDX1 DS    0C                                                               
WOPDHDX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DEVELOPMENTAL CONTRACT TYPE FILTER DCT=                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDT    DC    AL1(WOPDTX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDCT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDTL1 DC     AL1(WOPDTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'DCT '                SHORT NAME-USE START OF FULL            
         DC    C'DCONTYPE='             FULL NAME                               
WOPDTX1 DS     0C                                                               
WOPDTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DIRECT RESPONSE                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDR    DC    AL1(WOPDRX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDR)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDRL1 DC     AL1(WOPDRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'  '                  SHORT NAME-USE START OF FULL            
         DC    C'DR'                    FULL NAME                               
WOPDRX1 DS     0C                                                               
WOPDRX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        DEVELOPMENTAL SALESPERSON FILTER DS=                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDS    DC    AL1(WOPDSX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDSL1 DC     AL1(WOPDSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'DSP '                SHORT NAME-USE START OF FULL            
         DC    C'DSLSPRSN='             FULL NAME                               
WOPDSX1 DS     0C                                                               
WOPDSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        DOWNLOAD REPORT (DOWN)                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDWN   DC    AL1(WOPDWNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDWN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDWNL1 DC    AL1(WOPDWNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWN'                  FULL NAME                               
WOPDWNX1 DS    0C                                                               
WOPDWNX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DOWNLOAD TOTALS (DOWNTOT)                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTL   DC    AL1(WOPDTLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDTL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDTLL1 DC    AL1(WOPDTLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNTOT=*'             FULL NAME                               
WOPDTLX1 DS    0C                                                               
WOPDTLX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DOWNLOAD - ELIMINATE EXTRA BLANK COLUMN (DOWNFIX)                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDFX   DC    AL1(WOPDFXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDFX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDFXL1 DC    AL1(WOPDFXX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNFIX'               FULL NAME                               
WOPDFXX1 DS    0C                                                               
WOPDFXX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DOWNLOAD - HANDLE FIELDS LONGER THAN 128 CHARACTERS (DOWNLONG)         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDLG   DC    AL1(WOPDLGX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDLG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDLGL1 DC    AL1(WOPDLGX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNLONG'              FULL NAME                               
WOPDLGX1 DS    0C                                                               
WOPDLGX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DOWNLOAD TOTALS WITH TOTAL LINE TEXT (DOWNTOTT)                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTT   DC    AL1(WOPDTTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDTT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDTTL1 DC    AL1(WOPDTTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DOWNTOTT=*'            FULL NAME                               
WOPDTTX1 DS    0C                                                               
WOPDTTX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        FLOAT MINUS SIGN BEFORE NEGATIVE NUMBERS                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
***FLT   DC    AL1(WOPFLTX-*)           ENTRY LENGTH                            
***      DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
***      DC    AL2(RKQOPFLT)            INTERNAL CODE                           
***      DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
***      DC    AL2(Y)                   UNUSED        MASK                      
***      DC    AL2(Y)                   MEDIA    TYPE MASK                      
***      DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
***      DC    AL2(Y)                   FLAVOR   TYPE MASK                      
***      DC    AL1(Y)                   FIELD    TYPE MASK                      
***      DC    AL1(Y)                   UNUSED   TYPE MASK                      
***                  LANGUAGE DATA IN TABLE ENTRY                               
***FLTL1 DC    AL1(WOPFLTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
***      DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
***      DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
***      DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
***      DC    CL4'    '                SHORT NAME-USE START OF FULL            
***      DC    C'FLOAT='                FULL NAME                               
***FLTX1 DS    0C                                                               
***FLTX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        LEFT ALIGN REPORT (LEFT)                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPLEF   DC    AL1(WOPLEFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPLEF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPLEFL1 DC    AL1(WOPLEFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'LEFT'                  FULL NAME                               
WOPLEFX1 DS    0C                                                               
WOPLEFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRINT GRAND TOTALS (GRAND)                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPGRN   DC    AL1(WOPGRNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPGRN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPGRNL1 DC    AL1(WOPGRNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'GRAND'                 FULL NAME                               
WOPGRNX1 DS    0C                                                               
WOPGRNX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        SUPPRESS TOTAL LINE IF ONLY 1 DETAIL (MINTOT)                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPMNT   DC    AL1(WOPMNTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPMNT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPMNTL1 DC    AL1(WOPMNTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MINTOT'                FULL NAME                               
WOPMNTX1 DS    0C                                                               
WOPMNTX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        80 COLUMN REPORT (NARROW)                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNAR   DC    AL1(WOPNARX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNAR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNARL1 DC    AL1(WOPNARX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NARROW'                FULL NAME                               
WOPNARX1 DS    0C                                                               
WOPNARX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        NETWORK CONTRACT NUMBER =                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNCT   DC    AL1(WOPNCTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNCT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNCTL1 DC    AL1(WOPNCTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NCONTRACT='            FULL NAME                               
WOPNCTX1 DS    0C                                                               
WOPNCTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DROP HEADINGS FROM REPORT (NOHEAD)                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNHD   DC    AL1(WOPNHDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNHD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT+RKMMNDWN)   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNHDL1 DC    AL1(WOPNHDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NOHEAD'                FULL NAME                               
WOPNHDX1 DS    0C                                                               
WOPNHDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        DON'T PRINT REQUEST DETAILS PAGE                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNRD   DC    AL1(WOPNRDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT+RKMMNDWN)   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNRDL1 DC    AL1(WOPNRDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NOREQDET'              FULL NAME                               
WOPNRDX1 DS    0C                                                               
WOPNRDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DON'T TRUNCATE ANY TEXT (NOTRUNC)                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNTR   DC    AL1(WOPNTRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNTR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT+RKMMNDWN)   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNTRL1 DC    AL1(WOPNTRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NOTRUNC'               FULL NAME                               
WOPNTRX1 DS    0C                                                               
WOPNTRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ZERO NUMBERS AS BLANKS (PBLANK) - DEFAULT                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPBL   DC    AL1(WOPPBLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPPBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    XL1'60'                  NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPBLL1 DC    AL1(WOPPBLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PBLANK'                FULL NAME                               
WOPPBLX1 DS    0C                                                               
WOPPBLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        REPORT ZERO NUMBERS AS ZEROS  (PZERO)                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPZR   DC    AL1(WOPPZRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPPZR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPZRL1 DC    AL1(WOPPZRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Noblank'               FULL NAME                               
WOPPZRX1 DS    0C                                                               
WOPPZRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FORECAST ORDERS ONLY                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPFR    DC    AL1(WOPFRX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPFR)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPFRL1 DC     AL1(WOPFRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'FR  '                SHORT NAME-USE START OF FULL            
         DC    C'FORECAST'              FULL NAME                               
WOPFRX1  DS    0C                                                               
WOPFRX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDE FORCAST ORDERS                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXFR   DC    AL1(WOPXFRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXFR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXFRL1 DC    AL1(WOPXFRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'XFR '                SHORT NAME-USE START OF FULL            
         DC    C'XFORECAST'             FULL NAME                               
WOPXFRX1 DS    0C                                                               
WOPXFRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        MARKET FILTER MKT=                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPMKT   DC    AL1(WOPMKTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPMKT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPMKTL1 DC    AL1(WOPMKTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'MKT '                SHORT NAME-USE START OF FULL            
         DC    C'MARKET='               FULL NAME                               
WOPMKTX1 DS    0C                                                               
WOPMKTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        OWNER FILTER OWN=                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPOWN   DC    AL1(WOPOWNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPOWN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPOWNL1 DC    AL1(WOPOWNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'OWN '                SHORT NAME-USE START OF FULL            
         DC    C'OWNER='                FULL NAME                               
WOPOWNX1 DS    0C                                                               
WOPOWNX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRODUCE PRINT QUEUE INDICES PQIX                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPQIP   DC    AL1(WOPPQIX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPPQI)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPQIL1 DC    AL1(WOPPQIX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PQIX'                  FULL NAME                               
WOPPQIX1 DS    0C                                                               
WOPPQIX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        POINT PERSON FILTER PP=                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPP    DC    AL1(WOPPPX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPPTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPPL1 DC     AL1(WOPPPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'PTP '                SHORT NAME-USE START OF FULL            
         DC    C'PTPERSON='             FULL NAME                               
WOPPPX1 DS     0C                                                               
WOPPPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PAID PROGRAMMING                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPPD   DC    AL1(WOPPPDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPPPD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPPDL1 DC    AL1(WOPPPDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4' '                   SHORT NAME-USE START OF FULL            
         DC    C'PP'                    FULL NAME                               
WOPPPDX1 DS    0C                                                               
WOPPPDX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*        FLAVOR=RGON REPORT                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRRG   DC    AL1(WOPRRGX-*)           ENTRY LERRGTH                           
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNRG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LARRGUAGE DATA IN TABLE ENTRY                              
WOPRRGL1 DC    AL1(WOPRRGX1-*)          LERRGTH OF LARRGUAGE SUB-ENTRY          
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LERRGTH                  
         DC    AL1(9)                   MINIMUM OUTPUT LERRGTH                  
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NRRG'                  FULL NAME                               
WOPRRGX1 DS    0C                                                               
WOPRRGX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FLAVOR=RGON REPORT                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNG    DC    AL1(WOPNGX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNRG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    XL1'60'                  NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNGL1 DC     AL1(WOPNGX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NRGON'                 FULL NAME                               
WOPNGX1 DS     0C                                                               
WOPNGX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        FLAVOR=RGON REPORT                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNRG   DC    AL1(WOPNRGX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPNRG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    XL1'60'                  NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNRGL1 DC    AL1(WOPNRGX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'RGON'                  FULL NAME                               
WOPNRGX1 DS    0C                                                               
WOPNRGX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FLAVOR=READ CONTRACTS IN FLIGHT DATE ORDER                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPFDS   DC    AL1(WOPFDSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPFDS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    XL1'60'                  NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPFDSL1 DC    AL1(WOPFDSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'FLIGHT'                FULL NAME                               
WOPFDSX1 DS    0C                                                               
WOPFDSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REP FILTER REP=                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPREP   DC    AL1(WOPREPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPREP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPREPL1 DC    AL1(WOPREPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'REP='                  FULL NAME                               
WOPREPX1 DS    0C                                                               
WOPREPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        ROUND TOTALS (ROUND)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRND   DC    AL1(WOPRNDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPRND)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRNDL1 DC    AL1(WOPRNDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ROUND'                 FULL NAME                               
WOPRNDX1 DS    0C                                                               
WOPRNDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SECOND REPORT ID                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRP2   DC    AL1(WOPRP2X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPRP2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRP2L1 DC    AL1(WOPRP2X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'REPORT2='              FULL NAME                               
WOPRP2X1 DS    0C                                                               
WOPRP2X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BILLING OPTION R1                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPR1    DC    AL1(WOPR1X-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPR1)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPR1L1 DC     AL1(WOPR1X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4' '                   SHORT NAME-USE START OF FULL            
         DC    C'R1'                    FULL NAME                               
WOPR1X1 DS     0C                                                               
WOPR1X DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        BILLING OPTION R2                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPR2    DC    AL1(WOPR2X-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPR2)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPR2L1 DC     AL1(WOPR2X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4' '                   SHORT NAME-USE START OF FULL            
         DC    C'R2'                    FULL NAME                               
WOPR2X1 DS     0C                                                               
WOPR2X DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        BILLING OPTION R3                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPR3    DC    AL1(WOPR3X-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPR3)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPR3L1 DC     AL1(WOPR3X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4' '                   SHORT NAME-USE START OF FULL            
         DC    C'R3'                    FULL NAME                               
WOPR3X1 DS     0C                                                               
WOPR3X DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        BILLING OPTION R5                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPR5    DC    AL1(WOPR5X-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPR5)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNBLL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPR5L1 DC     AL1(WOPR5X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4' '                   SHORT NAME-USE START OF FULL            
         DC    C'R5'                    FULL NAME                               
WOPR5X1 DS     0C                                                               
WOPR5X DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        STANDARD COMMENT 1 (STCOM1)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSC1   DC    AL1(WOPSC1X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSC1)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSC1L1 DC    AL1(WOPSC1X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'STCOM1='               FULL NAME                               
WOPSC1X1 DS    0C                                                               
WOPSC1X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STANDARD COMMENT 2 (STCOM2)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSC2   DC    AL1(WOPSC2X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSC2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSC2L1 DC    AL1(WOPSC2X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'STCOM2='               FULL NAME                               
WOPSC2X1 DS    0C                                                               
WOPSC2X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SPACING BETWEEN LINES (S)                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSPC   DC    AL1(WOPSPCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSPC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSPCL1 DC    AL1(WOPSPCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'S='                    FULL NAME                               
WOPSPCX1 DS    0C                                                               
WOPSPCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SOLID  (SOLID)                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSOL   DC    AL1(WOPSOLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSOL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSOLL1 DC    AL1(WOPSOLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'SOLID'                 FULL NAME                               
WOPSOLX1 DS    0C                                                               
WOPSOLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        STATION TYPE (STATYPE=)                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSTY   DC    AL1(WOPSTYX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSTY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSTYL1 DC    AL1(WOPSTYX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'STATYPE='              FULL NAME                               
WOPSTYX1 DS    0C                                                               
WOPSTYX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        STRIP REPORT (STRIP)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSTP   DC    AL1(WOPSTPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPSTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSTPL1 DC    AL1(WOPSTPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'STRIP'                 FULL NAME                               
WOPSTPX1 DS    0C                                                               
WOPSTPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY TRUE DARE CONTRACTS                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTDR   DC    AL1(WOPTDRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPTDR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTDRL1 DC    AL1(WOPTDRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TDARE'                 FULL NAME                               
WOPTDRX1 DS    0C                                                               
WOPTDRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDE TRUE DARE CONTRACTS FROM REPORT                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXTD   DC    AL1(WOPXTDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXTD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXTDL1 DC    AL1(WOPXTDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XTDARE'                FULL NAME                               
WOPXTDX1 DS    0C                                                               
WOPXTDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        REPORT ALL TOTALS (ALLTOT)                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTOT   DC    AL1(WOPTOTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPTOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTOTL1 DC    AL1(WOPTOTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLTOT'                FULL NAME                               
WOPTOTX1 DS    0C                                                               
WOPTOTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        ONLY TOP NNN OF RANKING TO PRINT (TOP)                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTOP   DC    AL1(WOPTOPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPTOP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTOPL1 DC    AL1(WOPTOPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TOP='                  FULL NAME                               
WOPTOPX1 DS    0C                                                               
WOPTOPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITE REPORT TO TAPE (TAPE)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTAP   DC    AL1(WOPTAPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPTAP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTAPL1 DC    AL1(WOPTAPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TAPE'                  FULL NAME                               
WOPTAPX1 DS    0C                                                               
WOPTAPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SET TRACE ON (TRACE)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTRA   DC    AL1(WOPTRAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPTRA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCDD)            DDS ONLY                                
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTRAL1 DC    AL1(WOPTRAX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TRACE'                 FULL NAME                               
WOPTRAX1 DS    0C                                                               
WOPTRAX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        VARIABLE VALUE SET                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPVAR   DC    AL1(WOPVARX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPVAR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(RKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPVARL1 DC    AL1(WOPVARX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'&&VAR='                FULL NAME                               
WOPVARX1 DS    0C                                                               
WOPVARX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        WIDE REPORT - 165 (WIDE)                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPWID   DC    AL1(WOPWIDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPWID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPWIDL1 DC    AL1(WOPWIDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'WIDE'                  FULL NAME                               
WOPWIDX1 DS    0C                                                               
WOPWIDX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        EXTRA LINE AT END OF ROWS (XBOX)                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXBX   DC    AL1(WOPXBXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXBX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXBXL1 DC    AL1(WOPXBXX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XBOX'                  FULL NAME                               
WOPXBXX1 DS    0C                                                               
WOPXBXX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DATES IN YYMMDD FORMAT (YYMMDD)                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPYMD   DC    AL1(WOPYMDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPYMD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPYMDL1 DC    AL1(WOPYMDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'YYMMDD'                FULL NAME                               
WOPYMDX1 DS    0C                                                               
WOPYMDX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DATES IN CCYYMMDD FORMAT (CCYYMMDD)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCYM   DC    AL1(WOPCYMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPCYM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCYML1 DC    AL1(WOPCYMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CCYYMMDD'              FULL NAME                               
WOPCYMX1 DS    0C                                                               
WOPCYMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY DARE CONTRACTS                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDAR   DC    AL1(WOPDARX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPDAR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDARL1 DC    AL1(WOPDARX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DARE'                  FULL NAME                               
WOPDARX1 DS    0C                                                               
WOPDARX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDE DARE CONTRACTS FROM REPORT                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXDR   DC    AL1(WOPXDRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXDR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXDRL1 DC    AL1(WOPXDRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XDARE'                 FULL NAME                               
WOPXDRX1 DS    0C                                                               
WOPXDRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        YTD START DATE                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPYTD   DC    AL1(WOPYTDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPYTD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNOTR)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPYTDL1 DC    AL1(WOPYTDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'YTD='                  FULL NAME                               
WOPYTDX1 DS    0C                                                               
WOPYTDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY XML CONTRACTS                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXML   DC    AL1(WOPXMLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXML)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXMLL1 DC    AL1(WOPXMLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XML'                   FULL NAME                               
WOPXMLX1 DS    0C                                                               
WOPXMLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDE XML CONTRACTS FROM REPORT                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXXM   DC    AL1(WOPXXMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXXM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXXML1 DC    AL1(WOPXXMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XXML'                  FULL NAME                               
WOPXXMX1 DS    0C                                                               
WOPXXMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*MNRCU                                                                          
*                                                                               
*        RCU : REPPAK SCRIPT UPLOAD ORDERS ONLY                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRCU   DC    AL1(WOPRCUX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPRCU)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRCUL1 DC    AL1(WOPRCUX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'RCU '                SHORT NAME-USE START OF FULL            
         DC    C'RCSCRIPT'              FULL NAME                               
WOPRCUX1 DS    0C                                                               
WOPRCUX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XRCU EXCLUDE REPPAK SCRIP UPLOAD CONTRACTS                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXRC   DC    AL1(WOPXRCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXRC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXRCL1 DC    AL1(WOPXRCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'XRCU'                SHORT NAME-USE START OF FULL            
         DC    C'XRCSCRIPT'             FULL NAME                               
WOPXRCX1 DS    0C                                                               
WOPXRCX  DS    0C                       END OF TABLE ENTRY                      
*MNRCU                                                                          
*MNRCU                                                                          
*                                                                               
*        RCM : REPPAK SCRIPT MANUAL ORDERS ONLY                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRCM   DC    AL1(WOPRCMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPRCM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRCML1 DC    AL1(WOPRCMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'RCM '                SHORT NAME-USE START OF FULL            
         DC    C'RMSCRIPT'              FULL NAME                               
WOPRCMX1 DS    0C                                                               
WOPRCMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        XRCU EXCLUDE REPPAK SCRIP MANUAL CONTRACTS                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXRM   DC    AL1(WOPXRMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQOPXRM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXRML1 DC    AL1(WOPXRMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'XRCM'                SHORT NAME-USE START OF FULL            
         DC    C'XRMSCRIPT'             FULL NAME                               
WOPXRMX1 DS    0C                                                               
WOPXRMX  DS    0C                       END OF TABLE ENTRY                      
*MNRCU                                                                          
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 11 - HELP MENUS'                         
***********************************************************************         
*        HELP MENUS (11)                                                        
***********************************************************************         
         SPACE 1                                                                
HLPMTAB  DS    0F                                                               
         DC    AL1(182)                 EXTRA BYTES                             
HTBCMNYQ EQU   X'80'               MULTIPLE SELECTS IN SEPARATE FIELDS          
HTBCSAMQ EQU   X'40'               MULTIPLE SELECTS IN SINGLE   FIELDS          
HTBCSLAQ EQU   X'20'               SEPARATE MULTIPLES WITH SLASH                
*                                                                               
*                                                                               
*        WRITER OPTIONS                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOPW    DC    AL1(HMOPWX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOPW)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOPWXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNUOP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20' Repwriter Options',CL30' '                         
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C'                                 '                             
*                                                                               
HMOPWL1  DC    AL1(HMOPWX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Options'               FULL NAME                               
HMOPWX1  DS    0C                                                               
HMOPWX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER OPTIONS - FORMATS                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOFM    DC    AL1(HMOFMX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOFM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOFMXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNFMT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32' Repwriter Options - Formatting ',CL24' '           
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMOFML1  DC    AL1(HMOFMX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Formats'               FULL NAME                               
HMOFMX1  DS    0C                                                               
HMOFMX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - DOWNLOAD                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMODN    DC    AL1(HMODNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMODN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMODNXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNDWN)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30' Repwriter Options - Download ',CL25' '             
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMODNL1  DC    AL1(HMODNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Download'              FULL NAME                               
HMODNX1  DS    0C                                                               
HMODNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER OPTIONS - FILTERS                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOFL    DC    AL1(HMOFLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOFLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNFLT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30' Repwriter Options - Filters  ',CL25' '             
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMOFLL1  DC    AL1(HMOFLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Filters'               FULL NAME                               
HMOFLX1  DS    0C                                                               
HMOFLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - DATE FILTERS                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMODF    DC    AL1(HMODFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMODF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMODFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNDFL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL34' Repwriter Options - Date Filters ',CL21' '         
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMODFL1  DC    AL1(HMODFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Date Filters'          FULL NAME                               
HMODFX1  DS    0C                                                               
HMODFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - BILLING                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBLL    DC    AL1(HMBLLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBLLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNBLL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30' Repwriter Options - Billing  ',CL25' '             
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMBLLL1  DC    AL1(HMBLLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Billing'               FULL NAME                               
HMBLLX1  DS    0C                                                               
HMBLLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - BUDGET TYPES                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBTY    DC    AL1(HMBTYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOBT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBTYXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNBTY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34' Repwriter Options - Budget Types ',CL23' '         
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C'                                 '                             
*                                                                               
HMBTYL1  DC    AL1(HMBTYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Budget Types'          FULL NAME                               
HMBTYX1  DS    0C                                                               
HMBTYX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - FILE LISTINGS                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMLST    DC    AL1(HMLSTX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOLS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMLSTXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNLST)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34' Repwriter Options - File Listings',CL23' '         
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMLSTL1  DC    AL1(HMLSTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(13)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'File Listings'         FULL NAME                               
HMLSTX1  DS    0C                                                               
HMLSTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - OTHERS                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOTH    DC    AL1(HMOTHX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMOOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOTHXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNOTR)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30' Repwriter Options - Others   ',CL25' '             
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMOTHL1  DC    AL1(HMOTHX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Others'                FULL NAME                               
HMOTHX1  DS    0C                                                               
HMOTHX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER COLUMN FILTERS/FORMATS                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMCFW    DC    AL1(HMCFWX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMCFW)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFWXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNUCF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34' Repwriter Column Filters/Formats ',CL23' '         
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C'                                 '                             
*                                                                               
HMCFWL1  DC    AL1(HMCFWX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Colfilters'            FULL NAME                               
HMCFWX1  DS    0C                                                               
HMCFWX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER COLUMN FILTERS/FORMATS - FORMATS                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMCFM    DC    AL1(HMCFMX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMCFM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFMXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCFM)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL16' '                                                          
         DC    CL63' Repwriter Column Filters/Formats - Formatting '            
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''. '           
         DC    C' Multiple Selections Allowed.   '                              
*                                                                               
HMCFML1  DC    AL1(HMCFMX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Formats'               FULL NAME                               
HMCFMX1  DS    0C                                                               
HMCFMX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER COLUMN FILTERS/FORMATS - FILTERS                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMCFL    DC    AL1(HMCFLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMCFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCFL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL14' '                                                          
         DC    CL65' Repwriter Column Filters/Formats - Filters '               
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''. '           
         DC    C' Multiple Selections Allowed.   '                              
*                                                                               
HMCFLL1  DC    AL1(HMCFLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Filters'               FULL NAME                               
HMCFLX1  DS    0C                                                               
HMCFLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER COLUMN FILTERS/FORMATS - DATE FILTERS                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMCDF    DC    AL1(HMCDFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMCDF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCDFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCDF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL15' '                                                          
         DC    CL64' Repwriter Column Filters/Formats - Date Filters '          
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''. '           
         DC    C' Multiple Selections Allowed.   '                              
*                                                                               
HMCDFL1  DC    AL1(HMCDFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Date Filters'          FULL NAME                               
HMCDFX1  DS    0C                                                               
HMCDFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER COLUMN FILTERS/FORMATS - OTHERS                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMCTH    DC    AL1(HMCTHX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMCOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCTHXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(3)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCOT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL18' '                                                          
         DC    CL61' Repwriter Column Filters/Formats - Others '                
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''. '           
         DC    C' Multiple Selections Allowed.   '                              
*                                                                               
HMCTHL1  DC    AL1(HMCTHX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Others'                FULL NAME                               
HMCTHX1  DS    0C                                                               
HMCTHX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKYW    DC    AL1(HMKYWX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKYW)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKYWXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNUKY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20' Repwriter Keywords ',CL30' '                       
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''. '           
         DC    C'                                '                              
*                                                                               
HMKYWL1  DC    AL1(HMKYWX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Keywords'              FULL NAME                               
HMKYWX1  DS    0C                                                               
HMKYWX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - REG/OFF/TEAM/SALP                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKRG    DC    AL1(HMKRGX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKRG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKRGXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNREG)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL21' ',CL37' Repwriter Keywords - Reg/OFF/Div/Sls'              
         DC    CL21' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKRGL1  DC    AL1(HMKRGX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(15)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(15)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Reg/OFF/Div/Sls'       FULL NAME                               
HMKRGX1  DS    0C                                                               
HMKRGX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - AGY/ADV/PRD                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKAG    DC    AL1(HMKAGX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKAG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKAGXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNAGY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL33' Repwriter Keywords - Agy/Adv/Prd',CL23' '          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKAGL1  DC    AL1(HMKAGX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(11)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(11)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Agy/Adv/Prd'           FULL NAME                               
HMKAGX1  DS    0C                                                               
HMKAGX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - CONTRACT                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKCN    DC    AL1(HMKCNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKCN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKCNXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCON)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30' Repwriter Keywords - Contract'                     
         DC    CL25' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKCNL1  DC    AL1(HMKCNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(08)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Contract'              FULL NAME                               
HMKCNX1  DS    0C                                                               
HMKCNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - CONTRACT DOLLARS                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKC$    DC    AL1(HMKC$X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKC$)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKC$XTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCN$)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL20' ',CL38' Repwriter Keywords - Contract Dollars'             
         DC    CL21' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKC$L1  DC    AL1(HMKC$X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(08)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Con Dollars'           FULL NAME                               
HMKC$X1  DS    0C                                                               
HMKC$X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - PENDING/FORECAST SCREEN                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKPN    DC    AL1(HMKPNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKPN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKPNXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNPND)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL17' '                                                          
         DC    CL44'Repwriter Keywords - Pending/Forecast Screen'               
         DC    CL18' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKPNL1  DC    AL1(HMKPNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(08)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Pending'               FULL NAME                               
HMKPNX1  DS    0C                                                               
HMKPNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - BUDGET                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMNBD    DC    AL1(HMNBDX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKBD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMNBDXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNBUD)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL21' ',CL36'  Repwriter Keywords - Budget Data  '               
         DC    CL22' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMNBDL1  DC    AL1(HMNBDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(06)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Budget'                FULL NAME                               
HMNBDX1  DS    0C                                                               
HMNBDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - CONTRACT COUNTS                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKCC    DC    AL1(HMKCCX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKCC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKCCXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNCCT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL20' ',CL38' Repwriter Keywords - Contract Counts '             
         DC    CL21' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKCCL1  DC    AL1(HMKCCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(08)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Con Counts'            FULL NAME                               
HMKCCX1  DS    0C                                                               
HMKCCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - STATION                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKST    DC    AL1(HMKSTX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKSTXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNSTA)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32' Repwriter Keywords - Station   ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKSTL1  DC    AL1(HMKSTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Station'               FULL NAME                               
HMKSTX1  DS    0C                                                               
HMKSTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - OTHER                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKOT    DC    AL1(HMKOTX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKOTXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNOTH)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32' Repwriter Keywords - Other     ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKOTL1  DC    AL1(HMKOTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(5)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Other'                 FULL NAME                               
HMKOTX1  DS    0C                                                               
HMKOTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - NRGON KEYWORDS                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKNR    DC    AL1(HMKNRX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMKNR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKNRXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNNRG)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34'Repwriter Keywords - NRRG Dollars '                 
         DC    CL23' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKNRL1  DC    AL1(HMKNRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(04)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(16)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NRRG'                  FULL NAME                               
HMKNRX1  DS    0C                                                               
HMKNRX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STATION OPTIONS                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMSOP    DC    AL1(HMSOPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMSOP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMSOPXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQSTOPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNSOP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32' Station Options',CL24' '                           
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMSOPL1  DC    AL1(HMSOPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Station'               FULL NAME                               
HMSOPX1  DS    0C                                                               
HMSOPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STATION TYPES                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMSTP    DC    AL1(HMSTPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMSTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMSTPXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQSTTPS)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(0)              FILTER VALUE FOR SEARCH                      
******   DC    AL1(RKVMNSTP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32' Station Types',CL24' '                             
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMSTPL1  DC    AL1(HMSTPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Type'                  FULL NAME                               
HMSTPX1  DS    0C                                                               
HMSTPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STATION AFFILIATIONS                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMSAF    DC    AL1(HMSAFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMSAF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMSAFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQSTNAF)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVMNSAF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL30' ',CL19'Station Affliations',CL30' '                        
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMSAFL1  DC    AL1(HMSAFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Aff'                   FULL NAME                               
HMSAFX1  DS    0C                                                               
HMSAFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STATION BUDGET TYPES                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBST    DC    AL1(HMBSTX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBSTXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBUDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVBTPST)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Station Budget Types',CL30' '                       
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBSTL1  DC    AL1(HMBSTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BST '                SHORT NAME-USE START OF FULL            
         DC    C'Stabud'                FULL NAME                               
HMBSTX1  DS    0C                                                               
HMBSTX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        COMPANY BUDGET TYPES                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBCO    DC    AL1(HMBCOX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBCO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBCOXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBUDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVBTPCO)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Company Budget Types',CL30' '                       
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBCOL1  DC    AL1(HMBCOX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BCO '                SHORT NAME-USE START OF FULL            
         DC    C'Combud'                FULL NAME                               
HMBCOX1  DS    0C                                                               
HMBCOX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        OFFICE BUDGET TYPES                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBOF    DC    AL1(HMBOFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBOF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBOFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBUDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVBTPOF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Office Budget Types ',CL30' '                       
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBOFL1  DC    AL1(HMBOFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BOF '                SHORT NAME-USE START OF FULL            
         DC    C'Offbud'                FULL NAME                               
HMBOFX1  DS    0C                                                               
HMBOFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SALESPERSON BUDGET TYPES                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBSP    DC    AL1(HMBSPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBSPXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBUDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVBTPSP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL27' ',CL24'Salesperson Budget Types',CL28' '                   
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBSPL1  DC    AL1(HMBSPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BSP '                SHORT NAME-USE START OF FULL            
         DC    C'Salbud'                FULL NAME                               
HMBSPX1  DS    0C                                                               
HMBSPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SALESPERSON/STATION BUDGET TYPES                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBSS    DC    AL1(HMBSSX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBSS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBSSXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBUDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(RKVBTPSS)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Salesperson/Station Budget Types',CL24' '           
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBSSL1  DC    AL1(HMBSSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BSS '                SHORT NAME-USE START OF FULL            
         DC    C'Sastbud'               FULL NAME                               
HMBSSX1  DS    0C                                                               
HMBSSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CROSS-FILE GROUPINGS                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMXFL    DC    AL1(HMXFLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMXFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMXFLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ+HTBCSLAQ) MULTIPLE SELECTS IN SAME FIELD            
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQXFLRP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(0)              FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'  Accessable Reps   ',CL30' '                       
         DC    C'To Select, Enter ''S''. Multiple selections allowed. '         
         DC    C'                            '                                  
*                                                                               
HMXFLL1  DC    AL1(HMXFLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'XFiles'                FULL NAME                               
HMXFLX1  DS    0C                                                               
HMXFLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BACKBILLING OPTIONS                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMBKB    DC    AL1(HMBKBX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(RKQHMBKB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMBKBXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(RKQBKBOP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(0)              FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Back Billing Options',CL30' '                       
         DC    C'To Select, Enter ''S''.                              '         
         DC    C'                            '                                  
*                                                                               
HMBKBL1  DC    AL1(HMBKBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Backbill'              FULL NAME                               
HMBKBX1  DS    0C                                                               
HMBKBX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 06 - STATION FILTER OPTIONS'             
***********************************************************************         
*                                                                     *         
* (06)   STATION FILTERS                                              *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN REGLOBEQUS  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
STOPTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRA BYTES OF DATA PER ENTRY             
*                                                                               
*              STATION AFFILIATE CODE                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STOAFF   DC    AL1(STOAFFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSOAFF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STOAFFL1 DC    AL1(STOAFFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'A   '                SHORT NAME-USE START OF FULL            
         DC    C'Aff=+'                 FULL NAME                               
STOAFFX1 DS    0C                                                               
STOAFFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION MARKET CODE                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STOMKT   DC    AL1(STOMKTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSOMKT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STOMKTL1 DC    AL1(STOMKTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'M   '                SHORT NAME-USE START OF FULL            
         DC    C'Mkt='                  FULL NAME                               
STOMKTX1 DS    0C                                                               
STOMKTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              OWNER CODE                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STOOWN   DC    AL1(STOOWNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSOOWN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STOOWNL1 DC    AL1(STOOWNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'O   '                SHORT NAME-USE START OF FULL            
         DC    C'Own='                  FULL NAME                               
STOOWNX1 DS    0C                                                               
STOOWNX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION RANK CODE                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STORNK   DC    AL1(STORNKX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSORNK)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STORNKL1 DC    AL1(STORNKX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(5)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'R   '                SHORT NAME-USE START OF FULL            
         DC    C'RANK='                 FULL NAME                               
STORNKX1 DS    0C                                                               
STORNKX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION TVB REGION CODE                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STOTVB   DC    AL1(STOTVBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSOTVB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STOTVBL1 DC    AL1(STOTVBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'V   '                SHORT NAME-USE START OF FULL            
         DC    C'TVB='                  FULL NAME                               
STOTVBX1 DS    0C                                                               
STOTVBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION TYPE                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STOTYP   DC    AL1(STOTYPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSOTYP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STOTYPL1 DC    AL1(STOTYPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'T   '                SHORT NAME-USE START OF FULL            
         DC    C'TYPE=+'                FULL NAME                               
STOTYPX1 DS    0C                                                               
STOTYPX  DS    0C                       END OF TABLE ENTRY                      
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 07 - STATION TYPES'                      
***********************************************************************         
*                                                                     *         
* (07)   STATION TYPES                                                *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN REGLOBEQUS  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
STTYTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRA BYTES OF DATA PER ENTRY             
*                                                                               
*              JOINED                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STYJN    DC    AL1(STYJNX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSTTJ)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STYJNL1 DC     AL1(STYJNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Joined'                FULL NAME                               
STYJNX1 DS     0C                                                               
STYJNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*              LEFT                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STYLFT   DC    AL1(STYLFTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSTTL)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STYLFTL1 DC    AL1(STYLFTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Left'                   FULL NAME                              
STYLFTX1 DS    0C                                                               
STYLFTX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              NEW                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STYNEW   DC    AL1(STYNEWX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSTTN)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STYNEWL1 DC    AL1(STYNEWX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'New'                   FULL NAME                               
STYNEWX1 DS    0C                                                               
STYNEWX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              OLD                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STYOLD   DC    AL1(STYOLDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSTTO)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STYOLDL1 DC    AL1(STYOLDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(5)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Old'                   FULL NAME                               
STYOLDX1 DS    0C                                                               
STYOLDX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*              COMPARABLE                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
STYCMP   DC    AL1(STYCMPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSTTC)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
STYCMPL1 DC    AL1(STYCMPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Comparable'            FULL NAME                               
STYCMPX1 DS    0C                                                               
STYCMPX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 08 - AGECNY FILTER OPTIONS'              
***********************************************************************         
*                                                                     *         
* (08)   AGENCY  FILTERS                                              *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN REGLOBEQUS  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
AGOPTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRA BYTES OF DATA PER ENTRY             
*                                                                               
*              TERRITORY                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
AGOTER   DC    AL1(AGOTERX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQAGOTR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
AGOTERL1 DC    AL1(AGOTERX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(5)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TERR='                 FULL NAME                               
AGOTERX1 DS    0C                                                               
AGOTERX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 09 - STATION AFFILIATIONS'               
***********************************************************************         
*                                                                     *         
* (09)   STATION AFFILIATIONS                                         *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN REGLOBEQUS  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
STAFTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRA BYTES OF DATA PER ENTRY             
*                                                                               
*              ABC                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAABC    DC    AL1(SAABCX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAABC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAABCL1 DC     AL1(SAABCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ABC'                   FULL NAME                               
SAABCX1 DS     0C                                                               
SAABCX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*              NBC                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SANBC    DC    AL1(SANBCX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSANBC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SANBCL1 DC     AL1(SANBCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NBC'                   FULL NAME                               
SANBCX1 DS     0C                                                               
SANBCX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              CBS                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SACBS    DC    AL1(SACBSX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSACBS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SACBSL1 DC     AL1(SACBSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CBS'                   FULL NAME                               
SACBSX1 DS     0C                                                               
SACBSX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              INDEPENDENT                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAIND    DC    AL1(SAINDX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAIND)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAINDL1 DC     AL1(SAINDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'IND'                   FULL NAME                               
SAINDX1 DS     0C                                                               
SAINDX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              UNI                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAUNI    DC    AL1(SAUNIX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAUNI)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAUNIL1 DC     AL1(SAUNIX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'UNI'                   FULL NAME                               
SAUNIX1 DS     0C                                                               
SAUNIX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              TEL                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SATEL    DC    AL1(SATELX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSATEL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SATELL1 DC     AL1(SATELX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TEL'                   FULL NAME                               
SATELX1 DS     0C                                                               
SATELX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              FOX                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAFOX    DC    AL1(SAFOXX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAFOX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAFOXL1 DC     AL1(SAFOXX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'FOX'                   FULL NAME                               
SAFOXX1 DS     0C                                                               
SAFOXX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              GAL                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAGAL    DC    AL1(SAGALX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAGAL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAGALL1 DC     AL1(SAGALX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'GAL'                   FULL NAME                               
SAGALX1 DS     0C                                                               
SAGALX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              UPN                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAUPN    DC    AL1(SAUPNX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAUPN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAUPNL1 DC     AL1(SAUPNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'UPN'                   FULL NAME                               
SAUPNX1 DS     0C                                                               
SAUPNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              WBT                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAWBT    DC    AL1(SAWBTX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAWBT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAWBTL1 DC     AL1(SAWBTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'WBT'                   FULL NAME                               
SAWBTX1 DS     0C                                                               
SAWBTX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              PBS                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAPBS    DC    AL1(SAPBSX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAPBS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAPBSL1 DC     AL1(SAPBSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PBS'                   FULL NAME                               
SAPBSX1 DS     0C                                                               
SAPBSX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              CBL                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SACBL    DC    AL1(SACBLX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSACBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SACBLL1 DC     AL1(SACBLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CBL'                   FULL NAME                               
SACBLX1 DS     0C                                                               
SACBLX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              PAX                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAPAX    DC    AL1(SAPAXX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAPAX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAPAXL1 DC     AL1(SAPAXX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PAX'                   FULL NAME                               
SAPAXX1 DS     0C                                                               
SAPAXX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              GEM                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAGEM    DC    AL1(SAGEMX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAGEM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAGEML1 DC     AL1(SAGEMX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'GEM'                   FULL NAME                               
SAGEMX1 DS     0C                                                               
SAGEMX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              AZA                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAAZA    DC    AL1(SAAZAX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAAZA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAAZAL1 DC     AL1(SAAZAX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'AZA'                   FULL NAME                               
SAAZAX1 DS     0C                                                               
SAAZAX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              BNE - BOUNCE NETWORK                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SABNE    DC    AL1(SABNEX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSABNE)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SABNEL1 DC     AL1(SABNEX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BNE'                   FULL NAME                               
SABNEX1 DS     0C                                                               
SABNEX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              CW                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SACW     DC    AL1(SACWX-*)             ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSACW)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SACWL1   DC    AL1(SACWX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CW '                   FULL NAME                               
SACWX1  DS     0C                                                               
SACWX  DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              ETV - ESTRELLA                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAETV    DC    AL1(SAETVX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAETV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAETVL1  DC    AL1(SAETVX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ETV'                   FULL NAME                               
SAETVX1 DS     0C                                                               
SAETVX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              ION                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAION    DC    AL1(SAIONX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAION)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAIONL1  DC    AL1(SAIONX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ION'                   FULL NAME                               
SAIONX1 DS     0C                                                               
SAIONX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              IS - LIBERMAN                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAIS     DC    AL1(SAISX-*)             ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAIS)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAISL1   DC    AL1(SAISX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'IS '                   FULL NAME                               
SAISX1  DS     0C                                                               
SAISX  DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              ISP                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAISP    DC    AL1(SAISPX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAISP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAISPL1  DC    AL1(SAISPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ISP'                   FULL NAME                               
SAISPX1 DS     0C                                                               
SAISPX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              LAT - LATIN ALTERNATE TELEVISION                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SALAT    DC    AL1(SALATX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSALAT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SALATL1  DC    AL1(SALATX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'LAT'                   FULL NAME                               
SALATX1 DS     0C                                                               
SALATX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              MET - MEMORABLE ENTERTAINMENT TV                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAMET    DC    AL1(SAMETX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAMET)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAMETL1  DC    AL1(SAMETX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MET'                   FULL NAME                               
SAMETX1 DS     0C                                                               
SAMETX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              MNT                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAMNT    DC    AL1(SAMNTX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAMNT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAMNTL1  DC    AL1(SAMNTX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MNT'                   FULL NAME                               
SAMNTX1 DS     0C                                                               
SAMNTX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              MUN                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAMUN    DC    AL1(SAMUNX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAMUN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAMUNL1  DC    AL1(SAMUNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MUN'                   FULL NAME                               
SAMUNX1 DS     0C                                                               
SAMUNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              MY                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAMY     DC    AL1(SAMYX-*)             ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAMY)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAMYL1   DC    AL1(SAMYX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MY '                   FULL NAME                               
SAMYX1  DS     0C                                                               
SAMYX  DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              NBW                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SANBW    DC    AL1(SANBWX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSANBW)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SANBWL1  DC    AL1(SANBWX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NBW'                   FULL NAME                               
SANBWX1 DS     0C                                                               
SANBWX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              RTN                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SARTN    DC    AL1(SARTNX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSARTN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SARTNL1  DC    AL1(SARTNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'RTN'                   FULL NAME                               
SARTNX1 DS     0C                                                               
SARTNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              TAZ                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SATAZ    DC    AL1(SATAZX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSATAZ)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SATAZL1  DC    AL1(SATAZX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TAZ'                   FULL NAME                               
SATAZX1 DS     0C                                                               
SATAZX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              TF - TELEFUTURA                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SATF     DC    AL1(SATFX-*)             ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSATF)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SATFL1   DC    AL1(SATFX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TF '                   FULL NAME                               
SATFX1  DS     0C                                                               
SATFX  DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              THI - THIS TV NETWORK                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SATHI    DC    AL1(SATHIX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSATHI)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SATHIL1  DC    AL1(SATHIX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'THI'                   FULL NAME                               
SATHIX1 DS     0C                                                               
SATHIX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              TVA                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SATVA    DC    AL1(SATVAX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSATVA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SATVAL1  DC    AL1(SATVAX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TVA'                   FULL NAME                               
SATVAX1 DS     0C                                                               
SATVAX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*              WB - SPECIAL FOR ETV                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SAWB     DC    AL1(SAWBX-*)             ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQSAWB)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SAWBL1   DC    AL1(SAWBX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'WB '                   FULL NAME                               
SAWBX1  DS     0C                                                               
SAWBX  DS      0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 10 - BUDGET TYPES'                       
***********************************************************************         
*                                                                     *         
* (10)   BUDGET TYPES                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BDTPTAB  DS    0F                                                               
*                                                                               
         DC    AL1(1)              ONE EXTRA BYTE OF DATA PER ENTRY             
*                                                                               
*              STATION                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSTA   DC    AL1(BTPSTAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSTST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPST)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSTAXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'80'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSTAL1 DC    AL1(BTPSTAX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Station'               FULL NAME                               
BTPSTAX1 DS    0C                                                               
BTPSTAX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              OFFICE                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSOF   DC    AL1(BTPSOFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSTOF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPST)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSOFXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'40'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSOFL1 DC    AL1(BTPSOFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Office'               FULL NAME                                
BTPSOFX1 DS    0C                                                               
BTPSOFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION/OFFICE/CONTRACT TYPE                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSCT   DC    AL1(BTPSCTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSTCT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPST)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSCTXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'60'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSCTL1 DC    AL1(BTPSCTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Off/Cty'               FULL NAME                               
BTPSCTX1 DS    0C                                                               
BTPSCTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              COMPANY - COMPANY                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPCMP   DC    AL1(BTPCMPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBCOCO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPCO)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPCMPXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'80'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPCMPL1 DC    AL1(BTPCMPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Company'              FULL NAME                                
BTPCMPX1 DS    0C                                                               
BTPCMPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              COMPANY - OFFICE                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPCOF   DC    AL1(BTPCOFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBCOOF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPCO)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPCOFXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'40'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPCOFL1 DC    AL1(BTPCOFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Office'               FULL NAME                                
BTPCOFX1 DS    0C                                                               
BTPCOFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              COMPANY/OFFICE/CONTRACT TYPE                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPCCT   DC    AL1(BTPCCTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBCOCT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPCO)            STATION BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPCCTXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'60'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPCCTL1 DC    AL1(BTPCCTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Off/Cty'               FULL NAME                               
BTPCCTX1 DS    0C                                                               
BTPCCTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              OFFICE                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPOOF   DC    AL1(BTPOOFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBOFOF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPOF)            OFFICE  BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPOOFXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'80'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPOOFL1 DC    AL1(BTPOOFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE OFFRT OF FULL            
         DC    C'Office'                FULL NAME                               
BTPOOFX1 DS    0C                                                               
BTPOOFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              TEAM                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPOTM   DC    AL1(BTPOTMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBOFTM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPOF)            OFFICE  BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPOTMXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'40'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPOTML1 DC    AL1(BTPOTMX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Team'                 FULL NAME                                
BTPOTMX1 DS    0C                                                               
BTPOTMX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SUBGROUP                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSB    DC    AL1(BTPSBX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBOFSB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPOF)            OFFICE  BUDGET TYPE                     
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSBXT DS     0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'20'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSBL1 DC     AL1(BTPSBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Subgroup'              FULL NAME                               
BTPSBX1 DS     0C                                                               
BTPSBX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SALESPERSON                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSSP   DC    AL1(BTPSSPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSPSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPSP)            SALESPERSON  BUDGET TYPE                
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSSPXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'80'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSSPL1 DC    AL1(BTPSSPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(08)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Sal '                SHORT NAME-USE START OF FULL            
         DC    C'Slsprson'              FULL NAME                               
BTPSSPX1 DS    0C                                                               
BTPSSPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SALESPERSON/DEVELPOMENTAL CONTRACT TYPE                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSDC   DC    AL1(BTPSDCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSPDC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPSP)            SALESPERSON  BUDGET TYPE                
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSDCXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'40'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSDCL1 DC    AL1(BTPSDCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Dct '                SHORT NAME-USE START OF FULL            
         DC    C'Sal/Dct'               FULL NAME                               
BTPSDCX1 DS    0C                                                               
BTPSDCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION/SALESPERSON                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSSS   DC    AL1(BTPSSSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSSSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPSS)            SALESPERSON  BUDGET TYPE                
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSSSXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'80'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSSSL1 DC    AL1(BTPSSSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'SSal'                SHORT NAME-USE START OF FULL            
         DC    C'Sta/sal'               FULL NAME                               
BTPSSSX1 DS    0C                                                               
BTPSSSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              STATION/SALESPERSON/DEVELPOMENTAL CONTRACT TYPE                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BTPSSD   DC    AL1(BTPSSDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBSSDC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMBTPSS)            SALESPERSON  BUDGET TYPE                
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BTPSSDXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    XL1'40'                                                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
BTPSSDL1 DC    AL1(BTPSSDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(07)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(08)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'SDct'                SHORT NAME-USE START OF FULL            
         DC    C'Sta/Dct'                   FULL NAME                           
BTPSSDX1 DS    0C                                                               
BTPSSDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 11 - CROSS FILES GROUPS'                 
***********************************************************************         
*                                                                     *         
* (11)   CROSS FILES GROUPS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
XFILTAB  DS    0F                                                               
*                                                                               
         DC    AL1(2)              TWO EXTRA BYTES OF DATA PER ENTRY            
*                                                                               
*              BLAIR                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLBLR   DC    AL1(XFLBLRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG1)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLBLRXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG1)       GROUP CODE                                   
         DC    XL1'08'             SPARE                                        
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLBLRL1 DC    AL1(XFLBLRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'BL  '                SHORT NAME-USE BLRRT OF FULL            
         DC    C'Blair'                 FULL NAME                               
XFLBLRX1 DS    0C                                                               
XFLBLRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              FOX                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLFOX   DC    AL1(XFLFOXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLFN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG1)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLFOXXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG1)       GROUP CODE                                   
         DC    XL1'A8'             SPARE                                        
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLFOXL1 DC    AL1(XFLFOXX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'FN  '                SHORT NAME-USE START OF FULL            
         DC    C'FOX'                  FULL NAME                                
XFLFOXX1 DS    0C                                                               
XFLFOXX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PETRY                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLPET   DC    AL1(XFLPETX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLPV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG1)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLPETXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG1)       GROUP CODE                                   
         DC    XL1'A8'             SE NUMBER                                    
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLPETL1 DC    AL1(XFLPETX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'PV  '                SHORT NAME-USE START OF FULL            
         DC    C'Petry'                 FULL NAME                               
XFLPETX1 DS    0C                                                               
XFLPETX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              KATZ-TV                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLKZT   DC    AL1(XFLKZTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLMR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG2)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLKZTXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG2)       GROUP CODE                                   
         DC    XL1'78'             SE NUMBER                                    
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLKZTL1 DC    AL1(XFLKZTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'MR  '                SHORT NAME-USE START OF FULL            
         DC    C'Katz-TV'               FULL NAME                               
XFLKZTX1 DS    0C                                                               
XFLKZTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SELTEL                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLSTL   DC    AL1(XFLSTLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLSZ)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG2)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLSTLXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG2)       GROUP CODE                                   
         DC    XL1'98'             SE NUMBER                                    
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLSTLL1 DC    AL1(XFLSTLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'SZ  '                SHORT NAME-USE START OF FULL            
         DC    C'Seltel'                FULL NAME                               
XFLSTLX1 DS    0C                                                               
XFLSTLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              KATZ-RADIO                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
XFLKZR   DC    AL1(XFLKZRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQXFLKU)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(RKMXFLG2)            VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
XFLKZRXT DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(RKVXFLG2)       GROUP CODE                                   
         DC    XL1'58'             SE NUMBER                                    
*                    LANGUAGE DATA IN TABLE ENTRY                               
XFLKZRL1 DC    AL1(XFLKZRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(10)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'KU  '                SHORT NAME-USE START OF FULL            
         DC    C'Katz-Radio'            FULL NAME                               
XFLKZRX1 DS    0C                                                               
XFLKZRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         TITLE 'REVAL1 - TABLE PHASE - 12 - BACKBILLING OPTIONS'                
***********************************************************************         
*                                                                     *         
* (12)   BACKBILLING OPTIONS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BKOPTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)              ZERO EXTRA BYTES OF DATA PER ENTRY           
*                                                                               
*              INCLUDE                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BKBINC   DC    AL1(BKBINCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBKBIN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*                    LANGUAGE DATA IN TABLE ENTRY                               
BKBINCL1 DC    AL1(BKBINCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE INCRT OF FULL            
         DC    C'Include'               FULL NAME                               
BKBINCX1 DS    0C                                                               
BKBINCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              EXCLUDE                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BKBEXC   DC    AL1(BKBEXCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBKBEX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*                    LANGUAGE DATA IN TABLE ENTRY                               
BKBEXCL1 DC    AL1(BKBEXCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'X   '                SHORT NAME-USE EXCRT OF FULL            
         DC    C'EXclude'               FULL NAME                               
BKBEXCX1 DS    0C                                                               
BKBEXCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ONLY                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BKBONL   DC    AL1(BKBONLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(RKQBKBON)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   VALID FOR GROUP 1                       
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*                    LANGUAGE DATA IN TABLE ENTRY                               
BKBONLL1 DC    AL1(BKBONLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE ONLRT OF FULL            
         DC    C'Only'                  FULL NAME                               
BKBONLX1 DS    0C                                                               
BKBONLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
REVALX   DS    0C                  PAD OUT TO CONSTANT SIZE                     
         DS    (18*1024-24-(REVALX-REVAL))X'00'                                 
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* VALWRKD - WORKING STYRAGE DSECT                                     *         
***********************************************************************         
         SPACE 1                                                                
VALWRKD  DSECT                                                                  
Y        EQU   X'FFFF'                                                          
DUB      DS    D                                                                
FULL     DS    F                                                                
RELO     DS    F                                                                
CALLRD   DS    F                                                                
TEMP     DS    CL56                                                             
WORK     DS    CL56                                                             
DTWRK    DS    CL16                                                             
SPACES   DS    CL56                                                             
SVCALL   DS    CL24                PARAMETER LIST SAVEAREA                      
APARMS   DS    A                   A(CALLER'S PARAMETERS)                       
ATYP     DS    A                   A(TYPTAB ENTRY)                              
ALASTGLB DS    A                   A(VLGLBTAB ENTRY) - FOR HOOK MODE            
       ++INCLUDE PRVALPARMS                                                     
CALLTYPE DS    X                   VLPTYPE CALL TYPE BITS                       
EXTRALH  DS    H                   LENGTH OF EXTRA FOR VLTEXTRA                 
DUPICODE DS    H                   INTERNAL CODE FOR DUPLICATE CHECKS           
SAVICODE DS    H                   INTERNAL CODE FOR 2NDARY FILTERS             
ERRNUM   DS    H                   ERROR NUMBER                                 
INVALID  EQU   203+X'FF00'   INVALID INPUT FIELD                                
TOOLONG  EQU   202+X'FF00'   DATA TOO LONG                                      
NODATA   EQU   200+X'FF00'   DATA NOT ENTERED                                   
NOTNUMBR EQU   204+X'FF00'   DATA NOT NUMERIC                                   
VALUE    DS    CL40                                                             
VALUE2   DS    CL40                                                             
CLCOK    DS    C                   Y=VALIDATE/TRANSLATE COMPARE OK              
PTAB     DS    XL256               PARSNIP TABLE                                
PTAB1    DS    XL256                                                            
ASYSTAB  DS    A                   A(SYSTEM TABLE ENTRY)                        
VALWRKX  DS    0F                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DSECT TO COVER SYSTEMS TABLE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSTABD  DSECT                                                                  
*                                                                               
SYSENTRY DS    0X                  TABLE ENTRY START                            
SYSSYS   DS    AL1                 SYSTEM ID (SEE REVALPARMS)                   
         DS    XL3'00'             SPARE                                        
SYSTYPEA DS    A                   A(TYPE TABLE TABLE)                          
SYSMASKA DS    A                   A(SYSTEM MASKS)                              
SYSERRSA DS    A                   A(SYSTEM ERROR EQUATES)                      
SYSENTL  EQU   *-SYSENTRY          TABLE ENTRY LENGTH                           
*                                                                               
*        PRINT SYSTEM TABLE EQUATES                                             
*                                                                               
PRTAB1Q  EQU   X'4E'               PRINT SYSTEM TABLE 1                         
PRTAB2Q  EQU   X'43'               PRINT SYSTEM TABLE 2                         
*                                                                               
*        REP   SYSTEM TABLE EQUATES                                             
*                                                                               
RETAB1Q  EQU   X'A6'               REP   SYSTEM TABLE 1                         
RETAB2Q  EQU   X'A7'               REP   SYSTEM TABLE 2                         
*                                                                               
*        MEDIABASE SYSTEM TABLE EQUATES                                         
*                                                                               
MBTAB1Q  EQU   X'91'               MEDIABASE SYSTEM TABLE 1                     
MBTAB2Q  EQU   X'92'               MEDIABASE SYSTEM TABLE 2                     
MBTAB3Q  EQU   X'9C'               MEDIABASE SYSTEM TABLE 3                     
MBTAB4Q  EQU   X'9D'               MEDIABASE SYSTEM TABLE 4                     
*                                                                               
***********************************************************************         
* MASKD - DSECT TO COVER MASKTYPS                                     *         
***********************************************************************         
         SPACE 1                                                                
MASKD    DSECT                                                                  
MASKVDSP DS    XL2                 DISPLACEMEENT INTO W/S OF VALUE              
MASKMDSP DS    XL2                 DISPLACEMEENT INTO W/S OF MASK               
MASKMLEN DS    XL1                 LENGTH OF MASK                               
MASKVALS DS    XL16                VALUES FOR MASKS X'8000'-X'0001'             
MASKLEN  EQU   *-MASKD             ENTRY SIZE                                   
MASKEOT  EQU   X'FF'               EOT                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DSECT TO ERROR TRANSLATION TABLE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ERRTABD  DSECT                                                                  
ERTENTRY DS    0X                  ERROR TRANSLATE TABLE ENTRY                  
ERTVALER DS    AL2                 REVAL ERROR CODE                             
ERTSYSER DS    AL2                 SYSTEM ERROR CODE                            
ERTENTL  EQU   *-ERTENTRY          LENGTH OF TABLE ENTRY                        
*                                                                               
*        REVAL ERROR CODES                                                      
*                                                                               
VLENOTV  EQU   1                   NOT VALID                                    
VLENOTAV EQU   2                   FEATURE NOT AVAILABLE                        
VLELONG  EQU   3                   ENTRY TOO LONG                               
VLENONE  EQU   4                   NO DATA                                      
VLENOT#  EQU   5                   NOT NUMERIC                                  
VLEFLT1  EQU   11                  NOT VALID FOR FILTER 1                       
VLEFLT2  EQU   12                  NOT VALID FOR FILTER 2                       
VLEFLT3  EQU   13                  NOT VALID FOR FILTER 3                       
VLEFLT4  EQU   14                  NOT VALID FOR FILTER 4                       
VLEFLT5  EQU   15                  NOT VALID FOR FILTER 5                       
VLEFLT6  EQU   16                  NOT VALID FOR FILTER 6                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TYTYPD - DSECT TO COVER TYPTAB                                      *         
***********************************************************************         
         SPACE 1                                                                
TYTYPD   DSECT                                                                  
TYTICODE DS    XL2                 TYPE ICODE - NULLS IF NO TYPE                
TYTEOT   EQU   X'FFFF'             END OF TABLE                                 
TYTHELP  DS    XL2                 HELP PANEL NUMBER OR NULL                    
TYTPARNT DS    XL2                 PARENT ICODE IF ANY                          
TYTIND1  DS    XL1                 1ST TABLE/ROUTINE INDICATOR                  
TYT1ADRQ EQU   X'80'               TABLE/ROUTINE ADDRESS IN TYT1ADDR            
TYT1SYSQ EQU   X'40'               SYSFACS ROUTINE (DISP IN TYT1SYSD)           
TYT1TABQ EQU   X'20'               LOOK-UP TABLE (NOT ROUTINE)                  
*                                  TYTIND1=TYT1ADRQ                             
TYT1ADDR DS    0AL3                TAB/ROUTINE ADDR                             
*                                  TYTIND1=TYT1SYSQ                             
TYT1SYSD DS    0XL1                SYSFACS ROUTINE DISPLACEMENT IN LIST         
*                                  TYTIND1 NOT TYT1ADRQ OR TYT1SYSQ             
TYT1PHAS DS    XL1                 T00A PHASE NUMBER CONTAINING TABLE           
TYT1TNUM DS    XL1                 TABLE NUMBER WITHIN PHASE                    
*                                  NTH 2BYTE DISP FROM START OF PHASE           
         DS    XL1                 NULL                                         
TYTIND2  DS    XL1                 2ND INDICATOR (ROUTINE OR NULL)              
*                                  CAN ONLY APPLY IF MBMBNUM NON-ZERO           
TYT2ADRQ EQU   X'80'               ROUTINE ADDRESS IN TYT2ADDR                  
TYT2SYSQ EQU   X'40'               SYSFACS ROUTINE (DISP IN TYT2SYSD)           
*                                  TYTIND2=TYT2SYSQ                             
TYT2SYSD DS    0XL1                SYSFACS ROUTINE DISPLACEMENT IN LIST         
*                                  TYTIND2=TYT2ADRQ                             
TYT2ADDR DS    AL3                 ROUTINE ADDR                                 
TYTYPL   EQU   *-TYTYPD            ENTRY LENGTH                                 
         EJECT                                                                  
       ++INCLUDE PRGLOBTABD                                                     
         EJECT                                                                  
       ++INCLUDE PRVALTABD                                                      
         EJECT                                                                  
*REWRIEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE RENWREQUS                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*DDLANGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*DDMBFACS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMBFACS                                                       
         PRINT ON                                                               
*DDMINBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029REVAL     03/12/12'                                      
         END                                                                    
