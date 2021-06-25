*          DATA SET PRVALS     AT LEVEL 092 AS OF 05/01/02                      
*PHASE T00A40B,*                                                                
*              'DDVAL - GENERIC VALIDATOR'                                      
         TITLE 'DDVAL - GENERIC VALIDATOR'                                      
DDVAL    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 VALWRKX-VALWRKD,**DDVAL*,RA,RR=RE,CLEAR=YES                      
         USING VALWRKD,RC                                                       
         ST    RE,RELO                                                          
         L     RE,4(RD)            SAVE BACKWARD POINTER FOR HOOK CALLS         
M001     L     RE,4(RE)                                                         
         C     RB,16(RE)           GO BACK UNTIL RF NEQ A(DDVAL)                
         BH    M001B               (TO CATER FOR RECURSIVE DDVAL CALLS)         
         LR    RF,RB               LOOK FOR ANY CALL FROM WITHIN                
         AH    RF,=H'8192'         2 BASE REGS OF DDVAL                         
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
M045     LR    R3,RF               COME HERE IF TABLE IS IN DDVAL               
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
         BE    M056                FOUND ONE                                    
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
         OC    TYTICODE,TYTICODE   RETURN '*?*' = UNKNOWN                       
         BZ    M069A                                                            
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
***      OC    MBMBNUM,MBMBNUM                                                  
***      BZ    M090                NO MEDIABASE MEMBER TO FILTER FOR            
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
M105     L     RE,VLPATAB          HELP NUMBER RETURNED                         
         MVC   0(L'TYTHELP,RE),TYTHELP                                          
         OC    TYTHELP,TYTHELP                                                  
         BZ    M120                NO HELP NUMBER                               
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
M120     L     R1,APARMS           EXITS TO CALLER                              
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
* ON ENTRY R1 = APARMS = A(DDVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS           *         
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
         MVC   0(2,RE),=AL2(FVDATNVL)                                           
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DAYPROC - PROCESS DAY EXPRESSION (EG MON, MTW, M-F)                 *         
*                                                                     *         
* ON ENTRY R1 = APARMS = A(DDVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS           *         
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
         MVC   0(2,RE),=AL2(FVDAYNVL)                                           
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FLTPROC - PROCESS FILTER EXPRESSION                                 *         
*                                                                     *         
* ON ENTRY R1 = APPARM = A(DDVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS           *         
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
* ON ENTRY R1 = APPARM = A(DDVAL CALLER'S VLPARMS)                    *         
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
* ON ENTRY R1 = APPARM = A(DDVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS           *         
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
* ON ENTRY R1 = APARMS = A(DDVAL CALLER'S VLPARMS)                    *         
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS           *         
*                                                                     *         
* NOTE- **NON-STANDARD CODE LENGTH** - 2 (SINGLE DATE), 4 (RANGE)     *         
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
         MVC   0(2,RE),=AL2(FVDATNVL)                                           
         L     R1,APARMS                                                        
         OI    VLPERR-VLPARMS(R1),VLPINVQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TXTPROC - PROCESS TEXT QUALIFIER                                              
*                                                                               
* ON ENTRY R1 = APARMS = A(DDVAL CALLER'S VLPARMS)                              
* ON EXIT  VLPERR/VLPAERR ARE SET IN DDVAL CALLER'S VLPARMS                     
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
* SECONDARY PASS CHECK ROUTINE (V MBMNUM)                             *         
***********************************************************************         
         SPACE 1                                                                
MBNUMCHK BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MISCELLANEOUS TABLES AND EQUATES                                    *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
ALONEQ   EQU   X'FEFE'                                                          
*                                                                               
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
         DC    AL1(VLSYSPRQ)       PRINT SYSTEM                                 
         DC    XL3'00'             SPARE                                        
         DC    A(PRTYPTAB)         A(PARENTLESS ITEMS TABLE)                    
         DC    A(PRMASKS)          A(SYSTEM MASKS)                              
         DC    A(PRERRTAB)         A(SYSTEM MASK TYPES)                         
*                                                                               
         DC    AL1(VLSYSMBQ)       MEDIABASE SYSTEM                             
         DC    XL3'00'             SPARE                                        
         DC    A(MBTYPTAB)         A(PARENTLESS ITEMS TABLE)                    
         DC    A(MBMASKS)          A(SYSTEM MASKS)                              
         DC    A(MBERRTAB)         A(ERROR TRANSLATE TABLE)                     
*                                                                               
         DC    AL1(0)              END OF TABLE                                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT                                                        *         
*                                                                     *         
*        TABLE FOR VALUE-MASK CONVERSION - SEE MASKD                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRMASKS  DS    0C                                                               
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
         DC    AL2(VLFLT6V-VALWRKD,VLFLT6M-VALWRKD) MASK 5                      
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
PRERRTAB DS    0H                                                               
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
*        PRINT                                                        *         
*                                                                     *         
* TYPTAB - GLOBAL TABLE OF TYPES - SEE TYTYPD                         *         
* PARENTLESS SET AND STANDALONES FIRST, THEN IN ALPHA NAME SEQUENCE   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRTYPTAB DS    0F                                                               
*                                  MASTER = PARENTLESS SET                      
         DC    AL2(0,0,0)                                                       
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(PRMSTRTB)                             
         DC    AL1(0),AL3(0)                                                    
*                                  INCLUDE/EXCLUDE BUYS                         
         DC    AL2(PRQBUYS,0,0)                                                 
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(BUYSTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                  CIRCULATION FREQUENCIES                      
         DC    AL2(PRQCRFRQ,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(CFRQTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                  CIRCULATION SOURCES                          
         DC    AL2(PRQCRSRC,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(CSRCTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - BILLED                            
         DC    AL2(PRQCFDBD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - BILL INVOICE                      
         DC    AL2(PRQCFDBI,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - BILLABLE                          
         DC    AL2(PRQCFDBL,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - BILL RUN                          
         DC    AL2(PRQCFDBR,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - SPACE CLOSING                     
         DC    AL2(PRQCFDCL,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - INVOICE DUE                       
         DC    AL2(PRQCFDDU,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - INVOICE EDI                       
         DC    AL2(PRQCFDED,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - INSERT                            
         DC    AL2(PRQCFDID,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - INSERTION ORDER                   
         DC    AL2(PRQCFDIO,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - MATERIAL CLOSING                  
         DC    AL2(PRQCFDMC,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - MONTH OF SERVICE                  
         DC    AL2(PRQCFDMS,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - ON STAND                          
         DC    AL2(PRQCFDOS,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - PAID/CLEARED                      
         DC    AL2(PRQCFDPD,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - PAYABLE                           
         DC    AL2(PRQCFDPY,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     DATES - UNPAID                            
         DC    AL2(PRQCFDUP,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(PERPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     ESTIMATE FILTER                           
         DC    AL2(PRQCFEF,0,0)                                                 
         DC    AL1(TYT1ADRQ),AL3(FLTPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     COMMENT LINE - FREECOM                    
         DC    AL2(PRQCMFRE,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(NUMPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     EPIC DETAIL FILTERS                       
         DC    AL2(PRQPZFDT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(PZFDTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     EPIC     FILTERS                          
         DC    AL2(PRQPZFTR,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(PZFLTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     ESTIMATE STATUS - LOCKED                  
         DC    AL2(PRQESTLK,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(ESLKTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     ESTIMATE STATUS - TEST                    
         DC    AL2(PRQESTTS,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(ESTSTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENUS                                
         DC    AL2(PRQHLPMN,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - CIRC FREQUENCIES              
         DC    AL2(PRQHMCFR,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - CIRC SOURCES                  
         DC    AL2(PRQHMCSR,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - EPIC FILTERS                  
         DC    AL2(PRQHMPZF,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - SRDS DATA TYPES               
         DC    AL2(PRQHMSRD,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - SRDS PUB TYPES                
         DC    AL2(PRQHMSTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER COL FILT/FMTS          
         DC    AL2(PRQHMCFW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER DATE TYPES             
         DC    AL2(PRQHMDTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER OPTIONS                
         DC    AL2(PRQHMOPW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     HELP MENU - WRITER KEYWORDS               
         DC    AL2(PRQHMKYW,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(HLPMTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     MASTER TYPE INTERNAL CODE                 
         DC    AL2(PRQMTIC,0,0)                                                 
         DC    AL1(TYT1ADRQ),AL3(MTICPROC)                                      
         DC    AL1(0),AL3(0)                                                    
*                                     SPACE DESCRIPTION                         
         DC    AL2(PRQCFSP,0,0)                                                 
         DC    AL1(TYT1ADRQ),AL3(TXTPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT KEYWORDS                              
         DC    AL2(PRQWRKYW,0,0)                                                
         DC    AL1(TYT1TABQ),AL1(PRTAB2Q,01,00)                                 
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT OPTIONS                               
         DC    AL2(PRQWROPT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(WOPTTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT COLUMN FILTERS/FORMATS                
         DC    AL2(PRQWRCFL,0,0)                                                
         DC    AL1(TYT1TABQ),AL1(PRTAB2Q,02,00)                                 
         DC    AL1(0),AL3(0)                                                    
*                                  REPORT DATE TYPES                            
         DC    AL2(PRQWRDTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(WDTPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     SRDS DATA TYPES                           
         DC    AL2(PRQSRDAT,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(SRDATAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     SRDS PUB TYPES                            
         DC    AL2(PRQSRDTP,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(SRTPTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     TEARSHEET OPTIONS                         
         DC    AL2(PRQTSHOK,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(TSOKTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     TEARSHEET REPRO QUALITY                   
         DC    AL2(PRQCFTSR,0,0)         - COLUMN FILTER                        
         DC    AL1(TYT1ADRQ),AL3(NUMPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     TEARSHEET REPRO QUALITY                   
         DC    AL2(PRQTSHRP,0,0)                                                
         DC    AL1(TYT1ADRQ),AL3(NUMPROC)                                       
         DC    AL1(0),AL3(0)                                                    
*                                     TEARSHEET STATUS                          
         DC    AL2(PRQTSHST,0,0)                                                
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(TSSTTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                     TEARSHEET STATUS                          
         DC    AL2(PRQCFTSS,0,0)         -COLUMN FILTER                         
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(TSSTTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                  USER FIELD IDS                               
         DC    AL2(PRQUDEF,0,0)                                                 
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(UDEFTAB)                              
         DC    AL1(0),AL3(0)                                                    
*                                                                               
         DC    AL2(TYTEOT)                                                      
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT                                                        *         
*                                                                     *         
* PRMSTRTB - GLOBAL TABLE OF MASTER TYPES (PARENTLESS) -SEE VLGLOBTABD*         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRMSTRTB DS    0F                                                               
*                                                                               
         DC    AL1(0)              NO EXTRAS                                    
*                                  COLUMN FILTERS/FORMATS                       
MSCFL    DC    AL1(MSCFLX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(PRQWRCFL,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSCFLL1  DC    AL1(MSCFLX1-*,LANGEUK,3,6),C'    Filter'                         
MSCFLX1  DS    0C                                                               
MSCFLX   DS    0C                                                               
*                                                                               
MSHLP    DC    AL1(MSHLPX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(PRQHLPMN,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSHLPL1  DC    AL1(MSHLPX1-*,LANGEUK,4,4),C'    Menu'                           
MSHLPX1  DS    0C                                                               
MSHLPX   DS    0C                                                               
*                                                                               
MSOPT    DC    AL1(MSOPTX-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                         
         DC    AL2(PRQWROPT,0)                                                  
         DC    AL2(Y,Y,Y,Y),AL1(Y,Y)                                            
MSOPTL1  DC    AL1(MSOPTX1-*,LANGEUK,3,6),C'    Option'                         
MSOPTX1  DS    0C                                                               
MSOPTX   DS    0C                                                               
*                                                                               
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        MEDIABASE                                                    *         
*                                                                     *         
*        TABLE FOR VALUE-MASK CONVERSION - SEE MASKD                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MBMASKS  DS    0C                                                               
*                                                                               
         DC    AL2(VLFLT1V-VALWRKD,VLFLT1M-VALWRKD) DATABASE                    
         DC    AL1(L'VLFLT1M,MBQDBAS1,MBQDBAS2,MBQDBAS3),13X'00'                
*                                                                               
         DC    AL2(VLFLT2V-VALWRKD,VLFLT2M-VALWRKD) MEDIA TYPE                  
         DC    AL1(L'VLFLT2M,MBQMTNAT,MBQMTLOC,MBQMTMAG,MBQMTDIR)               
         DC    AL1(MBQMTTV,MBQMTRAD,MBQMTMOV,MBQMTOUT,MBQMTEPH)                 
         DC    AL1(MBQMTDIM,MBQMTOTX,MBQMTOTY,MBQMTTRD),3X'00'                  
*                                                                               
         DC    AL2(VLFLT3V-VALWRKD,VLFLT3M-VALWRKD) RECORD TYPE                 
         DC    AL1(L'VLFLT3M,MBQMRSUP,MBQMROWN,MBQMRREP,MBQMRPER)               
         DC    AL1(MBQMREDG,MBQMRLST,MBQMRENQ,MBQMRRPT),8X'00'                  
*                                                                               
         DC    AL2(VLFLT4V-VALWRKD,VLFLT4M-VALWRKD) FUNCTION TYPE               
         DC    AL1(L'VLFLT4M),X'0102030405060708090A0B0C0D0E0F10'               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        MEDIABASE ERROR CODE TRANSLATION TABLE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MBERRTAB DS    0H                                                               
*                                                                               
         DC    AL2(VLENOTV,INVALID)         NOT VALID                           
         DC    AL2(VLENOTAV,FVFNOTAV)       FEATURE NOT AVAILABLE               
         DC    AL2(VLELONG,TOOLONG)         ENTRY TOO LONG                      
         DC    AL2(VLENONE,NODATA)          NO DATA                             
         DC    AL2(VLENOT#,NOTNUMBR)        NOT NUMERIC                         
         DC    AL2(VLEFLT1,INVALID)         NOT VALID FOR DATABASE              
         DC    AL2(VLEFLT2,MSGMTYP)         NOT VALID FOR MEDIA    TYPE         
         DC    AL2(VLEFLT3,MSGRTYP)         NOT VALID FOR RECORD   TYPE         
         DC    AL2(VLEFLT4,MSGFTYP)         NOT VALID FOR FUNCTION TYPE         
         DC    AL2(VLEFLT5,MSGSTYP)         NOT VALID FOR SUB-REC  TYPE         
         DC    AL2(VLEFLT6,MSGCTYP)         NOT VALID FOR CARD     TYPE         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        MEDIABASE                                                    *         
*                                                                     *         
* TYPTAB - GLOBAL TABLE OF TYPES - SEE TYTYPD                         *         
* PARENTLESS SET AND STANDALONES FIRST, THEN IN ALPHA NAME SEQUENCE   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MBTYPTAB DS    0F                                                               
*                                  MASTER = PARENTLESS SET                      
         DC    AL2(0,0,0)                                                       
         DC    AL1(TYT1ADRQ+TYT1TABQ),AL3(MBMSTRTB)                             
         DC    AL1(0),AL3(0)                                                    
*                                  ACTIONS                                      
         DC    AL2(MBQACTN,0,0)                                                 
         DC    AL1(TYT1TABQ,QMBVAL1,2,0)                                        
         DC    AL1(0),AL3(0)                                                    
*                                  YES/NO                                       
         DC    AL2(MBQYESNO,0,0)                                                
         DC    AL1(TYT1TABQ,QMBVAL1,11,0)                                       
         DC    AL1(0),AL3(0)                                                    
*                                                                               
         DC    AL2(TYTEOT)                                                      
         EJECT                                                                  
***********************************************************************         
* MASTRTAB - GLOBAL TABLE OF MASTER TYPES (PARENTLESS) -SEE VLGLOBTABD*         
***********************************************************************         
         SPACE 1                                                                
MBMSTRTB DS    0F                                                               
         DC    AL1(0)              NO EXTRAS                                    
MST1     DC    AL1(MST1X-*,VLGCHLDQ+VLGCBYTQ+VLGBYTEQ)                          
         DC    AL2(MBQACTN,0)                                                   
         DC    AL2(MBMDBASA,Y,Y,Y),AL1(Y,Y)                                     
MST1L1   DC    AL1(MST1L2-*,LANGEUK,3,6),C'    Action'                          
MST1L2   DS    0C                                                               
MST1X    DS    0C                                                               
         DC    AL1(0)                                                           
*                                                                               
*CATALP PRVAL1                                                                  
         TITLE 'PRVAL1 - TABLE PHASE'                                           
PRVAL1   DS    0D                                                               
         DC    AL2(WRITTAB-PRVAL1)  01     WRITER REPORTABLE FIELDS             
         DC    AL2(0)               02                                          
         DC    AL2(0)               03                                          
         DC    AL2(WOPTTAB-PRVAL1)  04     WRITER OPTIONS                       
         DC    AL2(0)               05     SPARE                                
         DC    AL2(WDTPTAB-PRVAL1)  06     WRITER DATE TYPES                    
         DC    AL2(UDEFTAB-PRVAL1)  07     USER DEFINED FIELD IDS               
         DC    AL2(ESLKTAB-PRVAL1)  08     ESTIMATE STATUS - LOCKED             
         DC    AL2(ESTSTAB-PRVAL1)  09     ESTIMATE STATUS - TEST               
         DC    AL2(BUYSTAB-PRVAL1)  10     INCLUDE/EXCLUDE BUYS                 
         DC    AL2(HLPMTAB-PRVAL1)  11     HELP MENUS                           
         DC    AL2(TSOKTAB-PRVAL1)  12     TEARSHEET OPTIONS                    
         DC    AL2(TSSTTAB-PRVAL1)  13     TEARSHEET STATUS                     
         DC    AL2(PZFLTAB-PRVAL1)  14     EPIC FILTERS                         
         DC    AL2(0)               15                                          
         TITLE 'PRVAL1 - TABLE PHASE - 01 - WRITER REPORTABLE FIELDS'           
***********************************************************************         
*                                                                     *         
* (01)   WRITER REPORTABLE FIELDS                                     *         
*        INTERNAL CODES COME FROM PARENTLESS TYPE LIST IN PRGLOBEQUS  *         
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
         TITLE 'PRVAL1 - TABLE PHASE - 04 - WRITER OPTIONS'                     
***********************************************************************         
*        WRITER OPTIONS (04)                                                    
***********************************************************************         
         SPACE 1                                                                
WOPTTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        EXCLUDED CLIENT                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXCL   DC    AL1(WOPXCLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPXCL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXCLL1 DC    AL1(WOPXCLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Xclient='              FULL NAME                               
WOPXCLX1 DS    0C                                                               
WOPXCLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDED CLIENT group                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPXCG   DC    AL1(WOPXCGX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPXCG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPXCGL1 DC    AL1(WOPXCGX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Xcgroup='              FULL NAME                               
WOPXCGX1 DS    0C                                                               
WOPXCGX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        NO *RATE BUYS                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSRN   DC    AL1(WOPSRNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSRN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSRNL1 DC    AL1(WOPSRNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NO*RATE'               FULL NAME                               
WOPSRNX1 DS    0C                                                               
WOPSRNX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        *RATE BUYS ONLY                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSRO   DC    AL1(WOPSROX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSRO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSROL1 DC    AL1(WOPSROX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'*RATEONLY'             FULL NAME                               
WOPSROX1 DS    0C                                                               
WOPSROX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*              ALL DOWNLOAD IS ALPHA                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPALP   DC    AL1(WOPALPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPALP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
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
*        READ ALL CIRCULATION RECORDS (ALLCIRCS)                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCRC   DC    AL1(WOPCRCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCRC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCRCL1 DC    AL1(WOPCRCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLCIRCS'              FULL NAME                               
WOPCRCX1 DS    0C                                                               
WOPCRCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        READ ALL ESTIMATE RECORDS (ALLESTS)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPEST   DC    AL1(WOPESTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPEST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPESTL1 DC    AL1(WOPESTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLESTS'               FULL NAME                               
WOPESTX1 DS    0C                                                               
WOPESTX  DS    0C                       END OF TABLE ENTRY                      
*        REPORT ACROSS ALL AGENCIES FOR AOR (AOR = AG/CLT)                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPAOR   DC    AL1(WOPAORX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPAOR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPAORL1 DC    AL1(WOPAORX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'AOR='                  FULL NAME                               
WOPAORX1 DS    0C                                                               
WOPAORX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ACTIVITY DATE (ACTVDTE = PERIOD EXPRESSION)                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPATD   DC    AL1(WOPATDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPATD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPATDL1 DC    AL1(WOPATDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ACTVDATE='              FULL NAME                              
WOPATDX1 DS    0C                                                               
WOPATDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL DUE DATE (BHDDATE = PERIOD EXPRESSION)                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBDD   DC    AL1(WOPBDDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBDD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBDDL1 DC    AL1(WOPBDDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHDDATE='              FULL NAME                               
WOPBDDX1 DS    0C                                                               
WOPBDDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL DUE DATE (DU = PERIOD EXPRESSION)                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WO1BDD   DC    AL1(WO1BDDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBDD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WO1BDDL1 DC    AL1(WO1BDDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DU='                   FULL NAME                               
WO1BDDX1 DS    0C                                                               
WO1BDDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL EDI DATE (ED = PERIOD EXPRESSION)                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBED   DC    AL1(WOPBEDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBED)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBEDL1 DC    AL1(WOPBEDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHEDATE='              FULL NAME                               
WOPBEDX1 DS    0C                                                               
WOPBEDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL EDI DATE (ED = PERIOD EXPRESSION)                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WO1BED   DC    AL1(WO1BEDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBED)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WO1BEDL1 DC    AL1(WO1BEDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ED='                   FULL NAME                               
WO1BEDX1 DS    0C                                                               
WO1BEDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL INVOICE DATE (BHIDATE = PERIOD EXPRESSION)              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBID   DC    AL1(WOPBIDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBIDL1 DC    AL1(WOPBIDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHIDATE='              FULL NAME                               
WOPBIDX1 DS    0C                                                               
WOPBIDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL INVOICE DATE (BI = PERIOD EXPRESSION)                   
*                                                                               
WO1BID   DC    AL1(WO1BIDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WO1BIDL1 DC    AL1(WO1BIDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BI='                   FULL NAME                               
WO1BIDX1 DS    0C                                                               
WO1BIDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILLED ON DATE (BD = PERIOD EXPRESSION)                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBDT   DC    AL1(WOPBDTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBDT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBDTL1 DC    AL1(WOPBDTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BD='                   FULL NAME                               
WOPBDTX1 DS    0C                                                               
WOPBDTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        INCLUDE BOXES ON REPORT (BOX = Y/N)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBOX   DC    AL1(WOPBOXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBOX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
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
*        FILTER ON BILL POSTING DATE (BHPDATE = PERIOD EXPRESSION)              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBPD   DC    AL1(WOPBPDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBPD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBPDL1 DC    AL1(WOPBPDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHPDATE='              FULL NAME                               
WOPBPDX1 DS    0C                                                               
WOPBPDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL RUN DATE (BHRDATE = PERIOD EXPRESSION)                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBRD   DC    AL1(WOPBRDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBRDL1 DC    AL1(WOPBRDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHRDATE='              FULL NAME                               
WOPBRDX1 DS    0C                                                               
WOPBRDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        FILTER ON BILL RUN DATE (BR = PERIOD EXPRESSION)                       
*                                                                               
WO1BRD   DC    AL1(WO1BRDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WO1BRDL1 DC    AL1(WO1BRDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BR='                   FULL NAME                               
WO1BRDX1 DS    0C                                                               
WO1BRDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SELECT ONLY BILLED INSERTIONS (BILLSEL)                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBSL   DC    AL1(WOPBSLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBSL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBSLL1 DC    AL1(WOPBSLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BILLSEL'               FULL NAME                               
WOPBSLX1 DS    0C                                                               
WOPBSLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT NEWSPAPER BY BOTH LINE AND INCHES (TOTBT)                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBTH   DC    AL1(WOPBTHX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBTH)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBTHL1 DC    AL1(WOPBTHX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                  MINIMUM INPUT  LENGTH                    
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BOTH'                  FULL NAME                               
WOPBTHX1 DS    0C                                                               
WOPBTHX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SPECIFY BILL TYPE (BHTYPE = )                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBTP   DC    AL1(WOPBTPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBTPL1 DC    AL1(WOPBTPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BHTYPE='               FULL NAME                               
WOPBTPX1 DS    0C                                                               
WOPBTPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY BUYS WITH CD (CD)                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCDO   DC    AL1(WOPCDOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCDO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCDOL1 DC    AL1(WOPCDOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CDONLY'                FULL NAME                               
WOPCDOX1 DS    0C                                                               
WOPCDOX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY CONTRACTS WHOSE END DATE FALLS IN DATE RANGE               
*               (CONTEND)                                                       
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCEN   DC    AL1(WOPCENX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCEN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNCON)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCENL1 DC    AL1(WOPCENX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONTEND='              FULL NAME                               
WOPCENX1 DS    0C                                                               
WOPCENX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY BUY ACTIVITY OCCURRING ON OR AFTER THIS DATE               
*              (CHGDATE)                                                        
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCDT   DC    AL1(WOPCDTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCDT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCDTL1 DC    AL1(WOPCDTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CHGDATE='              FULL NAME                               
WOPCDTX1 DS    0C                                                               
WOPCDTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY BUYS THAT WERE CHANGED IN PERIOD                           
*              (CHGONLY)                                                        
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCOS   DC    AL1(WOPCOSX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCOS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCOSL1 DC    AL1(WOPCOSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CHGONLY='              FULL NAME                               
WOPCOSX1 DS    0C                                                               
WOPCOSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ON ALL BUYS FOR CONTRACT (CONTRULE)                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCRL   DC    AL1(WOPCRLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCRL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNCON)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCRLL1 DC    AL1(WOPCRLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONTRULE'              FULL NAME                               
WOPCRLX1 DS    0C                                                               
WOPCRLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        REPORT ON ALL BUYS FOR CONTRACT (CONTRACT)                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCRT   DC    AL1(WOPCRTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCRT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNCON)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCRTL1 DC    AL1(WOPCRTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONTRACT'              FULL NAME                               
WOPCRTX1 DS    0C                                                               
WOPCRTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SKIP BUYS NOT COUNTING TOWARDS CONTRACT (CONSPACE)                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCSP   DC    AL1(WOPCSPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCSP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNCON)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCSPL1 DC    AL1(WOPCSPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONSPACE'              FULL NAME                               
WOPCSPX1 DS    0C                                                               
WOPCSPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY DATA IN CONTRACT RECORDS (CONTONLY)                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCTO   DC    AL1(WOPCTOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCTO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNCON)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPCTOL1 DC    AL1(WOPCTOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CONTONLY'              FULL NAME                               
WOPCTOX1 DS    0C                                                               
WOPCTOX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        DON'T WRAP ANY FIELD TO NEXT LINE (CUT)                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCUT   DC    AL1(WOPCUTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCUT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         SPACE 2                                                                
*                                                                               
*        INCLUDE DELETED BUYS IN REPORT (DELETE)                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDEL   DC    AL1(WOPDELX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDEL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDELL1 DC    AL1(WOPDELX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DELETE'                FULL NAME                               
WOPDELX1 DS    0C                                                               
WOPDELX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        REPORT ONLY DELETED BUYS (DELONLY)                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDLO   DC    AL1(WOPDLOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDLO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDLOL1 DC    AL1(WOPDLOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DELONLY'               FULL NAME                               
WOPDLOX1 DS    0C                                                               
WOPDLOX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        REPORT USING DRD SCHEME OF ANOTHER CLIENT (DRDCLI)                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDRD   DC    AL1(WOPDRDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPDRDL1 DC    AL1(WOPDRDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'DRDCLI='               FULL NAME                               
WOPDRDX1 DS    0C                                                               
WOPDRDX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        REPORT ALL DETAILS (ALLDET)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTA   DC    AL1(WOPDTAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDTA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPDTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
         DC    AL2(PRQOPDHD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
*        DOWNLOAD REPORT (DOWN)                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDWN   DC    AL1(WOPDWNX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDWN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPDTL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
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
*        DOWNLOAD TOTALS WITH TOTAL lINE TEXT (DOWNTOTT)                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPDTT   DC    AL1(WOPDTTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPDTT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
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
WOPFLT   DC    AL1(WOPFLTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPFLT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPFLTL1 DC    AL1(WOPFLTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'FLOAT='                FULL NAME                               
WOPFLTX1 DS    0C                                                               
WOPFLTX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        ADD FOOT COMMENT TO REPORT (FOOTCOM)                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPFTC   DC    AL1(WOPFTCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPFTC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPFTCL1 DC    AL1(WOPFTCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'FOOTCOM='              FULL NAME                               
WOPFTCX1 DS    0C                                                               
WOPFTCX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        RELEASED BUYS ONLY                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRLO   DC    AL1(WOPRLOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPRLO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRLOL1 DC    AL1(WOPRLOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NOHELD'                FULL NAME                               
WOPRLOX1 DS    0C                                                               
WOPRLOX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRINT GRAND TOTALS                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPGND   DC    AL1(WOPGNDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPGND)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPGNDL1 DC    AL1(WOPGNDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'GRAND'                 FULL NAME                               
WOPGNDX1 DS    0C                                                               
WOPGNDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        HELD BUYS ONLY                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPHON   DC    AL1(WOPHONX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPHON)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPHONL1 DC    AL1(WOPHONX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'HELDONLY'              FULL NAME                               
WOPHONX1 DS    0C                                                               
WOPHONX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        REPORT NEWSPAPER IN INCHES (INCH)                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPINC   DC    AL1(WOPINCX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPINC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPINCL1 DC    AL1(WOPINCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'INCH'                  FULL NAME                               
WOPINCX1 DS    0C                                                               
WOPINCX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        INVOICE NUMBER FILTER                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPINV   DC    AL1(WOPINVX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPINV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPINVL1 DC    AL1(WOPINVX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'INV='                  FULL NAME                               
WOPINVX1 DS    0C                                                               
WOPINVX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        PRODUCE PRINT QUEUE INDICIES (PQIX)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPINX   DC    AL1(WOPINXX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPINX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   NOT DOCUMENTED                          
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPINXL1 DC    AL1(WOPINXX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PQIX'                  FULL NAME                               
WOPINXX1 DS    0C                                                               
WOPINXX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        LEFT ALIGN REPORT (LEFT)                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPLEF   DC    AL1(WOPLEFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPLEF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         SPACE 2                                                                
*                                                                               
*        REPORT NEWSPAPER IN LINES (LINE)                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPLNE   DC    AL1(WOPLNEX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPLNE)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPLNEL1 DC    AL1(WOPLNEX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'LINE'                  FULL NAME                               
WOPLNEX1 DS    0C                                                               
WOPLNEX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        LIST OF MEDIA FOR REPORT                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPMED   DC    AL1(WOPMEDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPMED)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPMEDL1 DC    AL1(WOPMEDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MEDIA='                FULL NAME                               
WOPMEDX1 DS    0C                                                               
WOPMEDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DATES IN MM/YY/DD FORMAT (MM/DD/YY)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPMDY   DC    AL1(WOPMDYX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPMDY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPMDYL1 DC    AL1(WOPMDYX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MM/DD/YY'              FULL NAME                               
WOPMDYX1 DS    0C                                                               
WOPMDYX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        SUPPRESS TOTAL LINE IF ONLY 1 DETAIL (MINTOT)                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPMNT   DC    AL1(WOPMNTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPMNT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPNAR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*        REPORT ONLY BUYS WITHOUT CD (NCDONLY)                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNCD   DC    AL1(WOPNCDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPNCD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNCDL1 DC    AL1(WOPNCDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NCDONLY'               FULL NAME                               
WOPNCDX1 DS    0C                                                               
WOPNCDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DROP HEADINGS FROM REPORT (NOHEAD)                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNHD   DC    AL1(WOPNHDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPNHD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*        IGNORE ZERO BUYS (NOZERO)                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNOZ   DC    AL1(WOPNOZX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPNOZ)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPNOZL1 DC    AL1(WOPNOZX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'NOZERO'                FULL NAME                               
WOPNOZX1 DS    0C                                                               
WOPNOZX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DON'T PRINT REQUEST DETAILS PAGE                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPNRD   DC    AL1(WOPNRDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPNRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT+PRMMNDWN)   MENU     TYPE MASK                      
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
         DC    AL2(PRQOPNTR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*        REPORT ZERO NUMBERS AS BLANKS (PBLANK)                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPBL   DC    AL1(WOPPBLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
*        CUT OFF DATE FOR PG REPORT (PGDATE=)                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPGD   DC    AL1(WOPPGDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPGD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDFL)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPGDL1 DC    AL1(WOPPGDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PGDATE='               FULL NAME                               
WOPPGDX1 DS    0C                                                               
WOPPGDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ONLY PAID INSERTIONS (PAYSEL)                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPSL   DC    AL1(WOPPSLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPSL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPSLL1 DC    AL1(WOPPSLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PAYSEL'                FULL NAME                               
WOPPSLX1 DS    0C                                                               
WOPPSLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        READ ALL PUB RECORDS (ALLPUBS)                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPUB   DC    AL1(WOPPUBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPUB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPUBL1 DC    AL1(WOPPUBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLPUBS'               FULL NAME                               
WOPPUBX1 DS    0C                                                               
WOPPUBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        READ ALL REP RECORDS (ALLREPS)                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPREP   DC    AL1(WOPREPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPREP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPREPL1 DC    AL1(WOPREPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLREPS'               FULL NAME                               
WOPREPX1 DS    0C                                                               
WOPREPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        RFPDATE - SPECIAL NUMERIC DATE FORMULAE                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRFP   DC    AL1(WOPRFPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPRFP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNDWN)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   DOCUMENTED                              
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPRFPL1 DC    AL1(WOPRFPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'RFPDATE'               FULL NAME                               
WOPRFPX1 DS    0C                                                               
WOPRFPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        ROUND TOTALS (ROUND)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPRND   DC    AL1(WOPRNDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPRND)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPRP2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
*                                                                               
*        STANDARD COMMENT 1 (STCOM1)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSC1   DC    AL1(WOPSC1X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSC1)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
         DC    AL2(PRQOPSC2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
         DC    AL2(PRQOPSPC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPSOL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*        SPLIT POOL PASSIVE POINTERS (SPLIT)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSPL   DC    AL1(WOPSPLX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSPL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSPLL1 DC    AL1(WOPSPLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'SPLIT'                 FULL NAME                               
WOPSPLX1 DS    0C                                                               
WOPSPLX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        STRIP REPORT (STRIP)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSTR   DC    AL1(WOPSTRX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSTR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSTRL1 DC    AL1(WOPSTRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'STRIP'                 FULL NAME                               
WOPSTRX1 DS    0C                                                               
WOPSTRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT LIVE AND TEST TOTALS SEPARATELY (TOTLT)                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTLT   DC    AL1(WOPTLTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTLT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTLTL1 DC    AL1(WOPTLTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TOTLT'                 FULL NAME                               
WOPTLTX1 DS    0C                                                               
WOPTLTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REPORT ALL TOTALS (ALLTOT)                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTOA   DC    AL1(WOPTOAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTOA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTOAL1 DC    AL1(WOPTOAX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ALLTOT'                FULL NAME                               
WOPTOAX1 DS    0C                                                               
WOPTOAX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        TOTAL LIVE AND TEST BUYS TOGETHER (TOTBT)                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTOB   DC    AL1(WOPTOBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTOB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTOBL1 DC    AL1(WOPTOBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TOTBT'                 FULL NAME                               
WOPTOBX1 DS    0C                                                               
WOPTOBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        ONLY TOP NNN OF RANKING TO PRINT (TOP)                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTOP   DC    AL1(WOPTOPX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTOP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTOPL1 DC    AL1(WOPTOPX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TOP'                   FULL NAME                               
WOPTOPX1 DS    0C                                                               
WOPTOPX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITE REPORT TO TAPE (TAPE)                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTPE   DC    AL1(WOPTPEX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTPE)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(PKMDOCN)             NOT DOCUMENTED                          
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTPEL1 DC    AL1(WOPTPEX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TAPE'                  FULL NAME                               
WOPTPEX1 DS    0C                                                               
WOPTPEX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SET TRACE ON (TRACE)                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTRA   DC    AL1(WOPTRAX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTRA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y-1)                 FLAVOR   TYPE MASK NOT ON MENU          
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(PKMDOCDD)            DDS ONLY                                
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
*        TEST BUYS ONLY (TSTONLY)                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTSO   DC    AL1(WOPTSOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTSO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTSOL1 DC    AL1(WOPTSOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TSTONLY'               FULL NAME                               
WOPTSOX1 DS    0C                                                               
WOPTSOX  DS    0C                       END OF TABLE ENTRY                      
         SPACE 2                                                                
*                                                                               
*        INCLUDE TEST BUYS (TEST)                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPTST   DC    AL1(WOPTSTX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPTST)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPTSTL1 DC    AL1(WOPTSTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TEST'                  FULL NAME                               
WOPTSTX1 DS    0C                                                               
WOPTSTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        VARIABLE VALUE SET                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPVAR   DC    AL1(WOPVARX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPVAR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNOTH)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
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
         DC    AL2(PRQOPWID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPXBX)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
         DC    AL2(PRQOPYMD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*        DATES IN DDYYMMDD FORMAT (CCYYMMDD)                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPCYM   DC    AL1(WOPCYMX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPCYM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFMT)            MENU     TYPE MASK                      
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
*              AD CODE FILTER                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPADF   DC    AL1(WOPADFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPADF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPADFL1 DC    AL1(WOPADFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ADFILTER='             FULL NAME                               
WOPADFX1 DS    0C                                                               
WOPADFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              BILLABLE                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBLB   DC    AL1(WOPBLBX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBLB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBLBL1 DC    AL1(WOPBLBX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BILLABLE'              FULL NAME                               
WOPBLBX1 DS    0C                                                               
WOPBLBX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              BILLED                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPBLD   DC    AL1(WOPBLDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPBLD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPBLDL1 DC    AL1(WOPBLDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BILLED'                FULL NAME                               
WOPBLDX1 DS    0C                                                               
WOPBLDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ORDERED                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPORD   DC    AL1(WOPORDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPORD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPORDL1 DC    AL1(WOPORDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ORDERED'               FULL NAME                               
WOPORDX1 DS    0C                                                               
WOPORDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAID BUYS                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPD    DC    AL1(WOPPDX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPD)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPDL1 DC     AL1(WOPPDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PAID'                  FULL NAME                               
WOPPDX1 DS     0C                                                               
WOPPDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAYABLE                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPPAY   DC    AL1(WOPPAYX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPPAY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPPAYL1 DC    AL1(WOPPAYX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PAYABLE'               FULL NAME                               
WOPPAYX1 DS    0C                                                               
WOPPAYX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        REFERENCE NUMBER FILTER                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPREF   DC    AL1(WOPREFX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPREF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPREFL1 DC    AL1(WOPREFX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'REFNO='                FULL NAME                               
WOPREFX1 DS    0C                                                               
WOPREFX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SPACE FILTER 1                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSP1   DC    AL1(WOPSP1X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSP1)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSP1L1 DC    AL1(WOPSP1X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'SP1='                  FULL NAME                               
WOPSP1X1 DS    0C                                                               
WOPSP1X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              SPACE FILTER 2                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPSP2   DC    AL1(WOPSP2X-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPSP2)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPSP2L1 DC    AL1(WOPSP2X1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'SP2='                  FULL NAME                               
WOPSP2X1 DS    0C                                                               
WOPSP2X  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              UNORDERED                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPUNO   DC    AL1(WOPUNOX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPUNO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPUNOL1 DC    AL1(WOPUNOX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(9)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'UNORDERED'             FULL NAME                               
WOPUNOX1 DS    0C                                                               
WOPUNOX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              UPID FILTER                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WOPUID   DC    AL1(WOPUIDX-*)           ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQOPUID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNFLT)            MENU     TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
WOPUIDL1 DC    AL1(WOPUIDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'UPID='                 FULL NAME                               
WOPUIDX1 DS    0C                                                               
WOPUIDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 06 - WRITER DATE TYPES'                  
***********************************************************************         
*        WRITER DATE TYPES (06)                                                 
***********************************************************************         
         SPACE 1                                                                
WDTPTAB  DS    0F                                                               
         DC    AL1(1)                   ONE EXTRA BYTE                          
*                                                                               
*              BILLABLE                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTBL    DC    AL1(WDTBLX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'B'                     INTERNAL CODE                           
*                                                                               
WDTBLL1  DC    AL1(WDTBLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BL'                    FULL NAME                               
WDTBLX1  DS    0C                                                               
WDTBLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              BILLED                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTBD    DC    AL1(WDTBDX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPBD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'L'                     INTERNAL CODE                           
*                                                                               
WDTBDL1  DC    AL1(WDTBDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BD'                    FULL NAME                               
WDTBDX1  DS    0C                                                               
WDTBDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              CLOSING                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTCL    DC    AL1(WDTCLX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPCL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'C'                     INTERNAL CODE                           
*                                                                               
WDTCLL1  DC    AL1(WDTCLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CL'                    FULL NAME                               
WDTCLX1  DS    0C                                                               
WDTCLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              CONTRACT START                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTCP    DC    AL1(WDTCPX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPCP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'X'                     INTERNAL CODE                           
*                                                                               
WDTCPL1  DC    AL1(WDTCPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CP'                    FULL NAME                               
WDTCPX1  DS    0C                                                               
WDTCPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              INSERT                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTID    DC    AL1(WDTIDX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPID)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'I'                     INTERNAL CODE                           
*                                                                               
WDTIDL1  DC    AL1(WDTIDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ID'                    FULL NAME                               
WDTIDX1  DS    0C                                                               
WDTIDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              INSERTION ORDER                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTIO    DC    AL1(WDTIOX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPIO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'O'                     INTERNAL CODE                           
*                                                                               
WDTIOL1  DC    AL1(WDTIOX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'IO'                    FULL NAME                               
WDTIOX1  DS    0C                                                               
WDTIOX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              MATERIAL CLOSING                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTMC    DC    AL1(WDTMCX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPMC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'M'                     INTERNAL CODE                           
*                                                                               
WDTMCL1  DC    AL1(WDTMCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MC'                    FULL NAME                               
WDTMCX1  DS    0C                                                               
WDTMCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              MONTH OF SERVICE                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTMS    DC    AL1(WDTMSX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPMS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'1'                     INTERNAL CODE                           
*                                                                               
WDTMSL1  DC    AL1(WDTMSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'MS'                    FULL NAME                               
WDTMSX1  DS    0C                                                               
WDTMSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              ON STAND                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTOS    DC    AL1(WDTOSX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPOS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'S'                     INTERNAL CODE                           
*                                                                               
WDTOSL1  DC    AL1(WDTOSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'OS'                    FULL NAME                               
WDTOSX1  DS    0C                                                               
WDTOSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAID                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTPD    DC    AL1(WDTPDX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPPD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'A'                     INTERNAL CODE                           
*                                                                               
WDTPDL1  DC    AL1(WDTPDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PD'                    FULL NAME                               
WDTPDX1  DS    0C                                                               
WDTPDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAID AND BILLED                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTBB    DC    AL1(WDTBBX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPBB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'Z'                     INTERNAL CODE                           
*                                                                               
WDTBBL1  DC    AL1(WDTBBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BB'                    FULL NAME                               
WDTBBX1  DS    0C                                                               
WDTBBX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAID AND/OR BILLED                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTEB    DC    AL1(WDTEBX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPEB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'T'                     INTERNAL CODE                           
*                                                                               
WDTEBL1  DC    AL1(WDTEBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'EB'                    FULL NAME                               
WDTEBX1  DS    0C                                                               
WDTEBX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              PAYABLE                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTPY    DC    AL1(WDTPYX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPPY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'P'                     INTERNAL CODE                           
*                                                                               
WDTPYL1  DC    AL1(WDTPYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PY'                    FULL NAME                               
WDTPYX1  DS    0C                                                               
WDTPYX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*              UNPAID                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
WDTUP    DC    AL1(WDTUPX-*)            ENTRY LENGTH                            
         DC    AL1(VLGBYTEQ)            1 BYTE INTERNAL CODE                    
         DC    AL2(PRQDTPUP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
         DC    C'U'                     INTERNAL CODE                           
*                                                                               
WDTUPL1  DC    AL1(WDTUPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'UP'                    FULL NAME                               
WDTUPX1  DS    0C                                                               
WDTUPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 07 - USER FIIELD IDS'                    
***********************************************************************         
*        USER DEFINED FIELDS ID (07)                                            
***********************************************************************         
         SPACE 1                                                                
UDEFTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*              PRODUCT 1                                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
UDFP1    DC    AL1(UDFP1X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQUDP1)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
UDFP1L1  DC    AL1(UDFP1X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'P1'                    FULL NAME                               
UDFP1X1  DS    0C                                                               
UDFP1X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRODUCT 2                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
UDFP2    DC    AL1(UDFP2X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQUDP2)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
UDFP2L1  DC    AL1(UDFP2X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'P2'                    FULL NAME                               
UDFP2X1  DS    0C                                                               
UDFP2X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        ESTIMATE 1                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
UDFE1    DC    AL1(UDFE1X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQUDE1)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
UDFE1L1  DC    AL1(UDFE1X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'E1'                    FULL NAME                               
UDFE1X1  DS    0C                                                               
UDFE1X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        ESTIMATE 2                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
UDFE2    DC    AL1(UDFE2X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQUDE2)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
UDFE2L1  DC    AL1(UDFE2X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'E2'                    FULL NAME                               
UDFE2X1  DS    0C                                                               
UDFE2X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 08 - ESTIMATE STATUS - LOCKED'           
***********************************************************************         
*        ESTIMATE STATUS - LOCKED (08)                                          
***********************************************************************         
         SPACE 1                                                                
ESLKTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*              OPEN                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
ELKOP    DC    AL1(ELKOPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQELKOP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
ELKOPL1  DC    AL1(ELKOPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'OPEN'                  FULL NAME                               
ELKOPX1  DS    0C                                                               
ELKOPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        LOCKED                                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
ELKLK    DC    AL1(ELKLKX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQELKLK)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
ELKLKL1  DC    AL1(ELKLKX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'LOCKED'                FULL NAME                               
ELKLKX1  DS    0C                                                               
ELKLKX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PERMANENTLY LOCKED                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
ELKPL    DC    AL1(ELKPLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQELKPL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
ELKPLL1  DC    AL1(ELKPLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PLOCKED'               FULL NAME                               
ELKPLX1  DS    0C                                                               
ELKPLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 09 - ESTIMATE STATUS - TEST'             
***********************************************************************         
*        ESTIMATE STATUS - LOCKED (09)                                          
***********************************************************************         
         SPACE 1                                                                
ESTSTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*              LIVE                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
ETSLV    DC    AL1(ETSLVX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQETSLV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
ETSLVL1  DC    AL1(ETSLVX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'LIVE'                  FULL NAME                               
ETSLVX1  DS    0C                                                               
ETSLVX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        TEST                                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
ETSTS    DC    AL1(ETSTSX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQETSTS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
ETSTSL1  DC    AL1(ETSTSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'TEST'                  FULL NAME                               
ETSTSX1  DS    0C                                                               
ETSTSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 10 - INCLUDE/EXCLUDE BUYS'               
***********************************************************************         
*        INCLUDE/EXCLUDE BUYS (10)                                              
***********************************************************************         
         SPACE 1                                                                
BUYSTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        #BUYS ONLY                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BYS#Y    DC    AL1(BYS#YX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQBYS#Y)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMCOL)              FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BYS#YL1  DC    AL1(BYS#YX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'#BUYS ONLY'            FULL NAME                               
BYS#YX1  DS    0C                                                               
BYS#YX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EXCLUDE #BUYS                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BYS#N    DC    AL1(BYS#NX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQBYS#N)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMCOL)              FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BYS#NL1  DC    AL1(BYS#NX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'-#  '                SHORT NAME-USE START OF FULL            
         DC    C'MINUS #BUYS'           FULL NAME                               
BYS#NX1  DS    0C                                                               
BYS#NX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        *BUYS ONLY                                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
BYSSY    DC    AL1(BYSSYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQBYSSY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMCOL)              FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
BYSSYL1  DC    AL1(BYSSYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'*BUYS ONLY'            FULL NAME                               
BYSSYX1  DS    0C                                                               
BYSSYX   DS    0C                       END OF TAPLE ENTRY                      
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 11 - HELP MENUS'                         
***********************************************************************         
*        HELP MENUS (11)                                                        
***********************************************************************         
         SPACE 1                                                                
HLPMTAB  DS    0F                                                               
         DC    AL1(182)                 EXTRA BYTES                             
HTBCMNYQ EQU   X'80'               MULTIPLE SELECTS IN SEPARATE FIELDS          
HTBCSAMQ EQU   X'40'               MULTIPLE SELECTS IN SINGLE   FIELDS          
*                                                                               
*        CIRCULATION FREQUENCIES                                                
*                    FIXED DATA IN TABLE ENTRY                                  
HMCFR    DC    AL1(HMCFRX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMCFR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFRXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS IN SAME FIELD            
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQCRFRQ)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCFR)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL28' ',CL23'Circulation Frequencies',CL28' '                    
         DC    C'    To Select, Enter ''S''.                     '              
         DC    C'                                 '                             
*                                                                               
HMCFRL1  DC    AL1(HMCFRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Frequencies'           FULL NAME                               
HMCFRX1  DS    0C                                                               
HMCFRX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CIRCULATION SOURCES                                                    
*                    FIXED DATA IN TABLE ENTRY                                  
HMCSR    DC    AL1(HMCSRX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMCSR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCSRXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS IN SAME FIELD            
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQCRSRC)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCSR)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL30' ',CL19'Circulation Sources',CL30' '                        
         DC    C'    To Select, Enter ''S''.                     '              
         DC    C'                                 '                             
*                                                                               
HMCSRL1  DC    AL1(HMCSRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Sources'               FULL NAME                               
HMCSRX1  DS    0C                                                               
HMCSRX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EPIC DETAIL FILTERS                                                    
*                    FIXED DATA IN TABLE ENTRY                                  
HMPZD    DC    AL1(HMPZDX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMPZD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMPZDXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQPZFDT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNPZD)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL30' ',CL19'EPIC Detail Filters',CL30' '                        
         DC    C'    To Select, Enter ''S''.                     '              
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMPZDL1  DC    AL1(HMPZDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Filters'               FULL NAME                               
HMPZDX1  DS    0C                                                               
HMPZDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        EPIC FILTERS                                                           
*                    FIXED DATA IN TABLE ENTRY                                  
HMPZF    DC    AL1(HMPZFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMPZF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMPZFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQPZFTR)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNPZF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL33' ',CL12'EPIC Filters',CL34' '                               
         DC    C'    To Select, Enter ''S''.                     '              
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMPZFL1  DC    AL1(HMPZFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Filters'               FULL NAME                               
HMPZFX1  DS    0C                                                               
HMPZFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        SRDS DATA TYPES                                                        
*                    FIXED DATA IN TABLE ENTRY                                  
HMSRD    DC    AL1(HMSRDX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMSRD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMSRDXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(0)                                                           
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQSRDAT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNSRD)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL33' ',CL12'  Data Types ',CL34' '                              
         DC    C'To Select, Enter ''S''.                         '              
         DC    C'                                 '                             
*                                                                               
HMSRDL1  DC    AL1(HMSRDX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Data'                  FULL NAME                               
HMSRDX1  DS    0C                                                               
HMSRDX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        SRDS PUB TYPES                                                         
*                    FIXED DATA IN TABLE ENTRY                                  
HMSTP    DC    AL1(HMSTPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMSTP)            INTERNAL CODE                           
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
         DC    AL1(2)              NUMBER OF COLUMNS                            
         DC    AL1(0)                                                           
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQSRDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNSTP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL33' ',CL12'  Pub Types ',CL34' '                               
         DC    C'To Select, Enter ''S''.                         '              
         DC    C'                                 '                             
*                                                                               
HMSTPL1  DC    AL1(HMSTPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Types  '               FULL NAME                               
HMSTPX1  DS    0C                                                               
HMSTPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER DATE TYPES                                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMDTP    DC    AL1(HMDTPX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMDTP)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(0)                   MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMDTPXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRDTP)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(0)              FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL22'Printwriter Date Types',CL28' '                     
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C'                                 '                             
*                                                                               
HMDTPL1  DC    AL1(HMDTPX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Dtyp'                SHORT NAME-USE START OF FULL            
         DC    C'Date Types'            FULL NAME                               
HMDTPX1  DS    0C                                                               
HMDTPX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOPW    DC    AL1(HMOPWX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMOPW)            INTERNAL CODE                           
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
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNUOP)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Printwriter Options',CL30' '                        
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
         DC    AL2(PRQHMOFM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOFMXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNFMT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Options - Formatting',CL24' '           
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
         DC    AL2(PRQHMODN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMODNXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNDWN)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30'Printwriter Options - Download',CL25' '             
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
*        WRITER OPTIONS - CONTRACTS                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOCN    DC    AL1(HMOCNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMOCN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOCNXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCON)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL31'Printwriter Options - Contracts',CL24' '            
         DC    C'    To Select, Enter ''S''. For Help, Enter ''?''.'            
         DC    C' Multiple Selections Allowed.    '                             
*                                                                               
HMOCNL1  DC    AL1(HMOCNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Contracts'             FULL NAME                               
HMOCNX1  DS    0C                                                               
HMOCNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER OPTIONS - FILTERS                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOFL    DC    AL1(HMOFLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMOFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOFLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNFLT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30'Printwriter Options - Filters ',CL25' '             
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
         DC    AL2(PRQHMODF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMODFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNDFL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL34'Printwriter Options - Date Filters',CL21' '         
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
*        WRITER OPTIONS - OTHERS                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMOTH    DC    AL1(HMOTHX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMOOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUOP)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMOTHXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWROPT)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNOTH)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL24' ',CL30'Printwriter Options - Others  ',CL25' '             
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
         DC    AL2(PRQHMCFW)            INTERNAL CODE                           
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
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(0)              NO MULTIPLE SELECTS                          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNUCF)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34'Printwriter Column Filters/Formats',CL23' '         
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
         DC    AL2(PRQHMCFM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFMXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SAME FIELD               
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCFM)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL16' '                                                          
         DC    CL63'Printwriter Column Filters/Formats - Formatting'            
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
         DC    AL2(PRQHMCFL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCFLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCFL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL17' '                                                          
         DC    CL62'Printwriter Column Filters/Formats - Filters'               
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
         DC    AL2(PRQHMCDF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCDFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCDT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL15' '                                                          
         DC    CL64'Printwriter Column Filters/Formats - Date Filters'          
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
         DC    AL2(PRQHMCOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUCF)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMCTHXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCSAMQ)       MULTIPLE SELECTS IN SINGLE FIELD             
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRCFL)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNCOT)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL18' '                                                          
         DC    CL61'Printwriter Column Filters/Formats - Others'                
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
         DC    AL2(PRQHMKYW)            INTERNAL CODE                           
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
         DC    AL2(PRQHLPMN)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PRVMNUKY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL29' ',CL20'Printwriter Keywords',CL30' '                       
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
*        WRITER KEYWORDS - AD                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKAD    DC    AL1(HMKADX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKAD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKADXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNAD)        FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - AD       ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKADL1  DC    AL1(HMKADX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Ads'                   FULL NAME                               
HMKADX1  DS    0C                                                               
HMKADX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - BILLING                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKBL    DC    AL1(HMKBLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKBLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNBLL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Billing  ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKBLL1  DC    AL1(HMKBLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Billing'               FULL NAME                               
HMKBLX1  DS    0C                                                               
HMKBLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - BUY                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKBY    DC    AL1(HMKBYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKBY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKBYXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNBUY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Buy      ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKBYL1  DC    AL1(HMKBYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(3)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Buy'                   FULL NAME                               
HMKBYX1  DS    0C                                                               
HMKBYX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - BUY DOLLARS                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKB$    DC    AL1(HMKB$X-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKB$)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(PKMCOL)              FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKB$XTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNBY$)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL22' ',CL34'Printwriter Keywords - Buy Dollars',CL23' '         
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKB$L1  DC    AL1(HMKB$X1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(4)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Buy$'                  FULL NAME                               
HMKB$X1  DS    0C                                                               
HMKB$X   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - CASHFLOW                                             
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKCF    DC    AL1(HMKCFX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKCF)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKCFXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNCFL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Cashflow ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKCFL1  DC    AL1(HMKCFX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(8)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Cashflow'              FULL NAME                               
HMKCFX1  DS    0C                                                               
HMKCFX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - MED/CLT/PRD/EST                                      
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKCL    DC    AL1(HMKCLX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKCL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKCLXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNMCL)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL38'Printwriter Keywords - Med/Clt/Prd/Est'             
         DC    CL18' '                                                          
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKCLL1  DC    AL1(HMKCLX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(14)                  MINIMUM INPUT  LENGTH                   
         DC    AL1(14)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Ag/Md/Cl/Pr/Es'        FULL NAME                               
HMKCLX1  DS    0C                                                               
HMKCLX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - CONTRACTS                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKCN    DC    AL1(HMKCNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKCN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
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
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNCON)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Contracts',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKCNL1  DC    AL1(HMKCNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(9)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Contracts'             FULL NAME                               
HMKCNX1  DS    0C                                                               
HMKCNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        WRITER KEYWORDS - OTHER                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKOT    DC    AL1(HMKOTX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKOT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
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
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNOTH)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Other    ',CL24' '           
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
*        WRITER KEYWORDS - PUB/DRD                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKPB    DC    AL1(HMKPBX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKPB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKPBXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNPUB)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Pub/DRD  ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKPBL1  DC    AL1(HMKPBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(7)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Pub/DRD'               FULL NAME                               
HMKPBX1  DS    0C                                                               
HMKPBX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        WRITER KEYWORDS - PAYING                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
HMKPY    DC    AL1(HMKPYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQHMKPY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y-PRMAORY)           AOR VALIDITY  MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(PRMMNUKY)            MENU     TYPE MASK                      
         DC    AL2(Y)                   DOCUMENT TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
HMKPYXTR DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
         DC    AL1(7)              START LINE FOR MENU                          
         DC    AL1(3)              WIDTH OF SELECT FIELD                        
         DC    AL1(2)              NUMBER OF TITLES                             
         DC    AL1(79)             LENGTH OF TITLE LINE                         
         DC    AL1(5)              NUMBER OF COLUMNS                            
         DC    AL1(HTBCMNYQ)       MULTIPLE SELECTS IN MULTIPLE FIELDS          
         DS    XL2                 SPARE                                        
*                                                                               
         DC    AL2(PRQWRKYW)       ICODE OF CHILDREN'S TABLE                    
         DC    XL1'00'             CONTROL BYTE                                 
         DC    AL1(PKVMNPAY)       FILTER VALUE FOR SEARCH                      
*                                                                               
         DC    XL2'00'             SPARE                                        
         DC    XL10'00'            SPARE                                        
*                                                                               
*                                  START OF TITLE LINES                         
*                                                                               
         DC    CL23' ',CL32'Printwriter Keywords - Paying   ',CL24' '           
         DC    C'To Select, Enter ''S''. For Help, Enter ''+''. Multip'         
         DC    C'le Selections Allowed.        '                                
*                                                                               
HMKPYL1  DC    AL1(HMKPYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Paying'                FULL NAME                               
HMKPYX1  DS    0C                                                               
HMKPYX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
         TITLE 'PRVAL1 - TABLE PHASE - 12 - TEARSHEET OKAY OPTIONS'             
***********************************************************************         
*                                                                     *         
* (12)   TEARSHEET OKAY OPTIONS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
TSOKTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        TEARSHEET UN-EVALUATED - BLANK                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSOKB    DC    AL1(TSOKBX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSOKB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSOKBL1 DC     AL1(TSOKBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(0)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C' '                     FULL NAME                               
TSOKBX1 DS     0C                                                               
TSOKBX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        TEARSHEET NOT ACCEPTABLE - N                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSOKN    DC    AL1(TSOKNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSOKN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSOKNL1 DC     AL1(TSOKNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'N'                     FULL NAME                               
TSOKNX1 DS     0C                                                               
TSOKNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        TEARSHEET ACCEPTABLE - Y                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSOKY    DC    AL1(TSOKYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSOKY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSOKYL1 DC     AL1(TSOKYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Y'                     FULL NAME                               
TSOKYX1 DS     0C                                                               
TSOKYX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        NON-ENTERED DATA - X = BLANK - FOR FILTERING                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSOKX    DC    AL1(TSOKXX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSOKB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSOKXL1 DC     AL1(TSOKXX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'X'                     FULL NAME                               
TSOKXX1 DS     0C                                                               
TSOKXX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 13 - TEARSHEET STATUS'                   
***********************************************************************         
*                                                                     *         
* (13)   TEARSHEET STATUS                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
TSSTTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        TEARSHEET STATUS - BLANK                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTB    DC    AL1(TSSTBX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTBL1 DC     AL1(TSSTBX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(0)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C' '                     FULL NAME                               
TSSTBX1 DS     0C                                                               
TSSTBX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        TEARSHEET STATUS A                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTA    DC    AL1(TSSTAX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTAL1 DC     AL1(TSSTAX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'A'                     FULL NAME                               
TSSTAX1 DS     0C                                                               
TSSTAX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*        TEARSHEET STATUS I                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTI    DC    AL1(TSSTIX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTI)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTIL1 DC     AL1(TSSTIX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'I'                     FULL NAME                               
TSSTIX1 DS     0C                                                               
TSSTIX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        TEARSHEET STATUS M                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTM    DC    AL1(TSSTMX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTM)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTML1 DC     AL1(TSSTMX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'M'                     FULL NAME                               
TSSTMX1 DS     0C                                                               
TSSTMX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        TEARSHEET STATUS R                                                     
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTR    DC    AL1(TSSTRX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTRL1 DC     AL1(TSSTRX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'R'                     FULL NAME                               
TSSTRX1 DS     0C                                                               
TSSTRX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        TEARSHEET STATUS X - MEANING UNENTERED OR BLANK                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTX    DC    AL1(TSSTXX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTB)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTXL1 DC     AL1(TSSTXX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'X'                     FULL NAME                               
TSSTXX1 DS     0C                                                               
TSSTXX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        TEARSHEET STATUS Y - MEANING YES                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTY    DC    AL1(TSSTYX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTY)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTYL1 DC     AL1(TSSTYX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Y'                     FULL NAME                               
TSSTYX1 DS     0C                                                               
TSSTYX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        TEARSHEET STATUS N - MEANING NO                                        
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
TSSTN    DC    AL1(TSSTNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQTSSTN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
TSSTNL1 DC     AL1(TSSTNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'N'                     FULL NAME                               
TSSTNX1 DS     0C                                                               
TSSTNX DS      0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 14 - EPIC FILTERS'                       
***********************************************************************         
*        EPIC FILTERS (14)                                                      
***********************************************************************         
         SPACE 1                                                                
PZFLTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        ACTIVITY                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFACT   DC    AL1(PZFACTX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFACT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFACTL1 DC    AL1(PZFACTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Activity='             FULL NAME                               
PZFACTX1 DS    0C                                                               
PZFACTX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CLIENT CODE                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFCC    DC    AL1(PZFCCX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFCC)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFCCL1 DC     AL1(PZFCCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Ccode='                FULL NAME                               
PZFCCX1 DS     0C                                                               
PZFCCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        CLIENT NAME                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFCN    DC    AL1(PZFCNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFCN)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFCNL1 DC     AL1(PZFCNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Cname='                FULL NAME                               
PZFCNX1 DS     0C                                                               
PZFCNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRODUCT CODE                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFPC    DC    AL1(PZFPCX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFPC)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFPCL1 DC     AL1(PZFPCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Pcode='                FULL NAME                               
PZFPCX1 DS     0C                                                               
PZFPCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        PRODUCT NAME                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFPN    DC    AL1(PZFPNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFPN)             INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFPNL1 DC     AL1(PZFPNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Pname='                FULL NAME                               
PZFPNX1 DS     0C                                                               
PZFPNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        CONVERTED                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFCNV   DC    AL1(PZFCNVX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFCNV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFCNVL1 DC    AL1(PZFCNVX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(10)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Converted'             FULL NAME                               
PZFCNVX1 DS    0C                                                               
PZFCNVX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        RE-CONVERTED                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFRCV   DC    AL1(PZFRCVX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFRCV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFRCVL1 DC    AL1(PZFRCVX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(13)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Rec='                SHORT NAME-USE START OF FULL            
         DC    C'Re-converted'          FULL NAME                               
PZFRCVX1 DS    0C                                                               
PZFRCVX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        UNCONVERTED                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFUNC   DC    AL1(PZFUNCX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFUNC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFUNCL1 DC    AL1(PZFUNCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Unconverted'           FULL NAME                               
PZFUNCX1 DS    0C                                                               
PZFUNCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        INVOICE                                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFINV   DC    AL1(PZFINVX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFINV)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFINVL1 DC    AL1(PZFINVX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(8)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Invoice='              FULL NAME                               
PZFINVX1 DS    0C                                                               
PZFINVX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        ACTIVITY                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFSRC   DC    AL1(PZFSRCX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFSRC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFSRCL1 DC    AL1(PZFSRCX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(7)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'Src='                SHORT NAME-USE START OF FULL            
         DC    C'Source='               FULL NAME                               
PZFSRCX1 DS    0C                                                               
PZFSRCX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DONE                                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFDNE   DC    AL1(PZFDNEX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDNE)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFDNEL1 DC    AL1(PZFDNEX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Done'                  FULL NAME                               
PZFDNEX1 DS    0C                                                               
PZFDNEX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BATCH                                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFBAT   DC    AL1(PZFBATX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFBAT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFBATL1 DC    AL1(PZFBATX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Batch='                FULL NAME                               
PZFBATX1 DS    0C                                                               
PZFBATX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        MEDIA                                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFMED   DC    AL1(PZFMEDX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFMED)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(PRMDOCDD)            DDS ONLY                                
*                                                                               
PZFMEDL1 DC    AL1(PZFMEDX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Media='                FULL NAME                               
PZFMEDX1 DS    0C                                                               
PZFMEDX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        TRACE                                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFTRA   DC    AL1(PZFTRAX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFTRA)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                            TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
         DC    AL1(PRMDOCDD)            DDS ONLY                                
*                                                                               
PZFTRAL1 DC    AL1(PZFTRAX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Trace'                 FULL NAME                               
PZFTRAX1 DS    0C                                                               
PZFTRAX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        OVERRIDE                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFOVR   DC    AL1(PZFOVRX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFOVR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFOVRL1 DC    AL1(PZFOVRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Override'              FULL NAME                               
PZFOVRX1 DS    0C                                                               
PZFOVRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        MONTH OF SERVICE                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFMOS   DC    AL1(PZFMOSX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFMOS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFMOSL1 DC    AL1(PZFMOSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Mos='                  FULL NAME                               
PZFMOSX1 DS    0C                                                               
PZFMOSX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        DELETED                                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFDEL   DC    AL1(PZFDELX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDEL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFDELL1 DC    AL1(PZFDELX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'Deleted'               FULL NAME                               
PZFDELX1 DS    0C                                                               
PZFDELX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        USER                                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZFUSR   DC    AL1(PZFUSRX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFUSR)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZFUSRL1 DC    AL1(PZFUSRX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'Usr '                SHORT NAME-USE START OF FULL            
         DC    C'User='                 FULL NAME                               
PZFUSRX1 DS    0C                                                               
PZFUSRX  DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 15 - EPIC DETAIL FILTERS'                
***********************************************************************         
*        EPIC DETAIL FILTERS (15)                                               
***********************************************************************         
         SPACE 1                                                                
PZFDTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        INSERION DATE                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZDIN    DC    AL1(PZDINX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDIN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZDINL1 DC     AL1(PZDINX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Ins='                SHORT NAME-USE START OF FULL            
         DC    C'Insertdt='             FULL NAME                               
PZDINX1 DS     0C                                                               
PZDINX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        INSERTION ORDER NUMBER                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZDIO    DC    AL1(PZDIOX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDIO)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZDIOL1 DC     AL1(PZDIOX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(9)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'IONumber='             FULL NAME                               
PZDIOX1 DS     0C                                                               
PZDIOX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PRODUCT CODE                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZDPC    DC    AL1(PZDPCX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDPC)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZDPCL1 DC     AL1(PZDPCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'Pc= '                SHORT NAME-USE START OF FULL            
         DC    C'Pcode='                FULL NAME                               
PZDPCX1 DS     0C                                                               
PZDPCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        PRODUCT NAME                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
PZDPN    DC    AL1(PZDPNX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQZFDPN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
PZDPNL1 DC     AL1(PZDPNX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(2)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(6)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'Pn= '                SHORT NAME-USE START OF FULL            
         DC    C'Pname='                FULL NAME                               
PZDPNX1 DS     0C                                                               
PZDPNX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 16 - CIRCULATION SOURCES'                
***********************************************************************         
*        CIRCULATION SOURCES (16)                                               
***********************************************************************         
         SPACE 2                                                                
CSRCTAB  DS    0F                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        ABC                                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CSABC    DC    AL1(CSABCX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCSABC)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
CSABCL1 DC     AL1(CSABCX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'ABC'                   FULL NAME                               
CSABCX1 DS     0C                                                               
CSABCX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        BPA                                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CSBPA    DC    AL1(CSBPAX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCSBPA)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
CSBPAL1 DC     AL1(CSBPAX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'BPA'                   FULL NAME                               
CSBPAX1 DS     0C                                                               
CSBPAX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        CCAB                                                                   
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CSCCA    DC    AL1(CSCCAX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCSCCA)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
CSCCAL1 DC     AL1(CSCCAX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'CCAB'                  FULL NAME                               
CSCCAX1 DS     0C                                                               
CSCCAX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        PSS                                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CSPSS    DC    AL1(CSPSSX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCSPSS)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
CSPSSL1 DC     AL1(CSPSSX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'PSS'                   FULL NAME                               
CSPSSX1 DS     0C                                                               
CSPSSX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        PSS                                                                    
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CSVAC    DC    AL1(CSVACX-*)            ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCSVAC)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
CSVACL1 DC     AL1(CSVACX1-*)           LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(3)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'    '                SHORT NAME-USE START OF FULL            
         DC    C'VAC'                   FULL NAME                               
CSVACX1 DS     0C                                                               
CSVACX   DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 17 - CIRCULATION FREQUENCIES'            
***********************************************************************         
*        CIRCULATION FREQUENCIES (17)                                           
***********************************************************************         
         SPACE 2                                                                
CFRQTAB  DS    0F                                                               
         DC    AL1(7)                   SEVEN EXTRA BYTES                       
*                                                                               
*        W(eekly)                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFW      DC    AL1(CFWX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFW)              INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'40'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(7)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFWL1 DC       AL1(CFWX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'W   '                SHORT NAME-USE START OF FULL            
         DC    C'Weekly'                FULL NAME                               
CFWX1 DS       0C                                                               
CFWX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        Bi-W(eekly)                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFBW     DC    AL1(CFBWX-*)             ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFBW)             INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'40'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(14)             DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFBWL1 DC      AL1(CFBWX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BW  '                SHORT NAME-USE START OF FULL            
         DC    C'Bi-Weekly'             FULL NAME                               
CFBWX1 DS      0C                                                               
CFBWX    DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*                                                                               
*        M(onthly)                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFM      DC    AL1(CFMX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFM)              INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(1)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFML1 DC       AL1(CFMX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'M   '                SHORT NAME-USE START OF FULL            
         DC    C'Monthly'               FULL NAME                               
CFMX1 DS       0C                                                               
CFMX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        Bi-M(onthly)                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFBM     DC    AL1(CFBMX-*)             ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFBM)             INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(2)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFBML1 DC      AL1(CFBMX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BM  '                SHORT NAME-USE START OF FULL            
         DC    C'Bi-Monthly'            FULL NAME                               
CFBMX1 DS      0C                                                               
CFBMX    DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        A(nnual)                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFA      DC    AL1(CFAX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFA)              INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(1)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFAL1 DC       AL1(CFAX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'A   '                SHORT NAME-USE START OF FULL            
         DC    C'Annual'                FULL NAME                               
CFAX1 DS       0C                                                               
CFAX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        Bi-A(nnual)                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFBA     DC    AL1(CFBAX-*)             ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFBA)             INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(2)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFBAL1 DC      AL1(CFBAX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(4)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'BA  '                SHORT NAME-USE START OF FULL            
         DC    C'Bi-Annual'             FULL NAME                               
CFBAX1 DS      0C                                                               
CFBAX    DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        Semi-A(nnual)                                                          
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFSA     DC    AL1(CFSAX-*)             ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFSA)             INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'80'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(1)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(6)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFSAL1 DC      AL1(CFSAX1-*)            LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'SA  '                SHORT NAME-USE START OF FULL            
         DC    C'Semi-Annual'           FULL NAME                               
CFSAX1 DS      0C                                                               
CFSAX    DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        Q(uarterly)                                                            
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFQ      DC    AL1(CFQX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFQ)              INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(3)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFQL1 DC       AL1(CFQX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(6)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'Q   '                SHORT NAME-USE START OF FULL            
         DC    C'Quarterly'             FULL NAME                               
CFQX1 DS       0C                                                               
CFQX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        I(ssue record)                                                         
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFI      DC    AL1(CFIX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFISS)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFIL1 DC       AL1(CFIX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'I   '                SHORT NAME-USE START OF FULL            
         DC    C'Issue'                 FULL NAME                               
CFIX1 DS       0C                                                               
CFIX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
*        X - Non-Periodic                                                       
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
CFX      DC    AL1(CFXX-*)              ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQCFNPR)            INTERNAL CODE                           
         DC    AL2(0)                   HELP FILE NUMBER (SYSTEM X'25')         
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(Y)                   FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                                                                               
*        EXTRA DATA FOR ENTRY                                                   
*                                                                               
         DC    XL1'00'             CONTROL BYTE                                 
*                                  X'80' - DATE HAS SECOND OCCURENCE            
*                                  X'40' - DATE REQUIRES START DATE             
*                                                                               
         DC    AL1(0)              YEARS  TO NEXT ENTRY                         
         DC    AL1(0)              MONTHS TO NEXT ENTRY                         
         DC    AL1(0)              DAYS   TO NEXT ENTRY                         
         DC    AL1(0)              YEARS  TO SECOND OCCURENCE                   
         DC    AL1(0)              MONTHS TO SECOND OCCURENCE                   
         DC    AL1(0)              DAYS   TO SECOND OCCURENCE                   
*                                                                               
CFXL1 DC       AL1(CFXX1-*)             LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(5)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(12)                  MINIMUM OUTPUT LENGTH                   
         DC    CL4'X   '                SHORT NAME-USE START OF FULL            
         DC    C'Non-Periodic'          FULL NAME                               
CFXX1 DS       0C                                                               
CFXX     DS    0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 18 - SRDS PUB TYPES'                     
***********************************************************************         
*                                                                     *         
* (18)   SRDS PUBLICATION TYPES                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SRTPTAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        BUSINESS                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRTPBS   DC    AL1(SRTPBSX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSTPBS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SRTPBSL1 DC    AL1(SRTPBSX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'B   '                SHORT NAME-USE START OF FULL            
         DC    C'Business'              FULL NAME                               
SRTPBSX1 DS    0C                                                               
SRTPBSX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*        CONSUMER                                                               
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRTPCN   DC    AL1(SRTPCNX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSTPCN)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SRTPCNL1 DC    AL1(SRTPCNX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'C   '                SHORT NAME-USE START OF FULL            
         DC    C'Consumer'              FULL NAME                               
SRTPCNX1 DS    0C                                                               
SRTPCNX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
         TITLE 'PRVAL1 - TABLE PHASE - 18 - SRDS PUB TYPES'                     
***********************************************************************         
*                                                                     *         
* (19)   SRDS DATA TYPES                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SRDATAB  DS    0F                                                               
*                                                                               
         DC    AL1(0)                   NO EXTRA BYTES                          
*                                                                               
*        ADSIZE                                                                 
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDAAD   DC    AL1(SRDAADX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDAD)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SRDAADL1 DC    AL1(SRDAADX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'A   '                SHORT NAME-USE START OF FULL            
         DC    C'Adsize'                FULL NAME                               
SRDAADX1 DS    0C                                                               
SRDAADX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*        BLEED ADSIZE                                                           
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDABL   DC    AL1(SRDABLX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDBL)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TABLE ENTRY                               
SRDABLL1 DC    AL1(SRDABLX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'B   '                SHORT NAME-USE START OF FULL            
         DC    C'Bleed'                 FULL NAME                               
SRDABLX1 DS    0C                                                               
SRDABLX DS     0C                       END OF TABLE ENTRY                      
*                                                                               
*        CONTACTS                                                               
*                                                                               
*                    FIXED DATA IN TACTE ENTRY                                  
SRDACT   DC    AL1(SRDACTX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDCT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TACTE ENTRY                               
SRDACTL1 DC    AL1(SRDACTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'C   '                SHORT NAME-USE START OF FULL            
         DC    C'Contacts'              FULL NAME                               
SRDACTX1 DS    0C                                                               
SRDACTX DS     0C                       END OF TACTE ENTRY                      
*                                                                               
*        DIGITAL                                                                
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDADG   DC    AL1(SRDADGX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDDG)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TADGE ENTRY                               
SRDADGL1 DC    AL1(SRDADGX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'D   '                SHORT NAME-USE START OF FULL            
         DC    C'Digital'               FULL NAME                               
SRDADGX1 DS    0C                                                               
SRDADGX DS     0C                       END OF TADGE ENTRY                      
*                                                                               
*        MATERIALS                                                              
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDAMT   DC    AL1(SRDAMTX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDMT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TAMTE ENTRY                               
SRDAMTL1 DC    AL1(SRDAMTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'M   '                SHORT NAME-USE START OF FULL            
         DC    C'Materials'             FULL NAME                               
SRDAMTX1 DS    0C                                                               
SRDAMTX DS     0C                       END OF TAMTE ENTRY                      
*                                                                               
*        ISSUE                                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDAIS   DC    AL1(SRDAISX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDIS)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TAISE ENTRY                               
SRDAISL1 DC    AL1(SRDAISX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'I   '                SHORT NAME-USE START OF FULL            
         DC    C'Issue'                 FULL NAME                               
SRDAISX1 DS    0C                                                               
SRDAISX DS     0C                       END OF TAISE ENTRY                      
*                                                                               
*        NOTES                                                                  
*                                                                               
*                    FIXED DATA IN TABLE ENTRY                                  
SRDANT   DC    AL1(SRDANTX-*)           ENTRY LENGTH                            
         DC    AL1(0)                   2 BYTE INTERNAL CODE                    
         DC    AL2(PRQSRDNT)            INTERNAL CODE                           
         DC    AL2(0)                   DICTIONARY NUMBER IN PRDDEQUS           
         DC    AL2(Y)                   UNUSED        MASK                      
         DC    AL2(Y)                   MEDIA    TYPE MASK                      
         DC    AL2(Y)                   RECORD   TYPE MASK                      
         DC    AL2(Y)                   FLAVOR   TYPE MASK                      
         DC    AL1(PRMHEAD)             FIELD    TYPE MASK                      
         DC    AL1(Y)                   UNUSED   TYPE MASK                      
*                    LANGUAGE DATA IN TANTE ENTRY                               
SRDANTL1 DC    AL1(SRDANTX1-*)          LENGTH OF LANGUAGE SUB-ENTRY            
         DC    AL1(LANGENG)             LANGUAGE - ENGLISH                      
         DC    AL1(1)                   MINIMUM INPUT  LENGTH                   
         DC    AL1(1)                   MINIMUM OUTPUT LENGTH                   
         DC    CL4'N   '                SHORT NAME-USE START OF FULL            
         DC    C'Notes'                 FULL NAME                               
SRDANTX1 DS    0C                                                               
SRDANTX DS     0C                       END OF TANTE ENTRY                      
*                                                                               
         DC    AL1(0)                   END OF TABLE                            
*                                                                               
DDVALX   DS    0C                  PAD OUT TO CONSTANT SIZE                     
         DS    (18*1024-24-(DDVALX-DDVAL))X'00'                                 
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* VALWRKD - WORKING STORAGE DSECT                                     *         
***********************************************************************         
         SPACE 1                                                                
VALWRKD  DSECT                                                                  
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
SYSSYS   DS    AL1                 SYSTEM ID (SEE DDVALPARMS)                   
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
ERTVALER DS    AL2                 DDVAL ERROR CODE                             
ERTSYSER DS    AL2                 SYSTEM ERROR CODE                            
ERTENTL  EQU   *-ERTENTRY          LENGTH OF TABLE ENTRY                        
*                                                                               
*        DDVAL ERROR CODES                                                      
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
*PRWRIEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRWRIEQUS                                                      
         PRINT ON                                                               
*PRGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
*MBGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE MBGLOBEQUS                                                     
         PRINT ON                                                               
*MBMSGEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE MBMSGEQUS                                                      
         PRINT ON                                                               
* INCLUDED BOOKS - DDCOMFACS/DDCOREQUS/DDLANGEQUS/DDMBFACS/DDMINBLK             
*                  DDPERVALD                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DDMBFACS                                                       
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092PRVALS    05/01/02'                                      
         END                                                                    
