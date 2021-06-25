*          DATA SET PRHELP     AT LEVEL 112 AS OF 05/11/05                      
*PHASE T00AA3A,*                                                                
*INCLUDE TWABLD                                                                 
*INCLUDE MENU                                                                   
*                                                                               
*               PRHELP - BUILDS HELP MENU VIA DDMENU                            
         TITLE 'PRHELP - BUILDS HELP MENU VIA DDMENU - INIT'                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD HELP MENU VIA DDMENU                        *         
*        RESULT IS A MENU OF VALID ENTRIES FROM WHICH TO SELECT       *         
*           MAY BE A SEVERAL LEVEL MENU                               *         
*                                                                     *         
*                                                                     *         
*NTRY                                                                 *         
*        P0 ==> A(HELP MENU INTERNAL CODE)                            *         
*        P1 ==> A(FIELD REQUESTING HELP MENU)                         *         
*        P2 ==> A(HLPMNUCB) - CONTROL BLOCK                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
PRHELP   CSECT                                                                  
         NMOD1 HMWORKL,PRHELP,RR=RE,CLEAR=YES                                   
*                                                                               
         USING HMWORKD,RC          ESTABLISH WORKING STORAGE                    
*                                                                               
         ST    RE,HMWRELO          SAVE RELOCATION FACTOR                       
*                                                                               
         MVC   HMWPARMS(HMWPARML),0(R1)  SAVE PARAMETER LIST                    
*                                                                               
         XC    HMWDTIC,HMWDTIC     INIT ASKED FOR ICODE                         
         ICM   RF,15,HMWDTICA      POINT TO ICODE FOR MENU                      
         BZ    *+10                                                             
         MVC   HMWDTIC,0(RF)       SAVE ICODE                                   
*                                                                               
         L     R9,HMWCBLKA         ESTABLISH HELP MENU CONTROL BLOCK            
         USING HELPCB,R9                                                        
*                                                                               
         L     RA,HCBATWA          ESTABLISH HELP MENU SCREEN                   
         USING T405FFD,RA          ESTABLISH STANDARD SCREEN TOP                
*                                                                               
         ICM   R7,15,HCBASAVE      IF NO A(SAVEAREA) GIVEN                      
         BNZ   PRHINI10                                                         
*                                     ASSUME CALLER IS PRINT WRITER             
         L     R7,=AL4(HMMENUSV-T405FFD)   DISPLACEMENT OF HELP SAVE            
         LA    R7,0(R7,RA)            ADDRESS OF HELP SAVE AREA                 
*                                                                               
PRHINI10 DS    0H                                                               
*                                                                               
         ST    R7,HMWMNSVA            SAVE ADDRESS                              
         USING HMMENUSV,R7         ESTABLISH MENU SAVEAREA                      
*                                                                               
         OC    HMWDTIC,HMWDTIC     IF THIS IS CALL TO SEE IF MENU ON            
         BNZ   *+12                   SCREEN                                    
         CLI   HMMENUSW,0          AND NO MENU BEING PROCESSED                  
         BE    PRHELPX                EXIT                                      
*                                                                               
         OC    HMWDTIC,HMWDTIC     IF THIS IS CALL FOR NEW MENU                 
         BZ    *+10                                                             
         MVC   HMCBLKSV,HELPCB        SAVE HELP MENU CONTROL BLOCK              
*                                                                               
*        GET SYSTEM ADDRESSES                                                   
*                                                                               
         L     RF,HCBACOM          POINT TO COMFACS                             
         MVC   VDATCON,CDATCON-COMFACSD(RF)  A(DATCON)                          
         MVC   VCALLOV,CCALLOV-COMFACSD(RF)  A(CALLOV)                          
*                                                                               
         MVC   HMWDMCB+4(4),=X'D9000A00'                                        
*                                                                               
         MVC   HMWDMCB+7(1),HCBQVAL   COREQU FOR VALIDATION ROUTINE             
*                                                                               
         CLI   HCBQVAL,0           IF NOT PASSED                                
         BNE   *+8                                                              
         MVI   HMWDMCB+7,X'40'       DEFAULT TO PRVAL                           
*                                                                               
         GOTO1 VCALLOV,HMWDMCB,0                                                
         MVC   VDDVAL,HMWDMCB      A(??VAL)                                     
*                                                                               
         BAS   RE,VALINIT          INIT DDVAL                                   
*                                                                               
         NI    CONHEADH+6,X'FF'-X'08' RESTORE NORMAL INTENSITY                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        FIRST TIME CALL FOR MENU                                     *         
*          FIND ENTRY IN HELP MENU TABLE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   HMMENUSW,0          SKIP IF NOT FIRST TIME                       
         BNE   VAL1STX                                                          
*                                                                               
         MVC   HMMTIC,HCBMTIC      SET MASTER ICODE                             
*                                                                               
*        MAY COME BACK HERE IF THIS MENU HAS CHILDREN                           
*                                                                               
VAL1ST   DS    0H                                                               
*                                                                               
*        FIND ENTRY FOR REQUESTED HELP IN HELP MENU TABLE                       
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPTRAQ',HMMTIC),(2,HMWDTIC),          X        
               HCBATAB,0,0,0                                                    
*                                                                               
         CLI   VLPERR,0            EXIT ON ERRORS                               
         BNE   PRHELPX                                                          
*                                                                               
*        SAVE TABLE ENTRY IN TWA                                                
*                                                                               
VAL1STSV DS    0H                                                               
*                                                                               
         LA    RF,HMHLPTBS         POINT TO HELP TABLE ENTRY SAVEAREA           
*                                                                               
         L     R8,HCBATAB          ESTABLISH RETURNED TABLE ENTRY               
         USING VLTABD,R8                                                        
*                                                                               
         MVC   0(VLTABLQ,RF),VLTAB  MOVE MINIMUM ENTRY SIZE                     
         LA    RF,VLTABLQ(RF)      BUMP POINTER                                 
         MVC   0(HTBXHDRL,RF),HTBXHDR  SAVE HELP HEADER                         
*                                                                               
         LA    RF,HTBXHDRL(RF)     BUMP POINTER                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,HTBTTLSN       NUMBER OF TITLES                             
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,HTBTTLSL       LENGTH OF A TITLE LINE                       
         BZ    *+6                                                              
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
*                                                                               
         LA    R1,HTBTTLS          START OF MENU TITLES                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R1)       SAVE TITLES                                  
         LA    RF,1(RE,RF)         BUMP POINTERS                                
         LA    R1,1(RE,R1)                                                      
         BCT   R0,*-22                                                          
*                                                                               
VAL1STX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        RETURN TO DDMENU IF MENU DISPLAYED AND SELECTS TO BE         *         
*           PROCESSED                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         LA    R8,HMHLPTBS         POINT TO TABLE ENTRY SAVEAREA                
         USING VLTAB,R8            ESTABLISH TABLE ENTRY                        
*                                                                               
         LA    R6,HMWMENUC         ESTABLISH DDMENU CONTROL BLOCK               
         USING MNBLKD,R6                                                        
*                                                                               
         BAS   RE,MENUINIT         INIT DDMENU                                  
*                                                                               
*        TWO CASES                                                              
*              1. CALL TO SEE IF IN THE MIDST OF MENU PROCESSING                
*              2. CALL TO START PUTTING OUT A MENU                              
*                                                                               
         OC    HMWDTIC,HMWDTIC     SKIP UNLESS FIRST CALL TO CHECK              
         BNZ   MENU1STN                 MENU STATUS                             
*                                                                               
         L     R2,HMSFLDA          DISPLACEMENT OF FIELD INVOKING HELP          
         LA    R2,0(R2,RA)         RELOCATE ADDRESS                             
*                                                                               
         ST    R2,MNAFLD           PASS     A(FIELD INVOKING HELP)              
*                                                                               
         L     RF,HCBATIOB         POINT TO TIOB                                
         MVC   HMSFLDCD,TIOBCURD-TIOBD(RF)   SAVE CURSOR DISPLACEMENT           
*                                                                               
*        RETURN TO DDMENU - WE ARE IN MIDDLE OF SELECT                          
*                                                                               
         L     RF,MNVALTAB            POINT TO VALUE TABLE(WAS SAVED)           
         USING MNVALTBD,RF            ESTABLISH TABLE HEADER                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MNVFRSTF          POINT TO FIRST SELECT FIELD               
         BZ    *+12                      SKIP IF NOT MENU SCREEN                
         LA    R1,0(R1,RA)                                                      
         OI    6(R1),X'40'            PLACE CURSOR AT THIS FIELD                
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(MENU),HMWDMCB,MNBLKD,RR=HMWRELO  RETURN TO MENU               
*                                                                               
         CLI   MNERR,0             ERROR EXIT IF THERE IS AN ERROR              
         BNE   MENUERR                                                          
*                                                                               
         CLI   HMMENUSW,HMMENUQ    IF MENU SWITCH ON NO SELECT MADE             
         BE    MENURTNX               DISPLAY SCREEN                            
*                                                                               
         OC    HMWSELIC,HMWSELIC   IF NO SELECT MADE                            
         BZ    MENURTNX               DISPLAY SCREEN                            
*                                                                               
         OC    HMWCHLIC,HMWCHLIC   DISPLAY SCREEN IF SELECTION HAS              
         BZ    MENURTNX               NO CHILDREN                               
*                                                                               
*        FIND ENTRY FOR SELECTED HELP IN HELP MENU TABLE                        
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPTRAQ',HMMTIC),(2,HMWSELIC),         X        
               HCBATAB,0,0,0                                                    
*                                                                               
         CLI   VLPERR,0            EXIT ON ERRORS                               
         BNE   PRHELPX                                                          
*                                                                               
         MVC   HMWDTIC,HMWCHLIC    ELSE RESET TO CHILD'S ICODE                  
         MVC   HMWFLDA,MNAFLD           SAVE A(FLD NEEDING HELP)                
*                                                                               
         B     VAL1STSV                   PUT OUT THIS TABLE                    
*                                                                               
MENUERR  DS    0H                  ERROR EXIT                                   
*                                                                               
         ICM   R2,7,MNACURSR       POINT TO FIELD IN ERROR                      
*                                                                               
         CLI   MNERR,MNERRHPQ      IF HELP MESSAGE                              
         BNE   *+14                                                             
         MVC   8(3,R2),=CL3' '        ERASE HELP SELECT CODE                    
         OI    6(R2),X'80'            TRANSMIT FIELD                            
*                                                                               
         L     RF,HCBATIOB         POINT TO TIOB                                
         USING TIOBD,RF            ESTABLISH TIOB                               
*                                                                               
         SR    R2,RA                                                            
         STCM  R2,3,TIOBCURD       SET DISP OF CURSOR FIELD                     
*                                                                               
         OI    TIOBINDS,TIOBSETC   SET TO SET CURSOR                            
         MVI   TIOBCURI,0          KILL CURSOR DISPLACEMENT                     
*                                                                               
MENURTNX DS    0H                                                               
*                                                                               
         B     HMNOUT              DISPLAY SCREEN                               
*                                                                               
MENU1STN DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NEW MENU TO BE DISPLAYED                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MENUNEW  DS    0H                                                               
*                                                                               
         L     R2,HMWFLDA          POINT TO FIELD ASKING FOR HELP               
         ST    R2,MNAFLD           PASS A(FIELD INVOKING HELP)                  
         SR    R2,RA               DISPLACEMENT OF FIELD                        
         ST    R2,HMSFLDA          SAVE A(FIELD INVOKING HELP)                  
*                                                                               
*        USE DDVAL TO BUILD TABLE OF ENTRIES IN CHILDREN'S MENU                 
*                                                                               
         MVC   VLFLT3V,HTBFVAL     SET FILTER VALUE                             
*                                                                               
         CLI   HCBFLT3V,0          IF CALLER PASSED AN OVERRIDE                 
         BE    *+10                                                             
         MVC   VLFLT3V,HCBFLT3V       USE IT                                    
*                                                                               
         MVI   VLFLT6V,PKVDOCY     ONLY DOCUMENTED ENTRIES                      
*                                                                               
         MVC   HMWDTIC,HTBCHLIC    SAVE CHILDREN'S ICODE                        
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPTABQ',HMWDTIC),                     X        
               0,(HCBSEGS,HCBATAB),0,0,0                                        
*                                                                               
         CLI   VLPERR,0            EXIT ON ERROR                                
         BNE   PRNOTAB                                                          
*                                                                               
         DROP  R8                                                               
*                                                                               
         L     R8,HCBATAB          ESTABLISH RETURNED DDVAL TABLE               
         USING VLTABD,R8              FOR CHILDREN                              
*                                                                               
         LM    R3,R5,VLTCATAB      LOAD BXLE PARMS FOR DDVAL TABLE              
         USING VLTAB,R3            ESTABLISH TABLE ENTRY                        
*                                                                               
         CR    R3,R5               GET OUT IF TABLE IS EMPTY                    
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,MNVALTAB         ESTABLISH DDMENU TABLE                       
         USING MNVALTBD,R2                                                      
*                                                                               
         LA    R2,L'MNVHEAD(R2)    BUMP TO FIRST ENTRY IN TABLE                 
         USING MNVALENT,R2         ESTABLISH TABLE ENTRY                        
*                                                                               
         XC    MNVALENT(MNVOVERQ+MNVHDRL),MNVALENT INIT 1ST ENTRY               
*                                                                               
*        FOR EACH ENTRY IN VALTAB CREATE ONE IN MENUTAB                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
HMNLOOP  DS    0H                                                               
*                                                                               
         MVC   MNVOPTIC,VLTICODE   SAVE OPTION ICODE                            
*                                                                               
*        ALL HELP MENUS HAVE SAME MASTER ICODE                                  
*                                                                               
         CLC   HCBMTIC,HMWDTIC     SKIP IF THIS IS NOT HELP MENU                
         BNE   *+10                                                             
         MVC   MNVCHLIC,HTBCHLIC   SAVE OPTION CHILDREN'S ICODE                 
*                                                                               
         MVC   MNVOPTL,VLTLFULL    SAVE OPTION LENGTH                           
*                                                                               
         ICM   RF,1,MNVOPTL        LENGTH OF OPTION                             
         BZ    *+6                 NO OPTION                                    
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MNVOPT(0),VLTFULL   SAVE OPTION                                  
*                                                                               
         CLI   HCBSYST,HCBSYSRE    SKIP IF NOT REP SYSTEM                       
         BNE   HMNLOOP1                                                         
*                                                                               
         CLC   VLTSHORT,SPACES     SKIP IF NO SHORT FORM                        
         BNH   HMNLOOP1                                                         
*                                                                               
         CLC   VLTSHORT,VLTFULL    SKIP IF SHORT SAME AS START OF FULL          
         BE    HMNLOOP1                                                         
*                                                                               
         MVC   MNVOPT(L'VLTSHORT),VLTSHORT  START WITH SHORT FORM               
*                                                                               
         LA    R1,MNVOPT+L'VLTSHORT-1 LAST BYTE OF SHORT                        
*                                                                               
         CLI   0(R1),C' '          FIND LAST CHARACTER                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         LA    RE,VLTFULL(RF)      IF FULL ENDS IN PLUS SIGN                    
         CLI   0(RE),C'+'             (RF HAS EXECUTE LENGTH)                   
         BNE   *+22                                                             
         MVC   1(2,R1),=C'=+'         ADD IT TO SHORT FORM                      
         AHI   RF,-2                  ELIMINATE FROM FULL                       
         LA    R1,2(R1)               BUMP POINTER                              
         B     HMNLOOPA                                                         
*                                                                               
         CLI   0(RE),C'='             (RF HAS EXECUTE LENGTH)                   
         BNE   *+14                                                             
         MVI   1(R1),C'='             ADD IT TO SHORT FORM                      
         BCTR  RF,0                   ELIMINATE FROM FULL                       
         LA    R1,1(R1)               BUMP POINTER                              
*                                                                               
HMNLOOPA DS    0H                                                               
*                                                                               
         MVI   1(R1),C'('                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),VLTFULL     SAVE OPTION                                  
*                                  PRINT SHORT FORM                             
         LA    R1,3(RF,R1)         END OF FULL FORM                             
*                                                                               
         MVI   0(R1),C')'                                                       
*                                                                               
         LA    RF,1(R1)            NEXT PRINT POSITION                          
*                                                                               
         LA    R1,MNVOPT                                                        
         SR    RF,R1               NEW OPTION LENGTH                            
         STC   RF,MNVOPTL                                                       
         BCTR  RF,0                RECTIFY LENGTH REGISTER                      
*                                                                               
HMNLOOP1 DS    0H                                                               
*                                                                               
         LA    RF,MNVHDRL+MNVOVERQ+1(RF)   ADD ON OVERHEAD LENGTH               
         STC   RF,MNVALLEN         SET ENTRY LENGTH                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,VLTICODE       GET INTERNAL ICODE                           
         LA    RE,100              SET TO 100 FOR NOW                           
******** LA    RE,100(RE)          ADD 100 FOR NOW                              
         STCM  RE,1,MNVSELID       USE AS HELP MSG NUMBER                       
*                                                                               
         LA    R2,0(RF,R2)         BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         XC    MNVALENT(MNVOVERQ+MNVHDRL),MNVALENT INIT NEXT ENTRY              
*                                                                               
HMNCONT  DS    0H                                                               
*                                                                               
         BXLE  R3,R4,HMNLOOP       LOOP THROUGH VALTAB                          
*                                                                               
HMNDONE  DS    0H                                                               
*                                                                               
         L     RF,MNVALTAB         FORCE INITIALIZATION                         
         USING MNVALTBD,RF                                                      
*                                                                               
         XC    MNVFRSTE,MNVFRSTE                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(MENU),HMWDMCB,MNBLKD,RR=HMWRELO  DDMENU TO DISPLAY            
*                                                                               
         NOP   *+4                 TESTING                                      
*                                                                               
HMNOUT   DS    0H                  EXIT TO FACPAK TO WRITE SCREEN               
         L     RD,HCBASYRD         SET TO EXIT IMMEDIATELY                      
         B     PRHELPX                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
PRNOTAB  DS    0H                                                               
*                                                                               
         L     R2,MNAFLD           POINT TO FIELD INVOKING HELP                 
         OI    6(R2),X'80'         FORCE FIELD TRANSMISSION                     
         LA    R1,8(R2)            SET DEFAULT CURSOR POSITION                  
*                                                                               
         MVC   CONHEAD,=CL60'NO KEYWORDS AVAILABLE FOR SELECTION'               
         OI    CONHEADH+6,X'88'    FORCE RE-WRITE/HIGH INTENSITY                
*                                                                               
         L     RF,HCBATIOB         POINT TO TIOB                                
         USING TIOBD,RF            ESTABLISH TIOB                               
*                                                                               
         SR    R1,R2               GET CURSOR DISPLACEMENT IN FIELD             
         SH    R1,=H'8'                                                         
         BM    *+8                 TAKING NO CHANCES                            
         STC   R1,TIOBCURI         SAVE                                         
*                                                                               
         SR    R2,RA                                                            
         STCM  R2,3,TIOBCURD       SET DISP OF CURSOR FIELD                     
*                                                                               
         OI    TIOBINDS,TIOBSETC   SET TO SET CURSOR                            
*                                                                               
PRHELPX DS     0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PRHELP - DDVAL CONTROL BLOCK INIT - VALINIT'                    
***********************************************************************         
*                                                                     *         
*        INIT VLBLOCK FOR DDVAL                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALINIT  NTR1  0H                                                               
*                                                                               
         LA    R5,VLPARMS          ESTABLISH DDVAL BLOCK                        
         USING VLPARMS,R5                                                       
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR BLOCK                            
*                                                                               
         MVC   VLACFACS,HCBACOM    A(COMFACS)                                   
         MVC   VLCTRY,HCBCTRY      SET COUNTRY CODE                             
         MVC   VLLANG,HCBLANG      LANGUAGE CODE                                
         MVC   VLAGENCY,TWAAGY     AGENCY                                       
         MVC   VLSYSTEM,HCBSYST                                                 
*                                                                               
         MVI   VLFLT6V,PKVDOCY     SHOW ONLY DOCUMENTED OPTIONS                 
*                                                                               
*        SET FLAVOR FILTER                                                      
*                                                                               
         OC    HMWDTIC,HMWDTIC     IF 1ST TIME TO CHECK FOR SELECTS             
         BNZ   *+8                                                              
         LA    R9,HMCBLKSV         USE SAVED CONTROL BLOCK FOR FILTERS          
*                                                                               
         CLI   HCBSYST,HCBSYSPR    SKIP IF NOT PRINT SYSTEM                     
         BNE   VALIFPRN                                                         
*                                                                               
         CLI   HCBFLVR,C'B'        BUY                                          
         BE    *+8                                                              
         CLI   HCBFLVR,C'C'        CONTRULE/CONTRACT                            
         BNE   *+8                                                              
         MVI   VLFLT4V,PKVFLBUY    BUY                                          
*                                                                               
         CLI   HCBFLVR,C'O'        SET FLAVOR OPTIONS                           
         BNE   *+8                                                              
         MVI   VLFLT4V,PKVFLCON    CONTRACT ONLY                                
*                                                                               
*        SET AOR    FILTER                                                      
*                                                                               
         MVC   VLFLT1V,HCBAOR      AOR ONLY                                     
*                                                                               
*        SET MEDIA TYPE FILTER                                                  
*                                                                               
         CLI   HCBMED,C'M'         MAGAZINE                                     
         BE    *+8                                                              
         CLI   HCBMED,C'*'         ALL MEDIA                                    
         BE    *+8                                                              
         CLI   HCBMED,C' '         NOT ENTERED                                  
         BE    *+8                                                              
         CLI   HCBMED,C'C'         COMBINED                                     
         BNE   *+8                                                              
         MVI   VLFLT2V,PKVMDMAG    MAGAZINE                                     
*                                                                               
         CLI   HCBMED,C'N'         NEWSPAPER                                    
         BNE   *+8                                                              
         MVI   VLFLT2V,PKVMDNWS                                                 
*                                                                               
         CLI   HCBMED,C'O'         OUTDOOR                                      
         BNE   *+8                                                              
         MVI   VLFLT2V,PKVMDOUT                                                 
*                                                                               
         CLI   HCBMED,C'S'         SUPLEMENT                                    
         BNE   *+8                                                              
         MVI   VLFLT2V,PKVMDSUP                                                 
*                                                                               
         CLI   HCBMED,C'T'         TRADE                                        
         BNE   *+8                                                              
         MVI   VLFLT2V,PKVMDTRD                                                 
*                                                                               
         B     VALIFLVX                                                         
*                                                                               
VALIFPRN DS    0H                                                               
*                                                                               
         CLI   HCBSYST,HCBSYSRE    SKIP IF NOT REP SYSTEM                       
         BNE   VALIFREN                                                         
*                                                                               
         CLI   HCBFLVR,C'C'        CONTRACT                                     
         BNE   *+8                                                              
         MVI   VLFLT4V,RKVFLCON    CON                                          
*                                                                               
         CLI   HCBFLVR,C'R'        RGON                                         
         BNE   *+8                                                              
         MVI   VLFLT4V,RKVFLNRG    RGON                                         
*                                                                               
         CLI   HCBFLVR,C'F'        FLIGHT DATES                                 
         BNE   *+8                                                              
         MVI   VLFLT4V,RKVFLFDS    FLIGHT DATES                                 
*                                                                               
         B     VALIFLVX                                                         
*                                                                               
VALIFREN DS    0H                                                               
*                                                                               
VALIFLVX DS    0H                                                               
*                                                                               
*        SET FIELD TYPE FILTER                                                  
*                                                                               
         CLI   HCBFTYP,C'H'        HEADLINE                                     
         BNE   *+8                                                              
         MVI   VLFLT5V,PKVHEAD                                                  
*                                                                               
         CLI   HCBFTYP,C'M'        MIDLINE                                      
         BNE   *+8                                                              
         MVI   VLFLT5V,PKVMID                                                   
*                                                                               
         CLI   HCBFTYP,C'R'        ROW                                          
         BNE   *+8                                                              
         MVI   VLFLT5V,PKVROW                                                   
*                                                                               
         CLI   HCBFTYP,C'C'        COLUMN                                       
         BNE   *+8                                                              
         MVI   VLFLT5V,PKVCOL                                                   
*                                                                               
         L     R9,HMWCBLKA         RE-POINT TO HELP MENU CONTROL BLOCK          
*                                                                               
         GOTO1 VDATCON,HMWDMCB,(5,0),(2,VLTODAYC) GET TODAY'S DATE              
*                                                                               
VALINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'PRHELP - DDMENU INIT - MENUINIT'                                
***********************************************************************         
*                                                                     *         
*        INITIALIZE DDMENU CONTROL BLOCK                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
MENUINIT NTR1                                                                   
*                                                                               
         USING MNBLKD,R6           ESTABLISH DDMENU CONTROL BLOCK               
         USING HMMENUSV,R7         ESTABLISH MENU SAVEAREA                      
         USING VLTAB,R8            ESTABLISH TABLE ENTRY                        
*                                                                               
         XC    HMWFLD,HMWFLD       INIT OUTPUT BUILD AREA                       
         LA    RF,HMWFLD           INIT OUTPUT POINTER                          
         ST    RF,HMWOUTA                                                       
*                                                                               
         XC    MNBLKD(MNBLKL),MNBLKD  INIT DDMENU CONTROL BLOCK                 
*                                                                               
         MVC   MNATWA,HCBATWA      PASS A(TWA)                                  
         MVC   MNATIOB,HCBATIOB    PASS A(TIOB)                                 
         MVC   MNACOM,HCBACOM      PASS A(COMFACS)                              
*                                                                               
         L     RF,=V(TWABLD)       GET A(TWABLD)                                
         A     RF,HMWRELO          RELOCATE ADDRESS                             
         ST    RF,MNATWBLD         PASS A(TWABLD)                               
*                                                                               
         L     RF,=A(HMNHOOK)                                                   
         A     RF,HMWRELO          RELOCATE ADDRESS                             
         ST    RF,MNHOOK           PASS A(HOOK)                                 
*                                                                               
         LA    RF,HMMENUTB         DISPLACEMENT OF MENU TABLE                   
         ST    RF,MNVALTAB         PASS A(DDMENU TABLE)                         
*                                                                               
         LA    RF,HTBTTLS          PASS A(MENU TITLES)                          
         STCM  RF,7,MNATITLE                                                    
*                                                                               
         MVC   MNATITLN,HTBTTLSN   NUMBER OF TITLES                             
*                                                                               
         MVC   MNSAVEPG,HCBPAGE    SAVE THINGS IN 1ST TEMPSTR                   
         MVC   MNSTRTLN,HTBLINE    START MENU ON LINE 7                         
         MVC   MNNCOLS,HTBCOLN     SET NUMBER OF COLUMNS                        
         MVC   MNSELWID,HTBSELWD   WIDTH OF SELECT FIELD                        
*                                                                               
         MVC   MNSCRID,HCBSCRN     SET SCREEN NUMBER FOR HELP                   
*                                                                               
         OI    MNCNTL,MNVREPQ      REPLACE ? WITH SELECT                        
         MVI   MNPFCAN,12          SET CANCEL SELECT PFKEY                      
*                                                                               
         TM    HTBCTL,HTBCMNYQ+HTBCSAMQ IF MULTIPLE SELECTS ALLOWED             
         BZ    *+8                                                              
         OI    MNCNTL,MNMULTQ              ALLOW MULTIPLE SELECTS               
*                                                                               
MENUINIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6,R7,R8                                                         
*                                                                               
         TITLE 'PRHELP - BUILD HELP MENUS - HMNHOOK'                            
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE FROM DDMENU                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HMNHOOK  NTR1                                                                   
*                                                                               
*        DETERMINE HOOK MODE                                                    
*                                                                               
         USING MNBLKD,R6           ESTABLISH DDMENU CONTROL BLOCK               
         USING HMMENUSV,R7         ESTABLISH MENU SAVEAREA                      
         USING VLTAB,R8            ESTABLISH TABLE ENTRY                        
*                                                                               
         CLI   MNMODE,MNDISPQ      BUILD DISPLAY LINE                           
         BE    HMHDISP                                                          
*                                                                               
         CLI   MNMODE,MNPROCQ      PROCESS SELECTION                            
         BE    HMHPROC                                                          
*                                                                               
         CLI   MNMODE,MNVSELQ      VALIDATE SELECTION                           
         BE    HMHVSEL                                                          
*                                                                               
         B     HMHOOKX             UNKNOWN HOOK                                 
*                                                                               
         TITLE 'PRHELP - DISPLAY MENU TABLE ENTRY - HMHDISP'                    
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE TO DISPLAY A MENU TABLE ENTRY         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHDISP  DS    0H                                                               
*                                                                               
         L     R7,HMWMNSVA         SAVE AREA ADDRESS                            
         USING HMMENUSV,R7         ESTABLISH MENU SAVEAREA                      
*                                                                               
         MVI   HMMENUSW,HMMENUQ    INDICATE DDMENU CALLED                       
*                                                                               
         L     R1,MNVALTAB         POINT TO VALUE TABLE                         
         USING MNVALTBD,R1         ESTABLISH TABLE HEADER                       
*                                                                               
         LH    R1,MNVFRSTF         POINT TO FIRST SELECT FIELD                  
         AR    R1,RA                                                            
         OI    6(R1),X'40'         PLACE CURSOR AT THIS FIELD                   
*                                                                               
         DROP  R1                                                               
*                                                                               
HMHD1STX DS    0H                                                               
*                                                                               
         L     R3,MNAVENT          ESTABLISH MENU TABLE ENTRY                   
         USING MNVALENT,R3                                                      
*                                                                               
         XC    MNVALTXT,MNVALTXT   INIT TEXT RETURN AREA                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MNVOPTL        GET OPTION LENGTH                            
         BZ    *+10                                                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MNVALTXT(0),MNVOPT  DISPLAY OPTION                               
*                                                                               
         MVC   CONHEAD,=CL60'PLEASE MAKE A SELECTION'                           
         OI    CONHEADH+6,X'80'    FORCE RE-WRITE                               
*                                                                               
         B     HMHOOKX                                                          
*                                                                               
         TITLE 'PRHELP - VALIDATE SELECTION ENTRY - HMHVSEL'                    
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE TO VALIDATE A SELECTION               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHVSEL  DS    0H                                                               
*                                                                               
         L     R3,MNAVENT          ESTABLISH MENU TABLE ENTRY                   
         USING MNVALENT,R3                                                      
*                                                                               
         CLC   MNVSELVL,SPACES     SKIP IF NOT SELECTED                         
         BNH   HMHVSELX                                                         
*                                                                               
*        ANALYZE SELECTION                                                      
*                                                                               
         LA    R0,L'MNVSELVL       LENGTH OF SELECT FIELD                       
         LA    RF,MNVSELVL         START OF SELECT FIELD                        
*                                                                               
         CLI   0(RF),C'S'          CHECK FOR A SELECTION                        
         BE    HMHVSSL                                                          
         CLI   0(RF),C'+'          CHECK FOR A HELP REQUEST                     
         BE    HMHVSHP                                                          
         CLI   0(RF),C'?'          CHECK FOR A HELP REQUEST                     
         BE    HMHVSHP                                                          
         CLI   0(RF),C'?'          CHECK FOR A HELP REQUEST                     
         BE    HMHVSHP                                                          
         CLI   1(RF),C'?'          CHECK FOR A HELP REQUEST                     
         BE    HMHVSHP                                                          
         LA    RF,1(RF)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         B     HMHVSER1                                                         
*                                                                               
         TITLE 'PRHELP - PROCESS A SELECTION - HMHVSSL'                         
***********************************************************************         
*                                                                     *         
*        ADD SELECTION TO ACCUMULATION AREA                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHVSSL  DS    0H                                                               
*                                                                               
         L     R2,HMWOUTA          POINT TO NEXT OUTPUT POSITION                
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,MNVOPTL        LENGTH OF SELECTED OPTION                    
         BZ    HMHVSSLX            MUST HAVE A LENGTH                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),MNVOPT      DISPLAY OPTION                               
*                                                                               
         MVC   HMWSELIC,MNVOPTIC   SAVE ICODE                                   
         MVC   HMWCHLIC,MNVCHLIC   SAVE CHILDREN'S ICODE                        
*                                                                               
         LA    R2,0(RF,R2)         END OF OPTION                                
*                                                                               
         CLI   0(R2),C')'          ELIMINATE ANY ABBREVIATIONS                  
         BNE   HMHVSSL1                                                         
*                                                                               
         MVI   0(R2),C' '                                                       
         BCTR  R2,0                                                             
*                                                                               
         CLI   0(R2),C'('                                                       
         BE    *+14                                                             
         MVI   0(R2),C' '                                                       
         BCTR  R2,0                                                             
         B     *-14                                                             
*                                                                               
         MVI   0(R2),C' '                                                       
         BCTR  R2,0                                                             
*                                                                               
HMHVSSL1 DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT AVAILABLE SPACE                 
         MVI   0(R2),C','          SEPARATING COMMA                             
*                                                                               
         TM    HTBCTL,HTBCSLAQ     IF SLASH TO BE USED AS SEPARATOR             
         BNO   *+8                                                              
         MVI   0(R2),C'/'             SET IT NOW                                
*                                                                               
         LA    R2,1(R2)                                                         
*                                                                               
         ST    R2,HMWOUTA          UPDATE OUTPUT POSITION                       
*                                                                               
HMHVSSLX DS    0H                                                               
*                                                                               
         B     HMHVSELX                                                         
*                                                                               
         TITLE 'PRHELP - PUT OUT HELP MESSAGE - HMHVSHP'                        
***********************************************************************         
*                                                                     *         
*        HELP REQUESTED - DISPLAY ENTRY RECORD DESCRIPTION            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHVSHP  DS    0H                                                               
*                                                                               
*        READ DICTIONARY RECORD                                                 
******                                                                          
******   CLC   HMWDTIC,=AL2(PRQWRKYW) SKIP IF NOT KEYWORD MENU                  
******   BNE   HMHVSER1                                                         
*                                                                               
         LA    R4,HMWKEY           INIT DICTIONARY KEY                          
         USING DICKEYD,R4             MENU ITEM                                 
         XC    DICKEY,DICKEY       INIT KEY                                     
*                                                                               
         MVI   DICSYS,DICSYSQ      SET SYSTEM TO FACILITIES                     
         MVI   DICKTYP,DICKTYPQ    SET FOR DICTIONARY RECORDS                   
         MVC   DICCODE,=CL8'PRWRI'  SET DICTIONARY NAME                         
*                                                                               
         CLI   HCBSYST,HCBSYSRE    IF REP SYSTEM                                
         BNE   *+10                                                             
         MVC   DICCODE,=CL8'RENWR'  SET DICTIONARY NAME                         
*                                                                               
         MVC   DICENTRY,SPACES     SPACE FILL ENTRY NAME                        
         SR    RE,RE                                                            
         ICM   RE,1,MNVOPTL        LENGTH OF DICTIONARY ENTRY                   
         BZ    HMHVSHPX            MUST HAVE SOMETHING                          
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DICENTRY(0),MNVOPT   FILL IN DICTIONARY ENTRY                    
*                                                                               
         MVC   HMWKEYSV,HMWKEY     SAVE KEY                                     
*                                                                               
*        SWITCH TO CONTROL SYSTEM                                               
*                                                                               
         L     RF,HCBACOM                                                       
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    HMWDMCB(8),HMWDMCB                                               
         MVI   HMWDMCB,X'0A'       CONTROL SYSTEM                               
         GOTO1 (RF),HMWDMCB        SWITCH TO CONTROL SYSTEM                     
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,HCBACOM                                                       
         L     RF,CDATAMGR-COMFACSD(RF)                                         
*                                                                               
         GOTO1 (RF),HMWDMCB,=C'DMRDHI',=C'GENDIR',DICKEY,DICKEY,0,0             
*                                                                               
         CLC   DICKEY,HMWKEYSV     MUST FIND RECORD                             
         BNE   HMHVSHSX                                                         
*                                                                               
         GOTO1 (RF),HMWDMCB,=C'GETREC',=C'GENFIL',DICKEY+36,           X        
               HMWIO,HMWDMWRK                                                   
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         LA    R4,HMWIO            POINT TO RECORD                              
         LA    R4,DICFIRST         POINT TO FIRST ELEMENT                       
         USING DENAMD,R4           ESTABLISH AS NAME ELEMENT                    
*                                                                               
*        FIND DESCRIPTION ELEMENT                                               
*                                                                               
HMHVSHPL DS    0H                                                               
*                                                                               
         CLI   DENAMEL,0           EXIT IF END OF RECORD FOUND                  
         BE    HMHVSHSX                                                         
*                                                                               
         CLI   DENAMEL,X'02'       FIND NAME ELEMENT                            
         BE    HMHVSHPF                                                         
*                                                                               
HMHVSHPC DS    0H                                                               
*                                                                               
         IC    RF,DENAMLEN         BUMP TO NEXT ELEMENT                         
         LA    R4,0(RF,R4)                                                      
         B     HMHVSHPL            CONTINUE SEARCH                              
*                                                                               
HMHVSHPF DS    0H                                                               
*                                                                               
         IC    RF,DENAMLEN         GET EXECUTE LENGTH OF DESCRIPTION            
         SH    RF,=Y(DENAME-DENAMEL-1)                                          
         BM    HMHVSHSX            INVALID LENGTH                               
*                                                                               
         CH    RF,=Y(L'CONHEAD)    MAX L'CONHEAD                                
         BNH   *+8                                                              
         LH    RF,=Y(L'CONHEAD)                                                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD(0),DENAME   MOVE DESCRIPTION TO MESSAGE AREA             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    CONHEAD(0),=CL60' ' FORCE TO UPPERCASE                           
*                                                                               
HMHVSHSX DS    0H                                                               
*                                                                               
*        SWITCH BACK TO PRINT SYSTEM                                            
*                                                                               
         CLI   HCBSYST,HCBSYSRE    SKIP IF REP SYSTEM                           
         BE    HMHVSWR                                                          
*                                                                               
         L     RF,HCBACOM                                                       
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),HMWDMCB,=C'PRINT',0   SWITCH BACK TO PRINT SYSTEM           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     HMHVSWX                                                          
*                                                                               
*        SWITCH BACK TO REP   SYSTEM                                            
*                                                                               
HMHVSWR  DS    0H                                                               
*                                                                               
         L     RF,HCBACOM                                                       
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),HMWDMCB,=C'REP',0   SWITCH BACK TO REP SYSTEM               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HMHVSWX  DS    0H                                                               
*                                                                               
         OI    CONHEADH+6,X'88'    FORCE RE-DISPLAY/HIGH INTENSITY              
         MVI   MNERR,MNERRHPQ      HELP MESSAGE                                 
*                                                                               
HMHVSHPX DS    0H                                                               
*                                                                               
         B     HMHVSELX                                                         
*                                                                               
*        SELECTION PROCESSING OUTINE EXITS                                      
*                                                                               
HMHVSER1 DS    0H                  INVALID SELECTION CODE                       
*                                                                               
         MVC   CONHEAD,=CL60'INVALID SELECTION CODE'     ERR MSG                
*                                                                               
         B     HMHVSERX                                                         
*                                                                               
HMHVSERX DS    0H                  ERROR EXIT                                   
*                                                                               
         OI    CONHEADH+6,X'88'    FORCE RE-DISPLAY/HIGH INTENSITY              
         MVI   MNERR,MNERRISQ      INVALID SELECT                               
*                                                                               
         B     HMHVSELX                                                         
*                                                                               
HMHVSELX DS    0H                                                               
*                                                                               
         B     HMHOOKX                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PRHELP - PROCESS A SELECTION - HMHPROC'                         
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE TO PROCESS A SELECTION                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHPROC  DS    0H                                                               
*                                                                               
         LA    R8,HMHLPTBS         POINT TO TABLE ENTRY SAVEAREA                
         USING VLTAB,R8            ESTABLISH TABLE ENTRY                        
*                                                                               
         OC    HMWCHLIC,HMWCHLIC   SKIP IF SELECTION HAS CHILDREN               
         BNZ   HMHPROCA                                                         
*                                                                               
         L     R2,MNAFLD           POINT TO FIELD REQUESTING HELP               
         OI    6(R2),X'80'         FORCE RE-TRANSMISSION                        
         LA    R1,8(R2)            SET DEFAULT CURSOR POSITION                  
*                                                                               
         LA    RF,HMWFLD           SAVE ADDRESS OF DATA START                   
         ST    RF,HMWAFLD                                                       
*                                                                               
         C     RF,HMWOUTA          SKIP IF NOT BEFORE END OF DATA               
         BNL   HMHPRDN                                                          
*                                                                               
         L     RF,HMWOUTA          POINT TO NEXT OUTPUT POSITION                
         BCTR  RF,0                BACK UP 1 POSITION                           
*                                                                               
         CLI   0(RF),C','          ELIMINATE ANY TRAILING COMMA                 
         BE    *+8                                                              
         CLI   0(RF),C'/'          ELIMINATE ANY TRAILING SLASH                 
         BNE   *+12                                                             
         MVI   0(RF),C' '                                                       
         B     *+8                                                              
         LA    RF,1(RF)            RESET POINTER                                
*                                                                               
         ST    RF,HMWOUTA                                                       
*                                                                               
HMHPRLP  DS    0H                                                               
*                                                                               
         OI    6(R2),X'80'         FORCE RE-TRANSMISSION                        
         OI    6(R2),X'01'         FORCE RE-INPUT                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R2)            GET FIELD LENGTH                             
*                                                                               
         SH    RF,=H'8'            SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    1(R2),X'02'         IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         SH    RF,=H'8'               SUBTRACT EXTENSION LENGTH                 
*                                                                               
         LR    R0,RF               SAVE FIELD LENGTH                            
*                                                                               
         LA    R1,8(R2)            POINT TO START OF FIELD                      
*                                                                               
         TM    HTBCTL,HTBCSAMQ     IF ALL SELECTS IN SAME FIELD                 
         BNO   HMHPRSMX                                                         
*                                                                               
         CLI   0(R1),C'?'             FIND ? POSITION                           
         BE    *+8                                                              
         CLI   0(R1),C'+'             FIND + POSITION                           
         BE    *+12                                                             
         LA    R1,1(R1)               BUMP UP A POSITION                        
         BCT   RF,*-20                                                          
*                                                                               
HMHPRSMX DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT MAX DATA LENGTH FOR EX             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR OLD DATA                               
*                                                                               
         LA    RF,8(R2)            START OF FIELD                               
*                                                                               
         CR    R1,RF               DONE IF AT START OF FIELD                    
         BNH   HMHPROC1            ACTUAL START OF FIELD                        
*                                                                               
         CLI   0(R1),C' '          BACK UP TO LAST NON-BLANK                    
         BH    *+10                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         B     *-16                                                             
*                                                                               
         LA    R1,1(R1)            POINT TO FIRST OUTPUT POSITION               
*                                                                               
HMHPROC1 DS    0H                                                               
*                                                                               
         LA    RF,8(R2)            START OF FIELD                               
         LR    RE,R1               COPY END OF FIELD POINTER                    
         SR    RE,RF               CALCULATE LENGTH OF WHAT IS THERE            
*                                                                               
         L     RF,HMWOUTA          POINT TO END OF OUTPUT                       
*                                                                               
         L     R3,HMWAFLD          POINT TO START OF OUTPUT                     
*                                                                               
         TM    HTBCTL,HTBCMNYQ     IF SELECTS TO GO IN SEVERAL FLDS             
         BNO   HMHPMNYX                                                         
*                                                                               
         ICM   R5,15,HMWOUTA       END OF OUTPUT                                
         S     R5,HMWAFLD          MAX DATA LEFT                                
*                                                                               
         L     RF,HMWAFLD          POINT TO START OF OUTPUT DATA AREA           
*                                                                               
         CLI   0(RF),C','          FIND END OF FIELD                            
         BE    *+12                                                             
         LA    RF,1(RF)            BUMP TO NEXT POSITION                        
         BCT   R5,*-12                                                          
*                                                                               
         LA    R4,1(RF)            START OF NEXT FIELD SELECTED                 
*                                                                               
         ST    R4,HMWAFLD          RESET NEXT OUTPUT AREA START                 
*                                                                               
HMHPMNYX DS    0H                                                               
*                                                                               
         SR    RF,R3               LENGTH OF OUTPUT TO BE DISPLAYED             
         BZ    HMHPROC9            SKIP IF NONE                                 
*                                                                               
         LA    R4,0(RE,RF)         NEW LENGTH OF FIELD                          
*                                                                               
         CR    R4,R0               MUST NOT BE LONGER THAN MAX LENGTH           
         BNH   *+6                                                              
         LR    R4,R0               DEFAULT TO MAX LENGTH                        
*                                                                               
         STC   R4,5(R2)            SET OUTPUT/INPUT LENGTH                      
*                                                                               
         SR    R4,RE               MAX ROOM LEFT ON LINE                        
*                                                                               
         CR    RF,R4               DON'T MOVE MORE THAN THERE IS ROOM           
         BNH   *+6                                                              
         LR    RF,R4                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)       DISPLAY SELECTIONS                           
*                                                                               
         LA    R1,1(RF,R1)         NEW CURSOR POSITION                          
*                                                                               
         CR    RF,R4               IF FIELD FILLED                              
         BL    *+8                                                              
         LA    R1,8(R2)               POINT TO START OF FIELD                   
*                                                                               
HMHPROC9 DS    0H                                                               
*                                                                               
HMHPRCN  DS    0H                                                               
*                                                                               
         TM    HTBCTL,HTBCMNYQ     IF SELECTS IN CONSECUTIVE FIELDS             
         BNO   HMHPRDN                                                          
*                                                                               
         CLC   HMWAFLD,HMWOUTA     DONE IF NO MORE TO DISPLAY                   
         BNL   HMHPRDN                                                          
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R2,RF                                                            
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         BNO   *+10                                                             
         IC    RF,0(R2)               KEEP GOING TO NEXT FIELD                  
         AR    R2,RF                                                            
*                                                                               
         CLI   0(R2),0             DONE IF END OF SCREEN REACHED                
         BNE   HMHPRLP                                                          
*                                                                               
HMHPRDN  DS    0H                                                               
*                                                                               
         L     RF,HCBATIOB         POINT TO TIOB                                
         USING TIOBD,RF            ESTABLISH TIOB                               
*                                                                               
         SR    R1,R2               GET CURSOR DISPLACEMENT IN FIELD             
         SH    R1,=H'8'                                                         
         BM    *+8                 TAKING NO CHANCES                            
         STC   R1,TIOBCURI         SAVE                                         
*                                                                               
         SR    R2,RA                                                            
         STCM  R2,3,TIOBCURD       SET DISP OF CURSOR FIELD                     
*                                                                               
         OI    TIOBINDS,TIOBSETC   SET TO SET CURSOR                            
*                                                                               
HMHPROCA DS    0H                                                               
*                                                                               
         OI    HCBRTRN,HCBRSELQ    INDICATE SELECTION MADE                      
*                                                                               
         MVI   HMMENUSW,0          CLEAR MENU SWITCH                            
*                                                                               
         MVC   CONHEAD,=CL60'PLEASE ENTER REMAINDER OF REQUEST AND HIT X        
               ENTER'                                                           
         OI    CONHEADH+6,X'80'    FORCE TRANSMISSION OF MESSAGE                
*                                                                               
HMHPROCX DS    0H                                                               
*                                                                               
         B     HMHOOKX                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         EJECT                                                                  
HMHOOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
SPACES   DC    CL80' '             SPACES                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE   'PRHELP DSECTS - HELPCB'                                       
*                                                                               
       ++INCLUDE PRHELPCB                                                       
*                                                                               
         TITLE 'PRHELP - WORKING STORAGE - HMWORK'                              
***********************************************************************         
*                                                                     *         
*        HLPMENU WORKING STORAGE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMWORKD  DSECT                                                                  
*                                                                               
HMWRELO  DS    A                   RELOCATION FACTOR                            
VDATCON  DS    A                   A(DATCON)                                    
VDDVAL   DS    A                   A(DDVAL)                                     
VCALLOV  DS    A                   A(CALLOV)                                    
*                                                                               
HMWMNSVA DS    A                   A(MENU SAVEAREA IN TWA)                      
*                                                                               
HMWDMCB  DS    6A                  WORKING PARAMETER LIST                       
*                                                                               
HMWPARMS DS    0A                  PARAMETER SAVEAREA                           
HMWDTICA DS    A                   A(HELP MENU ICODE)                           
HMWFLDA  DS    A                   A(FIELD INVOKING HELP)                       
HMWCBLKA DS    A                   A(HELP MENU CONTROL BLOCK)                   
HMWPARML EQU   *-HMWPARMS          PARAMETER LIST LENGTH                        
*                                                                               
HMWDTIC  DS    XL2                 HELP MENU ICODE                              
HMWSELIC DS    XL2                 SELECTED ICODE                               
HMWCHLIC DS    XL2                 SELECTED CHILDREN'S ICODE                    
*                                                                               
HMWAFLD  DS    A                   A(OUTPUT BUILD AREA)                         
HMWFLD   DS    XL80                OUTPUT BUILD AREA                            
HMWOUTA  DS    A                   A(NEXT AVAILABLE OUTPUT POSITION)            
HMWOUTSV DS    A                   SAVEAREA FOR A(NXT OUT POSITION)             
HMWMENUC DS    XL(MNBLKL)          DDMENU CONTROL BLOCK                         
*                                                                               
HMW1STSW DS    X                   1ST TIME TO DISP ROUTINE SWITCH              
HMW1STQ  EQU   X'80'               BEEN TO DISP ROUTINE AT LEAST ONCE           
*                                                                               
HMWKEY   DS    XL64                KEY BUILD AREA                               
HMWKEYSV DS    XL64                KEY SAVE  AREA                               
HMWDMWRK DS    XL256               DATAMGR WORK AREA                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DDVAL PARAMETER BLOCK                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE PRVALPARMS                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
HMWIO    DS    XL1024              IOAREA                                       
*                                                                               
HMWORKL  EQU   *-HMWORKD           WORKING STORAGE LENGTH                       
*                                                                               
         TITLE   'PRHELP DSECTS - MENUBLK'                                      
***********************************************************************         
*                                                                     *         
*        DDMENU CONTROL BLOCK                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE DDMENUBLK                                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        EXTENSION TO DDMENU TABLE ENTRY                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MNVALENT DSECT                                                                  
         ORG   MNVUSER                                                          
MNVOPTIC DS    XL2                 OPTION ICODE                                 
MNVCHLIC DS    XL2                 OPTION CHILDREN'S ICODE                      
MNVOPTL  DS    XL1                 OPTION LENGTH                                
MNVHDRL  EQU   *-MNVUSER           OPTION HEADER LENGTH                         
MNVOPT   DS    0X                  OPTION                                       
*                                                                               
         TITLE   'PRHELP DSECTS - TWA'                                          
*                                                                               
       ++INCLUDE PRWRIFFD                                                       
       EJECT                                                                    
       ++INCLUDE DDGENTWA                                                       
*                                                                               
       EJECT                                                                    
***********************************************************************         
*                                                                     *         
*        EXTENSION TO TWA DSECT                                       *         
*        SAVEAREA FOR DDMENU TABLE - GRAB ALL SPACE AVAILABLE         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T405FFD  DSECT                                                                  
         ORG                                                                    
HMMENUSV DS    0X                  HELP MENU SAVEAREA                           
*                                                                               
HMSFLDA  DS    AL4                 SELECTED FIELD DISPLACEMENT                  
HMSFLDCD DS    XL2                 DISPLACEMENT OF CURSOR IN FIELD              
*                                                                               
HMMENUSW DS    X                   DDMENU ACTIVE SWITCH                         
HMMENUQ  EQU   X'FF'               IN MIDDLE OF A MENU SELECT                   
*                                                                               
HMMTIC   DS    XL2                 CURRENT MASTER ICODE                         
*                                                                               
HMCNTL   DS    XL1                 CURRENT CONTROL FIELD                        
         DS    XL30                SPARE                                        
*                                                                               
HMCBLKSV DS    XL(HELPCBL)         HELP CONTROL BLOCK SAVE                      
*                                                                               
HMHLPTBS DS    XL1024              HELP MENU TABLE ENTRY SAVE AREA              
*                                                                               
HMMENUTB DS    (14*1024-(*-T405FFD))XL1 DDMENU TABLE                            
*                                                                               
HMMENUSX EQU   *-1                 END OF SAVEAREA                              
*                                                                               
         TITLE   'PRHELP DSECTS - VALTABD'                                      
*                                                                               
       ++INCLUDE PRVALTABD                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
       EJECT                                                                    
***********************************************************************         
*                                                                     *         
*        EXTENSION TO VALTAB DSECT FOR HELP MENU TABLE ENTRIES        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         ORG   VLTEXTRA                                                         
HTBXHDR  DS    0X                  EXTRA DATA FOR A HELP MENU TABLE             
HTBLINE  DS    XL1                 START LINE FOR MENU                          
HTBSELWD DS    XL1                 WIDTH OF SELECT FIELD                        
HTBTTLSN DS    XL1                 NUMBER OF TITLES                             
HTBTTLSL DS    XL1                 LENGTH OF TITLE LINE                         
HTBCOLN  DS    XL1                 NUMBER OF COLUMNS                            
HTBCTL   DS    XL1                 CONTROL BYTE                                 
HTBCMNYQ EQU   X'80'               MULTIPLE SELECTIONS ALLOWED                  
HTBCSAMQ EQU   X'40'               PLACE MULTIPLE SELECTS IN SAME FIELD         
HTBCSLAQ EQU   X'20'               SEPARATE MULTIPLES WITH SLASH                
         DS    XL2                 SPARE                                        
*                                                                               
HTBCHLIC DS    XL2                 ICODE OF CHILDREN'S TABLE                    
HTBCNTL  DS    XL1                 CONTROL BYTE                                 
HTBCHLPQ EQU   X'80'               CHILD IS ANOTHER HELP MENU                   
HTBFVAL  DS    XL1                 FILTER VALUE FOR SEARCH                      
*                                                                               
HTBCHDIC DS    XL2                 CHILDREN'S TABLE ICODE                       
         DS    XL10                SPARE                                        
*                                                                               
HTBXHDRL EQU   *-HTBXHDR           LENGTH OF FIXED PORTION OF EXTRA             
*                                                                               
HTBTTLS  DS    0X                  START OF TITLE LINES                         
*                                                                               
         TITLE   'PRHELP DSECTS - HIDDEN'                                       
*                                                                               
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
* PRWRIEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRWRIEQUS                                                      
         PRINT ON                                                               
* RENWREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE RENWREQUS                                                      
         PRINT ON                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
* CTGENDIC                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENDIC                                                       
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112PRHELP    05/11/05'                                      
         END                                                                    
