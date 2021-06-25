*          DATA SET PRSFM0F    AT LEVEL 003 AS OF 11/16/93                      
*PHASE T41C0FB,* =*=*=*=*=*= PHASE B =*=*=*=*=*=*=*=*=*                         
*INCLUDE TWABLD                                                                 
*INCLUDE MENU                                                                   
*                                                                               
*        TITLE   PRSFM0F - DDMENU INTERFACE                                     
         TITLE  'PRSFM0F - DDMENU INTERFACE'                                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO INTERFACE WITH DDMENU                             *         
*        RESULT IS A MENU OF VALID ENTRIES FROM WHICH TO SELECT       *         
*                                                                     *         
*NTRY    P1 ==> A(HLPMNUCB) - CONTROL BLOCK                           *         
*        P2 ==> A(FIELD REQUESTING HELP MENU)                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
T41C0F   CSECT                                                                  
         NMOD1 HMWORKL,**T41C0F,RR=RE,CLEAR=YES                                 
*                                                                               
         USING HMWORKD,RC          ESTABLISH ROUTINE'S STORAGE                  
*                                                                               
         ST    RE,HMWRELO          SAVE RELOCATION FACTOR                       
*                                                                               
         USING T41CFFD,RA                                                       
*                                                                               
         MVC   HMWPARMS,0(R1)      SAVE PARAMETER LIST                          
*                                                                               
         L     R4,HMWCBLKA         ESTABLISH HELP MENU CONTROL BLOCK            
         USING HLPMNUCB,R4                                                      
*                                                                               
         L     R2,HMWFLDA          POINT TO FIELD REQUESTING HELP               
*                                                                               
         LA    R6,HMWMENUC         ESTABLISH DDMENU CONTROL BLOCK               
         USING MNBLKD,R6                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE DDMENU CONTROL BLOCK                              *         
*        BUILD DDMENU TABLE                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
HMNINIT  DS    0H                                                               
*                                                                               
         XC    HMFLD,HMFLD         INIT OUTPUT BUILD AREA                       
         LA    RF,HMFLD            INIT OUTPUT POINTER                          
         ST    RF,HMOUTA                                                        
*                                                                               
         L     RF,HMAADDRS         POINT TO EXTERNAL ADDRESSES                  
         USING HMADDRD,RF          ESTABLISH THEM                               
*                                                                               
         XC    MNBLKD(MNBLKL),MNBLKD  INIT DDMENU CONTROL BLOCK                 
         MVC   MNATWA,HMATWA       PASS A(TWA)                                  
         MVC   MNATIOB,HMATIOB     PASS A(TIOB)                                 
         MVC   MNACOM,HMACOM       PASS A(COMFACS)                              
         DROP  RF                                                               
         L     RF,=V(TWABLD)       GET A(TWABLD)                                
         A     RF,HMWRELO          RELOCATE ADDRESS                             
         ST    RF,MNATWBLD         PASS A(TWABLD)                               
         L     RF,=A(HMNHOOK)                                                   
         A     RF,HMWRELO          RELOCATE ADDRESS                             
         ST    RF,MNHOOK           PASS A(HOOK)                                 
         L     RF,=AL4(HMMENUTB-T41CFFD)   DISPLACEMENT OF MENU TABLE           
         AR    RF,RA               ADDRESS OF MENU TABLE                        
         ST    RF,MNVALTAB         PASS A(DDMENU TABLE)                         
         LA    RF,HMCTTLS          PASS A(MENU TITLES)                          
         STCM  RF,7,MNATITLE                                                    
         MVC   MNATITLN,HMCTTLSN   NUMBER OF TITLES                             
         MVI   MNSAVEPG,1          SAVE THINGS IN 1ST TEMPSTR                   
         MVI   MNSTRTLN,7          START MENU ON LINE 7                         
         MVC   MNNCOLS,HMCCOLN     SET NUMBER OF COLUMNS                        
         MVI   MNSELWID,3          WIDTH OF SELECT FIELD                        
         MVI   MNCNTL,MNVREPQ+MNMULTQ  REPLACE ? WITH SELECT                    
*                                    AND ALLOW MULTIPLE SELECTS                 
         MVI   MNPFUP,PRPFKUP      SET SCROLLING PF KEYS - UP                   
         MVI   MNPFDN,PRPFKDWN     SET SCROLLING PF KEYS - DOWN                 
         MVI   MNPFTOP,5           SET SCROLLING PF KEYS - TOP                  
         MVI   MNPFBOT,6           SET SCROLLING PF KEYS - BOTTOM               
         MVI   MNPFSEL,4           SET SCROLLING PF KEYS - SELECT               
         MVC   MNSCRID,HMCSCRN     SET SCREEN NUMBER FOR HELP                   
*                                                                               
         OC    HMCMTIC,HMCMTIC     INIT ONLY ON FIRST CALL                      
         BNZ   HMNINITN                                                         
*                                                                               
*        RETURN TO DDMENU IF IN THE MIDDLE OF A SELECT                          
*                                                                               
         L     R2,=AL4(HMSFLDA-T41CFFD)   DISPLACEMENT OF FIELD                 
         AR    R2,RA               ADDRESS OF SELECT FIELD SAVEAREA             
         L     R2,0(R2)            RETRIEVE A(FIELD INVOKING HELP)              
         AR    R2,RA               RELOCATE ADDRESS                             
         ST    R2,MNAFLD           PASS     A(FIELD INVOKING HELP)              
*                                                                               
         L     RF,=AL4(HMMENUSW-T41CFFD)   DISPLACEMENT OF MENU SWITCH          
         AR    RF,RA               ADDRESS OF MENU SWITCH                       
*                                                                               
         CLI   0(RF),HMMENUQ       IF MENU BEING PROCESSED                      
         BNE   HMNMENUX                                                         
*                                                                               
         L     R1,MNVALTAB         POINT TO VALUE TABLE                         
         USING MNVALTBD,R1         ESTABLISH TABLE HEADER                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MNVFRSTF       POINT TO FIRST SELECT FIELD                  
         BZ    *+10                SKIP IF NOT MENU SCREEN                      
         AR    R1,RA                                                            
         OI    6(R1),X'40'         PLACE CURSOR AT THIS FIELD                   
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 =V(MENU),DMCB,MNBLKD,RR=HMWRELO  RETURN TO MENU                  
*                                                                               
         B     HMNOUT              DISPLAY SCREEN                               
*                                                                               
HMNMENUX DS    0H                                                               
*                                                                               
         L     R2,MNVALTAB         ESTABLISH DDMENU TABLE                       
         USING MNVALTBD,R2                                                      
*                                                                               
         XC    MNVHEAD,MNVHEAD     INIT TABLE HEADER                            
*                                                                               
         LA    R2,L'MNVHEAD(R2)    BUMP TO FIRST ENTRY IN TABLE                 
         USING MNVALENT,R2         ESTABLISH TABLE ENTRY                        
*                                                                               
         XC    MNVALENT(MNVOVERQ),MNVALENT  INIT FIRST ENTRY IN TABLE           
*                                                                               
HMNINITX DS    0H                                                               
*                                                                               
         B     HLPMENUX            EXIT                                         
*                                                                               
HMNINITN DS    0H                  NOT MENU INITIALIZATION                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INIT VLBLOCK FOR DDVAL                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMNVAL   DS    0H                                                               
*                                                                               
         LA    R5,VLPARMS          ESTABLISH DDVAL BLOCK                        
         USING VLPARMS,R5                                                       
*                                                                               
         ST    R2,MNAFLD           PASS A(FIELD INVOKING HELP)                  
*                                                                               
         L     RF,=AL4(HMSFLDA-T41CFFD)   DISPLACEMENT OF FIELD                 
         AR    RF,RA               ADDRESS OF SELECT FIELD SAVEAREA             
         SR    R2,RA               DISPLACEMENT OF FIELD                        
         ST    R2,0(RF)            SAVE A(FIELD INVOKING HELP)                  
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR BLOCK                            
         MVC   VLACFACS,ACOMFACS   A(COMFACS)                                   
         MVC   VLCTRY,CTRY         SET COUNTRY CODE                             
         MVC   VLLANG,LANG         LANGUAGE CODE                                
         MVC   VLAGENCY,AGENCY     AGENCY                                       
         MVI   VLSYSTEM,VLSYSPRQ                                                
         MVI   VLFLT4V,16          SHOW ONLY DOCUMENTED OPTIONS                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,VLTODAYC)                              
*                                                                               
*        BUILD SORTED TABLE OF OPTIONS                                          
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPTABQ+VLPSRTQ',HMCMTIC),             X        
               0,('HMWSEGS',HMWVALTB),0,0,0                                     
*                                                                               
         CLI   VLPERR,0            SKIP IF ERRORS                               
         BE    *+6                                                              
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
HMNVALX  DS    0H                                                               
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         LA    RF,HMWVALTB         ESTABLISH RETURNED DDVAL TABLE               
         USING VLTABD,RF                                                        
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
*        FOR EACH ENTRY IN VALTAB CREATE ONE IN MENUTAB                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
HMNLOOP  DS    0H                                                               
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
         LA    RF,2+MNVOVERQ(RF)   ADD ON OVERHEAD LENGTH                       
         STC   RF,MNVALLEN         SET ENTRY LENGTH                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,VLTICODE       GET INTERNAL ICODE                           
         LA    RE,100(RE)          ADD 100 FOR NOW                              
         STCM  RE,1,MNVSELID       USE AS HELP MSG NUMBER                       
*                                                                               
         LA    R2,0(RF,R2)         BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         XC    MNVALENT(MNVOVERQ),MNVALENT  INIT NEXT ENTRY IN TABLE            
*                                                                               
HMNCONT  DS    0H                                                               
*                                                                               
         BXLE  R3,R4,HMNLOOP       LOOP THROUGH VALTAB                          
*                                                                               
HMNDONE  DS    0H                                                               
*                                                                               
         GOTO1 =V(MENU),DMCB,MNBLKD,RR=HMWRELO  DDMENU                          
*                                                                               
         NOP   *+4                 TESTING                                      
*                                                                               
HMNOUT   DS    0H                  EXIT TO FACPAK TO WRITE SCREEN               
         L     RF,HMAADDRS         POINT TO EXTERNAL ADDRESSES                  
         L     RD,HMASYSRD         SET TO EXIT IMMEDIATELY                      
*                                                                               
HLPMENUX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRSFM0F - DDMENU INTERFACE - HLPHOOK'                           
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE FROM DDMENU                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HMNHOOK  NTR1                                                                   
*                                                                               
***      =================  ***                                                 
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
         USING T41CFFD,RA                                                       
***      =================  ***                                                 
         USING HMWORKD,R7          ESTABLISH ROUTINE'S STORAGE                  
         USING MNBLKD,R6           ESTABLISH DDMENU CONTROL BLOCK               
*                                                                               
*        DETERMINE HOOK MODE                                                    
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
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE TO DISPLAY A MENU TABLE ENTRY         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHDISP  DS    0H                                                               
*                                                                               
         L     RF,=AL4(HMMENUSW-T41CFFD)   DISPLACEMENT OF MENU SWITCH          
         AR    RF,RA               ADDRESS OF MENU SWITCH                       
         MVI   0(RF),HMMENUQ       INDICATE DDMENU CALLED                       
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
         EJECT                                                                  
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
         L     R2,HMOUTA           POINT TO NEXT OUTPUT POSITION                
*                                                                               
         CLC   MNVSELVL,SPACES     SKIP IF NOT SELECTED                         
         BNH   HMHVSELX                                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,MNVOPTL        LENGTH OF SELECTED OPTION                    
         BZ    HMHVSELX            MUST HAVE A LENGTH                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),MNVOPT      DISPLAY OPTION                               
*                                                                               
         LA    R2,1(RF,R2)         BUMP TO NEXT AVAILBLE SPACE                  
         MVI   0(R2),C','          SEPARATING COMMA                             
         LA    R2,1(R2)                                                         
*                                                                               
         ST    R2,HMOUTA           UPDATE OUTPUT POSITION                       
*                                                                               
HMHVSELX DS    0H                                                               
*                                                                               
         B     HMHOOKX                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        HELP MENU HOOK ROUTINE TO PROCESS A SELECTION                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMHPROC  DS    0H                                                               
*                                                                               
         L     R2,MNAFLD           POINT TO FIELD INVOKING HELP                 
         OI    6(R2),X'C0'         FORCE FIELD TRANSMISSION & CURSOR            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R2)            GET FIELD LENGTH                             
*                                                                               
         SH    RF,=H'8'            SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    1(R2),X'02'         IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         SH    RF,=H'8'            SUBTRACT EXTENSION LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT MAX DATA LENGTH FOR EX             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR OLD DATA                               
*                                                                               
         L     RF,HMOUTA           POINT TO NEXT OUTPUT POSITION                
         BCTR  RF,0                BACK UP 1 POSITION                           
*                                                                               
         CLI   0(RF),C','          ELIMINATE ANY TRAILING COMMA                 
         BNE   *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         LA    RF,1(RF)            RESET POINTER                                
*                                                                               
         LA    R3,HMFLD            POINT TO START OF OUTPUT AREA                
*                                                                               
         SR    RF,R3               LENGTH OF OUTPUT AREA                        
         BZ    HMHPROC9            SKIP IF NONE                                 
*                                                                               
         STC   RF,5(R2)            SET OUTPUT/INPUT LENGTH                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),HMFLD       DISPLAY SELECTIONS                           
*                                                                               
HMHPROC9 DS    0H                                                               
*                                                                               
         L     RF,=AL4(HMMENUSW-T41CFFD)   DISPLACEMENT OF MENU SWITCH          
         AR    RF,RA               ADDRESS OF MENU SWITCH                       
         MVI   0(RF),0             CLEAR MENU SWITCH                            
*                                                                               
         MVC   CONHEAD,=CL60'PLEASE ENTER REMAINDER OF SCREEN AND HIT EX        
               NTER'                                                            
         OI    CONHEADH+6,X'80'    FORCE TRANSMISSION OF MESSAGE                
*                                                                               
HMHPROCX DS    0H                                                               
*                                                                               
         B     HMHOOKX                                                          
*                                                                               
         EJECT                                                                  
HMHOOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM0F - HELPMENU CONTROL BLOCK - PRHLPMNUCB'                  
       ++INCLUDE PRHLPMNUCB                                                     
         TITLE 'PRSFM0F - WORKING STORAGE - HMWORK'                             
***********************************************************************         
*                                                                     *         
*        HLPMENU WORKING STORAGE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HMWORKD  DSECT                                                                  
*                                                                               
HMWRELO  DS    A                   RELOCATION FACTOR                            
*                                                                               
HMWPARMS DS    0XL8                PARAMETER SAVEAREA                           
HMWCBLKA DS    A                   A(HELP MENU CONTROL BLOCK)                   
HMWFLDA  DS    A                   A(FIELD INVOKING HELP)                       
*                                                                               
HMFLD    DS    XL80                OUTPUT BUILD AREA                            
HMOUTA   DS    A                   A(NEXT AVAILABLE OUTPUT POSITION)            
HMWMENUC DS    XL(MNBLKL)          DDMENU CONTROL BLOCK                         
*                                                                               
HMW1STSW DS    X                   1ST TIME TO DISP ROUTINE SWITCH              
HMW1STQ  EQU   X'80'               BEEN TO DISP ROUTINE AT LEAST ONCE           
*                                                                               
HMWVALTB DS    XL(HMWSEGS*256)     DDVAL  TABLE                                 
*                                                                               
HMWSEGS  EQU   20                  # OF 256 BYTE SEGMENTS IN TABLE              
*                                                                               
       ++INCLUDE PRVALPARMS                                                     
*                                                                               
HMWORKL  EQU   *-HMWORKD           WORKING STORAGE LENGTH                       
         TITLE   'PRSYSDRIVE DSECTS / STORAGE'                                  
       ++INCLUDE DDMENUBLK                                                      
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*        EXTENSION TO DDMENU TABLE ENTRY                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MNVALENT DSECT                                                                  
         ORG MNVUSER                                                            
MNVOPTL  DS    XL1                 OPTION LENGTH                                
MNVOPT   DS    0X                  OPTION                                       
         EJECT                                                                  
       ++INCLUDE PRVALTABD                                                      
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*        INCLUDE PRSFMFFD                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DDCOREQUS                                                      
*        INCLUDE DDPERVALD                                                      
*        INCLUDE PRGLOBEQUS                                                     
         PRINT OFF                                                              
       ++INCLUDE PRSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PRGLOBEQUS                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
       EJECT                                                                    
***********************************************************************         
*                                                                     *         
*        EXTENSION TO TWA DSECT                                       *         
*        SAVEAREA FOR DDMENU TABLE - GRAB ALL SPACE AVAILABLE         *         
*                                                                     *         
***********************************************************************         
T41CFFD  DSECT                                                                  
         ORG                                                                    
HMMENUSW DS    X                   DDMENU ACTIVE SWITCH                         
HMSFLDA  DS    A                   SELECTED FIELD ADDRESS                       
HMMENUQ  EQU   X'FF'               IN MIDDLE OF A MENU SELECT                   
*                                                                               
HMMENUTB DS    XL(TWAMXLEN-TWAENDLQ)   DDMENU TABLE                             
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PRSFM0F   11/16/93'                                      
         END                                                                    
