*          DATA SET SPNWS02    AT LEVEL 023 AS OF 02/26/07                      
*PHASE T20702C,*                                                                
         TITLE 'NWS02 T20702  BUYER''S WORK SHEET - BUYER OVERLAY'              
T20702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20702**,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING BYRRECD,R2                                                       
*                                                                               
         CLI   APMODE,APMVALK        VALIDATE KEY                               
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR        VALIDATE RECORD                            
         BE    VALREC                                                           
         CLI   APMODE,APMDISK        DISPLAY KEY                                
         BE    DISKEY                                                           
         CLI   APMODE,APMDISR        DISPLAY RECORD                             
         BE    DISREC                                                           
         CLI   APMODE,APMDELR        DELETE RECORD                              
         BE    DELREC                                                           
         CLI   APMODE,APMRESR        RESTORE RECORD                             
         BE    RESREC                                                           
         CLI   APMODE,APMVALP        VALIDATE KEY FOR LIST/SELECT               
         BE    VALPAR                                                           
         CLI   APMODE,APMGETS        GET RECORD FOR LIST/SELECT                 
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS        DISPLAY RECORD FOR LIST/SELECT             
         BE    DISSEL                                                           
         CLI   APMODE,APMLSCR        LAST FOR SCREEN                            
         BE    LSTSCR                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   XC    IOKEY,IOKEY         SET UP THE KEY                               
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         XC    BYRMDN,BYRMDN                                                    
         OI    BYRMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,BYRMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BYRKAGMD,BAGYMD                                                  
         MVC   BYRMDN,MEDNM                                                     
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BYRBYRH       VALIDATE BUYER                               
         BNE   VALKX                                                            
         MVC   BYRKBYR,FVIFLD                                                   
*                                                                               
         MVC   APRECKEY,IOKEY      SET KEY                                      
         GOTO1 AIO,DIRRDD+IO1      READ THE DIRECTORY POINTER                   
         BL    VALKX               (HARD IO ERROR)                              
         BAS   RE,IOCHECK                                                       
         BNE   VALKX                                                            
         MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
         CLI   APACTN,ACTADD                                                    
         BE    VALKX                                                            
         TM    APINDS,APIOKADD                                                  
         BO    VALKX                                                            
         LA    R1,FILGET1                                                       
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET THE RECORD                               
         BNE   VALKX                                                            
         MVC   APRECDA,IODA                                                     
         L     R2,AIOAREA1                                                      
         MVC   BBYR,BYRCODE        SAVE THE BUYER CODE                          
         MVC   BBYRMASK,BYRMASK    COPY THE AGY/MED MASK                        
         MVC   BYRPW,BYRPASS       SET THE PASSWORD                             
         OC    BYRPW,BYRPW         IF BUYER HAS A PASSWORD,                     
         BZ    VALKX                                                            
         TM    TWAMODE,TWAMLSM     AND LIST/SELECT IS ACTIVE,                   
         BZ    VALKX                                                            
         TM    SVPWIND,SVPWVALD    AND PASSWORD WAS NOT VALID,                  
         BO    VALKX                                                            
         CLC   BYRPW,INFPWD        AND (PASSWORD STILL NOT VALID                
         BNE   *+12                                                             
         CLI   BYRPASH+5,0              OR PASSWORD NOW VALID, BUT              
         BNE   VALKX                       NO NEW PASSWORD ENTERED),            
         MVI   APMODE,APMPFKS      THEN QUIT BACK TO THE LIST SCREEN.           
         MVI   SCPFKEY,PFK12                                                    
         B     VALKX                                                            
*                                                                               
VALKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         BUILD BUYER RECORD                           
         XC    BYRRECD(256),BYRRECD                                             
         MVC   BYRKEY,APRECKEY     KEY                                          
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           DESCRIPTION ELEMENT                          
         USING BYREL,R3                                                         
         MVI   BYRELCD,BYRELCDQ                                                 
         MVI   BYRELLN,BYRELLNQ                                                 
         MVI   FVMAXL,L'BYRNAME                                                 
         GOTO1 AFVAL,BYRNAMH       VALIDATE THE NAME                            
         BH    VALRX                                                            
         MVC   BYRNAME,FVIFLD                                                   
*                                                                               
*****  NEED TO CHECK IF WE NEED TO GO TO SFM BUYER RECORDS!!                    
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(4),=C'S0MK'   NEED TO CHECK MK PROFILE!!                   
         MVC   IOKEY+4(2),CUAALF   AGENCY ALPHA                                 
         MVC   IOKEY+6(1),BAGYMD                                                
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK   TEST PROFILE FOUND                           
         BZ    VALR00               - NOPE, NO MORE CHECKING                    
         CLI   APWORK+4,C'Y'       BUYER POPULATION?                            
         BNE   VALR00               - NOPE, CONTINUE                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0DE4'   PASSIVE BUYER KEY                            
         MVC   IOKEY+2(1),BAGYMD                                                
         NI    IOKEY+2,X'F0'       MEDIA MUST BE 0                              
         MVC   IOKEY+3(10),BYRNAME   WE'RE ONLY USE FIRST 10 BYTES              
*        MVC   BYRPTYP3,=X'0DE4'   PASSIVE BUYER KEY                            
*        MVC   BYRPAGY3,BAGYMD                                                  
*        MVC   BYRPBUBL,BYRNAME    WE'RE ONLY GONNA USE FIRST 10 BYTES          
*                                                                               
         LA    R1,DIRHI+IO3                                                     
         GOTO1 AIO                                                              
         L     R2,AIOAREA1         RESTORE R2                                   
*                                                                               
         CLC   IOKEY(13),IOKEYSAV   DOES IT EXISTS IN SFM??                     
         BE    VALR00               - YUP, EVERYTHING IS GOOD TO GO             
****   BUYER NAME DOES NOT EXIST IN SFM BUYER RECORD, ERROR!!                   
         MVC   FVMSGNO,=AL2(FVSFMBYR)   BUYER NAME NOT FOUND IN...              
         LA    R1,BYRNAMH               ...SFM BUYER RECORDS!!                  
         ST    R1,FVADDR                                                        
         B     VALRX                                                            
*****                                 MHC  09/16/03                             
*                                                                               
VALR00   CLI   APACTN,ACTCHA       CHECK FOR ACTION CHANGE                      
         BNE   VALR2                                                            
         TM    SVPWIND,SVPWVALD    YES - CHECK PW ALREADY VALID                 
         BO    VALR2                                                            
         OC    BYRPW,BYRPW         CHECK FOR EXISTENT PASSWORD                  
         BZ    VALR2                                                            
         OC    INFPWD,INFPWD       CHECK FOR DEFAULT PASSWORD                   
         BZ    *+14                                                             
         CLC   BYRPW,INFPWD                                                     
         BE    VALR2                                                            
         GOTO1 AFVAL,BYRPASH       VALIDATE PASSWORD                            
         BL    VALR9               NO PASSWORD ENTERED                          
         BH    VALRX                                                            
         CLC   BYRPW,FVIFLD        COMPARE PASSWORDS                            
         BNE   VALR9               NOT EQUAL - ERROR                            
*                                                                               
VALR2    MVI   FVMAXL,L'BYRPASS                                                 
         XC    BYRPW,BYRPW                                                      
         GOTO1 AFVAL,BYRPASH       VALIDATE THE PASSWORD                        
         BH    VALRX                                                            
         BL    VALR4                                                            
         CLC   FVIFLD,SPACES                                                    
         BE    VALR4                                                            
         MVC   BYRPASS,FVIFLD                                                   
         MVC   BYRPW,FVIFLD                                                     
         XC    BYRPAS,BYRPAS                                                    
         OI    BYRPASH+6,FVOXMT                                                 
*                                                                               
VALR4    MVC   BYRCODE,BBYR        BUYER CODE                                   
         MVC   BYRMASK,BBYRMASK                                                 
         CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALR6                                                            
         XC    IOKEY,IOKEY         YES-BUILD PASSIVE KEY                        
         LA    R2,IOKEY                                                         
         MVI   BYRPTYP,BYRPTYPQ                                                 
         MVI   BYRPSUB,BYRPSUBQ                                                 
         MVC   BYRPAGMD,BAGYMD     FOR 1ST 255 BUYERS                           
         MVI   BBYRMASK,0                                                       
*                                                                               
VALR4A00 GOTO1 AIO,DIRHI                                                        
         LA    RE,255                                                           
         CLC   IOKEY(3),IOKEYSAV   TEST FIRST BUYER                             
         BNE   VALR5               YES                                          
*                                                                               
         ZIC   RE,BYRPBYR                                                       
         BCTR  RE,0                NEXT BUYER                                   
*                                                                               
         LTR   RE,RE               WAS LAST BINARY BUYER = 1?                   
         BP    VALR5               NO                                           
*                                                                               
VALR4A10 ZIC   R0,BYRPBYR          R0 = LAST BINARY BUYER WE SAW                
         GOTO1 AIO,DIRSQ                                                        
*                                                                               
         CLC   IOKEY(3),IOKEYSAV   IF NO MORE CODES                             
         BNE   VALR4A20            THEN RAN OUT                                 
*                                                                               
         ZIC   R1,BYRPBYR                                                       
         SR    R1,R0                                                            
         CH    R1,=H'1'            IS THERE A GAP?                              
         BNH   VALR4A10            NO, CHECK NEXT BINARY BUYER CODE             
         LR    RE,R0                                                            
         LA    RE,1(RE)            YES, WE HAVE ONE                             
         B     VALR5                                                            
*                                                                               
VALR4A20 TM    IOKEYSAV+2,X'08'    GONE PAST 255 BUYERS ALREADY?                
         BNZ   VALRROUT                                                         
         XC    IOKEY,IOKEY         YES-BUILD PASSIVE KEY WITH MASK              
         MVI   BYRPTYP,BYRPTYPQ                                                 
         MVI   BYRPSUB,BYRPSUBQ                                                 
         MVC   BYRPAGMD,BAGYMD                                                  
         OI    BYRPAGMD,X'08'      FOR NEXT 255 BUYERS                          
         MVI   BBYRMASK,X'08'      REMEMBER THIS AS WELL                        
         B     VALR4A00                                                         
*                                                                               
VALRROUT MVC   FVMSGNO,=AL2(FV255BYR)   RAN OUT OF BUYER CODES                  
         LA    R1,BYRMEDH                                                       
         ST    R1,FVADDR                                                        
         B     VALRX                                                            
*                                                                               
VALR5    STC   RE,BYRCODE          SET BUYER CODE IN THE RECORD                 
         STC   RE,BBYR                                                          
         MVC   BYRMASK,BBYRMASK    SAVE AGY/MED MASK                            
         L     R2,AIOAREA1                                                      
*                                                                               
VALR6    GOTO1 AADDELS,BYRRECD     ADD ELEMENT TO RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,FILADD1                                                       
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,FILPUT1                                                       
         GOTO1 AIO                 ADD/PUT THE RECORD                           
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       TEST ACTION ADD                              
         BNE   VALRX                                                            
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R2,IOKEY                                                         
         OC    BYRPAGMD,BYRMASK    APPLY THE AGY/MED MASK                       
         MVC   BYRPBYR,BYRCODE     NEW BUYER CODE                               
         MVC   BYRKCNTL+1(4),IODA  D/A                                          
         GOTO1 AIO,DIRADD          ADD PASSIVE POINTER                          
         BNL   VALRX                                                            
         DC    H'0'                                                             
*                                                                               
VALR9    MVC   FVMSGNO,=AL2(FVCORPWD)      ENTER CORRECT PASSWORD               
         TM    TWAMODE,TWAMLSM                                                  
         BO    *+14                                                             
         XC    SAVRECK,SAVRECK     FOOL ROOT INTO DISPLAYING FOR CHANGE         
         B     VALRX                                                            
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(4),=C'here'          LIST/SELECT ACTIVE -                 
         LA    R1,BWSPWDH                  PASSWORD MUST BE IN                  
         ST    R1,FVADDR                   CONTROLLER PWD FIELD                 
         B     VALRX                                                            
*                                                                               
VALRX    NI    SVPWIND,255-SVPWVALD     RESET PASSWORD VALID IND                
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 AGETMED,BYRKAGMD                                                 
         BNE   *+10                                                             
         MVC   BYRMED,QMED                                                      
         OI    BYRMEDH+6,FVOXMT                                                 
         MVC   BYRBYR,BYRKBYR                                                   
         OI    BYRBYRH+6,FVOXMT                                                 
         B     DISKX                                                            
*                                                                               
DISKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   TWAXC BYRNAMH             CLEAR THE UNPROTECTED DATA FIELDS            
         L     R2,AIOAREA1                                                      
         MVC   BYRNAM,BYRNAME      OUTPUT BUYER'S NAME                          
         OI    BYRNAMH+6,FVOXMT                                                 
         CLI   APACTN,ACTCHA       CHECK FOR ACTION CHANGE                      
         BNE   DISR2                                                            
         OI    SVPWIND,SVPWVALD                                                 
         OC    BYRPW,BYRPW         YES - CHECK FOR EXISTENT PASSWORD            
         BZ    DISR2                                                            
         OC    INFPWD,INFPWD       CHECK FOR DEFAULT PASSWORD                   
         BZ    *+14                                                             
         CLC   BYRPW,INFPWD                                                     
         BE    DISR2                                                            
         GOTO1 AFVAL,BYRPASH       VALIDATE PASSWORD                            
         BL    DISR8               NO PASSWORD ENTERED                          
         BH    DISRX                                                            
         CLC   BYRPW,FVIFLD        COMPARE PASSWORDS                            
         BNE   DISR7               NOT EQUAL - ERROR                            
*                                                                               
DISR2    XC    BYRPAS,BYRPAS       CLEAR PASSWORD FIELD                         
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   *+10                                                             
         MVC   BYRPAS,BYRPW        SHOW THE PASSWORD THEY ALWAYS FORGET         
*                                                                               
         OI    BYRPASH+6,FVOXMT                                                 
         B     DISRX                                                            
*                                                                               
DISR7    MVC   FVMSGNO,=AL2(FVIPWD)        INVALID PASSWORD                     
         B     DISR9                                                            
*                                                                               
DISR8    MVC   FVMSGNO,=AL2(FVNOPWD)       ENTER PASSWORD                       
*                                                                               
DISR9    NI    SVPWIND,255-SVPWVALD        ERROR EXIT                           
         TM    TWAMODE,TWAMLSM             TEST LIST/SELECT ACTIVE              
         BZ    DISRX                                                            
         MVC   FVMSGNO,=AL2(FVCORPWD)      YES-                                 
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(4),=C'here'                                               
         LA    R1,BWSPWDH                  PASSWORD MUST BE IN                  
         ST    R1,FVADDR                   CONTROLLER PWD FIELD                 
         B     DISRX                                                            
*                                                                               
DISRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
DELREC   MVC   IOKEY(L'APRECKEY),APRECKEY                                       
         GOTO1 AIO,DIRRDUP            READ DIRECTORY FOR UPDATE                 
         BNE   DELRX                                                            
         LA    R2,IOKEY                                                         
         TM    BYRKCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    BYRKCNTL,X'80'         TURN ON DELETE BIT                        
         GOTO1 AIO,DIRWRT             WRITE DIRECTORY POINTER                   
         BNE   DELRX                                                            
         GOTO1 AIO,FILGETU1           READ FILE FOR UPDATE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         MVC   BBYRMASK,BYRMASK       COPY THE AGY/MED MASK                     
         TM    BYRCNTL,X'80'                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    BYRCNTL,X'80'          TURN ON DELETE BIT                        
         GOTO1 AIO,FILPUT1            WRITE THE RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOKEY,IOKEY            READ THE PASSIVE POINTER                  
         LA    R2,IOKEY                                                         
         MVI   BYRPTYP,BYRPTYPQ                                                 
         MVI   BYRPSUB,BYRPSUBQ                                                 
         MVC   BYRPAGMD,BAGYMD                                                  
         OC    BYRPAGMD,BBYRMASK      ALWAYS APPLY THE AGY/MED MASK             
         MVC   BYRPBYR,BBYR                                                     
         GOTO1 AIO,DIRRDUP            READ DIRECTORY FOR UPDATE                 
         BNE   DELRX                                                            
         TM    BYRKCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    BYRKCNTL,X'80'         TURN ON DELETE BIT                        
         GOTO1 AIO,DIRWRT             WRITE DIRECTORY POINTER                   
*                                                                               
DELRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
RESREC   MVC   IOKEY(L'APRECKEY),APRECKEY                                       
         GOTO1 AIO,DIRRDUP         READ DIRECTORY FOR UPDATE                    
         BL    RESRX               HARD ERROR                                   
         BE    RESR9               ALREADY EXISTS                               
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    RESRX               NO - ERROR                                   
         LA    R2,IOKEY                                                         
         TM    BYRKCNTL,X'80'                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    BYRKCNTL,X'7F'      TURN OFF DELETE BIT                          
         GOTO1 AIO,DIRWRT          WRITE DIRECTORY POINTER                      
         BNE   RESRX                                                            
         GOTO1 AIO,FILGETU1        READ FILE FOR UPDATE                         
         BNL   *+6                                                              
         DC    H'0'                HARD ERROR                                   
         BE    *+12                ALREADY EXISTS (OK)                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    RESRX               NO - ERROR                                   
         L     R2,IOADDR                                                        
         MVC   BBYRMASK,BYRMASK       COPY THE AGY/MED MASK                     
         TM    BYRCNTL,X'80'                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    BYRCNTL,X'80'       TURN ON DELETE BIT                           
         GOTO1 AIO,FILPUT1         WRITE THE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOKEY,IOKEY         READ THE PASSIVE POINTER                     
         LA    R2,IOKEY                                                         
         MVI   BYRPTYP,BYRPTYPQ                                                 
         MVI   BYRPSUB,BYRPSUBQ                                                 
         MVC   BYRPAGMD,BAGYMD                                                  
         OC    BYRPAGMD,BBYRMASK      ALWAYS APPLY THE AGY/MED MASK             
         MVC   BYRPBYR,BBYR                                                     
         GOTO1 AIO,DIRRDUP                                                      
         BL    RESRX               HARD ERROR                                   
         BE    RESR9               ALREADY EXISTS                               
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    RESRX               NO - ERROR                                   
         TM    BYRKCNTL,X'80'                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    BYRKCNTL,X'7F'      TURN OFF DELETE BIT                          
         GOTO1 AIO,DIRWRT          WRITE PASSIVE POINTER                        
         B     RESRX                                                            
*                                                                               
RESR9    MVC   FVMSGNO,=AL2(FVFERAE)     RECORD ALREADY EXISTS                  
*                                                                               
RESRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR CONTROLLER                                      *         
*          KEY COMPARE LENGTH                                         *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   XC    APRECKEY,APRECKEY                                                
         LA    R2,APRECKEY                                                      
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         XC    BYLMDN,BYLMDN                                                    
         OI    BYLMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,BYLMEDH     VALIDATE MEDIA                               
         BNE   VALPX                                                            
         MVC   BYRKAGMD,BAGYMD                                                  
         MVC   BYLMDN,MEDNM                                                     
         OI    SELKIND,SELKMED                                                  
         LA    R1,BYLBYRH          VALIDATE BUYER                               
         ST    R1,FVADDR                                                        
         CLI   5(R1),0                                                          
         BNE   *+16                                                             
         TM    SELKIND,SELKBYR                                                  
         BO    VALP9                                                            
         B     VALP2                                                            
         ZIC   RF,5(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BYRKBYR(0),BYLBYR                                                
         OI    SELKIND,SELKBYR                                                  
*                                                                               
VALP2    LA    RE,BYLSL1H          SET APPARM FIELDS FOR CONTROLLER             
         ST    RE,APPARM           A(FIRST TWA SELECT FIELD HEADER)             
         MVI   APPARM+4,15         NUMBER OF SELECT LINES ON SCREEN             
         LA    RE,BYLSL2H-BYLSL1H                                               
         STH   RE,APPARM+6         LENGTH OF SELECT LINE                        
         LA    RE,BYRKAGMD-BYRKEY  SET KEY COMPARE LENGTH                       
         TM    SELKIND,SELKMED                                                  
         BZ    *+8                                                              
         LA    RE,L'BYRKAGMD(RE)                                                
         BCTR  RE,0                                                             
         STC   RE,LKEYCOMP                                                      
         B     VALPX                                                            
*                                                                               
VALP9    MVC   FVMSGNO,=AL2(FVFNONE)                                            
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
* INPUT  : APINDS EQ 0 FIRST TIME                                     *         
*                 NE 0 NEXT TIME(S)                                   *         
*          APRECKEY                                                   *         
* OUTPUT : CURRENT APRECKEY                                           *         
*          APRECDA                                                    *         
*          APRECID (OPTIONAL)                                         *         
*          APMODE = APMEOFS FOR END-OF-LIST                           *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   MVC   IOKEY,APRECKEY                                                   
         TM    APINDS,APILRERD                                                  
         BZ    GETS2                                                            
         GOTO1 AIO,DIRRD                                                        
         BE    GETS4                                                            
         B     GETS9                                                            
*                                                                               
GETS2    TM    APINDS,APILNSEQ                                                  
         BO    GETS4                                                            
         LA    R1,DIRHI+IO1                                                     
         B     *+8                                                              
*                                                                               
GETS4    LA    R1,DIRSQ+IO1                                                     
         GOTO1 AIO                                                              
         BNE   GETS9                                                            
         ZIC   R3,LKEYCOMP                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETS9                                                            
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(13),IOKEY                                               
         MVC   APRECDA,IODA                                                     
         B     GETSX                                                            
*                                                                               
GETS9    MVI   APMODE,APMEOFS                                                   
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
* INPUT  : APPARM+0(4) = A(TWA DISPLAY LINE)                          *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   MVC   IODA,APRECDA                                                     
         GOTO1 AIO,FILGET1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         L     R3,APPARM                                                        
         USING BYLSL1H,R3                                                       
         MVC   BYLSBY,BYRKBYR                                                   
         MVC   BYLSBN,BYRNAME                                                   
         B     DISSX                                                            
*                                                                               
DISSX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IO ERROR BITS                                      *         
* IN : IOERR  - IO ERROR RETURN BYTE                                  *         
* OUT: APINDS - APPLICATION INDICATORS BYTE                           *         
*      CC     - EQ  OK                                                *         
*             - NE  NON RECOVERABLE ERROR                             *         
***********************************************************************         
         SPACE 1                                                                
IOCHECK  TM    IOERR,IOEDSK        NON-RECOVERABLE DISK ERROR                   
         BO    IOCH9                                                            
*                                                                               
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
         TM    IOERR,IOEEOF        END-OF-FILE                                  
         BO    *+12                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND                             
         BZ    *+12                                                             
         NI    APINDS,255-APIOKDIS-APIOKCHA-APIOKDEL                            
         OI    APINDS,APIOKADD                                                  
*                                                                               
         TM    IOERR,IOEDEL         RECORD IS DELETED                           
         BZ    *+12                                                             
         NI    APINDS,255-APIOKADD-APIOKCHA-APIOKDEL                            
         OI    APINDS,APIOKRES                                                  
*                                                                               
         CR    RE,RE                                                            
         B     IOCHX                                                            
*                                                                               
IOCH9    MVC   FVMSGNO,=AL2(FVFIOER)  IO ERROR                                  
         LA    R1,BYRMEDH                                                       
         ST    R1,FVADDR                                                        
         LTR   RE,RE                                                            
*                                                                               
IOCHX    BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SPACES   DC    CL80' '                                                          
*                                                                               
LOCALD   DSECT                                                                  
LKEYCOMP DS    X                                                                
SELKEY   DS    XL32                                                             
SELKIND  DS    XL1                                                              
SELKMED  EQU   X'80'                                                            
SELKBYR  EQU   X'40'                                                            
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFED                                                       
         SPACE 2                                                                
         ORG   SAVOVER                                                          
SVPWIND  DS    XL1                 SAVED PASSWORD INDICATOR                     
SVPWVALD EQU   X'80'                                                            
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDED                                                       
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPNWSBYR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPNWS02   02/26/07'                                      
         END                                                                    
