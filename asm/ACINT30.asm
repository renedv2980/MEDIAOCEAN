*          DATA SET ACINT30    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T61930A,*                                                                
         TITLE 'T61930 - PROFILE LIST'                                          
T61930   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61930**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
KEYLOGIC LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
RECLOGIC BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    DISLOGIC            YES                                          
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         BAS   RE,TSTEDT           TEST FOR ANYTHING TO EDIT                    
         BE    EDTLOGIC            YES                                          
         MVI   INTMODE,DISLIST     NO, CONTINUE LIST                            
         B     DISLOGIC                                                         
         EJECT                                                                  
*                                                                               
* CLEAR SCREEN AND TABLE, BRANCH TO READ AND DISPLAY ROUTINES                   
* AND DETERMINE APPROPRIATE MESSAGE                                             
*                                                                               
DISLOGIC GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
         MVI   NUMLINES,0                                                       
         LA    RE,LSELTAB          CLEAR TABLE                                  
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
*                                                                               
         XC    LLASTPRO,LLASTPRO   CLEAR OUT LAST PROFILE LISTED                
         BAS   RE,READLST                                                       
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    FULLMESS            YES                                          
         CLI   NUMLINES,0          NO, IS SCREEN EMPTY ?                        
         BE    NONEMESS            YES                                          
         LA    R2,PRLSTRTH         POSITION CURSOR AT FIRST KEY FIELD           
         XC    LLASTPRO,LLASTPRO   CLEAR LAST PROFILE LISTED                    
         MVC   CONHEAD(L'DONEMSG),DONEMSG                                       
DISLOGX  ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
FULLMESS MVC   CONHEAD(L'FULLMSG),FULLMSG                                       
         B     DISLOGX                                                          
*                                                                               
NONEMESS MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     DISLOGX                                                          
         EJECT                                                                  
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
EDTLOGIC BAS   RE,EDT                                                           
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE HEADING FIELD(S)                                                     
*                                                                               
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         XC    QFILTS(QFILTLN),QFILTS                                           
         LA    R2,PRLSTRTH         OPTIONAL START FIELD                         
         BAS   RE,TSTKEY                                                        
         CLI   PRLSTRTH+5,0        STARTING RECEIVABLE ACCT INPUT?              
         BE    VALHED2             NO - CHECK FOR CLIENT                        
         GOTO1 ANY                                                              
         OI    PRLSTRTH+6,X'80'    TRANSMIT                                     
         MVC   QSTRT,WORK          SAVE ACCOUNT                                 
         SPACE 1                                                                
VALHED2  LA    R2,PRLCLTH          CLIENT                                       
         BAS   RE,TSTKEY                                                        
         CLI   PRLCLTH+5,0                                                      
         BE    VALHED3             NO CHECK FOR PRD                             
         SR    R1,R1                                                            
         ICM   R1,1,PRLCLTH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   INVEND                                                           
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BE    INVEND              YES, THIS IS AN ERROR                        
         SPACE 1                                                                
         MVC   CUL+1(2),PRODLEDG                                                
         GOTO1 VALCLI              VALIDATE CLIENT                              
         OI    PRLCLTH+6,X'80'     TRANSMIT                                     
         MVC   QCLT,CLICODE        SAVE CLIENT                                  
         SPACE 1                                                                
VALHED3  LA    R2,PRLPRDH          PRODUCT                                      
         BAS   RE,TSTKEY                                                        
         CLI   PRLPRDH+5,0         PRODUCT INPUT                                
         BE    VALHED4             NO CHECK FOR MEDIA                           
         CLI   PRLCLTH+5,0         YES- A CLIENT MUST ALSO BE INPUT             
         BNE   *+12                                                             
         LA    R2,PRLCLTH                                                       
         B     INVEND                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         ICM   R1,1,PRLPRDH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   INVEND                                                           
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    INVEND              YES, THIS IS AN ERROR                        
         SPACE 1                                                                
         GOTO1 VALPROD             VALIDATE PRODUCT                             
         OI    PRLPRDH+6,X'80'     TRANSMIT                                     
         MVC   QPRD,PRODCODE       SAVE IT                                      
         SPACE 1                                                                
VALHED4  LA    R2,PRLMEDH          MEDIA                                        
         BAS   RE,TSTKEY                                                        
         CLI   PRLMEDH+5,0                                                      
         BE    VALHED5             NO CHECK FOR ESTIMATE                        
         GOTO1 VALMED              VALIDATE MEDIA                               
         OI    PRLMEDH+6,X'80'     TRANSMIT                                     
         MVC   QMED,MEDIA          SAVE IT                                      
         SPACE 1                                                                
VALHED5  LA    R2,PRLESTH          ESTIMATE                                     
         BAS   RE,TSTKEY                                                        
         CLI   PRLESTH+5,0                                                      
         BE    VALHEDX                                                          
         GOTO1 VALEST              VALIDATE ESTIMATE                            
         OI    PRLESTH+6,X'80'     TRANSMIT                                     
         MVC   QEST,ESTIMATE       SAVE IT                                      
VALHEDX  B     XIT                                                              
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO PROCESS PF KEYS                                                
*                                                                               
PROCPF   NTR1                                                                   
         L     R2,AFSTSEL                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    PROCPFX                                                          
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
         CLI   PFKEY,PF2                                                        
         BE    PROCPF0                                                          
         CLI   PFKEY,PF3                                                        
         BNE   PROCPFX                                                          
PROCPF0  L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               ANY FIELD MODIFIED AFTER ACTION              
         BH    PROCPFX             YES                                          
         L     RE,ATWA                                                          
         AH    RE,CURDISP                                                       
         ST    RE,ACURSOR                                                       
*                                                                               
PROCPF2  BAS   RE,SETLIN           GET ADDRESSES                                
         L     R2,ASEL                                                          
         C     R2,ACURSOR                                                       
         BNE   PROCPFNT            FIND OUT WHERE CURSOR IS                     
         LA    R6,SELKEY                                                        
         USING ACINKEY,R6                                                       
         XC    DUB,DUB                                                          
         MVI   DUB,L'ACINMED                                                    
         MVC   DUB+1(L'ACINMED),ACINMED                                         
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   MI= RECORD?                        
         BZ    PROCPF2A                                                         
         MVI   DUB,X'05'                                                        
         MVC   DUB+1,=C'MI='                                                    
         MVC   DUB+4(L'ACINMED),ACINMED                                         
         SPACE 1                                                                
PROCPF2A LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'                                                     
         CLI   PFKEY,PF2                                                        
         BE    PROCPF4                                                          
         CLI   PFKEY,PF3                                                        
         BNE   PROCPFX                                                          
         LA    R1,=C'ADJ'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
PROCPF4  MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         GOTO1 VCALL,WORK,=C'ESTIMATE',,(12,ACINACC),(3,ACINCLT),(3,ACIX        
               NPRD),(DUB,DUB+1),(6,ACINEST),0                                  
*                                                                               
PROCPFNT L     R2,ANEXTSEL         POINT TO NEXT SELECT FIELD                   
         LA    R5,SELTABL(R5)      NEXT SELECT FIELD ENTRY                      
         BCT   R3,PROCPF2                                                       
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
* ON EXIT: CC=EQ IF EDIT NEEDED; CC=NEQ IF EDIT NOT NEEDED                      
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES       EXIT IF NOTHING ON SCREEN                    
         BZ    TSTEDTN                                                          
*                                                                               
TSTEDT2  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTEDT4             NO, TEST IF ANYTHING ELSE WAS                
         CLI   8(R2),C'*'          YES, WAS IS ALREADY PROCESSED ?              
         BE    TSTEDT4             YES, TEST FOR ANYTHING ELSE                  
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
*                                                                               
TSTEDT4  LA    R1,MAXFIELD-1       GET NUMBER OF FIELDS                         
*                                                                               
TSTEDT6  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BCT   R1,TSTEDT6          NO, TRY NEXT FIELD                           
         BAS   RE,BUMP             THIS LINE DONE, TRY NEXT                     
         BCT   R3,TSTEDT2                                                       
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* READ A RECORD, SET THE LINE AND DISPLAY THE DATA                              
*                                                                               
READLST  NTR1                                                                   
         MVC   DIR,ACCFIL                                                       
         CLI   EMULATE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DIR,ACCDIR          FOR EMULATED FILE READ DIRECTORY             
*                                                                               
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    READL020                                                         
         MH    R3,=Y(MAXFIELD)                                                  
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
READL020 ST    R2,ATHISLIN         SAVE LINE ADDRESS                            
         LA    R6,KKEY                                                          
         MVC   KKEY,SPACES                                                      
         USING ACINKEY,R6                                                       
         OC    LLASTPRO,LLASTPRO   IS THIS A CONTINUATION ?                     
         BNZ   READL040            YES                                          
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
         MVC   CUL+1(2),RECVLEDG                                                
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL,CUL                                                      
         CLI   PRLSTRTH+5,0                                                     
         BE    READL060                                                         
         MVC   ACINACC,QSTRT                                                    
         CLI   PRLCLTH+5,0                                                      
         BE    READL060                                                         
         MVC   ACINCLT,QCLT                                                     
         CLI   PRLPRDH+5,0                                                      
         BE    READL060                                                         
         MVC   ACINPRD,QPRD                                                     
         CLI   PRLMEDH+5,0                                                      
         BE    READL060                                                         
         MVC   ACINMED,QMED                                                     
         CLI   PRLESTH+5,0                                                      
         BE    READL060                                                         
         MVC   ACINEST,QEST                                                     
         B     READL060                                                         
*                                                                               
READL040 MVC   ACINKEY(L'LLASTPRO),LLASTPRO     THE LAST ONE READ               
         MVI   ACINEST+5,X'FF'                  BUMPED UP                       
*                                                                               
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
READL060 BAS   RE,RDHIGH                                                        
         GOTO1 CATCHIOS                                                         
         B     READL100                                                         
*                                                                               
READL080 BAS   RE,RDSEQL                                                        
         GOTO1 CATCHIOS                                                         
*                                                                               
READL100 CLC   KKEY(5),SAVKKEY      EXIT IF COMPANY CHANGES                     
         BNE   XIT                                                              
         L     R6,AIO                                                           
         OC    QCLT,QCLT                                                        
         BZ    *+14                                                             
         CLC   QCLT,ACINCLT       FILTERING BY CLIENT                           
         BNE   READL080                                                         
         OC    QPRD,QPRD                                                        
         BZ    *+14                                                             
         CLC   QPRD,ACINPRD        PRODUCT                                      
         BNE   READL080                                                         
         OC    QMED,QMED                                                        
         BZ    *+14                                                             
         CLC   QMED,ACINMED        MEDIA                                        
         BNE   READL080                                                         
         OC    QEST,QEST                                                        
         BZ    *+14                                                             
         CLC   QEST,ACINEST        ESTIMATE                                     
         BNE   READL080                                                         
         SPACE 1                                                                
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLIN           SET ADDRESSES                                
         BAS   RE,DISLINE          DISPLAY RECORD                               
         MVC   LLASTPRO,ACINKEY    SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         MH    R5,=Y(SELTABL)                                                   
         LA    R5,LSELTAB(R5)      ADDRESS TABLE ENTRY                          
         USING SELTABD,R5                                                       
         MVC   SELKEY,ACINKEY                                                   
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BNE   READL080            NO, GET NEXT                                 
READLX   B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
********************************************************                        
*        READ RECORDS                                                           
********************************************************                        
         SPACE 1                                                                
RDREAD   NTR1                                                                   
         MVC   COMMAND,DMREAD                                                   
         B     GTREC01                                                          
RDHIGH   NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         B     GTREC                                                            
RDSEQL   NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
GTREC    DS    0H                                                               
         MVC   SAVKKEY,KKEY                                                     
GTREC01  L     R6,AIO                                                           
         USING INTRECD,R6                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,DIR,KKEY,(R6)                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KKEY,0(R6)          SAVE DIRECTORY KEY                           
         SPACE 1                                                                
         CLI   EMULATE,C'Y'                                                     
         BNE   XIT                                                              
         MVC   DA,INTKDA           SAVE DISK ADDRESS                            
         MVC   RDFIL,ACCMST        REGULAR FILE                                 
         TM    INTKSTA,X'04'                                                    
         BZ    *+10                                                             
         MVC   RDFIL,ACCARC        ARCHIVE FILE                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,GETRECD,RDFIL,DA,(R6),DMWORK                        
         LA    R2,DMCB             R2=A(DATAMGR PARMS)                          
         ICM   R2,8,=X'FF'         FF IN HOB                                    
         GOTO1 ACCEMU,DMCB,NEWO,,(R6),(R6)                                      
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY A LINE OF DATA                                                        
*                                                                               
DISLINE  NTR1                                                                   
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                 INDICATE VALIDATED                   
         L     R4,ASEL                                                          
         USING SCREEND,R4                                                       
         L     R6,AIO                                                           
         USING ACINKEY,R6                                                       
         OI    SLINEH+4,X'20'              MARK LINE VALIDATED                  
         MVC   SACCOUNT,ACINACC            ACCOUNT                              
         MVC   SCLIENT,ACINCLT             CLIENT                               
         MVC   SPRODUCT,ACINPRD            PRODUCT                              
         MVC   SMEDIA,SPACES                                                    
         MVC   SMEDIA(L'ACINMED),ACINMED     MEDIA                              
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   IS MEDIA AN MI RECORD?             
         BZ    DISLI01                                                          
         MVC   SMEDIA(3),=C'MI='                                                
         MVC   SMEDIA+3(L'ACINMED),ACINMED                                      
DISLI01  MVC   SESTIMAT,ACINEST            ESTIMATE                             
         MVI   ELCODE,ACIPFEQU             PROFILE ELEMENT                      
         MVI   ERROR,NOEL                                                       
         BAS   RE,GETELIO                                                       
         BNE   INVEND                                                           
         USING ACINPRFD,R6                                                      
         MVC   SESTDESC,ACIPFDES           ESTIMATE DESCRIPTION                 
         SPACE 1                                                                
         MVC   AIO,AIO2                    USE AIO2 FOR NAME LOOKUP             
         MVC   KEY,SPACES                                                       
         MVC   CUL+1(2),RECVLEDG                                                
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'SACCOUNT),SACCOUNT                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   ELCODE,ACNMELQ      NAME ELEMENT                                 
         SR    R3,R3                                                            
         LA    R3,L'SNAME(R3)                                                   
         BCTR  R3,0                                                             
         BAS   RE,GETELIO                                                       
         BNE   INVEND                                                           
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         CR    R1,R3                                                            
         BNH   *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SNAME(0),ACNMNAME                                                
         SPACE 1                                                                
         MVC   AIO,AIO1                     GO BACK TO ORIGINAL AIO             
         BAS   RE,RDREAD                                                        
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* EDIT THE LIST SCREEN                                                          
*                                                                               
EDT      NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         IC    R3,NUMLINES         NUMBER OF LINES ON SCREEN                    
         LA    R5,LSELTAB          ADDRESS OF SELECT TABLE                      
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN           GET FIELD ADDRESSES FOR THIS LINE            
         L     R2,ASEL                                                          
         CLI   5(R2),0             IF NOTHING IN SELECT TRY NEXT LINE           
         BE    EDT6                                                             
         CLI   5(R2),3             IF IN SELECT, CAN BE 1, 2 OR 3 BYTES         
         BH    INVEND                                                           
         CLI   8(R2),C'*'          IF ALREADY EDITED GET NEXT LINE              
         BE    EDT6                                                             
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         CLI   8(R2),C'S'          IF OTHER THAN 'S', 'C' OR 'D' -ERROR         
         BE    EDT4                                                             
         LA    R1,=C'CHA'                                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'                                                     
         CLI   8(R2),C'C'                                                       
         BE    EDT4                                                             
         CLC   8(2,R2),=C'DE'                                                   
         BNE   INVEND                                                           
         MVI   ERROR,SECLOCK                                                    
         TM    TWAAUTH,X'10'       MUST BE ON TO DELETE                         
         BZ    ERREXIT                                                          
         LA    R1,=C'DEL'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
*                                                                               
EDT4     MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R6,SELKEY                                                        
         USING ACINKEY,R6                                                       
         XC    DUB,DUB                                                          
         MVI   DUB,L'ACINMED                                                    
         MVC   DUB+1(L'ACINMED),ACINMED                                         
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   MI= RECORD?                        
         BZ    EDT4A                                                            
         MVI   DUB,X'05'                                                        
         MVC   DUB+1,=C'MI='                                                    
         MVC   DUB+4(L'ACINMED),ACINMED                                         
EDT4A    DS    0H                                                               
         GOTO1 VCALL,WORK,=C'PROFILE',,(12,ACINACC),(3,ACINCLT),(3,ACINX        
               PRD),(DUB,DUB+1),(6,ACINEST),0                                   
EDT6     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* SET SCREEN LINE ADDRESSES                                                     
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PRLSELH                                                       
         ST    R2,AFSTSEL                                                       
         LA    R0,MAXLINE                                                       
         LA    R1,MAXFIELD                                                      
         MR    R0,R0               COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SET DATA LINE ADDRESSES AND GET NEXT LINE                                     
* AT ENTRY R2 =A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,MAXFIELD         NUMBER OF FIELDS PER LINE                    
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR PROTECTED FIELDS                  
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
INVEND   MVI   ERROR,INVALID                                                    
ERREXIT  GOTO1 VERRCUR                                                          
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NONEMSG  DC    CL50'NO DATA TO DISPLAY'                                         
*                                                                               
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
GETRECD  DC    C'GETREC  '                                                      
NEWO     DC    C'NEWO    '                                                      
ACCFIL   DC    C'ACCOUNT '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
SCREEND  DSECT                                                                  
SSELECTH DS    CL8                                                              
SSELECT  DS    CL3                                                              
SLINEH   DS    CL8                                                              
SLINE    DS    0CL71                                                            
SACCOUNT DS    CL12                                                             
         DS    CL2                                                              
SNAME    DS    CL17                                                             
         DS    CL2                                                              
SCLIENT  DS    CL3                                                              
         DS    CL2                                                              
SPRODUCT DS    CL3                                                              
         DS    CL2                                                              
SMEDIA   DS    CL5                                                              
         DS    CL2                                                              
SESTIMAT DS    CL6                                                              
         DS    CL2                                                              
SESTDESC DS    CL14                                                             
SLENGTH  EQU   *-SCREEND                                                        
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACINTWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTE0D                                                       
         DS    0F                                                               
*                                                                               
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
LLASTPRO DS    CL(L'SELKEY)        LAST PROFILE ON SCREEN                       
LSELTAB  DS    CL(MAXLINE*SELTABL)                                              
SAVEKEY  DS    CL48                SAVE THE KEY                                 
*                                                                               
*                                                                               
MAXLINE  EQU   14                  MAXIMUM N'LINES PER SCREEN                   
MAXFIELD EQU   2                   MAXIMUM N'FIELDS PER LINE                    
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   LOCAL                                                            
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
*                                                                               
QFILTS   DS    0C                                                               
QSTRT    DS    CL(L'ACINACC)                                                    
QCLT     DS    CL(L'ACINCLT)                                                    
QPRD     DS    CL(L'ACINPRD)                                                    
QMED     DS    CL(L'ACINMED)                                                    
QEST     DS    CL(L'ACINEST)                                                    
QFILTLN  EQU   *-QFILTS                                                         
*                                                                               
DA       DS    XL4                 DISK ADDRESS                                 
DIR      DS    CL(L'ACCFIL)        DIR NAME (ACCFIL/ACCDIR)                     
RDFIL    DS    CL(L'ACCMST)        FIL NAME (ACCMST/ACCARC)                     
KKEY     DS    CL(ACCKLEN)                                                      
SAVKKEY  DS    CL(ACCKLEN)                                                      
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
ACURSOR  DS    A                                                                
*                                                                               
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
AACCOUNT DS    A                   A(ACCOUNT FIELD)                             
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELKEY   DS    CL48                                                             
SELTABL  EQU   *-SELTABD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACINT30   05/01/02'                                      
         END                                                                    
