*          DATA SET ACPRO65    AT LEVEL 006 AS OF 08/22/02                      
*PHASE T60B65A,*                                                                
         TITLE 'T60B65 - AUTHORIZATION LIST'                                    
T60B65   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B65**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*             VALKEY LOGIC                                            *         
***********************************************************************         
*                                                                               
VKEY     BAS   RE,VALHED           VALIDATE SCREEN                              
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*             VALREC LOGIC                                            *         
***********************************************************************         
*                                                                               
VREC     BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    DISLOGIC            YES                                          
         BAS   RE,TSTEDT           SEE IF ANYTHING IN SELECT                    
         BNE   *+12                NO                                           
         BAS   RE,EDT              YES                                          
         B     *+8                                                              
         MVI   INTMODE,DISLIST     NO, CONTINUE LIST                            
         B     DISLOGIC                                                         
         EJECT                                                                  
***********************************************************************         
*             VALIDATE THE HEADER FIELDS                              *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         XC    QFILTS(QFILTLN),QFILTS                                           
*                                                                               
         LA    R2,AUTOGRH          OPTIONAL OFFICE GROUP                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BE    VALHED2             NO - CHECK FOR OFFICE                        
         CLC   =C'ALL',8(R2)       TEST FOR ALL                                 
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         OI    6(R2),X'80'                                                      
         MVC   QOGR,EFFOFG         SAVE OFFICE GROUP                            
*                                                                               
VALHED2  LA    R2,AUTOFCH          OPTIONAL OFFICE                              
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED3                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   AUTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         OI    6(R2),X'80'                                                      
         MVC   QOFC,EFFOFFC        SAVE OFFICE                                  
*                                                                               
VALHED3  LA    R2,AUTCLIH          OPTIONAL CLIENT                              
         BAS   RE,TSTKEY                                                        
         CLI   AUTCLIH+5,0                                                      
         BE    VALHED4                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   AUTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   AUTOFCH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         OI    AUTCLIH+6,X'80'                                                  
         MVC   QCLI,CLICODE        SAVE CLIENT                                  
*                                                                               
VALHED4  LA    R2,AUTPROH          OPTIONAL PRODUCT                             
         BAS   RE,TSTKEY                                                        
         CLI   AUTPROH+5,0                                                      
         BE    VALHED5                                                          
         CLI   AUTPROH+5,0                                                      
         BE    VALHED5                                                          
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   AUTCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         OI    AUTPROH+6,X'80'                                                  
         MVC   QPRO,PRODCODE       SAVE PRODUCT                                 
*                                                                               
VALHED5  LA    R2,AUTMGRH          OPTINAL MEDIA GROUP                          
         BAS   RE,TSTKEY                                                        
         CLI   AUTMGRH+5,0                                                      
         BE    VALHED6                                                          
         GOTO1 VALMG                                                            
         OI    AUTMGRH+6,X'80'                                                  
         MVC   QMGR,MGROUP         SAVE MEDIA GROUP                             
*                                                                               
VALHED6  LA    R2,AUTMEDH          OPTION MEDIA                                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   AUTMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         OI    AUTMEDH+6,X'80'                                                  
         MVC   QMED,MEDIA          SAVE MEDIA                                   
*                                                                               
VALHED8  LA    R2,AUTSTAH          OPTION START                                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         GOTO1 ANY                                                              
         OI    AUTSTAH+6,X'80'                                                  
         MVC   QSTA,WORK           SAVE START                                   
*                                                                               
VALHED10 LA    R2,AUTSTATH         OPTION STATUS                                
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHEDX                                                          
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'A'                                                       
         BE    VALHED12                                                         
         CLI   8(R2),C'I'                                                       
         BNE   ERREXIT                                                          
*                                                                               
VALHED12 OI    AUTSTATH+6,X'80'                                                 
         MVC   QSTAT,WORK           SAVE START                                  
*                                                                               
VALHEDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*             DISPLAY LOGIC                                           *         
***********************************************************************         
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
         XC    LLASTAUT,LLASTAUT   CLEAR OUT LAST AUTHORIZATION                 
         BAS   RE,LIST                                                          
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,NLINES     IS SCREEN FULL ?                             
         BE    DISLOG2             YES                                          
         XC    LLASTAUT,LLASTAUT   CLEAR LAST AUTH LISTED                       
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         CLI   NUMLINES,0          NO, IS SCREEN EMPTY ?                        
         BNE   DISLOGX             NO                                           
         LA    R2,AUTOGRH          POSITION CURSOR AT FIRST KEY FIELD           
         MVC   CONHEAD(L'LIST3MSG),LIST3MSG                                     
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BNE   DISLOGX             NO                                           
         MVC   CONHEAD(L'LIST4MSG),LIST4MSG                                     
         B     DISLOGX                                                          
*                                                                               
DISLOG2  MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
*                                                                               
DISLOGX  ST    R2,ACURFORC                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              CHECK FOR SELECT ENTRIES                               *         
***********************************************************************         
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
*                                                                               
         LA    R1,=C'AUTH'         RECORD                                       
         ST    R1,WORK                                                          
         MVI   WORK,X'04'          LENGTH OF RECORD                             
*                                                                               
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         CLI   8(R2),C'S'          'S', 'H', 'C' OR 'F' ONLY                    
         BE    EDT4                                                             
*                                                                               
         LA    R1,=C'HIS'                                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'                                                     
         CLI   8(R2),C'H'                                                       
         BE    EDT4                                                             
*                                                                               
         LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'                                                     
         CLI   8(R2),C'C'                                                       
         BE    EDT4                                                             
*                                                                               
         CLI   8(R2),C'F'          CHANGE RECORD AND ACTION FOR THIS            
         BNE   INVEND                                                           
         LA    R1,=C'FUND'         RECORD                                       
         ST    R1,WORK                                                          
         MVI   WORK,X'04'          LENGTH OF RECORD                             
         LA    R1,=C'MAINT'        ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'05'                                                     
*                                                                               
EDT4     MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         USING AUTKEY,R6                                                        
         LA    R6,SELKEY                                                        
         GOTO1 VCALL,WORK,,,(1,AUTKOGR),(2,AUTKOFC),(6,AUTKCLI),       X        
               (6,AUTKPRO),(1,AUTKMGR),(1,AUTKMED),(19,AUTKNUM),0               
*                                                                               
EDT6     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
*             TEST FOR KEY CHANGES                                    *         
***********************************************************************         
*                                                                               
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*             TEST IF ANYTHING TO EDIT ONTHE SCREEN                   *         
***********************************************************************         
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES       EXIT IF NOTHING ON SCREEN                    
         BZ    TSTEDTN                                                          
*                                                                               
TSTEDT2  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTEDT4             NO                                           
         CLI   8(R2),C'*'          ALREADY PROCESSED?                           
         BE    TSTEDT4             YES                                          
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
*                                                                               
TSTEDT4  BAS   RE,BUMP                                                          
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
         BCT   R3,TSTEDT2          AND LOOK THERE                               
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*             READ AND LIST THE RECORDS                               *         
***********************************************************************         
*                                                                               
LIST     NTR1                                                                   
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    LIST02                                                           
         MH    R3,=Y(NFIELDS)                                                   
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
LIST02   ST    R2,ATHISLIN         SAVE LINE ADDRESS                            
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         MVC   AUTKEY,SPACES                                                    
         OC    LLASTAUT,LLASTAUT   IS THIS A CONTINUATION ?                     
         BNZ   LIST04              YES                                          
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
*                                                                               
         MVC   AUTKOGR,QOGR                                                     
         MVC   AUTKOFC,QOFC                                                     
         CLI   AUTKOFC,0                                                        
         BE    *+8                                                              
         OI    AUTKOFC+1,C' '                                                   
         MVC   AUTKCLI,QCLI                                                     
         MVC   AUTKPRO,QPRO                                                     
         MVC   AUTKMGR,QMGR                                                     
         MVC   AUTKMED,QMED                                                     
         MVC   AUTKNUM,QSTA                                                     
         B     LIST06                                                           
*                                                                               
LIST04   MVC   AUTKEY(L'LLASTAUT),LLASTAUT     THE LAST ONE READ                
         GOTO1 HIGH                RE-READ LAST ONE                             
         B     LIST08              THEN READ THE NEXT                           
*                                                                               
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST06   GOTO1 HIGH                                                             
         B     LIST10                                                           
*                                                                               
LIST08   GOTO1 SEQ                                                              
*                                                                               
LIST10   CLC   KEY(5),KEYSAVE      EXIT IF COMPANY CHANGES                      
         BNE   XIT                                                              
*                                                                               
         L     R6,AIO                                                           
         CLI   QSTAT,0             CHECK STATUS                                 
         BE    LIST14              NO, ACCEPT ALL                               
         TM    AUTRSTA,AUTRDACT    IS RECORD INACTIVE?                          
         BO    LIST12              YES                                          
         CLI   QSTAT,C'A'          NO, DO WE WANT ACTIVE?                       
         BE    LIST14              YES                                          
         B     LIST08              NO                                           
*                                                                               
LIST12   CLI   QSTAT,C'I'          NO WE WANT INACTIVE                          
         BNE   LIST08              NO                                           
*                                                                               
LIST14   OC    QOGR,QOGR           OFFICE GROUP FILTERING                       
         BZ    *+14                NO                                           
         CLC   QOGR,AUTKOGR                                                     
         BNE   LIST08                                                           
*                                                                               
         OC    QOFC,QOFC                                                        
         BZ    *+14                                                             
         CLC   QOFC,AUTKOFC       OFFICE FILTERING                              
         BNE   LIST08                                                           
*                                                                               
         OC    QCLI,QCLI                                                        
         BZ    *+14                                                             
         CLC   QCLI,AUTKCLI       CLIENT FILTERING                              
         BNE   LIST08                                                           
*                                                                               
         OC    QPRO,QPRO                                                        
         BZ    *+14                                                             
         CLC   QPRO,AUTKPRO        PRODUCT FILTERING                            
         BNE   LIST08                                                           
*                                                                               
         OC    QMGR,QMGR                                                        
         BZ    *+14                                                             
         CLC   QMGR,AUTKMGR        MEDIA GROUP FILTERING                        
         BNE   LIST08                                                           
*                                                                               
         OC    QMED,QMED                                                        
         BZ    *+14                                                             
         CLC   QMED,AUTKMED        MEDIA FILTERING                              
         BNE   LIST08                                                           
*                                                                               
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLIN           SET ADDRESSES                                
         BAS   RE,DISLINE          DISPLAY RECORD                               
         MVC   LLASTAUT,AUTKEY     SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
*                                                                               
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
*                                                                               
         MH    R5,=Y(SELTABL)                                                   
         LA    R5,LSELTAB(R5)      ADDRESS TABLE ENTRY                          
         USING SELTABD,R5                                                       
         MVC   SELKEY,AUTKEY                                                    
         CLI   NUMLINES,NLINES     IS SCREEN FULL ?                             
         BNE   LIST08              NO, GET NEXT                                 
*                                                                               
LISTX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* DISPLAY A LINE OF DATA                                                        
*                                                                               
DISLINE  NTR1                                                                   
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                 INDICATE VALIDATED                   
         L     R6,AIO                                                           
         USING AUTKEY,R6                                                        
         USING LSTDSECT,R3                                                      
*                                                                               
         LA    R3,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         MVC   LSTOGR,AUTKOGR                                                   
         MVC   LSTOFC,AUTKOFC                                                   
         MVC   LSTCLI,AUTKCLI                                                   
         MVC   LSTPRO,AUTKPRO                                                   
         MVC   LSTMGR,AUTKMGR                                                   
         MVC   LSTMED,AUTKMED                                                   
         MVC   LSTNUM,AUTKNUM                                                   
*                                                                               
         USING AUTHELD,R6                                                       
         MVI   ELCODE,AUTHELQ                                                   
         BAS   RE,GETELIO                                                       
         ST    R6,SAVER6                                                        
*                                                                               
DISL02   BAS   RE,NEXTEL           ANY MORE?                                    
         BNE   DISL04              NO                                           
         ST    R6,SAVER6           YES, SAVE ADDRESS AND LOOK AGAIN             
         B     DISL02                                                           
*                                                                               
DISL04   L     R6,SAVER6                                                        
         CURED (P6,AUTHAMT),(12,LSTAMT),2,ALIGN=RIGHT                           
*                                                                               
         L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'            SUBTRACT LENGTH OF HEADER +1                 
         EX    R1,*+8                                                           
         B     DISLX                                                            
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
DISLX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SET SCREEN LINE ADDRESSES                                                     
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,AUTSELH                                                       
         ST    R2,AFSTSEL                                                       
         LA    R0,NLINES                                                        
         LA    R1,NFIELDS                                                       
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
         LA    R0,NFIELDS          NUMBER OF FIELDS PER LINE                    
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
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LIST2MSG DC    C'LIST DISPLAYED'                                                
LIST3MSG DC    C'THERE ARE NO MORE RECORDS TO DISPLAY'                          
LIST4MSG DC    C'THERE ARE NO RECORDS TO DISPLAY     '                          
*                                                                               
SAVER6   DS    A                                                                
SAVERE   DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LSTDSECT DSECT                                                                  
         DS    CL1                                                              
LSTOGR   DS    CL1                                                              
         DS    CL3                                                              
LSTOFC   DS    CL2                                                              
         DS    CL1                                                              
LSTCLI   DS    CL6                                                              
         DS    CL1                                                              
LSTPRO   DS    CL6                                                              
         DS    CL3                                                              
LSTMGR   DS    CL1                                                              
         DS    CL3                                                              
LSTMED   DS    CL1                                                              
         DS    CL2                                                              
LSTNUM   DS    CL19                                                             
         DS    CL1                                                              
LSTAMT   DS    CL12                                                             
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROBFD                                                       
         EJECT                                                                  
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
LLASTAUT DS    CL(L'SELKEY)        LAST AUTHORIZATION ON SCREEN                 
LSELTAB  DS    CL(NLINES*SELTABL)                                               
SAVEKEY  DS    CL48                SAVE THE KEY                                 
*                                                                               
*                                                                               
NLINES   EQU   15                  N'LINES PER SCREEN                           
NFIELDS  EQU   2                   N'FIELDS PER LINE                            
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
*                                                                               
QFILTS   DS    0C                                                               
QOGR     DS    CL(L'AUTKOGR)                                                    
QOFC     DS    CL(L'AUTKOFC)                                                    
QCLI     DS    CL(L'AUTKCLI)                                                    
QPRO     DS    CL(L'AUTKPRO)                                                    
QMGR     DS    CL(L'AUTKMGR)                                                    
QMED     DS    CL(L'AUTKMED)                                                    
QSTA     DS    CL(L'AUTKNUM)                                                    
QSTAT    DS    CL(L'AUTRSTA)                                                    
QFILTLN  EQU   *-QFILTS                                                         
*                                                                               
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
APROT    DS    A                                                                
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
**PAN#1  DC    CL21'006ACPRO65   08/22/02'                                      
         END                                                                    
