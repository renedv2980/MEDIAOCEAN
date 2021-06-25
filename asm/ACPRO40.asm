*          DATA SET ACPRO40    AT LEVEL 007 AS OF 04/24/07                      
*PHASE T60B40A                                                                  
         TITLE 'T60B40 - SCHEME LIST'                                           
T60B40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B40**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
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
RECLOGIC GOTO1 DICTATE,DMCB,C'LU  ',DDIN,DDOUT                                  
         BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
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
         XC    LLASTSCH,LLASTSCH   CLEAR OUT LAST SCHEME LISTED                 
         BAS   RE,READLST                                                       
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    FULLMESS            YES                                          
         CLI   INTMODE,FSTLIST     IS THIS THE FIRST TIME?                      
         BNE   *+12                NO                                           
         CLI   NUMLINES,0          YES, IS SCREEN EMPTY ?                       
         BE    NONEMESS            YES                                          
         LA    R2,SCHSTRTH         POSITION CURSOR AT FIRST KEY FIELD           
         XC    LLASTSCH,LLASTSCH   CLEAR LAST SCHEME LISTED                     
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
         LA    R2,SCHSTRTH         OPTIONAL START FIELD                         
         BAS   RE,TSTKEY                                                        
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    VALHEDX                                                          
         GOTO1 ANY                                                              
         MVC   QSTRT,WORK                                                       
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
         BNE   PROCPFX                                                          
         L     RE,ATWA                                                          
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
         USING ACSHKEY,R6                                                       
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         GOTO1 VCALL,WORK,=C'CATEGORY',=C'MAINT',(8,ACSHCODE),0                 
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
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    READL020                                                         
         MH    R3,=Y(MAXFIELD)                                                  
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
READL020 ST    R2,ATHISLIN         SAVE LINE ADDRESS                            
         LA    R6,KEY                                                           
         USING ACSHKEY,R6                                                       
         XC    ACSHKEY,ACSHKEY                                                  
         OC    LLASTSCH,LLASTSCH   IS THIS A CONTINUATION ?                     
         BNZ   READL040            YES                                          
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         CLI   SCHSTRTH+5,0                                                     
         BE    *+10                                                             
         MVC   ACSHCODE,QSTRT                                                   
         B     READL060                                                         
*                                                                               
READL040 MVC   ACSHKEY(L'LLASTSCH),LLASTSCH                                     
         MVI   ACSHCODE+7,X'FF'                                                 
*                                                                               
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
READL060 GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
         B     READL100                                                         
*                                                                               
READL080 GOTO1 SEQ                                                              
         GOTO1 CATCHIOS                                                         
*                                                                               
READL100 CLC   KEY(5),KEYSAVE      EXIT IF COMPANY CHANGES                      
         BNE   XIT                                                              
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLIN           SET ADDRESSES                                
         BAS   RE,DISLINE          DISPLAY RECORD                               
         MVC   LLASTSCH,ACSHKEY    SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         MH    R5,=Y(SELTABL)                                                   
         LA    R5,LSELTAB(R5)      ADDRESS TABLE ENTRY                          
         USING SELTABD,R5                                                       
         MVC   SELKEY,ACSHKEY                                                   
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BNE   READL080            NO, GET NEXT                                 
READLX   B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* DISPLAY A LINE OF DATA                                                        
*                                                                               
DISLINE  NTR1                                                                   
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         L     R6,AIO                                                           
         USING SCHRECD,R6                                                       
*                                                                               
         L     R2,ABOE                                                          
         MVC   8(L'SCHBOE,R2),SPACES                                            
         TM    SCHRSTA,SCHSMCSO                                                 
         BZ    *+10                                                             
         MVC   8(L'SCHBOE,R2),AC@ONLY                                           
         TM    SCHRSTA,SCHSMCSN                                                 
         BZ    *+10                                                             
         MVC   8(L'SCHBOE,R2),AC@NO                                             
*                                                                               
         L     R2,ACODE                                                         
         MVC   8(L'SCHCODE,R2),SCHKCODE                                         
*                                                                               
         MVI   ELCODE,ACSDELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISLINEX                                                         
         USING ACSDD,R6                                                         
         L     R2,ANAME                                                         
         MVC   8(L'SCHNAM,R2),ACSDNAME                                          
DISLINEX B     XIT                                                              
         DROP  R6                                                               
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
         BE    EDT8                                                             
         CLI   5(R2),1             IF IN SELECT, MUST BE 1 BYTE                 
         BNE   INVEND                                                           
         CLI   8(R2),C'*'          IF ALREADY EDITED GET NEXT LINE              
         BE    EDT8                                                             
         CLI   8(R2),C'C'          CHECK FOR COPY                               
         BE    EDT6                                                             
         CLI   8(R2),C'S'          IF OTHER THAN 'S' - ERROR                    
         BNE   INVEND                                                           
*                                                                               
EDT4     MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R6,SELKEY                                                        
         USING ACSHKEY,R6                                                       
         GOTO1 VCALL,WORK,=C'SCHEME',=C'CHANGE',(8,ACSHCODE),0                  
         B     EDT8                                                             
*                                                                               
EDT6     MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R6,SELKEY                                                        
         USING ACSHKEY,R6                                                       
         GOTO1 VCALL,WORK,=C'SCHEME',=C'COPY',(8,ACSHCODE),0                    
         B     EDT8                                                             
*                                                                               
EDT8     L     R2,ANEXTSEL                                                      
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
         LA    R2,SCHSEL1H                                                      
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
         GOTO1 VERRCUR                                                          
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NONEMSG  DC    CL50'NO DATA TO DISPLAY'                                         
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
DDIN     DS    0C                                                               
         DCDDL AC#ONLY,4                                                        
         DCDDL AC#NO,4                                                          
         DC    X'00'                                                            
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
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROB0D                                                       
         DS    0F                                                               
*                                                                               
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
LLASTSCH DS    CL(L'SELKEY)        LAST JOB ON SCREEN                           
LSELTAB  DS    CL(MAXLINE*SELTABL)                                              
*                                                                               
*                                                                               
MAXLINE  EQU   15                  MAXIMUM N'LINES PER SCREEN                   
MAXFIELD EQU   4                   MAXIMUM N'FIELDS PER LINE                    
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
*                                                                               
QSTRT    DS    CL(L'SCHCODE)                                                    
*                                                                               
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
ACODE    DS    A                   A(CODE FIELD)                                
ANAME    DS    A                   A(NAME FIELD)                                
ABOE     DS    A                   A(BRAND FIELD)                               
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELKEY   DS    CL(ACSHCODE-ACSHKEY+8)                                           
SELTABL  EQU   *-SELTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACPRO40   04/24/07'                                      
         END                                                                    
