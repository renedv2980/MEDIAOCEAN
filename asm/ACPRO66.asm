*          DATA SET ACPRO66    AT LEVEL 016 AS OF 07/23/13                      
*PHASE T60B66C                                                                  
*INCLUDE TIMEOUT                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B66 - FUNDING'                                               
T60B66   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B66**,R8                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         LA    RE,BUFF                                                          
         ST    RE,AJOBTAB                                                       
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
*                                                                               
VKEY     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    SVFLDS,SVFLDS       CLEAR KEY SAVE AREA                          
*                                                                               
         L     RE,ATIA             USE TIA FOR JOBBER BUFFER                    
         ST    RE,ACOLTAB                                                       
         L     RF,=A(LENTIA)                                                    
         SRL   RF,1                SPLIT TIA IN HALF                            
         ST    RF,LCOLTAB                                                       
         ST    RF,LOPVTAB                                                       
         LA    RE,0(RF,RE)                                                      
         ST    RE,AOPVTAB                                                       
*                                                                               
         BAS   RE,VALHED           VALIDATE SCREEN HEADER                       
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       IS RECORD/ACTION CHANGE ?                    
         BNE   *+8                 NO                                           
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         DID KEY FIELDS CHANGE ?                      
         BNE   *+8                 NO                                           
         MVI   INTMODE,FSTLIST     YES, SET FIRST TIME LIST                     
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    CLEARNEW,CLEARNEW                                                
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A50'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QSORT,DMCB                                                       
*                                                                               
         CLI   KEYCHG,C'Y'         HAS KEY CHANGED?                             
         BNE   VKEYX               NO                                           
         BAS   RE,GETTAB           GET THE JOB TABLE                            
         L     RE,AJOBTAB          AND CLEAR IT OUT                             
         LH    RF,=Y(L'BUFF)                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,PUTTAB                                                        
*                                                                               
VKEYX    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
*                                                                               
VREC     BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         CLI   INTMODE,FSTLIST     IS THIS FIRST TIME LIST ?                    
         BE    VREC02              YES                                          
         BAS   RE,PROCPF           NO, PROCESS PFKEYS                           
         BE    VREC04              FORCE EDIT IF PF10                           
         BAS   RE,TSTEDT           ANYTHING TO EDIT ?                           
         BE    VREC04              YES                                          
*                                                                               
VREC02   BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
*                                                                               
VREC04   BAS   RE,EDT              EDIT SCREEN AND HANDLE DELETES               
         CLI   UPDATE,C'Y'                                                      
         BNE   VREC06                                                           
         MVI   INTMODE,FSTLIST                                                  
         BAS   RE,PUTTAB                                                        
         BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
*                                                                               
VREC06   L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVI   INTMODE,DISLIST                                                  
         MVC   NXTSEQ,FSTSEQ                                                    
         BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO VALIDATE THE HEADING FIELD(S)                         *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         MVC   AIO,AIO2            USE AIO2 FOR THIS                            
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
*                                                                               
         LA    R2,FNDOGRH          OFFICE GROUP                                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED2                                                            
         CLC   =C'ALL',8(R2)       TEST FOR ALL                                 
         BE    VHED2                                                            
         GOTO1 VALOG                                                            
         MVC   SVOGR,EFFOFG                                                     
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VHED2    LA    R2,FNDOFCH          OFFICE                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   FNDOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   SVOFC,EFFOFFC                                                    
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VHED4    LA    R2,FNDCLIH          CLIENT                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   FNDOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   FNDOFCH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         MVC   SVCLI,CLICODE                                                    
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VHED6    LA    R2,FNDPROH          PRODUCT                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   FNDCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   SVPRO,PRODCODE                                                   
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VHED8    LA    R2,FNDMGRH          MEDIA GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED10                                                           
         GOTO1 VALMG                                                            
         MVC   SVMGR,MGROUP                                                     
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VHED10   LA    R2,FNDMEDH          MEDIA                                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VHED12                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   FNDMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         MVC   SVMED,MEDIA                                                      
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
         MVC   AIO,AIO1            SWAP BACK IO AREA                            
*                                                                               
         USING FUNRECD,R6                                                       
VHED12   LA    R6,KEY                                                           
         MVC   FUNKEY,BLANKS                                                    
         MVI   FUNKTYP,FUNKTYPQ                                                 
         MVI   FUNKSUB,FUNKSUBQ                                                 
         MVC   FUNKCPY(L'CUL),CUL                                               
         MVC   FUNKOGR,SVOGR                                                    
         MVC   FUNKOFC,SVOFC                                                    
         MVC   FUNKCLI,SVCLI                                                    
         MVC   FUNKPRO,SVPRO                                                    
         MVC   FUNKMGR,SVMGR                                                    
         MVC   FUNKMED,SVMED                                                    
*                                                                               
         LA    R2,FNDNUMH          THIS IS THE ONLY REQUIRED FIELD              
         GOTO1 ANY                                                              
         BAS   RE,TSTKEY                                                        
         MVC   FUNKNUM,WORK                                                     
         MVC   SVNUM,FUNKNUM                                                    
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
         CLI   KEYCHG,C'Y'         DID SOMETHING CHANGE ?                       
         BNE   VHEDX               NO, EXIT                                     
         XC    CLEARNEW,CLEARNEW   YES, CLEAR FIELDS                            
*                                                                               
VHEDX    BAS   RE,BLDCOLS          SET UP JOBBER COLUMNS                        
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIRST SELECT LINE, PF LINE AND END OF SCREEN       *         
***********************************************************************         
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,FNDSEL1H                                                      
         ST    R2,AFSTSEL                                                       
         LA    R0,MAXLINE                                                       
         LA    R1,MAXFIELD                                                      
         MR    R0,R0               LINES X FIELDS = # TO BUMP                   
*                                                                               
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
         ST    R2,APFFLD           SAVE ADDRESS OF PFKEYS                       
         BAS   RE,BUMP                                                          
*                                                                               
         ST    R2,AENDSCR          SAVE ADDRESS OF END-OF-SCREEN                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO PROCESS PFKEYS                                        *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         L     R2,AFSTSEL                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    PROCPFN             NOTHING ON SCREEN TO PROCESS                 
*                                                                               
PROCPF9  CLI   PFKEY,PF9                                                        
         BNE   PROCPF8                                                          
         OC    LLASTSCH,LLASTSCH                                                
         BNZ   NOTLAST                                                          
         MVI   INTMODE,NEWLIST                                                  
         B     PROCPFN                                                          
*                                                                               
PROCPF8  CLI   PFKEY,PF8           SCROLL FORWARDS                              
         BNE   PROCPF7                                                          
         SR    R3,R3                                                            
         ICM   R3,3,FSTSEQ                                                      
         LA    R1,MAXLINE                                                       
         MHI   R1,20                                                            
         AR    R3,R1               ADD ADD MAX TO STARTING DISP                 
         SR    R1,R1                                                            
         ICM   R1,3,LSTSEQ                                                      
         CR    R3,R1               IF HIGHER THAN LAST SEQUENCE                 
         BNH   *+8                 FORCE DISPLACEMENT OF 0                      
         LA    R3,0                                                             
         B     PROCPF78                                                         
*                                                                               
PROCPF7  CLI   PFKEY,PF7           SCROLL BACKWARDS                             
         BNE   PROCPFN                                                          
         SR    R3,R3                                                            
         ICM   R3,3,FSTSEQ                                                      
         LA    R1,MAXLINE                                                       
         MHI   R1,20                                                            
         SR    R3,R1               SUBTRACT FROM STARTING DISPLACEMENT          
         BNM   *+8                 IF NEGATIVE, FORCE DISPLACEMENT OF 0         
         LA    R3,0                                                             
*                                                                               
PROCPF78 STCM  R3,3,NXTSEQ                                                      
         MVI   INTMODE,DISLIST                                                  
*                                                                               
PROCPFN  LTR   R8,R8                                                            
         B     PROCPFX                                                          
*                                                                               
PROCPFY  CR    R8,R8                                                            
*                                                                               
PROCPFX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                  *         
* ON EXIT: CC=EQ IF EDIT NEEDED; CC=NEQ IF EDIT NOT NEEDED            *         
***********************************************************************         
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    TSTE080                                                          
*                                                                               
TSTE020  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTE040             NO, TEST IF ANYTHING ELSE WAS                
         CLI   8(R2),C'*'          YES, WAS IT ALREADY PROCESSED ?              
         BE    TSTE040             YES, TEST FOR ANYTHING ELSE                  
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
*                                                                               
TSTE040  LA    R1,MAXFIELD-1       GET NUMBER OF FIELDS                         
*                                                                               
TSTE060  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BCT   R1,TSTE060          NO, TRY NEXT FIELD                           
         BAS   RE,BUMP             THIS LINE DONE, TRY NEXT                     
         BCT   R3,TSTE020                                                       
*                                                                               
TSTE080  SR    R3,R3               TEST FOR NEW DATA                            
         ICM   R3,1,NEWLINES                                                    
         BZ    TSTEDTN                                                          
*                                                                               
TSTE100  CLI   5(R2),0             TEST THESE LINES TO SEE IF FIELD             
         BE    TSTE120              IS PROTECTED OR WAS INPUT RATHER            
         CLI   8(R2),C'*'           THAN IF IT WAS VALIDATED                    
         BE    TSTE120                                                          
         B     TSTEDTY                                                          
*                                                                               
TSTE120  LA    R1,MAXFIELD-1                                                    
*                                                                               
TSTE140  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    TSTEDTY                                                          
         BCT   R1,TSTE140                                                       
         BAS   RE,BUMP                                                          
         BCT   R3,TSTE100                                                       
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN, READ RECORDS AND DISPLAY THEM. MAKE ANY UNUSED LINES  *         
* AVAILABLE FOR ADDS.                                                 *         
***********************************************************************         
*                                                                               
DISLOGIC NTR1                                                                   
         MVC   AIO,AIO2            USE OTHER IO AREA                            
*                                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AUTKEY,BLANKS                                                    
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
         MVC   AUTKOGR,SVOGR                                                    
         MVC   AUTKOFC,SVOFC                                                    
         MVC   AUTKCLI,SVCLI                                                    
         MVC   AUTKPRO,SVPRO                                                    
         MVC   AUTKMGR,SVMGR                                                    
         MVC   AUTKMED,SVMED                                                    
*                                                                               
         LA    R2,FNDNUMH          THIS IS THE ONLY REQUIRED FIELD              
         GOTO1 ANY                                                              
         MVC   AUTKNUM,WORK                                                     
*                                                                               
         GOTO1 READ                                                             
*                                                                               
         LA    R2,FNDAUTDH         PRINT THE DESCRIPTION                        
         GOTO1 NAMEOUT                                                          
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         USING AUTHELD,R6                                                       
         MVI   ELCODE,AUTHELQ      GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ONE                           
         ST    R6,SAVER6                                                        
*                                                                               
DISL010  BAS   RE,NEXTEL           ANY MORE?                                    
         BNE   *+12                NO                                           
         ST    R6,SAVER6           YES, SAVE ADDRESS AND LOOK AGAIN             
         B     DISL010                                                          
*                                                                               
         L     R6,SAVER6           GET ADDRESS OF LAST AUTHEL                   
         LA    R2,FNDTAUTH                                                      
         CURED (P6,AUTHAMT),(12,8(R2)),2,ALIGN=RIGHT                            
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
         ZAP   TAUTH,AUTHAMT       SAVE AS AVAILABLE AMOUNT                     
*                                                                               
         MVC   AIO,AIO1            SWAP BACK                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 VCLEARF,DMCB,AFSTSEL,APFFLD     CLEAR FIELDS AND TURN            
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD    ON TRANSMIT BIT                  
         MVI   NUMLINES,0                                                       
         MVI   NEWLINES,0                                                       
*                                                                               
         CLI   INTMODE,NEWLIST                                                  
         BNE   DISL020                                                          
         SR    R1,R1               IF NEW SCREEN REQUESTED, GET TO              
         ICM   R1,3,LSTSEQ          END MARKER AND UNPROTECT LINES              
         LA    R1,1(R1)                                                         
         STCM  R1,3,FSTSEQ                                                      
         MVI   INTMODE,EDTLIST                                                  
         MVC   CONHEAD(L'NEWMSG),NEWMSG                                         
         B     DISL080                                                          
*                                                                               
DISL020  CLI   INTMODE,FSTLIST     IF FIRST TIME, CLEAR LAST SCHEME             
         BNE   DISL040                                                          
         XC    LLASTSCH,LLASTSCH                                                
*                                                                               
DISL040  BAS   RE,READFUN          READ FUND RECORD AND GET JOBS                
*                                                                               
         LA    R2,FNDTFUNH                                                      
         CURED (P6,TFUNDED),(12,8(R2)),2,ALIGN=RIGHT                            
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         ZAP   TAVAIL,TAUTH                                                     
         SP    TAVAIL,TFUNDED                                                   
*                                                                               
         LA    R2,FNDTAVAH                                                      
         CURED (P6,TAVAIL),(12,8(R2)),2,ALIGN=RIGHT                             
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    IS SCREEN IS FULL ?                          
         BNE   DISL060             NO                                           
         CLC   NXTSEQ,LSTSEQ       YES, ANY SEQUENCE CODES LEFT                 
         BL    FULLMESS            YES, INDICATE MORE TO COME                   
*                                                                               
DISL060  SR    R3,R3               PRINT MESSAGE AND CLEAR LAST                 
         ICM   R3,1,NUMLINES        SCHEME                                      
         BNZ   *+8                                                              
         LA    R2,FNDOGRH                                                       
         XC    LLASTSCH,LLASTSCH                                                
         MVC   CONHEAD(L'DONEMSG),DONEMSG                                       
*                                                                               
DISL080  L     R2,AFSTSEL          CLEAR BLANK LINES, IS SPACE                  
         LA    R3,MAXLINE           AVAILABLE                                   
         SR    R1,R1                                                            
         ICM   R1,1,NUMLINES                                                    
         BZ    DISL100                                                          
*                                                                               
         SR    R3,R1                                                            
         BNP   DISLOGX                                                          
*                                                                               
         MH    R1,=Y(MAXFIELD)     GET TO NEXT AVAILABLE LINE                   
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
DISL100  STC   R3,NEWLINES                                                      
*                                                                               
DISL120  BAS   RE,SETLINE          IF LAST SCREEN, UNPROTECT REMAINING          
         L     R2,ASEL              LINES (ALL IF INTMODE=NEWLIST)              
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,AACCT                                                         
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,ANEXTSEL                                                      
         BCT   R3,DISL120                                                       
*                                                                               
         L     R2,AFSTSEL                                                       
*                                                                               
DISLOGX  ST    R2,ACURFORC                                                      
         B     XIT                                                              
         SPACE 3                                                                
FULLMESS MVC   CONHEAD(L'FULLMSG),FULLMSG                                       
         B     DISLOGX                                                          
         EJECT                                                                  
***********************************************************************         
* UPDATE FUND RECORD FROM JOBTABLE                                    *         
***********************************************************************         
*                                                                               
UPFUND   NTR1                                                                   
         MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
         USING FUNRECD,R6                                                       
         LA    R6,KEY              GET ORIGINAL FUND RECORD                     
         MVC   FUNKEY,SVFUNKEY                                                  
         MVC   KEYSAVE,FUNKEY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
*                                                                               
         L     R6,AIO                                                           
         CLC   KEYSAVE(L'FUNKEY),0(R6)                                          
         BE    *+6                 IT MUST BE THERE - READ IT BEFORE            
         DC    H'0'                                                             
*                                                                               
         L     R5,AJOBTAB                                                       
         USING JOBTABD,R5                                                       
         ICM   R4,15,JOBCNT                                                     
         BZ    UPFUN01                                                          
         GOTO1 QSORT,DMCB,(R5),(R4),JOBTABL,L'JOBTJOB,0                         
*                                                                               
UPFUN01  LA    R1,0                INDEX INTO JOB TABLE                         
         MVI   ORIGFUND,C'Y'       SET FOR FIRST TIME                           
*                                                                               
UPFUN02  BAS   RE,GET0B0C          DELETE X'0B'& X'0C' & ADD NEW                
         LA    R6,ELEMENT          GET READY FOR NEW JOB ELEMENTS               
         USING FJNELD,R6                                                        
         LA    R0,MAXELS           MAXIMUM NUMBER OF ELS ON A RECORD            
*                                                                               
UPFUN04  OC    JOBTJOB,JOBTJOB                                                  
         BZ    UPFUN06                                                          
         MVC   FJNJOB,JOBTJOB      ADD THE JOB TO THE ELEMENT                   
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         BCTR  R0,0                                                             
*                                                                               
UPFUN06  LA    R5,JOBTABL(R5)      GET NEXT TABLE ENTRY                         
         LA    R1,JOBTABL(R1)                                                   
         CLI   0(R5),X'FF'         AT END OF TABLE?                             
         BE    UPFUN08             YES, WRITE IT OUT                            
         CHI   R0,0                NO, ANY ROOM LEFT?                           
         BNE   UPFUN04             YES                                          
*                                                                               
         BAS   RE,WRITEIT          NO, WRITE THE RECORD BACK                    
*                                                                               
         USING FUNRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   FUNKEY,KEYSAVE                                                   
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMRSEQ'),SYSFIL,KEY,AIO,0                 
*                                                                               
         L     R6,AIO                                                           
         CLC   0(L'FUNKEY-1,R6),KEYSAVE                                         
         BNE   UPFUN12             ADD A NEW FUND RECORD                        
*                                                                               
         MVC   KEYSAVE,FUNKEY                                                   
         NI    FUNRSTA,X'FF'-X'80' UNDELETE IT                                  
         MVI   ORIGFUND,C'N'                                                    
         B     UPFUN02             DELETE ELEMENTS ETC                          
*                                                                               
UPFUN08  CLI   ORIGFUND,C'Y'                                                    
         BE    UPFUN09                                                          
         MVI   ELCODE,FJNELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE ANY ELEMENTS?                     
         BE    UPFUN09             YES                                          
         L     R6,AIO                                                           
         OI    FUNRSTA,X'80'       NO, MARK IT DELETED                          
*                                                                               
UPFUN09  BAS   RE,REWRITE          RE-WRITE THE RECORD                          
*                                                                               
UPFUN10  GOTO1 DATAMGR,DMCB,(X'80',=C'DMRSEQ'),SYSFIL,KEY,AIO,0                 
         L     R6,AIO                                                           
         CLC   0(L'FUNKEY-1,R6),KEYSAVE                                         
         BNE   UPFUNX              NO MORE, DONE                                
         OI    FUNRSTA,X'80'       MARK DELETED                                 
         BAS   RE,WRITEIT                                                       
         B     UPFUN10                                                          
*                                                                               
         USING FUNRECD,R6                                                       
UPFUN12  LA    R6,KEY              GET ORIGINAL FUND RECORD                     
         MVC   FUNKEY,KEYSAVE      START WITH LAST KEY READ                     
         SR    R1,R1                                                            
         IC    R1,FUNKSEQ          GET LAST SEQUENCE NUMBER                     
         LA    R1,1(R1)            BUMP IT UP                                   
         STC   R1,FUNKSEQ          STORE IT BACK                                
         MVC   KEYSAVE,FUNKEY                                                   
*                                                                               
         L     R2,AIO2             CLEAR THE IO AREA                            
         LA    R3,2000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   FUNKEY,KEYSAVE                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',SYSFIL,KEY,AIO,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
*                                                                               
         MVI   ORIGFUND,C'N'                                                    
         L     R6,AIO                                                           
         CLC   KEYSAVE(L'FUNKEY),0(R6)                                          
         BE    UPFUN02             IT MUST BE THERE - READ IT BEFORE            
         DC    H'0'                                                             
*                                                                               
UPFUNX   MVC   AIO,AIO2            RESTORE IO AREA FOR EDT                      
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ THE FUND RECORD AND GET JOBS                     *         
***********************************************************************         
*                                                                               
READFUN  NTR1                                                                   
         BAS   RE,GETTAB                                                        
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    READF02                                                          
         MH    R3,=Y(MAXFIELD)                                                  
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
READF02  ST    R2,ATHISLIN         SAVE LINE ADDRESS                            
         LA    R6,KEY                                                           
         SR    R7,R7                                                            
         ICM   R7,3,NXTSEQ                                                      
         CLI   INTMODE,DISLIST                                                  
         MVI   INTMODE,FSTLIST                                                  
         BE    READF06                                                          
         OC    LLASTSCH,LLASTSCH   IS THIS A CONTINUATION ?                     
         BZ    READF04             NO, DO FIRST TIME LOGIC                      
         STCM  R7,3,FSTSEQ         SAVE STARTING DISPLACEMENT                   
         L     R4,AJOBTAB                                                       
         LA    R4,0(R4,R7)         AND ADDRESS THE TABLE                        
         B     READF18                                                          
*                                                                               
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
*                                                                               
         USING FUNKEY,R6                                                        
READF04  LA    R7,0                FORCE DISPLACEMENT OF 0 FIRST TIME           
         ZAP   TFUNDED,=P'0'       CLEAR TOTAL FUNDED                           
*                                                                               
READF06  STCM  R7,3,FSTSEQ                                                      
         L     R4,AJOBTAB                                                       
         LA    R4,0(R4,R7)                                                      
         LA    R2,FUNKOGR          ADDRESS 1ST KEY FIELD                        
         MVC   FUNKEY,BLANKS                                                    
         MVI   FUNKTYP,FUNKTYPQ                                                 
         MVI   FUNKSUB,FUNKSUBQ                                                 
         MVC   FUNKCPY(L'CUL),CUL                                               
         MVC   FUNKOGR,SVOGR                                                    
         MVC   FUNKOFC,SVOFC                                                    
         MVC   FUNKCLI,SVCLI                                                    
         MVC   FUNKPRO,SVPRO                                                    
         MVC   FUNKMGR,SVMGR                                                    
         MVC   FUNKMED,SVMED                                                    
         MVC   FUNKNUM,SVNUM                                                    
         MVC   SVFUNKEY,FUNKEY                                                  
         GOTO1 READ                                                             
*                                                                               
         USING JOBTABD,R3                                                       
         L     R3,AJOBTAB                                                       
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
         XC    JOBCNT,JOBCNT                                                    
         B     READF10                                                          
*                                                                               
READF08  GOTO1 SEQ                 GET THE NEXT                                 
         L     R6,AIO                                                           
         CLC   0(L'FUNKEY-1,R6),SVFUNKEY                                        
         BNE   READF16             ALL DONE, SAVE THE TABLE                     
*                                                                               
         USING FUNELD,R6                                                        
READF10  MVI   ELCODE,FUNELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   READF12                                                          
         ZAP   TFUNDED,FUNAMT      GET TOTAL FUNDED                             
*                                                                               
READF12  MVI   ELCODE,FJNELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE ANY JOBS YET?                     
         BNE   READFX              NO                                           
*                                                                               
         USING FJNELD,R6                                                        
READF14  MVC   JOBTJOB,FJNJOB                                                   
         LA    R3,JOBTABL(R3)                                                   
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
*                                                                               
         L     RE,JOBCNT           BUMP JOB COUNT                               
         LA    RE,1(RE)                                                         
         ST    RE,JOBCNT                                                        
*                                                                               
         LR    R1,R3               AND UPDATE IT'S DISPLACMENT                  
         L     RF,AJOBTAB                                                       
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         STCM  R1,3,LSTSEQ                                                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    READF14             ADD NEXT TO TABLE                            
         B     READF08                                                          
*                                                                               
READF16  BAS   RE,PUTTAB                                                        
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY              READ EACH JOB NOW                            
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCPY(L'CUL),CUL                                               
         MVC   ACTKACT,0(R4)                                                    
         B     READF20                                                          
*                                                                               
READF18  CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    READFX              YES, SAVE REGISTERS                          
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKEY(L'LLASTSCH),LLASTSCH                                      
         MVC   ACTKACT,0(R4)                                                    
*                                                                               
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
*                                                                               
READF20  OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTO1 CATCHIOS                                                         
*                                                                               
         CLC   KEY(15),KEYSAVE     WE MUST HAVE RECORD IF IT IS                 
         BE    *+6                  SEQUENCE ELEMENT                            
         DC    H'0'                                                             
         MVC   LLASTSCH,ACTKEY     SAVE ACCOUNT KEY                             
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLINE          SET ADDRESSES                                
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
*                                                                               
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,NUMLINES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
*                                                                               
         LA    R7,JOBTABL(R7)       INCREMENT DISPLACEMENT AND GET NEXT         
         L     R4,AJOBTAB                                                       
         LA    R4,0(R4,R7)          SEQUENCE, IF ANY                            
         CLI   0(R4),X'FF'                                                      
         BNE   READF18                                                          
*                                                                               
READFX   STCM  R7,3,NXTSEQ         SAVE NEXT STARTING DISPLACEMENT              
         BAS   RE,PUTTAB                                                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ X'0D' ELEMENT AND DISPLAY DATA. ALSO REDISPLAYS  *         
* SELECT FIELD DATA IF ANY.                                           *         
* ADDS AMOUNT AND ESTIMATE # TO BUFF (R4)                             *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
         USING JOBTABD,R4                                                       
         USING ACTRECD,R6                                                       
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SELECT VALIDATED                             
         NI    1(R2),X'DF'         UNPROTECT SELECT FIELD                       
*                                                                               
         L     R6,AIO                                                           
         L     R2,AACCT                                                         
         OI    4(R2),X'20'         CODE VALIDATED                               
         OI    1(R2),X'20'         AND PROTECTED ON DISPLAY                     
         MVC   8(L'ACTKACT,R2),ACTKACT                                          
*                                                                               
         L     R2,ANAME                                                         
         GOTO1 NAMEOUT                                                          
*                                                                               
         USING JFNELD,R6                                                        
         MVI   ELCODE,JFNELQ                                                    
         BAS   RE,GETELIO          GET FUNDING DATA                             
         BNE   DISPLAYX                                                         
*                                                                               
         L     R2,AAMNT                                                         
         CURED (P6,JFNAMT),(12,8(R2)),2,ALIGN=RIGHT                             
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,AEST#                                                         
         MVC   8(1,R2),JFNEST                                                   
         EDIT  (B1,JFNEST+1),(3,9(R2)),ALIGN=LEFT                               
*                                                                               
         MVC   JOBTAMT,JFNAMT                                                   
         MVC   JOBTEST,JFNEST                                                   
*                                                                               
DISPLAYX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT THE SCREEN.                                      *         
***********************************************************************         
*                                                                               
EDT      NTR1                                                                   
         BAS   RE,GETTAB           GET THE JOB TABLE                            
         MVI   UPDATE,C'N'         CLEAR UPDATE SWITCH                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 SETHEIR                                                          
*                                                                               
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R7,R7               GET STARTING SEQUENCE CODE                   
         ICM   R7,3,FSTSEQ                                                      
         L     R4,AJOBTAB                                                       
         LA    R7,0(R4,R7)                                                      
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES       NUMBER OF LINES ON SCREEN                    
         BZ    EDT180                                                           
*                                                                               
EDT020   BAS   RE,SETLINE          GET FIELD ADDRESSES FOR THIS LINE            
*        LA    R6,SELKEY                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT160                                                           
         CLI   8(R2),C'*'                                                       
         BE    EDT160                                                           
         CLI   8(R2),C'D'                                                       
         BNE   INVEND                                                           
*                                                                               
         USING ACTKEY,R6                                                        
EDT100   LA    R6,KEY              READ AND UPDATE FOR DELETIONS                
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCULA,CUL                                                     
         MVC   ACTKACT,0(R7)                                                    
         MVC   KEYSAVE,ACTKEY                                                   
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
         L     RE,AIO                                                           
         MVC   KEY,0(RE)                                                        
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,RSTELQ       GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ABLELD,R6                                                        
         MVI   ELCODE,ABLELQ       GET BALANCE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    ABLCR,=P'0'         CHECK IF ANY CREDITS                         
         BNE   BILLERR             YES, CAN'T DELETE IT                         
*                                                                               
EDT120   MVI   ELCODE,JFNELQ       GET JOB FUNDING ELEMENT                      
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
*                                                                               
         USING JFNELD,R6                                                        
         SP    TFUNDED,JFNAMT      UPDATE TOTAL FUNDED AMOUNT                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'0D',AIO)                             
*                                                                               
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
*                                                                               
         USING JOBELD,R6                                                        
         NI    JOBSTA2,X'FF'-JOBSFUN                                            
         BAS   RE,WRITEIT          UPDATE THE JOB RECORD                        
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         XC    0(JOBTABL,R7),0(R7)                                              
*                                                                               
         L     RE,JOBCNT                                                        
         BCTR  RE,0                                                             
         ST    RE,JOBCNT                                                        
*                                                                               
         MVC   9(1,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'                                                      
         BAS   RE,UPFUND           UPDATE THE FUND RECORD NOW                   
*                                                                               
EDT160   L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         LA    R7,JOBTABL(R7)      GET NEXT JOB CODE                            
         BCT   R3,EDT020                                                        
*                                                                               
EDT180   SR    R3,R3               EDIT THE NEW LINES                           
         ICM   R3,1,NEWLINES                                                    
         BZ    EDTX                                                             
*                                                                               
EDT200   BAS   RE,SETLINE                                                       
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT320                                                           
         CLI   8(R2),C'*'                                                       
         BE    EDT280                                                           
         CLI   8(R2),C'N'                                                       
         BNE   INVEND                                                           
         L     R2,AACCT                                                         
         CLI   5(R2),0                                                          
         BE    NOCODE                                                           
*                                                                               
EDT220   CLI   0(R7),X'FF'         MUST BE AT END OF JOB TABLE                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCPY(L'CUL),CUL                                               
         CLC   5(1,R2),LCLIPRO                                                  
         BNH   INVEND                                                           
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),8(R2)                                                 
*                                                                               
         BAS   RE,PUTTAB           SAVE IN CASE OF ERROR                        
         GOTO1 READ                READ THE NEW JOB                             
         MVC   AJOB,AIO            SAVE ADDRESS OF JOB                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ERROR,JOBFUND                                                    
         MVI   ELCODE,JFNELQ       SEE IF THERE IS A FUNDING ELEMENT?           
         BAS   RE,GETELIO                                                       
         BE    FUNDERR             YES, ERROR                                   
*                                                                               
         BAS   RE,LOOK             GET THE HIGHEST APPROVED REVISION            
*                                                                               
         MVI   ERROR,LESSFUND                                                   
         CP    TAVAIL,SVAMT        SEE IF WE CAN FUND IT?                       
         BL    ERREXIT             NO, NOT ENOUGH MONEY                         
         AP    TFUNDED,SVAMT       ADD TO TOTAL FUNDED                          
         SP    TAVAIL,SVAMT        SUBTRACT FROM AVAILABLE                      
*                                                                               
         BAS   RE,EDTJFN           BUILD THE JOB FUNDING ELEMENT                
*                                                                               
         USING JOBELD,R6                                                        
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE JOB ELEMENT                        
         OI    JOBSTA2,JOBSFUN     MARK IT FUNDED                               
*                                                                               
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         BAS   RE,WRITEIT          UPDATE THE RECORD                            
*                                                                               
         MVC   0(JOBTABL,R7),BLANKS        SET FLAG, UPDATE TABLE, ETC          
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),8(R2)       ADD ENTRY TO JOB TABLE                       
         MVI   JOBTABL(R7),X'FF'    MARK NEW END                                
*                                                                               
         L     RE,JOBCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,JOBCNT                                                        
*                                                                               
         SR    R4,R4                AND UPDATE IT'S DISPLACMENT                 
         ICM   R4,3,LSTSEQ                                                      
         LA    R4,JOBTABL(R4)                                                   
         STCM  R4,3,LSTSEQ                                                      
*                                                                               
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT280                                                           
         MVC   9(1,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
EDT280   MVI   UPDATE,C'Y'                                                      
         BAS   RE,UPFUND                                                        
         LA    R7,JOBTABL(R7)                                                   
*                                                                               
EDT300   L     R2,ANEXTSEL                                                      
         BCT   R3,EDT200                                                        
         B     EDTX                                                             
*                                                                               
EDT320   L     R1,AACCT            CODE MUST BE BLANK ALSO                      
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         B     EDT300                                                           
*                                                                               
EDTX     MVC   AIO,AIO1                                                         
         BAS   RE,PUTTAB                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO BUILD THE JOB FUNDING ELEMENT FOR ADD                 *         
***********************************************************************         
*                                                                               
EDTJFN   NTR1                                                                   
         USING JFNELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         MVI   JFNEL,JFNELQ                                                     
         MVI   JFNLN,JFNLNQ                                                     
*                                                                               
         MVC   JFNOGR,SVOGR                                                     
         MVC   JFNOFC,SVOFC                                                     
         MVC   JFNCLI,SVCLI                                                     
         MVC   JFNPRO,SVPRO                                                     
         MVC   JFNMGR,SVMGR                                                     
         MVC   JFNMED,SVMED                                                     
         MVC   JFNNUM,SVNUM        SET THE AUTHORIZATION NUMBER                 
         MVC   JFNSEQ,BLANKS                                                    
         MVC   JFNAMT,SVAMT        THE FUNDED AMOUNT                            
         MVC   JFNEST,SVEST        THE ESTIMATE NUMBER                          
*                                                                               
EDTJFNX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD THE JOBCOLS COLUMN LIST                        *         
***********************************************************************         
*                                                                               
BLDCOLS  NTR1  ,                                                                
         LA    R3,COLIST                                                        
         GOTO1 VJOBCOL,DMCB,FLDH,(R3),ACOMFACS                                  
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   BLDCOLSX            NO                                           
         DC    H'0'                                                             
BLDCOLSX B     XIT                                                              
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE'                                                            
         EJECT                                                                  
***********************************************************************         
* CALL JOBBER AND GET HIGHEST REVISION AMOUNT AND NUMBER              *         
***********************************************************************         
*                                                                               
LOOK     NTR1  ,                                                                
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK            CLEAR JOBLOCK                                
         LR    RE,R6                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R4,AIO                                                           
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVC   GOSELCLI,BLANKS                                                  
         IC    R1,LCLI                                                          
         BCTR  R1,0                                                             
         EXMVC R1,GOSELCLI,0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELPRO,BLANKS                                                  
         SR    R1,R1                                                            
         IC    R1,LPRO                                                          
         BCTR  R1,0                                                             
         EXMVC R1,GOSELPRO,0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELJOB,BLANKS                                                  
         IC    R1,LJOB                                                          
         BCTR  R1,0                                                             
         EXMVC R1,GOSELJOB,0(R4)                                                
*                                                                               
         LA    R1,COLIST                                                        
         ST    R1,JBACOLS                                                       
*                                                                               
         LA    RE,GOBLOCK                                                       
         ST    RE,JBAGOBLK                                                      
*                                                                               
         LA    RE,GOBLOCKX                                                      
         ST    RE,GOAEXT                                                        
*                                                                               
         MVC   JBAJOB,AJOB         COMPLETE THE JOBBLOCK                        
         MVC   JBACOM,ACOMFACS                                                  
*                                                                               
         MVC   JBAIO,AIO3          USE IO AREA 3                                
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         MVI   GOWHICH,C'N'        NEW OPTIONS ONLY                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0           TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
*                                                                               
         L     R5,JBACOLTB                                                      
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK                                                         
         MVI   ERROR,NOTAPPRV                                                   
         CLI   JBNEWEST,JBMCSQ                                                  
         BNE   LOOK2                                                            
         USING MJETABD,R5                                                       
         ZAP   SVAMT,MJETVAL(6)                                                 
         CLI   JBCURVER,0          ANY REVISIONS?                               
         BE    ERREXIT             NO                                           
         CLC   JBCURVER,JBHIAPP    IS THIS ESTIMATE APPROVED?                   
         BNE   ERREXIT             NO                                           
         MVI   SVEST,C'E'                                                       
         MVC   SVEST+1(1),JBCURVER YES, SAVE THE NUMBER                         
         B     LOOKX                                                            
         DROP  R5                                                               
*                                                                               
         USING JBCOLD,R5                                                        
LOOK2    ZAP   SVAMT,JBCOLVAL(6)                                                
         CLI   JBCURTYP,0          ANY REVISIONS?                               
         BE    ERREXIT             NO                                           
         CLC   JBCURVER,JBHIAPP    IS THIS ESTIMATE APPROVED?                   
         BNE   ERREXIT             NO                                           
         MVC   SVEST,JBCURTYP      YES, SAVE THE NUMBER                         
         DROP  R5                                                               
*                                                                               
LOOKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIELDS ON A LINE AND NEXT SELECT                   *         
* AT ENTRY R2 =A(SELECT FIELD HEADER)                                 *         
***********************************************************************         
*                                                                               
SETLINE  ST    RE,SAVERE                                                        
         LA    R0,MAXFIELD         NUMBER OF FIELDS PER LINE                    
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
*                                                                               
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR EACH FIELD                        
         BCT   R0,*-12                                                          
*                                                                               
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DELETE FUNDING DATA ELEMENT (X'0B') AND THE FUNDING    *         
* JOB ELEMENTS (X'0C'). IT WILL ADD A NEW FUNDING DATA ELEMENT (TO    *         
* THE ORIGINAL FUNDING RECORD) AND FORMAT A NEW JOB FUNDING ELEMENT   *         
***********************************************************************         
*                                                                               
GET0B0C  NTR1                                                                   
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,FUNELQ       DELETE FUNDING ELEMENT                       
         BAS   RE,GETELIO                                                       
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   ORIGFUND,C'Y'       IS THIS THE ORIGINAL FUNDING RECORD?         
         BNE   GET0B2              NO                                           
*                                                                               
         XC    ELEMENT,ELEMENT     YES, ADD A NEW FUNDING                       
         LA    R6,ELEMENT                                                       
         USING FUNELD,R6                                                        
         MVI   FUNEL,FUNELQ                                                     
         MVI   FUNLN,FUNLNQ                                                     
         ZAP   FUNAMT,TFUNDED                                                   
*                                                                               
         BAS   RE,GETDATE                                                       
         MVC   FUNDATE,4(R1)                                                    
         ZAP   FUNTIME,DUB                                                      
         GOTO1 ADDELEM             ADD NEW, UPDATED ONE                         
*                                                                               
GET0B2   MVI   ELCODE,FJNELQ                                                    
         BAS   RE,GETELIO                                                       
         GOTO1 REMELEM             DELETE JOB ELEMENTS                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING FJNELD,R6                                                        
         MVI   FJNEL,FJNELQ                                                     
         MVI   FJNLN,FJNLNQ        PREPARE TO ADD NEW ONES                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL GET THE CURRENT DATE AND TIME                     *         
***********************************************************************         
*                                                                               
GETDATE  ST    RE,SAVERE                                                        
         GOTO1 GETFACT,DMCB,(0,0)  GET CURRENT TIME                             
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         ZAP   DUB,FATIME          SAVE THE TIME                                
         AP    DUB,=P'60000'       ADJUST FOR DDS TIME                          
         CP    DUB,=P'240000'      MAKE SURE WE HAVEN'T GONE TOO FAR            
         BNH   *+10                                                             
         SP    DUB,=P'240000'                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL REWRITE THE FUND RECORD                           *         
***********************************************************************         
*                                                                               
REWRITE  NTR1                                                                   
         BAS   RE,WRITEIT                                                       
         MVI   INTMODE,DISLIST                                                  
         MVI   UPDATE,C'N'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL UPDATE PERSONAL ELEMENT AND WRITE THE RECORD.     *         
***********************************************************************         
*                                                                               
WRITEIT  ST    RE,SAVERE                                                        
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
*                                                                               
WRITX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PUT JOB TABLE FROM BUFF TO TWA 1                               
*                                                                               
PUTTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1            PAGE=TWA 1                                   
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,DMWRT,TEMPSTR,,AJOBTAB                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
* SUB-ROUTINE TO GET SAVED JOB TABLE FROM TWA 1                                 
*                                                                               
GETTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1                                                         
         MVC   DMCB+10(2),TERM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         LHI   RF,LENTWAS                                                       
         STH   RF,DMCB+22                                                       
         GOTO1 DATAMGR,DMCB,DMREAD,TEMPSTR,,BUFF                                
         LA    RE,BUFF                                                          
         ST    RE,AJOBTAB                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
NOCODE   MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
NOTLAST  MVI   ERROR,LASTSCR                                                    
*                                                                               
*        BAS   RE,PUTTAB                                                        
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
         USING JFNELD,R6                                                        
FUNDERR  MVC   MYMSGNO1,ERROR                                                   
         XC    WORK,WORK                                                        
         MVI   WORK,L'JFNNUM+1                                                  
         MVC   WORK+1(L'JFNNUM),JFNNUM                                          
         ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         BAS   RE,PUTTAB                                                        
         GOTO1 XTRAXIT                                                          
*                                                                               
         USING RSTELD,R6                                                        
BILLERR  MVI   MYMSGNO1,BILLER                                                  
         XC    WORK,WORK                                                        
         MVI   WORK,L'ACTKACT+1                                                 
         MVC   WORK+1(L'ACTKACT),KEY+3                                          
         ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         BAS   RE,PUTTAB                                                        
         GOTO1 XTRAXIT                                                          
*                                                                               
XIT      XIT1                                                                   
*                                                                               
DMWRT    DC    C'DMWRT '                                                        
DMREAD   DC    C'DMREAD'                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
*                                                                               
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NEWMSG   DC    CL50'FULL SCREEN AVAILABLE FOR ADDS'                             
ACCFIL   DC    CL8'ACCFIL  '                                                    
BLANKS   DC    CL255' '                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE ACJOBBERD                                                      
         EJECT                                                                  
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
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
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROB6D                                                       
         DS    0F                                                               
*                                                                               
CLEARNEW DS    0CL(CLEAREND)       CLEAR THESE FIELDS WHEN KEY CHANGES          
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
NEWLINES DS    X                   NEW LINES FOR ADDS                           
*                                                                               
LLASTSCH DS    XL(L'SELKEY)        KEY OF LAST LINE ON SCREEN                   
LSELTAB  DS    XL(MAXLINE*SELTABL)                                              
*                                                                               
FSTSEQ   DS    XL2                 DISPL OF 1ST CODE THIS SCREEN                
NXTSEQ   DS    XL2                 DISPL OF NEXT CODE THIS SCREEN               
LSTSEQ   DS    XL2                 DISPL OF LAST CODE IN TABLE                  
CLEAREND EQU   *-NUMLINES                                                       
*                                                                               
UPDATE   DS    CL1                 'Y' IF THERE ARE ADDS AND/OR DELETES         
*                                                                               
JOBCNT   DS    F                                                                
*                                                                               
SVFLDS   DS    0CL(SVFLDLQ)                                                     
SVOGR    DS    CL(L'FUNKOGR)                                                    
SVOFC    DS    CL(L'FUNKOFC)                                                    
SVCLI    DS    CL(L'FUNKCLI)                                                    
SVPRO    DS    CL(L'FUNKPRO)                                                    
SVMGR    DS    CL(L'FUNKMGR)                                                    
SVMED    DS    CL(L'FUNKMED)                                                    
SVNUM    DS    CL(L'FUNKNUM)                                                    
SVAMT    DS    CL(L'FUNAMT)                                                     
SVEST    DS    CL(L'EVEKTYP+L'EVEKVER)                                          
SVFLDLQ  EQU   *-SVOGR                                                          
*                                                                               
SVFUNKEY DS    CL(L'FUNKEY)                                                     
*                                                                               
TAUTH    DS    PL6                 TOTAL AUTHORIZED                             
TFUNDED  DS    PL6                 TOTAL FUNDED                                 
TAVAIL   DS    PL6                 TOTAL AVAILABLE                              
*                                                                               
QSORT    DS    V                                                                
*                                                                               
MAXLINE  EQU   10                  MAXIMUM N'LINES PER SCREEN                   
MAXFIELD EQU   5                   MAXIMUM N'FIELDS PER LINE                    
MAXELS   EQU   135                 MAXIMUM N'ELEMENTS ON A RECORD               
*                                                                               
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
NEWLIST  EQU   4                   FULL SCREEN ADD SWITCH                       
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
SCROLL   DS    X                   SCROLL AMOUNT                                
COLIST   DS    CL80                                                             
*                                                                               
ORIGFUND DS    C                   INDICATOR FOR ADDING ELEMENTS                
*                                                                               
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
SAVER6   DS    A                   SAVE R6 FOR AUTHORIZED AMOUNT                
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
AACCT    DS    A                   A(ACCOUNT FIELD)                             
ANAME    DS    A                   A(NAME FIELD)                                
AAMNT    DS    A                   A(AMOUNT FUND)                               
AEST#    DS    A                   A(EST#)                                      
*                                                                               
AJOB     DS    A                   A(JOB RECORD)                                
*                                                                               
AJOBTAB  DS    A                                                                
*                                                                               
ACOLTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LCOLTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
AOPVTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LOPVTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                                                                
SELKEY   DS    CL(ACTKEND)                                                      
SELTABL  EQU   *-SELTABD                                                        
*                                                                               
*                                                                               
* DSECT TO COVER JOB TABLE                                                      
*                                                                               
JOBTABD  DSECT                                                                  
JOBTJOB  DS    CL12                                                             
JOBTAMT  DS    PL6                                                              
JOBTEST  DS    CL2                                                              
JOBTABL  EQU   *-JOBTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACPRO66   07/23/13'                                      
         END                                                                    
