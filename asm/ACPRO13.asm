*          DATA SET ACPRO13    AT LEVEL 048 AS OF 04/10/15                      
*PHASE T60B13A                                                                  
         TITLE 'T60B13 - JOB LIST'                                              
T60B13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B13**,R7,R8,RR=R2                                           
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    JOB2                                                             
         CLI   MODE,VALREC                                                      
         BE    JOB4                                                             
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
JOB2     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED                                                        
*                                                                               
         MVI   CHASW,C'Y'                                                       
         CLI   AUTHOR,0                                                         
         BE    JOB3                                                             
         TM    AUTHOR,CAT1Q+CAT3Q+CAT5Q     TEST FOR CAT 1,3 OR 5               
         BNZ   *+8                                                              
         MVI   CHASW,C'N'                                                       
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    *+8                                                              
         MVI   CHASW,C'N'                                                       
*                                                                               
JOB3     MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST                                                  
         CLI   INTMODE,FSTLIST     TEST FIRST TIME LIST                         
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES-BUILD A SCREEN                           
         B     XIT                                                              
*                                                                               
* VALREC LOGIC-DISPLAY OR CHANGE                                                
*                                                                               
JOB4     BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    JOB6                                                             
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    JOB10               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
JOB6     GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LLASTJOB,LLASTJOB   CLEAR OUT LAST JOB LISTED                    
         CLI   SCRTYPE,C'O'        TEST OPTIONS TYPE DISPLAY                    
         BNE   *+8                                                              
         BAS   RE,INOPT            INITIALIZE FOR OPTIONS READ                  
*                                                                               
JOB7     BAS   RE,LIST                                                          
         L     R2,AFSTSEL                                                       
         CLC   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    JOB8                YES                                          
         XC    LLASTJOB,LLASTJOB   NO-MUST BE AT END-OF-LIST                    
         MVI   MYMSGNO1,IENDLIST                                                
         CLI   LNLISTS,0           TEST ANYTHING ON SCREEN                      
         BNE   JOB9                YES                                          
*                                                                               
         LA    R2,PROCLIH          POSITION CURSOR AT FIRST KEY FIELD           
         MVC   CONHEAD(L'LIST3MSG),LIST3MSG                                     
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BNE   JOB9                NO                                           
         MVI   MYMSGNO1,INOLIST                                                 
         B     JOB9                                                             
*                                                                               
JOB8     MVI   MYMSGNO1,ILISTDIS                                                
*                                                                               
JOB9     B     INFEXIT                                                          
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
JOB10    BAS   RE,EDT                                                           
         L     R2,AFSTSEL                                                       
         MVI   MYMSGNO1,ICHGCOM                                                 
         B     JOB9                                                             
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   SCRTYPE,C'R'        DEFAULT SCREEN TYPE=REGULAR                  
         MVI   KEYCHG,C'N'                                                      
         MVI   EXLEN,2                                                          
         LA    R2,PROCLIH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
*                                                                               
         MVI   OPTION,0            NO NAME FIELDS NEEDED                        
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         MVI   ERROR,NOTCLNOF                                                   
         CLI   PROOFFH+5,0         TEST IF OFFICE INPUT                         
         BNE   ERREND              YES-CANNOT HAVE BOTH ON SCREEN               
         ZIC   R1,LCLI                                                          
         LA    R1,2(R1)            SET KEY COMPARE LENGTH                       
         STC   R1,EXLEN                                                         
*                                                                               
VALHED2  LA    R2,PROPROH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
*                                                                               
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         ZIC   R1,LCLIPRO                                                       
         LA    R1,2(R1)                                                         
         STC   R1,EXLEN                                                         
         MVC   SAVEOFC,EFFOFFC     SAVE OFFICE FOR LABELS                       
*                                                                               
VALHED4  LA    R2,PROSTAH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         GOTO1 ANY                                                              
         MVC   QJOB,WORK                                                        
*                                                                               
VALHED6  LA    R2,PROMEDH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED7                                                          
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
*                                                                               
VALHED7  LA    R2,PROOFFH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR OFFICE INPUT                        
         BE    VALHED8             NO                                           
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
                                                                                
VALHED8  BAS   RE,VALFILS          VALIDATE A SERIES OF 5 FILTER FILTER         
         LA    R2,PROSTATH         STATUS FIELD                                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALSTA                                                        
*                                                                               
VALHED10 LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALOPT                                                        
*                                                                               
         LA    R2,PROUSERH                                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         MVI   ERROR,NOTUSOPT                                                   
         CLI   PROOPTH+5,0         TEST FOR OPTIONS INPUT                       
         BNE   ERREND                                                           
*                                                                               
         MVI   SCRTYPE,C'U'        NOTE SCREEN TYPE IS USER                     
         MVI   ERROR,INVALID                                                    
         GOTO1 ANY                                                              
         CLI   5(R2),2                                                          
         BL    ERREND                                                           
         BH    VALHED12            LENGTH IS 3                                  
*                                                                               
VALHED11 CLI   WORK+1,C'+'         TEST FOR USER FIELD FILTER                   
         BE    *+12                                                             
         CLI   WORK+1,C'-'                                                      
         BNE   VALHED14            NO                                           
         MVC   QUSERF,WORK+1                                                    
         MVI   WORK+1,C' '                                                      
         B     VALHED14                                                         
*                                                                               
VALHED12 CLI   WORK+2,C'+'         TEST FOR USER FIELD FILTER                   
         BE    *+12                                                             
         CLI   WORK+2,C'-'                                                      
         BNE   ERREND              MUST BE ONE PRESENT                          
         MVC   QUSERF,WORK+2                                                    
*                                                                               
VALHED14 MVC   QUSER,WORK                                                       
                                                                                
VALHED16 LA    R2,PRODTYPH         DATE TYPE                                    
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALDTY                                                        
                                                                                
         LA    R2,PRODATEH         DATE FIELD                                   
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALDTE                                                        
                                                                                
VALHED18 OI    PROCLIH+4,X'20'     TURN ON PREV VALID BITS                      
         OI    PROPROH+4,X'20'                                                  
         OI    PROSTAH+4,X'20'                                                  
         OI    PROMEDH+4,X'20'                                                  
         OI    PROOFFH+4,X'20'                                                  
         LA    R2,PROFILTH                                                      
         LA    R0,NFILTS                                                        
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,*-8                                                           
         OI    PROSTATH+4,X'20'                                                 
         OI    PROOPTH+4,X'20'                                                  
         OI    PROUSERH+4,X'20'                                                 
         OI    PRODTYPH+4,X'20'                                                 
         OI    PRODATEH+4,X'20'                                                 
*                                                                               
VALHED20 LA    R0,TYPES                                                         
         LA    RE,TYPETAB                                                       
         CLC   SCRTYPE,0(RE)                                                    
         BE    VALHED22                                                         
         LA    RE,L'TYPETAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                DID NOT RECOGNIZE SCREEN TYPE                
*                                                                               
VALHED22 MVC   SCRTYPE(L'TYPETAB),0(RE) EXTRACT ENTIRE ENTRY                    
         LA    R0,4                COUNTER                                      
         LA    RE,ADIS             RE=A(ADCONS)                                 
         L     R1,RELO                                                          
*                                                                               
VALHED25 L     RF,0(RE)                                                         
         LTR   RF,RF               TEST FOR ZERO ADCON                          
         BZ    *+6                                                              
         AR    RF,R1               RELOCATE                                     
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,VALHED25                                                      
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE FILTER FIELD                                      
* AT ENTRY, R2=A(FLDH), P1=A(OUTPUT), P2=N'OUTPUT POSITIONS                     
*                                                                               
VALFILS  NTR1  ,                                                                
         LA    R2,PROFILTH                                                      
         LA    R3,QFILTS                                                        
         LA    R5,NFILTS                                                        
*                                                                               
VALFIL1  BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALFIL6             NEXT FIELD                                   
         MVI   ERROR,INVALID                                                    
         MVI   0(R3),C' '          PRE-CLEAR OUTPUT                             
         LA    R4,8(R2)                                                         
         ZIC   R6,5(R2)                                                         
*                                                                               
VALFIL2  CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                MUST BE FILTER ITSELF                        
*                                                                               
VALFIL3  MVC   0(1,R3),0(R4)       SET VALUE                                    
         B     VALFIL6                                                          
*                                                                               
VALFIL4  LA    R4,1(R4)            NEXT INPUT CHARACTER                         
         SH    R6,=H'1'                                                         
         BZ    ERREND                                                           
         LA    R0,1                                                             
         MVC   0(1,R3),0(R4)                                                    
         NI    0(R3),X'FF'-X'40'   TURN OFF UPPER CASE BIT                      
*                                                                               
VALFIL6  LA    R3,1(R3)            NEXT OUTPUT FIELD                            
         BAS   RE,BUMPTOUN         NEXT SCREEN INPUT FIELD                      
         BCT   R5,VALFIL1                                                       
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE STATUS FIELD                                          
*                                                                               
VALSTA   NTR1  ,                                                                
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         CLI   4(R1),3             TEST MORE THAN 3 ENTRIES IN FIELD            
         BH    ERREND              YES-NO GO                                    
         ZIC   R6,4(R1)            R6=LOOP COUNTER                              
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
*                                                                               
VALSTA2  CLI   1(R4),0             TEST FOR DIVIDED FIELD                       
         BNE   ERREND              MUST BE A PROBLEM                            
         CLI   0(R4),0             TEST FOR COMMA ONLY                          
         BE    ERREND                                                           
         CLI   0(R4),10            TEST FOR BIG STRING                          
         BH    ERREND                                                           
         ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
VALSTA4  EX    R1,CLOSCOMP         TEST FOR CLOSED                              
         BE    *+12                                                             
         EX    R1,OPENCOMP         TEST FOR OPEN                                
         BNE   VALSTA6                                                          
         CLI   QCLOSED,0           TEST FOR PREVIOUS CLOSED ENTRY               
         BNE   ERREND              YES                                          
         MVC   QCLOSED,12(R4)      FIRST LETTER IS ENOUGH                       
         B     VALSTA10                                                         
*                                                                               
VALSTA6  EX    R1,LOCKCOMP         TEST FOR LOCKED                              
         BE    *+12                                                             
         EX    R1,UNLCOMP          TEST FOR UNLOCKED                            
         BNE   VALSTA8                                                          
         CLI   QLOCKED,0                                                        
         BNE   ERREND                                                           
         MVC   QLOCKED,12(R4)                                                   
         B     VALSTA10                                                         
*                                                                               
VALSTA8  EX    R1,XJOBCOMP         TEST FOR XJOBS                               
         BE    *+12                                                             
         EX    R1,NOXJCOMP         TEST FOR NO X-JOBS                           
         BNE   ERREND              MUST BE A BAD KEYWORD                        
         CLI   QXJOB,0                                                          
         BNE   ERREND                                                           
         MVC   QXJOB,12(R4)                                                     
*                                                                               
VALSTA10 LA    R4,32(R4)                                                        
         BCT   R6,VALSTA2                                                       
*                                                                               
VALSTAX  B     XIT                                                              
         SPACE 2                                                                
CLOSCOMP CLC   12(0,R4),=C'CLOSED'                                              
OPENCOMP CLC   12(0,R4),=C'OPEN'                                                
LOCKCOMP CLC   12(0,R4),=C'LOCKED'                                              
UNLCOMP  CLC   12(0,R4),=C'UNLOCKED'                                            
XJOBCOMP CLC   12(0,R4),=C'XJOBS ONLY'                                          
NOXJCOMP CLC   12(0,R4),=C'NO XJOBS'                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE OPTIONS FIELD                                     
*                                                                               
VALOPT   NTR1  ,                                                                
*                                                                               
         MVI   SCRTYPE,C'O'        NOTE OPTIONS SCREEN                          
         MVI   FLAG,0                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         CLI   4(R1),2             TEST MORE THAN 2 ENTRIES                     
         BH    ERREND              YES                                          
         ZIC   R6,4(R1)                                                         
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
*                                                                               
VALOPT02 CLI   0(R4),0             TEST FOR COMMA ONLY                          
         BE    ERREND                                                           
         CLI   1(R4),0             TEST FOR KEYWORD/PARM                        
         BNE   ERREND              YES                                          
         CLI   0(R4),L'OPTBKEY     CANNOT BE LONGER THAN KEY LENGTH             
         BH    ERREND                                                           
         CLI   12(R4),C'$'         LOOKING FOR BALANCE?                         
         BE    VALOPT14            YES                                          
         CLC   =C'PR',12(R4)                                                    
         BE    VALOPT20            YES                                          
         CLC   =C'DATE',12(R4)                                                  
         BE    VALOPT22            YES                                          
         L     R5,AOPTTAB                                                       
         USING OPTBD,R5                                                         
*                                                                               
VALOPT04 CLI   OPTBOPN,0           TEST FOR EOT                                 
         BE    ERREND                                                           
         CLI   0(R4),L'OPTBSHRT    TEST FOR SHORT KEY                           
         BH    VALOPT06            NO                                           
         CLC   OPTBSHRT,12(R4)     MATCH ON SHORT KEY                           
         BE    VALOPT10                                                         
         B     VALOPT08                                                         
*                                                                               
VALOPT06 CLC   0(1,R4),OPTBMINL    TEST MINIMUM LENGTH FOR KEY                  
         BL    VALOPT08            NO                                           
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),OPTBKEY                                                 
         BE    VALOPT10                                                         
*                                                                               
VALOPT08 LA    R5,OPTBL(R5)                                                     
         B     VALOPT04                                                         
*                                                                               
VALOPT10 TM    OPTBSHOW,OPTBJOB    TEST VALID AT JOB LEVEL                      
         BZ    ERREND              NO                                           
*                                                                               
         LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         CLI   QOPT1,0             TEST IF FIRST OPTION                         
         BE    *+12                YES                                          
         LA    RE,QOPT2                                                         
         LA    R3,OPTST2                                                        
         CLI   FLAG,1              PR OR DATE OPTION USED?                      
         BE    ERREND              YES, CAN'T USE THEM TOGETHER                 
         MVC   0(1,RE),OPTBOPN     SET OPTION NUMBER                            
         MVI   0(R3),C'P'          SET FOR PROTECTED                            
         TM    OPTBPROT,OPTBJOB    TEST IF JOB LEVEL PROTECTED                  
         BO    *+8                 YES                                          
         MVI   0(R3),C'U'          NO-ITS AN UNPROTECTED FIELD                  
         MVC   1(L'OPTBDESC,R3),OPTBDESC                                        
*                                                                               
VALOPT12 LA    R4,32(R4)           NEXT SCANNER BLOCK                           
         BCT   R6,VALOPT02                                                      
         B     VALOPTX                                                          
*                                                                               
VALOPT14 LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         CLI   QOPT1,0             TEST IF FIRST OPTION                         
         BE    *+12                                                             
         LA    RE,QOPT2                                                         
         LA    R3,OPTST2                                                        
         CLI   FLAG,1              PR OPTION?                                   
         BE    ERREND              YES, ERROR                                   
         MVI   0(RE),C'$'          SET OPTION                                   
         MVI   0(R3),C'P'          SET FOR PROTECTED                            
         MVC   1(11,R3),=C'JOB BALANCE'                                         
         B     VALOPT12                                                         
*                                                                               
VALOPT20 LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         CLI   QOPT1,0             TEST IF FIRST OPTION                         
         BNE   ERREND                                                           
         MVI   0(RE),C'P'          SET OPTION                                   
         MVI   0(R3),C'P'          SET FOR PROTECTED                            
         MVI   SCRTYPE,C'P'        NOTE OPTIONS SCREEN                          
         MVI   FLAG,1              PR USED                                      
         B     VALOPT12                                                         
*                                                                               
VALOPT22 LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         CLI   QOPT1,0             TEST IF FIRST OPTION                         
         BNE   ERREND                                                           
         MVI   0(RE),C'D'          SET OPTION                                   
         MVI   0(R3),C'D'          SET FOR PROTECTED                            
         MVI   SCRTYPE,C'D'        NOTE DATE SCREEN                             
         MVI   FLAG,1              DATE USED                                    
         B     VALOPT12                                                         
*                                                                               
VALOPTX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE DATE TYPE FIELD                                       
*                                                                               
VALDTY   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         MVC   QDTYPE,8(R2)                                                     
         CLI   QDTYPE,C'A'                                                      
         BE    VALDTYX                                                          
         CLI   QDTYPE,C'B'                                                      
         BE    VALDTYX                                                          
         CLI   QDTYPE,C'C'                                                      
         BNE   ERREND                                                           
*                                                                               
VALDTYX  B     XIT                                                              
         EJECT                                                                  
                                                                                
* SUB-ROUTINE TO EDIT THE DATE FIELD                                            
*                                                                               
         USING PERVALD,R4                                                       
VALDTE   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    SDATE,SDATE                                                      
         MVC   EDATE,=XL3'FFFFFF'                                               
                                                                                
         GOTO1 PERVAL,DMCB,(PRODATEH+5,PRODATE),(X'20',WORK)                    
         CLI   4(R1),PVRCOK        DATE RANGE FOUND?                            
         BE    *+12                                                             
         CLI   4(R1),PVRCONE       SINGLE DATE FOUND?                           
         BNE   ERREND                                                           
         LA    R4,WORK                                                          
                                                                                
*                                        CHECK IF START DATE ASSUMED            
         MVC   BYTE,PVALASSM                                                    
         NI    BYTE,PVALASD+PVALASM+PVALASY                                     
         CLI   BYTE,PVALASD+PVALASM+PVALASY                                     
         BE    *+10                                                             
         MVC   SDATE,PVALPSTA      NO, GET THE START                            
                                                                                
*                                        CHECK IF END DATE ASSUMED              
         MVC   BYTE,PVALASSM                                                    
         NI    BYTE,PVALAED+PVALAEM+PVALAEY                                     
         CLI   BYTE,PVALAED+PVALAEM+PVALAEY                                     
         BNE   VALD30                                                           
                                                                                
         SR    R0,R0                                                            
         IC    R0,PRODATEH+5                                                    
         LA    RE,PRODATE                                                       
         CLI   0(RE),C'-'                                                       
         BE    VALDTEX                                                          
         AHI   RE,1                                                             
         BCT   R0,*-12                                                          
                                                                                
         MVC   BYTE,PVALASSM                                                    
         NI    BYTE,PVALASD+PVALASM+PVALASY                                     
         CLM   R4,8,=X'4'         SINGLE DATE ENTERED?                          
         BE    VALD29                                                           
         CLI   BYTE,0             START DATE ASSUMED?                           
         BE    *+14                                                             
         MVC   EDATE,PVALPEND      SET END DATE                                 
         B     *+10                                                             
VALD29   MVC   EDATE,PVALPSTA      SET END DATE                                 
         B     VALDTEX                                                          
                                                                                
VALD30   CLI   4(R1),PVRCONE       SINGLE DATE ENTERED?                         
         BE    *+14                                                             
         MVC   EDATE,PVALPEND      SET END DATE                                 
         B     *+10                                                             
         MVC   EDATE,PVALPSTA                                                   
                                                                                
VALDTEX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE SCREEN DYNAMICALLY                                   
*                                                                               
BLDTWA   NTR1  ,                                                                
         LA    R2,PROTAGH          START SCREEN BUILDING AT SAME POINT          
         CLI   SCRTYPE,C'O'        TEST FOR OPTIONS                             
         BNE   BLDTWA2                                                          
*                                                                               
BLDTWA1  L     RE,ALISTELS         FUDGE THE ELEMENTS AS NEEDED                 
         MVC   BLOCK(OPTELSLN),0(RE)                                            
         LA    R3,BLOCK                                                         
         ST    R3,ALISTELS         CHANGE LIST ELEMENT ADDRESS                  
         USING TWAELEMD,R3                                                      
         LA    R3,TWAELLNQ*2(R3)   POINT TO THIRD ELEMENT                       
         MVC   TWAEFLD,QOPT1       FIELD NUMBER=OPTION NUMBER                   
         CLI   OPTST1,C'U'                                                      
         BNE   *+8                                                              
         MVI   TWAEATB,X'08'       UNPROTECTED/HIGH INTENSITY                   
         CLI   QOPT2,0             TEST FOR SECOND OPTION                       
         BE    BLDTWA2             NO                                           
*                                                                               
         LA    R3,TWAELLNQ*2(R3)   POINT TO FIFTH ELEMENT                       
         MVC   TWAEFLD,QOPT2                                                    
         CLI   OPTST2,C'U'                                                      
         BNE   *+8                                                              
         MVI   TWAEATB,X'08'       SET TO UNP/HI INTENSITY                      
*                                                                               
BLDTWA2  CLI   CHASW,C'Y'          TEST CHANGES ALLOWED                         
         BE    BLDTWA4             YES                                          
         L     R3,ALISTELS                                                      
         SR    R0,R0                                                            
*                                                                               
BLDTWA3  CLI   TWAELCD,0           TEST FOR EOT                                 
         BE    BLDTWA4                                                          
         CLI   TWAELCD,X'03'       TEST FOR POTENTIAL UNP FIELD                 
         BNE   *+8                                                              
         MVI   TWAEATB,X'20'       MAKE IT PROTECTED                            
         IC    R0,TWAELLN                                                       
         AR    R3,R0                                                            
         B     BLDTWA3                                                          
*                                                                               
BLDTWA4  LA    R1,WORK                                                          
         USING TWAPARMD,R1                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,ATWA                                                    
         MVC   TWAPAFST,ALISTELS                                                
         MVC   TWAPAMAX,=AL4(LSAVES-T60BFFD)                                    
         ST    R2,TWAPAOUT                                                      
         ZIC   R0,NLINES                                                        
*                                                                               
BLDTWA6  GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,TWAPANXT                                                      
         ST    R2,TWAPAOUT         SET A(OUTPUT)=A(NEXT FIELD)                  
         BCT   R0,BLDTWA6                                                       
*                                                                               
BLDTWA8  MVC   TWAPAFST,APFELS     NOW BUILD PF KEY LINE                        
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTWAX  B     XIT                                                              
         DROP  R1,R3                                                            
         EJECT                                                                  
* SUB-ROUTINES TO SET SCREEN ADDRESSES                                          
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PROTAGH                                                       
         ST    R2,AFSTSEL                                                       
         ZIC   R0,NLINES                                                        
         ZIC   R1,NFIELDS                                                       
         MR    R0,R0               COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         ZIC   R0,NFIELDS                                                       
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS PF KEYS - ROUTINE HANDLES GLOBAL BILLING               
* SELECT INPUT                                                                  
*                                                                               
PROCPF   NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         LA    R5,LSELTAB          R5=A(SELECT TABLE ENTRY)                     
         USING SELTABD,R5                                                       
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PROCPF10            NOTHING ON SCREEN                            
         CLI   PFKEY,PF1           TEST PF1=PRODUCT LIST                        
         BE    PROCPF1             YES                                          
         CLI   SCRTYPE,C'R'        TEST FOR REGULAR LIST                        
         BNE   PROCPFX             NO-DO NOT HONOR PF KEYS                      
         CLI   PFKEY,PF9           TEST FOR PF9-PF11                            
         BL    PROCPFX                                                          
         CLI   PFKEY,PF11                                                       
         BH    PROCPFX                                                          
         CLI   CHASW,C'Y'          TEST CHANGES ALLOWED                         
         BE    PROCPF2                                                          
         B     PROCPFX             NO                                           
*                                                                               
PROCPF1  L     RE,ATWA                                                          
         AH    RE,MODLAST          A(LAST MODIFIED FIELD)                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST IF ANYTHING CHANGED AFTER ACTN          
         BH    PROCPFX             YES-IGNORE PF KEY                            
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,CURDISP                                                       
         ST    RE,ACURSOR          SAVE A(CURSOR)                               
*                                                                               
PROCPF2  BAS   RE,SETLIN                                                        
         CLI   PFKEY,PF1           TEST PF1=PRODUCT LIST                        
         BNE   PROCPF3                                                          
         CLC   ASEL,ACURSOR        TEST IF CURSOR IS IN SELECT FIELD            
         BNE   PROCPF8             NO                                           
*                                                                               
         MVC   CLICODE,BLANKS                                                   
         MVC   PRODCODE,BLANKS                                                  
*                                                                               
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLICODE(0),SELKEY+3 EXTRACT ITS CLIENT AND PRODUCT               
*                                                                               
         LA    RE,SELKEY+4(R1)                                                  
         ZIC   R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRODCODE(0),0(RE)                                                
*                                                                               
         MVI   DUB,RECNPRO                                                      
         MVI   DUB+1,ACTNLIST                                                   
         GOTO1 VCHKACC            NO-OPED SECURITY CODE                         
         BE    *+12                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0            CLEAR SAVED STACK                            
         GOTO1 VTRANSF,DMCB,=C'PRODUCT',=C'LIST',(6,CLICODE),          X        
               (6,PRODCODE),0                                                   
*                                                                               
PROCPF3  L     R2,ABILLS           R2=A(BILLING SELECT FIELD)                   
         CLI   8(R2),XJOB          X-JOB DISPLAY                                
         BE    PROCPF8             YES, IGNORE FOR GLOBAL BILLING SEL           
*                                                                               
         CLI   CHASW,C'Y'          TEST CHANGES ALLOWED                         
         BNE   PROCPFX                                                          
*                                                                               
         CLI   PFKEY,PF10          TEST FOR PF10                                
         BE    PROCPF4             YES                                          
         BH    PROCPF6             ITS PF11                                     
*                                                                               
         CLI   8(R2),C'Y'          TEST FIELD IS ALREADY 'YES'                  
         BE    PROCPF8             YES-LEAVE IT ALONE                           
         MVI   8(R2),C'Y'          FORCE FIELD TO C'Y'                          
         MVI   5(R2),1             FUDGE INPUT LENGTH                           
         OI    4(R2),X'80'         MAKE IT INPUT THIS TIME                      
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID BIT                
         B     PROCPF8                                                          
*                                                                               
PROCPF4  CLI   8(R2),C'N'          TEST ITS ALREADY 'NO'                        
         BE    PROCPF8             YES                                          
         MVI   8(R2),C'N'                                                       
         MVI   5(R2),1                                                          
         OI    4(R2),X'80'         MAKE IT INPUT THIS TIME                      
         NI    4(R2),X'FF'-X'20'                                                
         B     PROCPF8                                                          
*                                                                               
PROCPF6  CLI   8(R2),C' '                                                       
         BNH   PROCPF8             FIELD HAS NO DATA                            
         MVI   8(R2),C' '                                                       
         MVI   5(R2),0                                                          
         NI    4(R2),X'FF'-X'20'                                                
         OI    4(R2),X'80'                                                      
*                                                                               
PROCPF8  L     R2,ANEXTSEL         POINT TO NEXT SELECT FIELD                   
         LA    R5,SELTABL(R5)      NEXT SELECT TABLE ENTRY                      
         BCT   R3,PROCPF2                                                       
*                                                                               
PROCPF10 CLI   PFKEY,PF1           TEST PF1=PRODUCT LIST                        
         BNE   PROCPFX                                                          
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R1,CONACTH          TEST SOMETHING MODIFIED AFTER ACTION         
         CR    RE,R1                                                            
         BH    PROCPFX             YES                                          
*                                                                               
         L     R2,AFSTSEL                                                       
         MVI   DUB,RECNPRO                                                      
         MVI   DUB+1,ACTNLIST                                                   
         GOTO1 VCHKACC            NO-OPED SECURITY CODE                         
         BE    *+12                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         GOTO1 VTRANSF,WORK,=C'PRODUCT',=C'LIST',(L'QCLI,QCLI),        X        
               (L'QPROD,QPROD),0                                                
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    TSTEDTN             NOTHING ON SCREEN                            
*                                                                               
TSTEDT2  CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    TSTEDT4             NO                                           
         CLI   8(R2),C'*'          TEST ALREADY PROCESSED                       
         BE    TSTEDT4             YES                                          
         B     TSTEDTY             INPUT IN A SELECT FIELD                      
*                                                                               
TSTEDT4  ZIC   R4,NFIELDS          R4=FIELD COUNTER                             
         BCTR  R4,0                LESS ONE FOR SELECT                          
*                                                                               
TSTEDT5  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
*                                                                               
         BCT   R4,TSTEDT5                                                       
*                                                                               
         BAS   RE,BUMP             ADVANCE TO SELECT                            
         BCT   R3,TSTEDT2          ANOTHER LINE                                 
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE FOR AN OPTION TYPE DISPLAY                          
* CALLED FROM MAIN LINE LOGIC                                                   
*                                                                               
INOPT    ST    RE,SAVERE                                                        
         MVC   KEY,BLANKS                                                       
         MVC   KEY(1),CUL          READ COMPANY RECORD                          
         GOTO1 READ                                                             
         LA    R0,BUFF             SAVE IT AT BUFF                              
         ST    R0,ACOMP                                                         
         LA    R1,1000                                                          
         L     RE,AIO                                                           
         LH    RF,ACLENGTH-ACKEYD(RE) GET RECORD LENGTH                         
         MVCL  R0,RE                                                            
*                                                                               
         LA    RE,BUFF                                                          
         LA    RE,1000(RE)         CLEAR OPTIMIZATION                           
         L     RF,OPTBUFFL         LENGTH OF OPTIONS BUFFER                     
         LA    RF,GOFROML(RF)      ADD ROOM FOR FROM BUFFER                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO LIST JOB RECORDS                                               
*                                                                               
LIST     NTR1  ,                                                                
         L     R2,AFSTSEL                                                       
         ST    R2,ATHISLIN         SET POINTER TO LIST LINE                     
         L     RE,AIO3             USE IO3 FOR CLIENT AND PRODUCT               
         ST    RE,ACLI                                                          
         LA    RE,1000(RE)                                                      
         ST    RE,APRO                                                          
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         OC    LLASTJOB,LLASTJOB   TEST IF CONTINUING LIST                      
         BNZ   LIST04                                                           
*                                                                               
* FIRST TIME LOGIC-BUILD FIRST KEY                                              
*                                                                               
         MVC   ACKEYD(ACLENGTH-ACKEYD),BLANKS                                   
         MVC   ACKEYACC(3),CUL                                                  
         LA    RE,QCLI                                                          
         OC    QCLI,QCLI           TEST FOR CLIENT INPUT                        
         BNZ   *+8                 YES                                          
         LA    RE,=CL6'A'                                                       
         MVC   ACKEYACC+3(6),0(RE)                                              
         OC    QOFF,QOFF           TEST FOR OFFICE READ                         
         BNZ   LIST06              YES-GO RIGHT INTO READ SEQUENCE              
         GOTO1 HIGH                                                             
         CLC   ACKEYACC(3),CUL     DO WE HAVE THE RIGHT CUL?                    
         BNE   LISTX               NO                                           
         GOTO1 SETCLI              YES, SAVE THE CLIENT                         
         BAS   RE,SAVECLI                                                       
                                                                                
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         LA    RE,QPROD                                                         
         OC    QPROD,QPROD                                                      
         BNZ   *+8                                                              
         LA    RE,=CL6'A'                                                       
         MVC   0(6,R1),0(RE)                                                    
         GOTO1 HIGH                                                             
         CLC   ACKEYACC(3),CUL     DO WE HAVE THE RIGHT CUL?                    
         BNE   LISTX               NO                                           
                                                                                
         OC    QCLI,QCLI           TEST FOR CLIENT INPUT                        
         BZ    LIST02              NO                                           
         ZIC   R1,LCLI                                                          
         AHI   R1,-1                                                            
         BNP   LIST02                                                           
         CLC   ACKEYACC+3(0),QCLI  IF CLI FILT MAKE SURE SAME CLI               
         EX    R1,*-6                                                           
         BNE   LISTX               NO,EXIT                                      
                                                                                
LIST02   GOTO1 SETPROD                                                          
         BAS   RE,SAVEPRO                                                       
                                                                                
         ZIC   R1,LCLIPRO                                                       
         LA    R1,ACKEYACC+3(R1)                                                
         LA    RE,QJOB                                                          
         OC    QJOB,QJOB           TEST FOR START JOB                           
         BNZ   *+8                                                              
         LA    RE,=CL6'A'                                                       
         MVC   0(6,R1),0(RE)                                                    
         B     LIST06              ALL SET TO READ HIGH                         
*                                                                               
* CONTINUING LIST LOGIC                                                         
                                                                                
LIST04   MVC   ACKEYD(ACLENGTH-ACKEYD),BLANKS                                   
         ZIC   R1,LCLI             NO                                           
         LA    R1,2(R1)            RE-READ LAST JOB'S CLIENT                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC(0),LLASTJOB                                             
         GOTO1 READ                                                             
         GOTO1 SETCLI                                                           
         BAS   RE,SAVECLI                                                       
                                                                                
         ZIC   R1,LCLIPRO          NO                                           
         LA    R1,2(R1)            NOW RE-READ ITS PRODUCT                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC(0),LLASTJOB                                             
         GOTO1 READ                                                             
         GOTO1 SETPROD                                                          
         BAS   RE,SAVEPRO                                                       
                                                                                
         MVC   ACKEYACC,LLASTJOB                                                
         GOTO1 READ                                                             
         MVI   ACKEYWRK,X'FF'      BUMP TO NEXT ACCOUNT                         
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST06   OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
         B     LIST10                                                           
                                                                                
LIST08   OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
         GOTO1 CATCHIOS                                                         
                                                                                
LIST10   TM    ACSTATUS,X'80'      TEST FOR DELETED RECORD                      
         BO    LIST90              YES-FORCE NEXT ACCOUNT                       
         ZIC   R1,EXLEN                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACKEYD(0),KEYSAVE                                                
         BNE   LISTX               ALL DONE                                     
                                                                                
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         CLI   0(R1),C' '          TEST FOR CLIENT RECORD                       
         BH    LIST12              NO                                           
         GOTO1 SETCLI                                                           
         BAS   RE,SAVECLI                                                       
         B     LIST08                                                           
                                                                                
LIST12   ZIC   R5,LCLIPRO                                                       
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '          TEST FOR PRODUCT RECORD                      
         BH    LIST16              NO                                           
         GOTO1 SETPROD                                                          
         OC    QOFF,QOFF           TEST FOR OFFICE FILTER                       
         BZ    LIST14              NO                                           
         CLC   QOFF,EFFOFFC        MATCH ON OFFICE                              
         BE    LIST14                                                           
         MVI   0(R5),X'FF'         NO-FORCE NEXT PRODUCT                        
         B     LIST06                                                           
                                                                                
LIST14   BAS   RE,SAVEPRO                                                       
         B     LIST08                                                           
                                                                                
LIST16   CLI   QMED,0              TEST FOR MEDIA FILTER                        
         BE    LIST18              NO                                           
         CLC   0(1,R5),QMED        TEST FOR MATCH ON MEDIA                      
         BE    LIST18              YES                                          
         BL    *+12                ITS BELOW TARGET MEDIA                       
         MVI   0(R5),X'FF'         FORCE NEXT PRODUCT                           
         B     LIST06                                                           
                                                                                
         MVC   0(1,R5),QMED        FORCE FIRST JOB FOR MEDIA                    
         ZIC   R1,LJOB                                                          
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),BLANKS                                                   
         B     LIST06                                                           
*                                                                               
* APPLY FILTERS TO JOB                                                          
*                                                                               
LIST18   GOTO1 SETJOB                                                           
         OC    QFILTS,QFILTS       TEST FOR FILTERS                             
         BZ    LIST20              NONE                                         
                                                                                
         MVC   BYTE,QFILT1                                                      
         GOTO1 APPFIL,EFF1                                                      
         BNE   LIST90              REJECTED-SKIP RECORD                         
                                                                                
         MVC   BYTE,QFILT2                                                      
         GOTO1 APPFIL,EFF2                                                      
         BNE   LIST90              REJECTED-SKIP RECORD                         
                                                                                
         MVC   BYTE,QFILT3                                                      
         GOTO1 APPFIL,EFF3                                                      
         BNE   LIST90              REJECTED-SKIP RECORD                         
                                                                                
         MVC   BYTE,QFILT4                                                      
         GOTO1 APPFIL,EFF4                                                      
         BNE   LIST90              REJECTED-SKIP RECORD                         
                                                                                
         MVC   BYTE,QFILT5                                                      
         GOTO1 APPFIL,EFF5                                                      
         BNE   LIST90              REJECTED-SKIP RECORD                         
                                                                                
         USING RSTELD,R6                                                        
LIST20   XC    ADATE,ADATE         CLEAR ACTIVITY DATE                          
         XC    BDATE,BDATE         AND BAL FORW'D DATE                          
         MVI   ELCODE,RSTELQ       GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ADATE,RSTTDATE      SAVE ACTIVITY DATE AND                       
         MVC   BDATE,RSTBDATE      BALANCE FORW'D DATE IN CASE NEEDED           
                                                                                
         CLI   QCLOSED,0           TEST FOR CLOSED FILTERS                      
         BE    LIST24              NO                                           
         TM    RSTSTAT,RSTSACIC    TEST FOR CLOSED JOB                          
         BO    LIST22              YES                                          
         CLI   QCLOSED,C'C'        TEST FOR CLOSED JOBS ONLY                    
         BE    LIST90              YES-SKIP THIS JOB                            
         B     LIST24                                                           
                                                                                
LIST22   CLI   QCLOSED,C'O'        TEST FOR OPEN JOBS ONLY                      
         BE    LIST90              YES-SKIP A CLOSED JOB                        
                                                                                
LIST24   CLI   QLOCKED,0           TEST FOR LOCKED FILTERS                      
         BE    LIST28              NO                                           
         TM    RSTSTAT,RSTSACIL    TEST FOR LOCKED JOB                          
         BO    LIST26              YES                                          
         CLI   QLOCKED,C'L'        TEST FOR LOCKED JOBS ONLY                    
         BE    LIST90              YES-SKIP IT                                  
         B     LIST28              NO-TAKE IT                                   
                                                                                
LIST26   CLI   QLOCKED,C'U'        TEST FOR UNLOCKED ONLY                       
         BE    LIST90                                                           
                                                                                
LIST28   CLI   QXJOB,0             FILTER XJOBS                                 
         BE    LIST32                                                           
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACJOBD,R6                                                        
         TM    ACJBSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BZ    LIST30              NO                                           
         CLI   QXJOB,C'N'          EXCLUDE XJOBS                                
         BE    LIST90              YES, REJECT THIS X JOB                       
         B     LIST32                                                           
                                                                                
LIST30   CLI   QXJOB,C'X'          X JOBS ONLY                                  
         BE    LIST90              YES, REJECT THIS NON-XJOB                    
                                                                                
         USING PACELD,R6                                                        
LIST32   XC    CDATE,CDATE         CLEAR CHANGE DATE                            
         MVI   ELCODE,PACELQ       GET LAST ACTIVITY DATE                       
         BAS   RE,GETELIO                                                       
         BNE   LIST34                                                           
         MVC   CDATE,PACDATE       SAVE LATEST DATE                             
         GOTO1 DATCON,DMCB,(1,PACDATE),(2,WORK)                                 
         CLI   PACLN,PACLNQ2                                                    
         BL    LIST34                                                           
         GOTO1 DATCON,DMCB,(1,PACDATE2),(2,WORK+2)                              
         CLC   WORK(2),WORK+2                                                   
         BH    LIST34                                                           
         MVC   CDATE,PACDATE2                                                   
                                                                                
LIST34   OC    SDATE,SDATE         ANY DATES REQUESTED?                         
         BNZ   *+10                                                             
         OC    EDATE,EDATE                                                      
         BZ    LIST80              NO, PROCESS EVERYTHING                       
                                                                                
         CLI   QDTYPE,C'C'         LOOKING FOR CHANGED DATE ONLY?               
         BE    LIST38              YES                                          
         CLI   QDTYPE,C'B'         LOOKING FOR BALANCE FORW'D ONLY?             
         BE    LIST42              YES                                          
         CLC   ADATE,SDATE         SEE IF RECORD IN RANGE                       
         BL    LIST36              NO                                           
         CLC   ADATE,EDATE                                                      
         BH    LIST36                                                           
         B     LIST80              TAKE IT IF ACTIVITY OR ALL                   
                                                                                
LIST36   CLI   QDTYPE,C'A'         LOOKING FOR ACTIVITY ONLY?                   
         BE    LIST90              YES, THEN SKIP THESE                         
                                                                                
LIST38   OC    CDATE,CDATE                                                      
         BZ    LIST40                                                           
         CLC   CDATE,SDATE                                                      
         BL    LIST40                                                           
         CLC   CDATE,EDATE                                                      
         BH    LIST40                                                           
         B     LIST80                                                           
                                                                                
LIST40   CLI   QDTYPE,C'C'         LOOKING FOR CHANGED ONLY?                    
         BE    LIST90              YES, THEN SKIP THESE                         
                                                                                
LIST42   CLC   BDATE,SDATE         MUST BE OPEN/BAL FORW'D OR ALL               
         BL    LIST90              NO                                           
         CLC   BDATE,EDATE                                                      
         BH    LIST90                                                           
*                                                                               
LIST80   TM    COMPSTA9,CPYLACCP   VERIFY LIMITED ACCESS?                       
         BNO   LIST85              NO                                           
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,EFFOFFC                                                 
         MVC   OFFAOPOS,LEDGTOFF   LEDGER OFFICE POSITION                       
         MVC   OFFAREC,AIO                                                      
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BNE   LIST90              SKIP THIS JOB                                
*                                                                               
* PROCESS THE JOB                                                               
*                                                                               
LIST85   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN           SET LIST LINE ADDRESSES                      
         L     RF,ADIS             RF=A(DISPLAY ROUTINE)                        
         BASR  RE,RF                                                            
         BNE   LIST90              JOB IS REJECTED                              
*                                                                               
         MVC   LLASTJOB,ACKEYACC   SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST RECORD COUNT                  
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)                                                   
         USING SELTABD,R1                                                       
         MVC   SELKEY,ACKEYACC     SAVE JOB KEY                                 
         MVC   SELACT,BLANKS                                                    
*                                                                               
         CLC   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
* SKIP TO NEXT ACCOUNT                                                          
*                                                                               
LIST90   MVI   ACKEYWRK,X'FF'      NEXT ACCOUNT                                 
         B     LIST06                                                           
*                                                                               
LISTX    B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE CLIENT RECORD                                             
*                                                                               
SAVECLI  ST    RE,SAVERE                                                        
         L     R0,ACLI                                                          
         LA    R1,1000                                                          
         L     RE,AIO                                                           
         LH    RF,ACLENGTH-ACKEYD(RE)                                           
         MVCL  R0,RE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE PRODUCT RECORD                                            
*                                                                               
SAVEPRO  ST    RE,SAVERE                                                        
         L     R0,APRO                                                          
         LA    R1,1000                                                          
         L     RE,AIO                                                           
         LH    RF,ACLENGTH-ACKEYD(RE)                                           
         MVCL  R0,RE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY A FILTER TO JOB'S FILTER                                 
* AT ENTRY, R1=A(JOB FILTER VALUE) BYTE=INPUT FILTER VALUE                      
* ON EXIT, CC=EQ IF OK, CC=NEQ TO REJECT JOB                                    
*                                                                               
APPFIL   CLI   BYTE,C' '           TEST FOR BLANK=TAKE EVERYTHING               
         BE    APPFILY                                                          
         CLI   BYTE,C'.'           TEST FOR NO FILTER ON JOB                    
         BNE   APPFIL2                                                          
         CLI   0(R1),C' '          FILTER POSITION CANNOT BE A VALUE            
         BH    APPFILN                                                          
         B     APPFILY                                                          
*                                                                               
APPFIL2  TM    BYTE,X'40'          TEST FOR POSITIVE FILTER                     
         BZ    APPFIL4                                                          
         CLC   BYTE,0(R1)          TEST FOR MATCH ON FILTER                     
         BE    APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFIL4  OI    BYTE,X'40'          RESTORE UPPER CASE BIT                       
         CLC   BYTE,0(R1)          TEST FOR DIFFERENCE                          
         BNE   APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFILY  CR    RB,RB                                                            
         B     APPFILX                                                          
*                                                                               
APPFILN  LTR   RB,RB                                                            
*                                                                               
APPFILX  BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE REGULAR LIST LINE                                  
*                                                                               
DISREG   NTR1  ,                                                                
         MVC   PROTIT(L'REGTIT),REGTIT                                          
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SET ON PREV VALIDATED BIT                    
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVC   LISTAR,BLANKS                                                    
         LA    R3,LISTAR                                                        
         USING REGPROTD,R3                                                      
         BAS   RE,ACCN                                                          
         MVC   REGACCN,WORK                                                     
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   REGNAME,WORK                                                     
         MVI   ELCODE,X'32'        GET BALANCE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACBALD,R6                                                        
         ZAP   BALANCE,ACBLFRWD                                                 
         AP    BALANCE,ACBLDR                                                   
         SP    BALANCE,ACBLCR                                                   
         EDIT  (P8,BALANCE),(12,REGBAL),2,MINUS=YES                             
         OC    JOBCLOSE,JOBCLOSE                                                
         BZ    DISREG2                                                          
         GOTO1 DATCON,DMCB,(1,JOBCLOSE),(8,REGCLOSE)                            
*                                                                               
DISREG2  L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     DISREG4                                                          
         MVC   8(0,R2),LISTAR      MOVE OUT PROTECTED FIELD DATA                
*                                                                               
DISREG4  L     R2,ABILLS                                                        
         OI    4(R2),X'20'                                                      
         MVI   ELCODE,X'30'        GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R6                                                       
         CLI   ACSTLEN,ACSTLNQ2    TEST FOR RIGHT LENGTH                        
         BL    DISREG4A            NO                                           
         TM    ACSTSTA2,X'C0'      TEST BILLABLE OPTION BITS                    
         BZ    DISREG4A                                                         
         MVI   8(R2),C'Y'                                                       
         TM    ACSTSTA2,X'80'                                                   
         BO    *+8                                                              
         MVI   8(R2),C'N'                                                       
*                                                                               
DISREG4A MVI   ELCODE,X'26'        TEST IF X JOB                                
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CHASW,C'Y'          TEST FOR CHANGE ALLOWED                      
         BNE   *+8                                                              
         NI    1(R2),X'FF'-X'20'   INIT FIELD TO UNPROTECTED                    
         USING ACJOBD,R6                                                        
         TM    ACJBSTAT,ACJBXJOB                                                
         BZ    DISREG6                                                          
*                                                                               
         MVI   8(R2),XJOB                                                       
         OI    1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'         VALIDATED PREV                               
*                                                                               
DISREG6  L     R2,AJOBAPP                                                       
         OI    4(R2),X'20'                                                      
         MVI   8(R2),C'Y'          Y=APPROVED, N=DRAFT                          
         TM    ACTRSTAT-ACTRECD(R4),ACTSDRFT                                    
         BZ    DISREG8                                                          
         MVI   8(R2),C'N'                                                       
*                                                                               
DISREG8  L     R2,AFILTS                                                        
         LA    R1,EFLTS                                                         
         LA    R0,NFLTS                                                         
*                                                                               
DISREG9  MVC   8(1,R2),0(R1)                                                    
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMP                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,DISREG9                                                       
*                                                                               
DISREGX  CR    RB,RB               SET CC=EQ ON EXIT                            
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OPTION LIST LINE ON SCREEN                             
*                                                                               
DISOPT   NTR1  ,                                                                
         MVC   PROTIT(L'OPTTIT),OPTTIT                                          
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         BAS   RE,ACCN             GET CLIENT,PRODUCT,JOB                       
         L     R2,AJOBNUM                                                       
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK                                                     
*                                                                               
DISOPT2  GOTO1 SETNAME,DMCB,AIO,WORK                                            
         L     R2,AJOBNAME         DISPLAY THE JOB NAME                         
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK                                                     
*                                                                               
         MVI   ELCODE,X'32'        GET BALANCE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACBALD,R6                                                        
         ZAP   BALANCE,ACBLFRWD                                                 
         AP    BALANCE,ACBLDR                                                   
         SP    BALANCE,ACBLCR                                                   
*                                                                               
DISOPT4  BAS   RE,SETGET           SETUP FOR GETOPT CALL                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
DISOPT6  L     R2,AOPT1                                                         
         MVC   PROTIT+21(L'OPTNAME1),OPTNAME1                                   
         MVI   PROTIT+21+L'OPTNAME1,X'5F'                                       
*                                                                               
         CLI   QOPT1,C'$'          SHOW BALANCE?                                
         BE    DISOPT7             YES                                          
*                                                                               
         GOTO1 VDISOPT,DMCB,(QOPT1,GOBLOCK),(R2)                                
         OI    4(R2),X'20'                                                      
         ZIC   R1,QOPT1                                                         
         SLL   R1,2                                                             
         L     RE,GOAFROM                                                       
         LA    RE,0(R1,RE)         INDEX INTO FROM BUFFER                       
         L     R2,AFROM1                                                        
         MVC   8(4,R2),0(RE)                                                    
*                                                                               
         LA    R0,LEVELS                                                        
         LA    RF,LEVTAB                                                        
         CLC   0(1,RE),0(RF)       MATCH ON ONE CHARACTER GETOPT LEVEL          
         BE    *+16                                                             
         LA    RF,L'LEVTAB(RF)                                                  
         BCT   R0,*-14                                                          
         B     DISOPT8                                                          
*                                                                               
         MVC   8(2,R2),1(RF)       MOVE OUT TWO CHARACTER LABEL                 
         B     DISOPT8                                                          
*                                                                               
DISOPT7  EDIT  (P8,BALANCE),(12,8(R2)),2,MINUS=YES                              
*                                                                               
DISOPT8  L     R2,AOPT2                                                         
         OI    4(R2),X'20'                                                      
         CLI   QOPT2,0             TEST FOR SECOND OPTION                       
         BE    DISOPTX             NO                                           
         MVC   PROTIT+50(L'OPTNAME2),OPTNAME2                                   
         MVI   PROTIT+50+L'OPTNAME2,X'5F'                                       
         CLI   QOPT2,C'$'          SHOW BALANCE?                                
         BE    DISOPT9             YES                                          
*                                                                               
         GOTO1 VDISOPT,DMCB,(QOPT2,GOBLOCK),(R2)                                
         ZIC   R1,QOPT2                                                         
         SLL   R1,2                                                             
         L     RE,GOAFROM                                                       
         LA    RE,0(R1,RE)                                                      
         L     R2,AFROM2                                                        
         MVC   8(4,R2),0(RE)                                                    
*                                                                               
         LA    R0,LEVELS                                                        
         LA    RF,LEVTAB                                                        
         CLC   0(1,RE),0(RF)       MATCH ON ONE CHARACTER GETOPT LEVEL          
         BE    *+16                                                             
         LA    RF,L'LEVTAB(RF)                                                  
         BCT   R0,*-14                                                          
         B     DISOPTX                                                          
*                                                                               
         MVC   8(2,R2),1(RF)       MOVE OUT TWO CHARACTER LABEL                 
         B     DISOPTX                                                          
*                                                                               
DISOPT9  EDIT  (P8,BALANCE),(12,8(R2)),2,MINUS=YES                              
*                                                                               
DISOPTX  CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY USER FIELD LIST SCREEN                                 
*                                                                               
* ON EXIT, CC=EQ IF JOB SHOULD BE DISPLAYED, CC=NEQ TO SKIP JOB                 
*                                                                               
DISUSER  NTR1  ,                                                                
         MVC   PROTIT,USERTIT                                                   
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVC   LISTAR,BLANKS                                                    
         LA    R3,LISTAR                                                        
         USING USPROTD,R3                                                       
         BAS   RE,ACCN                                                          
         MVC   USACCN,WORK                                                      
         MVI   ELCODE,X'A2'        SEARCH USER FIELD ELEMENTS                   
         BAS   RE,GETELIO                                                       
         B     DISUSER3                                                         
*                                                                               
DISUSER2 BAS   RE,NEXTEL                                                        
*                                                                               
DISUSER3 BE    DISUSER4                                                         
         B     DISUSERN                                                         
*                                                                               
         USING ACUFD,R6                                                         
DISUSER4 CLC   ACUFCODE,QUSER      MATCH ON USER FIELD CODE                     
         BNE   DISUSER2            NO                                           
         MVC   PROTIT+24(L'ACUFDESC),ACUFDESC  USER FIELD DESCRIPTION           
         CLI   ACUFLEN,ACUFDATA-ACUFD TEST FOR ANY DATA                         
         BE    *+12                NO                                           
         CLI   ACUFDATA,C' '       ANY SIGNIFICANT DATA                         
         BH    DISUSER5            YES                                          
*                                                                               
         CLI   QUSERF,C'+'         TEST FILTER JOBS WITH DATA                   
         BE    DISUSERN            YES-MUST REJECT THIS JOB                     
         B     DISUSER6                                                         
*                                                                               
DISUSER5 CLI   QUSERF,C'-'         TEST FILTER JOBS WITH NO DATA                
         BE    DISUSERN                                                         
*                                                                               
DISUSER6 ZIC   R1,ACUFLEN                                                       
         SH    R1,=Y(ACUFDATA-ACUFD+1)                                          
         BM    DISUSER8                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USDATA(0),ACUFDATA  EXTRACT DATA                                 
*                                                                               
DISUSER8 MVI   USREQ,C'N'                                                       
         TM    ACUFSTAT,X'80'      TEST REQUIRED FIELD                          
         BZ    *+8                                                              
         MVI   USREQ,C'Y'                                                       
*                                                                               
         MVI   USNFB,C'N'                                                       
         TM    ACUFSTAT,X'40'      TEST NEED FOR BILLS                          
         BZ    *+8                                                              
         MVI   USNFB,C'Y'                                                       
*                                                                               
         MVI   USSOE,C'N'                                                       
         TM    ACUFSTAT,X'20'      TEST SHOW ON ESTIMATES                       
         BZ    *+8                                                              
         MVI   USSOE,C'Y'                                                       
*                                                                               
         MVI   USSOB,C'N'                                                       
         TM    ACUFSTAT,X'10'      TEST SHOW ON BILLS                           
         BZ    *+8                                                              
         MVI   USSOB,C'Y'                                                       
*                                                                               
DISUSER9 L     R2,APROT                                                         
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DISUSERY CR    RB,RB                                                            
         B     DISUSERX                                                         
*                                                                               
DISUSERN LTR   RB,RB                                                            
*                                                                               
DISUSERX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OPTION = PR LIST LINE SCREEN                           
*                                                                               
DISOPR   NTR1  ,                                                                
         MVC   PROTIT(L'OPRTIT),OPRTIT                                          
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SET ON PREV VALIDATED BIT                    
         MVC   LISTAR,BLANKS                                                    
         LA    R3,LISTAR                                                        
         USING OPRPROTD,R3                                                      
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         BAS   RE,ACCN             GET CLIENT,PRODUCT,JOB                       
         L     R2,AJOBNUM                                                       
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OPRACCN,WORK                                                     
         MVC   OPROFF,EFFOFFC                                                   
*                                                                               
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISOPR10                                                         
         USING ACPROFD,R6                                                       
         MVC   OPRRECV,ACPRRECV+3 RECEIVABLE                                    
         MVC   OPRCOST,ACPRCOST+3 COSTING                                       
         DROP  R6                                                               
*                                                                               
DISOPR10 L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     DISOPRX                                                          
         MVC   8(0,R2),LISTAR      MOVE OUT PROTECTED FIELD DATA                
*                                                                               
DISOPRX  CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE DATE LIST LINE                                     
*                                                                               
DISDATE  NTR1  ,                                                                
         MVC   PROTIT(L'DATTIT),DATTIT                                          
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SET ON PREV VALIDATED BIT                    
                                                                                
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVC   LISTAR,BLANKS                                                    
         LA    R3,LISTAR                                                        
         USING DATPROTD,R3                                                      
         BAS   RE,ACCN                                                          
         MVC   DATACCN,WORK                                                     
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   DATNAME,WORK                                                     
         GOTO1 DATCON,DMCB,(1,BDATE),(8,DATOPEN)                                
         GOTO1 DATCON,DMCB,(1,ADATE),(8,DATACTV)                                
         GOTO1 DATCON,DMCB,(1,CDATE),(8,DATCHNG)                                
         OI    4(R2),X'20'                                                      
                                                                                
DISD02   L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     DISDATX                                                          
         MVC   8(0,R2),LISTAR      MOVE OUT PROTECTED FIELD DATA                
                                                                                
DISDATX  CR    RB,RB               SET CC=EQ ON EXIT                            
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
* NOTE-GENCON DISABLES READ FOR UPDATE FOR ACTION LIST (ACTNUM=ACTLIST)         
*                                                                               
EDT      NTR1  WORK=(R4,POINTLN)                                                
         ST    R4,APOINT           SAVE A(POINTER BLOCK)                        
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT8                                                             
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT8                YES                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         MVC   DUB(1),RECNUM                                                    
         CLI   5(R2),2             TEST FOR 2 BYTES OF INPUT                    
         BE    EDT3                YES                                          
         BH    ERREND              ITS 3-SO RETURN ERROR                        
*                                                                               
         MVI   DUB+1,ACTNDIS                                                    
         CLI   8(R2),C'S'          TEST 'S'=SELECT FOR DISPLAY                  
         BE    EDT6                                                             
         MVI   DUB+1,ACTNCHA                                                    
         CLI   8(R2),C'C'          TEST 'C'=CHANGE                              
         BE    EDT6                YES                                          
         MVI   DUB,RECNOPT                                                      
         MVI   DUB+1,ACTNMNT                                                    
         CLI   8(R2),C'O'          TEST 'O'=OPTION MAINT                        
         BE    EDT6                                                             
         MVC   DUB(1),RECNUM                                                    
         MVI   DUB+1,ACTNEST                                                    
         CLI   8(R2),C'E'          TEST 'E'=JOB ESTIMATE                        
         BE    EDT6                                                             
         B     ERREND                                                           
*                                                                               
EDT3     MVI   DUB+1,ACTNEL                                                     
         CLC   =C'EL',8(R2)        TEST FOR 'EL'=JOB ELIST                      
         BE    EDT6                YES                                          
         MVI   DUB+1,ACTNSUM                                                    
         CLC   =C'JS',8(R2)        TEST FOR 'JS'=JOB SUMMARY                    
         BE    EDT6                YES                                          
         MVI   DUB,RECNOPT                                                      
         MVI   DUB+1,ACTNLIST                                                   
         CLC   =C'OL',8(R2)        TEST FOR 'OL'=OPTION LIST                    
         BE    EDT6                                                             
         CLI   CHASW,C'Y'                                                       
         BNE   EDT7                                                             
         CLC   =C'DL',8(R2)        TEST FOR 'DL'=JOB DELETE                     
         BE    EDT6                                                             
         CLC   =C'OP',8(R2)        TEST FOR 'OP'=JOB REOPEN                     
         BE    EDT6                                                             
         CLC   =C'CL',8(R2)        TEST FOR 'CL'=JOB CLOSE                      
         BE    EDT6                                                             
         CLC   =C'LK',8(R2)        TEST FOR 'LK'=JOB LOCK                       
         BE    EDT6                                                             
         CLC   =C'UL',8(R2)        TEST FOR 'UL'=JOB UNLOCK                     
         BNE   ERREND                                                           
*                                                                               
EDT6     GOTO1 VCHKACC                                                          
         BE    EDT8                                                             
EDT7     MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
EDT8     LA    R4,KEY              READ THE JOB RECORD                          
         USING ACKEYD,R4                                                        
         MVC   KEY,BLANKS                                                       
         MVC   ACKEYACC,SELKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),SYSFIL,KEY,AIO,0                     
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           EXTRACT RETURNED KEY                         
         CLC   ACKEYD(ACLENGTH-ACKEYD),KEYSAVE                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         GOTO1 SETJOB                                                           
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINT                                 
         BAS   RE,ACCN             SET CLI/PRD/JOB CODES                        
         CLI   8(R2),C'J'          TEST FOR JOB SUMMARY                         
         BE    *+12                                                             
         CLI   8(R2),C'E'          TEST FOR ESTIMATE/ELIST SWAP                 
         BNE   EDT9                NO                                           
         TM    JOBJSTAT,ACJBNEWQ   TEST JOB USES NEW ESTIMATES                  
         BO    EDT9                YES                                          
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,JOBSMCSE                                                
         BO    ERREND                                                           
         MVI   ERROR,OLDESERR      NO                                           
         B     ERREND                                                           
*                                                                               
EDT9     OC    AEDT,AEDT           TEST FOR AN EDIT ROUTINE                     
         BZ    EDT10               NO                                           
         CLI   CHASW,C'Y'          TEST FOR CHANGE ALLOWED                      
         BNE   EDT10               NO                                           
         L     RF,AEDT                                                          
         BASR  RE,RF                                                            
*                                                                               
         CLI   SCRTYPE,C'R'        TEST FOR REGULAR SCREEN                      
         BNE   EDT9A                                                            
         CLI   UPDATE,C'Y'         TEST FOR CHANGE TO JOB                       
         BNE   EDT9A               NO                                           
*                                                                               
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINT                                 
*                                                                               
EDT9A    LA    R4,KEY              RE-READ THE JOB RECORD                       
         USING ACKEYD,R4                                                        
         MVC   KEY,BLANKS                                                       
         MVC   ACKEYACC,SELKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),SYSFIL,KEY,AIO,0                     
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           EXTRACT RETURNED KEY                         
         CLC   ACKEYD(ACLENGTH-ACKEYD),KEYSAVE                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINT                                 
*                                                                               
EDT10    CLI   5(R2),0             TEST FOR SELECT INPUT                        
         BE    EDT34               NO                                           
         CLI   8(R2),C'*'          TEST ALREADY THERE                           
         BE    EDT34                                                            
         MVC   SELACT,8(R2)        SAVE ACTION                                  
         MVC   8(3,R2),BLANKS      CLEAR SELECT FIELD                           
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT FIELD BACK                              
         MVI   PFKEY,0             CLEAR PFKEY BEFORE CALLING OVERLAY           
*                                                                               
EDT12    CLI   SELACT,C'S'         TEST FOR JOB MAINT SWAP                      
         BE    EDT13                                                            
         CLI   SELACT,C'C'                                                      
         BNE   EDT14                                                            
         CLI   SELACT+1,C'L'       CLOSE?                                       
         BE    EDT13A                                                           
         GOTO1 VCALL,WORK,=C'JOB',=C'CHANGE',(6,CLICODE),(6,PRODCODE), X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT13    GOTO1 VCALL,WORK,=C'JOB',=C'DISPLAY',(6,CLICODE),(6,PRODCODE),X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT13A   GOTO1 VCALL,WORK,=C'JOB',=C'CLOSE',(6,CLICODE),(6,PRODCODE),  X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT14    CLI   SELACT,C'O'         TEST FOR OPTION MAINT SWAP                   
         BNE   EDT16                                                            
         CLI   SELACT+1,C'L'       TEST FOR OPTION LIST SWAP                    
         BE    EDT15               YES                                          
         CLI   SELACT+1,C'P'       TEST FOR JOB OPEN                            
         BE    EDT24               YES                                          
         GOTO1 VCALL,WORK,=C'OPTION',=C'MAINT',=C',',=C',',(6,CLICODE),X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',0                            
*                                                                               
EDT15    GOTO1 VCALL,WORK,=C'OPTION',=C'LIST',=C',',=C',',(6,CLICODE), X        
               (6,PRODCODE),(6,JOBNUM),0                                        
*                                                                               
EDT16    CLI   SELACT,C'E'         TEST FOR JOB ESTIMATE SWAP                   
         BNE   EDT20                                                            
         CLI   SELACT+1,C'L'       TEST FOR JOB ELIST SWAP                      
         BE    EDT18                                                            
         GOTO1 VCALL,WORK,=C'JOB',=C'EST',(6,CLICODE),(6,PRODCODE),    X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT18    GOTO1 VCALL,WORK,=C'JOB',=C'ELIST',(6,CLICODE),(6,PRODCODE),  X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT20    CLI   SELACT,C'J'         TEST FOR JOB SUMMARY SWAP                    
         BNE   EDT22                                                            
         GOTO1 VCALL,WORK,=C'JOB',=C'SUMMARY',(6,CLICODE),(6,PRODCODE),X        
               (6,JOBNUM),0                                                     
*                                                                               
EDT22    CLC   =C'CL',SELACT       TEST FOR JOB CLOSE                           
         BNE   EDT24                                                            
         GOTO1 JOBCLS                                                           
         BAS   RE,LABELS           SEE IF WE NEED LABELS                        
         B     EDT34                                                            
*                                                                               
EDT24    CLC   =C'OP',SELACT       TEST FOR JOB OPEN                            
         BNE   EDT26                                                            
         GOTO1 JOBOPN                                                           
         BAS   RE,LABELS           SEE IF WE NEED LABELS                        
         B     EDT34                                                            
*                                                                               
EDT26    CLC   =C'DL',SELACT       TEST FOR JOB DELETE                          
         BNE   EDT28                                                            
         GOTO1 JOBDEL                                                           
         BAS   RE,LABELS           SEE IF WE NEED LABELS                        
         B     EDT34                                                            
*                                                                               
EDT28    CLC   =C'LK',SELACT       TEST FOR JOB LOCK                            
         BNE   EDT30                                                            
         MVI   ELCODE,ACSTELQ      GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACSTATD,R6                                                       
         OI    ACSTSTAT,X'20'      SET TO LOCKED                                
         B     EDT32                                                            
*                                                                               
EDT30    CLC   =C'UL',SELACT       TEST FOR JOB UNLOCK                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,ACSTELQ      GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACSTATD,R6                                                       
         NI    ACSTSTAT,X'FF'-X'20'   UNLOCK IT                                 
*                                                                               
EDT32    GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINT                                 
*                                                                               
EDT34    L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A REGULAR LIST LINE                                       
*                                                                               
EDTREG   NTR1  ,                                                                
         MVI   UPDATE,C'N'         SET UPDATE SWITCH TO NO                      
         L     R2,ABILLS           R2=A(BILLING SELECT FIELD)                   
         TM    4(R2),X'20'         TEST IF ANY CHANGE                           
         BO    EDTREG20                                                         
*                                                                               
         MVI   ELCODE,X'26'        TEST FOR XJOBS                               
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACJOBD,R6                                                        
         TM    ACJBSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BO    EDTREG20            YES DONT ADD BILLING SELECT EL               
                                                                                
         MVI   UPDATE,C'Y'                                                      
         MVI   ELCODE,ACSTELQ                                                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING ACSTATD,R6                                                       
         CLI   ACSTEL,ACSTELQ      TEST IF ELEM FOUND                           
         BE    *+6                                                              
         DC    H'0'                NO-BIG TROUBLE                               
         MVI   ACSTLEN,ACSTLNQ3    USE LARGER LENGTH                            
         NI    ACSTSTA2,X'FF'-X'C0'                                             
         CLI   5(R2),0                                                          
         BE    EDTREG5             ERASED SELECT FIELD                          
*                                                                               
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   WORK,C'Y'                                                        
         BNE   *+12                                                             
         OI    ACSTSTA2,X'80'                                                   
         B     EDTREG5                                                          
*                                                                               
         CLI   WORK,C'N'                                                        
         BNE   ERREND                                                           
         OI    ACSTSTA2,X'40'                                                   
*                                                                               
EDTREG5  GOTO1 ADDELEM                                                          
*                                                                               
EDTREG20 L     R2,AFILTS           R2=A(FILTER FIELD HEADER)                    
         LA    R0,NFLTS                                                         
EDTREG25 TM    4(R2),X'20'         PREV VALIDATED?                              
         BNO   EDTREG30            NO, UPDATE ELEMENT                           
         BAS   RE,BUMP                                                          
         BCT   R0,EDTREG25                                                      
         B     EDTREG40            NO FILTER VALUSE CHANGED                     
*                                                                               
EDTREG30 L     R2,AFILTS                                                        
         MVI   ELCODE,X'30'        GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         USING RSTELD,R6                                                        
         CLI   RSTLN,RSTLN3Q       IS THIS BIG ENOUGH FOR FILTER VALS           
         BNL   EDTREG35            YES                                          
*                                                                               
         MVI   ELCODE,RSTELQ       EXTRACT RSTEL TO "ELEMENT"                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT          UPDATE LENGTH                                
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 ADDELEM                                                          
         BAS   RE,GETELIO          READDRESS STATUS ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                ??!!!                                        
*                                                                               
EDTREG35 LA    R1,X'20'            UPDATE NON VALIDATED FIELDS ONLY             
         SLL   R1,24               X'20 TO THE HOB OF R1                        
         GOTO1 VALFLTS                                                          
         MVI   UPDATE,C'Y'                                                      
*                                                                               
EDTREG40 CLI   UPDATE,C'Y'                                                      
         BNE   EDTREGX                                                          
*                                                                               
         GOTO1 PERSIN                                                           
         GOTOR ACTMAIN                                                          
         GOTO1 WRITE                                                            
*                                                                               
         BAS   RE,ACCN             SET CLI/PRD/JOB CODES                        
*                                                                               
         MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL          READ CLIENT RECORD                           
         MVC   KEY+3(3),CLICODE                                                 
         GOTO1 READ                                                             
         GOTO1 SETCLI                                                           
*                                                                               
         MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL          READ PRODUCT RECORD                          
         MVC   KEY+3(3),CLICODE                                                 
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         GOTO1 READ                                                             
         GOTO1 SETPROD                                                          
*                                                                               
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),JOBNUM                                                   
         GOTO1 READ                                                             
         GOTO1 SETJOB                                                           
                                                                                
         GOTOR PREAMJD             PRE AMEND JOB DETAILS                        
         JAS   RE,MANJDTP          MANAGE JDT PASSIVES                          
*                                                                               
         L     R2,AJOBAPP                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
                                                                                
         L     R2,ABILLS                                                        
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
                                                                                
         L     R2,AFILTS                                                        
         LA    R0,NFLTS                                                         
EDTREG50 OI    4(R2),X'20'         MARK FILTER AS VALIDATED                     
         OI    6(R2),X'80'         XMIT BACK TO USER                            
         BAS   RE,BUMP                                                          
         BCT   R0,EDTREG50                                                      
*                                                                               
EDTREGX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT AN OPTIONS LIST LINE                                      
*                                                                               
EDTOPT   NTR1  WORK=(R4,POINTLN)                                                
         ST    R4,APOINTO                                                       
         MVI   ADDSW,C'Y'                                                       
         MVI   UPDATE,C'N'         SET UPDATE OPTIONS RECORD TO 'NO'            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ACOPKEY,R4                                                       
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL,CUL                                                      
         MVC   ACOPCLI,CLICODE     KEY FOR JOB LEVEL OPTIONS                    
         MVC   ACOPPRO,PRODCODE                                                 
         MVC   ACOPJOB,JOBNUM                                                   
         MVC   KEYSAVE,KEY         SAVE KEY BEFORE READ                         
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMRDHI'),SYSFIL,KEY,AIO,0                 
         L     RE,AIO                                                           
         MVC   KEY,0(RE)                                                        
         CLC   ACOPKEY,KEYSAVE     TEST IF JOB LEVEL OPTION EXISTS              
         BNE   EDTOPT1             NO                                           
*                                                                               
         MVI   ADDSW,C'N'          NOT ADDING OPTION                            
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINTO                                
         TM    ACSTATUS,X'80'      TEST IF RECORD IS DELETED                    
         BO    EDTOPT1             YES                                          
         B     EDTOPT2                                                          
*                                                                               
EDTOPT1  L     R4,AIO                                                           
         LR    RE,R4               BUILD SKELETAL RECORD                        
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   ACOPKEY,KEYSAVE                                                  
         MVC   ACLENGTH,=Y(ACRECORD+1-ACKEYD)                                   
         CLI   ADDSW,C'Y'          TEST ADDING RECORD                           
         BNE   EDTOPT2             NO-HAVE SAVED POINTERS ALREADY               
         GOTO1 VSAVPTRS,DMCB,(X'80',0),APOINTO                                  
*                                                                               
EDTOPT2  L     R2,AOPT1                                                         
         TM    1(R2),X'20'         TEST IF PROTECTED FIELD                      
         BO    EDTOPT4             YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    EDTOPT4             NO                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         MVC   ACOPNUM,QOPT1                                                    
         BAS   RE,OPTVAL                                                        
*                                                                               
EDTOPT4  L     R2,AOPT2                                                         
         TM    1(R2),X'20'                                                      
         BO    EDTOPT6                                                          
         TM    4(R2),X'20'                                                      
         BO    EDTOPT6                                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         MVC   ACOPNUM,QOPT2                                                    
         BAS   RE,OPTVAL                                                        
*                                                                               
EDTOPT6  CLI   UPDATE,C'Y'                                                      
         BNE   EDTOPTX                                                          
*                                                                               
         GOTO1 PERSIN                                                           
         CLI   ADDSW,C'Y'          TEST IF ADDING OPTION                        
         BNE   EDTOPT8                                                          
         MVI   ELCODE,ACOPELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   EDTOPT10            NO ELEMENTS ON RECORD-SKIP ADD               
         GOTO1 ADD                                                              
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINTO                                
         B     EDTOPT10                                                         
*                                                                               
EDTOPT8  MVI   ELCODE,ACOPELQ                                                   
         BAS   RE,GETELIO          TEST IF ANY OPTIONS ELEMENTS                 
         BE    *+12                YES                                          
         L     R4,AIO                                                           
         OI    ACSTATUS,X'80'                                                   
         GOTO1 WRITE                                                            
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINTO                                
*                                                                               
EDTOPT10 L     R2,AOPT1                                                         
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         L     R2,AOPT2                                                         
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
EDTOPTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A OPTION = PR LIST LINE                                   
*                                                                               
EDTOPR   NTR1  ,                                                                
                                                                                
                                                                                
*                                                                               
EDTOPRX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT OPTION VALUE FIELD                                        
*                                                                               
* AT ENTRY, R2=A(FIELD HEADER TO VALIDATE), R6=A(SKELETON ELEMENT)              
* ON EXIT, SETS UPDATE IF FIELD HAS REALLY CHANGED                              
*                                                                               
OPTVAL   NTR1  ,                                                                
         CLI   5(R2),0             TEST IF ANY INPUT                            
         BNE   OPTVAL2             YES                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         CLI   12(R1),0            TEST IF JOB HAS THIS OPTION                  
         BNE   OPTVALX             NO-NO REAL CHANGE                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         BAS   RE,BUMP             ADVANCE TO FROM FIELD                        
         XC    8(4,R2),8(R2)       CLEAR IT                                     
         OI    6(R2),X'80'                                                      
         B     OPTVALX                                                          
*                                                                               
OPTVAL2  MVI   ERROR,INVALID                                                    
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   ERREND                                                           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACOPELQ',AIO),(1,ACOPNUM)             
         GOTO1 ADDELEM                                                          
         GOTO1 VDISOPT,DMCB,(R6),(R2)                                           
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMP             AHEAD TO FROM FIELD                          
         MVC   8(4,R2),=C'JB**'                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
OPTVALX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUBROUTINE TO DETERMINE IF LABELS ARE REQUIRED, AND GENERATE                  
*                                                                               
LABELS   NTR1                                                                   
         BAS   RE,SETGET           SETUP FOR GETOPT                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLI   GOSUPSTI,C'Y'                                                    
         BE    LABELX                                                           
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),BLANKS                                            
         MVC   ELEMENT+26(2),=C'12'                                             
         MVC   ELEMENT+28(1),CUL                                                
         MVC   ELEMENT+29(1),SAVEOFC                                            
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPROFD,R6                                                       
         CLC   ACPROFFC,BLANKS                                                  
         BNH   *+10                                                             
         MVC   ELEMENT+29(1),ACPROFFC                                           
         L     RE,AIO                                                           
         MVC   ELEMENT+35(15),0(RE)                                             
         GOTO1 DATAMGR,DMCB,DMADD,=C'ACCREQS',ELEMENT,ELEMENT,0                 
         CLI   DMCB+8,0                                                         
         BE    LABELX                                                           
         DC    H'0'                                                             
*                                                                               
LABELX   B     XIT                                                              
         EJECT                                                                  
* SUBROUTINE TO SET UP GOBLOCK FOR GETOPT CALL                                  
*                                                                               
SETGET   ST    RE,SAVERE                                                        
         LA    RE,BUFF                                                          
         LA    RE,1000(RE)         CLEAR OPTIMIZATION                           
         L     RF,OPTBUFFL         LENGTH OF OPTIONS BUFFER                     
         LA    RF,GOFROML(RF)      ADD ROOM FOR FROM BUFFER                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   GOADM,DATAMGR       READ THE JOB'S OPTIONS                       
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOANYWC,C'N'        NO WORKCODE OPTIONS                          
         MVI   GOWHICH,C'N'        ONLY NEW OPTIONS                             
         MVC   GOACOMP,ACOMP       COMPANY RECORD                               
         MVC   GOACLI,ACLI                                                      
         MVC   GOAPRO,APRO                                                      
         MVC   GOAJOB,AIO                                                       
                                                                                
         LA    R1,BUFF                                                          
         LA    R1,1000(R1)         BUFF+1000=A(BUFFER)                          
         ST    R1,GOABUFF          OPTIMIZATION BUFFER                          
                                                                                
         MVC   GOLBUFF,OPTBUFFL                                                 
         A     R1,OPTBUFFL         INDEX PAST OPT BUFFER                        
         ST    R1,GOAFROM          USE IT FOR FROM BUFFER                       
*                                  CLEAR GOAFROM                                
         LR    RE,R1                                                            
         LHI   RF,GOFROML                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   GOAKEY,AIO                                                       
         LA    R1,GOPANBLK         A(PANEL BLOCK)                               
         ST    R1,GOAPAN                                                        
         LA    R1,GOXBLOCK         A(EXTENSION BLOCK)                           
         ST    R1,GOAEXT                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT ACCOUNT NUMBER AT WORK.                                 
*                                                                               
* AT ENTRY, R4=A(KEY)                                                           
* ON EXIT, ALSO SETS CLICODE, PRODCODE, JOBNUM                                  
*                                                                               
ACCN     NTR1  ,                                                                
         USING ACKEYD,R4                                                        
         MVC   WORK,BLANKS         PRE-CLEAR OUTPUT AREA TO SPACES              
         LA    RE,WORK             RE=A(OUTPUT STRING)                          
         LA    RF,ACKEYACC+3                                                    
         MVC   CLICODE,BLANKS                                                   
         MVC   PRODCODE,BLANKS                                                  
         MVC   JOBNUM,BLANKS                                                    
*                                                                               
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,ACCMOVE                                                       
         EX    R1,CLIMOVE                                                       
         LA    RE,2(R1,RE)         NEXT OUTPUT POSITION                         
         LA    RF,1(R1,RF)         POINT TO PRODUCT                             
*                                                                               
         ZIC   R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,ACCMOVE                                                       
         EX    R1,PRODMOVE                                                      
         LA    RE,2(R1,RE)                                                      
         LA    RF,1(R1,RF)                                                      
*                                                                               
         ZIC   R1,LJOB                                                          
         BCTR  R1,0                                                             
         EX    R1,ACCMOVE                                                       
         EX    R1,JOBMOVE                                                       
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
ACCMOVE  MVC   0(0,RE),0(RF)                                                    
CLIMOVE  MVC   CLICODE(0),0(RF)                                                 
PRODMOVE MVC   PRODCODE(0),0(RF)                                                
JOBMOVE  MVC   JOBNUM(0),0(RF)                                                  
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         EJECT                                                                  
* MAINTAIN ACTIVITY ON CHANGE JOB                                               
         USING RACELD,R6                                                        
ACTMAIN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,RACELQ                                                    
         JAS   RE,GETELIO                                                       
         JNE   ACTMAIN5                                                         
         XR    R0,R0                                                            
         XR    RE,RE                                                            
         XC    WORK,WORK                                                        
         CLI   RACTYPE,RACTCHA                                                  
         JNE   ACTMAIN0                                                         
         MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         AHI   RE,1                                                             
                                                                                
ACTMAIN0 IC    R0,RACLN                                                         
         AR    R6,R0                                                            
         CLI   RACEL,0                                                          
         JE    ACTMAIN3                                                         
                                                                                
ACTMAIN1 CLI   RACEL,RACELQ                                                     
         JNE   ACTMAIN0                                                         
         CLI   RACTYPE,RACTCHA                                                  
         JNE   ACTMAIN0                                                         
         AHI   RE,1                                                             
         OC    WORK(4),WORK                                                     
         JZ    ACTMAIN2                                                         
         CLC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         JL    ACTMAIN0                                                         
                                                                                
ACTMAIN2 MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         J     ACTMAIN0                                                         
                                                                                
ACTMAIN3 CHI   RE,RACMAXQ                                                       
         JNL   ACTMAIN4                                                         
         LA    R1,2000                                                          
         SHI   R1,RACMAXQ*50                                                    
         L     RF,AIO                                                           
         AHI   RF,ACCORLEN                                                      
         CLM   R1,3,0(RF)                                                       
         JH    ACTMAIN5                                                         
                                                                                
ACTMAIN4 L     R6,WORK           REPLACE ELEMENT                                
         LTR   R6,R6                                                            
         BZ    ACTMAIN5                                                         
         MVI   BYTE,0                                                           
         B     ACTMAIN6                                                         
                                                                                
ACTMAIN5 LA    R6,ELEMENT        ADD NEW CHANGE RACELD                          
         MVI   BYTE,1                                                           
                                                                                
ACTMAIN6 XC    RACEL(RACLNQ),RACEL                                              
         MVI   RACEL,RACELQ      BUILD NEW ACTIVITY ELEMENT                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,FAPASSWD                                                 
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,FATIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         DROP  R1                                                               
         CLI   BYTE,0                                                           
         JE    ACTMAINX                                                         
         MVI   ELCODE,RACELQ                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
ACTMAINX B     XIT                                                              
         DROP  R6                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
XIT      XIT1                                                                   
*                                                                               
LISTMSG  DS    0CL42                                                            
LIST2MSG DS    0CL14                                                            
         DC    C'LIST DISPLAYED'                                                
         DC    C' - PRESS ENTER FOR NEXT PAGE'                                  
*                                                                               
LIST3MSG DC    C'THERE ARE NO MORE RECORDS TO DISPLAY'                          
LIST4MSG DC    C'THERE ARE NO RECORDS TO DISPLAY     '                          
EDTMSG   DC    C'CHANGES COMPLETED'                                             
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMADD    DC    CL8'DMADD '                                                      
DMGET    DC    CL8'GETREC'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
RELO     DC    A(0)                                                             
OPTBUFFL DC    F'4000'                                                          
BLANKS   DC    CL255' '            USE INSTEAD OF 'SPACES'                      
*                                                                               
* SCREEN TYPE TABLE                                                             
*                                                                               
         DS    0F                                                               
TYPETAB  DS    0CL20                                                            
         DC    C'R',AL1(14),AL1(REGFLDS),X'00'                                  
         DC    A(DISREG),A(EDTREG),A(REGELS),A(REGPF)                           
                                                                                
         DC    C'U',AL1(14),AL1(USERFLDS),X'00'                                 
         DC    A(DISUSER),A(0),A(USRELS),A(OPTPF)                               
                                                                                
         DC    C'P',AL1(14),AL1(OPRFLDS),X'00'                                  
         DC    A(DISOPR),A(EDTOPR),A(OPRELS),A(OPTPF)                           
                                                                                
         DC    C'O',AL1(7),AL1(OPTFLDS),X'00'                                   
         DC    A(DISOPT),A(EDTOPT),A(OPTELS),A(OPTPF)                           
                                                                                
         DC    C'D',AL1(14),AL1(DATFLDS),X'00'                                  
         DC    A(DISDATE),A(0),A(DATELS),A(OPTPF)                               
                                                                                
TYPES    EQU   (*-TYPETAB)/L'TYPETAB                                            
*                                                                               
* LEVEL ABBREVIATION TABLE                                                      
*                                                                               
*   BYTE 0    = 1 CHARACTER GETOPT CODE                                         
*   BYTES 1-2 = 2 CHARACTER OPTION MAINT CODE                                   
*                                                                               
LEVTAB   DS    0CL3                                                             
         DC    C'D',C'DF'                                                       
         DC    C'A',C'AG'                                                       
         DC    C'G',C'OG'                                                       
         DC    C'O',C'OF'                                                       
         DC    C'C',C'CL'                                                       
         DC    C'P',C'PR'                                                       
         DC    C'J',C'JB'                                                       
LEVELS   EQU   (*-LEVTAB)/L'LEVTAB                                              
                                                                                
REGTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   REGTIT                                                           
         DC    CL4'SEL'                                                         
         DC    CL16'CLT/PRD/JOB #'                                              
         DC    CL22'JOB NAME'                                                   
         DC    CL12'   BALANCE'                                                 
         DC    CL8'CLOSING^'                                                    
         ORG   REGTIT+63                                                        
         DC    C'BL^AP^1 2 3 4 5^'                                              
         ORG                                                                    
                                                                                
OPTTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   OPTTIT                                                           
         DC    CL5'SEL'                                                         
         DC    C'JOB CODE/NAME^'                                                
         ORG                                                                    
                                                                                
OPRTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   OPRTIT                                                           
         DC    CL4'SEL'                                                         
         DC    CL18'CLT/PRD/JOB #'                                              
         DC    CL6'OFF'                                                         
         DC    CL13'RECEIVABLE'                                                 
         DC    CL13'COSTING'                                                    
         ORG                                                                    
                                                                                
USERTIT  DC    CL(L'PROTIT)' '                                                  
         ORG   USERTIT                                                          
         DC    CL8'SEL'                                                         
         DC    CL16'CLT/PRD/JOB #'                                              
         DC    CL32'USER FIELD'                                                 
         DC    C'REQ^NFB^SOE^SOB^'                                              
         ORG                                                                    
                                                                                
DATTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   DATTIT                                                           
         DC    CL4'SEL'                                                         
         DC    CL18'CLT/PRD/JOB #'                                              
         DC    CL24'JOB NAME'                                                   
         DC    CL12'BAL B/FD'                                                   
         DC    CL12'ACTIVITY'                                                   
         DC    CL8'CHANGED'                                                     
         ORG                                                                    
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* ELEMENT TABLES FOR SCREEN GENERATION                                          
*                                                                               
REGELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(58),X'20',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(65),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(68),AL1(01),X'20',AL1(0)          
*        FILTER VALUES                                                          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(71),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(73),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(75),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(77),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(79),AL1(01),X'08',AL1(0)          
         DC    X'00'                                                            
REGELSLN EQU   *-REGELS                                                         
                                                                                
OPTELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(07),AL1(14),X'28',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(23),AL1(20),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(45),AL1(04),X'20',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(52),AL1(20),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(74),AL1(04),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(05),AL1(36),X'20',AL1(0)          
         DC    X'00'                                                            
OPTELSLN EQU   *-OPTELS                                                         
                                                                                
OPRELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(50),X'20',AL1(0)          
         DC    X'00'                                                            
OPRELSLN EQU   *-OPRELS                                                         
                                                                                
USRELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(10),AL1(70),X'20',AL1(0)          
         DC    X'00'                                                            
USRELSLN EQU   *-USRELS                                                         
                                                                                
DATELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(74),X'20',AL1(0)          
         DC    X'00'                                                            
DATELSLN EQU   *-DATELS                                                         
                                                                                
REGPF    DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'S=JOB DIS,C=JOB CHA,O=OPT MAINT,OL=OPT LIST,E=JOB EX        
               ST,EL=JOB ELIST,JS=JOB SUM'                                      
         DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=PRODUCT LIST  GLOBAL BILLING SELECT - PF9=Y,PF1X        
               0=N,PF11=ERASE'                                                  
         DC    X'00'                                                            
*                                                                               
OPTPF    DC    X'01',AL1(7+78),AL1(0),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'S=JOB DIS,C=JOB CHA,O=OPT MAINT,OL=OPT LIST,E=JOB EX        
               ST,EL=JOB ELIST'                                                 
         DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=PRODUCT LIST'                                           
         DC    X'00'                                                            
         EJECT                                                                  
         EJECT                                                                  
* EXTRACT PRE AMEND JOB DETAILS *                                               
         USING ACTRECD,R3                                                       
PREAMJD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    SVJDPAS,SVJDPAS                                                  
                                                                                
         L     R2,ASEL                                                          
         JAS   RE,BUMP                                                          
         AHI   R2,8                                                             
         MVC   SVJDOFF,EFFOFFC                                                  
                                                                                
         L     R1,AIO                                                           
         LA    R3,KEY2                                                          
         MVC   ACTKEY,0(R1)                                                     
                                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY                         
         JNE   *+2                                                              
                                                                                
         MVC   SVJDDA,ACTKDA                                                    
         MVC   SVJDSTAB,ACTKSTAT                                                
                                                                                
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIO3,DMWORK                     
         JNE   *+2                                                              
                                                                                
         L     R3,AIO3                                                          
         USING JOBELD,R4                                                        
         LA    R4,ACTRFST                                                       
                                                                                
PREAJ02  CLI   JOBEL,0                                                          
         JE    PREAJ06                                                          
         CLI   JOBEL,JOBELQ                                                     
         JE    PREAJ04                                                          
         LLC   R0,JOBLN                                                         
         AR    R4,R0                                                            
         J     PREAJ02                                                          
                                                                                
PREAJ04  GOTO1 DATCON,DMCB,(1,JOBCDATE),(2,SVJDICD)                             
         CLI   JOBLN,JOBLN1Q                                                    
         JNH   PREAJ06                                                          
         GOTO1 DATCON,DMCB,(1,JOBODATE),(2,SVJDOPN)                             
                                                                                
PREAJ06  L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
                                                                                
PREAJ08  CLI   JOBEL,0                                                          
         JE    PREAJ12                                                          
         CLI   JOBEL,JOBELQ                                                     
         JE    PREAJ10                                                          
         LLC   R0,JOBLN                                                         
         AR    R4,R0                                                            
         J     PREAJ08                                                          
                                                                                
PREAJ10  GOTO1 DATCON,DMCB,(1,JOBCDATE),(2,SVJDOCD)                             
                                                                                
PREAJ12  DS    0H                                                               
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
* MANAGE JDT PASSIVES *                                                         
         USING ACTRECD,R3                                                       
MANJDTP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
REC      USING ACTRECD,R6                                                       
         LA    R3,KEY2             GET POST UPDATE STATUS                       
         L     R6,AIO                                                           
         MVC   ACTKEY,REC.ACTKEY                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY                         
         JNE   *+2                                                              
                                                                                
         CLC   SVJDDA,ACTKDA       ENSURE SAME D/A AND DIFFERENT STATUS         
         JNE   *+2                                                              
         MVC   SVJDSTAA,ACTKSTAT                                                
         CLC   SVJDSTAB,SVJDSTAA                                                
***      JE    *+2                 ALLOW NOT DIE AS JOB/LIST ALLOWS FOR         
***      JE    MANJDTPX            LOCKING LOCKED JOBS ... OR SKIP?             
         DROP  R3                                                               
                                                                                
         USING JDTPASD,R3                                                       
         LA    R3,KEY2                                                          
                                                                                
         OC    SVJDOPN,SVJDOPN    ANY OPEN DATE?                                
         JZ    MANJP10                                                          
         XC    JDTPASD(ACCKLEN),JDTPASD                                         
         MVI   JDTPTYP,JDTPTYPQ                                                 
         MVI   JDTPSUB,JDTPSUBQ                                                 
         MVC   JDTPCPY,REC.ACTKCPY                                              
         MVI   JDTPDTYP,JDTPDTOQ                                                
         MVC   JDTPDATE,SVJDOPN                                                 
         MVC   JDTPOFFC,SVJDOFF                                                 
         MVC   JDTPJACT,REC.ACTKACT                                             
                                                                                
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,JDTPAS,JDTPAS,0               
         JNE   MANJP10                                                          
                                                                                
         MVC   JDTPSTA(1),SVJDSTAA                                              
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,JDTPAS,JDTPAS,0                        
         JNE   *+2                                                              
                                                                                
MANJP10  OC    SVJDICD,SVJDICD     ANY 'BEFORE' CLOSE DATE?                     
         JZ    MANJP20                                                          
                                                                                
         XC    JDTPASD(ACCKLEN),JDTPASD                                         
         MVI   JDTPTYP,JDTPTYPQ                                                 
         MVI   JDTPSUB,JDTPSUBQ                                                 
         MVC   JDTPCPY,REC.ACTKCPY                                              
         MVI   JDTPDTYP,JDTPDTCQ                                                
         MVC   JDTPDATE,SVJDICD                                                 
         MVC   JDTPOFFC,SVJDOFF                                                 
         MVC   JDTPJACT,REC.ACTKACT                                             
                                                                                
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,JDTPAS,JDTPAS,0               
         JNE   MANJP20                                                          
                                                                                
         MVC   JDTPSTA(1),SVJDSTAA IF CLOSE DATE CHANGED DELETE ELSE            
         CLC   SVJDICD,SVJDOCD     UPDATE STATUS                                
         JE    MANJP12                                                          
         OI    JDTPSTA,ACTSDELT                                                 
                                                                                
MANJP12  GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,JDTPAS,JDTPAS,0                        
         JNE   *+2                                                              
                                                                                
MANJP20  OC    SVJDOCD,SVJDOCD     ANY 'AFTER' CLOSE DATE?                      
         JZ    MANJDTPX                                                         
         CLC   SVJDICD,SVJDOCD     READY IF CLSOED DATE UNCHANGED               
         JE    MANJDTPX                                                         
                                                                                
         XC    JDTPASD(ACCKLEN),JDTPASD                                         
         MVI   JDTPTYP,JDTPTYPQ                                                 
         MVI   JDTPSUB,JDTPSUBQ                                                 
         MVC   JDTPCPY,REC.ACTKCPY                                              
         MVI   JDTPDTYP,JDTPDTCQ                                                
         MVC   JDTPDATE,SVJDOCD                                                 
         MVC   JDTPOFFC,SVJDOFF                                                 
         MVC   JDTPJACT,REC.ACTKACT                                             
         MVC   SAVEKEY,JDTPASD                                                  
                                                                                
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,JDTPAS,JDTPAS,0               
         JE    MANJP22                                                          
         CLI   8(R1),X'02'         FOUND OR DELETED - WRITE                     
         JE    MANJP22                                                          
                                                                                
         XC    KEY2,KEY2           ADD IT                                       
         MVC   JDTPAS,SAVEKEY                                                   
         MVC   JDTPSTA,SVJDSTAA                                                 
         MVC   JDTPDA,SVJDDA                                                    
                                                                                
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,JDTPASD,JDTPASD,0                      
         JNE   *+2                                                              
         J     MANJDTPX                                                         
                                                                                
MANJP22  MVC   JDTPSTA(1),SVJDSTAA                                              
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,JDTPAS,JDTPAS,0                        
         JNE   *+2                                                              
                                                                                
MANJDTPX DS    0H                                                               
         B     XIT                                                              
         DROP  R3,REC                                                           
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
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*ACGENRAC                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
*FAFACTS                                                                        
       PRINT OFF                                                                
       ++INCLUDE FAFACTS                                                        
       PRINT ON                                                                 
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   ACIOBLOK            USE THE ACIO BLOCK                           
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
CHASW    DS    C                                                                
*                                                                               
QCONTROL DS    0C                                                               
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QJOB     DS    CL(L'JOBNUM)                                                     
QMED     DS    CL1                                                              
QOFF     DS    CL2                                                              
QFILTS   DS    0CL5                                                             
QFILT1   DS    C                                                                
QFILT2   DS    C                                                                
QSUBC    DS    C                                                                
         ORG   QSUBC                                                            
QFILT3   DS    C                                                                
QFILT4   DS    C                                                                
QFILT5   DS    C                                                                
NFILTS   EQU   *-QFILTS                                                         
QCLOSED  DS    C                   C=CLOSED,O=OPEN                              
QLOCKED  DS    C                   L=LOCKED,U=UNLOCKED                          
QXJOB    DS    X                   X=XJOBS ONLY,N=NO XJOBS                      
QUSER    DS    CL2                 USER FIELD CODE                              
QUSERF   DS    C                   USER FIELD FILTER (+ OR -)                   
QOPT1    DS    X                   OPTION 1                                     
QOPT2    DS    X                   OPTION 2                                     
QDTYPE   DS    C                   DATE TYPE                                    
QCONTRLN EQU   *-QCONTROL                                                       
*                                                                               
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
SCRTYPE  DS    C                   SCREEN TYPE (R,O,U)                          
NLINES   DS    X                   N'LIST SCREEN LINES                          
NFIELDS  DS    X                   N'FIELDS PER LIST SCREEN LINE                
         DS    X                                                                
ADIS     DS    A                                                                
AEDT     DS    A                                                                
ALISTELS DS    A                                                                
APFELS   DS    A                                                                
APOINT   DS    A                   A(POINTER BLOCK)                             
APOINTO  DS    A                   A(POINTER BLOCK FOR OPTION REC)              
*                                                                               
OPTST1   DS    C                   FIELD STATUS-OPTION 1                        
OPTNAME1 DS    CL(L'OPTBDESC)      FIRST OPTION NAME                            
OPTST2   DS    C                   FIELD STATUS-OPTION 2                        
OPTNAME2 DS    CL(L'OPTBDESC)      FIRST OPTION NAME                            
*                                                                               
ACOMP    DS    A                   A(COMPANY RECORD)                            
ACLI     DS    A                   A(CLIENT RECORD)                             
APRO     DS    A                   A(PRODUCT RECORD)                            
*                                                                               
AFSTSEL  DS    A                                                                
APFFLD   DS    A                                                                
AENDSCR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
*                                                                               
APROT    DS    A                   REGULAR FIELDS                               
ABILLS   DS    A                                                                
AJOBAPP  DS    A                                                                
AFILTS   DS    0A                                                               
AF1      DS    A                                                                
AF2      DS    A                                                                
AF3      DS    A                                                                
AF4      DS    A                                                                
AF5      DS    A                                                                
ANEXTSEL DS    A                                                                
         ORG   APROT                                                            
AJOBNUM  DS    A                                                                
AOPT1    DS    A                   OPTION FIELDS                                
AFROM1   DS    A                                                                
AOPT2    DS    A                                                                
AFROM2   DS    A                                                                
AJOBNAME DS    A                                                                
         ORG   ANEXTSEL+L'ANEXTSEL                                              
*                                                                               
FLAG     DS    XL1                                                              
BALANCE  DS    PL8                                                              
EXLEN    DS    XL1                                                              
UPDATE   DS    C                                                                
ADDSW    DS    C                                                                
*                                                                               
SDATE    DS    XL3                 START DATE                                   
EDATE    DS    XL3                 END DATE                                     
ADATE    DS    XL3                 ACTIVITY DATE                                
BDATE    DS    XL3                 BALANCE FORW'D DATE                          
CDATE    DS    XL3                 LAST CHANGE DATE                             
*                                                                               
SVPASSWD DS    XL2                                                              
SVJDPAS  DS    0XL21                                                            
SVJDOPN  DS    XL2                                                              
SVJDICD  DS    XL2                                                              
SVJDOCD  DS    XL2                                                              
SVJDDA   DS    XL4                                                              
SVJDSTAB DS    XL1                                                              
SVJDSTAA DS    XL8                                                              
SVJDOFF  DS    XL2                                                              
KEY2     DS    XL64                                                             
SAVEKEY  DS    XL64                                                             
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACPANBLOCK                                                     
       ++INCLUDE ACGOXBLOCK                                                     
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROE3D                                                       
*                                                                               
         ORG   T60BFFD+X'E00'-SAVAREAL-LSAVESLN                                 
LSAVES   DS    0X                                                               
SAVEOFC  DS    CL2                 SAVE OFFICE FOR LABELS                       
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LLASTJOB DS    CL(L'ACKEYACC)      LAST JOB ON SCREEN                           
LSELTAB  DS    CL(15*SELTABL)                                                   
LSAVESLN EQU   *-LSAVES                                                         
*                                                                               
* EQUATES                                                                       
*                                                                               
REGFLDS  EQU   9                   N'FIELDS ON REGULAR LIST SCREEN              
USERFLDS EQU   2                   N'FIELDS ON USER LIST SCREEN                 
OPTFLDS  EQU   7                   N'FIELDS ON OPTION LIST SCREEN               
OPRFLDS  EQU   2                   N'FILEDS ON OPTION = PR LIST SCR             
DATFLDS  EQU   2                   N'FILEDS ON DATE LIST SCREEN                 
*                                                                               
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
XJOB     EQU   C'X'                DISPLAY FOR XJOBS IN BILLING COL             
POINTLN  EQU   8*54+1              LENGTH OF POINTER BLOCK                      
*                                                                               
* DSECT TO COVER REGULAR LIST LINE PROTECTED FIELD                              
*                                                                               
REGPROTD DSECT                                                                  
REGACCN  DS    CL14                                                             
         DS    CL2                                                              
REGNAME  DS    CL20                                                             
         DS    CL1                                                              
REGBAL   DS    CL12                                                             
         DS    CL1                                                              
REGCLOSE DS    CL8                                                              
*                                                                               
* DSECT TO COVER OPTION = PR LIST LINE PROTECTED FIELD                          
*                                                                               
OPRPROTD DSECT                                                                  
OPRACCN  DS   CL14                                                              
         DS   CL4                                                               
OPROFF   DS   CL2                                                               
         DS   CL4                                                               
OPRRECV  DS   CL12                                                              
         DS   CL1                                                               
OPRCOST  DS   CL12                                                              
*                                                                               
* DSECT TO COVER USER FIELD LIST LINE (ALL PROTECTED FOR NOW)                   
*                                                                               
USPROTD  DSECT                                                                  
USACCN   DS    CL14                                                             
         DS    CL2                                                              
USDATA   DS    CL30                                                             
         DS    CL2                                                              
USREQ    DS    C                                                                
         DS    CL3                                                              
USNFB    DS    C                                                                
         DS    CL3                                                              
USSOE    DS    C                                                                
         DS    CL3                                                              
USSOB    DS    C                                                                
*                                                                               
* DSECT TO COVER DATE LIST LINE PROTECTED FIELD                                 
*                                                                               
DATPROTD DSECT                                                                  
DATACCN  DS    CL14                                                             
         DS    CL4                                                              
DATNAME  DS    CL20                                                             
         DS    CL4                                                              
DATOPEN  DS    CL8                                                              
         DS    CL4                                                              
DATACTV  DS    CL8                                                              
         DS    CL4                                                              
DATCHNG  DS    CL8                                                              
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    CL3                 SELECT ACTION                                
SELKEY   DS    CL(L'ACKEYACC)                                                   
SELTABL  EQU   *-SELTABD                                                        
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACPRO13   04/10/15'                                      
         END                                                                    
