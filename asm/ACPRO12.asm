*          DATA SET ACPRO12    AT LEVEL 050 AS OF 02/14/12                      
*PHASE T60B12A                                                                  
         TITLE 'T60B12 - PRODUCT LIST'                                          
T60B12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B12**,R7,RR=R2                                              
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    PRO2                                                             
         CLI   MODE,VALREC                                                      
         BE    PRO4                                                             
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
PRO2     LA    RE,LOCAL            CLEAR LOCAL WORKING STORAGE                  
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED                                                        
*                                                                               
         MVI   CHASW,C'Y'          SET CHANGES ARE OK                           
         CLI   AUTHOR,0                                                         
         BE    PRO3                                                             
         TM    AUTHOR,CAT1Q        TEST FOR CATEGORY 1                          
         BNZ   *+8                                                              
         MVI   CHASW,C'N'                                                       
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    *+8                                                              
         MVI   CHASW,C'N'                                                       
*                                                                               
PRO3     MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES-BUILD THE SCREEN                         
         B     XIT                                                              
*                                                                               
* VALREC LOGIC-DISPLAY OR CHANGE                                                
*                                                                               
PRO4     BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    PRO6                                                             
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    PRO10               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
PRO6     GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LLASTPRO,LLASTPRO   CLEAR OUT LAST CLIENT LISTED                 
*                                                                               
PRO7     BAS   RE,LIST                                                          
         L     R2,AFSTSEL                                                       
         CLC   LNLISTS,NOLINES     TEST IF SCREEN FILLED                        
         BE    PRO8                YES                                          
         XC    LLASTPRO,LLASTPRO   NO-MUST BE AT END-OF-LIST                    
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         CLI   LNLISTS,0           TEST FOR EMPTY SCREEN                        
         BNE   PRO9                NO                                           
*                                                                               
         LA    R2,PROCLIH          YES-PUT CURSOR AT CLIENT                     
         MVC   CONHEAD(L'LIST3MSG),LIST3MSG                                     
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BNE   PRO9                NO                                           
         MVC   CONHEAD(L'LIST4MSG),LIST4MSG                                     
         B     PRO9                                                             
*                                                                               
PRO8     MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
*                                                                               
PRO9     ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
PRO10    BAS   RE,EDT                                                           
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE SCREEN                                               
*                                                                               
BLDTWA   NTR1  ,                                                                
*                                                                               
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
*                                                                               
BLDTWA2  CLI   CHASW,C'Y'          TEST CHANGES PERMITTED ON SCREEN             
         BE    BLDTWA4             YES                                          
         L     R3,ALISTELS                                                      
         SR    R0,R0                                                            
*                                                                               
BLDTWA3  CLI   TWAELCD,0           TEST FOR EOT                                 
         BE    BLDTWA4             YES                                          
         CLI   TWAELCD,X'03'       TEST FOR POSSIBLE UNP DATA FLD               
         BNE   *+8                                                              
         MVI   TWAEATB,X'20'       YES-CHANGE IT TO PROTECTED                   
         IC    R0,TWAELLN                                                       
         AR    R3,R0                                                            
         B     BLDTWA3                                                          
*                                                                               
BLDTWA4  LA    R5,TWAPARM          R5=ATWABLD BLOCK                             
         USING TWAPARMD,R5                                                      
         MVC   TWAPATWA,ATWA                                                    
         MVC   TWAPAFST,ALISTELS                                                
         MVC   TWAPAMAX,=AL4(LSAVES-T60BFFD)                                    
         ST    R2,TWAPAOUT         FIRST OUTPUT POSITION                        
         ZIC   R0,NOLINES                                                       
*                                                                               
BLDTWA6  GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,TWAPANXT                                                      
         ST    R2,TWAPAOUT         SET NEXT OUTPUT POSITION                     
         BCT   R0,BLDTWA6                                                       
*                                                                               
BLDTWA8  MVC   TWAPAFST,APFELS     NOW BUILD PF KEY LINE                        
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTWAX  B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   SCRTYPE,C'R'        DEFAULT SCREEN TYPE=REGULAR                  
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   EXLEN,2                                                          
*                                                                               
VALHED1  LA    R2,PROCLIH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
*                                                                               
         MVI   OPTION,0                                                         
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         ZIC   R1,LCLI                                                          
         LA    R1,2(R1)                                                         
         STC   R1,EXLEN                                                         
*                                                                               
VALHED2  LA    R2,PROSTAH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR START CODE                          
         BE    VALHED4                                                          
         MVI   ERROR,INVALID                                                    
         CLC   5(1,R2),LPRO        TEST LONGER THAN A PRODUCT CODE              
         BH    ERREND                                                           
         GOTO1 ANY                                                              
         MVC   QSTPRO,WORK                                                      
*                                                                               
VALHED4  LA    R2,PROOFFH          OFFICE                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   OPTION,0                                                         
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
*                                                                               
VALHED6  BAS   RE,VALFILS                                                       
*                                                                               
         LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALOPT                                                        
*                                                                               
         LA    R2,PROSTATH         STATUS FIELD                                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALSTA                                                        
*                                                                               
         LA    R2,PRODTYPH         DATE TYPE                                    
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALDTY                                                        
*                                                                               
         LA    R2,PRODATEH         DATE FIELD                                   
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALDTE                                                        
*                                                                               
         OI    PROCLIH+4,X'20'     SET ON PREV VALID BITS                       
         OI    PROSTAH+4,X'20'                                                  
         OI    PROOFFH+4,X'20'                                                  
         OI    PROOPTH+4,X'20'                                                  
         OI    PROSTATH+4,X'20'                                                 
         OI    PRODATEH+4,X'20'                                                 
         OI    PRODTYPH+4,X'20'                                                 
         LA    R2,PROFILTH                                                      
         LA    R0,NFLTS                                                         
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,*-8                                                           
*                                                                               
VALHED8  LA    R0,TYPES                                                         
         LA    RE,TYPETAB                                                       
         CLC   SCRTYPE,0(RE)                                                    
         BE    VALHED10                                                         
         LA    RE,L'TYPETAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                DID NOT RECOGNIZE SCREEN TYPE                
*                                                                               
VALHED10 MVC   SCRTYPE(L'TYPETAB),0(RE) EXTRACT ENTIRE ENTRY                    
         LA    R0,4                COUNTER                                      
         LA    RE,ADIS             RE=A(ADCONS)                                 
         L     R1,RELO                                                          
*                                                                               
VALHED15 L     RF,0(RE)                                                         
         LTR   RF,RF               TEST FOR ZERO ADCON                          
         BZ    *+6                                                              
         AR    RF,R1               RELOCATE                                     
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,VALHED15                                                      
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE FILTER FIELD                                      
*                                                                               
VALFILS  NTR1  ,                                                                
         LA    R2,PROFILTH                                                      
         MVI   ERROR,INVALID                                                    
         LA    R3,QFILTS           R3=A(OUTPUT)                                 
         LA    R5,L'QFILTS         R5=N'OUTPUT POSITIONS                        
*                                                                               
VALFIL2  BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALFIL6                                                          
         LA    R4,8(R2)            R4=A(INPUT)                                  
         ZIC   R6,5(R2)                                                         
*                                                                               
         CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                                                             
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
VALFIL6  LA    R3,1(R3)                                                         
         BAS   RE,BUMPTOUN                                                      
         BCT   R5,VALFIL2                                                       
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE OPTIONS FIELD                                     
*                                                                               
VALOPT   NTR1  ,                                                                
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    ERREND                                                           
         CLI   4(R1),2             TEST MORE THAN 2 ENTRIES                     
         BH    ERREND              YES                                          
         ZIC   R6,4(R1)                                                         
         LA    R4,BLOCK            R4=A(SCANNER BLOCK)                          
*                                                                               
         MVC   OPTNAME1,SPACES     DEFAULT                                      
         MVC   OPTNAME2,SPACES                                                  
*                                                                               
VALOPT10 CLI   0(R4),0             TEST FOR COMMA ONLY                          
         BE    ERREND                                                           
         CLI   1(R4),0             TEST FOR KEYWORD/PARM                        
         BNE   ERREND              YES                                          
         CLC   =CL20'PR',12(R4)                                                 
         BE    VALOPT30            YES                                          
*                                                                               
         CLI   0(R4),3             IS LENGTH > 3                                
         BH    ERREND              ERROR                                        
*                                                                               
         LA    R5,AETAB            LOOK FOR VALID OPTIONS                       
VALOPT20 CLI   0(R5),X'FF'                                                      
         BE    ERREND              DID NOT FIND ANYTHING SO ERROR               
         CLC   0(3,R5),12(R4)                                                   
         BE    VALOPT40                                                         
         LA    R5,L'AETAB(R5)      INCREMENT                                    
         B     VALOPT20                                                         
*                                                                               
VALOPT30 TM    OPTFLAG,AEON                                                     
         BO    ERREND                                                           
         LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         OI    OPTFLAG,PRON                                                     
         MVI   0(RE),C'P'          SET OPTION                                   
         MVI   0(R3),C'U'          SET FOR UNPROTECTED                          
         MVI   SCRTYPE,C'O'        NOTE OPTIONS SCREEN                          
         B     VALOPT50                                                         
*                                                                               
VALOPT40 TM    OPTFLAG,PRON                                                     
         BO    ERREND                                                           
         LA    RE,QOPT1                                                         
         LA    R3,OPTST1                                                        
         OI    OPTFLAG,AEON                                                     
         OI    OPTFLAG,OPT1        TURN ON BIT                                  
*                                                                               
         CLI   QOPT1,0             TEST IF FIRST OPTION                         
         BE    *+16                YES                                          
         LA    RE,QOPT2                                                         
         LA    R3,OPTST2                                                        
         OI    OPTFLAG,OPT2        TURN ON BIT                                  
*                                                                               
         MVI   0(RE),C'P'          SET OPTION                                   
         MVI   0(R3),C'U'          SET FOR UNPROTECTED                          
         LA    R5,3(R5)            INCREMENT TABLE                              
         MVC   1(1,RE),0(R5)       SAVE SEQ                                     
         BAS   RE,GETHEAD          GET HEADERS TO DISPLAY                       
         MVI   SCRTYPE,C'A'        NOTE AE SCREEN                               
*                                                                               
VALOPT50 LA    R4,32(R4)           NEXT SCANNER BLOCK                           
         BCT   R6,VALOPT10                                                      
*                                                                               
         MVC   SVFLAG,OPTFLAG                                                   
*                                                                               
VALOPTX  B     XIT                                                              
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
         BNE   ERREND                                                           
         CLI   QLOCKED,0                                                        
         BNE   ERREND                                                           
         MVC   QLOCKED,12(R4)                                                   
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
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE DATE TYPE FIELD                                       
*                                                                               
VALDTY   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         MVC   QDTYPE,8(R2)                                                     
         CLI   QDTYPE,C'O'                                                      
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
* SUB-ROUTINE TO PROCESS PF KEYS - ROUTINE HANDLES GLOBAL BILLING               
* SELECT INPUT                                                                  
*                                                                               
PROCPF   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PROCPF20            NOTHING ON SCREEN                            
         LA    R5,LSELTAB          R5=A(SELECT FIELD TABLE)                     
         USING SELTABD,R5                                                       
*                                                                               
PROCPF2  CLI   PFKEY,PF1           TEST FOR PF1=CLIENT LIST                     
         BE    *+12                YES                                          
         CLI   PFKEY,PF2           TEST FOR PF2=JOB LIST                        
         BNE   PROCPF3                                                          
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST FOR ANY FIELD MODIFIED                  
         BH    PROCPFX             AFTER ACTION                                 
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,CURDISP          RE=A(CURSOR FIELD)                           
         ST    RE,ACURSOR          SAVE IT                                      
         B     PROCPF4                                                          
*                                                                               
PROCPF3  CLI   PFKEY,PF9           TEST FOR PF9-PF11                            
         BL    PROCPFX                                                          
         CLI   PFKEY,PF11                                                       
         BH    PROCPFX                                                          
         CLI   CHASW,C'Y'          TEST CHANGES ALLOWED                         
         BNE   PROCPFX             NO                                           
*                                                                               
PROCPF4  BAS   RE,SETLIN                                                        
         CLI   PFKEY,PF1           TEST FOR PF1=CLIENT LIST                     
         BE    *+12                                                             
         CLI   PFKEY,PF2           TEST FOR PF2=JOB LIST                        
         BNE   PROCPF6                                                          
         L     R2,ASEL                                                          
         C     R2,ACURSOR          TEST IF CURSOR PRESSED IN SELECT             
         BNE   PROCPF15            NO                                           
*                                                                               
         MVC   CLICODE,SPACES                                                   
         MVC   PRODCODE,SPACES                                                  
         MVI   CALLSP,0            ERASE SAVED STACK                            
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLICODE(0),SELKEY+3                                              
         CLI   PFKEY,PF1           TEST FOR PF1=CLIENT LIST                     
         BE    PROCPF5                                                          
*                                                                               
         LA    RE,SELKEY+4(R1)                                                  
         ZIC   R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRODCODE(0),0(RE)                                                
*                                                                               
         MVI   DUB,RECNJOB                                                      
         MVI   DUB+1,ACTNLIST                                                   
         GOTO1 VCHKACC                                                          
         BE    *+12                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'JOB',=C'LIST',(6,CLICODE),(6,PRODCODE),0         
*                                                                               
PROCPF5  MVI   DUB,RECNCLI                                                      
         MVI   DUB+1,ACTNLIST                                                   
         GOTO1 VCHKACC                                                          
         BE    *+12                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'CLIENT',=C'LIST',(6,CLICODE),0                   
*                                                                               
PROCPF6  L     R2,ABILLS           R2=A(BILLING SELECT FIELD)                   
         CLI   PFKEY,PF10          TEST FOR PF10                                
         BE    PROCPF8             YES                                          
         BH    PROCPF10            ITS PF11                                     
*                                                                               
         CLI   8(R2),C'Y'          TEST FIELD IS ALREADY 'YES'                  
         BE    PROCPF15            YES-LEAVE IT ALONE                           
         MVI   8(R2),C'Y'          FORCE FIELD TO C'Y'                          
         MVI   5(R2),1             FUDGE INPUT LENGTH                           
         OI    4(R2),X'80'         MAKE IT INPUT THIS TIME                      
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID BIT                
         B     PROCPF15                                                         
*                                                                               
PROCPF8  CLI   8(R2),C'N'          TEST ITS ALREADY 'NO'                        
         BE    PROCPF15            YES                                          
         MVI   8(R2),C'N'                                                       
         MVI   5(R2),1                                                          
         OI    4(R2),X'80'         MAKE IT INPUT THIS TIME                      
         NI    4(R2),X'FF'-X'20'                                                
         B     PROCPF15                                                         
*                                                                               
PROCPF10 CLI   8(R2),C' '                                                       
         BNH   PROCPF15            FIELD HAS NO DATA                            
         MVI   8(R2),C' '                                                       
         MVI   5(R2),0                                                          
         NI    4(R2),X'FF'-X'20'                                                
         OI    4(R2),X'80'                                                      
*                                                                               
PROCPF15 L     R2,ANEXTSEL         POINT TO NEXT SELECT FIELD                   
         LA    R5,SELTABL(R5)      NEXT SELECT FIELD ENTRY                      
         BCT   R3,PROCPF4                                                       
*                                                                               
PROCPF20 CLI   PFKEY,PF1           TEST FOR PF1=CLIENT LIST                     
         BNE   PROCPFX                                                          
*                                                                               
         L     RE,ATWA             CURSOR WAS NOT IN SELECT FIELD               
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST ANYTHING MODIFIED AFTER ACTION          
         BH    PROCPFX             YES                                          
*                                                                               
         L     R2,AFSTSEL          POSITION FOR POSSIBLE ERROR                  
         MVI   DUB,RECNCLI                                                      
         MVI   DUB+1,ACTNLIST                                                   
         GOTO1 VCHKACC                                                          
         BE    *+12                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         GOTO1 VTRANSF,WORK,=C'CLIENT',=C'LIST',(L'QCLI,QCLI),0                 
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1                                                                   
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
* SUB-ROUTINE TO LIST PRODUCT RECORDS                                           
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LLASTPRO CONTAINS LAST KEY READ OR BINARY ZERO                  
*                                                                               
LIST     NTR1                                                                   
         L     R2,AFSTSEL                                                       
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         SR    R0,R0                                                            
         ICM   R0,1,LNLISTS                                                     
         BZ    LIST2               NOTHING ON SCREEN NOW                        
         ZIC   R1,NFIELDS                                                       
         MR    R0,R0                                                            
         BAS   RE,BUMP             ADVANCE TO FIRST SELECT FIELD                
         BCT   R0,*-4                                                           
*                                                                               
LIST2    ST    R2,ATHISLIN         INITIALIZE LIST LINE POINTER                 
         OC    LLASTPRO,LLASTPRO   TEST RESUMING A READ                         
         BNZ   LIST6                                                            
*                                                                               
         MVC   ACKEYACC(3),CUL                                                  
         LA    RE,QCLI                                                          
         OC    QCLI,QCLI           TEST SPECIFIC CLIENT REQUESTED               
         BNZ   *+8                 YES                                          
         LA    RE,=CL6'A'          START AT BEGINNING                           
         MVC   ACKEYACC+3(6),0(RE)                                              
         GOTO1 HIGH                                                             
         CLC   ACKEYACC(3),CUL     TEST A CLIENT FOUND                          
         BNE   LISTX                                                            
         GOTO1 SETCLI                                                           
*                                                                               
LIST4    LA    RE,QSTPRO                                                        
         OC    QSTPRO,QSTPRO       TEST START CODE GIVEN                        
         BNZ   *+8                                                              
         LA    RE,=CL6'A'                                                       
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(6,R1),0(RE)       SET START CODE INTO KEY                      
         B     LIST10                                                           
*                                                                               
* CONTINUING LIST                                                               
*                                                                               
LIST6    OC    QCLI,QCLI           TEST SPECIFIC CLIENT REQUESTED               
         BNZ   LIST8                                                            
*                                                                               
         MVC   ACKEYACC(3),CUL     RE-READ LAST PRODUCT'S CLIENT                
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYACC+3(0),LLASTPRO+3                                         
         GOTO1 READ                                                             
         GOTO1 SETCLI                                                           
*                                                                               
LIST8    MVC   KEY,SPACES                                                       
         MVC   ACKEYACC,LLASTPRO                                                
         ZIC   R1,LCLIPRO                                                       
         LA    R1,ACKEYACC+3(R1)                                                
         MVI   0(R1),X'FF'         BUMP AHEAD TO NEXT PRODUCT                   
         B     LIST10                                                           
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST10   OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
         B     LIST12                                                           
*                                                                               
LIST11   OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
         GOTO1 CATCHIOS                                                         
*                                                                               
LIST12   MVC   SVKEY,KEY                                                        
         TM    ACSTATUS,X'80'      TEST FOR DELETED RECORD                      
         BO    LIST60              SKIP TO NEXT ACCOUNT                         
         ZIC   R1,EXLEN                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACKEYD(0),KEYSAVE   TEST SAME CUL                                
         BNE   LISTX               ALL DONE                                     
*                                                                               
LIST14   ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         CLI   0(R1),C' '          TEST FOR CLIENT RECORD                       
         BH    LIST15              NO                                           
         GOTO1 SETCLI                                                           
         B     LIST11              READ NEXT RECORD                             
*                                                                               
LIST15   GOTO1 SETPROD                                                          
         OC    QOFF,QOFF           TEST FOR OFFICE FILTER                       
         BZ    *+14                                                             
         CLC   QOFF,EFFOFFC        MATCH ON OFFICE FILTER                       
         BNE   LIST60              NO                                           
*                                                                               
LIST17   OC    QFILTS,QFILTS       TEST FOR FILTERS                             
         BZ    LIST20              NONE                                         
*                                                                               
         MVC   BYTE,QFILT1                                                      
         GOTO1 APPFIL,EFF1                                                      
         BNE   LIST60              REJECT PRODUCT                               
*                                                                               
         MVC   BYTE,QFILT2                                                      
         GOTO1 APPFIL,EFF2                                                      
         BNE   LIST60              REJECT PRODUCT                               
*                                                                               
         MVC   BYTE,QFILT3                                                      
         GOTO1 APPFIL,EFF3                                                      
         BNE   LIST60              REJECT PRODUCT                               
*                                                                               
         MVC   BYTE,QFILT4                                                      
         GOTO1 APPFIL,EFF4                                                      
         BNE   LIST60              REJECT PRODUCT                               
*                                                                               
         MVC   BYTE,QFILT5                                                      
         GOTO1 APPFIL,EFF5                                                      
         BNE   LIST60              REJECT PRODUCT                               
*                                                                               
         USING RSTELD,R6                                                        
LIST20   XC    ODATE,ODATE         CLEAR OPEN DATE                              
         MVI   ELCODE,RSTELQ       GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ODATE,RSTTDATE      SAVE OPEN DATE IN CASE NEEDED                
*                                                                               
         CLI   QCLOSED,0           TEST FOR CLOSED FILTERS                      
         BE    LIST22              NO                                           
*                                                                               
         TM    RSTSTAT,RSTSACIC    TEST FOR CLOSED JOB                          
         BO    LIST21              YES                                          
         CLI   QCLOSED,C'C'        TEST FOR CLOSED JOBS ONLY                    
         BE    LIST60              YES-SKIP THIS JOB                            
         B     LIST22                                                           
*                                                                               
LIST21   CLI   QCLOSED,C'O'        TEST FOR OPEN JOBS ONLY                      
         BE    LIST60              YES-SKIP A CLOSED JOB                        
*                                                                               
LIST22   CLI   QLOCKED,0           TEST FOR LOCKED FILTERS                      
         BE    LIST30              NO                                           
         TM    RSTSTAT,RSTSACIL    TEST FOR LOCKED JOB                          
         BO    LIST23              YES                                          
         CLI   QLOCKED,C'L'        TEST FOR LOCKED JOBS ONLY                    
         BE    LIST60              YES-SKIP IT                                  
         B     LIST30              NO-TAKE IT                                   
*                                                                               
LIST23   CLI   QLOCKED,C'U'        TEST FOR UNLOCKED ONLY                       
         BE    LIST60                                                           
*                                                                               
         USING PACELD,R6                                                        
LIST30   XC    CDATE,CDATE         CLEAR CHANGE DATE                            
         MVI   ELCODE,PACELQ       GET LAST ACTIVITY DATE                       
         BAS   RE,GETELIO                                                       
         BNE   LIST32                                                           
         MVC   CDATE,PACDATE       SAVE THE LATEST DATE                         
         GOTO1 DATCON,DMCB,(1,PACDATE),(2,WORK)                                 
         CLI   PACLN,PACLNQ2                                                    
         BL    LIST32                                                           
         GOTO1 DATCON,DMCB,(1,PACDATE2),(2,WORK+2)                              
         CLC   WORK(2),WORK+2                                                   
         BH    LIST32                                                           
         MVC   CDATE,PACDATE2                                                   
*                                                                               
LIST32   OC    SDATE,SDATE         ANY OPEN DATES?                              
         BNZ   *+10                                                             
         OC    EDATE,EDATE                                                      
         BZ    LIST50                                                           
*                                                                               
         CLI   QDTYPE,C'C'         LOOKING FOR CHANGED DATE ONLY?               
         BE    LIST40              YES                                          
         CLC   ODATE,SDATE         SEE IF RECORD IN RANGE                       
         BL    LIST35              NO                                           
         CLC   ODATE,EDATE                                                      
         BH    LIST35                                                           
         B     LIST50              TAKE IT IF OPEN OR BOTH                      
*                                                                               
LIST35   CLI   QDTYPE,C'O'         LOOKING FOR OPEN ONLY?                       
         BE    LIST60              YES, THEN SKIP THESE                         
*                                                                               
LIST40   OC    CDATE,CDATE         ANY 1ST DATE?                                
         BZ    LIST60              NO, SKIP IT                                  
         CLC   CDATE,SDATE         YES, CHECK IF WITHIN RANGE                   
         BL    LIST60              NO, SKIP IT                                  
         CLC   CDATE,EDATE                                                      
         BH    LIST60                                                           
LIST50   TM    COMPSTA9,CPYLACCP   VERIFY LIMITED ACCESS?                       
         BNO   LIST55              NO                                           
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,EFFOFFC                                                 
         MVC   OFFAOPOS,LEDGTOFF   LEDGER OFFICE POSITION                       
         MVC   OFFAREC,AIO                                                      
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BNE   LIST60              SKIP THIS PRODUCT                            
*                                                                               
*                                                                               
* DISPLAY NEW LIST LINE ON SCREEN                                               
*                                                                               
LIST55   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     RF,ADIS             RF=A(DISPLAY ROUTINE)                        
         BASR  RE,RF                                                            
         BNE   LIST60              JOB IS REJECTED                              
*                                                                               
         MVC   LLASTPRO,ACKEYACC   SAVE LAST ACCOUNT ON SCREEN                  
         MVC   ATHISLIN,ANEXTSEL   NEXT LINE                                    
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)                                                   
         USING SELTABD,R1                                                       
         MVC   SELKEY,ACKEYACC     SAVE PRODUCT KEY                             
         MVI   SELACT,C' '                                                      
*                                                                               
         CLC   LNLISTS,NOLINES     TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LIST60   ZIC   R1,LCLIPRO          NEXT PRODUCT                                 
         LA    R1,ACKEYACC+3(R1)                                                
         MVI   0(R1),X'FF'                                                      
         B     LIST10                                                           
*                                                                               
LISTX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO APPLY THE INPUT FILTERS AGAINST THE PRODUCT'S FILTERS          
* AT ENTRY, BYTE=INPUT FILTER AND R1=A(PRODUCT FILTER VALUE)                    
* ON EXIT, CC=EQ IF OK, CC=NEQ TO REJECT                                        
*                                                                               
APPFIL   CLI   BYTE,C' '           TEST FOR BLANK=TAKE EVERYTHING               
         BE    APPFILY                                                          
         CLI   BYTE,C'.'           TEST FOR NO FILTER ON PRODUCT                
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
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR A PRODUCT                           
*                                                                               
DISPRO   NTR1                                                                   
*                                                                               
         MVC   PROTIT(L'REGTIT),REGTIT                                          
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DISPRO2  L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LSTPROTD,R3                                                      
         MVC   LSTCLI,CLICODE                                                   
         MVC   LSTPRO,PRODCODE                                                  
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   LSTNAME,WORK                                                     
         MVC   LSTOFF,EFFOFFC                                                   
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISPRO4  L     R2,ABILLS                                                        
         OI    4(R2),X'20'                                                      
         MVI   ELCODE,ACSTELQ      GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R6                                                       
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    DISPRO6                                                          
         TM    ACSTSTA2,X'C0'      TEST BILLABLE OPTIONS ACTIVE                 
         BZ    DISPRO6                                                          
         MVI   8(R2),C'Y'                                                       
         TM    ACSTSTA2,X'80'                                                   
         BO    *+8                                                              
         MVI   8(R2),C'N'                                                       
*                                                                               
DISPRO6  L     R2,AFILTS                                                        
         LA    R1,EFLTS                                                         
         LA    R0,NFLTS                                                         
*                                                                               
DISPRO7  MVC   8(1,R2),0(R1)                                                    
         OI    4(R2),X'20'                                                      
         BAS   RE,BUMP                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,DISPRO7                                                       
*                                                                               
DISPRO8  L     R2,AOPEN                                                         
         GOTO1 DATCON,DMCB,(1,ODATE),(8,8(R2))                                  
         OI    4(R2),X'20'                                                      
*                                                                               
DISPRO10 L     R2,ALAST                                                         
         GOTO1 DATCON,DMCB,(1,CDATE),(8,8(R2))                                  
         OI    4(R2),X'20'                                                      
*                                                                               
DISPROX  CR    RB,RB               SET CC=EQ ON EXIT                            
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OPTION LIST LINE ON SCREEN                             
*                                                                               
* ON EXIT, CC=EQ IF JOB SHOULD BE DISPLAYED, CC=NEQ TO SKIP JOB                 
*                                                                               
DISOPT   NTR1  ,                                                                
*                                                                               
         MVC   PROTIT(L'OPTTIT),OPTTIT                                          
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING OPTPROTD,R3                                                      
         MVC   OPTCLI,CLICODE                                                   
         MVC   OPTPRO,PRODCODE                                                  
         MVC   OPTOFF,EFFOFFC                                                   
*                                                                               
         MVI   ELCODE,ACPRELQ      GET PROFILE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPROFD,R6                                                       
         MVC   OPTRECV,ACPRRECV+3  RECEIVABLES                                  
         MVC   OPTCOST,ACPRCOST+3  COST                                         
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'3D'        GET SALES ANALYSIS ELEMENT                   
         BAS   RE,GETELIO                                                       
         BNE   DISOPT10                                                         
         USING ACSAND,R6                                                        
         MVC   OPTSALES,ACSACODE+3                                              
DISOPT10 BAS   RE,MOVEFLD                                                       
*                                                                               
DISOPTX  CR    RB,RB                                                            
         DROP  R3,R4,R6                                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OPTION LIST W/ ACCOUNT EQUIVALENCYS                    
*                                                                               
* ON EXIT, CC=EQ IF JOB SHOULD BE DISPLAYED, CC=NEQ TO SKIP JOB                 
*                                                                               
DISAE    NTR1  ,                                                                
*                                                                               
         MVC   PROTIT(L'AETIT),AETIT                                            
         MVC   PROTIT+16(L'OPTNAME1),OPTNAME1                                   
         MVI   PROTIT+16+L'OPTNAME1,X'5F'                                       
         MVC   PROTIT+39(L'OPTNAME2),OPTNAME2                                   
         MVI   PROTIT+39+L'OPTNAME2,X'5F'                                       
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING AEPROTD,R3                                                       
         MVC   AECLI,CLICODE                                                    
         MVC   AEPROD,PRODCODE                                                  
*                                                                               
         MVC   OPTFLAG,SVFLAG                                                   
         NI    OPTFLAG,X'FF'-AEON  TURN OFF                                     
DISAE10  TM    OPTFLAG,OPT1        CHECK FLAG                                   
         BZ    DISAE20                                                          
         MVC   SVSEQ,AESEQ1                                                     
         LA    R1,AE1                                                           
         ST    R1,SVAPRO                                                        
         NI    OPTFLAG,X'FF'-OPT1  TURN OF BIT                                  
         B     DISAE30                                                          
*                                                                               
DISAE20  TM    OPTFLAG,OPT2        CHECK FLAG                                   
         BZ    DISAE50                                                          
         MVC   SVSEQ,AESEQ2                                                     
         LA    R1,AE2                                                           
         ST    R1,SVAPRO                                                        
         NI    OPTFLAG,X'FF'-OPT2  TURN OF BIT                                  
*                                                                               
DISAE30  BAS   RE,GETCLI           GET CLIENT LEVEL                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,FFTELQ       READ THE DATA                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISAE40  BAS   RE,NEXTEL                                                        
         BNE   DISAE50                                                          
*                                                                               
         USING FFTELD,R6                                                        
         CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BNE   DISAE40                                                          
*                                                                               
         CLC   FFTSEQ,SVSEQ        SAME SEQUENCE #?                             
         BNE   DISAE40             NO                                           
*                                                                               
         L     R5,SVAPRO                                                        
         MVC   0(L'AE1,R5),SPACES  CLEAR ANYTHING THERE                         
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),FFTDATA     MOVE IN DATA                                 
*                                                                               
DISAE50  CLI   OPTFLAG,0           COMPARE TO 0, EVERYTHING DONE?               
         BNE   DISAE10             NO                                           
*                                                                               
         BAS   RE,MOVEFLD                                                       
         MVC   OPTFLAG,SVFLAG      RESTORE                                      
*                                                                               
DISAEX   CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
* SUB-ROUTINE TO GET HEADERS FOR LIST DISPLAY                                   
* R3 IS PASSED IN AS PARAMETER                                                  
* R5 IS SEQUENCE #                                                              
*                                                                               
GETHEAD  NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES          CLEAR KEY                                    
         MVC   KEY(3),CUL          COMPANY,UNIT,LEDGER                          
         GOTO1 READ                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,APRELQ       ANY RULES RECORDS?                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETHD10  BAS   RE,NEXTEL                                                        
         BNE   GETHEADX                                                         
*                                                                               
         USING APRELD,R6                                                        
         CLC   APRSEQ,0(R5)        SAME SEQUENCE #?                             
         BNE   GETHD10             NO                                           
*                                                                               
         MVC   1(L'APRDESC,R3),APRDESC                                          
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
GETHEADX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET CLIENT LEVEL ACCOUNT EQUIVALENCYS                          
*                                                                               
GETCLI   NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES          CLEAR KEY                                    
         MVC   KEY(3),CUL          COMPANY,UNIT,LEDGER                          
         MVC   KEY+3(L'CLICODE),CLICODE                                         
         GOTO1 READ                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,FFTELQ       READ THE DATA                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETCLI10 BAS   RE,NEXTEL                                                        
         BNE   GETCLI20                                                         
*                                                                               
         USING FFTELD,R6                                                        
         CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BNE   GETCLI10                                                         
*                                                                               
         CLC   FFTSEQ,SVSEQ        SAME SEQUENCE #?                             
         BNE   GETCLI10            NO                                           
*                                                                               
         L     R5,SVAPRO                                                        
         MVI   0(R5),C'('                                                       
         LA    R5,1(R5)                                                         
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),FFTDATA     MOVE IN DATA                                 
         AHI   R1,1                                                             
         AR    R5,R1                                                            
         MVI   0(R5),C')'                                                       
*                                                                               
GETCLI20 MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   KEY,SVKEY           RESTORE SEQUENCE                             
*                                                                               
GETCLIX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
* NOTE-GENCON DISABLES READ FOR UPDATE FOR ACTION LIST (ACTNUM=ACTLIST)         
*                                                                               
EDT      NTR1                                                                   
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT4                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT4                YES                                          
*                                                                               
         MVI   DUB+1,ACTNDIS                                                    
         CLI   8(R2),C'S'          TEST 'S'=DISPLAY                             
         BE    EDT3                                                             
         MVI   DUB+1,ACTNCHA                                                    
         CLI   8(R2),C'C'          TEST 'C'=CHANGE                              
         BNE   ERREND                                                           
*                                                                               
EDT3     MVI   DUB,RECNPRO         TEST FOR ACCESS                              
         GOTO1 VCHKACC                                                          
         BE    EDT4                                                             
         MVI   ERROR,ACCSERR                                                    
         B     ERREND                                                           
*                                                                               
EDT4     LA    R4,KEY              READ THE PRODUCT RECORD                      
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC,SELKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           EXTRACT RETURNED KEY                         
         CLC   ACKEYD(ACLENGTH-ACKEYD),KEYSAVE                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         GOTO1 SETPROD                                                          
         CLI   CHASW,C'Y'          TEST FOR CHANGES ON SCREEN                   
         BNE   *+10                NO-SKIP EDITING DATA FIELDS                  
         L     RF,AEDT                                                          
         BASR  RE,RF                                                            
*                                                                               
EDT6     CLI   5(R2),0             TEST FOR SELECT INPUT                        
         BE    EDT10               NO                                           
         CLI   8(R2),C'*'          TEST ALREADY THERE                           
         BE    EDT10                                                            
         MVC   SELACT,8(R2)        SAVE ACTION                                  
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT FIELD BACK                              
*                                                                               
         MVC   CLICODE,SPACES      GET THE CLIENT CODE                          
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLICODE(0),ACKEYACC+3                                            
*                                                                               
         CLI   SELACT,C'C'         TEST FOR 'C'=CHANGE                          
         BE    EDT8                                                             
         TM    OPTFLAG,AEON                                                     
         BO    EDT7                                                             
*                                                                               
         GOTO1 VCALL,WORK,=C'PRODUCT',=C'DISPLAY',(6,CLICODE),         X        
               (6,PRODCODE),0                                                   
EDT7     GOTO1 VCALL,WORK,=C'PRO2',=C'DISPLAY',(6,CLICODE),            X        
               (6,PRODCODE),0                                                   
*                                                                               
EDT8     TM    OPTFLAG,AEON                                                     
         BO    EDT9                                                             
         GOTO1 VCALL,WORK,=C'PRODUCT',=C'CHANGE',(6,CLICODE),          X        
               (6,PRODCODE),0                                                   
EDT9     GOTO1 VCALL,WORK,=C'PRO2',=C'CHANGE',(6,CLICODE),             X        
               (6,PRODCODE),0                                                   
*                                                                               
EDT10    L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A LIST LINE                                               
*                                                                               
EDTPRO   NTR1                                                                   
         MVI   UPDATE,C'N'         SET UPDATE SWITCH TO NO                      
         L     R2,ABILLS           R2=A(BILLING SELECT FIELD)                   
         TM    4(R2),X'20'         TEST IF ANY CHANGE                           
         BO    EDTPRO10            NO                                           
*                                                                               
EDTPRO2  MVI   UPDATE,C'Y'                                                      
         MVI   ELCODE,ACSTELQ                                                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING ACSTATD,R6                                                       
         CLC   ELCODE,ELEM         TEST IF ELEM FOUND                           
         BE    *+6                                                              
         DC    H'0'                NO-BIG TROUBLE                               
         MVI   ACSTLEN,ACSTLNQ2    USE LARGER LENGTH                            
         NI    ACSTSTA2,X'FF'-X'C0'                                             
         CLI   5(R2),0                                                          
         BE    EDTPRO5             ERASED SELECT FIELD                          
*                                                                               
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   WORK,C'Y'                                                        
         BNE   *+12                                                             
         OI    ACSTSTA2,X'80'                                                   
         B     EDTPRO5                                                          
*                                                                               
         CLI   WORK,C'N'                                                        
         BNE   ERREND                                                           
         OI    ACSTSTA2,X'40'                                                   
*                                                                               
EDTPRO5  GOTO1 ADDELEM                                                          
*                                                                               
EDTPRO10 L     R2,AFILTS           R2=A(FILTER FIELD HEADER)                    
         LA    R0,NFLTS                                                         
EDTPRO15 TM    4(R2),X'20'         PREV VALIDATED?                              
         BNO   EDTPRO20            NO, UPDATE ELEMENT                           
         BAS   RE,BUMP                                                          
         BCT   R0,EDTPRO15                                                      
         B     EDTPRO30            NO FILTER VALUSE CHANGED                     
*                                                                               
EDTPRO20 L     R2,AFILTS                                                        
         MVI   ELCODE,X'30'        GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         USING RSTELD,R6                                                        
         CLI   RSTLN,RSTLN3Q       IS THIS BIG ENOUGH FOR FILTER VALS           
         BNL   EDTPRO25            YES                                          
*                                                                               
         MVI   ELCODE,RSTELQ       EXTRACT RSTEL TO "ELEMENT"                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT          UPDATE LENGTH                                
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 ADDELEM                                                          
         BAS   RE,GETELIO          READDRESS STATUS ELEMENT                     
         BE    EDTPRO25                                                         
         DC    H'0'                ??!!!                                        
*                                                                               
EDTPRO25 LA    R1,X'20'            UPDATE NON VALIDATED FIELDS ONLY             
         SLL   R1,24               X'20 TO THE HOB OF R1                        
         GOTO1 VALFLTS                                                          
         MVI   UPDATE,C'Y'                                                      
*                                                                               
EDTPRO30 CLI   UPDATE,C'Y'                                                      
         BNE   EDTPROX                                                          
*                                                                               
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         L     R2,ABILLS                                                        
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AFILTS                                                        
         LA    R0,NFLTS                                                         
EDTPRO50 OI    4(R2),X'20'         MARK FILTER AS VALIDATED                     
         OI    6(R2),X'80'         XMIT BACK TO USER                            
         BAS   RE,BUMP                                                          
         BCT   R0,EDTPRO50                                                      
*                                                                               
*                                                                               
EDTPROX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT AN OPTIONS LIST LINE                                      
*                                                                               
EDTOPT   NTR1                                                                   
*                                                                               
EDTOPTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINES TO SET SCREEN ADDRESSES                                          
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PROTAGH                                                       
         ST    R2,AFSTSEL                                                       
         ZIC   R0,NOLINES                                                       
         ZIC   R1,NFIELDS                                                       
         MR    R0,R0               COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
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
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LIST2MSG DC    C'LIST DISPLAYED'                                                
LIST3MSG DC    C'THERE ARE NO MORE RECORDS TO DISPLAY'                          
LIST4MSG DC    C'THERE ARE NO RECORDS TO DISPLAY     '                          
EDTMSG   DC    C'CHANGES COMPLETED'                                             
         SPACE 2                                                                
RELO     DC    A(0)                                                             
         SPACE 2                                                                
*VALID OPTIONS TABLE                                                            
*                                                                               
AETAB    DS    0XL4                                                             
         DC    CL3'AEA',X'00'                                                   
         DC    CL3'AEB',X'01'                                                   
         DC    CL3'AEC',X'02'                                                   
         DC    CL3'AED',X'03'                                                   
         DC    CL3'AEE',X'04'                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
* SCREEN TYPE TABLE                                                             
*                                                                               
         DS    0F                                                               
TYPETAB  DS    0CL20                                                            
         DC    C'R',AL1(15),AL1(REGFLDS),X'00'                                  
         DC    A(DISPRO),A(EDTPRO),A(REGELS),A(PFELS)                           
*                                                                               
         DC    C'O',AL1(15),AL1(OPTFLDS),X'00'                                  
         DC    A(DISOPT),A(EDTOPT),A(OPTELS),A(PFELS)                           
*                                                                               
         DC    C'A',AL1(15),AL1(OPTFLDS),X'00'                                  
         DC    A(DISAE),A(EDTOPT),A(OPTELS),A(PFELS)                            
*                                                                               
TYPES    EQU   (*-TYPETAB)/L'TYPETAB                                            
         SPACE 2                                                                
REGTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   REGTIT                                                           
         DC    CL4'SEL'                                                         
         DC    CL8'CLT/PRD'                                                     
         DC    CL22'PRODUCT NAME'                                               
         DC    CL4'OF^'                                                         
         DC    CL5'BIL^'                                                        
         DC    CL11'1 2 3 4 5^'                                                 
         DC    CL9' OPENED ^'                                                   
         DC    CL9'CHANGED ^'                                                   
         ORG                                                                    
         SPACE 1                                                                
AETIT    DC    CL(L'PROTIT)' '                                                  
         ORG   AETIT                                                            
         DC    CL8'SEL'                                                         
         DC    CL10'CLT/PRD'                                                    
         ORG                                                                    
         SPACE 1                                                                
OPTTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   OPTTIT                                                           
         DC    CL8'SEL'                                                         
         DC    CL10'CLT/PRD'                                                    
         DC    CL7'OFF'                                                         
         DC    CL13'RECEIVABLE'                                                 
         DC    CL13'COSTING'                                                    
         DC    CL5'SALES'                                                       
         ORG                                                                    
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
* ELEMENT TABLES FOR SCREEN GENERATION                                          
*                                                                               
REGELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(32),X'20',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(41),AL1(01),X'08',AL1(0)          
*        FILTER VALUES                                                          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(45),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(47),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(49),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(51),AL1(01),X'08',AL1(0)          
         DC    X'03',AL1(TWAELLNQ),AL1(0),AL1(53),AL1(01),X'08',AL1(0)          
*        DATES                                                                  
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(56),AL1(08),X'20',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(65),AL1(08),X'20',AL1(0)          
         DC    X'00'                                                            
REGELSLN EQU   *-REGELS                                                         
         SPACE 2                                                                
OPTELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(32)         
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(10),AL1(69),X'20',AL1(0)          
         DC    X'00'                                                            
OPTELSLN EQU   *-OPTELS                                                         
*                                                                               
PFELS    DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=CLIENT LIST,PF2=JOB LIST,S=SEL, C=CHA,GLOBAL SEX        
               L-PF9=Y,PF10=N,PF11=ERASE'                                       
         DC    X'00'                                                            
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
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
EXLEN    DS    X                                                                
*                                                                               
SVKEY    DS    CL48                SAVED KEY                                    
SVAPRO   DS    A                   LOCATION IN PROTECTED FIELD                  
SVSEQ    DS    X                   SAVE SEQUENCE #                              
SVFLAG   DS    X                   SAVE FLAG STATE                              
OPTFLAG  DS    X                   OPTION FLAG                                  
OPT1     EQU   X'01'               OPTION 1 REQUESTED                           
OPT2     EQU   X'02'               OPTION 2 REQUESTED                           
PRON     EQU   X'80'               PR REQUESTED                                 
AEON     EQU   X'40'               ACCOUNT EQUIVALENCY REQUESTED                
*                                                                               
QCONTROL DS    0C                                                               
QCLI     DS    CL(L'CLICODE)                                                    
QSTPRO   DS    CL(L'PRODCODE)                                                   
QOFF     DS    CL2                 OFFICE CODE                                  
QFILTS   DS    0CL5                                                             
QFILT1   DS    C                                                                
QFILT2   DS    C                                                                
QFILT3   DS    C                                                                
QFILT4   DS    C                                                                
QFILT5   DS    C                                                                
QDTYPE   DS    C                                                                
QCLOSED  DS    C                   C=CLOSED,O=OPEN                              
QLOCKED  DS    C                   L=LOCKED,U=UNLOCKED                          
QOPT1    DS    X                   OPTION 1                                     
AESEQ1   DS    X                   ACCOUNT EQUIV. - OPTION 1                    
QOPT2    DS    X                   OPTION 2                                     
AESEQ2   DS    X                   ACCOUNT EQUIV. - OPTION 2                    
QCONTRLN EQU   *-QCONTROL                                                       
*                                                                               
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
SCRTYPE  DS    C                   SCREEN TYPE (R,O)                            
NOLINES  DS    X                   N'LIST SCREEN LINES                          
NFIELDS  DS    X                   N'FIELDS PER LIST SCREEN LINE                
         DS    X                                                                
ADIS     DS    A                                                                
AEDT     DS    A                                                                
ALISTELS DS    A                                                                
APFELS   DS    A                                                                
*                                                                               
OPTST1   DS    C                   FIELD STATUS-OPTION 1                        
OPTNAME1 DS    CL22                FIRST OPTION NAME                            
OPTST2   DS    C                   FIELD STATUS-OPTION 2                        
OPTNAME2 DS    CL22                SECOND OPTION NAME                           
*                                                                               
AFSTSEL  DS    A                                                                
APFFLD   DS    A                                                                
AENDSCR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ADATA    DS    A                                                                
*                                                                               
ABILLS   DS    A                                                                
AFILTS   DS    0A                                                               
AF1      DS    A                                                                
AF2      DS    A                                                                
AF3      DS    A                                                                
AF4      DS    A                                                                
AF5      DS    A                                                                
AOPEN    DS    A                                                                
ALAST    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
TWAPARM  DS    CL(TWAPARML)                                                     
*                                                                               
CHASW    DS    C                                                                
UPDATE   DS    C                                                                
*                                                                               
SDATE    DS    XL3                 START DATE                                   
EDATE    DS    XL3                 END DATE                                     
ODATE    DS    XL3                 DATE OPENED                                  
CDATE    DS    XL3                 LAST CHANGE DATE                             
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROE2D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+X'E00'-SAVAREAL-LSAVESLN                                 
LSAVES   DS    0X                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LLASTPRO DS    CL(L'ACKEYACC)      LAST PRODUCT ON SCREEN                       
LSELTAB  DS    CL(NLINES*SELTABL)                                               
LSAVESLN EQU   *-LSAVES                                                         
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NLINES   EQU   15                  N'LIST SCREEN LINES                          
REGFLDS  EQU   10                  N'FIELDS ON REGULAR LIST SCREEN              
OPTFLDS  EQU   2                   N'FIELDS ON OPTION LIST SCREEN               
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER LIST LINE PROTECTED FIELD                                      
*                                                                               
LSTPROTD DSECT                                                                  
LSTCLI   DS    CL3                                                              
         DS    CL1                                                              
LSTPRO   DS    CL3                                                              
         DS    CL1                                                              
LSTNAME  DS    CL20                                                             
         DS    CL2                                                              
LSTOFF   DS    CL2                                                              
         SPACE 2                                                                
* DSECT TO COVER OPTION ACCOUNT EQUIV. PROTECTED FIELD                          
*                                                                               
AEPROTD  DSECT                                                                  
AECLI    DS    CL3                                                              
         DS    CL1                                                              
AEPROD   DS    CL3                                                              
         DS    CL1                                                              
AE1      DS    CL22                                                             
         DS    CL1                                                              
AE2      DS    CL22                                                             
         DS    CL1                                                              
AE3      DS    CL22                                                             
         SPACE 2                                                                
* DSECT TO COVER OPTION LIST LINE PROTECTED FIELD                               
*                                                                               
OPTPROTD DSECT                                                                  
OPTCLI   DS    CL3                                                              
         DS    CL1                                                              
OPTPRO   DS    CL3                                                              
         DS    CL3                                                              
OPTOFF   DS    CL2                                                              
         DS    CL5                                                              
OPTRECV  DS    CL12                                                             
         DS    CL1                                                              
OPTCOST  DS    CL12                                                             
         DS    CL1                                                              
OPTSALES DS    CL12                                                             
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                   SELECT ACTION                                
SELKEY   DS    CL(L'ACKEYACC)                                                   
SELTABL  EQU   *-SELTABD                                                        
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACPRO12   02/14/12'                                      
         END                                                                    
