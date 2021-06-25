*          DATA SET ACPRO32    AT LEVEL 029 AS OF 08/23/07                      
*PHASE T60B32A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B32 - JOB ESTIMATE - CONTROLLER'                             
T60B32   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B32**,R7,RR=R2                                              
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
         BE    EST2                                                             
         CLI   MODE,VALREC                                                      
         BE    EST4                                                             
         B     ESTX                                                             
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
EST2     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ST    RB,ABASE1           SAVE MAIN CSECTS' BASE REGISTERS             
         ST    R7,ABASE2                                                        
         LA    RE,VALEST                                                        
         ST    RE,AVALEST                                                       
         LA    RE,GETEST                                                        
         ST    RE,AGETEST                                                       
         LA    RE,BLDLST                                                        
         ST    RE,ABLDLST                                                       
         L     RE,=A(CALLSES)                                                   
         A     RE,RELO                                                          
         ST    RE,ACALLSES                                                      
*                                                                               
         L     RF,ATIA                                                          
         ST    RF,AOPVTAB          FIRST PART FOR OPERANDS                      
         MVC   LCOLTAB,=A(8*1024)  USE 8K FOR COLUMNS                           
         L     RE,=A(LENTIAA)      REAL TIA LENGTH                              
         S     RE,LCOLTAB          TAKE OFF 8K                                  
         ST    RE,LOPVTAB          REMAINDER FOR OPERANDS                       
         LA    RF,0(RF,RE)                                                      
         ST    RF,ACOLTAB          SECOND PART FOR COLUMNS                      
*                                                                               
         MVC   ORIGEST,=AL2(JBDORG)                                             
         MVC   CURREST,=AL2(JBDCUR)                                             
         MVC   ACTUALS,=AL2(JBDACT)                                             
         MVC   REGEST,=AL2(JBDEST)                                              
         MVC   PREVEST,=AL2(JBDPRV)                                             
*                                                                               
EST3     CLI   RACHANGE,C'Y'       WAS RECORD/ACTION CHANGED ?                  
         BNE   EST3A               NO                                           
         NI    PROCLIH+4,X'FF'-X'20'  YES, TURN OFF VALIDATE BIT                
         NI    PROPROH+4,X'FF'-X'20'                                            
         NI    PROJOBH+4,X'FF'-X'20'                                            
*                                                                               
EST3A    BAS   RE,VALHED                                                        
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RETURNED,0          TEST IF RETURN                               
         BNE   ESTX                YES-SKIP RECORD/ACTION KEY TESTS             
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST                                                  
         CLI   INTMODE,FSTLIST     TEST FIRST TIME SWITCH SET                   
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES-BUILD A SCREEN                           
         CLI   INTMODE,FSTLIST     TEST FIRST TIME SWITCH SET                   
         BNE   *+8                                                              
         MVI   SVSWSTRT,X'00'                                                   
         B     ESTX                                                             
*                                                                               
* VALREC LOGIC-DISPLAY OR CHANGE                                                
*                                                                               
EST4     BAS   RE,SETSCR                                                        
         CLI   RETURNED,0          TEST IF RETURNED TO                          
         BE    *+16                NO                                           
         BAS   RE,TSTSEL           YES-TEST ANY PENDING SELECTS                 
         BE    EST20               YES-PROCEED WITH EDIT                        
         B     EST5                NO-RE-DISPLAY PAGE                           
*                                                                               
         BAS   RE,PROCPF                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    EST6                                                             
         CLI   PFKEY,PF1           REQUEST PFKEY SWAP?                          
         BNE   EST4A               NO                                           
         MVI   PFKEY,0             CLEAR PFKEY                                  
         B     ESTX                DONT SCROLL                                  
*                                                                               
EST4A    BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    EST20               YES                                          
*                                                                               
EST5     MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
EST6     XC    PRONOTE,PRONOTE                                                  
         OI    PRONOTEH+6,X'80'                                                 
         GOTO1 VCLEARF,DMCB,AFSTNAME,AENDSCR                                    
         GOTO1 (RF),(R1),(1,AFSTNAME),APFFLD                                    
*                                                                               
         CLI   SVPFSTAT,SVPFBASE   USING BASE PFKEYS ?                          
         BE    EST6A               YES                                          
         CLI   PFKEY,PF3           TEST FOR OVERNIGHT REQUEST                   
         BE    EST30                                                            
         B     EST6B                                                            
*                                                                               
EST6A    CLI   PFKEY,PF9           TEST FOR DOWNLOAD                            
         BE    EST30                                                            
         CLI   PFKEY,PF10          TEST FOR PRINTING                            
         BE    EST30                                                            
         CLI   PFKEY,PF11          TEST FOR SOON REQUEST                        
         BE    EST30                                                            
*                                                                               
EST6B    CLI   INTMODE,DISLIST                                                  
         BE    EST8                                                             
         BAS   RE,LOOK                                                          
         BAS   RE,PUTTAB                                                        
         GOTO1 DISP,PARAS,1                                                     
         BNE   EST16               XIT WITH NO DATA FOUND                       
*                                                                               
*                                                                               
EST7     BAS   RE,DISTOT                                                        
         BAS   RE,PUTTAB                                                        
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         B     EST15                                                            
*                                                                               
* CONTINUING LIST                                                               
*                                                                               
EST8     CLI   RETURNED,0          TEST IF RETURNED TO                          
         BE    EST9                NO                                           
         CLI   RETURNED,X'37'      ARE WE COMING FROM JOB DETAIL ?              
         BE    EST9                YES, TWA OK THEN                             
*                                                                               
* CASE OF MULTIPLE SELECTIONS--MAY NEED TO RE-LOOK UP JOB (JOB ELIST)           
*                                                                               
         BAS   RE,LOOK                                                          
         BAS   RE,PUTTAB                                                        
         SR    R3,R3               RE-DISPLAY ORIGINAL SCREEN                   
         ICM   R3,1,SVLOW                                                       
         BNZ   *+8                                                              
         LA    R3,1                PROTECT AGAINST NO WC'S ON IT                
         GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST12               SOMETHING TO DISPLAY                         
         STC   R3,SVLOW                                                         
         B     EST10               TRY GOING BACK A PAGE                        
*                                                                               
EST9     BAS   RE,GETTAB                                                        
         CLI   PFKEY,PF7           TEST PF7=PREVIOUS PAGE                       
         BE    EST10                                                            
         ZIC   R3,SVHI                                                          
         LA    R3,1(R3)                                                         
         C     R3,NESTENT          TEST IF PAST TABLE                           
         BNH   *+8                 NO                                           
         LA    R3,1                YES-WRAP AROUND TO START                     
         GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST12               SOMETHING TO DISPLAY                         
         B     EST16               NOTHING TO DISPLAY                           
*                                                                               
EST10    BAS   RE,BACK             BACK UP TO PREVIOUS PAGE                     
         ZIC   R3,BYTE                                                          
         GOTO1 DISP,PARAS,(R3)                                                  
         BNE   EST16               NOTHING TO DISPLAY                           
*                                                                               
EST12    BAS   RE,DISTOT                                                        
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
*                                                                               
EST15    L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         B     ESTX                                                             
*                                                                               
EST16    MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         LA    R2,PROJOBH                                                       
         ST    R2,ACURFORC                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
EST20    CLI   SVNWCS,0            TEST ANYTHING ON SCREEN                      
         BE    EST25               NO                                           
*                                                                               
         BAS   RE,GETTAB           GET EST TAB SO I KNOW THE WORKCODES          
         GOTO1 LOADOV,EDTMOD       LOAD AND CALL EDIT MODULE                    
         GOTO1 (RF),DMCB,(RC)                                                   
         BAS   RE,LOOK                                                          
         BAS   RE,PUTTAB                                                        
         XC    PRONOTE,PRONOTE                                                  
         OI    PRONOTEH+6,X'80'                                                 
         GOTO1 VCLEARF,DMCB,AFSTNAME,AENDSCR                                    
         GOTO1 (RF),(R1),(1,AFSTNAME),APFFLD                                    
*                                                                               
* USER CAN BE DOING CONCURRENT MAINTENANCE ON SCHEME CHANGING                   
* NUMBER OF WORKCODES IN SCHEME                                                 
*                                                                               
         ZIC   R3,SVLOW                                                         
         C     R3,NESTENT          TEST IF > ENTRIES IN TABLE                   
         BNH   *+8                 NO                                           
         L     R3,NESTENT          YES-CANNOT BE MORE THAN MAXIMUM              
         GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST22               SOMETHING ON SCREEN                          
*                                                                               
         STC   R3,SVLOW            RESET LOW ENTRY                              
         BAS   RE,BACK             TRY BACKING UP                               
         ZIC   R3,BYTE                                                          
         GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST22                                                            
*                                                                               
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         LA    R4,CONHEAD+L'EDTMSG-2                                            
         MVC   0(27,R4),=C'--NO DATA LEFT TO DISPLAY**'                         
         LA    R2,PROCLIH                                                       
         ST    R2,ACURFORC                                                      
         B     ESTX                                                             
*                                                                               
EST22    BAS   RE,DISTOT                                                        
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         B     ESTX                                                             
*                                                                               
EST25    MVC   CONHEAD(40),=C'**NO DATA TO EDIT--TRY ANOTHER DISPLAY**'         
         LA    R2,PROCLIH                                                       
         ST    R2,ACURFORC                                                      
         B     ESTX                                                             
*                                                                               
* PRINT OR DOWNLOAD A REPORT AND GENERATE A SOON REQUEST                        
*                                                                               
EST30    CLI   INTMODE,DISLIST     TEST FOR CONTINUING LIST                     
         BE    EST31               YES                                          
*                                                                               
         LA    R3,1                START AT FIRST WORKCODE                      
         BAS   RE,LOOK                                                          
         BAS   RE,PUTTAB                                                        
         B     EST32                                                            
*                                                                               
EST31    BAS   RE,GETTAB           RETRIEVE THE ESTIMATE TABLE                  
         SR    R3,R3                                                            
         ICM   R3,1,SVLOW                                                       
         BNZ   *+8                                                              
         LA    R3,1                DEFEND AGAINST PREV NOTHING TO DISP          
*                                                                               
EST32    GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST40               SOMETHING TO DISPLAY                         
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR NEW DISPLAY                         
         BE    EST34                                                            
*                                                                               
         STC   R3,SVLOW            TRY TO GO BACKWARDS                          
         BAS   RE,BACK                                                          
         ZIC   R3,BYTE                                                          
         GOTO1 DISP,PARAS,(R3)                                                  
         BE    EST40               NOW HAVE SOMETHING TO DISPLAY                
*                                                                               
EST34    MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     EST48                                                            
*                                                                               
EST40    BAS   RE,DISTOT                                                        
         CLI   PFKEY,PF11          TEST PF11=SOON REQUEST                       
         BE    EST46                                                            
         CLI   PFKEY,PF3           TEST PF3 (ALT) OVERNIGHT REQUEST             
         BE    EST45                                                            
*                                                                               
         MVC   REMUSER,TWAALIAS    SUPPLY REQUESTOR ID                          
         GOTO1 OPENPQ                                                           
         MVI   INTMODE,PRTREP                                                   
         CLI   PFKEY,PF10          TEST FOR PF10=PRINT REPORT                   
         BE    *+8                 YES                                          
         MVI   INTMODE,DOWNREP                                                  
         GOTO1 DISP,PARAS,1                                                     
*                                                                               
EST42    MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
EST44    LA    R4,CONHEAD          R4=OUTPUT POINTER                            
         MVC   0(2,R4),=C'**'                                                   
         MVC   2(3,R4),SPOOLID                                                  
         MVI   5(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,6(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,7(R4)                                                         
*                                                                               
         MVC   0(9,R4),=C'SPOOLED**'                                            
         B     EST48                                                            
*                                                                               
EST45    MVI   INTMODE,OVERREP                                                  
         B     *+8                                                              
*                                                                               
EST46    MVI   INTMODE,SOONREP     CALL DISPLAY TO GENERATE REQUEST             
         GOTO1 DISP,PARAS,1                                                     
*                                                                               
EST48    LA    R2,PROCLIH                                                       
         ST    R2,ACURFORC                                                      
         B     ESTX                                                             
*                                                                               
ESTX     MVI   ERROR,0             CLEAR ERROR FOR NORMAL EXIT                  
         XMOD1 1                                                                
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1  ,                                                                
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   OPTION,0            NO NAMES TO DISPLAY                          
         MVI   KEYCHG,C'N'                                                      
         MVI   REPRESET,C'N'                                                    
         MVI   REPFLAG,C'N'                                                     
*                                                                               
VALHED2  LA    R2,PROCLIH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   KEYCHG,C'Y'         DID KEY CHANGE ?                             
         BNE   *+8                 NO                                           
         MVI   REPRESET,C'Y'       SET, INDICATE REPORT TO BE RESET             
         GOTO1 VALCLI                                                           
*                                                                               
VALHED4  LA    R2,PROPROH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   KEYCHG,C'Y'         DID KEY CHANGE ?                             
         BNE   *+8                 NO                                           
         MVI   REPRESET,C'Y'       SET, INDICATE REPORT TO BE RESET             
         GOTO1 VALPROD                                                          
*                                                                               
VALHED6  LA    R2,PROJOBH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   KEYCHG,C'Y'         DID KEY CHANGE ?                             
         BNE   *+8                 NO                                           
         MVI   REPRESET,C'Y'       SET, INDICATE REPORT TO BE RESET             
         MVI   OPTION,C'Y'         GET JOB NAME                                 
         GOTO1 VALJOB                                                           
         MVI   OPTION,0            RESET FOR NEXT                               
*                                                                               
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BZ    *+10                NO                                           
         MVC   PROJNM+L'PROJNM-L'JEXJOB(L'JEXJOB),JEXJOB                        
*                                                                               
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,JOBSMCSE                                                
         BO    ERREND                                                           
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,ACJBNEWQ   INSURE JOB USES NEW ESTIMATES                
         BZ    ERREND                                                           
*                                                                               
         USING ACTRECD,RE                                                       
         MVI   ERROR,JOBUNAPP                                                   
         L     RE,AIO1             CHECK IF JOB IS APPROVED                     
         TM    ACTRSTAT,ACTSDRFT                                                
         BNZ   ERREND                                                           
         DROP  RE                                                               
*                                                                               
         L     R0,AIO3                                                          
         ST    R0,AJOB             SAVE THE JOB AT IO3                          
         BAS   RE,SAVEREC                                                       
         ST    R0,ACOLIST          PUT THE COLUMN LIST AFTER THE JOB            
*                                                                               
         BAS   RE,RDOPT            READ THE JOB'S OPTIONS                       
*                                                                               
         MVI   ERROR,MISSSCH                                                    
         OC    JOBSCH,JOBSCH       TEST THAT JOB HAS A SCHEME                   
         BZ    ERREND              NO                                           
*                                                                               
VALHED8  LA    R2,PROCOMH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BE    VALHED9             NO                                           
*                                                                               
         MVI   ERROR,CLOSERR       NO COMMANDS FOR CLOSED JOBS                  
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BO    ERREND              YES                                          
*                                                                               
VALHED9  LA    R2,PROCOLH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR COLUMN INPUT                        
         BNE   VALHED10            YES                                          
         MVI   ERROR,MISSING       NO                                           
         CLI   PROCOMH+5,0         MUST HAVE COMMAND INPUT                      
         BE    ERREND                                                           
         B     VALHED12            YES-SKIP TO OPTION EDIT                      
*                                                                               
* COLUMN FIELD EDIT                                                             
*                                                                               
VALHED10 GOTO1 VJOBCOL,DMCB,(R2),ACOLIST,ACOMFACS                               
         CLI   4(R1),0             TEST IF ERROR                                
         BNE   VALHED11            NO                                           
         MVI   ERROR,INVALID                                                    
         MVC   ERRNDX,5(R1)        SET DISP TO CURSOR                           
         B     ERREND                                                           
*                                                                               
VALHED11 MVC   NCOLS,4(R1)         SAVE NUMBER OF COLUMNS                       
         MVI   ERROR,COLSERR                                                    
         CLI   NCOLS,MAXCOLS       TEST FOR TOO MANY COLUMNS                    
         BNH   *+14                                                             
         MVC   BYTE,NCOLS          SET ERROR FIELD NUMBER                       
         B     COLCUR              POSITION CURSOR                              
*                                                                               
         GOTO1 AVALEST,DMCB,(RC),0                                              
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     COLCUR                                                           
*                                                                               
         BAS   RE,SETCOLS          SET COLUMN INDICATORS                        
*                                                                               
* OPTION FIELD EDIT                                                             
*                                                                               
VALHED12 LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0             TEST FOR ANY OPTION INPUT                    
         BE    VALHED14            NO                                           
         BAS   RE,VALOPT                                                        
*                                                                               
* REPORT FIELD EDIT                                                             
*                                                                               
VALHED14 LA    R2,PROREPH                                                       
         CLI   REPRESET,C'Y'       SHOULD WE REVALIDATE REPORT NAME ?           
         BE    VALHED16            YES                                          
         CLI   5(R2),0                                                          
         BNE   VALHED22                                                         
         B     VALHED24                                                         
*                                                                               
VALHED16 TM    4(R2),X'80'         WAS FIELD INPUT THIS TIME ?                  
         BO    VALHED18            YES, SEE WHAT THEY ENTERED                   
         SR    R1,R1               NO, CLEAR IT OUT AND RESET FIELD             
         ICM   R1,1,5(R2)                                                       
         BZ    VALHED20                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
VALHED18 CLI   5(R2),0                                                          
         BNE   VALHED22                                                         
*                                                                               
VALHED20 OC    GOESTREP,GOESTREP   NO, DO WE HAVE A DEFAULT ?                   
         BZ    VALHED24            NO                                           
         MVC   8(L'GOESTREP,R2),GOESTREP                                        
         MVI   5(R2),L'GOESTREP                                                 
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
VALHED22 XC    LISTAR,LISTAR       CLEAR WORK AREA                              
         MVC   LISTAR(L'PROREPH+L'PROREP),PROREPH                               
         CLI   LISTAR+8,C'-'       REPLACE SCREEN COLUMNS W/REPORT ?            
         BNE   VALHED23            NO                                           
         MVI   ERROR,INVALID       YES, PREPARE FOR ERRO                        
         SR    R1,R1                                                            
         IC    R1,LISTAR+5                                                      
         SH    R1,=H'1'                                                         
         BZ    ERREND                                                           
         STC   R1,LISTAR+5                                                      
         MVC   LISTAR+8(L'REPNAME),LISTAR+9                                     
         MVI   REPFLAG,C'Y'                                                     
*                                                                               
VALHED23 GOTO1 VALPROG,DMCB,LISTAR,AIO,REPNAME                                  
         BNE   ERREND                                                           
*                                                                               
* EXIT TO COMMAND MODULE IF COMMAND INPUT                                       
*                                                                               
VALHED24 CLI   PROCOMH+5,0         TEST FOR COMMAND INPUT                       
         BE    VALHED30            NO                                           
*                                                                               
         L     RF,SVLOAD42                                                      
         LTR   RF,RF               DO WE ALREADY HAVE THIS ADDRESS?             
         BNZ   VALHED28            YES                                          
*                                                                               
         GOTO1 LOADOV,COMMOD       LOAD COMMAND MODULE                          
         ST    RF,SVLOAD42                                                      
*                                                                               
VALHED28 GOTO1 (RF),DMCB,(RC)      GO TO IT-WON'T RETURN                        
*                                                                               
VALHED30 OI    PROCLIH+4,X'20'     TURN ON PREV VALID BITS                      
         OI    PROPROH+4,X'20'                                                  
         OI    PROJOBH+4,X'20'                                                  
         OI    PROCOLH+4,X'20'                                                  
         OI    PROCOMH+4,X'20'                                                  
         OI    PROOPTH+4,X'20'                                                  
         OI    PROREPH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         SPACE 2                                                                
SAVEREC  ST    RE,SAVERE                                                        
         L     RE,AIO                                                           
         LH    R1,ACLENGTH-ACKEYD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* PLACE THE CURSOR AT THE FIELD IN ERROR (BYTE=FIELD NUMBER IN ERROR)           
*                                                                               
COLCUR   LA    RE,PROCOL                                                        
         ZIC   R1,BYTE             GET FIELD NUMBER IN ERROR                    
         SH    R1,=H'1'                                                         
         BZ    COLCURX                                                          
*                                                                               
COLCUR2  CLI   0(RE),C','          TEST FOR A COMMA                             
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R1,COLCUR2                                                       
*                                                                               
COLCURX  LA    RF,8(R2)                                                         
         SR    RE,RF                                                            
         STC   RE,ERRNDX                                                        
         B     ERREND                                                           
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO VALIDATE THE OPTION FIELDS--CALLED FROM VALHED  *              
******************************************************************              
         SPACE 1                                                                
VALOPT   NTR1  ,                                                                
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         MVI   BYTE,PSNNONLQ+PSNMVOKQ  NO NULLS, MULTIPLE VALUES OK             
         GOTO1 PARSNIP,DMCB,(R2),BLOCK,(BYTE,0)                                 
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    VALOPT5             NO                                           
         MVI   ERROR,INVALID                                                    
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)          PARSNIP ERROR LOCATION                       
         B     VALOPTR                                                          
*                                                                               
VALOPT5  ZIC   R2,4(R1)            R2=N'COMPONENTS                              
         LA    R4,BLOCK                                                         
         USING PSND,R4             R4=A(PARSNIP BLOCK)                          
*                                                                               
VALOPT10 L     R3,PSNCOMP          R3=A(COMPONENT)                              
         CLI   PSNTAG,PSNFLDQ      TEST FOR KEYWORD FIELD                       
         BNE   VALOPTR                                                          
*                                                                               
         LA    R5,OPTTAB                                                        
         USING OPTTABD,R5                                                       
         LA    R0,OPTIONS          R0=LOOP COUNTER                              
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,L'OPTNAME    TEST LONGER THAN KEYWORD                     
         BH    VALOPTR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
*                                                                               
VALOPT15 CLC   PSNLEN,OPTMINL      TEST FOR MINIMUM LENGTH                      
         BL    VALOPT17            NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),OPTNAME                                                  
         BE    VALOPT20                                                         
*                                                                               
VALOPT17 LA    R5,OPTTABL(R5)                                                   
         BCT   R0,VALOPT15                                                      
         B     VALOPTR                                                          
*                                                                               
VALOPT20 MVI   ERROR,DUPINPUT      DUPLICATE INPUT CHECK                        
         MVC   HALF,OPTDISP                                                     
         LH    R6,HALF             DISP TO STORAGE VALUE                        
         LA    R6,SUBSYSD(R6)      POINT TO VALUE                               
         ZIC   R1,OPTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R6),0(R6)       TEST IF ANYTHING THERE                       
         BNZ   VALOPTR                                                          
*                                                                               
VALOPT22 MVI   ERROR,MISSING                                                    
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)         POINT PAST 'KEY='                            
         BAS   RE,NEXTFLD          NEXT PARSNIP                                 
         MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNVALQ      TEST FOR VALUE FIELD                         
         BNE   VALOPTR                                                          
*                                                                               
VALOPT23 TM    OPTIND,OPTIYN       TEST TO VALIDATE Y/N                         
         BZ    VALOPT25                                                         
*                                                                               
         CLI   PSNLEN,3                                                         
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)       COPY IN 'Y' OR 'N'                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BE    VALOPT30            OK                                           
         EX    R1,NOCOMP                                                        
         BE    VALOPT30                                                         
         B     VALOPTR                                                          
*                                                                               
VALOPT25 SR    RF,RF                                                            
         ICM   RF,3,OPTROUT        DISP TO ROUTINE                              
         LA    RF,T60B32(RF)                                                    
         BASR  RE,RF                                                            
*                                                                               
VALOPT30 LA    R4,PSNL(R4)         NEXT PARSNIP ENTRY                           
         BCT   R2,VALOPT10                                                      
*                                                                               
VALOPTX  B     XIT                                                              
         SPACE 2                                                                
VALZERO  ST    RE,SAVERE                                                        
         CLI   PSNLEN,8            TEST FOR BIG FIELD                           
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)       COPY IN FIRST BYTE                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,SUPCOMP          TEST FOR S(UPPRESS)                          
         BE    VALZEROX                                                         
         EX    R1,DISPCOMP         TEST FOR D(ISPLAY)                           
         BE    VALZEROX                                                         
         CLI   PSNLEN,3                                                         
         BH    VALOPTR                                                          
         EX    R1,AGYCOMP          TEST FOR A(GY)                               
         BNE   VALOPTR                                                          
*                                                                               
VALZEROX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
SUPCOMP  CLC   0(0,R3),=C'SUPPRESS'                                             
DISPCOMP CLC   0(0,R3),=C'DISPLAY'                                              
AGYCOMP  CLC   0(0,R3),=C'AGY'                                                  
         SPACE 2                                                                
* VALIDATE CAT=A(,B,C...) OR CAT=*A(,B,C...)                                    
*                                                                               
VALCATG  ST    RE,SAVERE                                                        
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALOPTR                                                          
         MVI   NEGSW,C'N'                                                       
         CLI   0(R3),C'*'          TEST FOR NEGATIVE FILTER                     
         BNE   VALCATG2            NO                                           
*                                                                               
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         STC   R1,PSNLEN           ADJUST BLOCK LENGTH                          
         LA    R3,1(R3)            ADVANCE COMPONENT POINTER                    
*                                                                               
VALCATG2 ZIC   RE,NCATS            INCREMENT CATEGORY COUNT                     
         LA    RE,1(RE)                                                         
         STC   RE,NCATS                                                         
         CLI   NCATS,MAXCATS       TEST IF MORE THAN MAX                        
         BH    VALOPTR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALOPTR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         CLI   NEGSW,C'Y'          TEST NEGATIVE FILTER                         
         BNE   *+8                                                              
         NI    0(R6),X'FF'-X'40'   YES                                          
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
VALCATG4 LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALCATGX            YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALCATGX                                                         
         CLI   PSNVSEP-PSND(RE),C'=' TEST FOR KEYWORD                           
         BE    VALCATGX            YES-CANNOT BE ANOTHER CATEGORY               
*                                                                               
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALCATG2                                                         
*                                                                               
VALCATGX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE WC=AA(,BB,CC...) OR WC=*AA(,BB,CC...)                                
*                                                                               
VALWC    ST    RE,SAVERE                                                        
         CLI   SWOPT,0             DO WE HAVE START OPTION ?                    
         BH    VALOPTW             YES, ERROR                                   
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALOPTR                                                          
         MVI   NEGSW,C'N'                                                       
         CLI   0(R3),C'*'          TEST FOR NEGATIVE FILTER                     
         BNE   VALWC2              NO                                           
*                                                                               
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         STC   R1,PSNLEN           ADJUST BLOCK LENGTH                          
         LA    R3,1(R3)            ADVANCE COMPONENT POINTER                    
*                                                                               
VALWC2   ZIC   RE,NWORK            INCREMENT WORKCODE COUNT                     
         LA    RE,1(RE)                                                         
         STC   RE,NWORK                                                         
         CLI   NWORK,MAXWORK       TEST IF MORE THAN MAX                        
         BH    VALOPTR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALOPTR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         CLI   NEGSW,C'Y'          TEST NEGATIVE FILTER                         
         BNE   *+8                                                              
         NI    0(R6),X'FF'-X'40'   YES                                          
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
VALWC4   LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALWCX              YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALWCX                                                           
         CLI   PSNVSEP-PSND(RE),C'=' TEST FOR KEYWORD                           
         BE    VALWCX              YES-CANNOT BE ANOTHER WORKCODE               
*                                                                               
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALWC2                                                           
*                                                                               
VALWCX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE SW=AA                                                                
*                                                                               
VALSW    ST    RE,SAVERE                                                        
         CLI   WCOPT,0             DO WE HAVE A SELECT OPTION ?                 
         BH    VALOPTW             YES, ERROR                                   
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALOPTR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALOPTR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
         LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALSWX              YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALSWX                                                           
         CLI   PSNVSEP-PSND(RE),C'=' TEST FOR KEYWORD                           
         BE    VALSWX              YES, MUST BE ANOTHER FIELD                   
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALOPTR                                                          
*                                                                               
VALSWX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE ROUNDING OPTION                                                      
*                                                                               
VALRND   ST    RE,SAVERE                                                        
         CLI   PSNLEN,3                                                         
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)                                                    
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP          ROUND=Y,N,P                                  
         BE    VALRNDX                                                          
         EX    R1,NOCOMP                                                        
         BE    VALRNDX                                                          
         CLI   PSNLEN,1                                                         
         BNE   VALOPTR                                                          
         CLI   0(R3),C'P'                                                       
         BNE   VALOPTR                                                          
*                                                                               
VALRNDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE DRAFT OPTION                                                         
*                                                                               
VALDFT   ST    RE,SAVERE                                                        
         CLI   PSNLEN,3                                                         
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)                                                    
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP          DRAFT=Y,N,O                                  
         BE    VALDFTX                                                          
         EX    R1,NOCOMP                                                        
         BE    VALDFTX                                                          
         CLI   PSNLEN,1                                                         
         BNE   VALOPTR                                                          
         CLI   0(R3),C'O'                                                       
         BNE   VALOPTR                                                          
*                                                                               
VALDFTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALBOX   ST    RE,SAVERE                                                        
         CLI   PSNLEN,8                                                         
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)       FIRST BYTE IS VALUE                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=C'COMPRESS' BOX=Y(ES),N(0),C(OMPRESS)                   
         BE    VALBOXX                                                          
         CLI   PSNLEN,3            NOW CANNOT BE MORE THAN 3                    
         BH    VALOPTR                                                          
         EX    R1,YESCOMP          BOX=Y(ES),N(O),P                             
         BE    VALBOXX                                                          
         EX    R1,NOCOMP                                                        
         BNE   VALOPTR                                                          
*                                                                               
VALBOXX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE MAXLINES OVERRIDE                                         
*                                                                               
VALMAXL  TM    PSNSTAT,PSNNUMQ     TEST FOR NUMERIC VALUE                       
         BO    *+12                YES                                          
         MVI   ERROR,NOTNUM                                                     
         B     VALOPTR                                                          
*                                                                               
         ICM   RF,15,PSNNUM        GET VALUE                                    
         CH    RF,=H'40'           TEST BETWEEN 40-112                          
         BL    VALOPTR                                                          
         CH    RF,=H'112'                                                       
         BH    VALOPTR                                                          
         STC   RF,MAXLOPT                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ERROR EXIT WITH CURSOR POSITIONING                                            
*                                                                               
VALOPTR  LA    R2,PROOPTH          SET R2=A(OPTION FLDH)                        
         LA    RE,8(R2)            START OF FIELD                               
         SR    R3,RE               COMPUTE DISPLACEMENT INTO FIELD              
         STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         SPACE 2                                                                
* ERROR EXIT WITH CURSOR POSITIONING                                            
*                                                                               
VALOPTW  MVI   ERROR,OPTCNFLT                                                   
         LA    R2,PROOPTH          SET R2=A(OPTION FLDH)                        
         LA    RE,8(R2)            START OF FIELD                               
         SH    R4,=AL2(PSNL)                                                    
         L     R3,PSNCOMP                                                       
         SR    R3,RE               COMPUTE DISPLACEMENT INTO FIELD              
         STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO ADVANCE TO NEXT PARSNIP FIELD                                  
*                                                                               
NEXTFLD  LA    R4,PSNL(R4)                                                      
         BCTR  R2,RE                                                            
         B     VALOPTR                                                          
         SPACE 2                                                                
YESCOMP  CLC   0(0,R3),=C'YES'                                                  
NOCOMP   CLC   0(0,R3),=C'NO '                                                  
         EJECT                                                                  
* SUB-ROUTINE TO SET THE COLUMN INDICATORS FOR PROTECTED/UNPROTECTED            
* FIELDS--ASSUMES THAT VALEST HAS BEEN CALLED IMMEDIATELY BEFORE                
*                                                                               
         USING COLD,RF                                                          
SETCOLS  NTR1  ,                                                                
         LA    R0,MAXCOLS                                                       
         LA    RF,COLINDS                                                       
         MVI   COLATTB,COLAPROT    INITIALIZE THEM TO PROTECTED                 
         LA    RF,COLLENQ(RF)                                                   
         BCT   R0,*-8                                                           
*                                                                               
SETCOLS1 ZIC   R0,NCOLS            N'COLUMNS                                    
         L     R1,ACOLIST          R1=A(COLUMN LIST)                            
         USING JBCLD,R1                                                         
         LA    RF,COLINDS          RF=A(COLUMN INDICATORS)                      
*                                                                               
SETCOLS2 CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BNE   SETCOLS8                                                         
         MVI   COLFLAG,COLFEST     ASSUME IT'S AN ESTIMATE                      
         CLC   JBCLCN1,ORIGEST     TEST FOR ORIGINAL ESTIMATE                   
         BE    SETCOLS3                                                         
         CLC   JBCLCN1,CURREST     TEST FOR CURRENT ESTIMATE                    
         BE    SETCOLS4                                                         
         CLC   JBCLCN1,REGEST      TEST FOR REGULAR ESTIMATE                    
         BE    SETCOLS5                                                         
         CLC   JBCLCN1,=Y(JBDRATE) TEST FOR COMMISSION RATE                     
         BNE   *+8                                                              
         MVI   COLFLAG,COLFCOM     MARK IT FOR OVERLAYS                         
         NI    COLFLAG,X'FF'-COLFEST  NOT AN ESTIMATE                           
         B     SETCOLS8                                                         
*                                                                               
SETCOLS3 MVC   COLEST,ORGTYPE      SAVE ORIGINAL ESTS TYPE/VERSION              
         B     SETCOLS6                                                         
*                                                                               
SETCOLS4 MVC   COLEST,CURTYPE      SAVE CURRENT ESTS TYPE/VERSION               
         B     SETCOLS6                                                         
*                                                                               
SETCOLS5 MVC   COLEST,JBCLCN1E                                                  
*                                                                               
SETCOLS6 LA    R5,BUFF             R5=A(ESTIMATE LIST)                          
         ZIC   RE,NESTS                                                         
         CLC   COLEST,0(R5)        MATCH ON ESTIMATE                            
         BE    SETCOLS7                                                         
         LA    R5,4(R5)                                                         
         BCT   RE,*-14                                                          
         DC    H'0'                PROBLEM IF I CANNOT LOCATE ESTIMATE          
*                                                                               
SETCOLS7 CLI   3(R5),C'T'          IS THERE TIME?                               
         BE    SETCOLS8            YES, SKIP ALL THIS                           
         MVI   COLATTB,COLAUNP     NOTE AS UNPROTECTED                          
         CLI   2(R5),C'A'          IS IT APPROVED ?                             
         BNE   *+8                 NO                                           
         OI    COLFLAG,COLFEAPP                                                 
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED ESTIMATE                  
         BNE   SETCOLS8                                                         
         TM    COLFLAG,COLFEAPP    TEST IF ESTIMATE IS APPROVED                 
         BZ    *+8                 NO                                           
         MVI   COLATTB,COLAPROT    YES-MAKE COLUMN PROTECTED                    
*                                                                               
SETCOLS8 LA    RF,COLLENQ(RF)      NEXT COLUMN INDICATOR                        
         LA    R1,JBCLENQ(R1)      NEXT COLUMN LIST ENTRY                       
         BCT   R0,SETCOLS2                                                      
*                                                                               
SETCOLSX B     XIT                                                              
         DROP  R1,RF                                                            
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO PROCESS PF KEYS BEFORE HANDLING EDITS            *             
*******************************************************************             
         SPACE 1                                                                
PROCPF   NTR1  ,                                                                
         CLI   PFKEY,0                                                          
         BE    PROCPFX                                                          
*                                                                               
PROCPF2  L     RE,ATWA                                                          
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R1,CONACTH          TEST NOTHING CHANGED AFTER ACTION            
         CR    RE,R1                                                            
         BH    PROCPFX                                                          
*                                                                               
         CLI   SVPFSTAT,SVPFALT    ALTERNATE PF KEY LIST DISPLAYED              
         BE    PROCPF4             YES                                          
*                                                                               
         CLI   PFKEY,PF2           TEST FOR PF2=OPTION MAINT                    
         BE    PROCPF6             YES                                          
         CLI   PFKEY,PF3           TEST FOR PF3=JOB ELIST                       
         BE    PROCPF8                                                          
         CLI   PFKEY,PF4           TEST FOR PF4=JOB SUMMARY                     
         BE    PROCPF10                                                         
         CLI   PFKEY,PF5           TEST FOR PF5=TEXT MAINT                      
         BE    PROCPF12                                                         
         CLI   PFKEY,PF6           TEST FOR PF6=TEXT LIST                       
         BE    PROCPF14                                                         
         CLI   PFKEY,PF1           REQUEST ALT PF KEY LIST                      
         BE    PROCPF16                                                         
         B     PROCPFX             DID NOT RECOGNIZE PFKEY                      
*                                                                               
PROCPF4  CLI   PFKEY,PF2           CALL SESSION ESTIMATE                        
         BE    PROCPF22                                                         
         CLI   PFKEY,PF1           REQUEST ALT PF KEY LIST                      
         BE    PROCPF16                                                         
         B     PROCPFX             DID NOT RECOGNIZE PFKEY                      
*                                                                               
PROCPF6  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'OPTION',=C'MAINT',=C',',=C',',(6,CLICODE),X        
               (6,PRODCODE),(6,JOBNUM),0                                        
*                                                                               
PROCPF8  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'JOB',=C'ELIST',(L'CLICODE,CLICODE),       X        
               (L'PRODCODE,PRODCODE),(L'JOBNUM,JOBNUM),0                        
*                                                                               
PROCPF10 MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'JOB',=C'SUMMARY',(6,CLICODE),(6,PRODCODE),X        
               (6,JOBNUM),0                                                     
*                                                                               
PROCPF12 MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'TEXT',=C'MAINT',=C',',=C',',(6,CLICODE),  X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',=C',',=C',',=C'E',0          
*                                                                               
PROCPF14 MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'TEXT',=C'LIST',=C',',=C',',(6,CLICODE),   X        
               (6,PRODCODE),(6,JOBNUM),0                                        
*                                                                               
PROCPF16 EQU   *                   PF1/PF13 PRESSED, REQUEST ALT LIST           
         L     R6,=A(PFELS)                                                     
         CLI   SVPFSTAT,SVPFALT    IF ALTERNATE, REDISPLAY BASE                 
         MVI   SVPFSTAT,SVPFBASE                                                
         BE    PROCPF18                                                         
*                                                                               
         L     R6,=A(ALTPFELS)                                                  
         MVI   SVPFSTAT,SVPFALT    ASSUME SWITCH TO ALT PFKEYS                  
*                                                                               
PROCPF18 A     R6,RELO                                                          
         MVI   ELCODE,1                                                         
         L     R2,APFFLD                                                        
         SR    R1,R1                                                            
*                                                                               
         USING TWAELEMD,R6                                                      
PROCPF20 IC    R1,TWAEFLN          FIELD LENGTH                                 
         STC   R1,5(R2)            STORE FIELD LENGTH                           
         BCTR  R1,0                MOVE FIELD DATA                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TWAEDTA                                                  
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BAS   RE,BUMP                                                          
         BAS   RE,NEXTEL                                                        
         BE    PROCPF20                                                         
         B     PROCPFX                                                          
         DROP  R6                                                               
*                                                                               
PROCPF22 MVI   PFAID,0                                                          
         MVI   PFKEY,0                                                          
         L     R1,SYSPARMS                                                      
         L     RE,0(R1)                                                         
         USING TIOBD,RE                                                         
         MVI   TIOBAID,0                                                        
         DROP  RE                                                               
         GOTO1 ACALLSES,DMCB,(RC)  CALL SESSION ESTIMATING                      
*                                                                               
PROCPFX  B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO BACK UP TO START ENTRY FOR PREVIOUS PAGE    *                  
* ON EXIT, BYTE CONTAINS START ENTRY NUMBER                  *                  
**************************************************************                  
         SPACE 1                                                                
BACK     NTR1  ,                                                                
         MVI   BYTE,1              INITIALIZE START ENTRY TO ONE                
         SR    R2,R2                                                            
         ICM   R2,1,SVLOW          GET LOW ENTRY NUMBER ON PAGE                 
         BZ    BACKX               NONE                                         
         SH    R2,=H'1'            START AT THE ONE BEFORE IT                   
         BZ    BACKX               ALREADY AT START OF TABLE                    
*                                                                               
         L     R5,AESTTAB                                                       
         L     R6,LESTTAB          R6=TABLE ENTRY LENGTH                        
         LR    RF,R2                                                            
         MR    RE,R6               INDEX INTO TABLE                             
         LA    R5,0(RF,R5)         R5=A(ESTIMATE TABLE ENTRY)                   
         USING ESTTABD,R5                                                       
         LA    R0,MAXDATA          R0=MAX N'DATA LINES ON SCREEN                
*                                                                               
BACK2    BAS   RE,FILCAT           FILTER ON CATEGORIES                         
         BNE   BACK6                                                            
         CLI   ZEROPT,C'S'         TEST FOR ZERO SUPPRESSION                    
         BE    *+12                                                             
         CLI   ZEROPT,C'A'                                                      
         BNE   BACK4                                                            
*                                                                               
         USING COLD,RF                                                          
         ZIC   R1,NCOLS                                                         
         LA    RE,ESTVALS                                                       
         LA    RF,COLINDS                                                       
BACK3    TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE                     
         BO    *+14                                                             
         CP    0(L'ESTVALS,RE),=P'0'                                            
         BNE   BACK4                                                            
         LA    RE,L'ESTVALS(RE)                                                 
         LA    RF,COLLENQ(RF)                                                   
         BCT   R1,BACK3                                                         
         B     BACK6               ZERO LINE-SKIP IT                            
*                                                                               
BACK4    STC   R2,BYTE             SAVE ENTRY NUMBER                            
         BCT   R0,BACK6                                                         
         B     BACKX               SCREEN IS FILLED                             
*                                                                               
BACK6    SR    R5,R6               BACK UP ONE ENTRY                            
         BCT   R2,BACK2                                                         
*                                                                               
BACKX    B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FILTER AGAINST CATEGORY LIST                                   
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT CATEGORY                               
*                                                                               
FILCAT   CLI   NCATS,0                                                          
         BER   RE                                                               
         ZIC   R0,NCATS                                                         
         LA    R1,CATOPT                                                        
         TM    CATOPT,X'40'        TEST POSITIVE FILTER                         
         BZ    FILCAT2             NO                                           
*                                                                               
FILCAT1  CLC   ESTCAT,0(R1)                                                     
         BE    FILCATY                                                          
         LA    R1,L'ESTCAT(R1)                                                  
         BCT   R0,FILCAT1                                                       
         B     FILCATN                                                          
*                                                                               
FILCAT2  MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   ESTCAT,HALF                                                      
         BE    FILCATN                                                          
         LA    R1,L'ESTCAT(R1)                                                  
         BCT   R0,*-14                                                          
*                                                                               
FILCATY  CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILCATN  LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE SCREEN DYNAMICALLY                                   
*                                                                               
         USING COLD,R4                                                          
BLDTWA   NTR1  ,                                                                
         LA    R3,HEADELS          R3=A(SCREEN BUILD ELEMENTS)                  
         USING TWAELEMD,R3                                                      
         ZIC   R2,NCOLS            R2=LOOP COUNTER                              
         LA    R4,COLINDS          R4=A(COLUMN INDICATOR)                       
         MVI   BYTE,C'N'           SET SECOND PASS SWITCH TO NO                 
*                                                                               
BLDTWA2  MVI   TWAEATB,X'20'       SET FOR PROTECTED FIELD                      
         CLI   COLATTB,COLAPROT                                                 
         BE    *+8                                                              
         MVI   TWAEATB,X'48'       SET UNP/HI INTENSITY/LOWER CASE              
         SR    R0,R0                                                            
         IC    R0,TWAELLN                                                       
         AR    R3,R0                                                            
         LA    R4,COLLENQ(R4)                                                   
         BCT   R2,BLDTWA2                                                       
*                                                                               
BLDTWA3  CLI   BYTE,C'Y'           TEST FOR SECOND PASS                         
         BE    BLDTWA4             YES                                          
*                                                                               
         MVI   BYTE,C'Y'           SET SECOND PASS                              
         LA    R3,HEADELS2         R3=A(SECOND NAME LINE)                       
         LA    R4,COLINDS                                                       
         ZIC   R2,NCOLS                                                         
         B     BLDTWA2                                                          
         DROP  R4                                                               
*                                                                               
BLDTWA4  XC    TWAPARM,TWAPARM                                                  
         LA    R1,TWAPARM                                                       
         USING TWAPARMD,R1                                                      
         MVC   TWAPATWA,ATWA                                                    
         LA    R3,HEADELS                                                       
         ST    R3,TWAPAFST         SET A(SCREEN ELEMENTS)                       
         MVC   TWAPAMAX,=AL4(SVDATA-T60BFFD)                                    
         LA    R2,PROTAGH          R2=A(START OF SCREEN)                        
         ST    R2,TWAPAOUT                                                      
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWAPAOUT,TWAPANXT   UPDATE NEXT SCREEN POSITION                  
*                                                                               
         USING COLD,RE                                                          
BLDTWA6  LA    R3,DATAELS          NOW PREPARE DATA LINE ELEMENTS               
         ZIC   R2,NCOLS            R2=ELEMENT COUNTER                           
         LA    RE,COLINDS                                                       
         SR    R0,R0                                                            
*                                                                               
BLDTWA7  CLI   TWAELCD,X'02'       TEST FOR DATA FIELDS                         
         BNE   BLDTWA8             NO                                           
         MVI   TWAEATB,X'20'       INITIALIZE TO PROTECTED                      
         CLI   COLATTB,COLAUNP     TEST FOR UNPROTECTED FIELD                   
         BNE   *+8                                                              
         MVI   TWAEATB,X'08'       YES-SET TO UNP/HI INTENSITY                  
         LA    RE,COLLENQ(RE)                                                   
         BCT   R2,BLDTWA8                                                       
         B     BLDTWA10            HAVE DEALT WITH ALL FIELDS                   
         DROP  RE                                                               
*                                                                               
BLDTWA8  IC    R0,TWAELLN                                                       
         AR    R3,R0                                                            
         B     BLDTWA7                                                          
*                                                                               
BLDTWA10 LA    R3,DATAELS                                                       
         ST    R3,TWAPAFST                                                      
         LA    R0,MAXDATA          R0=N'DATA LINES                              
BLDTWA11 GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWAPAOUT,TWAPANXT                                                
         BCT   R0,BLDTWA11                                                      
*                                                                               
BLDTWA15 LA    R3,TOTALELS         NOW DO THE TOTAL LINE                        
         ST    R3,TWAPAFST                                                      
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWAPAOUT,TWAPANXT                                                
*                                                                               
BLDTWA20 L     R3,=A(PFELS)        FINISH WITH THE PF KEY LINES                 
         A     R3,RELO                                                          
         ST    R3,TWAPAFST                                                      
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVPFSTAT,SVPFBASE   FLAG AS BASE PF KEYS DISPLAYED               
*                                                                               
BLDTWAX  B     XIT                                                              
         DROP  R1,R3                                                            
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO DISPLAY THE DATA LINES ON THE SCREEN          *                
*     AT ENTRY, P1=N'START TABLE ENTRY                         *                
*     ON EXIT, CC=EQ AND SVNWCS SET IF SOMETHING TO DISPLAY    *                
*     AND CC=NEQ IF NOTHING TO DISPLAY                         *                
****************************************************************                
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         L     R4,0(R1)            GET START ENTRY NUMBER                       
         OC    NESTENT,NESTENT     TEST FOR ANY WORKCODE ENTRIES                
         BZ    DISPX               NONE IN TABLE-SKIP DISPLAY                   
         OC    VDISP,VDISP         TEST IF DISPLAY MODULE AVAILABLE             
         BNZ   DISP2                                                            
*                                                                               
         GOTO1 LOADOV,DISPMOD                                                   
         ST    RF,VDISP                                                         
*                                                                               
DISP2    CLI   SCHCHG,C'Y'         HAS SCHEME CHANGED?                          
         MVI   SCHCHG,C'N'         RESET IT ANYWAY                              
         BNE   *+8                 NO                                           
         LA    R4,1                YES, RESET R4                                
         GOTO1 VDISP,DMCB,(RC),(R4)                                             
         BE    YESXIT                                                           
         B     NOXIT                                                            
*                                                                               
DISPX    MVI   SVNWCS,0            IN CASE YOU RETURN, TELL BASE THERE          
         B     NOXIT               ARE NO W/C'S                                 
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
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE JOB TOTALS LINE                                    
*                                                                               
         USING COLD,R6                                                          
DISTOT   ST    RE,SAVERE                                                        
         L     R2,ATOTFLD          R2=A(FIELD HEADER)                           
         ZIC   R3,NCOLS            R3=LOOP COUNTER                              
         LA    R5,SVJOBTOT         R5=A(JOB TOTALS)                             
         MVC   LISTAR,SPACES                                                    
         MVC   8(10,R2),=C'JOB TOTALS'                                          
         LA    R4,LISTAR           R4=OUTPUT POINTER                            
         LA    R6,COLINDS                                                       
*                                                                               
DISTOT2  TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE                     
         BO    DISTOT3             YES                                          
         EDIT  (P6,0(R5)),(14,(R4)),2,ALIGN=LEFT,MINUS=YES                      
         B     DISTOT4                                                          
*                                                                               
DISTOT3  CP    0(L'ESTVALS,R5),=P'0' TEST FOR ZERO RATE                         
         BE    DISTOT4             YES-SKIP EDIT                                
         EDIT  (P6,0(R5)),(14,(R4)),4,ALIGN=LEFT                                
*                                                                               
DISTOT4  LA    R4,15(R4)           NEXT OUTPUT POSITION                         
         LA    R5,L'SVJOBTOT(R5)                                                
         LA    R6,COLLENQ(R6)                                                   
         BCT   R3,DISTOT2                                                       
*                                                                               
         MVC   23(59,R2),LISTAR                                                 
*                                                                               
DISTOTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINES TO SET SCREEN ADDRESSES                                          
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PROTAGH                                                       
         ST    R2,AFSTNAME                                                      
*                                                                               
         LA    R1,MAXCOLS                                                       
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,ASECNAME         A(SECOND NAME LINE)                          
*                                                                               
         LA    R1,MAXCOLS-1                                                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,ALSTNAME                                                      
*                                                                               
         BAS   RE,BUMP                                                          
         ST    R2,AFSTSEL                                                       
*                                                                               
         LA    R1,10*NDATAFLD      COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,ATOTFLD                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         ST    R2,APFFLD                                                        
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NDATAFLD                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO LOAD IN AN OVERLAY                                             
* AT ENTRY, R1=MODULE NUMBER.  ON EXIT, RF=A(MODULE)                            
*                                                                               
LOADOV   ST    RE,FULL                                                          
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB             SET LOAD POINT                               
         MVC   DMCB+4(3),SYSPHASE                                               
         STC   R1,DMCB+7           SET MODULE NUMBER                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'         TEST FOR ERROR IN LOAD                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             RF=A(MODULE)                                 
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO TEST FOR ANY PENDING SELECT FIELDS TO EDIT                     
* ON EXIT, CC=EQ FOR A PENDING SELECT, CC=NEQ FOR NO PENDING SELECTS            
*                                                                               
TSTSEL   ST    RE,FULL                                                          
         SR    R3,R3                                                            
         ICM   R3,1,SVNWCS         R3=LOOP COUNTER                              
         BZ    TSTSELN                                                          
         L     R2,AFSTSEL                                                       
*                                                                               
TSTSEL2  BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    TSTSEL4                                                          
         CLI   8(R2),C'*'          TEST FOR PRIOR PROCESSING                    
         BNE   TSTSELY                                                          
*                                                                               
TSTSEL4  L     R2,ANEXTSEL                                                      
         BCT   R3,TSTSEL2                                                       
*                                                                               
TSTSELN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTSELX                                                          
*                                                                               
TSTSELY  CR    RB,RB                                                            
*                                                                               
TSTSELX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO LOOK UP JOB VALUES FOR THE SCREEN               *              
******************************************************************              
         SPACE 1                                                                
LOOK     NTR1  ,                                                                
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK            CLEAR JOBLOCK                                
         LR    RE,R6                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   JBAJOB,AJOB         COMPLETE THE JOBBLOCK                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         LA    RE,GOBLOCK                                                       
         ST    RE,JBAGOBLK                                                      
         MVC   JBAIO,AIO1          USE IO AREA 1                                
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBSELSCH,JOBSCH                                                  
         MVC   JBGETDFT,DFTOPT                                                  
         MVC   JBACOLTB(16),ACOLTAB COLUMN/OPERAND VALUE TABLES                 
*                                                                               
LOOK2    GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0           TEST FOR ERROR                               
         BE    LOOK4                                                            
         CLI   JBERROR,JBERRCOL    ERROR WITH COLUMN OR OPERAND?                
         BE    LOOK3               YES, PRINT MESSAGE                           
         CLI   JBERROR,JBERROPV                                                 
         BE    LOOK3                                                            
         CLI   JBERROR,JBERRSCH    TEST FOR SCHEME ERROR                        
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
         CLI   INTMODE,EDTLIST     TEST FOR EDIT MODE                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ERROR,BADSCH        INVALID SCHEME                               
         LA    R2,PROJOBH                                                       
         B     ERREND                                                           
*                                                                               
LOOK3    MVI   ERROR,COOPERR       COLUMN/OPERAND TABLE ERROR                   
         LA    R2,PROCOLH                                                       
         B     ERREND                                                           
*                                                                               
LOOK4    LA    R4,BUFF             R4=A(JOB VALUE TABLE)                        
         USING ESTTABD,R4                                                       
         ST    R4,AESTTAB                                                       
         ZIC   RE,JBNCOLS                                                       
         MH    RE,=Y(L'ESTVALS)                                                 
         LA    RE,ESTVALS-ESTTABD(RE)                                           
         ST    RE,LESTTAB                                                       
         XC    NESTENT,NESTENT                                                  
         L     R5,JBACOLTB         R5=A(COLUMN VALUE TABLE)                     
         USING JBCOLD,R5                                                        
         LH    R2,JBNROWS          R2=LOOP COUNTER                              
         LH    R3,JBLCOL           R3=L'COLUMN TABLE ENTRY                      
*                                                                               
LOOK6    CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ENTRY                      
         BNE   LOOK10              NO                                           
*                                                                               
LOOK8    MVC   ESTWORKC,JBCOLWC                                                 
         MVC   ESTSUF,JBCOLSUF                                                  
         MVC   ESTCAT,JBCOLCAT                                                  
         ZIC   R1,JBNCOLS                                                       
         MH    R1,=Y(L'JBCOLVAL)                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ESTVALS(0),JBCOLVAL                                              
*                                                                               
         L     RE,NESTENT          INCREMENT ESTIMATE TABLE COUNT               
         LA    RE,1(RE)                                                         
         ST    RE,NESTENT                                                       
         A     R4,LESTTAB          NEXT ENTRY                                   
*                                                                               
LOOK10   LA    R5,0(R3,R5)         NEXT COLUMN ENTRY                            
         BCT   R2,LOOK6                                                         
*                                                                               
LOOK12   L     R5,JBACOLTB         R5=A(COLUMN TABLE)                           
         ZIC   R1,JBNCOLS          JOB TOTALS ARE FIRST ENTRY                   
         MH    R1,=Y(L'JBCOLVAL)                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVJOBTOT(0),JBCOLVAL EXTRACT JOB TOTALS                          
*                                                                               
LOOKX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTNAME         CHECK THE NAME FIELDS FIRST                  
         L     R1,ALSTNAME         R1=BXLE LIMIT                                
         SR    R0,R0                                                            
*                                                                               
TSTEDT1  TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES-SOMETHING TO EDIT                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R1                                                            
         BL    TSTEDT1                                                          
*                                                                               
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,SVNWCS                                                      
         BZ    TSTEDTN             NOTHING ON SCREEN                            
*                                                                               
TSTEDT2  BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    TSTEDT4             NO                                           
         CLI   8(R2),C'*'          TEST ALREADY PROCESSED                       
         BE    TSTEDT4             YES                                          
         B     TSTEDTY             INPUT IN A SELECT FIELD                      
*                                                                               
TSTEDT4  ZIC   R4,NCOLS            R4=FIELD COUNTER                             
         L     R2,ADATA1                                                        
*                                                                               
TSTEDT5  TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
*                                                                               
         BAS   RE,BUMP                                                          
         BCT   R4,TSTEDT5                                                       
*                                                                               
         L     R2,ANEXTSEL         ADVANCE TO SELECT                            
         BCT   R3,TSTEDT2          ANOTHER LINE                                 
*                                                                               
TSTEDTN  B     NOXIT               SET CC=NEQ                                   
*                                                                               
TSTEDTY  B     YESXIT              SET CC=EQ                                    
         EJECT                                                                  
* SUB-ROUTINE TO READ THE JOB'S OPTIONS                                         
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR       READ THE JOB'S OPTIONS                       
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOWHICH,C'N'        ONLY NEW OPTIONS                             
         MVC   GOAJOB,AJOB                                                      
         LA    R1,GOXBLOCK                                                      
         ST    R1,GOAEXT                                                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         MVI   SCHCHG,C'N'         INITIALIZE NO SCHEME CHANGE                  
         CLI   SVSCHEM,0           DO WE HAVE A SCHEME YET?                     
         BNH   RDOPT2              NO, SKIP COMPARE                             
         CLC   SVSCHEM,GOSCHEME    HAS SCHEME CHANGED?                          
         BE    RDOPT2              NO                                           
         MVI   SCHCHG,C'Y'         YES, INDICATOR IT                            
*                                                                               
RDOPT2   MVC   JOBSCH,GOSCHEME     EXTRACT SCHEME VALUE                         
         MVC   SVSCHEM,JOBSCH                                                   
*                                                                               
RDOPTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE ESTIMATE EXPRESSIONS IN THE COLUMN LIST               
*                                                                               
* AT ENTRY, ACOLIST=A(COLUMN LIST) P2=0 TO CHECK KEY'S JOB P2=1 TO              
*           CHECK FROMJOB (CALL FROM EDCOPY) P1=A(GEND)                         
* ON EXIT, CC=EQ IF OK, CC=NEQ IF ERROR AND BYTE CONTAINS ERROR ENTRY           
*          NUMBER                                                               
*                                                                               
VALEST   NTR1  BASE=ABASE1                                                      
         L     R7,ABASE2                                                        
         L     RC,0(R1)                                                         
         MVI   NREVS,0             CLEAR NUMBER OF REVISIONS                    
         SR    R3,R3                                                            
         LA    R5,BUFF             R5=A(ESTIMATE LIST)                          
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         ICM   RE,15,4(R1)         TEST WHICH JOB TO CHECK                      
         BZ    *+10                ITS THE KEY                                  
         MVC   ACEVCLI(ACEVTYPE-ACEVCLI),FROMCLI                                
         GOTO1 HIGH                                                             
         B     VALEST2                                                          
*                                                                               
VALEST1  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
VALEST2  CLC   ACEVKEY(ACEVTYPE-ACEVKEY),KEYSAVE                                
         BNE   VALEST5                                                          
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF)  IS IT A TIME EST RECORD?                   
         BNZ   VALEST1               YES - SO READ NEXT RECORD                  
*                                                                               
         XC    0(4,R5),0(R5)       CLEAR OUT ENTRY                              
         MVC   0(2,R5),ACEVTYPE    START WITH TYPE/VERSION NUMBER               
*                                                                               
VALEST3  CLI   ACEVTYPE,ACEVREV    TEST FOR REVISION                            
         BNE   VALEST00                                                         
*                                                                               
         ZIC   R1,NREVS                                                         
         LA    R1,1(R1)            INCREMENT COUNT OF REVISIONS                 
         STC   R1,NREVS                                                         
         CLI   ORGTYPE,0           TEST IF FIRST REVISION                       
         BNE   *+10                                                             
         MVC   ORGTYPE(2),ACEVTYPE YES-ITS THE ORIGINAL                         
         MVC   CURTYPE(2),ACEVTYPE CURRENT ESTIMATE=HIGHEST REVISION            
         MVC   HIRTYPE(2),ACEVTYPE SET HIGHEST REVISION                         
         MVI   ELCODE,ACEAELQ      SEARCH FOR APPROVAL ELEMENT                  
         BAS   RE,GETELIO                                                       
         BNE   VALEST00                                                         
         MVC   HIATYPE(2),ACEVTYPE SET HIGHEST APPROVED ESTIMATE                
         MVI   2(R5),C'A'          NOTE ESTIMATE IS APPROVED                    
*                                                                               
VALEST00 MVI   ELCODE,ACEDELQ      LOOK THROUGH ESTIMATES NOW                   
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
         USING EDAELD,R6                                                        
VALES01  BAS   RE,NEXTEL                                                        
         BNE   VALEST4                                                          
         TM    EDATYPE,EDATTEST    IS THERE TIME ON THIS WORKCODE?              
         BZ    VALES01             NO, KEEP CHECKING                            
         MVI   3(R5),C'T'          NOTE ESTIMATE HAS TIME                       
*                                                                               
VALEST4  LA    R5,4(R5)                                                         
         LA    R3,1(R3)            BUMP ESTIMATE COUNT                          
         B     VALEST1                                                          
*                                                                               
VALEST5  STC   R3,NESTS            SAVE ESTIMATE COUNT                          
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED ESTIMATE TO BILL          
         BNE   *+18                NO                                           
         CLI   HIATYPE,0           TEST FOR HIGHEST APPROVED REVISION           
         BE    *+10                                                             
         MVC   CURTYPE(2),HIATYPE  YES-RESET CURRENT ESTIMATE                   
*                                                                               
         L     R2,ACOLIST                                                       
         USING JBCLD,R2                                                         
         MVI   BYTE,0                                                           
         LA    R4,1                                                             
*                                                                               
VALEST6  CLI   JBCLTYP,0                                                        
         BE    VALESTX                                                          
         CLI   JBCLTYP,JBCLSUPP                                                 
         BE    VALEST7                                                          
         CLI   JBCLTYP,JBCLCOL                                                  
         BNE   VALEST11                                                         
         CLC   JBCLCN1,ORIGEST                                                  
         BE    VALEST8                                                          
         CLC   JBCLCN1,CURREST                                                  
         BE    VALEST9                                                          
         CLC   JBCLCN1,PREVEST                                                  
         BE    VALEST10                                                         
         CLC   JBCLCN1,REGEST                                                   
         BNE   VALEST11                                                         
*                                                                               
         LTR   R0,R3                                                            
         BZ    VALESTR                                                          
         LA    R5,BUFF                                                          
         CLC   JBCLCN1E(2),0(R5)   MATCH ON LIST                                
         BE    VALEST11            YES-FOUND IT                                 
         LA    R5,4(R5)                                                         
         BCT   R0,*-14                                                          
         B     VALESTR                                                          
*                                                                               
VALEST7  CLI   NREVS,0                                                          
         BE    VALESTR                                                          
         CLC   JBCLCN1V,NREVS                                                   
         BNL   VALESTR                                                          
*                                                                               
         ZIC   RE,NESTS            INDEX TO REVISIONS IN BUFF                   
         ZIC   RF,NREVS                                                         
         SR    RE,RF                                                            
         MH    RE,=H'4'            ENTRIES ARE 3 BYTES LONG                     
         LA    RF,BUFF(RE)                                                      
*                                                                               
         ZIC   RE,JBCLCN1V                                                      
         MH    RE,=H'4'            MULTIPLY BY 3                                
         LA    RF,0(RF,RE)         INDEX TO CORRECT REVISION                    
*                                                                               
         MVC   JBCLSNV,JBCLCN1E    SAVE SUPPLEMENT                              
         MVC   JBCLCN1E,0(RF)                                                   
         MVC   JBCLCN1V,1(RF)                                                   
*                                                                               
         SH    RF,=H'4'                                                         
         MVC   JBCLCN2,JBCLCN1                                                  
         MVC   JBCLCN2E,0(RF)                                                   
         MVC   JBCLCN2V,1(RF)                                                   
*                                                                               
         MVI   JBCLOPER,C'-'       ALWAYS A MINUS                               
         MVI   JBCLTYP,JBCLFRM                                                  
         B     VALEST11                                                         
*                                                                               
VALEST8  CLI   ORGTYPE,0           TEST IF ORIGINAL FOUND                       
         BE    VALESTR                                                          
         B     VALEST11                                                         
*                                                                               
VALEST9  CLI   CURTYPE,0           TEST IF CURRENT FOUND                        
         BE    VALESTR                                                          
         B     VALEST11                                                         
*                                                                               
VALEST10 CLI   CURTYPE,0           DO WE HAVE A CURRENT?                        
         BE    VALESTR             NO, ERROR                                    
         CLI   CURTYPE+1,1         YES, IS IT REVISION 1?                       
         BE    VALESTR             YES, NO PREVIOUS THEN                        
*                                                                               
VALEST11 LA    R2,JBCLENQ(R2)                                                   
         LA    R4,1(R4)                                                         
         B     VALEST6                                                          
*                                                                               
VALESTR  STC   R4,BYTE                                                          
         B     NOXIT                                                            
*                                                                               
VALESTX  B     YESXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET AN ESTIMATE.                                               
* AT ENTRY, P1=A(GEND), P2=A(TYPE/VERSION)                                      
* ON EXIT, CC=EQ IF FOUND, NEQ IF NOT FOUND                                     
*                                                                               
GETEST   NTR1  BASE=ABASE1                                                      
         L     RC,0(R1)                                                         
         L     R7,ABASE2                                                        
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         L     RE,4(R1)                                                         
         MVC   ACEVTYPE,0(RE)                                                   
         MVC   ACEVERS,1(RE)                                                    
         GOTO1 HIGH                                                             
         CLC   ACEVKEY,KEYSAVE                                                  
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A LIST OF ESTIMATES BASED ON PARAMETER PASSED            
*           IN P2                                                               
*                                                                               
* AT ENTRY, P1 = A(GEND)                                                        
*           P2 = A(TYPE/BUFFER)                                                 
*                                                                               
* ON EXIT,  P1 = NUMBER OF ESTIMATES IN LIST                                    
*                                                                               
BLDLST   NTR1  BASE=ABASE1                                                      
         L     R7,ABASE2                                                        
         L     RC,0(R1)                                                         
*                                                                               
         LR    R6,R1               SAVE ADDRESS OF PARAMETER LIST               
         L     R5,4(R1)            GET ADDRESS FOR LIST                         
         MVC   BYTE,4(R1)          SAVE TYPE                                    
         CLI   BYTE,C'P'           TEST 'P' ESTIMATES REQUESTED                 
         BE    *+8                 YES                                          
         MVI   NREVS,0             NO-CLEAR N'REVISIONS                         
*                                                                               
         SR    R3,R3               CLEAR ESTIMATE COUNTER                       
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         B     BLDLST2                                                          
*                                                                               
BLDLST1  LA    R4,KEY                                                           
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 SEQ                                                              
*                                                                               
BLDLST2  CLC   ACEVKEY(ACEVTYPE-ACEVKEY),KEYSAVE                                
         BNE   BLDLST6                                                          
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF)  IS IT A TIME EST RECORD?                   
         BNZ   BLDLST1               YES - SO READ NEXT RECORD                  
*                                                                               
         USING ACKEYD,R4                                                        
BLDLST3  CLI   ACEVTYPE,ACEVREV    IS THIS A REVISION ?                         
         BNE   BLDLST4             NO                                           
         MVC   HIRTYPE,ACEVTYPE    YES, SET TYPE                                
         MVC   HIRONFIL,ACEVERS    UPDATE NUMBER OF ALL REVISIONS               
         TM    ACSTATUS,X'80'      IS RECORD DELETED ?                          
         BO    BLDLST1             YES, ALL FINISHED THEN                       
         MVC   HIRVERS,ACEVERS     NO, UPDATE HIGHEST REVISION                  
         ZIC   R1,NREVS                                                         
         LA    R1,1(R1)            INCREMENT COUNT OF REVISIONS                 
         STC   R1,NREVS                                                         
*                                                                               
BLDLST4  TM    ACSTATUS,X'80'      IS RECORD DELETED ?                          
         BO    BLDLST1             YES, DO NO MORE WITH THIS ONE                
         CLI   BYTE,X'00'          ARE WE LOOKING FOR ALL ESTIMATES ?           
         BE    BLDLST5             YES                                          
         CLC   BYTE,ACEVTYPE       NO, DO TYPES MATCH ?                         
         BNE   BLDLST1             NO, GET NEXT ESTIMATE                        
*                                                                               
BLDLST5  MVC   0(2,R5),ACEVTYPE    ADD TYPE/REVISION NUMBER TO LIST             
         LA    R5,2(R5)                                                         
         LA    R3,1(R3)            BUMP ESTIMATE LIST COUNTER                   
         B     BLDLST1                                                          
*                                                                               
BLDLST6  ST    R3,0(R6)            RETURN NUMBER OF LIST ENTRIES                
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PUT ESTIMATE TABLE FROM BUFF TO TWA 1                          
*                                                                               
PUTTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1            PAGE=TWA 1                                   
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,DMWRT,TEMPSTR,,AESTTAB                              
         MVC   SVNESTTB(8),NESTENT SAVE N'ENTRIES/L'ENTRIES                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET SAVED ESTIMATE TABLE FROM TWA 1                            
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
         ST    RE,AESTTAB                                                       
         MVC   NESTENT(8),SVNESTTB                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
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
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DC    A(0)                                                             
         SPACE 2                                                                
REGTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   REGTIT                                                           
         DC    CL5'SEL'                                                         
         DC    CL16'CLT/PRD/JOB #'                                              
         ORG                                                                    
         SPACE 2                                                                
HEADELS  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(21),AL1(14),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(36),AL1(14),X'2C',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(51),AL1(14),X'2C',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(66),AL1(14),X'2C',AL1(0)          
HEADELS2 DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(21),AL1(14),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(36),AL1(14),X'2C',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(51),AL1(14),X'2C',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(66),AL1(14),X'2C',AL1(0)          
         DC    X'00'                                                            
         SPACE 2                                                                
DATAELS  DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(14),X'20',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(21),AL1(14),X'20',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(36),AL1(14),X'2C',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(51),AL1(14),X'2C',AL1(0)          
         DC    X'02',AL1(TWAELLNQ),AL1(0),AL1(66),AL1(14),X'2C',AL1(0)          
         DC    X'00'                                                            
         SPACE 2                                                                
TOTALELS DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(06),AL1(74),X'20',AL1(0)          
         DC    X'00'                                                            
         SPACE 2                                                                
* TABLE OF OPTION KEYWORDS (COVERED BY OPTTABD)                                 
*                                                                               
         DS    0F                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'CATEGORY'                                                    
         DC    AL1(3,MAXCATS*L'CATOPT),AL1(0,0)                                 
         DC    AL2(CATOPT-SUBSYSD,VALCATG-T60B32)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ZERO'                                                        
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(ZEROPT-SUBSYSD,VALZERO-T60B32)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ROUND'                                                       
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(ROUNDOPT-SUBSYSD,VALRND-T60B32)                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'JT'                                                          
         DC    AL1(2,1),AL1(OPTIYN,0)                                           
         DC    AL2(JTOPT-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'BOXES'                                                       
         DC    AL1(3,1),AL1(0,0)                                                
         DC    AL2(BOXESOPT-SUBSYSD,VALBOX-T60B32)                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'MAXLINES'                                                    
         DC    AL1(4,1),AL1(0,0)                                                
         DC    AL2(MAXLOPT-SUBSYSD,VALMAXL-T60B32)                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'COMMA'                                                       
         DC    AL1(2,1),AL1(OPTIYN,0)                                           
         DC    AL2(COMMAOPT-SUBSYSD,0)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'WCODES'                                                      
         DC    AL1(2,MAXWORK*L'WCOPT),AL1(0,0)                                  
         DC    AL2(WCOPT-SUBSYSD,VALWC-T60B32)                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'SWCODE'                                                      
         DC    AL1(2,2),AL1(0,0)                                                
         DC    AL2(SWOPT-SUBSYSD,VALSW-T60B32)                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DF'                                                          
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(DFTOPT-SUBSYSD,VALDFT-T60B32)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'CROUND'                                                      
         DC    AL1(2,1),AL1(OPTIYN,0)                                           
         DC    AL2(CROPT-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SLUSH    DC    C'&&&&'                                                          
DMWRT    DC    C'DMWRT '                                                        
DMREAD   DC    C'DMREAD'                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
DISMSG   DC    C'** DATA DISPLAYED - ENTER CHANGES **'                          
EDTMSG   DC    C'** DATA CHANGED **'                                            
NONEMSG  DC    C'** NO DATA TO DISPLAY--CHECK HEADLINE FIELDS **'               
JEXJOB   DC    C' (EXP)'                                                        
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PFELS    DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'SELECT  Z=ZOOM  TH,TF=TEXT MAINT HEADER,FOOTER  O=WX        
               C OPTIONS'                                                       
         DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=ALTPFS PF2=JOB OPT PF3=JOB ELIST PF4=JOB SUM PFX        
               5=TEXT MAINT PF6=TEXT LIST'                                      
         DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF7=PREVIOUS PF8=NEXT PF9=DOWNLOAD PF10=PRINT PF11=X        
               EST REPORT PF12=RETURN'                                          
         DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
ALTPFELS DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'SELECT  Z=ZOOM  TH,TF=TEXT MAINT HEADER,FOOTER  O=WX        
               C OPTIONS'                                                       
         DC    X'01',AL1(7+78),AL1(0),AL1(00),AL1(78),X'00',AL1(0)              
         DC    CL78' '                                                          
         DC    X'01',AL1(7+78),AL1(0),AL1(00),AL1(78),X'00',AL1(0)              
         DC    CL78'ALTPFS: PF1=BASE PFS PF2=SESSION EST PF3=OV EST'            
         DC    X'00'                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CALL TALENT SESSION ESTIMATEING                                        
*        DECIDE WHICH VCALL TO MAKE BASED ON                                    
*              1) THE MEDIA OF THE JOB  (RECORD ETV OR ERAD)                    
*              2) WHETHER THE SESSION ESTIMATE EXISTS (ADD OR CHANGE)           
*----------------------------------------------------------------------         
CALLSES  NMOD1 0,CALLSES                                                        
         L     RC,0(R1)                                                         
         SR    R3,R3               EXTRACT LENGTH OF FIRST COL FIELD            
         LA    R1,PROCOL                                                        
         LA    R2,PROCOLH                                                       
         ZIC   R0,5(R2)                                                         
*                                                                               
CALLS05  CLI   0(R1),C' '                                                       
         BE    CALLS10                                                          
         CLI   0(R1),C','                                                       
         BE    CALLS10                                                          
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)            LENGTH COUNTER                               
         BCT   R0,CALLS05                                                       
*                                                                               
         USING COLD,RF                                                          
CALLS10  LA    RF,COLINDS                                                       
         MVI   ERROR,BADEST                                                     
         TM    COLFLAG,COLFEST     IS THIS AN EST                               
         BNO   CALLSERR            NO, ERROR                                    
*                                                                               
         BAS   RE,GETMEDIA         DEFINE,TALMEDIA                              
*                                                                               
         TM    COLFLAG,COLFEAPP    APPROVED                                     
         BNO   CALLS20             NO                                           
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED ESTIMATE                  
         BE    CALLSD              YES, CALL FOR DISPLAY                        
*                                                                               
CALLS20  BAS   RE,CHKSES           SEE IF A SESSION EST EXISTS                  
         BE    CALLSC              YES, CALL WITH ACTION CHANGE                 
*                                                                               
CALLSA   GOTO1 VCALL,WORK,=C'SESSION',=C'ADD',(6,CLICODE),             X        
               (6,PRODCODE),(6,JOBNUM),((R3),PROCOL),(1,TALMEDIA),0             
*                                                                               
         USING SESRECD,R4                                                       
CALLSC   LA    R4,KEY                                                           
*                                                                               
CALLSC30 GOTO1 VCALL,WORK,=C'SESSION',=C'CHA',(6,CLICODE),             X        
               (6,PRODCODE),(6,JOBNUM),((R3),PROCOL),(1,SESKMED),0              
*                                                                               
CALLSD   GOTO1 VCALL,WORK,=C'SESSION',=C'DIS',(6,CLICODE),             X        
               (6,PRODCODE),(6,JOBNUM),((R3),PROCOL),(1,TALMEDIA),0             
*                                                                               
CALLSERR GOTO1 VERRCUR                                                          
CALLSX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*======================================================================         
*        SUPPORTING SUBROUTINES FOR CALL TO SESSION ESTIMATE                    
*======================================================================         
GETMEDIA NTR1                                                                   
*                                                                               
         XC    TALMEDIA,TALMEDIA                                                
*                                                                               
GETM40   TM    JOBJSTAT,JOBSRAD    IS THIS A RADIO JOB                          
         BO    GETMR                                                            
*                                                                               
         TM    JOBJSTAT,JOBSTV     IS THIS A TV JOB                             
         BO    GETMT                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         USING PMDRECD,R6                                                       
         LA    R6,KEY                                                           
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUL                                                      
         MVC   PMDKMED,MEDIA                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(PMDKEND),KEYSAVE                                             
         BNE   CALLSERR                                                         
*                                                                               
         USING PMDELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL                                                         
         TM    PMDSTAT,PMDSTV                                                   
         BO    GETMT                                                            
         TM    PMDSTAT,PMDSRAD                                                  
         BNO   CALLSX                                                           
*                                                                               
GETMR    MVI   TALMEDIA,C'R'                                                    
         B     GETMX                                                            
GETMT    MVI   TALMEDIA,C'T'                                                    
GETMX    B     CALLSX                                                           
*======================================================================         
*        SEE IF A TALENT SESSION ESTIMATE EXISTS                                
*        RETURN NEQ TO ADD                                                      
*======================================================================         
*                                                                               
CHKSES   NTR1                                                                   
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         XC    SESKEY,SESKEY                                                    
         MVI   SESKTYP,SESKTYPQ                                                 
         MVI   SESKSUB,SESKSUBQ                                                 
         MVC   SESKCUL,CUL                                                      
         MVC   SESKCLI,CLICODE                                                  
         MVC   SESKPRO,PRODCODE                                                 
         MVC   SESKJOB,JOBNUM                                                   
*                                                                               
         USING COLD,RF                                                          
         LA    RF,COLINDS                                                       
         MVC   SESKTYPE,COLESTYP   TYPE                                         
         MVC   SESKVER,COLESVER    VERSION                                      
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
CHKS10   CLC   SESKEY((SESKVER-SESKEY)+L'SESKVER),KEYSAVE                       
         BNE   CALLSX              ADD ONE                                      
*                                                                               
         CLI   SESKMED,0           IS THIS AN OLD SESSION                       
         BNE   CHKS40              NO                                           
*                                                                               
         TM    ACCOSTAT(R4),X'80'  DELETED?                                     
         BO    CHKS20              YES, TRY AGAIN                               
*                                                                               
         MVI   RDUPDATE,C'Y'       NO, DELETE THIS SESSION                      
         GOTO1 READ                                                             
         OI    ACCOSTAT(R4),X'80'                                               
         GOTO1 WRITE                                                            
         LTR   RB,RB               AND ADD A NEW ONE (RETURN NEQ)               
         B     CALLSX                                                           
*                                                                               
CHKS20   GOTO1 SEQ                                                              
         B     CHKS10                                                           
*                                                                               
CHKS40   CLI   TALMEDIA,0          ANY MEDIA ON JOB                             
         BE    CALLSX              NO, GO WITH THIS ESTIMATE                    
*                                                                               
         CLC   SESKMED,TALMEDIA    IS THE MEDIA OF THIS SESSION AS              
         BE    CALLSX              DEFINED, YES RETURN EQ                       
*                                                                               
         MVC   SESKMED,TALMEDIA    SEE IF THE SESSION FOR JOB MEDIA             
         GOTO1 HIGH                EXISTS                                       
         CLC   KEY,KEYSAVE                                                      
         B     CALLSX                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO32COM                                                     
* DSECT TO COVER OPTION TABLE KEYWORDS                                          
*                                                                               
OPTTABD  DSECT                     **OPTION TABLE ENTRY**                       
OPTNAME  DS    CL8                 OPTION KEYWORD NAME                          
OPTMINL  DS    X                   MINIMUM INPUT LENGTH                         
OPTLEN   DS    X                   OUTPUT LENGTH                                
OPTIND   DS    X                   INDICATORS                                   
OPTIYN   EQU   X'80'               VALIDATE FOR YES/NO PARM VALUE               
         DS    X                                                                
OPTDISP  DS    XL2                 DISPLACEMENT TO OUTPUT VALUE                 
OPTROUT  DS    XL2                 DISPLACEMENT TO PARM VALUE ROUTINE           
         DS    XL4                 SPARE                                        
OPTTABL  EQU   *-OPTTABD           OPTION TABLE ENTRY LENGTH                    
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
DISPMOD  EQU   X'46'               DISPLAY MODULE NUMBER                        
EDTMOD   EQU   X'43'               EDIT MODULE                                  
COMMOD   EQU   X'42'               COMMAND MODULE                               
TABMOD   EQU   X'50'               TABLE MODULE                                 
         EJECT                                                                  
* DSECT TO COVER JOBBLOCK                                                       
*                                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACPRO32   08/23/07'                                      
         END                                                                    
