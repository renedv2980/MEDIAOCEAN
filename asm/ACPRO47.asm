*          DATA SET ACPRO47    AT LEVEL 047 AS OF 04/11/07                      
*PHASE T60B47A                                                                  
*INCLUDE ACDISCOL                                                               
*INCLUDE RIGHT                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B47 - JOB SUMMARY'                                           
T60B47   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B47**,R7,RR=R2                                              
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    SUM2                                                             
         CLI   MODE,VALREC                                                      
         BE    SUM4                                                             
         B     SUMX                                                             
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
SUM2     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,SETSCR           SET SCREEN ADCONS                            
*                                                                               
         L     RE,=A(OPTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,AOPTTBL                                                       
         L     RE,=V(ACDISCOL)                                                  
         A     RE,RELO                                                          
         ST    RE,VDISCOL                                                       
         L     RE,=V(RIGHT)                                                     
         A     RE,RELO                                                          
         ST    RE,VRIGHT                                                        
*                                                                               
         MVC   ORIGEST,=AL2(JBDORG)                                             
         MVC   ORIGESTG,=AL2(JBDORGG)                                           
         MVC   ORIGESTC,=AL2(JBDORGC)                                           
         MVC   CURREST,=AL2(JBDCUR)                                             
         MVC   CURRESTG,=AL2(JBDCURG)                                           
         MVC   CURRESTC,=AL2(JBDCURC)                                           
         MVC   HIREV,=AL2(JBDHR)                                                
         MVC   HIREVC,=AL2(JBDHRC)                                              
         MVC   HIREVG,=AL2(JBDHRG)                                              
         MVC   REGEST,=AL2(JBDEST)                                              
         MVC   REGESTG,=AL2(JBDESTG)                                            
         MVC   REGESTC,=AL2(JBDESTC)                                            
         MVC   ACTUALS,=AL2(JBDACT)                                             
*                                                                               
SUM3     CLI   RACHANGE,C'Y'       WAS RECORD/ACTION CHANGED ?                  
         BNE   SUM3A               NO                                           
         NI    PROCLIH+4,X'FF'-X'20'  YES, TURN OFF VALIDATE BIT                
         NI    PROPROH+4,X'FF'-X'20'                                            
         NI    PROJOBH+4,X'FF'-X'20'                                            
*                                                                               
SUM3A    BAS   RE,VALHED                                                        
         MVI   INTMODE,FSTLIST     ASSUME FIRST TIME IN                         
         CLI   RETURNED,0          ARE WE BACK FROM SOME PLACE ?                
         BNE   SUMX                YES                                          
         MVI   INTMODE,DISLIST     NO, ASSUME IN MID DISPLAY                    
*                                                                               
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
*                                                                               
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST                                                  
*                                                                               
         B     SUMX                                                             
*                                                                               
* VALREC LOGIC-DISPLAY                                                          
*                                                                               
SUM4     L     RE,AIO3             RESTORE LOCAL WORKING STORAGE                
         LA    RE,1500(RE)                                                      
         ST    RE,AJOBRTAB                                                      
*                                                                               
         LA    RE,BUFF                                                          
         ST    RE,AVECTAB                                                       
*                                                                               
         BAS   RE,LOAD             GET SPACE FOR JOBBER                         
*                                                                               
         BAS   RE,SETSCR           SET SCREEN ADCONS                            
*                                                                               
         BAS   RE,LOOK                                                          
         BAS   RE,BLDVECT                                                       
*                                                                               
         CLI   INTMODE,FSTLIST                                                  
         BE    SUM6                                                             
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
         BAS   RE,PROCPF                                                        
*                                                                               
         CLI   ERROR,0             ERROR IN A PF KEY                            
         BNE   SUMX                YES, CONHEAD WAS SET                         
*                                                                               
         BAS   RE,PROCSEL                                                       
*                                                                               
         CLI   ERROR,0             ERROR IN A SELECT FIELD                      
         BE    SUM8                NO                                           
         B     SUMX                EXIT, CONHEAD WAS SET                        
*                                                                               
*                                                                               
SUM6     BAS   RE,DISNOTE          DISPLAY THE NOTE FIELD                       
*                                                                               
         BAS   RE,DISHEAD          DISPLAY THE COLUMN NAMES                     
*                                                                               
         GOTO1 DISP,PARAS,0        DISPLAY THE ROW DATA                         
         BE    SUM7                                                             
*                                                                               
         MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     SUM15               NOTHING TO DISPLAY-EXIT                      
*                                                                               
SUM7     BAS   RE,DISTOT                                                        
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         MVI   INTMODE,DISLIST     SET CONTINUE LISTING                         
         B     SUM15                                                            
*                                                                               
* CONTINUING LIST                                                               
*                                                                               
SUM8     CLI   PFKEY,PF7           TEST PF7=PREVIOUS PAGE                       
         BE    SUM10                                                            
*                                                                               
         XR    R3,R3                                                            
         LA    R3,MAXDATA                                                       
         C     R3,NVECENT          ENOUGH DATA TO FILL UP A SCREEN?             
         BNL   SUM9                NO, START OVER                               
*                                                                               
         ZIC   R3,SVHI             DO NEXT PAGE                                 
         LA    R3,1(R3)                                                         
         C     R3,NVECENT          TEST IF PAST TABLE                           
         BNH   *+6                 NO                                           
SUM9     XR    R3,R3               YES-WRAP AROUND TO START                     
         B     SUM11                                                            
*                                                                               
SUM10    BAS   RE,BACK             BACK UP TO PREVIOUS PAGE                     
         ZIC   R3,BYTE                                                          
*                                                                               
SUM11    GOTO1 DISP,PARAS,(R3)                                                  
         BE    SUM12                                                            
         MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     SUM15                                                            
*                                                                               
SUM12    BAS   RE,DISTOT                                                        
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         MVI   INTMODE,DISLIST     CONTINUE LISTING                             
*                                                                               
SUM15    L     R2,AOPTIONS                                                      
         ST    R2,ACURFORC                                                      
*                                                                               
         BAS   RE,BUMPTOUN         SEE IF THERE IS AN UNPROTECTED FIELD         
         CLI   0(R2),0             DID I GET ONE                                
         BE    SUMX                NO                                           
         ST    R2,ACURFORC         YES, USE IT                                  
*                                                                               
SUMX     MVI   ERROR,0             CLEAR ERROR FOR NORMAL EXIT                  
         XMOD1 1                                                                
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1  ,                                                                
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   OPTION,0            NO NAMES TO DISPLAY                          
         MVI   KEYCHG,C'N'                                                      
*                                                                               
         LA    R2,PROCLIH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCLI                                                           
*                                                                               
         LA    R2,PROPROH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
*                                                                               
         LA    R2,PROJOBH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   OPTION,C'Y'         GET JOB NAME                                 
         GOTO1 VALJOB                                                           
         MVI   OPTION,0            RESET FOR NEXT                               
*                                                                               
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BZ    *+10                NO                                           
*                                                                               
         MVC   PROJNM+L'PROJNM-L'JSXJOB(L'JSXJOB),JSXJOB                        
*                                                                               
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,JOBSMCSE   JOB UES MCS ESTIMATES                        
         BO    ERREND                                                           
*                                                                               
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,ACJBNEWQ   INSURE JOB USES NEW ESTIMATES                
         BZ    ERREND                                                           
*                                                                               
         L     R0,AIO3                                                          
         ST    R0,AJOB             SAVE THE JOB AT IO3                          
         BAS   RE,SAVEREC                                                       
*                                                                               
         ST    R0,ACOLIST          PUT THE COLUMN LIST AFTER THE JOB            
*                                                                               
         BAS   RE,RDOPT            READ THE JOB'S OPTIONS                       
*                                                                               
         MVI   ERROR,MISSSCH                                                    
         OC    JOBSCH,JOBSCH       TEST THAT JOB HAS A SCHEME                   
         BZ    ERREND              NO                                           
*                                                                               
*                                                                               
* OPTION FIELD EDIT                                                             
*                                                                               
         LA    R2,PROOPTH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   NCATS,0             INITIALIZE FILTER VARIBLES                   
         XC    ZEROPT,ZEROPT                                                    
         XC    DISPOPT,DISPOPT                                                  
         XC    DEFOPT,DEFOPT                                                    
         XC    SENDOPTS,SENDOPTS                                                
         MVI   LSENDOPT,X'00'                                                   
         CLI   5(R2),0             TEST FOR ANY OPTION INPUT                    
         BE    *+8                 NO                                           
         BAS   RE,VALOPT                                                        
*                                                                               
         LA    R2,PROCOLH                                                       
         BAS   RE,TSTKEY                                                        
*                                                                               
         BAS   RE,SETCOLS                                                       
*                                                                               
* COLUMN FIELD EDIT                                                             
*                                                                               
         GOTO1 VJOBCOL,DMCB,(R2),ACOLIST,ACOMFACS                               
         CLI   4(R1),0             TEST IF ERROR                                
         BNE   VALHED11            NO                                           
*                                                                               
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
         BAS   RE,VALEST                                                        
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     COLCUR                                                           
*                                                                               
VALHED20 BAS   RE,SETEDIT                                                       
*                                                                               
*&&US*&& GOTO1 =A(GETBILLS),DMCB,(RC),RR=RELO         GET JOBS BILLS            
*                                                                               
         OI    PROCLIH+4,X'20'     TURN ON PREV VALID BITS                      
         OI    PROPROH+4,X'20'                                                  
         OI    PROJOBH+4,X'20'                                                  
         OI    PROCOLH+4,X'20'                                                  
         OI    PROOPTH+4,X'20'                                                  
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
* ----------------------------------------------------------------*             
* SUB-ROUTINE TO PROCESS PF KEYS                                                
* ----------------------------------------------------------------*             
         SPACE 1                                                                
PROCPF   NTR1  ,                                                                
         CLI   PFKEY,0                                                          
         BE    PROCPFX                                                          
*                                                                               
*                                                                               
PROCPF2  L     RE,ATWA                                                          
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R1,CONACTH          TEST NOTHING CHANGED AFTER ACTION            
         CR    RE,R1                                                            
         BH    PROCPFX                                                          
*                                                                               
PROCPF4  CLI   PFKEY,PF2           TEST FOR PF2=OPTION MAINT                    
         BE    PROCPF6             YES                                          
         CLI   PFKEY,PF3           TEST FOR PF3=JOB ELIST                       
         BE    PROCPF8                                                          
         CLI   PFKEY,PF4           TEST FOR PF5=JOB ESTIMATE                    
         BE    PROCPF9                                                          
         CLI   PFKEY,PF5           TEST FOR PF5=TEXT MAINT                      
         BE    PROCPF10                                                         
         B     PROCPFX             DID NOT RECOGNIZE PFKEY                      
*                                                                               
PROCPF6  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,RECNOPT,ACTNMNT,=C',',=C',',(6,CLICODE),     X        
               (6,PRODCODE),(6,JOBNUM),0                                        
*                                                                               
PROCPF8  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,RECNJOB,ACTNEL,(L'CLICODE,CLICODE),          X        
               (L'PRODCODE,PRODCODE),(L'JOBNUM,JOBNUM),0                        
*                                                                               
PROCPF9  MVI   PFKEY,0                                                          
*                                                                               
         CLI   CALLSP,4            SEE IF THERE IS ROOM IN STACK                
         BNL   PROCPFER            NO, SET ERROR MESSAGE                        
*                                                                               
         BAS   RE,SETESTCL         ONLY PASS 4 COLS TO JOB EST                  
*                                                                               
         BAS   RE,JEST             CALL JOB ESTIMATE                            
*                                                                               
PROCPF10 MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,RECNTEXT,ACTNMNT,=C',',=C',',(6,CLICODE),    X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',=C',',=C',',=C'E',0          
*                                                                               
PROCPFER MVI   PFKEY,0                                                          
         MVC   CONHEAD(L'PF12MSG),PF12MSG                                       
         MVI   ERROR,X'FF'         FLAG FOR CALLER                              
*                                                                               
PROCPFX  B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
* SUB-ROUTINE TO BACK UP TO START ENTRY FOR PREVIOUS PAGE    *                  
* ON EXIT, BYTE CONTAINS START ENTRY NUMBER                  *                  
*------------------------------------------------------------*                  
         SPACE 1                                                                
BACK     NTR1  ,                                                                
         MVI   BYTE,0              INITIALIZE START ENTRY TO ONE                
         SR    R4,R4                                                            
         ICM   R4,1,SVLOW          GET LOW ENTRY NUMBER ON PAGE                 
         BZ    BACKX               NONE                                         
*                                                                               
         SH    R4,=Y(MAXDATA)                                                   
         BM    BACKX                                                            
*                                                                               
         STC   R4,BYTE                                                          
*                                                                               
BACKX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------*                
* SUB-ROUTINE TO DISPLAY THE DATA LINES ON THE SCREEN          *                
*     AT ENTRY, P1=N'START TABLE ENTRY                         *                
*     ON EXIT, CC=EQ AND SVNWCS SET IF SOMETHING TO DISPLAY    *                
*     AND CC=NEQ IF NOTHING TO DISPLAY                         *                
*                                                              *                
* USES R4 AS A POINTER INTO THE VECTOR TABLE                   *                
*      R5 TO POINT TO THE JOBBER TABLE ENTEY                   *                
*      R3 DISPLAY COUNTER                                      *                
*      R2 POINTS TO THE DATA FIELD HEADER OG THE SCREEN ROW    *                
*         YOU ARE DISPLAYING                                   *                
*--------------------------------------------------------------*                
*                                                                               
DISP     NTR1  ,                                                                
         L     R4,0(R1)            GET START ENTRY NUMBER                       
*                                                                               
         GOTO1 VCLEARF,DMCB,AFSTSEL,AJOBTOT    CLEAR UNPROTECTED                
         GOTO1 (RF),(R1),(1,AFSTSEL),AJOBTOT   AND PROTECTED                    
*                                                                               
         OC    NVECENT,NVECENT     TEST FOR ANY ENTRIES                         
         BZ    NOXIT               NONE IN TABLE-SKIP DISPLAY                   
*                                                                               
         MVI   SVNWCS,0            CLEAR COUNT OF SAVED ITEMS TO ZERO           
         STC   R4,SVLOW            CLEAR LOWEST AND HIGHEST ENTRY NUM           
         MVI   SVHI,0                                                           
*                                                                               
         L     R3,NVECENT                                                       
         L     R2,AFSTSEL                                                       
*                                                                               
DISP2    BAS   RE,CONVEC           SET R5 TO THE JOBBER ROW                     
         BNE   DISPX               NO MORE LEFT                                 
         STC   R4,SVHI                                                          
         CLI   SVLOW,0             TEST FIRST ENTRY NOTED                       
         BNE   *+8                 YES                                          
         STC   R4,SVLOW                                                         
*                                                                               
         BAS   RE,SETLIN                                                        
*                                                                               
         BAS   RE,DISDATA          DISPLAY THE DATA FIELDS                      
*                                                                               
         ZIC   R1,SVNWCS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SVNWCS                                                        
*                                                                               
*                                                                               
         CLI   SVNWCS,MAXDATA      TEST FOR FILLED SCREEN                       
         BE    DISPX               YES-ALL DONE                                 
*                                                                               
         L     R2,ANEXTSEL         POINT TO NEXT SCREEN LINE                    
         LA    R4,1(R4)            INCREMENT ENTRY NUMBER                       
         BCT   R3,DISP2                                                         
*                                                                               
DISPX    CLI   SVNWCS,0            TEST FOR ANY WORKCODE LINES                  
         BE    NOXIT               SET CC ON EXIT                               
         B     YESXIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------*                
* SUB-ROUTINE TO DISPLAY THE NOTE FIELD--CALLED FROM DISP--    *                
* ROUTINE SHOWS SCHEME AND MARKS ORIGINAL AND CURRENT ESTIMATE *                
*--------------------------------------------------------------*                
         SPACE 1                                                                
DISNOTE  NTR1  ,                                                                
*                                                                               
         MVC   PRONOTE,SPACES                                                   
         MVC   PRONOTE(7),=C'SCHEME='                                           
         MVC   PRONOTE+7(L'JOBSCH),JOBSCH DISPLAY SCHEME                        
         MVC   PRONOTE+16(4),=C'NAE='                                           
         MVC   PRONOTE+20(1),GONEEDAE SHOW NEED APPROVED EST VALUE              
         GOTO1 SQUASHER,DMCB,PRONOTE,21                                         
         LA    R2,PRONOTEH                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
*&&US                                                                           
         LA    R3,JBBLNET                                                       
         LA    R4,PRONBL                                                        
         BAS   RE,EDITIT           EDIT 0(R3) INTO 0(R4)                        
         LA    R2,PRONBLH                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R3,JBBLGRS                                                       
         LA    R4,PROGBL                                                        
         BAS   RE,EDITIT           EDIT 0(R3) INTO 0(R4)                        
         LA    R2,PROGBLH                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
*&&                                                                             
DISNOTEX B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DISPLAY THE COLUMN HEADINGS                                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
DISHEAD  NTR1  ,                                                                
         PUSH  USING                                                            
         USING JBCLD,R5                                                         
         L     R5,ACOLIST                                                       
         USING DISPD,R6                                                         
         LA    R6,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         ZIC   R3,NCOLS                                                         
*                                                                               
DISHD10  CLI   JBCLTYP,JBCLFRM     IS THIS A FORMULA                            
         BNE   DISHD15             NO                                           
         BAS   RE,DISFORM                                                       
         B     DISHD30                                                          
*                                                                               
DISHD15  LA    R1,HIREVS           SEE IF ITS A HIGHEST REV                     
         LA    R0,NHIREVS                                                       
DISHD16  CLC   JBCLCN1,0(R1)                                                    
         BNE   *+12                                                             
         BAS   RE,FUDGCOLS         FUDGE HR'S TO LOOK LIKE CE, OE OR RN         
         B     DISHD20                                                          
         LA    R1,L'HIREV(R1)                                                   
         BCT   R0,DISHD16                                                       
*                                                                               
DISHD20  GOTO1 VDISCOL,DMCB,(R5),(1,(R6))                                       
*                                                                               
DISHD30  GOTO1 SQUASHER,DMCB,(R6),10                                            
         GOTO1 VRIGHT,DMCB,(R6),10 RIGHT JUSTIFY COL HEADINGS                   
*                                                                               
         LA    R6,11(R6)           NEXT AREA IN LISTAR                          
         LA    R5,JBCLENQ(R5)      NEXT COLUMN                                  
         BCT   R3,DISHD10                                                       
*                                                                               
DISHD40  L     R2,AHEADER                                                       
         OI    4(R2),X'20'                                                      
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DISHDX   B     XIT                                                              
         POP   USING                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE FORMULA EXPRESSION FROM THE COLUMN FIELD               
*    R3 IS N'COLS LEFT                                                          
*    R5 POINTS TO THE COL LIST ENTRY WHICH IS A FORMULA                         
*    R6 POINTS TO THE TEN BYTE OUTPUT AREA                                      
*                                                                               
DISFORM  NTR1  ,                                                                
         LA    R2,PROCOLH          GET FORMULA DATA FROM SCREEN                 
         GOTO1 ANY                 EXTRACT INPUT                                
         LA    RE,WORK             RE=INPUT POINTER                             
         ZIC   R2,NCOLS                                                         
         SR    R2,R3               COMPUTE N`COMMAS TO LOOK FOR                 
         BZ    DISFORM5                                                         
*                                                                               
DISFORM2 CLI   0(RE),C','          TEST FOR A COMMA                             
         BE    DISFORM3                                                         
         LA    RE,1(RE)                                                         
         B     DISFORM2                                                         
*                                                                               
DISFORM3 LA    RE,1(RE)                                                         
         BCT   R2,DISFORM2                                                      
*                                                                               
DISFORM5 CLI   0(RE),C' '          TEST IF A SPACE                              
         BE    DISFORM6            YES-AT END OF FIELD                          
         CLI   0(RE),C','          TEST FOR ANOTHER COMMA                       
         BE    DISFORM6            YES-AT END                                   
*                                                                               
         MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         B     DISFORM5                                                         
*                                                                               
DISFORM6 EQU   *                                                                
*                                                                               
DISFORMX B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
* SUB-ROUTINE TO DISPLAY A ROW OF DATA                                          
* AT ENTRY, R5=A(JOBBER ROW ENTRY TO DISPLAY)                                   
* AT ENTRY, R2=A(FIELD HEADER OF THE SCREEN ROW TO DISPLAY)                     
*------------------------------------------------------------*                  
         SPACE 1                                                                
         USING JBCOLD,R5                                                        
         USING DISPD,R6                                                         
DISDATA  NTR1                                                                   
         L     R2,ASEL                                                          
         NI    1(R2),X'FF'-X'20'   UNPROTECT SELECTS                            
*                                                                               
         L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R6,LISTAR                                                        
         MVC   DISCODE,JBCOLCOD                                                 
*                                                                               
         NI    1(R2),X'FF'-X'08'   LOWLIGHT                                     
         CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   DISD10                                                           
*                                                                               
         BAS   RE,CHKTEXT          ANY TEXT ON WC?                              
         BNE   DISD20                                                           
*                                                                               
         MVI   DISCTFL,C'*'        FLAG WC FOR TEXT                             
         B     DISD20                                                           
*                                                                               
DISD10   CLI   JBCOLTYP,JBCOLTCT   IS THIS A CAT ROW                            
         BE    *+12                YES                                          
         CLI   JBCOLTYP,JBCOLTCA   IS THIS AN AGY COMM CAT                      
         BNE   DISDATAX            NO, DONT DISPLAY ROW                         
*                                                                               
         MVC   DISCODE,JBCOLCAT                                                 
*                                                                               
         MVI   DISCTFL,C'>'        FLAG ROW AS CATAGORY                         
         OI    1(R2),X'08'         HIGHLIGHT                                    
*                                                                               
DISD20   BAS   RE,CODENAME                                                      
         BAS   RE,DISAMNTS                                                      
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DISDATAX B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET THE WORKCODE AND NAME                                      
* AT ENTRY, R5=A(JOB ROW  ENTRY)                                                
* ON EXIT, LISTAR CONTAINS OUTPUT DATA                                          
*                                                                               
CODENAME NTR1  ,                                                                
         USING DISPD,R4                                                         
         USING JBCOLD,R5                                                        
         LA    R4,LISTAR                                                        
*                                                                               
         CLC   JBCOLCAT,SLUSH      TEST FOR SLUSH ITEM                          
         BNE   CODENM3             NO                                           
*                                                                               
         CLI   JBCOLTYP,JBCOLTCT   IS THIS A SLUSH CATAGORY                     
         BNE   CODENM8             NO READ WORKCODE RECORD                      
         MVC   DISDESC,=CL8'MISC.' HARDCODE SLUSH CAT AS MISC.                  
         B     CODENMX                                                          
*                                                                               
CODENM3  CLC   JBCOLCAT,LASTCAT    TEST SAME CATEGORY AS LAST TIME              
         BE    CODENM4                                                          
*                                                                               
         MVC   LASTCAT,JBCOLCAT                                                 
*                                                                               
         LA    R6,KEY                                                           
         USING ACCTKEY,R6                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,JOBSCH                                                   
         MVC   ACCTCODE,JBCOLCAT                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
CODENM4  L     R6,AIO2                                                          
         SR    R0,R0                                                            
         LA    R3,ACRECORD                                                      
*                                                                               
         CLI   JBCOLTYP,JBCOLTCT   IS THIS A CATAGORY?                          
         BE    CODENM9             YES, GET CAT NAME                            
*                                                                               
         CLI   JBCOLTYP,JBCOLTCA   IS THIS AN AGENCT COMMISSION CAT?            
         BE    CODENM9             YES, GET CAT NAME                            
*                                                                               
         USING ACCWD,R3                                                         
*                                                                               
CODENM6  CLI   0(R3),0             TEST FOR EOR                                 
         BE    CODENMX                                                          
         CLI   0(R3),ACCWELQ       TEST FOR WORKCODE ELEM                       
         BNE   CODENM7                                                          
         CLI   ACCWTYPE,1          TEST FOR WORKCODE                            
         BNE   CODENM7                                                          
         CLC   JBCOLWC,ACCWWORK    MATCH ON WORKCODE                            
         BNE   CODENM7                                                          
*                                                                               
         CLI   ACCWLEN,ACCWLNQ1    TEST FOR DESCRIPTION OVERRIDE                
         BE    CODENM8             NO                                           
         MVC   DISDESC,ACCWDESC                                                 
         B     CODENMX                                                          
*                                                                               
CODENM7  IC    R0,ACCWLEN                                                       
         AR    R3,R0                                                            
         B     CODENM6                                                          
*                                                                               
CODENM8  MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),JBCOLWC                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,ACANELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R6                                                       
         MVC   DISDESC,ACANDESC                                                 
         B     CODENMX                                                          
*                                                                               
         USING ACCDD,R3                                                         
CODENM9  CLI   0(R3),0             TEST FOR EOR                                 
         BE    CODENMX                                                          
         CLI   0(R3),ACCDELQ       TEST FOR CAT DESC ELEMENT                    
         BNE   CODENM10                                                         
*                                                                               
         MVC   DISDESC,ACCDNAME                                                 
         B     CODENMX                                                          
*                                                                               
CODENM10 IC    R0,ACCDLEN                                                       
         AR    R3,R0                                                            
         B     CODENM9                                                          
CODENMX  B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*                                                                               
*        DISPLAY THE COLUMN BUCKETS OF THE JOBBER ENTRY AT 0(R5)                
*                                                                               
DISAMNTS NTR1                                                                   
         USING DISPD,R6                                                         
         USING JBCOLD,R5                                                        
         LA    R6,LISTAR                                                        
         ZIC   R2,NCOLS                                                         
         LA    R3,JBCOLVAL         ADDRESS AMOUNTS                              
         LA    R4,DISCOLS                                                       
         USING JBCLD,R5                                                         
         L     R5,ACOLIST          R5=A(COLUMN INDICATORS)                      
*                                                                               
DISA50   BAS   RE,SETEDIT          SET FOR NORMAL EDIT                          
         CLC   JBCLCN1,=Y(JBDRATE) TEST FOR COMMISSION RATE COLUMN              
         BNE   *+8                                                              
         BAS   RE,RATEEDIT         SET EDIT FOR RATES                           
*                                                                               
         BAS   RE,EDITIT           EDIT 0(R3) INTO 0(R4)                        
         LA    R3,L'JBCOLVAL(R3)                                                
         LA    R5,JBCLENQ(R5)                                                   
         LA    R4,11(R4)                                                        
         BCT   R2,DISA50                                                        
         B     XIT                                                              
         DROP  R5,R6                                                            
*                                                                               
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
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        SET THE EDITOR BLOCK                                                   
*        MUST BE (RE)CALLED AFTER EACH VALOPT CALL                              
*--------------------------------------------------------------------           
*                                                                               
SETEDIT  EQU   *                                                                
         MVI   EBLIN,L'JBCOLVAL                                                 
         MVI   EBTIN,C'P'          PACKED                                       
         MVI   EBSCIN,0                                                         
*                                                                               
         MVI   EBLOUT,10                                                        
         MVI   EBDECS,2                                                         
         CLI   ROUNDOPT,C'P'                                                    
         BE    *+12                                                             
         CLI   ROUNDOPT,C'Y'                                                    
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBROUND,2                                                        
*                                                                               
         MVI   EBTRIM,X'80'                                                     
         MVI   EBOPT,X'40'+X'20'   MINUS=Y, ZERO=NOBLANK                        
         BR    RE                                                               
*                                                                               
RATEEDIT EQU   *                   SET EDITOR TO EDIT A RATE                    
         MVI   EBLIN,L'JBCOLVAL                                                 
         MVI   EBTIN,C'P'          PACKED                                       
         MVI   EBSCIN,0                                                         
*                                                                               
         MVI   EBLOUT,10                                                        
         MVI   EBDECS,4                                                         
         MVI   EBROUND,0                                                        
*                                                                               
         MVI   EBTRIM,EBTQROD                                                   
         MVI   EBOPT,EBOQMEY+EBOQZEN MINUS=Y, ZERO=NOBLANK                      
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        SEE IF THE WORKCODE IN THE JOBBER ROW AT 0(R5) HAS TEXT                
*-------------------------------------------------------------------            
CHKTEXT  NTR1                                                                   
         USING JBCOLD,R5                                                        
         LA    R4,KEY              LOOK FOR ANY TEXT UNDER THE WORKCODE         
         USING ACTXKEY,R4                                                       
         XC    ACTXKEY,ACTXKEY                                                  
         MVI   ACTXRTYP,ACTXEQU                                                 
         MVI   ACTXSREC,ACTXSEQU                                                
         MVC   ACTXCUL,CUL                                                      
         MVC   ACTXCLI,CLICODE                                                  
         MVC   ACTXPROD,PRODCODE                                                
         MVC   ACTXJOB,JOBNUM                                                   
         MVC   ACTXWORK,JBCOLWC                                                 
         MVC   ACTXSUFF,JBCOLSUF                                                
         MVI   ACTXFORM,C'E'       FORM=ESTIMATE                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   ACTXKEY(ACTXWHER-ACTXKEY),KEYSAVE                                
*                                                                               
*        PASS BACK CONDITION CODE                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CALL EDITOR                                                    
*        R3 IS A(PACKED FIELD TO EDIT)                                          
*        R4 IS A(FIELD TO EDIT IT INTO)                                         
*        R5 IS A(COL LIST ENTRY)                                                
*                                                                               
         USING JBCLD,R5                                                         
EDITIT   NTR1                                                                   
         ST    R3,EBAIN                                                         
*                                                                               
         CLI   ROUNDOPT,C'Y'       ROUNDING?                                    
         BE    EDITIT20            YES, DON'T WORRY ABOUT SHIFT                 
*                                                                               
         CP    0(6,R3),=P'99999999'     SEE IF I CAN SHIFT NOT ROUND            
         BNH   EDITIT20                                                         
*                                                                               
         CP    0(6,R3),=P'999999999'                                            
         BH    EDITIT20                                                         
         LA    R4,1(R4)                                                         
         NI    EBTRIM,X'FF'-EBTQROD TURN OFF TRIM SO I PRINT TO LEFT            
         NI    EBOPT,X'FF'-EBOQMEY  TURN OFF MINUS=Y                            
         MVI   EBLOUT,9            MAKE LENGTH 9                                
*                                                                               
EDITIT20 ST    R4,EBAOUT                                                        
         GOTO1 EDITOR,PARAS,EBLOCK                                              
*                                                                               
         CLI   EBLOUT,9            DID I DO THE STUPID SHIFT                    
         BNE   EDITIT30            NO, I'M OK                                   
*                                                                               
         OI    EBTRIM,EBTQROD      TURN TRIM BACK ON                            
         OI    EBOPT,EBOQMEY       TURN ON MINUS=Y                              
*                                  DON'T RESTORE LENGTH UNTILL LATER            
         BCTR  R4,0                RESTORE R4                                   
*                                                                               
EDITIT30 CLC   JBCLCN1,=Y(JBDRATE) TEST FOR COMMISSION RATE COLUMN              
         BE    EDITX               YES, FORGET THIS ROUND=P CRAP                
*                                                                               
         CLI   ROUNDOPT,C'P'       ROUND=P                                      
         BNE   EDITX               NO                                           
*                                                                               
         LA    R1,2                NEED 3 CHEAR SPACES                          
         CLC   0(3,R4),SPACES      ROOM TO PUT PENNIES BACK                     
         BNE   EDITX               NO                                           
*                                                                               
         CLI   EBLOUT,9            DID I SHIFT                                  
         BNE   EDITIT40            NO                                           
*                                                                               
         MVC   0(8,R4),3(R4)       SHIFT ROUNDED NUMBER LEFT                    
         MVC   7(3,R4),=C'.00'     PUT BACK THEIR STINKING PENNIES              
         MVI   EBLOUT,10           RESTORE PROPER LENGTH                        
         B     EDITX                                                            
*                                                                               
EDITIT40 MVC   0(7,R4),3(R4)       SHIFT ROUNDED NUMBER LEFT (LEAVE -)          
         MVC   6(3,R4),=C'.00'     PUT BACK THEIR STINKING PENNIES              
*                                                                               
EDITX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE JOB TOTALS LINE                                    
*                                                                               
         USING JBCLD,R5                                                         
DISTOT   NTR1                                                                   
         ZIC   R2,NCOLS            R3=LOOP COUNTER                              
         LA    R3,SVJOBTOT         R5=A(JOB TOTALS)                             
         MVC   LISTAR,SPACES                                                    
         USING DISPD,R4                                                         
         LA    R4,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         L     R5,ACOLIST                                                       
*                                                                               
DISTOT2  CLC   JBCLCN1,=Y(JBDRATE) IS THIS A RATE                               
         BE    DISTOT4             DONT PRINT                                   
*                                                                               
         BAS   RE,SETEDIT                                                       
*                                                                               
         BAS   RE,EDITIT                                                        
*                                                                               
DISTOT4  LA    R4,11(R4)           NEXT OUTPUT POSITION                         
         LA    R3,L'SVJOBTOT(R3)                                                
         LA    R5,JBCLENQ(R5)                                                   
         BCT   R2,DISTOT2                                                       
*                                                                               
         L     R2,ATOTALS          R2=A(FIELD HEADER)                           
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DISTOTX  B     XIT                                                              
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        SET SCREEN ADCONS                                                      
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PROHEADH                                                      
         ST    R2,AHEADER                                                       
*                                                                               
         LA    R2,PROOPTH                                                       
         ST    R2,AOPTIONS                                                      
*                                                                               
         LA    R2,PROCOLH                                                       
         ST    R2,ACOLS                                                         
*                                                                               
         LA    R2,PROSFH                                                        
         ST    R2,AFSTSEL                                                       
*                                                                               
         LA    R2,PRODFH                                                        
         ST    R2,AFSTDATA                                                      
*                                                                               
         LA    R2,PRODLH                                                        
         ST    R2,ALSTDATA                                                      
*                                                                               
         LA    R2,PROTOTH                                                       
         ST    R2,ATOTALS                                                       
*                                                                               
         LA    R2,PROJOBTH                                                      
         ST    R2,AJOBTOT                                                       
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         ST    R2,ASEL                                                          
         BAS   RE,BUMP                                                          
         ST    R2,ADATA                                                         
         BAS   RE,BUMP                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANY SELECT FIELDS TO ZOOM                             
*        I READ THE W/C TO ZOOM TO OFF THE SCREEN                               
*                                                                               
PROCSEL  NTR1                                                                   
         SR    R3,R3                                                            
         ICM   R3,1,SVNWCS       R3=LOOP COUNTER                                
*                                                                               
         BZ    PROCSELX                                                         
         L     R2,AFSTSEL                                                       
*                                                                               
PROCSEL2 BAS   RE,SETLIN                                                        
*                                                                               
         L     R2,ASEL                                                          
*                                                                               
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    PROCSEL9                                                         
*                                                                               
         CLI   8(R2),C'*'          TEST FOR PREVOIUS                            
         BE    PROCSEL9                                                         
*                                                                               
         CLI   8(R2),C'Z'          TEST FOR ZOOM                                
         BNE   PROCSEL3                                                         
*                                                                               
         MVI   8(R2),C'*'          SET ALREADY PROCESSED                        
         USING DISPD,R4                                                         
         L     R2,ADATA            GET DATA FIELD                               
         LA    R4,8(R2)                                                         
*                                                                               
         CLI   DISCTFL,C'>'        IS THIS A CAT?                               
         BNE   PROCSELZ                                                         
         MVC   CONHEAD(L'NOCATS),NOCATS                                         
         L     R2,ASEL             FLAG AN ERROR HERE                           
         OI    6(R2),X'40'         PUT CURSOR HERE                              
         MVI   ERROR,X'FF'         FLAG FOR CALLER                              
         B     PROCSELX                                                         
*                                                                               
PROCSELZ CLI   CALLSP,4            SEE IF THERE IS ROOM IN STACK                
         BNL   PROCSLER            NO, SET ERROR MESSAGE                        
*                                                                               
         GOTO1 VCALL,WORK,RECNJOB,ACTNDET,(6,CLICODE),(6,PRODCODE),    X        
               (6,JOBNUM),(2,DISCODE),(1,DFTOPT),0                              
*                                                                               
PROCSEL3 CLI   8(R2),C'H'          TEXT MAINT HEADER                            
         BE    PROCSEL5                                                         
*                                                                               
PROCSEL4 CLI   8(R2),C'F'          TEXT MAINT FOOTER                            
         BE    PROCSEL5                                                         
*                                                                               
         MVI   ERROR,INVALID       INPUT MUST BE INVALID                        
         B     ERREND                                                           
*                                                                               
PROCSEL5 MVC   WHERE,SPACES                                                     
         MVC   WHERE(1),8(R2)                                                   
         MVI   8(R2),C'*'          SET ALREADY PROCESSED                        
*                                                                               
         CLI   CALLSP,4            SEE IF THERE IS ROOM IN STACK                
         BNL   PROCSLER            NO, SET ERROR MESSAGE                        
*                                                                               
         L     R2,ADATA            GET DATA FIELD                               
         LA    R4,8(R2)                                                         
*                                                                               
         CLI   DISCTFL,C'>'        IS THIS A CAT CALL                           
         BNE   PROCSEL6                                                         
*                                  CALL TXT MNT W/ A CAT                        
         GOTO1 VCALL,WORK,RECNTEXT,ACTNMNT,=C',',=C',',(6,CLICODE),    X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',(2,DISCODE),=C',',  X        
               =C'E',(L'WHERE,WHERE),0                                          
*                                                                               
PROCSEL6 GOTO1 VCALL,WORK,RECNTEXT,ACTNMNT,=C',',=C',',(6,CLICODE),    X        
               (6,PRODCODE),(6,JOBNUM),=C',',=C',',=C',',(2,DISCODE),  X        
               =C'E',(L'WHERE,WHERE),0                                          
*                                                                               
PROCSEL9 L     R2,ANEXTSEL                                                      
         BCT   R3,PROCSEL2                                                      
         B     PROCSELX                                                         
*                                                                               
PROCSLER MVI   PFKEY,0                                                          
         MVC   CONHEAD(L'PF12MSG),PF12MSG                                       
         MVI   ERROR,X'FF'         FLAG FOR CALLER                              
*                                                                               
PROCSELX B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
* SUB-ROUTINE TO CALL JOB ESTIMATE, MODIFY THE COLUMNS FIELD     *              
*    AND THEN CALL GENCON                                        *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
JEST     NTR1  WORK=(R6,JEWORKL)                                                
         USING JEWORKD,R6                                                       
         MVC   JESYSRD,SYSRD       SAVE SYSRD                                   
         LA    RE,JESAVE           RE=A(SAVE STORAGE)                           
         LA    RF,LOCALLN          RF=L'LOCAL WORKING STORAGE                   
         LR    R1,RF                                                            
         LA    R0,LOCAL                                                         
         MVCL  RE,R0               SAVE LOCAL STORAGE                           
*                                                                               
         MVI   PFKEY,0                                                          
         L     RF,=V(DUMMY)                                                     
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY         LOAD IN ELIST AFTER THIS MODULE              
         BAS   RE,JESETRD          SET RETURN POINT HERE                        
*                                                                               
         GOTO1 VCALL,WORK,RECNJOB,ACTNEST,(L'CLICODE,CLICODE),         X        
               (L'PRODCODE,PRODCODE),(L'JOBNUM,JOBNUM),0                        
*                                                                               
         LA    R2,CONTAG           FIRST FIELD ON JOB EST SCREEN                
         SH    R2,=H'8'            BACK UP TO HEADER                            
         SR    R1,R1                                                            
         LA    R1,8                                                             
         BAS   RE,BUMP             BUMP TO COL FIELD                            
         BCT   R1,*-4                                                           
*                                                                               
         MVC   5(1,R2),LSENDCOL    SET NEW FIELD LENGTH                         
         NI    4(R2),X'FF'-X'20'   UNVALIDATE FIELD                             
         LA    R3,8(R2)                                                         
         MVC   0(L'SENDCOLS,R3),SENDCOLS                                        
*                                                                               
         BAS   RE,BUMP             BUMP TO OPTIONS FIELD                        
         BAS   RE,BUMP                                                          
         MVC   5(1,R2),LSENDOPT    SET NEW FIELD LENGTH                         
         NI    4(R2),X'FF'-X'20'   UNVALIDATE FIELD                             
         LA    R3,8(R2)                                                         
         MVC   0(L'SENDOPTS,R3),SENDOPTS                                        
*                                                                               
         MVI   GOAGAIN,C'N'        TURN OFF GENCON CALL FLAG                    
         MVC   SYSRD,JESYSRD                                                    
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
         DC    H'0'                SHOULD NOT COME BACK                         
*                                                                               
         DROP  R6                                                               
         SPACE 2                                                                
JESETRD  NTR1  ,                                                                
         ST    RD,SYSRD                                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LOAD IN THE JOBBER TABLES                                      
*                                                                               
LOAD     ST    RE,SAVERE                                                        
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        LOAD IN JOBBER TABLE PHASE                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LM    R0,R1,0(RF)                                                      
         AR    R0,RF               FORM ADDRESS OF COLUMN TABLE                 
         STM   R0,R1,ACOLTAB                                                    
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
* SUB-ROUTINE TO LOOK UP JOB VALUES FOR THE SCREEN               *              
*                                                                *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
LOOK     NTR1  ,                                                                
         L     R6,AIO3                                                          
         LA    R6,1500(R6)                                                      
         ST    R6,AJOBRTAB                                                      
         USING JBLOCKD,R6                                                       
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
         MVC   JBGETDFT,DFTOPT     PASS DRAFT OPTION                            
         MVC   JBACOLTB(16),ACOLTAB COLUMN/OPERAND VALUE TABLES                 
*                                                                               
LOOK2    GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0           TEST FOR ERROR                               
         BE    LOOKX                                                            
*                                                                               
         CLI   JBERROR,JBERRCOL    ERROR WITH COLUMN OR OPERAND?                
         BE    LOOK3               YES, PRINT MESSAGE                           
         CLI   JBERROR,JBERROPV                                                 
         BE    LOOK3                                                            
*                                                                               
         CLI   JBERROR,JBERRSCH    TEST FOR SCHEME ERROR                        
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
*                                                                               
         MVI   ERROR,BADSCH        INVALID SCHEME                               
         LA    R2,PROJOBH                                                       
         B     ERREND                                                           
*                                                                               
LOOK3    MVI   ERROR,COOPERR       COLUMN/OPERAND TABLE ERROR                   
         LA    R2,PROCOLH                                                       
         B     ERREND                                                           
*                                                                               
LOOKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SELECT ROWS FROM JOBBERS TABLE                                         
*                                                                               
BLDVECT  NTR1                                                                   
         USING JBLOCKD,R6                                                       
         L     R6,AJOBRTAB         A(JOBBER BLOCK)                              
         LA    R4,BUFF             R4=A(VECTOR TABLE)                           
         ST    R4,AVECTAB                                                       
         ZIC   RE,JBNCOLS                                                       
         XC    NVECENT,NVECENT                                                  
         MVI   GOTSTART,C'N'                                                    
         L     R5,JBACOLTB         R5=A(COLUMN VALUE TABLE)                     
         USING JBCOLD,R5                                                        
         LH    R2,JBNROWS          R2=LOOP COUNTER                              
         LH    R3,JBLCOL           R3=L'COLUMN TABLE ENTRY                      
*                                                                               
*LDV6    CLC   JBCOLCAT,SLUSH      TEST FOR SLUSH ITEM                          
*        BE    BLDV10                                                           
*                                                                               
BLDV6    CLI   JBCOLTYP,JBCOLTWC   WORKCODES                                    
         BE    BLDV7                                                            
*                                                                               
         CLI   JBCOLTYP,JBCOLTCA   AGENCY COMMISSION CATAGORY                   
         BE    BLDV7                                                            
*                                                                               
         CLI   JBCOLTYP,JBCOLTCT   OR CATAGORIES ONLY                           
         BNE   BLDV10                                                           
*                                                                               
*                                                                               
BLDV7    BAS   RE,FILCAT                                                        
         BNE   BLDV10                                                           
*                                                                               
         BAS   RE,FILZERO                                                       
         BNE   BLDV10                                                           
*                                                                               
         BAS   RE,FILDISP                                                       
         BNE   BLDV10                                                           
*                                                                               
         BAS   RE,FILWC                                                         
         BNE   BLDV10                                                           
*                                                                               
         BAS   RE,FILSW                                                         
         BNE   BLDV10                                                           
*                                                                               
         L     RE,NVECENT          INCREMENT VECTOR TABLE COUNT                 
         LA    RE,1(RE)                                                         
         ST    RE,NVECENT                                                       
*                                                                               
         ICM   RE,3,JBCOLROW       SAVE THIS ROW NUM AS OFFSET                  
         BCTR  RE,0                                                             
         STCM  RE,3,0(R4)                                                       
         LA    R4,2(R4)                                                         
*                                                                               
BLDV10   LA    R5,0(R3,R5)         NEXT COLUMN ENTRY                            
         BCT   R2,BLDV6                                                         
*                                                                               
         L     R5,JBACOLTB         R5=A(COLUMN TABLE)                           
         ZIC   R1,JBNCOLS          JOB TOTALS ARE FIRST ENTRY                   
         MH    R1,=Y(L'JBCOLVAL)                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVJOBTOT(0),JBCOLVAL EXTRACT JOB TOTALS                          
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO CONVERT THE VECTOR TABLE OFFSET IN R4 INTO                     
*        THE ADDRESS OF THE JOBBER ROW IN R5                                    
*        USES R0, R1, RE, RF                                                    
*                                                                               
CONVEC   ST    RE,SAVERE                                                        
         ST    R4,FULL                                                          
         C     R4,NVECENT                                                       
         BNL   CONVNO                                                           
*                                                                               
         LR    R1,R4               OFFSET INTO R1                               
         MH    R1,=Y(VECLENQ)      OFFSET TIMES LEN OF ENTRY                    
         A     R1,AVECTAB                                                       
         SR    R5,R5                                                            
         ICM   R5,3,0(R1)          R5 HAS COL NUMBER                            
         USING JBLOCKD,RF                                                       
         L     RF,AJOBRTAB                                                      
         LH    RE,JBLCOL                                                        
         MR    R4,RE               R5 HAS OFFSET                                
         A     R5,ACOLTAB                                                       
         USING JBCOLD,R5                                                        
         OC    JBCOLTYP,JBCOLTYP                                                
         BZ    CONVNO                                                           
*                                                                               
CONVYES  CR    RB,RB                                                            
         B     CONVX                                                            
*                                                                               
CONVNO   MVI   SVHI,X'FF'          FORCE NEW DISPLAY, NEXT TIME                 
         LTR   RB,RB                                                            
*                                                                               
CONVX    L     R4,FULL             RESTORE COUNT REGISTER                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
* ---------------------------------------------------------------------         
* SUB-ROUTINE TO FILTER AGAINST CATEGORY LIST                                   
* CALLED WHILE BUILDING VECTOR TABLE                                            
* ON ENTRY, R5 POINTS TO THE JOBBER TABLE ENTRY YOU WISH TO TEST                
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT CATEGORY                               
* ---------------------------------------------------------------------         
*                                                                               
         USING JBCOLD,R5                                                        
FILCAT   CLI   NCATS,0                                                          
         BER   RE                                                               
         ZIC   R0,NCATS                                                         
         LA    R1,CATOPT                                                        
         TM    CATOPT,X'40'        TEST POSITIVE FILTER                         
         BZ    FILCAT2             NO                                           
*                                                                               
FILCAT1  CLC   JBCOLCAT,0(R1)                                                   
         BE    FILCATY                                                          
         LA    R1,L'JBCOLCAT(R1)                                                
         BCT   R0,FILCAT1                                                       
         B     FILCATN                                                          
*                                                                               
FILCAT2  MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   JBCOLCAT,HALF                                                    
         BE    FILCATN                                                          
         LA    R1,L'JBCOLCAT(R1)                                                
         BCT   R0,*-14                                                          
*                                                                               
FILCATY  CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILCATN  LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
* ---------------------------------------------------------------------         
* SUB-ROUTINE TO APPLY ZERO OPTION FILTER                                       
* CALLED WHILE BUILDING VECTOR TABLE                                            
* ON ENTRY, R5 POINTS TO THE JOBBER TABLE ENTRY YOU WISH TO TEST                
* ON EXIT, CC=EQ TO ACCEPT ENTRY, CC=NEQ TO REJECT ENTRY                        
* ---------------------------------------------------------------------         
*                                                                               
         USING JBCOLD,R5                                                        
FILZERO  CLI   ZEROPT,C'S'         TEST FOR ZERO SUPPRESSION                    
         BE    *+12                YES                                          
         CLI   ZEROPT,C'A'         TEST SPECIAL ZERO SUPPRESS OPTION            
         BNE   FILZEROY            NO-TAKE LINE                                 
*                                                                               
         USING JBCLD,RF                                                         
         ZIC   R0,NCOLS                                                         
         LA    R1,JBCOLVAL         R1=A(WORKCODE VALUES)                        
         L     RF,ACOLIST          RF=A(COLUMN INDICATORS)                      
FILZERO2 CLC   JBCLCN1,=Y(JBDRATE) TEST FOR COMMISSION RATE COLUMN              
         BE    *+14                YES-SKIP ZERO TEST                           
         CP    0(L'JBCOLVAL,R1),=P'0'                                           
         BNE   FILZEROY            FOUND A NON-ZERO ENTRY                       
         LA    R1,L'JBCOLVAL(R1)                                                
         LA    RF,JBCLENQ(RF)                                                   
         BCT   R0,FILZERO2                                                      
*                                                                               
FILZERON LTR   RB,RB               SET CC=NEQ FOR ZERO LINE                     
         BR    RE                                                               
*                                                                               
FILZEROY CR    RB,RB                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
* ---------------------------------------------------------------------         
* SUB-ROUTINE TO FILTER W/C'S OR CATEGORIES FROM SCREEN                         
* CALLED WHILE BUILDING VECTOR TABLE                                            
* ON ENTRY, R5 POINTS TO THE JOBBER TABLE ENTRY YOU WISH TO TEST                
* ON EXIT, CC=EQ TO ACCEPT ENTRY, CC=NEQ TO REJECT ENTRY                        
* ---------------------------------------------------------------------         
*                                                                               
         USING JBCOLD,R5                                                        
FILDISP  CLI   DISPOPT,C'W'        TEST FOR WORKCODES ONLY                      
         BE    FILDW               YES                                          
*                                                                               
         CLI   DISPOPT,C'C'        TEST CATS ONLY                               
         BNE   FILDISPY            NO-TAKE LINE                                 
*                                                                               
         CLI   JBCOLTYP,JBCOLTCT   CAT ROW                                      
         BE    FILDISPY            YES                                          
*                                                                               
         CLI   JBCOLTYP,JBCOLTCA   AGENCY COMMISSION CATAGORY?                  
         BE    FILDISPY            YES                                          
*                                                                               
         B     FILDISPN                                                         
*                                                                               
FILDW    CLI   JBCOLTYP,JBCOLTWC   W/C ROW                                      
         BE    FILDISPY            YES                                          
*                                                                               
FILDISPN LTR   RB,RB               SET CC=NEQ FOR ZERO LINE                     
         BR    RE                                                               
*                                                                               
FILDISPY CR    RB,RB                                                            
         BR    RE                                                               
* SUB-ROUTINE TO FILTER AGAINST WORKCODE LIST                                   
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
*                                                                               
         USING JBCOLD,R5                                                        
FILWC    CLI   NWORK,0                                                          
         BER   RE                                                               
         ZIC   R0,NWORK                                                         
         LA    R1,WCOPT                                                         
         TM    WCOPT,X'40'         TEST POSITIVE FILTER                         
         BZ    FILWC2              NO                                           
*                                                                               
FILWC1   CLC   JBCOLWC,0(R1)                                                    
         BE    FILWCY                                                           
         LA    R1,L'JBCOLWC(R1)                                                 
         BCT   R0,FILWC1                                                        
         B     FILWCN                                                           
*                                                                               
FILWC2   MVC   HALF,0(R1)          EXCLUDE A WORKCODE                           
         OI    HALF,X'40'                                                       
         CLC   JBCOLWC,HALF                                                     
         BE    FILWCN                                                           
         LA    R1,L'JBCOLWC(R1)                                                 
         BCT   R0,FILWC2                                                        
*                                                                               
FILWCY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILWCN   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER AGAINST WORKCODE START                                  
* AT ENTRY, R5=JOBBER COL TABLE                                                 
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
* SET GOTSTART TO YES TO START BUILDING VECTOR TAB                              
*                                                                               
FILSW    CLI   SWOPT,0             START WC                                     
         BER   RE                                                               
         USING JBCOLD,R5                                                        
         CLI   GOTSTART,C'Y'       DID WE FIND START ALREADY ?                  
         BE    FILSWY              YES, OK                                      
*                                                                               
         CLC   JBCOLWC,SWOPT                                                    
         BNE   FILSWN              NOT YET                                      
*                                                                               
         MVI   GOTSTART,C'Y'    START LISTING WORKCODES                         
*                                                                               
FILSWY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILSWN   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
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
         MVC   JOBSCH,GOSCHEME     EXTRACT SCHEME VALUE                         
*                                                                               
RDOPTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
* SUB-ROUTINE TO VALIDATE THE OPTION FIELDS--CALLED FROM VALHED  *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
VALOPT   NTR1                                                                   
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
         L     R5,AOPTTBL                                                       
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
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
*                                                                               
VALOPT30 LA    R4,PSNL(R4)         NEXT PARSNIP ENTRY                           
         BCT   R2,VALOPT10                                                      
*                                                                               
         MVC   SENDOPTS,PROOPT     SAVE PROOPT                                  
         MVC   WORK(2),=C'DE'      GET RID OF DEF=                              
         BAS   RE,DELOPT                                                        
*                                                                               
         MVC   WORK(2),=C'DI'      GET RID OF DIS=                              
         BAS   RE,DELOPT                                                        
*                                                                               
         XC    SENDOPTS,PROOPT     SWAP SENDOPTS AND PROOPT                     
         XC    PROOPT,SENDOPTS                                                  
         XC    SENDOPTS,PROOPT                                                  
*                                                                               
         GOTO1 SQUASHER,DMCB,SENDOPTS,(C',',L'SENDOPTS)                         
         L     RE,DMCB+4                                                        
         STC   RE,LSENDOPT                                                      
*                                                                               
*        TURN OFF DEF=Y IF LAST FIELD MODIFIED IS THE COL LIST                  
*                                                                               
         CLI   DEFOPT,C'Y'         DEFAULT=Y SET                                
         BNE   VALOPTX             NO                                           
*                                                                               
         BAS   RE,CHKOPT           SEE IF I NEED IT                             
         BNE   VALOPTX             YES, KEEP IT                                 
*                                                                               
         MVI   DEFOPT,C'N'         TURN IT OFF                                  
         MVC   WORK(2),=C'DE'                                                   
         BAS   RE,DELOPT                                                        
*                                                                               
         GOTO1 SQUASHER,DMCB,PROOPT,(C',',L'PROOPT)                             
         LA    R2,PROOPTH                                                       
         L     R1,DMCB+4                                                        
         STC   R1,5(R2)            SAVE LENGTH IN FIELD HEADER                  
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VALOPTX  XIT1                                                                   
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
         EX    R1,YESCOMP          ROUND=Y,N,O                                  
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
* VALIDATE DISPLAY OPTION                                                       
*                                                                               
VALDIS   ST    RE,SAVERE                                                        
         CLI   PSNLEN,8            TEST FOR BIG FIELD                           
         BH    VALOPTR                                                          
         MVC   0(1,R6),0(R3)       COPY IN FIRST BYTE                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,WRKCOMP          TEST FOR W(ORKCODE)                          
         BE    VALDISX                                                          
         EX    R1,CATCOMP          TEST FOR C(ATAGORY)                          
         BE    VALDISX                                                          
         B     VALOPTR                                                          
*                                                                               
VALDISX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
WRKCOMP  CLC   0(0,R3),=C'WORKCODE'                                             
CATCOMP  CLC   0(0,R3),=C'CATAGORY'                                             
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
* ROUTINE TO VALIDATE MAXLINES OVERRIDE                                         
*                                                                               
* ERROR EXIT WITH CURSOR POSITIONING                                            
*                                                                               
VALOPTR  LA    R2,PROOPTH          SET R2=A(OPTION FLDH)                        
         LA    RE,8(R2)            START OF FIELD                               
         SR    R3,RE               COMPUTE DISPLACEMENT INTO FIELD              
         STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         SPACE 2                                                                
*                                                                               
VALOPTW  LA    R2,PROOPTH          SET R2=A(OPTION FLDH)                        
         LA    RE,8(R2)            START OF FIELD                               
         SH    R4,=Y(PSNL)         COMPUTE DISPLACEMENT INTO FIELD              
         L     R3,PSNCOMP                                                       
         SR    R3,RE                                                            
         STC   R3,ERRNDX                                                        
         MVI   ERROR,OPTCNFLT                                                   
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
*                                                                               
*        REMOVE AN OPTION FROM THE LIST                                         
*        THE 2 CHARACTER OPTION IS PASSED IN WORK                               
*                                                                               
DELOPT   NTR1                                                                   
         LA    R4,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         USING PSND,R4                                                          
*                                                                               
DELOPT40 CLI   PSNTAG,PSNFLDQ                                                   
         BNE   DELOPT50                                                         
*                                                                               
         L     R3,PSNCOMP          FIND THE OPTION PASSED HERE                  
         CLC   0(2,R3),WORK                                                     
         BE    DELOPT60                                                         
*                                                                               
DELOPT50 LA    R4,PSNL(R4)                                                      
         BCT   R0,DELOPT40                                                      
         B     DELOPTX             NOT FOUND                                    
*                                                                               
DELOPT60 ZIC   R1,PSNLEN           SAVE KEY LEN                                 
         LA    R1,1(R1)            BUMP PAST =                                  
         LA    R4,PSNL(R4)                                                      
         ZIC   R0,PSNLEN           SAVE ATTR LEN                                
         AR    R1,R0               ACCUMULATE LENGTH                            
*                                                                               
         BCTR  R3,0                LOOK FOR LEADING COMMA                       
         CLI   0(R3),C','                                                       
         BNE   DELOPT70                                                         
         LA    R1,1(R1)            BUMP LEN                                     
         B     *+8                                                              
*                                                                               
DELOPT70 LA    R3,1(R3)                                                         
*                                                                               
         LR    R4,R3               LOOK FOR TRAILING COMMA                      
         AR    R4,R1                                                            
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         LA    R1,1(R1)            BUMP LENGTH TO INCLUDE TRAILING ,            
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      SPACE OUT THE OPTION                         
*                                                                               
DELOPTX  B     XIT                                                              
         EJECT                                                                  
SETCOLS  ST    RE,SAVERE                                                        
         LA    R2,PROCOLH                                                       
         LA    R1,L'DEFCOLS                                                     
         LA    RE,DEFCOLS                                                       
         CLI   DEFOPT,C'Y'         FORCE DEFAULT                                
         BE    SETC50                                                           
*                                                                               
         CLI   RACHANGE,C'Y'       REC/ACT CHANGED?                             
         BE    SETC20              YES, RESET COLS                              
*                                                                               
         CLI   INTMODE,DISLIST     IN MID DISPLAY?                              
         BE    SETCOLX             YES, LEAVE COLS                              
*                                                                               
         CLI   5(R2),0             ANYTHING THERE                               
         BNE   SETCOLX             YES, LEAVE COLS                              
*                                                                               
SETC20   MVC   WORK,SPACES                                                      
         MVC   WORK(L'GOJSUM),GOJSUM                                            
         MVC   WORK+L'GOJSUM+1(L'GOJSUMC),GOJSUMC                               
*                                                                               
         CLC   WORK,SPACES         ANYTHING IN OPTIONS                          
         BNH   SETC50              NO, USE DEFAULT                              
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(C',',L'WORK)                                 
         L     R1,DMCB+4                                                        
         LA    RE,WORK                                                          
*                                                                               
         CLM   R1,1,=AL1(L'PROCOL) LENGTH TOO LONG                              
         BNH   *+8                 NO                                           
         LA    R1,L'PROCOL                                                      
*                                                                               
         LA    R0,6                MAKE SURE THERE ARE ONLY 6 COLS              
         LA    R3,WORK                                                          
         XR    R4,R4                                                            
*                                                                               
SETC30   CLI   0(R3),C','                                                       
         BE    SETC40                                                           
         CR    R4,R1               AM I AT THE MAXIMUM LENGTH                   
         BE    SETC50              STOP                                         
*                                                                               
SETC35   LA    R4,1(R4)            LENGTH CHECKED SO FAR                        
         LA    R3,1(R3)            NEXT POSITION IN WORK                        
         B     SETC30                                                           
*                                                                               
SETC40   BCT   R0,SETC35           COMMA COUNTER                                
*                                                                               
         LR    R1,R4               SAVE NEW LENGTH (TO MOVE)                    
*                                                                               
SETC50   MVC   PROCOL,SPACES                                                    
         NI    4(R2),X'FF'-X'20'   UN-VALIDATE COL FIELD                        
*                                                                               
         STC   R1,5(R2)            SAVE LENGTH IN FIELD HEADER                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RE)                                                    
         MVI   6(R2),X'80'                                                      
*                                                                               
SETCOLX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        BEFORE CALLING JOB EST, ONLY PASS FIRST 4 COL HEADINGS                 
*                                                                               
SETESTCL ST    RE,SAVERE                                                        
*                                                                               
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'PROCOL),PROCOL                                            
         LA    R2,WORK                                                          
         LA    R1,L'PROCOL                                                      
         LA    R0,4                                                             
*                                                                               
SETEC20  CLI   0(R2),C','          GOT A COMMA                                  
         BE    SETEC30             YES                                          
*                                                                               
         BCTR  R1,0                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         LTR   R1,R1                                                            
         BZ    SETECX                                                           
         B     SETEC20                                                          
*                                                                               
SETEC30  LA    R2,1(R2)                                                         
         BCTR  R1,0                                                             
         BCT   R0,SETEC20                                                       
*                                                                               
         BCTR  R2,0                GET FROM LAST COMMA                          
         EX    R1,*+8              LENGTH IS CORRECT CAUSE I DECR'ED R2         
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         MVI   NCOLS,4                                                          
*                                                                               
SETECX   MVC   SENDCOLS,WORK                                                    
         LA    R2,SENDCOLS                                                      
         LA    R1,L'SENDCOLS                                                    
*                                                                               
SETECX10 CLI   0(R2),C' '                                                       
         BNH   SETECX20                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,SETECX10                                                      
*                                                                               
SETECX20 LA    R1,SENDCOLS                                                      
         SR    R2,R1                                                            
         STC   R2,LSENDCOL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        FUDGCOLS-FUDGE THE JBCLD ENTRY                                         
*              ON ENTRY, R1=A(TYPE OF HI REVESION THEY WANT                     
*                        R5=JBCLD TABLE ENTRY I HAVE TO FUDGE                   
*                                                                               
         USING JBCLD,R5                                                         
FUDGCOLS NTR1                                                                   
         L     R6,AJOBRTAB                                                      
         USING JBLOCKD,R6                                                       
         CLI   JBHIREV,1           IS THE HR ALSO THE OE                        
         BNE   FUDGCE                                                           
         LA    R1,ORIGOFF(R1)      ADD OFFSET TO ORIGINAL EST NUMS              
         MVC   JBCLCN1,0(R1)       SET THE COL LIST ENTRY                       
         B     FUDGCOLX                                                         
*                                                                               
FUDGCE   CLC   JBHIREV,JBHIAPP     IS THE HR=CE                                 
         BNE   FUDGRN                                                           
         LA    R1,CURROFF(R1)     ADD OFFSET TO CURRENT EST NUMS                
         MVC   JBCLCN1,0(R1)       SET THE COL LIST ENTRY                       
         B     FUDGCOLX                                                         
*                                                                               
FUDGRN   LA    R1,REGOFF(R1)       ADD OFFSET TO REQULAR EST NUMBERS            
         MVC   JBCLCN1,0(R1)       SET THE COL LIST ENTRY                       
         MVI   JBCLCN1E,C'R'                                                    
         MVC   JBCLCN1V,JBHIREV    SET THE R NUMBER                             
*                                                                               
FUDGCOLX B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
CHKOPT   ST    RE,SAVERE                                                        
         L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         L     R2,ACOLS                                                         
         CR    R2,RE               WERE THE COLS THE LAST FIELD MODED           
         BNE   CHKOPTX             NO                                           
*                                                                               
         CR    RE,RE                                                            
CHKOPTX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE ESTIMATE EXPRESSIONS IN THE COLUMN LIST               
*                                                                               
* AT ENTRY, ACOLIST=A(COLUMN LIST) P2=0 TO CHECK KEY'S JOB P2=1 TO              
*           CHECK FROMJOB (CALL FROM EDCOPY) P1=A(GEND)                         
* ON EXIT, CC=EQ IF OK, CC=NEQ IF ERROR AND BYTE CONTAINS ERROR ENTRY           
*          NUMBER                                                               
*                                                                               
VALEST   NTR1                                                                   
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
         GOTO1 HIGH                                                             
         B     VALEST2                                                          
*                                                                               
VALEST1  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
VALEST2  CLC   ACEVKEY(ACEVTYPE-ACEVKEY),KEYSAVE                                
         BNE   VALEST5                                                          
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF) IS IT A TIME EST RECORD?                    
         BNZ   VALEST1             YES, SO READ NEXT RECORD                     
*                                                                               
         XC    0(3,R5),0(R5)       CLEAR OUT ENTRY                              
         MVC   0(2,R5),ACEVTYPE    START WITH TYPE/VERSION NUMBER               
*                                                                               
VALEST3  CLI   ACEVTYPE,ACEVREV    TEST FOR REVISION                            
         BNE   VALEST4                                                          
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
         BNE   VALEST4                                                          
         MVC   HIATYPE(2),ACEVTYPE SET HIGHEST APPROVED ESTIMATE                
         MVI   2(R5),C'A'          NOTE ESTIMATE IS APPROVED                    
*                                                                               
VALEST4  LA    R5,3(R5)                                                         
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
         BNE   VALEST10                                                         
*                                                                               
         CLC   JBCLCN1,ORIGEST                                                  
         BE    VALEST8                                                          
         CLC   JBCLCN1,CURREST                                                  
         BE    VALEST9                                                          
         CLC   JBCLCN1,REGEST                                                   
         BNE   VALEST10                                                         
*                                                                               
         LTR   R0,R3                                                            
         BZ    VALESTR                                                          
         LA    R5,BUFF                                                          
         CLC   JBCLCN1E(2),0(R5)   MATCH ON LIST                                
         BE    VALEST10            YES-FOUND IT                                 
         LA    R5,3(R5)                                                         
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
         MH    RE,=H'3'            ENTRIES ARE 3 BYTES LONG                     
         LA    RF,BUFF(RE)                                                      
*                                                                               
         ZIC   RE,JBCLCN1V                                                      
         MH    RE,=H'3'            MULTIPLY BY 3                                
         LA    RF,0(RF,RE)         INDEX TO CORRECT REVISION                    
*                                                                               
         MVC   JBCLSNV,JBCLCN1E    SAVE SUPPLEMENT                              
         MVC   JBCLCN1E,0(RF)                                                   
         MVC   JBCLCN1V,1(RF)                                                   
*                                                                               
         SH    RF,=H'3'                                                         
         MVC   JBCLCN2,JBCLCN1                                                  
         MVC   JBCLCN2E,0(RF)                                                   
         MVC   JBCLCN2V,1(RF)                                                   
*                                                                               
         MVI   JBCLOPER,C'-'       ALWAYS A MINUS                               
         MVI   JBCLTYP,JBCLFRM                                                  
         B     VALEST10                                                         
*                                                                               
VALEST8  CLI   ORGTYPE,0           TEST IF ORIGINAL FOUND                       
         BE    VALESTR                                                          
         B     VALEST10                                                         
*                                                                               
VALEST9  CLI   CURTYPE,0           TEST IF CURRENT FOUND                        
         BE    VALESTR                                                          
*                                                                               
VALEST10 LA    R2,JBCLENQ(R2)                                                   
         LA    R4,1(R4)                                                         
         B     VALEST6                                                          
*                                                                               
VALESTR  STC   R4,BYTE                                                          
         B     NOXIT                                                            
*                                                                               
VALESTX  B     YESXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
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
         EJECT                                                                  
*                                                                               
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
DISMSG   DC    C'** DATA DISPLAYED -- PRESS ENTER FOR NEXT **'                  
NONEMSG  DC    C'** NO DATA TO DISPLAY--CHECK HEADLINE FIELDS **'               
PF12MSG  DC    C'** PRESS PF12 TO RETURN                    **'                 
NOCATS   DC    C'** YOU CAN''T ZOOM TO A CATAGORY **'                           
JSXJOB   DC    C' (EXP)'                                                        
*&&US                                                                           
DEFCOLS  DC    C'OE,CE,ACT,PO,UNC,CEG'                                          
*&&                                                                             
*&&UK                                                                           
DEFCOLS  DC    C'OE,CE,ACT,PO,UNC    '                                          
*&&                                                                             
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
GETBILLS NMOD1 0,*GETBLS*                                                       
         L     RC,0(R1)                                                         
         ZAP   JBBLGRS,=P'0'                                                    
         ZAP   JBBLNET,=P'0'                                                    
*                                                                               
         SR    R3,R3                                                            
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES          TRANSACTIONS ARE SPACE FILLED                
         L     R1,AJOB                                                          
         MVC   ACKEYACC,0(R1)                                                   
         MVC   ACKEYWRK,=C'99'                                                  
*                                                                               
         L     R4,AIO                                                           
         GOTO1 HIGH                                                             
         B     GETBL2                                                           
*                                                                               
GETBL1   GOTO1 SEQ                                                              
*                                                                               
GETBL2   CLC   ACKEYACC(L'ACKEYACC+L'ACKEYWRK),KEYSAVE                          
*                                  (STILL) READING THIS JOBS BILLS              
         BNE   GETBL5              NO                                           
*                                                                               
         OC    ACDTPEEL,ACDTPEEL   WAS TRANSACTION PEELED?                      
         BNZ   GETBL5              YES - SKIP IT                                
*                                                                               
         USING TRANSD,R5                                                        
         LA    R5,ACRECORD                                                      
         CLI   TRNSEL,X'44'                                                     
         BNE   GETBL1                                                           
*                                                                               
         AP    JBBLNET,TRNSAMNT                                                 
         AP    JBBLGRS,TRNSNARR+27(6)                                           
*                                                                               
         B     GETBL1                                                           
*                                                                               
GETBL5   XIT1                                                                   
         DROP  R4,R5                                                            
*&&                                                                             
         EJECT                                                                  
* TABLE OF OPTION KEYWORDS (COVERED BY OPTTABD)                                 
*                                                                               
         DS    0F                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'CATEGORY'                                                    
         DC    AL1(3,MAXCATS*L'CATOPT),AL1(0,0)                                 
         DC    AL2(CATOPT-SUBSYSD,VALCATG-T60B47)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ZERO'                                                        
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(ZEROPT-SUBSYSD,VALZERO-T60B47)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ROUND'                                                       
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(ROUNDOPT-SUBSYSD,VALRND-T60B47)                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DISPLAY'                                                     
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(DISPOPT-SUBSYSD,VALDIS-T60B47)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DEFAULT'                                                     
         DC    AL1(2,1),AL1(OPTIYN,0)                                           
         DC    AL2(DEFOPT-SUBSYSD,0)                                            
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'WCODES'                                                      
         DC    AL1(2,MAXWORK*L'WCOPT),AL1(0,0)                                  
         DC    AL2(WCOPT-SUBSYSD,VALWC-T60B47)                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'SWCODE'                                                      
         DC    AL1(2,2),AL1(0,0)                                                
         DC    AL2(SWOPT-SUBSYSD,VALSW-T60B47)                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DF'                                                          
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(DFTOPT-SUBSYSD,VALDFT-T60B47)                                
         DC    XL4'00'                                                          
*                                                                               
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
*              DSECTS ARE HIDDEN IN HERE                                        
         EJECT                                                                  
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
* LOCAL WORKING STORAGE, TWA DSECT                                              
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   ACIOBLOK            USE THE ACCIO AND DRONEBLK AREAS             
LOCAL    DS    0C                                                               
AOPTTBL  DS    A                                                                
VDISCOL  DS    A                                                                
VRIGHT   DS    A                                                                
*                                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
*                                                                               
ACTUALS  DS    AL2                                                              
HIREVS   DS    0C                                                               
HIREV    DS    AL2                                                              
HIREVG   DS    AL2                                                              
HIREVC   DS    AL2                                                              
NHIREVS  EQU   (*-HIREVS)/L'HIREV                                               
*                                                                               
ORIGESTS DS    0C                                                               
ORIGOFF  EQU   *-HIREVS            OFFSET TO ORIGINAL ESTIMATE NUMBERS          
ORIGEST  DS    AL2                                                              
ORIGESTG DS    AL2                                                              
ORIGESTC DS    AL2                                                              
*                                                                               
CURRESTS DS    0C                                                               
CURROFF  EQU   *-HIREVS            OFFSET TO CURRENT ESTIMATE NUMBERS           
CURREST  DS    AL2                                                              
CURRESTG DS    AL2                                                              
CURRESTC DS    AL2                                                              
*                                                                               
REGESTS  DS    0C                                                               
REGOFF   EQU   *-HIREVS            OFFSET TO REGULAR ESTIMATE NUMBERS           
REGEST   DS    AL2                                                              
REGESTG  DS    AL2                                                              
REGESTC  DS    AL2                                                              
*                                                                               
SAVERE   DS    A                                                                
VDISP    DS    V                   V(DISPLAY MODULE)                            
*                                                                               
ACOLTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LCOLTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
AOPVTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LOPVTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
*                                                                               
AJOBRTAB DS    A                   A(JOB TABLE)                                 
AVECTAB  DS    A                   A(VECTOR TABLE)                              
*                                                                               
JOBSCH   DS    CL(L'ACSHCODE)                                                   
NCOLS    DS    X                                                                
ZEROPT   DS    C                   Y=SUPPRESS ZERO ROWS                         
DISPOPT  DS    C                   W OR C                                       
DEFOPT   DS    C                   Y OR B                                       
*                                  TYPE OF ROWS TO DISLPAY                      
NCATS    DS    X                   N'FILTER CATEGORIES                          
CATOPT   DS    (MAXCATS)CL2        CATEGORY FILTERS                             
NEGSW    DS    C                                                                
ROUNDOPT DS    C                                                                
SWOPT    DS    C                                                                
WCOPT    DS    (MAXWORK)CL2        LIST OF W/C'S TO DISPLAY                     
NWORK    DS    X                   NUMBER OF WC'S IN ABOVE LIST                 
*                                                                               
ORGTYPE  DS    X                   ORIGINAL ESTIMATE TYPE                       
ORGVERS  DS    X                   ORIGINAL ESTIMATE VERSION                    
CURTYPE  DS    X                   CURRENT ESTIMATE TYPE                        
CURVERS  DS    X                   CURRENT ESTIMATE VERSION                     
HIATYPE  DS    C                   HIGHEST APPROVED ESTIMATE TYPE               
HIAVERS  DS    X                   HIGHEST APPROVED ESTIMATE VERSION            
NESTS    DS    X                   N'ESTIMATES FOR JOB                          
HIRTYPE  DS    C                   HIGHEST REVISION TYPE                        
HIRVERS  DS    X                   HIGHEST REVISION VERSION                     
HIRONFIL DS    X                   HIGHEST REV NUMBER(INCLUDES DELETES)         
NREVS    DS    X                   N'REVISIONS                                  
DFTOPT   DS    C                   INCLUDE/EXCLUDE DRAFT TRANSACTIONS           
*                                                                               
AJOB     DS    A                   A(JOB RECORD)                                
ACOLIST  DS    A                   A(COLUMN LIST)                               
*                                                                               
COLLIST  DS    (MAXCOLS)CL(JBCLENQ)                                             
LASTCAT  DS    CL2                                                              
*                                                                               
AHEADER  DS    A                                                                
ACOLS    DS    A                                                                
AFSTSEL  DS    A                   SCREEN ADCONS                                
AFSTDATA DS    A                                                                
ALSTDATA DS    A                                                                
AENDSCR  DS    A                                                                
ATOTALS  DS    A                                                                
AJOBTOT  DS    A                                                                
*                                                                               
ASEL     DS    A                   COL ADDRESSES                                
ADATA    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
AOPTIONS DS    A                                                                
*                                                                               
ESTTYPE  DS    CL1                 COMMAND ESTIMATE TYPE                        
ESTVERS  DS    XL1                 COMMAND ESTIMATE VERSION                     
*                                                                               
ADDTYPE  DS    C                                                                
ADDVERS  DS    X                                                                
ADJOP    DS    C                                                                
ADJ      DS    PL6                                                              
NEWSCHSW DS    C                   Y=OK TO COPY TO NEW SCHEME                   
AFROMEST DS    A                                                                
ADIFFSCH DS    A                                                                
         DS    XL10                SPARE                                        
*                                                                               
JBBLGRS  DS    PL6                 GROSS BILLED ON JOB                          
JBBLNET  DS    PL6                 NET BILLED ON JOB                            
WHERE    DS    CL2                 WHERE FOR TEXT MAINT CALL                    
*                                                                               
* EDIT MODULE STORAGE                                                           
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
         DS    F                   SPARE                                        
*                                                                               
* GETOPT STORAGE                                                                
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
         SPACE 2                                                                
       ++INCLUDE ACGOXBLOCK                                                     
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROB7D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2624                                                     
SVDATA   DS    0D                                                               
SVNWCS   DS    X                                                                
SVLOW    DS    X                                                                
SVHI     DS    X                                                                
SVTABLE  DS    (MAXDATA)XL5                                                     
NVECENT  DS    F                                                                
SVJOBTOT DS    (MAXCOLS)PL(L'JBCOLVAL)                                          
SENDCOLS DS    CL(L'PROCOL)                                                     
SENDOPTS DS    CL(L'PROOPT)                                                     
GOTSTART DS    CL1                 WITH A STARTING WC                           
LSENDCOL DS    X                   LENGTH OF DATA ON SENDCOLS                   
LSENDOPT DS    X                   LENGTH OF DATA ON SENDOPTS                   
         DS    CL((SAVAREA-SVDATA)-(*-SVDATA))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
VECLENQ  EQU   2                                                                
EDTLIST  EQU   3                                                                
PRTREP   EQU   4                                                                
DOWNREP  EQU   5                                                                
SOONREP  EQU   6                                                                
MAXCOLS  EQU   6                   MAXIMUM COLUMNS ON SCREEN                    
MAXCATS  EQU   4                   MAXIMUM CATEGORY FILTERS                     
NDATAFLD EQU   2                   N'FIELDS ON A DATA LINE                      
MAXDATA  EQU   13                  LINES ON JOB SUMMARY SCREEN                  
MAXWORK  EQU   6                   NUMBER OF W/C YOU CAN LIST IN WC=OPT         
         SPACE 2                                                                
         EJECT                                                                  
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
*                                                                               
JEWORKD  DSECT                     **OPTION TABLE ENTRY**                       
JESYSRD  DS    A                                                                
JESAVE   DS    XL(LOCALLN)         SAVED LWS                                    
JEWORKL  EQU   *-JEWORKD                                                        
         EJECT                                                                  
         SPACE 2                                                                
* DSECT TO DATA FIELD IN A DISPLAY ROW                                          
*                                                                               
DISPD    DSECT                                                                  
DISCODE  DS    CL2                                                              
DISCTFL  DS    CL1                 > IF ROW IS A CATAGORY                       
DISDESC  DS    CL8                 W/C OR CAT NAME                              
         DS    CL1                                                              
DISCOLS  DS    CL66                NUMERIC COLS (10+1)*6                        
DISPL    EQU   *-DISPD             ENTRY LENGTH                                 
         SPACE 2                                                                
* DSECT TO SUPPLEMENT COLUMN TABLE                                              
* DSECT TO COVER JOBBLOCK                                                       
*                                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE ACJOBBERD                                                      
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047ACPRO47   04/11/07'                                      
         END                                                                    
