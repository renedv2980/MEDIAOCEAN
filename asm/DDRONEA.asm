*          DATA SET DDRONEA    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T00A39A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DRONE  -- DRIVAL INTERFACE                           *         
*                T00A39 -- CORE-RESIDENT PHASE NUMBER                 *         
*                                                                     *         
*  COMMENTS:     CONTAINS TRANSLATION ROUTINES FROM USER PROGRAMS     *         
*                TO DRIVAL INPUT FORMAT.                              *         
*                                                                     *         
*  CALLED FROM:  ONLINE APPLICATION PROGRAMS                          *         
*                                                                     *         
*  CALLS TO:     DATAMGR                                              *         
*                                                                     *         
*  INPUTS:       0(R1)    A(DRONE BLOCK)         SEE DRONEBLKD        *         
*                                                                     *         
*  OUTPUTS:      UPDATED DRONE BLOCK                                  *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
*                R8 -- COMFACS                                        *         
*                R9 -- DRONE PARAMETER BLOCK                          *         
*                RA -- SECOND BASE                                    *         
*                RB -- FIRST BASE                                     *         
*                RC -- WORKING STORAGE                                *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'DRONE -- ONLINE DRIVAL INTERFACE'                               
DRONE    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**DRONE*,RR=R3                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING DRONE,RB,RA                                                      
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
*                                                                               
         L     R9,0(R1)            A(DRONE BLOCK)                               
         USING DRONED,R9                                                        
         ST    RD,SAVERD           HANG ON TO REGISTER SAVE AREA                
         L     R8,DRCOMFAC         A(COMFACS)                                   
         USING COMFACSD,R8                                                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A36'                                           
         GOTO1 CCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADRIVAL,DMCB        ADDRESS OF DRIVAL                            
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A0D'                                           
         GOTO1 CCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ASQUASH,DMCB        ADDRESS OF SQUASHER                          
*                                                                               
         MVI   MADESW,C'N'         DID NOT DO SYSTEM SWITCH YET                 
         BAS   RE,VACTION                                                       
*                                                                               
XMOD     XMOD1                                                                  
         EJECT                                                                  
* MACRO AND ROUTINE TO GENERATE TWA FIELD HEADER FOR DATA                       
*                                                                               
         MACRO                                                                  
*                                                                               
&NAME    FLDG  &IN,&OUT,&LEN                                                    
*                                                                               
* PARAMS --    A(INPUT FIELD),A(OUTPUT TWA HEADER),A(DATA LENGTH)               
*                                                                               
&NAME    LA    R0,&IN                                                           
         LA    R1,&OUT                                                          
         MVC   FLDGLEN,&LEN                                                     
         BAS   RE,FLDGEN                                                        
*                                                                               
         MEND                                                                   
         SPACE 3                                                                
FLDGEN   NTR1                                                                   
*                                                                               
         CLI   FLDGLEN,0           TEST LENGTH OF ZERO                          
         BE    FLDGENX             IF SO, DO NOTHING                            
*                                                                               
         XC    0(8,R1),0(R1)       CLEAR FIELD HEADER                           
         MVC   5(1,R1),FLDGLEN     STORE INPUT LENGTH                           
*                                                                               
         LR    R2,R0                                                            
         ZIC   R4,5(R1)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),0(R2)       PUT DATA INTO DATA FIELD                     
*                                                                               
         ZIC   R4,5(R1)            INPUT LENGTH                                 
         AH    R4,=H'8'            HEADER LENGTH                                
         STC   R4,0(R1)            TOTAL FIELD LENGTH                           
*                                                                               
         LA    R5,0(R4,R1)         JUST PAST END OF OUTPUT FIELD                
*                                                                               
BACKUP   BCTR  R5,0                MOVE BACKWARDS, LOOK AT EACH CHAR.           
         CLI   0(R5),C' '          IF BLANK OR NULL, KEEP BACKING UP            
         BE    *+12                                                             
         CLI   0(R5),0                                                          
         BNE   FLDGENX                                                          
*                                                                               
         ZIC   R4,0(R1)            ADJUST LENGTHS IN HEADER                     
         BCTR  R4,0                                                             
         STC   R4,0(R1)                                                         
         ZIC   R4,5(R1)                                                         
         BCTR  R4,0                                                             
         STC   R4,5(R1)                                                         
*                                                                               
         B     BACKUP                                                           
*                                                                               
FLDGENX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE SYSTEM AND ACTION CODE                                               
*                                                                               
VACTION  NTR1                                                                   
*                                                                               
         LA    R5,WHOTAB           TABLE OF SYSTEMS                             
VACTA    CLI   0(R5),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                INVALID SYSTEM                               
*                                                                               
         CLC   DRWHO,0(R5)         TEST MATCH IN TABLE                          
         BE    *+12                                                             
         LA    R5,8(R5)            NEXT TABLE ENTRY                             
         B     VACTA                                                            
*                                                                               
         L     RF,4(R5)            A(SYSTEM TABLE)                              
         A     RF,RELO                                                          
*                                                                               
         LR    R5,RF               TABLE OF SYSTEM OPTIONS                      
VACTB    CLI   0(R5),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                INVALID OPTION                               
*                                                                               
         CLC   DRACTION,0(R5)      TEST MATCH IN TABLE                          
         BE    *+12                                                             
         LA    R5,8(R5)            NEXT TABLE ENTRY                             
         B     VACTB                                                            
*                                                                               
         L     RF,4(R5)            A(VALIDATION ROUTINE)                        
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
WHOTAB   DS    0D                                                               
         DC    AL1(DRNETWHO),AL3(0),A(NETTAB)                                   
         DC    AL1(DRABCWHO),AL3(0),A(ABCTAB)                                   
         DC    AL1(DRDEMWHO),AL3(0),A(DEMOTAB)                                  
         DC    AL1(DRACCWHO),AL3(0),A(ACCTAB)                                   
         DC    AL1(DRSPTWHO),AL3(0),A(SPOTTAB)                                  
         DC    AL1(DRPRTWHO),AL3(0),A(PRNTTAB)                                  
         DC    AL1(DRTALWHO),AL3(0),A(TALTAB)                                   
         DC    AL1(DRMBAWHO),AL3(0),A(MBATAB)                                   
         DC    X'FF'                                                            
*                                                                               
NETTAB   DS    0D                                                               
ACCTAB   EQU   *                                                                
DEMOTAB  EQU   *                                                                
SPOTTAB  EQU   *                                                                
PRNTTAB  EQU   *                                                                
TALTAB   EQU   *                                                                
MBATAB   EQU   *                                                                
         DC    AL1(DRINIT),AL3(0),A(INIT1)                                      
         DC    AL1(DRROW),AL3(0),A(ROW1)                                        
         DC    AL1(DRGENROW),AL3(0),A(GEN1)                                     
         DC    AL1(DRCOL),AL3(0),A(COL1)                                        
         DC    AL1(DRGENCOL),AL3(0),A(GEN1)                                     
         DC    AL1(DRCMP),AL3(0),A(CMP1)                                        
         DC    AL1(DRGENCMP),AL3(0),A(GENCMP1)                                  
         DC    AL1(DRWRAPUP),AL3(0),A(WRAP1)                                    
         DC    AL1(DRENTRY),AL3(0),A(ENTRY1)                                    
         DC    AL1(DRUSER),AL3(0),A(USER1)                                      
         DC    X'FF'                                                            
*                                                                               
ABCTAB   DS    0D                                                               
         DC    AL1(DRINIT),AL3(0),A(INIT2)                                      
         DC    AL1(DRGENROW),AL3(0),A(GEN2)                                     
         DC    AL1(DRGENCOL),AL3(0),A(GEN2)                                     
         DC    AL1(DRWRAPUP),AL3(0),A(WRAP1)                                    
         DC    X'FF'                                                            
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT1    NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
*                                                                               
         LA    R1,DRINFEND-DRINFST                                              
         XCEF  DRITEM,(R1)         CLEAR THE DRONE BLOCK ITEM SECTION           
*                                                                               
         OC    DRSTBUF,DRSTBUF     TEST IF WE ARE GIVEN A BUFFER                
         BNZ   INIT110             WE ARE                                       
         LA    R0,BUFFER           USE MY BUFFER                                
         ST    R0,DRSTBUF                                                       
         AH    R0,=H'1024'         LENGTH 1024                                  
         ST    R0,DRENDBUF                                                      
*                                                                               
INIT110  MVC   DRCURBUF,DRSTBUF    SET CURRENT POINTER                          
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVI   DBOPCODE,DBINIT     INITIALIZE DRIVAL                            
         MVC   DBELOADR,DRCURBUF   FILL IN REQUIRED PARAMETERS                  
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         OC    DRMAXWID,DRMAXWID   TEST MAXIMUM WIDTH GIVEN                     
         BNZ   *+10                                                             
         MVC   DRMAXWID,=H'132'    DEFAULT WIDTH IS 132 CHARS                   
         LA    R2,1                                                             
         STH   R2,DRCURWID         BEGIN WIDTH COUNTER AT ONE                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
INIT2    NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
*                                                                               
         LA    R1,DRINFEND-DRINFST                                              
         XCEF  DRITEM,(R1)         CLEAR THE DRONE BLOCK ITEM SECTION           
*                                                                               
         MVC   DRCURBUF,DRSTBUF    SET CURRENT POINTER                          
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVI   DBOPCODE,DBINIT     INITIALIZE DRIVAL                            
         MVC   DBELOADR,DRCURBUF   FILL IN REQUIRED PARAMETERS                  
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         OC    DRMAXWID,DRMAXWID   TEST MAXIMUM WIDTH GIVEN                     
         BNZ   *+10                                                             
         MVC   DRMAXWID,=H'132'    DEFAULT WIDTH IS 132 CHARS                   
         LA    R2,1                                                             
         STH   R2,DRCURWID         BEGIN WIDTH COUNTER AT ONE                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE A ROW                                                                
*                                                                               
ROW1     NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         MVI   OPTNUM,0            NO OPTIONS YET                               
         BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
         XCEF  SCANBLK,1480        CLEAR SCANNER BLOCK                          
*                                                                               
         L     R4,DRNETFLD                                                      
         CLI   5(R4),0             TEST NO INPUT                                
         BNE   *+12                                                             
         MVI   DRERROR,ERRNILQ                                                  
         B     MYERR                                                            
*                                                                               
         ZIC   R1,0(R4)            PASS SCANNER A TWA FIELD HEADER              
         TM    1(R4),X'02'         TEST EXTENDED                                
         BZ    *+8                 NO                                           
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              R1 = FIELD LENGTH WITHOUT EXTENSION          
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
*                                                                               
         TM    1(R4),X'02'         TEST EXTENDED                                
         BZ    ROW120              NO                                           
         ZIC   R1,WORK                                                          
         SH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         NI    WORK+1,X'FD'        MAKE TWA HEADER STANDARD FOR SCANNER         
*                                                                               
ROW120   GOTO1 CSCANNER,DMCB,WORK,(8,SCANBLK),0                                 
         MVC   SCANLEN,4(R1)       NUMBER OF ENTRIES IN TABLE                   
         XC    DMCB,DMCB                                                        
*                                                                               
         CLI   SCANLEN,4           MAXIMUM OF 4 ENTRIES                         
         BNH   *+12                                                             
         MVI   DRERROR,ERRINVOP                                                 
         B     MYERR                                                            
*                                                                               
         LA    R4,SCANBLK          TOP OF SCANNER BLOCK                         
         XC    WORK,WORK                                                        
         FLDG  12(R4),WORK,0(R4)   GENERATE A TWA FIELD WITH HEADER             
         MVI   OPTNUM,1            FIRST OPTION                                 
         LA    R4,WORK             A(FIELD HEADER)                              
         BAS   RE,VENTRY                                                        
*                                                                               
         USING DICKEYD,R3                                                       
         LA    R0,DICFIRST-DICKEY  DISPLACEMENT TO FIRST ELEMENT                
         STH   R0,DATADISP                                                      
*                                                                               
         L     R3,AIO              A(DICTIONARY ENTRY)                          
         XC    DRATTRIB,DRATTRIB                                                
         MVI   ELCODE,X'08'        ATTRIBUTE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   DRATTRIB,2(R3)      ATTRIBUTE BYTES                              
*                                                                               
         L     R3,AIO              A(DICTIONARY ENTRY)                          
         MVI   ELCODE,X'33'        OUTPUT LENGTH ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   ROW130                                                           
         ZIC   R2,2(R3)            OUTPUT LENGTH                                
         LA    R2,1(R2)            PLUS ONE FOR SPACE BETWEEN COLUMNS           
         AH    R2,DRCURWID         PLUS CURRENT COUNTER                         
         CH    R2,DRMAXWID         TEST OUTPUT TOO WIDE                         
         BNH   *+12                                                             
         MVI   DRERROR,ERR2WIDQ                                                 
         B     MYERR                                                            
         STH   R2,DRCURWID         UPDATE CURRENT WIDTH                         
*                                                                               
ROW130   OI    DRFLAGI,X'80'       GENERATE 'IN' STATEMENT                      
         L     R3,AIO              A(DICTIONARY RECORD)                         
*                                                                               
         TM    DRFLAGS,DREXPDIC    TEST ENTRY GETS EXPANDED                     
         BZ    *+12                NO                                           
         BAS   RE,EXPENTRY                                                      
         B     ROW135                                                           
*                                                                               
         MVC   DRDICTI,DICCODE     DICTIONARY NAME                              
         OC    DRDICTI,SPACES      PAD WITH BLANKS                              
         MVC   DRENTRYI,DICENTRY   ENTRY NAME                                   
*                                                                               
ROW135   MVI   ELCODE,X'34'        OUTPUT ROUTINE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         CLC   =C'OMIT    ',2(R3)  TEST DON'T GENERATE 'OUT'                    
         BE    ROW140              CORRECT -- DON'T GENERATE 'OUT'              
         OI    DRFLAGO,X'80'       GENERATE 'OUT' STATEMENT                     
*                                                                               
         TM    DRFLAGS,DREXPDIC    TEST ENTRY GETS EXPANDED                     
         BO    ROW140              YES                                          
         MVC   DRDICTO,DRDICTI     USE SAME DICTIONARY AND ENTRY NAMES          
         MVC   DRENTRYO,DRENTRYI                                                
         DROP  R3                                                               
*                                                                               
ROW140   CLI   SCANLEN,1           TEST ANY OPTIONS                             
         BE    ROW1X               NO                                           
         LA    R4,SCANBLK          A(SCANNER BLOCK)                             
         ZIC   R0,SCANLEN          NUMBER OF SCANNER ENTRIES                    
         BCTR  R0,0                MINUS ONE FOR ENTRY NAME                     
*                                                                               
ROW150   ZIC   R1,OPTNUM           INCREMENT OPTION NUMBER                      
         LA    R1,1(R1)                                                         
         STC   R1,OPTNUM                                                        
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,0(R4)            LENGTH OF OPTION                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'TOTAL'  TEST TOTAL OPTION                            
         BE    ROW160                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'*'      * ALSO MEANS TOTAL                           
         BNE   *+16                                                             
*                                                                               
ROW160   OI    DRTOTAL,X'80'       GENERATE TOTAL STATEMENT                     
         MVI   DRTSPACE,1          AND SPACE 1 LINE                             
         B     ROW180                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'SKIP'   TEST SKIP OPTION                             
         BNE   *+16                                                             
*                                                                               
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     ROW180                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'SPACE'  TEST SPACE OPTION                            
         BNE   ROW170                                                           
*                                                                               
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         CLI   1(R4),0             TEST ANY SPACING VALUE GIVEN                 
         BNE   *+12                                                             
         MVI   DRLSPACE,1          DEFAULT IS 1 SPACE                           
         B     ROW180                                                           
*                                                                               
         TM    3(R4),X'80'         TEST ARGUMENT NUMERIC                        
         BO    *+12                                                             
         MVI   DRERROR,DBARG                                                    
         B     MYERR                                                            
*                                                                               
         ICM   R3,15,8(R4)         NUMERIC VALUE                                
         C     R3,=F'1'            TEST VALUE WITHIN RANGE                      
         BNL   *+12                                                             
         MVI   DRERROR,DBNUMOUT                                                 
         B     MYERR                                                            
         C     R3,=F'255'                                                       
         BNH   *+12                                                             
         MVI   DRERROR,DBNUMOUT                                                 
         B     MYERR                                                            
         STC   R3,DRLSPACE         SPACE REQUESTED NO. OF LINES                 
         B     ROW180                                                           
*                                                                               
ROW170   MVI   DRERROR,ERRINVOP    ALL OTHER OPTIONS ARE INVALID                
         B     MYERR                                                            
*                                                                               
ROW180   BCT   R0,ROW150           TRY NEXT OPTION                              
*                                                                               
ROW1X    BAS   RE,SWBACK           SWITCH BACK TO PREVIOUS SYSTEM               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE A COLUMN                                                             
*                                                                               
COL1     NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         MVI   OPTNUM,0                                                         
         BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
         XCEF  SCANBLK,1480        CLEAR SCANNER BLOCK                          
*                                                                               
         L     R4,DRNETFLD                                                      
         CLI   5(R4),0             TEST NO INPUT                                
         BNE   *+12                                                             
         MVI   DRERROR,ERRNILQ                                                  
         B     MYERR                                                            
*                                                                               
         ZIC   R1,0(R4)            PASS SCANNER A TWA FIELD HEADER              
         TM    1(R4),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                 NO                                           
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
*                                                                               
         TM    1(R4),X'02'         TEST EXTENDED HEADER                         
         BZ    COL120              NO                                           
         ZIC   R1,WORK                                                          
         SH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         NI    WORK+1,X'FD'        STRIP OFF EXTENSION FOR SCANNER              
*                                                                               
COL120   GOTO1 CSCANNER,DMCB,WORK,(8,SCANBLK),C',=, '                           
         MVC   SCANLEN,4(R1)       NUMBER OF ENTRIES IN TABLE                   
         XC    DMCB,DMCB                                                        
*                                                                               
         CLI   SCANLEN,1           MAXIMUM OF 1 ENTRY                           
         BNH   *+12                                                             
         MVI   DRERROR,ERRINVOP                                                 
         B     MYERR                                                            
*                                                                               
         LA    R4,SCANBLK          TOP OF SCANNER BLOCK                         
         XC    WORK,WORK                                                        
         FLDG  12(R4),WORK,0(R4)   GENERATE A TWA FIELD WITH HEADER             
         MVI   OPTNUM,1            FIRST OPTION                                 
         LA    R4,WORK             A(FIELD HEADER)                              
         BAS   RE,VENTRY                                                        
*                                                                               
         USING DICKEYD,R3                                                       
         LA    R0,DICFIRST-DICKEY  DISPLACEMENT TO FIRST ELEMENT                
         STH   R0,DATADISP                                                      
*                                                                               
         L     R3,AIO              A(DICTIONARY ENTRY)                          
         XC    DRATTRIB,DRATTRIB                                                
         MVI   ELCODE,X'08'        ATTRIBUTE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   DRATTRIB,2(R3)      ATTRIBUTE BYTES                              
*                                                                               
         L     R3,AIO              A(DICTIONARY ENTRY)                          
         MVI   ELCODE,X'33'        OUTPUT LENGTH ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   COL150                                                           
         ZIC   R2,2(R3)            OUTPUT LENGTH                                
         LA    R2,1(R2)            PLUS ONE FOR SPACE BETWEEN COLUMNS           
         AH    R2,DRCURWID         PLUS CURRENT COUNTER                         
         CH    R2,DRMAXWID         TEST OUTPUT TOO WIDE                         
         BNH   *+12                                                             
         MVI   DRERROR,ERR2WIDQ                                                 
         B     MYERR                                                            
         STH   R2,DRCURWID         UPDATE CURRENT WIDTH                         
*                                                                               
COL150   OI    DRFLAGI,X'80'       GENERATE 'IN' STATEMENT                      
         L     R3,AIO              A(DICTIONARY RECORD)                         
*                                                                               
         TM    DRFLAGS,DREXPDIC    TEST ENTRY GETS EXPANDED                     
         BZ    *+12                NO                                           
         BAS   RE,EXPENTRY                                                      
         B     COL160                                                           
*                                                                               
         MVC   DRDICTI,DICCODE     DICTIONARY NAME                              
         OC    DRDICTI,SPACES      PAD WITH BLANKS                              
         MVC   DRENTRYI,DICENTRY   ENTRY NAME                                   
*                                                                               
COL160   MVI   ELCODE,X'34'        OUTPUT ROUTINE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         CLC   =C'OMIT    ',2(R3)  TEST DON'T GENERATE 'OUT'                    
         BE    COL1X               CORRECT -- DON'T GENERATE 'OUT'              
         OI    DRFLAGO,X'80'       GENERATE 'OUT' STATEMENT                     
*                                                                               
         TM    DRFLAGS,DREXPDIC    TEST ENTRY GETS EXPANDED                     
         BO    COL1X               YES                                          
         MVC   DRDICTO,DRDICTI     USE SAME DICTIONARY AND ENTRY NAMES          
         MVC   DRENTRYO,DRENTRYI                                                
         DROP  R3                                                               
*                                                                               
COL1X    BAS   RE,SWBACK           SWITCH BACK TO PREVIOUS SYSTEM               
         B     XIT                                                              
         EJECT                                                                  
* GENERATE CODE FOR A DRONE BLOCK                                               
*                                                                               
GEN1     NTR1                                                                   
*                                                                               
         GOTO1 CGETFACT,DMCB,0     GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'01'       TEST OFFLINE                                 
         BO    *+6                                                              
         DC    H'0'                THIS MODE SHOULD ONLY BE OFFLINE             
*                                                                               
         L     RE,FAAUTL           SWITCH TO CONTROL SYSTEM                     
         MVC   SAVESE,4(RE)                                                     
         MVI   4(RE),X'0A'                                                      
         GOTO1 CDATAMGR,DMCB,=C'OPEN',=C'CONTROL',FLIST,IO1                     
         DROP  R1                                                               
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR DRIVAL PARAMETER BLOCK                 
         MVI   DBOPCODE,DBDRROW    ASSUME IT'S A ROW                            
         CLI   DRACTION,DRGENROW                                                
         BE    *+8                                                              
         MVI   DBOPCODE,DBDRCOL    NO, IT'S A COLUMN                            
         MVC   DBDICTNM,DRDICT     FILL IN REQUIRED PARAMETERS                  
         MVC   DBALTDIC,DRALTDIC                                                
         MVC   DBCNTRL,DRCNTRL                                                  
         LA    R0,DRONED                                                        
         ST    R0,DBSRCADR                                                      
         MVC   DBELOADR,DRCURBUF                                                
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         LA    R1,DRINFEND-DRINFST                                              
         XCEF  DRITEM,(R1)         CLEAR THE DRONE BLOCK ITEM SECTION           
*                                                                               
         GOTO1 CGETFACT,DMCB,0     GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'01'       TEST OFFLINE                                 
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,FAAUTL           RESTORE OLD SYSTEM                           
         MVC   4(1,RE),SAVESE                                                   
         DROP  R1                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
GEN2     NTR1                                                                   
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR DRIVAL PARAMETER BLOCK                 
         MVI   DBOPCODE,DBDRROW    ASSUME IT'S A ROW                            
         CLI   DRACTION,DRGENROW                                                
         BE    *+8                                                              
         MVI   DBOPCODE,DBDRCOL    NO, IT'S A COLUMN                            
         MVC   DBCNTRL,DRCNTRL     FILL IN REQUIRED PARAMETERS                  
         LA    R0,DRONED                                                        
         ST    R0,DBSRCADR                                                      
         MVC   DBELOADR,DRCURBUF                                                
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         LA    R1,DRINFEND-DRINFST                                              
         XCEF  DRITEM,(R1)         CLEAR THE DRONE BLOCK ITEM SECTION           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE A COMPUTATIONAL EXPRESSION                                           
*                                                                               
CMP1     NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         L     R4,DRACCFLD                                                      
         CLI   5(R4),0             TEST NO INPUT                                
         BNE   *+12                                                             
         MVI   DRERROR,ERRNILQ                                                  
         B     MYERR                                                            
*                                                                               
         XC    WORK,WORK           PUT NEW STRING IN WORK                       
         LA    R3,WORK                                                          
         LA    R4,8(R4)            BUMP PAST HEADER                             
*                                                                               
CMP10    CLI   0(R4),C'A'          TEST OPERAND IS A LABEL                      
         BL    CMP20               NO                                           
         CLI   0(R4),C'P'                                                       
         BH    CMP20                                                            
         CLC   0(1,R4),DRCMPMAX    YES - TEST LABEL HAS BEEN DEFINED            
         BNH   *+12                YES                                          
         MVI   DRERROR,ERRCOMP                                                  
         B     MYERR                                                            
*                                                                               
         BAS   RE,BUMPIT           BUMP PAST LABEL                              
         BE    CMP40               THERE IS MORE                                
         B     CMP70               END OF STRING                                
*                                                                               
CMP20    CLI   0(R4),C'-'          TEST MINUS SIGN                              
         BNE   *+12                                                             
CMP30    BAS   RE,BUMPIT           BUMP TO NEXT CHARACTER                       
         BNE   CMP70               END OF STRING                                
*                                                                               
         CLI   0(R4),C'0'          TEST NUMERIC                                 
         BL    CMP40               NO - END OF LITERAL                          
         CLI   0(R4),C'9'                                                       
         BNH   CMP30                                                            
*                                                                               
CMP40    MVI   0(R3),C','          INSERT COMMA                                 
         LA    R3,1(R3)                                                         
         CLI   0(R4),C'V'          TEST 'V' TYPE OPERATOR                       
         BE    *+16                YES                                          
*                                                                               
         BAS   RE,BUMPIT           BUMP PAST SINGLE-CHARACTER OPERATOR          
         BNE   CMP70                                                            
         B     CMP60                                                            
*                                                                               
CMP50    BAS   RE,BUMPIT           BUMP PAST 'V'                                
         BNE   CMP70                                                            
         CLI   0(R4),C'0'          TEST NUMERIC                                 
         BL    CMP60               NO - END OF LITERAL                          
         CLI   0(R4),C'9'                                                       
         BNH   CMP50               LOOK FOR MORE DIGITS                         
*                                                                               
CMP60    MVI   0(R3),C','          INSERT COMMA                                 
         LA    R3,1(R3)                                                         
         B     CMP10                                                            
*                                                                               
CMP70    XCEF  SCANBLK,1480        CLEAR SCANNER BLOCK                          
         LA    RF,WORK                                                          
         SR    R3,RF               LENGTH OF STRING                             
         STC   R3,BYTE                                                          
         CLI   BYTE,0                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FLDG  WORK,WORK+80,BYTE   GENERATE A TWA FIELD WITH HEADER             
         GOTO1 CSCANNER,DMCB,(52,WORK+80),(20,SCANBLK),C',=, '                  
         CLI   4(R1),19                                                         
         BNH   *+12                                                             
         MVI   DRERROR,ERRCOMP                                                  
         B     MYERR                                                            
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVC   DBSCANLN,4(R1)                                                   
         MVI   DBOPCODE,X'50'      VALIDATE COMP STATEMENT                      
         MVC   DBCNTRL,DRCNTRL     FILL IN REQUIRED PARAMETERS                  
         MVC   DBELOADR,DRCURBUF                                                
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
         LA    RF,SCANBLK                                                       
         ST    RF,DBSRCADR                                                      
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BE    *+12                                                             
         MVI   DRERROR,ERRCOMP                                                  
         B     MYERR                                                            
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
BUMPIT   MVC   0(1,R3),0(R4)       INSERT CHARACTER IN NEW STRING               
         LA    R3,1(R3)            BUMP NEW STRING POINTER                      
         LA    R4,1(R4)            BUMP OLD STRING POINTER                      
         CLI   0(R4),C' '          TEST END OF STRING                           
         BH    *+10                NO - SET CC EQUAL                            
         CR    RE,R3               YES - SET CC NOT EQUAL                       
         B     *+6                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE A COMPUTATIONAL EXPRESSION AND GENERATE CODE                         
*                                                                               
GENCMP1  NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVC   DBCNTRL,DRCNTRL     FILL IN REQUIRED PARAMETERS                  
         MVC   DBELOADR,DRCURBUF                                                
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
         LA    RF,=C'         COMP '                                            
         ST    RF,DBSRCADR         GENERATE COMP HEADER ELEMENT                 
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BE    *+12                                                             
         MVI   DRERROR,ERRCOMP                                                  
         B     MYERR                                                            
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         BAS   RE,CMP1             GENERATE REMAINING COMP ELEMENTS             
         B     XIT                                                              
         EJECT                                                                  
* WRAP UP                                                                       
*                                                                               
WRAP1    NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
*                                                                               
         XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVI   DBOPCODE,DBCLOSE    COMPLETION ACTION                            
         MVC   DBCNTRL,DRCNTRL     FILL IN REQUIRED PARAMETERS                  
         MVC   DBELOADR,DRCURBUF                                                
         MVC   DBENDADR,DRENDBUF                                                
         MVC   DBCOMADR,DRCOMFAC                                                
*                                                                               
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,DBOK       TEST ERROR FOUND                             
         BNE   ERR                                                              
*                                                                               
         MVC   DRCNTRL,DBCNTRL     RETURN UPDATED BLOCK                         
         MVC   DROLDBUF,DBELOADR                                                
         MVC   DRCURBUF,DBELNADR                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* READ DICTIONARY ENTRY                                                         
*                                                                               
ENTRY1   NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         BAS   RE,SWTOCON          SWITCH TO CONTROL SYSTEM                     
*                                                                               
         L     R4,DRNETFLD                                                      
         CLI   5(R4),0             TEST NO INPUT                                
         BNE   *+12                                                             
         MVI   DRERROR,ERRNILQ                                                  
         B     MYERR                                                            
         BAS   RE,VENTRY           VALIDATE THE ENTRY                           
*                                                                               
         BAS   RE,SWBACK           SWITCH BACK TO PREVIOUS SYSTEM               
         B     XIT                                                              
         SPACE 5                                                                
* USER RECORD                                                                   
*                                                                               
USER1    NTR1                                                                   
*                                                                               
         MVI   DRERROR,0           CLEAR ERROR BYTE                             
         BAS   RE,VUSER            VALIDATE THE USER CODE                       
         BAS   RE,EXPUSER          PUT USER DATA INTO DRONE BLOCK               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* READ DICTIONARY ENTRY RECORD                                                  
*                                                                               
VENTRY   NTR1                      R4 POINTS TO FIELD HEADER                    
*                                                                               
         ZIC   R2,5(R4)                                                         
         C     R2,=F'1'            TEST BETWEEN 1 AND 8 CHARACTERS              
         BNL   *+12                                                             
         MVI   DRERROR,DBARG                                                    
         B     MYERR                                                            
         C     R2,=F'8'                                                         
         BNH   *+12                                                             
         MVI   DRERROR,DBARG                                                    
         B     MYERR                                                            
*                                                                               
         MVI   USEDALT,C'N'        ASSUME WE ARE NOT USING ALTERNATE            
         XC    KEY,KEY                                                          
         USING DICKEYD,R3                                                       
         LA    R3,KEY                                                           
         MVC   DICCODE,DRDICT      DICTIONARY NAME                              
         OC    DICCODE,SPACES      PAD WITH BLANKS                              
*                                                                               
VENTRY10 MVI   DICSYS,DICSYSQ      SYSTEM                                       
         MVI   DICKTYP,DICKTYPQ    RECORD TYPE                                  
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DICENTRY(0),8(R4)   ENTRY NAME                                   
         OC    DICENTRY,SPACES     PAD WITH BLANKS                              
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD DATAMGR RETURN CODE                      
*                                                                               
         CLC   KEY(28),KEYSAVE     TEST WAS RECORD FOUND                        
         BNE   VENTRY20            NO                                           
*                                                                               
         LA    R1,IO1              A(MY I/O AREA)                               
         ST    R1,AIO                                                           
         OC    DRNETIO,DRNETIO     TEST USER I/O AREA SUPPLIED                  
         BZ    *+10                                                             
         MVC   AIO,DRNETIO                                                      
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',DICDA,AIO,DMWORK             
         CLI   8(R1),0                                                          
         BE    VENTRYX                                                          
         DC    H'0'                BAD DATAMGR RETURN CODE                      
*                                                                               
VENTRY20 CLI   USEDALT,C'N'        TEST HAVE WE TRIED ALTERNATE                 
         BE    *+12                NO, WE HAVE NOT                              
         MVI   DRERROR,DBNOENT     RECORD NOT FOUND                             
         B     MYERR                                                            
*                                                                               
         MVI   USEDALT,C'Y'        TRY ALTERNATE DICTIONARY                     
         XC    KEY,KEY                                                          
         MVC   DICCODE,DRALTDIC    ALTERNATE DICTIONARY NAME                    
         OC    DICCODE,SPACES      PAD WITH BLANKS                              
         B     VENTRY10                                                         
*                                                                               
VENTRYX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* READ USER RECORD                                                              
*                                                                               
VUSER    NTR1                                                                   
*                                                                               
         LA    RF,DRUSRKEY         A(USER KEY)                                  
         USING CT02REC,R3                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   CT02KTYP,X'02'      USER RECORD TYPE                             
         MVC   CT02KAGY,0(RF)      ALPHA AGENCY                                 
         MVC   CT02KCOD,2(RF)      USER CODE                                    
         OC    CT02KCOD,SPACES     PAD WITH BLANKS                              
         DROP  R3                                                               
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,IO1                      
         CLI   8(R1),0                                                          
         BE    VUSERX              RECORD WAS FOUND                             
         TM    8(R1),X'EF'         TEST ANY FATAL ERROR                         
         BZ    *+6                 NO, BUT RECORD WASN'T THERE                  
         DC    H'0'                                                             
         MVI   DRERROR,DBNOENT     RECORD NOT FOUND                             
         B     MYERR                                                            
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
* SWITCH TO CONTROL SYSTEM                                                      
*                                                                               
SWTOCON  NTR1                                                                   
*                                                                               
         GOTO1 CGETFACT,DMCB,0     GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'01'       TEST OFFLINE                                 
         BO    SWTOCON5                                                         
*                                                                               
         MVC   SAVESE,FASYS        WE ARE ONLINE                                
         MVC   DUB,=X'0AFFFFFF00000000'                                         
         GOTO1 CSWITCH,DUB                                                      
         CLI   4(R1),0                                                          
         BE    SWTOCONX                                                         
*                                                                               
         BAS   RE,SWBACK           COULD NOT SWITCH, SO SWITCH BACK             
         MVI   DRERROR,ERRCLOSE                                                 
         B     MYERR                                                            
*                                                                               
SWTOCON5 L     RE,FAAUTL           OFFLINE                                      
         MVC   SAVESE,4(RE)                                                     
         MVI   4(RE),X'0A'                                                      
         GOTO1 CDATAMGR,DMCB,=C'OPEN',=C'CONTROL',FLIST,IO1                     
         DROP  R1                                                               
*                                                                               
SWTOCONX MVI   MADESW,C'Y'         SET ON SWITCH FLAG                           
         B     XIT                                                              
         SPACE 3                                                                
* SWITCH BACK TO PREVIOUS SYSTEM                                                
*                                                                               
SWBACK   NTR1                                                                   
*                                                                               
         GOTO1 CGETFACT,DMCB,0     GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'01'       TEST OFFLINE                                 
         BO    SWBACK5                                                          
*                                                                               
         MVC   DUB(1),SAVESE       RESTORE OLD SYSTEM (ONLINE)                  
         MVC   DUB+1(7),=X'FFFFFF00000000'                                      
         GOTO1 CSWITCH,DUB                                                      
         CLI   4(R1),0                                                          
         BE    SWBACKX                                                          
         DC    H'0'                                                             
*                                                                               
SWBACK5  L     RE,FAAUTL           RESTORE OLD SYSTEM (OFFLINE)                 
         MVC   4(1,RE),SAVESE                                                   
         DROP  R1                                                               
*                                                                               
SWBACKX  MVI   MADESW,C'N'         SWITCH IS OVER                               
         B     XIT                                                              
         EJECT                                                                  
* EXPAND DICTIONARY RECORD INTO DRONE BLOCK                                     
*                                                                               
EXPENTRY NTR1                                                                   
*                                                                               
         L     R3,AIO              A(DICTIONARY RECORD)                         
         USING DICKEYD,R3                                                       
         LA    R3,DICFIRST         A(FIRST ELEMENT)                             
         DROP  R3                                                               
*                                                                               
DD2      CLI   0(R3),0             CHECK DICTIONARY ELEMENTS                    
         BE    XIT                                                              
         CLI   0(R3),X'22'                                                      
         BE    DD22                                                             
         CLI   0(R3),X'23'                                                      
         BE    DD23                                                             
         CLI   0(R3),X'24'                                                      
         BE    DD24                                                             
         CLI   0(R3),X'25'                                                      
         BE    DD25                                                             
         CLI   0(R3),X'26'                                                      
         BE    DD26                                                             
         CLI   0(R3),X'32'                                                      
         BE    DD32                                                             
         CLI   0(R3),X'33'                                                      
         BE    DD33                                                             
         CLI   0(R3),X'34'                                                      
         BE    DD34                                                             
         CLI   0(R3),X'35'                                                      
         BE    DD35                                                             
         CLI   0(R3),X'36'                                                      
         BE    DD36                                                             
         CLI   0(R3),X'82'                                                      
         BE    DD82                                                             
         CLI   0(R3),X'83'                                                      
         BE    DD83                                                             
         CLI   0(R3),X'87'                                                      
         BE    DD87                                                             
         B     DDNEXT                                                           
*                                                                               
         USING DEITD,R3                                                         
DD22     MVC   DRTYPEI,DEITYPE     INPUT ELEMENTS                               
         CLI   DEITLEN,4           TEST OLD STYLE INPUT TYPE                    
         BNE   DDNEXT              NO                                           
         MVI   DRTYPEI+2,1         YES - USE REPETITION FACTOR OF 1             
         B     DDNEXT                                                           
*                                                                               
         USING DEILD,R3                                                         
DD23     MVC   DRLENI,DEILEN                                                    
         B     DDNEXT                                                           
*                                                                               
         USING DEIRD,R3                                                         
DD24     MVC   DRRTNI,DEIROUT                                                   
         B     DDNEXT                                                           
*                                                                               
         USING DEIAD,R3                                                         
DD25     ZIC   R1,DEIALEN                                                       
         SH    R1,=H'2'                                                         
         STC   R1,DRNARGSI                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     DDNEXT                                                           
         MVC   DRARGSI(0),DEIARGS                                               
*                                                                               
         USING DEIOD,R3                                                         
DD26     CLI   DEIONUM,1                                                        
         BNE   *+10                                                             
         MVC   DRDECI,DEIOVAL                                                   
         CLI   DEIONUM,3                                                        
         BNE   *+10                                                             
         MVC   DRSCALEI,DEIOVAL                                                 
         B     DDNEXT                                                           
*                                                                               
         USING DEOTD,R3                                                         
DD32     MVC   DRTYPEO,DEOTYPE     OUTPUT ELEMENTS                              
         B     DDNEXT                                                           
*                                                                               
         USING DEOLD,R3                                                         
DD33     MVC   DRLENO,DEOLEN                                                    
         B     DDNEXT                                                           
*                                                                               
         USING DEORD,R3                                                         
DD34     MVC   DRRTNO,DEOROUT                                                   
         B     DDNEXT                                                           
*                                                                               
         USING DEOAD,R3                                                         
DD35     ZIC   R1,DEOALEN                                                       
         SH    R1,=H'2'                                                         
         STC   R1,DRNARGSO                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     DDNEXT                                                           
         MVC   DRARGSO(0),DEOARGS                                               
*                                                                               
         USING DEOOD,R3                                                         
DD36     CLI   DEOONUM,1           OUTPUT OPTIONS                               
         BNE   *+10                                                             
         MVC   DRDECO,DEOOVAL                                                   
         CLI   DEOONUM,2                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRCOMMAO                                                 
         CLI   DEOONUM,3                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRMINUSO                                                 
         CLI   DEOONUM,4                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRZEROO                                                  
         CLI   DEOONUM,5                                                        
         BNE   *+10                                                             
         MVC   DRFILLO,DEOOVAL                                                  
         CLI   DEOONUM,6                                                        
         BNE   *+10                                                             
         MVC   DRFLOATO,DEOOVAL                                                 
         CLI   DEOONUM,7                                                        
         BNE   DD36B                                                            
         CLI   DEOOVAL,C'M'                                                     
         BNE   *+12                                                             
         OI    DROPTSO,DRBKMINO                                                 
         B     DD36B                                                            
         OI    DROPTSO,DRBKALLO                                                 
*                                                                               
DD36B    CLI   DEOONUM,8                                                        
         BNE   *+10                                                             
         MVC   DRDIVO,DEOOVAL                                                   
         CLI   DEOONUM,9                                                        
         BNE   *+10                                                             
         MVC   DRDIVO,DEOOVAL                                                   
         CLI   DEOONUM,X'20'                                                    
         BNE   DD36D                                                            
         CLI   DEOOVAL,C'L'                                                     
         BNE   *+12                                                             
         OI    DROPTSO,DRALGNLO                                                 
         B     DD36D                                                            
         OI    DROPTSO,DRALGNRO                                                 
*                                                                               
DD36D    CLI   DEOONUM,X'24'                                                    
         BNE   *+8                                                              
         OI    DROPTSO,DRCHOPO                                                  
         CLI   DEOONUM,X'27'                                                    
         BNE   *+10                                                             
         MVC   DRSCALEO,DEOOVAL                                                 
         B     DDNEXT                                                           
*                                                                               
         USING DESRD,R3                                                         
DD82     MVC   DRH1RTN,DESROUT     HEADING ELEMENTS                             
         OI    DRHEAD1,X'80'                                                    
         B     DDNEXT                                                           
*                                                                               
         USING DESAD,R3                                                         
DD83     ZIC   R1,DESALEN                                                       
         SH    R1,=H'2'                                                         
         STC   R1,DRH1NARG                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     DDNEXT                                                           
         MVC   DRH1ARGS(0),DESARGS                                              
*                                                                               
         USING DELITD,R3                                                        
DD87     LA    RF,DRHEAD1                                                       
         CLI   DELITLIN,1                                                       
         BE    DD87B                                                            
         LA    RF,DRHEAD2                                                       
         CLI   DELITLIN,2                                                       
         BE    DD87B                                                            
         LA    RF,DRHEAD3                                                       
         CLI   DELITLIN,3                                                       
         BE    DD87B                                                            
         LA    RF,DRHEAD4                                                       
         CLI   DELITLIN,4                                                       
         BE    DD87B                                                            
         DC    H'0'                                                             
*                                                                               
         USING DRHEADD,RF                                                       
DD87B    OI    DRHEAD,X'80'        TURN ON GENERATE BIT                         
         ZIC   R1,DELITLEN         LITERALS                                     
         SH    R1,=H'3'                                                         
         STC   R1,DRHLITL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHLIT(0),DELITRAL                                               
         DROP  R3,RF                                                            
*                                                                               
DDNEXT   ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DD2                                                              
         EJECT                                                                  
* EXPAND USER RECORD INTO DRONE BLOCK                                           
*                                                                               
EXPUSER  NTR1                                                                   
*                                                                               
         LA    R3,IO1              A(USER RECORD)                               
         USING CT02REC,R3                                                       
         LA    R3,CT02DATA         A(FIRST ELEMENT)                             
         DROP  R3                                                               
*                                                                               
DU2      CLI   0(R3),0             CHECK USER ELEMENTS                          
         BE    XIT                                                              
         CLI   0(R3),X'33'                                                      
         BE    DU33                                                             
         CLI   0(R3),X'36'                                                      
         BE    DU36                                                             
         CLI   0(R3),X'87'                                                      
         BE    DU87                                                             
         B     DUNEXT                                                           
*                                                                               
         USING USOLD,R3                                                         
DU33     MVC   DRLENO,USOLEN       OUTPUT LENGTH                                
         B     DUNEXT                                                           
         DROP  R3                                                               
*                                                                               
         USING USOOD,R3                                                         
DU36     CLI   USOONUM,1           OUTPUT OPTIONS                               
         BNE   *+10                                                             
         MVC   DRDECO,USOOVAL                                                   
         CLI   USOONUM,2                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRCOMMAO                                                 
         CLI   USOONUM,3                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRMINUSO                                                 
         CLI   USOONUM,4                                                        
         BNE   *+8                                                              
         OI    DROPTSO,DRZEROO                                                  
         CLI   USOONUM,5                                                        
         BNE   *+10                                                             
         MVC   DRFILLO,USOOVAL                                                  
         CLI   USOONUM,6                                                        
         BNE   *+10                                                             
         MVC   DRFLOATO,USOOVAL                                                 
         CLI   USOONUM,7                                                        
         BNE   DU36B                                                            
         CLI   USOOVAL,C'M'                                                     
         BNE   *+12                                                             
         OI    DROPTSO,DRBKMINO                                                 
         B     DU36B                                                            
         OI    DROPTSO,DRBKALLO                                                 
*                                                                               
DU36B    CLI   USOONUM,8                                                        
         BNE   *+10                                                             
         MVC   DRDIVO,USOOVAL                                                   
         CLI   USOONUM,9                                                        
         BNE   *+10                                                             
         MVC   DRDIVO,USOOVAL                                                   
         CLI   USOONUM,X'20'                                                    
         BNE   DU36D                                                            
         CLI   USOOVAL,C'L'                                                     
         BNE   *+12                                                             
         OI    DROPTSO,DRALGNLO                                                 
         B     DU36D                                                            
         OI    DROPTSO,DRALGNRO                                                 
*                                                                               
DU36D    CLI   USOONUM,X'24'                                                    
         BNE   *+8                                                              
         OI    DROPTSO,DRCHOPO                                                  
         CLI   USOONUM,X'27'                                                    
         BNE   *+10                                                             
         MVC   DRSCALEO,USOOVAL                                                 
         CLI   USOONUM,X'28'                                                    
         BNE   *+10                                                             
         MVC   DRTRAILO,USOOVAL                                                 
         B     DUNEXT                                                           
         DROP  R3                                                               
*                                                                               
         USING DELITD,R3                                                        
DU87     LA    RF,DRHEAD1                                                       
         CLI   DELITLIN,1                                                       
         BE    DU87B                                                            
         LA    RF,DRHEAD2                                                       
         CLI   DELITLIN,2                                                       
         BE    DU87B                                                            
         LA    RF,DRHEAD3                                                       
         CLI   DELITLIN,3                                                       
         BE    DU87B                                                            
         LA    RF,DRHEAD4                                                       
         CLI   DELITLIN,4                                                       
         BE    DU87B                                                            
         DC    H'0'                                                             
*                                                                               
         USING DRHEADD,RF                                                       
DU87B    OI    DRHEAD,X'80'        TURN ON GENERATE BIT                         
         ZIC   R1,DELITLEN         LITERALS                                     
         SH    R1,=H'3'                                                         
         STC   R1,DRHLITL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRHLIT(0),DELITRAL                                               
         DROP  R3,RF                                                            
*                                                                               
DUNEXT   ZIC   R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     DU2                                                              
         EJECT                                                                  
* ERROR ROUTINE                                                                 
*                                                                               
ERR      MVC   DRERROR,DBERRNUM    RETURN ERROR CODE                            
*                                                                               
MYERR    XC    DRERRMSG,DRERRMSG                                                
         TM    DRFLAGS,DRUSERRQ    TEST USER HANDLES HIS OWN MESSAGES           
         BO    ERREX               HE DOES                                      
*                                                                               
         LA    R2,ERRTABLE         TABLE OF ERROR MESSAGES                      
*                                                                               
ERR10    CLC   DRERROR,0(R2)       TEST MATCH ON ERROR CODE                     
         BE    ERR20                                                            
         ZIC   R3,1(R2)            LENGTH OF ERROR MESSAGE                      
         LA    R2,3(R3,R2)         NEXT ERROR MESSAGE                           
         CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BNE   ERR10                                                            
         DC    H'0'                NO MESSAGE FOR THIS CODE                     
*                                                                               
ERR20    ZIC   R3,1(R2)            LENGTH OF ERROR MESSAGE                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DRERRMSG(0),3(R2)   PUT MESSAGE IN PARAMETER BLOCK               
         CLI   OPTNUM,0            TEST ANY OPTION NUMBER                       
         BE    *+14                                                             
         MVC   DRERRMSG+53(1),OPTNUM                                            
         OI    DRERRMSG+53,X'F0'   PUT OPTION NUMBER IN MESSAGE                 
         MVC   DRERRMSG+55(2),=C' *'                                            
         GOTO1 ASQUASH,DMCB,DRERRMSG,60                                         
*                                                                               
ERREX    CLI   MADESW,C'Y'         TEST WE HAVE SWITCHED SYSTEMS                
         BNE   *+8                                                              
         BAS   RE,SWBACK           SWITCH BACK TO PREVIOUS SYSTEM               
         L     RD,SAVERD           RETURN DIRECTLY TO CALLING PROGRAM           
         B     XMOD                                                             
         EJECT                                                                  
* ERROR MESSAGES                                                                
*                                                                               
ERRTABLE DS    0D                                                               
*                                                                               
         DC    AL1(DBARG,MSG1X-MSG1,0)                                          
MSG1     DC    C'* ERROR * INVALID ARGUMENT ON OPTION '                         
MSG1X    EQU   *                                                                
*                                                                               
         DC    AL1(DBNUMOUT,MSG2X-MSG2,0)                                       
MSG2     DC    C'* ERROR * ARGUMENT RANGE IS 1 TO 255 ON OPTION '               
MSG2X    EQU   *                                                                
*                                                                               
         DC    AL1(DBNOENT,MSG3X-MSG3,0)                                        
MSG3     DC    C'* ERROR * INVALID CODE ON OPTION '                             
MSG3X    EQU   *                                                                
*                                                                               
         DC    AL1(ERRNILQ,MSG4X-MSG4,0)                                        
MSG4     DC    C'* ERROR * REQUIRED FIELD MISSING '                             
MSG4X    EQU   *                                                                
*                                                                               
         DC    AL1(ERR2WIDQ,MSG5X-MSG5,0)                                       
MSG5     DC    C'* ERROR * OUTPUT LINE TOO WIDE WITH OPTION '                   
MSG5X    EQU   *                                                                
*                                                                               
         DC    AL1(ERRCLOSE,MSG6X-MSG6,0)                                       
MSG6     DC    C'* ERROR * CONTROL SYSTEM NOT STARTED '                         
MSG6X    EQU   *                                                                
*                                                                               
         DC    AL1(ERRINVOP,MSG7X-MSG7,0)                                       
MSG7     DC    C'* ERROR * INVALID OPTION '                                     
MSG7X    EQU   *                                                                
*                                                                               
         DC    AL1(ERRCOMP,MSG8X-MSG8,0)                                        
MSG8     DC    C'* ERROR * INVALID COMPUTATIONAL EXPRESSION '                   
MSG8X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 3                                                                
SPACES   DC    CL132' '                                                         
         SPACE 3                                                                
         DS    0D                                                               
FLIST    DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFILE'                                                    
         DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DMWORK   DS    12D                                                              
RELO     DS    F                                                                
SAVERD   DS    F                                                                
DMCB     DS    6F                                                               
ADRIVAL  DS    A                   A(DRIVAL)                                    
ASQUASH  DS    A                   A(SQUASHER)                                  
AIO      DS    A                   A(I/O AREA)                                  
WORK     DS    XL256                                                            
DUB      DS    D                                                                
DATADISP DS    H                   FOR GETEL MACRO                              
ELCODE   DS    X                   FOR GETEL MACRO                              
BYTE     DS    X                                                                
KEY      DS    XL40                                                             
KEYSAVE  DS    XL40                                                             
SCANBLK  DS    20XL74              LOOKS LIKE DPG SCANNER BLOCK                 
FLDGLEN  DS    X                   USED BY FLDG MACRO                           
SCANLEN  DS    X                   NUMBER OF SCANNER ENTRIES                    
SAVESE   DS    X                   SAVE SYSTEM NUMBER FOR FASWITCH              
OPTNUM   DS    X                   OPTION NUMBER (FOR ERROR MESSAGES)           
USEDALT  DS    C                   'Y' IF ALTERNATE DICTIONARY WAS USED         
MADESW   DS    C                   'Y' IF PERFORMED FASWITCH                    
IO1      DS    2000X               I/O AREA FOR CONTROL FILE GENFIL             
         SPACE 5                                                                
       ++INCLUDE DRIVALBLKD                                                     
         SPACE 5                                                                
BUFFER   DS    1024X               GENERATE USELESS ELEMENTS HERE               
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
DRONED   DSECT                                                                  
       ++INCLUDE DRONEBLKD                                                      
         PRINT OFF                                                              
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE CTGENDIC                                                       
       ++INCLUDE CTGENUSER                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DDRONEA   05/01/02'                                      
         END                                                                    
