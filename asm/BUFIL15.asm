*          DATA SET BUFIL15    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T50215A                                                                  
         TITLE 'T50215 - BUDGET CONTROL LFM - OUTLINE LIST'                     
T50215   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI15**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     GOTO1 VUPKEY                                                           
*                                                                               
         LA    R2,OULCLTH          VALIDATE CLIENT                              
         GOTO1 VVALCLT,PARAS,(R2),0                                             
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
*                                                                               
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VKEY2    LA    R2,OULPRDH          VALIDATE PRODUCT                             
         GOTO1 VVALPRD,PARAS,(R2),0                                             
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
*                                                                               
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VKEY4    LA    R2,OULPLAH                                                       
         GOTO1 VVALPLAN,PARAS,(R2),0                                            
         BAS   RE,TSTNEWPL         TEST FOR NEW PLAN                            
         BE    *+10                                                             
         XC    MOVEVALS,MOVEVALS   YES-CLEAR MOVEMENT VALUES                    
         MVC   SVPLNKEY,NODKEY                                                  
         MVC   SVPLNVAL,PLANVALS   SAVE PLAN VALUES                             
*                                                                               
* EDIT AND VALIDATE OUTLINE CODE                                                
*                                                                               
VKEY6    LA    R2,OULCODH                                                       
         XC    SVOUTKEY,SVOUTKEY   CLEAR SAVE OUTLINE KEY START                 
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VKEY10                                                           
         MVC   OUTCODE,FLD                                                      
*                                                                               
VKEY8    GOTO1 VFINDOUT,PARAS,OUTCODE,NDIOA                                     
         BNE   TRAPERR                                                          
         GOTO1 VTRACE              GET NODAL KEY AND READ THROUGH NODIO         
         GOTO1 VGETVAL                                                          
         MVC   SVOUTKEY,NODKEY     SAVE START OUTLINE NODAL KEY                 
*                                                                               
* EDIT OUTLINE LIST OPTIONS                                                     
*                                                                               
VKEY10   XC    SVFILLEV,SVFILLEV   CLEAR SAVED LEVEL FILTER                     
         GOTO1 OPTED,PARAS,OULOPTH,LOPTTAB                                      
*                                                                               
VKEYX    B     XIT                                                              
*                                                                               
TSTNEWPL CLI   TWALREC,RECOUT      TEST IF CONTINUING SAME PLAN                 
         BNER  RE                                                               
         CLI   TWALACT,ACTLIST                                                  
         BNER  RE                                                               
         CLC   SVPLNKEY,NODKEY                                                  
         BR    RE                                                               
         EJECT                                                                  
* LIST OUTLINE RECORDS                                                          
*                                                                               
LIST     MVI   NDUPDTSW,NO         DO NOT READ FOR UPDATE                       
         MVI   NLISTS,14           OVERRIDE N'LIST LINES ON SCREEN              
         LA    RE,SVOUTLST         POINT TO OUTLINE TABLE                       
         MVC   FRSTKEY,OLNODKEY-OLISTD(RE)  EXTRACT FIRST KEY                   
*                                                                               
         CLI   GCMODE,C'S'         TEST CALLED FROM BASE                        
         BE    LIST2               YES-KNOW WHERE TO START MOVE                 
*                                                                               
         CLI   THISPF,PF7          TEST FOR PREVIOUS PAGE                       
         BNE   LIST1               NO                                           
         LR    RE,R7                                                            
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R2,CONACTH                                                       
         CR    RE,R2               TEST IF KEY FIELDS CHANGED                   
         BH    LIST1               YES-START LIST AGAIN                         
         OC    PLANCNT,PLANCNT     TEST FOR ZERO OUTLINES                       
         BZ    LIST1               YES-IGNORE PREVIOUS PAGE                     
         CLI   TRACEOPT,YES        TEST FOR TRACE OPTION                        
         BE    LIST2               YES-IGNORE SCROLLING                         
         B     LIST8               GO SCROLL BACKWARDS                          
*                                                                               
LIST1    OC    KEY,KEY             TEST FOR FIRST TIME                          
         BZ    *+12                YES                                          
         CLI   THISPF,PF1          NO-TEST TO RE-START LIST                     
         BNE   LIST2               NO-CONTINUE LIST                             
*                                                                               
         BAS   RE,INIT             INITIALIZE FOR LIST                          
         MVC   NODKEY,SVPLNKEY     GET PLAN'S KEY TO START                      
         OC    SVOUTKEY,SVOUTKEY   TEST START OUTLINE SPECIFIED                 
         BZ    *+10                NO                                           
         MVC   NODKEY,SVOUTKEY     YES-GET OUTLINE'S KEY                        
*                                                                               
         LA    R1,LISTHK           SET IN-LINE HOOK                             
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST4                                                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    XIT                 YES-NO MORE RECORDS SO EXIT                  
         LA    R2,OULCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* RE-SET FOR SEQUENTIAL WITH A READ                                             
*                                                                               
LIST2    BAS   RE,INIT                                                          
         MVC   NODKEY,SVNKEY       GET LAST SAVED NODAL KEY                     
         XC    NDHOOK,NDHOOK                                                    
         CLI   GCMODE,C'S'         TEST CALLED FROM BASE PROGRAM                
         BNE   LIST3                                                            
         LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         MVC   FILTLEV,SVFILLEV    RESTORE SAVED LEVEL FILTER                   
*                                                                               
LIST3    GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST4                                                            
         LA    R2,OULCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* READ SEQUENTIAL TO FILL SCREEN OR UP TO EOF                                   
*                                                                               
LIST4    LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         MVI   NDSQBACK,3          KEEP READING UNTIL NEXT PLAN                 
         OC    NODCOMM,NODCOMM     TEST IF OVERRIDE COMMAND GIVEN               
         BNZ   *+10                                                             
         MVC   NODCOMM,=CL8'LSEQ'                                               
         GOTO1 VNODIO,DMCB,NODBLKD,NODCOMM,NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,OULCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* GO BACK TO PREVIOUS SCREEN                                                    
*                                                                               
LIST8    BAS   RE,BACK             READ BACKWARDS                               
         CLI   NKEYS,0             TEST IF NO KEYS FOUND                        
         BE    *+8                                                              
         BAS   RE,INVERT           NO-INVERT LIST OF THOSE FOUND                
         BAS   RE,MERGE            MERGE WITH EXISTING SCREEN IF NEEDED         
*                                                                               
LIST9    BAS   RE,INIT                                                          
         ZIC   R2,NKEYS            R2=N'KEYS ON PREVIOUS SCREEN                 
         L     R3,AIO3             R3=A(KEY LIST FOR PREVIOUS SCREEN)           
         LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
*                                                                               
LIST10   MVC   NODKEY,0(R3)                                                     
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,L'NODKEY(R3)     NEXT KEY                                     
         BCT   R2,LIST10                                                        
         B     XIT                                                              
*                                                                               
* HOOK TO PROCESS RECORDS RETURNED BY NODIO                                     
*                                                                               
LISTHK   ST    RE,SAVERE                                                        
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT     MAKE SURE IT IS AN OUTLINE                   
         BZ    LISTHKX                                                          
*                                                                               
         CLI   NDMODE,NDPROC                                                    
         BNE   LISTHKX                                                          
*                                                                               
         GOTO1 VGETVAL                                                          
*                                                                               
         OC    FILTLEV,FILTLEV     TEST FOR OUTLINE LEVEL FILTER                
         BZ    LISTHK1             NO                                           
         CLC   OUTLEV,FILTLO       TEST OUTLINE LEVEL BELOW MINIMUM             
         BL    LISTHKX             YES-EXIT                                     
         CLC   OUTLEV,FILTHI       TEST OUTLINE LEVEL ABOVE MAXIMUM             
         BH    LISTHKX                                                          
*                                                                               
LISTHK1  L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),NDLVKEY EXTRACT KEY                                 
         MVC   SVNKEY,NODKEY       EXTRACT NODAL KEY                            
         MVC   DMDSKADD,NDLVDA     AND THE DISK ADDRESS                         
         LA    RF,DISLIST          NORMAL LIST DISPLAY                          
         CLI   TRACEOPT,YES        TEST FOR NODAL TRACE                         
         BNE   *+8                                                              
         LA    RF,TRALIST          YES                                          
         BASR  RE,RF                                                            
*                                                                               
         L     RE,AOUTLST          POINT TO NEXT OUTLINE LIST ENTRY             
         USING OLISTD,RE                                                        
         MVC   OLNODKEY,NODKEY     OUTLINE'S NODAL KEY                          
         MVC   OLNODE,NDLVNOD      OUTLINE'S NODE                               
         CLI   OUTLEV,MAXOUTS      TEST AT LOWEST LEVEL                         
         BE    *+14                YES-CANNOT BE A PARENT                       
         LA    R1,NDLVTABL(R3)     R1=NEXT LEVEL TABLE ENTRY                    
         MVC   OLNODE2,NDLVNOD-NDLVTABD(R1) NODE ESTABLISHED BY OUTLINE         
         MVC   OLCODE,OUTCODE      AND CODE                                     
         MVC   OLEVEL,OUTLEV       SET OUTLINE LEVEL                            
         LA    RE,OLISTLNQ(RE)     UPDATE ENTRY POINTER                         
         ST    RE,AOUTLST                                                       
*                                                                               
         ZIC   R1,SVNOUTS                                                       
         LA    R1,1(R1)            INCREMENT N'SAVED OUTLINES                   
         STC   R1,SVNOUTS                                                       
*                                                                               
LISTHK2  L     R2,ATHISSEL                                                      
         CLC   FROMCODE,NDLVCOD    TEST IF OUTLINE IS PART OF                   
         BNE   LISTHK3             PENDING MOVE                                 
         MVC   8(L'OULSEL,R2),SPACES                                            
         MVC   8(2,R2),=C'*M'                                                   
         OI    6(R2),X'80'         XMIT FIELD BACK                              
         B     LISTHK4                                                          
*                                                                               
LISTHK3  CLC   TOOUTCOD,NDLVCOD    TEST IF OUTLINE IS MOVE 'TO'                 
         BNE   LISTHK4             POSITION                                     
         MVC   8(L'OULSEL,R2),SPACES                                            
         MVI   8(R2),STAR                                                       
         MVC   9(1,R2),POSCOMM                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
LISTHK4  BAS   RE,BUMPU            BUMP TO NEXT SELECT FIELD                    
         BE    *+8                 AT EOS                                       
         ST    R2,ATHISSEL                                                      
         GOTO1 LISTMON                                                          
         CLC   LISTNUM,NLISTS      TEST IF SCREEN FILLED(SLAVE MODE)            
         BNL   XIT                 YES-UNWIND OUT OF OVERLAY                    
*                                                                               
LISTHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RE                                                               
*                                                                               
LISTHD   DC    C'CODE                OUTLINE NAME'                              
TRACEHD  DC    C'CODE          NODE  D/A      FORWARD  BACKWARD ATTCH FX        
               IRST    LAST'                                                    
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE FOR LIST                                            
*                                                                               
INIT     ST    RE,SAVERE                                                        
         MVI   SVNOUTS,0           CLEAR N'SAVED OUTLINE ENTRIES                
         LA    RE,SVOUTLST         CLEAR THE OUTLINE LIST TABLE                 
         ST    RE,AOUTLST          SET ADDRESS OF NEXT LIST ENTRY               
         LA    RF,15*OLISTLNQ                                                   
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE LIST                               
         LA    R1,SVOUTLST-SYSD    DISPLACEMENT TO OUTLINE TABLE                
         STH   R1,DISPLTAB                                                      
         LA    R2,OULSELH          SET POINTER TO SELECT FIELD                  
         ST    R2,ATHISSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BACK-UP TO START OF PREVIOUS PAGE                              
*                                                                               
BACK     NTR1                                                                   
         MVI   NKEYS,0             INITIALIZE N'KEYS FOUND                      
         MVC   NODKEY,FRSTKEY      RE-READ FIRST OUTLINE ON THIS PAGE           
         XC    NDHOOK,NDHOOK       DO NOT WANT TO DISPLAY IT                    
         CLI   SVNOUTS,0           TEST IF ANY OUTLINES ON SCREEN               
         BNE   BACK1               YES                                          
         LA    R1,BACKHK           NO-WANT TO DISPLAY OUTLINE                   
         ST    R1,NDHOOK                                                        
         MVC   NODKEY,SVNKEY       READ LAST OUTLINE LISTED                     
*                                                                               
BACK1    GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BACK2    LA    R1,BACKHK                                                        
         ST    R1,NDHOOK                                                        
         MVI   NDSQBACK,3          READ BACK UNTIL PLAN                         
         MVI   NDSKIP,YES                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'BSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    BACKX                                                            
         CLI   NDERR,NDRNFERR                                                   
         BE    BACKX                                                            
         LA    R2,OULCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
BACKX    B     XIT                                                              
         SPACE 1                                                                
* HOOK ROUTINE FOR READ BACKWARDS CODE                                          
*                                                                               
BACKHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDPROC                                                    
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT     MAKE SURE IT IS AN OUTLINE                   
         BZ    BACKHKX                                                          
         GOTO1 VGETVAL                                                          
         OC    FILTLEV,FILTLEV     TEST FOR LEVEL FILTER                        
         BZ    BACKHK2             NO                                           
         CLC   OUTLEV,FILTLO                                                    
         BL    BACKHKX                                                          
         CLC   OUTLEV,FILTHI                                                    
         BH    BACKHKX                                                          
*                                  KEEP NODAL KEYS AT IO3                       
BACKHK2  L     RE,AIO3                                                          
         ZIC   RF,NKEYS                                                         
         LR    R1,RF                                                            
         LA    R1,1(R1)            INCREMENT COUNT OF RECORDS FOUND             
         STC   R1,NKEYS                                                         
         MH    RF,=Y(L'NODKEY)                                                  
         LA    RE,0(RE,RF)         INDEX INTO LIST                              
         MVC   0(L'NODKEY,RE),NODKEY                                            
         CLC   NKEYS,NLISTS        TEST IF SCREEN IS FULL                       
         BL    *+8                                                              
         MVI   NDMODE,NDEND        YES-FORCE NODIO TO QUIT                      
*                                                                               
BACKHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO INVERT THE LIST OF NODAL KEYS AT IO3                           
*                                                                               
INVERT   NTR1                                                                   
         ZIC   R1,NKEYS            GET N'KEYS IN LIST                           
         BCTR  R1,0                                                             
         LA    R0,L'NODKEY                                                      
         MR    R0,R0               R1=INDEX TO LAST KEY                         
         L     RE,AIO3                                                          
         LA    R1,0(RE,R1)         R1=A(LAST KEY)                               
         LA    RE,L'NODKEY                                                      
         LNR   RE,RE               RE=BXH INCREMENT                             
         L     RF,AIO3                                                          
         BCTR  RF,0                RF=BXH LIMIT                                 
         L     R2,AIO2             PUT THE LIST AT IO2                          
*                                                                               
INVERT2  MVC   0(L'NODKEY,R2),0(R1)                                             
         LA    R2,L'NODKEY(R2)                                                  
         BXH   R1,RE,INVERT2                                                    
*                                                                               
INVERTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO MERGE LIST OF KEYS ON PREVIOUS PAGE WITH                       
* LIST OF KEYS ON THIS PAGE IF NEEDED UNTIL PAGE IS FILLED.                     
* IT MOVES FINISHED LIST TO IO3                                                 
*                                                                               
MERGE    NTR1                                                                   
         CLC   NKEYS,NLISTS        TEST IF PAGE FILLED ALREADY                  
         BE    MERGE4                                                           
         CLI   SVNOUTS,0           TEST IF NOTHING ON CURRENT SCREEN            
         BE    MERGE4              YES-NOTHING TO MERGE                         
*                                                                               
         L     RE,AIO2                                                          
         ZIC   R1,NKEYS                                                         
         LA    R0,L'NODKEY                                                      
         MR    R0,R0                                                            
         LA    RE,0(R1,RE)         RE=A(NEXT OUTLINE POSITION)                  
*                                                                               
MERGE1   ZIC   R1,NLISTS                                                        
         ZIC   R0,NKEYS                                                         
         ZIC   R2,SVNOUTS                                                       
         SR    R1,R0               R1=N'AVAILABLE LINES ON SCREEN               
         CR    R1,R2               TEST AGAINST N'AVAILABLE LINES               
         BNH   *+6                                                              
         LR    R1,R2               CANNOT BE MORE THAN WHAT WE HAVE             
         AR    R0,R1               UPDATE N'KEYS ON PREVIOUS SCREEN             
         STC   R0,NKEYS                                                         
*                                                                               
MERGE2   LA    RF,SVOUTLST                                                      
         USING OLISTD,RF                                                        
*                                                                               
MERGE3   MVC   0(L'NODKEY,RE),OLNODKEY                                          
         LA    RE,L'NODKEY(RE)                                                  
         LA    RF,OLISTLNQ(RF)                                                  
         BCT   R1,MERGE3                                                        
*                                                                               
MERGE4   ZIC   R1,NKEYS            MOVE MERGED LIST FROM IO3 TO IO2             
         LA    R0,L'NODKEY                                                      
         MR    R0,R0                                                            
         L     R0,AIO3                                                          
         LR    RF,R1                                                            
         L     RE,AIO2                                                          
         MVCL  R0,RE                                                            
*                                                                               
MERGEX   B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A LINE ON LIST SCREEN                                  
*                                                                               
DISLIST  NTR1                                                                   
         MVC   OULHED,SPACES       CLEAR LIST HEADLINE                          
         MVC   OULHED(L'LISTHD),LISTHD SET LIST HEADLINE                        
         OI    OULHEDH+6,X'80'     XMIT                                         
         LA    RE,LISTAR                                                        
         USING LSTLIND,RE                                                       
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,OUTLEV                                                        
         BCTR  R1,0                                                             
         SLL   R1,1                INDEX IS (LEVEL-1)*2                         
         LA    RF,LSTCOD(R1)                                                    
         MVC   0(L'OUTCODE,RF),OUTCODE                                          
         OC    0(L'OUTCODE,RF),SPACES                                           
         LA    RF,LSTNAM(R1)                                                    
         MVC   0(L'OUTNAME,RF),OUTNAME                                          
*                                                                               
DISLIST2 MVI   ELCODE,BUPTRELQ     BAD PASSIVE POINTER CHECK                    
         L     R4,NDIOA                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         LA    R2,BUPOINT                                                       
         USING BUCRECD,R2                                                       
         CLC   BUCCODE,OUTCODE     TEST CODE IN ELEM MATCHES KEY                
         BE    DISLISTX                                                         
         MVC   LSTNAM(26),=CL26'** BAD POINTER XXXXXXXX **'                     
         MVC   LSTNAM+15(8),BUCCODE                                             
*                                                                               
DISLISTX B     XIT                                                              
         DROP  R2,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A NODAL TRACE FOR AN OUTLINE                           
* AT ENTRY, R3=A(LEVEL TABLE ENTRY), R4=A(RECORD)                               
*                                                                               
TRALIST  NTR1                                                                   
         MVC   OULHED,SPACES       CLEAR LIST HEADLINE                          
         MVC   OULHED(L'TRACEHD),TRACEHD SET LIST HEADLINE                      
         OI    OULHEDH+6,X'80'     XMIT                                         
         LA    R2,LISTAR                                                        
         USING TRALIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,OUTLEV                                                        
         BCTR  R1,0                                                             
         LA    RF,TRACOD(R1)                                                    
         MVC   0(L'OUTCODE,RF),OUTCODE                                          
         OC    0(L'OUTCODE,RF),SPACES                                           
         GOTO1 HEXOUT,DMCB,BUKNODE+1,DUB,3,=C'TOG'                              
         MVC   TRANODE,DUB+1                                                    
         GOTO1 (RF),DMCB,DMDSKADD,TRADA,4,=C'TOG'                               
         LA    R6,BUFRSTEL                                                      
*                                                                               
TRALIST2 CLI   0(R6),0             TEST FOR EOR                                 
         BE    TRALISTX                                                         
*                                                                               
         CLI   0(R6),X'B8'         TEST FOR FORWARD ATTACHMENT ELEM             
         BNE   TRALIST3                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRAFWD(0),2(R6)     EXTRACT CODE                                 
         B     TRALIST9                                                         
*                                                                               
TRALIST3 CLI   0(R6),X'B9'         TEST FOR BACKWARD ATTACHMENT ELEM            
         BNE   TRALIST4                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRABKWD(0),2(R6)                                                 
         B     TRALIST9                                                         
*                                                                               
TRALIST4 CLI   0(R6),X'B1'         TEST FOR NODE ATTACHMENT ELEM                
         BNE   TRALIST5                                                         
         GOTO1 HEXOUT,DMCB,3(R6),DUB,3,=C'TOG'                                  
         MVC   TRAATT,DUB+1                                                     
         B     TRALIST9                                                         
*                                                                               
TRALIST5 CLI   0(R6),X'B6'         TEST FOR FIRST CODE AT NEXT LEVEL            
         BNE   TRALIST6                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRAFIRST(0),2(R6)                                                
         B     TRALIST9                                                         
*                                                                               
TRALIST6 CLI   0(R6),X'B7'         TEST FOR LAST CODE AT NEXT LEVEL             
         BNE   TRALIST7                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRALAST(0),2(R6)                                                 
*                                                                               
TRALIST7 CLI   0(R6),BUPTRELQ                                                   
         BNE   TRALIST9                                                         
         USING BUPTRD,R6                                                        
         LA    RE,BUPOINT                                                       
         USING BUCRECD,RE                                                       
         CLC   BUCCLT,CLTCODE      TEST FOR CORRECT POINTER                     
         BNE   TRALIST8                                                         
         CLC   BUCPRD,PRDCODE                                                   
         BNE   TRALIST8                                                         
         CLC   BUCPLAN,PLANCODE                                                 
         BNE   TRALIST8                                                         
         CLC   BUCCODE,OUTCODE     TEST CODE IN ELEM MATCHES KEY                
         BE    TRALIST9                                                         
*                                                                               
TRALIST8 MVC   TRANODE(TRALINX-TRANODE),SPACES                                  
         MVC   TRANODE(15),=CL15'** BAD POINTER'                                
         MVC   TRANODE+15(3),BUCCLT                                             
         MVC   TRANODE+19(3),BUCPRD                                             
         MVC   TRANODE+23(3),BUCPLAN                                            
         MVC   TRANODE+27(8),BUCCODE                                            
         MVC   TRANODE+36(2),=C'**'                                             
         OC    LISTAR,SPACES                                                    
         B     TRALISTX                                                         
*                                                                               
TRALIST9 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     TRALIST2                                                         
*                                                                               
TRALISTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT OUTLINE LIST OPTIONS                                      
*                                                                               
* P1 = A(OPTION FIELD HEADER)                                                   
* P2 = A(OPTION TABLE)                                                          
*                                                                               
OPTED    NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(OPTION FLDH),R3=A(OPTION TABLE)         
         ST    R3,AOPTTAB          SAVE A(OPTION TABLE)                         
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    OPTEDX                                                           
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
OPTED2   XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'     LOOK FOR EQUALS SIGN OR COMMA                
         GOTO1 VFVAL                                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   OPTED4                                                           
         CLI   FSTOP,X'FF'         TEST FOR EOF                                 
         BE    OPTEDX              YES                                          
         B     OPTEDR                                                           
*                                                                               
OPTED4   CLI   FLDH+5,L'OPTNAME    VALIDATE THE KEYWORD                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     R3,AOPTTAB          R3=A(OPTION TABLE)                           
         USING OPTTABD,R3                                                       
*                                                                               
OPTED6   CLI   OPTNAME,X'FF'       TEST EOT                                     
         BE    OPTEDR                                                           
         CLC   FLDH+5(1),OPTMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    OPTED6A             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BNE   OPTED6A             VALID KEYWORD                                
         TM    OPTCTL,DDSONLY      TEST FOR DDS ONLY OPTION                     
         BZ    OPTED7              NO-FOUND A VALID KEY                         
         CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BE    OPTED7              YES-SO ALLOW OPTION                          
OPTED6A  LA    R3,OPTTABL(R3)                                                   
         B     OPTED6                                                           
*                                                                               
OPTED7   TM    OPTCTL,KEYONLY      TEST FOR KEYWORD ONLY OPTION                 
         BZ    *+16                NO                                           
         CLI   FSTOP,EQUALS        YES-TEST IF EQUALS SIGN AFTER OPTION         
         BE    OPTEDR              YES-MUST BE ERROR                            
         B     OPTED8                                                           
*                                                                               
         XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BNE   OPTED8              YES                                          
         MVC   XTRA(12),=C'NO VALUE FOR'                                        
         MVC   XTRA+13(L'OPTNAME),OPTNAME                                       
         B     OPTEDR                                                           
*                                                                               
OPTED8   SR    RF,RF                                                            
         ICM   RF,3,OPTAED         GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     OPTED2                                                           
*                                                                               
OPTEDX   B     XIT                                                              
         SPACE 2                                                                
OPTEDR   B     SPERR                                                            
         EJECT                                                                  
* LIST OPTION VALUE EDITS                                                       
*                                                                               
OPTLEV   ST    RE,SAVERE                                                        
         CLI   FLDH+5,3            TEST MORE THAN 3 DIGITS                      
         BH    OPTEDR              YES                                          
         CLI   FLDH+5,2            TEST TWO DIGITS                              
         BE    OPTEDR              YES-MUST BE AN ERROR                         
         MVI   BYTE,MAXOUTS        SET OUTLINE LEVEL MAXIMUM                    
         OI    BYTE,X'F0'          MAKE CHARACTER                               
         SPACE 1                                                                
OPTLEV2  CLI   FLD,C'1'            TEST VALID LEVEL                             
         BL    OPTEDR                                                           
         CLC   FLD(1),BYTE                                                      
         BH    OPTEDR                                                           
         MVC   FILTLO,FLD                                                       
         NI    FILTLO,X'0F'        ISOLATE BINARY NUMBER                        
         MVC   FILTHI,FILTLO       SET HIGH FILTER=LOW FILTER                   
*                                                                               
         CLI   FLDH+5,1            TEST FOR ONE DIGIT INPUT                     
         BE    OPTLEV4             YES                                          
         CLI   FLD+1,C'-'          TEST DASH SEPARATES NUMBERS                  
         BNE   OPTEDR                                                           
*                                                                               
         CLI   FLD+2,C'1'          TEST VALID LEVEL FOLLOWS                     
         BL    OPTEDR                                                           
         CLC   FLD+2(1),BYTE                                                    
         BH    OPTEDR                                                           
         MVC   FILTHI,FLD+2                                                     
         NI    FILTHI,X'0F'                                                     
         CLC   FILTLO,FILTHI       TEST LEVELS ARE IN ORDER                     
         BH    OPTEDR                                                           
         SPACE 1                                                                
OPTLEV4  MVC   SVFILLEV,FILTLEV                                                 
         B     OPTX                                                             
         SPACE 2                                                                
OPTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
OPTTRA   MVI   TRACEOPT,YES                                                     
         BR    RE                                                               
         SPACE 2                                                                
OPTCOM   MVC   NODCOMM(L'NODCOMM),OPTNAME                                       
         BR    RE                                                               
         SPACE 2                                                                
NOCOMP   CLC   FLD(0),=C'NO '                                                   
YESCOMP  CLC   FLD(0),=C'YES'                                                   
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         OI    6(R2),X'C0'         IN SLAVE MODE HERE                           
         GOTO1 SAVEUWKA                                                         
         L     RD,AWORK                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 1                                                                
BUMPU    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    BUMPU                                                            
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
         EJECT                                                                  
* TABLE OF OUTLINE LIST OPTIONS (COVERED BY OPTTABD)                            
*                                                                               
LOPTTAB  DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'LEVEL   ',AL1(1),AL1(0),AL1(0)                               
         DC    AL2(0),AL2(OPTLEV-T50215),CL3'   '                               
*                                                                               
         DC    CL8'TRACE   ',AL1(1),AL1(KEYONLY+DDSONLY),AL1(0)                 
         DC    AL2(TRACEOPT-SYSD),AL2(OPTTRA-T50215),CL3'   '                   
*                                                                               
         DC    CL8'LSEQ    ',AL1(2),AL1(KEYONLY+DDSONLY),AL1(0)                 
         DC    AL2(0),AL2(OPTCOM-T50215),CL3'   '                               
*                                                                               
         DC    CL8'BSEQ    ',AL1(1),AL1(KEYONLY+DDSONLY),AL1(0)                 
         DC    AL2(0),AL2(OPTCOM-T50215),CL3'   '                               
*                                                                               
         DC    CL8'SEQ     ',AL1(1),AL1(KEYONLY+DDSONLY),AL1(0)                 
         DC    AL2(0),AL2(OPTCOM-T50215),CL3'   '                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILE5D                                                       
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
*                                                                               
AOPTTAB  DS    A                   A(OPTION TABLE)                              
AOUTLST  DS    A                   A(NEXT OUTLINE LIST TABLE ENTRY)             
ATHISSEL DS    A                   A(THIS SELECT FIELD)                         
*                                                                               
FILTLEV  DS    0XL2                OUTLINE LEVEL FILTER (LIST)                  
FILTLO   DS    X                                                                
FILTHI   DS    X                                                                
TRACEOPT DS    C                   Y=NODAL DATA TRACE (DDS TERMINALS)           
NODCOMM  DS    CL(L'OPTNAME)       NODIO SEQ COMMAND (DEFAULT=LSEQ)             
*                                                                               
NKEYS    DS    X                                                                
FRSTKEY  DS    CL(L'NODKEY)                                                     
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
       ++INCLUDE BUFILOUTSV                                                     
* DSECT TO COVER OPTION TABLE                                                   
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                 NAME                                         
OPTMINL  DS    X                   MINIMUM LENGTH OF VALID KEYWORD              
OPTCTL   DS    X                   CONTROL VALUES                               
*                                  X'80'=KEYWORD ONLY OPTION                    
*                                  X'40'=DDS ONLY OPTION                        
*                                  X'01'=1 DIGIT NUMERIC VALUE                  
OPTEQU   DS    X                   BIT SETTING FOR BUOUTIND                     
OPTAVAL  DS    AL2                 DISPL. TO VALUE IN SYSD                      
OPTAED   DS    AL2                 DISPL. TO PARM VALIDATION RTN.               
OPTPARM  DS    CL3                 PARM VALUE FOR DISPLAY                       
OPTTABL  EQU   *-OPTTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTCOD   DS    CL18                OUTLINE CODE                                 
         DS    CL2                 SPARE                                        
LSTNAM   DS    CL30                OUTLINE NAME                                 
         SPACE 2                                                                
* DSECT TO COVER TRACE LIST LINE                                                
*                                                                               
TRALIND  DSECT                                                                  
TRACOD   DS    CL13                OUTLINE CODE                                 
         DS    C                                                                
TRANODE  DS    CL5                 OUTLINE'S NODE                               
         DS    C                                                                
TRADA    DS    CL8                 DISK ADDRESS                                 
         DS    C                                                                
TRAFWD   DS    CL8                 FORWARD POINTER (CURRENT LEVEL)              
         DS    C                                                                
TRABKWD  DS    CL8                 BACKWARD POINTER (CURRENT LEVEL)             
         DS    C                                                                
TRAATT   DS    CL5                 NODE OF ATTACHED LEVEL                       
         DS    C                                                                
TRAFIRST DS    CL8                 FIRST CODE (NEXT LEVEL)                      
         DS    C                                                                
TRALAST  DS    CL8                 LAST CODE (NEXT LEVEL)                       
TRALINX  EQU   *                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
COMMA    EQU   C','                                                             
KEYONLY  EQU   X'80'                                                            
DDSONLY  EQU   X'40'                                                            
PF1      EQU   X'01'                                                            
PF7      EQU   X'07'                                                            
PF8      EQU   X'08'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020BUFIL15   05/01/02'                                      
         END                                                                    
