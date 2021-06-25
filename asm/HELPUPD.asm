*          DATA SET HELPUPD    AT LEVEL 067 AS OF 05/01/02                      
*PHASE HELPUPD,*,NOAUTO                                                         
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
         TITLE 'HELPUPD -- HELP RECORD UPDATE'                                  
*HIPO******************************************************************         
*                                                                     *         
*  TITLE:        HELPUPD -- HELP RECORD UPDATE PROGRAM                *         
*                                                                     *         
*  COMMENTS:     UPDATES EXISTING HELP RECORDS BASED UPON AN INPUT    *         
*                FILE OF HELP TEXT.                                   *         
*                                                                     *         
*  CALLED FROM:  OVERNIGHT REQUEST                                    *         
*                                                                     *         
*  CALLS TO:     DATAMGR                                              *         
*                                                                     *         
*  INPUTS:       HELP TEXT FILE                                       *         
*                                                                     *         
*  OUTPUTS:      UPDATED HELP RECORDS (CONTROL FILE)                  *         
*                FILE UPDATE REPORT                                   *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
*                R8 -- WORK                                           *         
*                R9 -- DPRINT                                         *         
*                RA -- SECOND BASE                                    *         
*                RB -- FIRST BASE                                     *         
*                RC -- WORK                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
*  LOGIC:        FAULTY                                               *         
*                                                                     *         
*ENDHIPO***************************************************************         
         EJECT                                                                  
HELPUPD  CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE 0,**HELP**,=V(REGSAVE),RA                                        
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         XC    DUB,DUB                                                          
         ST    RB,DUB                                                           
         L     R1,=V(REGSAVE)                                                   
         A     R1,=F'20000'                                                     
         ST    R1,DUB+4                                                         
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
         B     RDCARDS                                                          
         SPACE 5                                                                
DUMPLIST DS    0F                                                               
         DC    A(HELPUPD,65000)                                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
RDCARDS  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    INIT                                                             
*                                                                               
         CLC   =C'WRITE=NO',CARD                                                
         BNE   *+16                                                             
         MVI   FLIST,C' '          DON'T WRITE TO CTFILE                        
         MVI   WRITESW,C'N'                                                     
         B     RDCARDS                                                          
*                                                                               
         CLC   =C'PROTECT=YES',CARD                                             
         BNE   *+12                                                             
         MVI   PROTSW,C'Y'         TURN ON PROTECT FLAG                         
         B     RDCARDS                                                          
*                                                                               
         CLC   =C'RESTRICT=YES',CARD                                            
         BNE   *+12                                                             
         MVI   RESTSW,C'Y'         TURN ON RESTRICT FLAG                        
         B     RDCARDS                                                          
*                                                                               
         CLC   =C'TEST=YES',CARD                                                
         BNE   BADCARD                                                          
         MVI   TESTSW,C'Y'         WRITE HELP RECORDS TO TESTOUT                
         OPEN  (TESTOUT,(OUTPUT))                                               
         B     RDCARDS                                                          
*                                                                               
BADCARD  MVC   PMESS,=CL28'UNKNOWN PARAMETER CARD'                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
INIT     MVC   TITLE(25),=CL25'HELP SCREEN UPDATE REPORT'                       
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,IO              
*                                                                               
         B     READKEY                                                          
         SPACE 5                                                                
FLIST    DS    0H                                                               
         DC    CL8'UCTFILE '                                                    
         DC    CL8'UCTRCVR '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
READKEY  OPEN  (INFILE,(INPUT))    OPEN HELP TEXT FILE                          
*                                                                               
         MVC   PMESS,=CL28'FIRST RECORD MUST BE A KEY'                          
         MVI   RDHELPSW,C'K'       LOOK FOR THE FIRST KEY                       
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    ERREOJ                                                           
         CLI   RDHELPSW,C'K'       TEST A KEY WAS FOUND                         
         BNE   ERREOJ                                                           
*                                                                               
RDKEY20  XC    WORK,WORK           KEY PORTION                                  
         MVI   WORK,8              LENGTH OF FAKED TWA FIELD SO FAR             
         LA    R2,WORK                                                          
         MVC   HEADING,SPACES      HEADING PORTION                              
         LA    R1,CARD+6           PARSE CARD                                   
*                                                                               
RDKEY30  LA    R0,CARD+25          A(END OF KEY)                                
         CR    R1,R0               TEST ENTIRE CARD PARSED                      
         BE    RDKEY50                                                          
         CLI   0(R1),C' '          TEST END OF DATA                             
         BE    RDKEY50                                                          
         CLI   0(R1),C'/'          TEST START OF HEADING                        
         BNE   RDKEY40                                                          
*                                                                               
         MVI   NOHEAD,C'N'         ASSUME THERE IS A HEADING                    
         MVC   HEADING(L'HEADING),1(R1)                                         
         CLC   HEADING,SPACES      TEST BLANK HEADING IS BEING FORCED           
         BNE   RDKEY50                                                          
         MVI   NOHEAD,C'Y'         FORCE NO HEADING                             
         B     RDKEY50                                                          
*                                                                               
RDKEY40  ZIC   R3,WORK             FAKED FIELD LENGTH                           
         LA    R3,1(R3)                                                         
         STC   R3,WORK                                                          
         ZIC   R3,WORK+5           FAKED INPUT LENGTH                           
         LA    R3,1(R3)                                                         
         STC   R3,WORK+5                                                        
         MVC   8(1,R2),0(R1)       MOVE KEY DATA TO WORK                        
*                                                                               
         CLI   8(R2),X'81'         TEST WITHIN LOWER CASE 'A'..'I'              
         BL    RDKEY45                                                          
         CLI   8(R2),X'89'                                                      
         BH    *+12                                                             
         OI    8(R2),X'40'         CONVERT TO UPPER CASE                        
         B     RDKEY45                                                          
         CLI   8(R2),X'91'         TEST WITHIN LOWER CASE 'J'..'R'              
         BL    RDKEY45                                                          
         CLI   8(R2),X'99'                                                      
         BH    *+12                                                             
         OI    8(R2),X'40'         CONVERT TO UPPER CASE                        
         B     RDKEY45                                                          
         CLI   8(R2),X'A2'         TEST WITHIN LOWER CASE 'S'..'Z'              
         BL    RDKEY45                                                          
         CLI   8(R2),X'A9'                                                      
         BH    RDKEY45                                                          
         OI    8(R2),X'40'         CONVERT TO UPPER CASE                        
*                                                                               
RDKEY45  LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         B     RDKEY30             CONTINUE PARSING                             
*                                                                               
RDKEY50  MVC   PMESS,=CL28'NO KEY PROVIDED'                                     
         CLI   WORK+5,0            TEST A KEY WAS PROVIDED                      
         BE    ERR                                                              
*                                                                               
         XC    SCANBLK,SCANBLK     SCAN THE KEY                                 
         GOTO1 =V(SCANNER),DMCB,WORK,(5,SCANBLK),C',=, '                        
         MVC   PMESS,=CL28'BAD KEY DATA'                                        
         CLI   DMCB+4,4            SHOULD BE EXACTLY 4 ENTRIES                  
         BNE   ERR                 (SYSTEM,PROGRAM,SCREEN,FIELD)                
*                                                                               
         LA    R4,KEY              FILL IN HELP RECORD KEY                      
         USING HV1KEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   HV1TYPE,HV1TYPEQ    RECORD TYPE                                  
*                                                                               
         LA    R3,SCANBLK          SYSTEM NUMBER                                
         MVC   PMESS,=CL28'BAD SYSTEM NUMBER'                                   
         CLI   0(R3),2             TEST 2 CHARACTERS                            
         BNE   ERR                                                              
         CLI   12(R3),C'T'         SYSTEM NUMBER BEGINS WITH 'T'                
         BNE   ERR                                                              
         MVI   12(R3),C'0'         REPLACE 'T' WITH '0'                         
         GOTO1 =V(HEXIN),DMCB,12(R3),BYTE,2                                     
         CLC   DMCB+12(4),=F'1'    ONLY ONE BYTE SHOULD BE RETURNED             
         BNE   ERR                                                              
         MVC   HV1SYS,BYTE         SAVE SYSTEM NUMBER                           
*                                                                               
         MVI   HV1PROG,0           ASSUME PROGRAM 'ALL'                         
         LA    R3,32(R3)           PROGRAM NUMBER ENTRY                         
         CLC   12(20,R3),=CL20'ALL'                                             
         BE    RDKEY60             IT IS PROGRAM 'ALL'                          
         MVC   PMESS,=CL28'BAD PROGRAM NUMBER'                                  
         CLI   0(R3),2             TEST 2 CHARACTERS                            
         BNE   ERR                                                              
         GOTO1 =V(HEXIN),DMCB,12(R3),BYTE,2                                     
         CLC   DMCB+12(4),=F'1'    ONLY ONE BYTE SHOULD BE RETURNED             
         BNE   ERR                                                              
         MVC   HV1PROG,BYTE        SAVE PROGRAM NUMBER                          
*                                                                               
RDKEY60  MVI   HV1SCRN,0           ASSUME SCREEN 'ALL'                          
         LA    R3,32(R3)           SCREEN NUMBER ENTRY                          
         CLC   12(20,R3),=CL20'ALL'                                             
         BE    RDKEY70             SCREEN 'ALL'                                 
         MVC   PMESS,=CL28'INVALID KEY'                                         
         CLI   HV1PROG,0           TEST PROGRAM 'ALL'                           
         BE    ERR                                                              
         MVC   PMESS,=CL28'BAD SCREEN NUMBER'                                   
         CLI   0(R3),2             TEST 2 CHARACTERS                            
         BNE   ERR                                                              
         GOTO1 =V(HEXIN),DMCB,12(R3),BYTE,2                                     
         CLC   DMCB+12(4),=F'1'    ONLY ONE BYTE SHOULD BE RETURNED             
         BNE   ERR                                                              
         CLI   BYTE,X'7F'          ALL SCREEN NAMES ARE GRTR THAN X'7F'         
         BNH   ERR                                                              
         MVC   HV1SCRN,BYTE        SAVE SCREEN NUMBER                           
*                                                                               
RDKEY70  MVI   HV1FIELD,0          ASSUME FIELD 'ALL'                           
         LA    R3,32(R3)           FIELD NUMBER ENTRY                           
         CLC   12(20,R3),=CL20'ALL'                                             
         BNE   RDKEY75             A FIELD IS SPECIFIED                         
         CLI   HV1SCRN,0           TEST SCREEN 'ALL'                            
         BE    RDKEY80             YES - NO PROBLEM                             
         MVC   PMESS,=CL28'SCREEN-WIDE NOT SUPPORTED'                           
         B     ERR                                                              
RDKEY75  MVC   PMESS,=CL28'BAD FIELD NUMBER'                                    
         TM    2(R3),X'80'         TEST NUMERIC                                 
         BZ    ERR                                                              
         ICM   R1,15,4(R3)         VALUE OF FIELD NUMBER                        
         C     R1,=F'1'                                                         
         BL    ERR                                                              
         C     R1,=F'255'                                                       
         BH    ERR                                                              
         STC   R1,HV1FIELD         SAVE FIELD NUMBER                            
*                                                                               
RDKEY80  MVI   HV1PAGE,1           FIRST PAGE                                   
         EJECT                                                                  
UPDATE   LA    R4,IO               BUILD A HELP RECORD                          
         MVI   DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         BAS   RE,READ             SEE IF HELP RECORD ALREADY EXISTS            
         BZ    UPD40               IT DOES                                      
*                                                                               
         XC    0(256,R4),0(R4)     CLEAR BEGINNING OF IO AREA                   
         MVC   HV1KEY,KEY          KEY OF NEW HELP RECORD                       
         MVC   HV1RECLN,=H'28'     LENGTH OF RECORD SO FAR                      
*                                                                               
         XC    WORK,WORK           BUILD STATUS ELEMENT                         
         LA    R6,WORK                                                          
         USING HV1STAD,R6                                                       
         MVI   HV1STAEL,HV1STAEQ                                                
         MVI   HV1STALN,HV1STALQ                                                
         GOTO1 =V(HELLO),DMCB,(C'P',CTFILE),IO,WORK                             
         B     UPD60                                                            
         DROP  R6                                                               
*                                                                               
UPD40    MVI   ELCODE,HV1STAEQ     LOOK FOR STATUS ELEMENT                      
         USING HV1STAD,R6                                                       
         LA    R6,HV1FSTEL                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   *+12                                                             
         TM    HV1STAST,HV1STAPQ   TEST RECORD IS PROTECTED                     
         BZ    UPD50                                                            
         DROP  R6                                                               
*                                                                               
         MVI   RDHELPSW,C'K'       LOOK FOR THE NEXT KEY                        
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    EOJ                                                              
*                                                                               
         CLI   RDHELPSW,C'K'       TEST A KEY WAS FOUND                         
         BE    RDKEY20             PROCESS THE NEW KEY                          
         DC    H'0'                                                             
*                                                                               
UPD50    TM    HV1STAT,X'80'       TEST RECORD DELETED                          
         BZ    *+8                                                              
         NI    HV1STAT,X'7F'       UNDELETE THE RECORD                          
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',CTFILE),(HV1HEDEQ,IO),0                     
         CLI   DMCB+12,6           TEST HEADING ELEMENT NOT FOUND               
         BE    *+14                                                             
         CLI   DMCB+12,0           TEST ANY OTHER ERROR                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',CTFILE),(HV1TXTEQ,IO),0                     
         CLI   DMCB+12,0           TEST ANY ERROR                               
         BE    *+6                                                              
         DC    H'0'                AT LEAST ONE TEXT LINE MUST BE THERE         
         EJECT                                                                  
UPD60    CLI   NOHEAD,C'Y'         TEST FORCE NO HEADING                        
         BE    UPD70                                                            
         XC    WORK,WORK           BUILD HEADING ELEMENT                        
         LA    R6,WORK                                                          
         USING HV1HEDD,R6                                                       
         MVI   HV1HEDEL,HV1HEDEQ                                                
         MVI   HV1HEDLN,HV1HEDLQ                                                
         LA    R1,HEADING+L'HEADING                                             
         BCTR  R1,0                                                             
         LA    R0,L'HEADING                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R0,*-8                                                           
         STC   R0,HV1HEDTL                                                      
         MVC   HV1HEDTX,HEADING                                                 
         GOTO1 =V(HELLO),DMCB,(C'P',CTFILE),IO,WORK                             
         DROP  R6                                                               
*                                                                               
UPD70    CLI   MOREPGSW,C'Y'       TEST WE ARE CONTINUING PAGES                 
         BNE   *+12                                                             
         MVI   MOREPGSW,C'N'       RESET SWITCH                                 
         B     UPD75                                                            
         MVC   PMESS,=CL28'TEXT IS REQUIRED'                                    
         MVI   RDHELPSW,C'T'       READ A TEXT LINE FROM HELP TEXT FILE         
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    EOJ                                                              
         CLI   RDHELPSW,C'T'       TEST TEXT WAS FOUND                          
         BNE   ERR                                                              
UPD75    LA    R3,1                R3 IS NO. OF TEXT LINES FOUND                
*                                                                               
UPD80    XC    WORK,WORK           BUILD TEXT ELEMENT                           
         LA    R6,WORK                                                          
         USING HV1TXTD,R6                                                       
         MVI   HV1TXTEL,HV1TXTEQ                                                
         STC   R3,HV1TXTSQ         SEQUENCE NUMBER                              
         MVC   HV1TXTVB,=X'FFFF'   ALL VALIDITY BITS ARE ON                     
         CLI   HILITESW,C'Y'       TEST LINE GET HIGHLIGHTED                    
         BNE   *+8                                                              
         OI    HV1TXTOP,X'80'                                                   
         CLC   CARD+1(79),SPACES   TEST BLANK LINE                              
         BNE   *+14                                                             
         MVI   HV1TXTTX,C' '       SAVE ONE SPACE                               
         SR    R2,R2                                                            
         B     UPD85                                                            
*                                                                               
         LA    R1,CARD+74          DETERMINE LENGTH OF TEXT LINE                
         LA    R2,74                                                            
         CLI   0(R1),C' '          LOOK FOR LAST SIGNIFICANT CHARACTER          
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R2,*-10                                                          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   HV1TXTTX(0),CARD+1  MOVE IN HELP TEXT                            
*                                                                               
UPD85    LA    R1,HV1TXTOV         OVERHEAD LENGTH                              
         LA    R2,1(R1,R2)         PLUS LENGTH OF TEXT                          
         STC   R2,HV1TXTLN         ELEMENT LENGTH                               
         GOTO1 =V(HELLO),DMCB,(C'P',CTFILE),IO,WORK                             
         DROP  R6                                                               
*                                                                               
         MVI   RDHELPSW,C'T'       READ A TEXT LINE FROM HELP TEXT FILE         
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    EOJ                                                              
         CLI   RDHELPSW,C'T'       TEST TEXT WAS FOUND                          
         BNE   UPD90               NO - RECORD IS COMPLETE                      
*                                                                               
         LA    R3,1(R3)            INCREMENT SEQUENCE NUMBER                    
         CH    R3,=H'16'           TEST TOO MANY LINES                          
         BNH   UPD80                                                            
         MVI   MOREPGSW,C'Y'       ANOTHER PAGE WILL BE NEEDED                  
*                                                                               
         USING HV1TXTD,R6          TEXT ELEMENTS                                
UPD90    MVI   ELCODE,HV1TXTEQ     LOOK FOR TRAILING BLANK LINES                
         LA    R6,HV1FSTEL                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE TEXT ELEMENT            
*                                                                               
UPD100   BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         LR    R3,R6               SAVE THIS ELEMENT ADDRESS                    
         B     UPD100                                                           
*                                                                               
         LR    R6,R3               A(LAST TEXT ELEMENT IN RECORD)               
         ZIC   R1,HV1TXTLN         ELEMENT LENGTH                               
         LA    R0,HV1TXTOV         OVERHEAD LENGTH                              
         SR    R1,R0               LENGTH OF TEXT LINE                          
         C     R1,=F'1'            TEST LENGTH OF 1                             
         BNE   UPD110              IT'S NOT A BLANK LINE - LEAVE                
         CLI   HV1TXTTX,C' '       TEST BLANK LINE                              
         BNE   UPD110                                                           
*                                                                               
         MVI   HV1TXTEL,X'FF'      MARK ELEMENT FOR DELETION                    
         B     UPD90                                                            
         DROP  R6                                                               
*                                                                               
UPD110   GOTO1 =V(HELLO),DMCB,(C'D',CTFILE),(X'FF',IO),0                        
         CLI   WRITESW,C'Y'        TEST WRITE=YES                               
         BNE   *+14                                                             
         BAS   RE,WRT              WRITE RECORD TO FILE                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TESTSW,C'Y'         TEST TESTFILE=YES                            
         BNE   UPD120                                                           
         XC    IOLEN(4),IOLEN                                                   
         ICM   R1,3,IO+25          RECORD LENGTH                                
         LA    R1,4(R1)            PLUS OVERHEAD FOR VB RECORDS                 
         STH   R1,IOLEN                                                         
         PUT   TESTOUT,IOLEN       WRITE HELP RECORD TO TESTFILE                
*                                                                               
UPD120   CLI   MOREPGSW,C'Y'       TEST ANOTHER PAGE IS NEEDED                  
         BNE   UPDX                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   KEY(L'HV1KEY),IO                                                 
         ZIC   R1,HV1PAGE          PREVIOUS PAGE NUMBER                         
         LA    R1,1(R1)            INCREMENT PAGE NUMBER                        
         STC   R1,HV1PAGE                                                       
         B     UPDATE              PROCESS NEXT HELP RECORD                     
*                                                                               
UPDX     MVI   RDHELPSW,C'K'       LOOK FOR THE NEXT KEY                        
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    EOJ                                                              
         CLI   RDHELPSW,C'K'       TEST A KEY WAS FOUND                         
         BE    RDKEY20                                                          
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
READHELP NTR1                                                                   
*                                                                               
RDHELP10 GET   INFILE,CARD         READ A RECORD FROM HELP TEXT FILE            
         MVC   PCARD,CARD          FOR ERROR MESSAGES                           
         CLC   =C' <COMMENT>',CARD TEST COMMENT CARD UPPER/LOWER CASE           
         BE    RDHELP10            YES - IGNORE                                 
         CLC   =X'404C839694948595A36E',CARD                                    
         BE    RDHELP10                                                         
*                                                                               
         CLC   =C' <KEY>',CARD     TEST FOR 'KEY' UPPER/LOWER CASE              
         BNE   *+12                                                             
         MVI   RDHELPSW,C'K'       A KEY WAS FOUND                              
         B     RDHELPX                                                          
         CLC   =X'404C9285A86E',CARD                                            
         BNE   *+12                                                             
         MVI   RDHELPSW,C'K'       A KEY WAS FOUND                              
         B     RDHELPX                                                          
*                                                                               
         CLI   RDHELPSW,C'K'       TEST A KEY WAS DESIRED                       
         BE    RDHELP10                                                         
         CLI   RDHELPSW,C'T'       TEST TEXT IS DESIRED                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   HILITESW,C'N'       ASSUME NO HIGHLIGHTING                       
         CLI   CARD,X'6A'          TEST FOR HIGHLIGHTING CHARACTER              
         BNE   *+12                                                             
         MVI   HILITESW,C'Y'       HIGHLIGHT THIS LINE                          
         B     *+18                                                             
         MVC   PMESS,=CL28'INVALID CHARACTER IN COL. 1'                         
         CLI   CARD,C' '           ONLY SPACE OR HIGHLIGHT IS PERMITTED         
         BNE   ERR                                                              
         MVC   PMESS,=CL28'MAXIMUM OF 74 CHARACTERS'                            
         CLC   CARD+75(5),SPACES   MAXIMUM OF 74 CHARACTERS PER LINE            
         BNE   ERR                                                              
         B     RDHELPX                                                          
*                                                                               
EOF      CLOSE (INFILE,)           CLOSE HELP TEXT FILE                         
         XC    CARD,CARD                                                        
         MVI   EOFSW,C'Y'                                                       
*                                                                               
RDHELPX  B     EXIT                                                             
         EJECT                                                                  
READ     NTR1                                                                   
         LA    RF,=C'DMREAD  '                                                  
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 =V(DATAMGR),DMCB,,CTFILE,KEY,IO                                  
         TM    8(R1),X'FD'         SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
RSEQ     NTR1                                                                   
         LA    RF,=C'DMRSEQ  '                                                  
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 =V(DATAMGR),DMCB,,CTFILE,KEY,IO                                  
         TM    8(R1),X'FD'         SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
WRT      NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMWRT   ',CTFILE,KEY,IO                      
         CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
         EJECT                                                                  
ERR      GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         MVI   ERRORSW,C'Y'                                                     
*                                                                               
         MVI   RDHELPSW,C'K'       LOOK FOR THE NEXT KEY                        
         BAS   RE,READHELP                                                      
         CLI   EOFSW,C'Y'          TEST END OF FILE                             
         BE    EOJ                                                              
         CLI   RDHELPSW,C'K'       TEST A KEY WAS FOUND                         
         BE    RDKEY20                                                          
         DC    H'0'                                                             
         SPACE 5                                                                
ERREOJ   GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         MVI   ERRORSW,C'Y'                                                     
         SPACE 3                                                                
EOJ      CLI   TESTSW,C'Y'         END OF JOB                                   
         BNE   EOJ10                                                            
         CLOSE (TESTOUT,)          CLOSE TEST OUTPUT FILE                       
*                                                                               
EOJ10    MVC   PCARD,SPACES                                                     
         MVC   PMESS,=CL28'SUCCESSFUL HELP FILE UPDATE'                         
         CLI   ERRORSW,C'N'        TEST NO ERRORS                               
         BE    *+10                                                             
         MVC   PMESS,=CL28'*** UNSUCCESSFUL UPDATE ***'                         
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
         SPACE 3                                                                
INFILE   DCB   DDNAME=INFILE,DSORG=PS,RECFM=FB,LRECL=80,               +        
               BLKSIZE=1200,MACRF=GM,EODAD=EOF                                  
*                                                                               
TESTOUT  DCB   DDNAME=TESTOUT,DSORG=PS,RECFM=VB,LRECL=2100,            +        
               BLKSIZE=2104,MACRF=PM                                            
         SPACE 2                                                                
         GETEL R6,28,ELCODE                                                     
         SPACE 2                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
UTL      DC    F'0',X'0A'          CONTROL SYSTEM                               
SSB      DC    F'0'                SUPPRESS RECOVERY                            
CTFILE   DC    C'CTFILE  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
DUB      DS    D                                                                
         DC    C'**DMCB**'                                                      
DMCB     DS    6F                                                               
         DC    C'**CARD**'                                                      
CARD     DS    CL80                INPUT RECORD                                 
         DC    C'**WORK**'                                                      
WORK     DS    XL256               GENERAL WORK AREA                            
         DC    C'**SCAN**'                                                      
SCANBLK  DS    XL256               SCANNER BLOCK (8 32-BYTE ENTRIES)            
         DC    C'***KEY**'                                                      
KEY      DS    XL25                HELP RECORD KEY                              
*                                                                               
BYTE     DS    X                                                                
DMINBTS  DS    X                   FOR DATA MANAGER                             
HEADING  DS    CL(L'HV1HEDTX)      SAVES HELP SCREEN HEADING                    
NOHEAD   DC    C'N'                'Y' TO FORCE NO HEADING                      
ELCODE   DS    X                   FOR GETEL MACRO                              
RDHELPSW DS    C                   ARGUMENT FOR READHELP                        
*                                  'K' READ FOR A KEY/KEY FOUND                 
*                                  'T' READ FOR A LINE OF TEXT                  
ERRORSW  DC    C'N'                'Y' IF AN ERROR WAS FOUND                    
TESTSW   DC    C'N'                'Y' TO WRITE TO TEST OUTPUT FILE             
WRITESW  DC    C'Y'                'N' TO SUPPRESS CTFILE UPDATE                
PROTSW   DC    C'N'                'Y' TO PROTECT EXISTING RECORDS              
RESTSW   DC    C'N'                'Y' TO RESTRICT RECORDS                      
HILITESW DC    C'N'                'Y' IF LINE GETS HIGHLIGHTED                 
MOREPGSW DC    C'N'                'Y' IF ANOTHER HELP PAGE IS NEEDED           
EOFSW    DC    C'N'                'Y' ON END OF INPUT FILE                     
         DS    0D                                                               
         DC    C'***IO***'                                                      
IOLEN    DS    H                   RECORD LENGTH                                
         DS    H                                                                
IO       DS    2048X               IO AREA FOR HELP RECORD                      
         EJECT                                                                  
       ++INCLUDE CTGENHV1                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
         ORG   P                                                                
PMESS    DS    CL28                                                             
         DS    CL2                                                              
PCARD    DS    CL80                                                             
         DS    CL22                                                             
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067HELPUPD   05/01/02'                                      
         END                                                                    
