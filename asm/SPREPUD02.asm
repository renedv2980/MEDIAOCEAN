*          DATA SET SPREPUD02  AT LEVEL 016 AS OF 01/13/20                      
*PHASE SPUD02A                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
* Modify DARE station records for cable syscodes based on COSUPD file           
***********************************************************************         
SPUD02   TITLE 'SPREPUD02 - DARE Station Cable Update'                          
SPUD02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                  GOT 3 BASE REGISTERS                         
         NMOD1 0,SPUD02,R8,R7,RR=R3                                             
*                                                                               
         LR    RC,RB               SET UP MY WORK AREA                          
         AH    RC,=Y(SPUDWRKD-SPUD02)                                           
         USING SPUDWRKD,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPUD02)                                            
         ST    R6,AREC1                                                         
         LR    R6,RB               SET ADDRESS OF RECORD #2 FOR SPONSOR         
         A     R6,=A(SPOTREC2-SPUD02)                                           
         ST    R6,AREC2                                                         
*                                                                               
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         MVC   AREC,AREC1          DEFAULT RECORD AREA                          
*                                                                               
         OPEN  (FILEIN,INPUT)      OPEN UP INPUT FILE FOR USE                   
*                                                                               
         ZAP   COUNTACT,=P'0'                                                   
         ZAP   COUNTDEA,=P'0'                                                   
         XC    ACTHDEND,ACTHDEND                                                
         XC    SYSHDEND,SYSHDEND                                                
         XC    SVDHDEND,SVDHDEND                                                
*                                                                               
         L     RE,UTL                                                           
         MVI   4(RE),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'UGENDIR UGENFIL X',DMWORK,0                                   
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
READALIN XCEFL INPTLINE,4004       CLEAR THE INPUT LINE                         
         MVI   BITFLAG1,0          CLEAR OUR BIT FLAGS                          
*                                                                               
         GET   FILEIN,INPTLINE     READ A LINE FROM THE FILE                    
*                                                                               
         BAS   RE,FETCHINF         FETCH IMPORTANT INFO FROM LINE               
         BNE   READALIN                                                         
*                                                                               
         BAS   RE,PRCSSINF         PROCESS THE INFORMATION                      
         B     READALIN            READ THE NEXT LINE                           
*                                                                               
***********************************************************************         
* NOMORE                                                                        
*                                                                               
* WHAT HAPPENS WHEN THERE ARE NO MORE LINES LEFT IN THE COSUPD FILE             
***********************************************************************         
*                                                                               
NOMORE   DS    0H                                                               
         MVC   P(30),=CL30'NUMBER OF SYSCODES ACTIVATED: '                      
         EDIT  (P8,COUNTACT),(17,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         MVC   P3(32),=CL32'NUMBER OF SYSCODES DEACTIVATED: '                   
         EDIT  (P8,COUNTDEA),(17,P3+30),ALIGN=LEFT,ZERO=NOBLANK                 
         GOTO1 REPORT                                                           
*                                                                               
NOMOREX  CLOSE FILEIN                                                           
*                                                                               
MAINX    GOTO1 AENDREQ                                                          
         EJECT                                                                  
***********************************************************************         
* FETCHINF                                                                      
***************                                                                 
*     THIS ROUTINE FETCHES ALL THE NECESSARY INFOMATION FROM THE LINE           
* WE GOT FROM THE FILE AND PUTS IT INTO OUR STORAGE AREA                        
***********************************************************************         
*                                                                               
FETCHINF NTR1                                                                   
         LA    R2,INPTDATA         R2=A(DATA OR 1ST FIELD)                      
         MVI   CODEFLG1,0                                                       
*****                                                                           
* ACTION DATA                                                                   
*****                                                                           
*                                                                               
FINF00   OI    BITFLAG1,B1ACTION   GOT AN ACTION                                
*                                                                               
         GOTO1 NEXTFLD,DMCB,(0,(R2)),(L'IN_SYSCD,IN_SYSCD)                      
         BNE   FINFNO                                                           
         L     R2,DMCB                                                          
         EDIT  (B2,IN_SYSCD),(4,CBLHDEND),FILL=0                                
         MVI   CBLHDEND+4,C'T'     CABLE IS A SUBSET OF SPOT TV                 
*                                                                               
FINF02   GOTO1 NEXTFLD,DMCB,(1,(R2)),(L'IN_ACTCD,IN_ACTCD)                      
         BNE   FINFNO                                                           
*                                                                               
         LA    R1,CODTABLE         VALIDATE THE ACTION CODE                     
         LA    RF,CODEFLG1         FIRST CODE FLAG                              
*                                                                               
FINF04   CLI   0(R1),0             EXITN IF NOT IN TABLE                        
         BE    FINFNO                                                           
*                                                                               
         CLC   IN_ACTCD,0(R1)      CODE IN THIS ENTRY?                          
         BE    FINF05              YES                                          
         LA    R1,L'CODTABLE(R1)   NO, CHECK THE NEXT ENTRY                     
         B     FINF04              NEXT CODE                                    
*                                                                               
FINF05   MVC   BYTE,0(RF)          SET THE APPROPRIATE BIT ON FOR THIS          
         OC    BYTE,1(R1)              ACTION CODE                              
         MVC   0(1,RF),BYTE                                                     
*                                                                               
         MVC   ACTHDEND,CBLHDEND   SAVE CABLE HEADEND FOR THIS ACTION           
*                                                                               
FINFYES  B     YES                                                              
*                                                                               
FINFNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* NEXTFLD                                                                       
***************                                                                 
*     THIS ROUTINE POINTS TO THE NEXT FIELD OF THE INPUT LINE.                  
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     NUMBER OF FIELDS TO SKIP                     
*                      BYTES 1-3   ADDRESS OF CURRENT FIELD                     
*                                                                               
*              DMCB+4  BYTE  0     MAX LENGTH OF STORAGE AREA                   
*                      BYTES 1-3   ADDRESS OF WHERE TO STORE FIELD              
*                                                                               
* ON EXIT:     DMCB    BYTE  0     FIRST BYTE OF FIELD WE FOUND                 
*                      BYTES 1-3   ADDRESS OF FIELD WE FOUND                    
***********************************************************************         
*                                                                               
NEXTFLD  NTR1                                                                   
         L     R2,DMCB             R2=A(CURRENT FIELD IN INPTLINE)              
         L     R4,DMCB+4           R4=A(STORAGE AREA)                           
*                                                                               
         ZIC   R1,DMCB+4           CLEAR THE STORAGE AREA                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
*                                                                               
         CLI   DMCB,0              SKIP ANY FIELDS?                             
         BNE   *+12                                                             
         LA    R3,1                NO                                           
         B     NFLD25                                                           
*                                                                               
         ZIC   R3,DMCB             R3=NUMBER OF FIELDS TO SKIP                  
NFLDRNTR NI    BITFLAG1,X'FF'-B1DBLQTE-B1COMMA                                  
*                                                                               
NFLDLOOP CLI   0(R2),C'"'          DO WE HAVE A DOUBLE QUOTE?                   
         BNE   NFLD10                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD25              YES, NEXT FIELD                              
*                                                                               
         TM    BITFLAG1,B1DBLQTE   USING A DOUBLE QUOTE ALREADY?                
         BZ    *+12                                                             
         NI    BITFLAG1,X'FF'-B1DBLQTE                                          
         B     NFLDNXTC                                                         
         OI    BITFLAG1,B1DBLQTE                                                
         B     NFLDNXTC                                                         
*                                                                               
NFLD10   CLI   0(R2),C','          DO WE HAVE A COMMA?                          
         BNE   NFLD20                                                           
*                                                                               
         TM    BITFLAG1,B1COMMA    GOT A COMMA ALREADY?                         
         BNZ   NFLD15                                                           
         TM    BITFLAG1,B1DBLQTE   NO, COMMA IN BETWEEN QUOTES?                 
         BNZ   NFLDNXTC                YES                                      
         OI    BITFLAG1,B1COMMA        NO, WE HAVE A COMMA NOW                  
         B     NFLDNXTC            NEXT CHAR SHOULD BE START OF NXT FLD         
*                                                                               
NFLD15   ST    R2,DMCB             RETURN WITH A COMMA IN DMCB SO WE            
         MVI   DMCB,C','               KNOW WE HAVE 2 COMMAS IN A ROW           
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDYES             NO DATA TO STORE                             
*                                                                               
NFLD20   TM    BITFLAG1,B1COMMA                                                 
         BZ    NFLDNXTC                                                         
*                                                                               
NFLD25   ST    R2,DMCB                                                          
         MVC   DMCB(1),0(R2)                                                    
         BCT   R3,NFLDRNTR         RE-ENTER IF WE NEED TO SKIP MORE             
         B     NFLDSTOR                                                         
*                                                                               
NFLDNXTC LA    R2,1(R2)            R2 = A(NEXT CHARACTER)                       
         LA    R0,INPTLINE                                                      
         LR    R1,R2                                                            
         SR    R1,R0                                                            
         CLM   R1,3,INPTLGTH       DID WE GO BEYOND THE DATA?                   
         BL    NFLDLOOP            NO                                           
         B     NFLDNO                                                           
*                                                                               
NFLDSTOR CLI   0(R2),C'"'          DEALING WITH A STRING?                       
         BNE   NFLD40                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         ZIC   R1,DMCB+4           COPY STRING WITHOUT FIRST QUOTE              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
*                                                                               
         L     R1,DMCB+4                                                        
         LA    R1,0(R1)            CLEAR HOB                                    
         LR    R0,R1                                                            
NFLD30   CLI   0(R4),C'"'          FOUND SECOND QUOTE?                          
         BE    NFLD35                                                           
         LA    R4,1(R4)            NO, CHECK NEXT CHARACTER                     
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         CLM   R1,1,DMCB+4                                                      
         BNL   NFLDYES                                                          
         B     NFLD30                                                           
*                                                                               
NFLD35   L     R0,DMCB+4           CLEAR FROM SECOND QUOTE ON                   
         LR    R1,R4                                                            
         SR    R1,R0                                                            
         ZIC   R0,DMCB+4                                                        
         SR    R0,R1                                                            
         BCTR  R0,0                                                             
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     NFLDYES                                                          
         XC    0(0,R4),0(R4)                                                    
*                                                                               
NFLD40   LA    R1,1                AT LEAST ONE DIGIT IN NUMBER                 
         LR    R3,R2                                                            
*                                                                               
         LA    R3,1(R3)                                                         
         CLI   0(R3),C','                                                       
         BE    *+12                                                             
         LA    R1,1(R1)            INCREMENT DIGIT COUNTER                      
         B     *-16                                                             
*                                                                               
         BCTR  R1,0                CONVERT NUMBER TO BINARY                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         CLI   DMCB+4,1            ONE BYTE                                     
         BNE   *+12                                                             
         STC   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,2            HALF WORD                                    
         BNE   *+12                                                             
         STH   R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,3            3 BYTES                                      
         BNE   *+12                                                             
         STCM  R1,7,0(R4)                                                       
         B     NFLDYES                                                          
*                                                                               
         CLI   DMCB+4,4            FULL WORD                                    
         JNE   *+2                                                              
         ST    R1,0(R4)                                                         
         B     NFLDYES                                                          
*                                                                               
NFLDNO   B     NO                  NO MORE DATA                                 
*                                                                               
NFLDYES  B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE INFORMATION FROM THE LINE WE GOT FROM THE          
* FILE.                                                                         
***********************************************************************         
*                                                                               
PRCSSINF NTR1                                                                   
         XC    BIGKEY,BIGKEY                                                    
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING STAKEYD,R6                                                       
         MVI   STAKTYP,STAKTYPQ    DSTA RECORDS ARE X'005A'                     
         MVI   STAKMEDA,C'T'                                                    
         MVC   STAKSTIN,CBLHDEND  LOOK FOR THE CURRENT CALLS                    
*                                                                               
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'GENDIR'),BIGKEYSV,    +        
               BIGKEY,0                                                         
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   BIGKEY(STAKEFDA-STAKEY),BIGKEYSV                                 
         JNE   *+2                 DSTA BETTER EXIST                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),(0,=C'GENFIL'),         +        
               BIGKEY+36,AREC,DMWORK                                            
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
***************                                                                 
* PROCESSING NEW SYSTEM?                                                        
***************                                                                 
         TM    CODEFLG1,CDNEWSYS   ADDING NEW SYSTEM ("D")?                     
         BZ    PINFD00             NO, PROBABLY DEACTIVATE ("X") THEN           
*                                                                               
         L     R6,AREC                                                          
         LA    R2,STAFSTEL                                                      
PINFA05  CLI   0(R2),STAREPCQ      X'10' - INFO ELEM                            
         BE    PINFA10                                                          
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0             EOR?                                         
         BNE   PINFA05             YES, THIS SHOULDN'T BE                       
         DC    H'0'                                                             
*                                                                               
         USING STAREPD,R2                                                       
PINFA10  CLC   =C'NCC',STAREPCR    CURRENT REP IS NCC?                          
         BNE   PINFA20                                                          
         MVC   P(36),=C'NCC IS ALREADY THE CURRENT REP FOR: '                   
         MVC   P+36(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
         B     PINFYES                                                          
*                                                                               
PINFA20  CLC   =C'NOR',STAREPCR    CURRENT REP IS NOR?                          
         BE    PINFA30             YES                                          
         MVC   P(41),=C'CURRENT REP OF: XXX IS QUESTIONABLE FOR: '              
         MVC   P+16(L'STAREPCR),STAREPCR                                        
         MVC   P+41(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
*                                                                               
PINFA30  MVC   STAREPPR,STAREPCR   CURRENT REP IS NOW PREV REP                  
         MVC   STAREPCR,=C'NCC'    CURRENT REP IS NCC                           
         GOTO1 DATCON,DMCB,(5,0),(15,STAREPED)  FULL JULIAN EFF DATE            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),(0,=C'GENFIL'),         +        
               BIGKEY,AREC,DMWORK                                               
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         MVC   P(19),=C'ACTIVATED SYSCODE: '                                    
         MVC   P+19(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
*                                                                               
         AP    COUNTACT,=P'1'                                                   
*                                                                               
         B     PINFYES                                                          
***********************************                                             
* DE-ACTIVATE SYSTEM?                                                           
***********************************                                             
PINFD00  TM    CODEFLG1,CDNTACTV   DEACTIVATE SYSTEM ("X")?                     
         JZ    PINFR00             NO, PROBABLY REPURPOSE ("!") THEN            
*                                                                               
         L     R6,AREC                                                          
         LA    R2,STAFSTEL                                                      
PINFD05  CLI   0(R2),STAREPCQ      X'10' - INFO ELEM                            
         BE    PINFD10                                                          
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0             EOR?                                         
         BNE   PINFD05             YES, THIS SHOULDN'T BE                       
         DC    H'0'                                                             
*                                                                               
         USING STAREPD,R2                                                       
PINFD10  CLC   =C'NOR',STAREPCR    CURRENT REP IS NOR?                          
         BNE   PINFD20                                                          
         MVC   P(36),=C'NOR IS ALREADY THE CURRENT REP FOR: '                   
         MVC   P+35(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
         B     PINFYES                                                          
*                                                                               
PINFD20  CLC   =C'NCC',STAREPCR    CURRENT REP IS NCC?                          
         BE    PINFD30             YES                                          
         MVC   P(41),=C'CURRENT REP OF:     IS QUESTIONABLE FOR: '              
         MVC   P+16(L'STAREPCR),STAREPCR                                        
         MVC   P+41(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
*                                                                               
PINFD30  MVC   STAREPPR,STAREPCR   CURRENT REP IS NOW PREV REP                  
         MVC   STAREPCR,=C'NOR'    CURRENT REP IS NCC                           
         GOTO1 DATCON,DMCB,(5,0),(15,STAREPED)  FULL JULIAN EFF DATE            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),(0,=C'GENFIL'),         +        
               BIGKEY,AREC,DMWORK                                               
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         MVC   P(22),=C'DE-ACTIVATED SYSCODE: '                                 
         MVC   P+22(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
*                                                                               
         AP    COUNTDEA,=P'1'                                                   
         J     PINFYES                                                          
***************                                                                 
* REPURPOSING SYSTEM?                                                           
***************                                                                 
PINFR00  TM    CODEFLG1,CDREPURP   REPURPOSE SYSTEM ("!")?                      
         BZ    PINFYES             NO, THEN SKIP IT                             
*                                                                               
         L     R6,AREC                                                          
         LA    R2,STAFSTEL                                                      
PINFR05  CLI   0(R2),STAREPCQ      X'10' - INFO ELEM                            
         BE    PINFR10                                                          
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0             EOR?                                         
         BNE   PINFR05             YES, THIS SHOULDN'T BE                       
         DC    H'0'                                                             
*                                                                               
         USING STAREPD,R2                                                       
PINFR10  DS    0H                                                               
         MVC   STAREPCR,=C'NCC'    CURRENT REP IS NCC                           
         GOTO1 DATCON,DMCB,(5,0),(15,STAREPED)  FULL JULIAN EFF DATE            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),(0,=C'GENFIL'),         +        
               BIGKEY,AREC,DMWORK                                               
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         MVC   P(20),=C'REPURPOSED SYSCODE: '                                   
         MVC   P+20(L'CBLHDEND),CBLHDEND                                        
         GOTO1 REPORT                                                           
*                                                                               
         AP    COUNTACT,=P'1'                                                   
*                                                                               
PINFYES  B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================*         
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA)  OR  ZERO FOR ELEMENTAL RECORD                   
*        PARAMETER 3 - A(LABEL) OR  ZERO FOR NO LABEL                           
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=====================================================================*         
MYTRACE  NTR1                                                                   
         CLI   QOPT1,C'Y'          OPTION SET TO DISPLAY TRACE?                 
         BNE   TRX                 NO                                           
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* VARIABLES AND STUFF                                                           
***********************************************************************         
CODTABLE DS    0XL2                                                             
         DC    C'D',AL1(CDNEWSYS)      NEW SYSTEM                               
         DC    C'X',AL1(CDNTACTV)      SYSTEM DEACTIVATED                       
         DC    C'!',AL1(CDREPURP)      REPURPOSE (OVERWRITE) WAS C'R'           
         DC    X'00'                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
RELO     DS    A                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
FILEIN   DCB   DDNAME=COSUPD,MACRF=GM,DSORG=PS,RECFM=VB,BLKSIZE=6233,  +        
               LRECL=255,EODAD=NOMORE                                           
*                                                                               
*                                                                               
SPUDWRKD DS    0A                                                               
AREC1    DS    A                   A(FIRST RECORD AREA)                         
AREC2    DS    A                   A(SECOND RECORD AREA)                        
ACABLTAB DS    A                   A(CABLE NETWORK TABLE)                       
CBLTABLN DS    H                                                                
         DS    H                                                                
*                                                                               
BIGKEY   DS    XL48                                                             
BIGKEYSV DS    XL48                                                             
*                                                                               
SVDREGRE DS    A                   SAVED REGISTER RE                            
*                                                                               
ADCURAGY DS    A                   A(CURRENT AGENCY ENTRY)                      
VXSORT   DS    V                                                                
*                                                                               
COUNTACT DS    PL8                 COUNTER OF STATIONS ACTIVATED                
COUNTDEA DS    PL8                 COUNTER OF STATIONS DE-ACTIVATED             
*                                                                               
BITFLAG1 DS    XL1                 FIRST SET OF BIT FLAGS                       
B1DBLQTE EQU   X'80'                   WE FOUND A DOUBLE QUOTE ALREADY          
B1COMMA  EQU   X'40'                   WE FOUND A COMMA ALREADY                 
B1NETWRK EQU   X'20'                   GOT NETWORK DATA                         
B1ACTION EQU   X'10'                   GOT ACTION CODE DATA                     
*                                                                               
BITFLAG2 DS    XL1                 SECOND SET OF BIT FLAGS                      
B2ADDING EQU   X'80'                   ADDING RECORD (0=WRITE RECORD)           
B2ADDK   EQU   X'40'                   ADD K KEY (0=WRITE RECORD)               
*                                                                               
ACTNFLG1 DS    XL1                 SAME BIT DEFINITION AS CODEFLG1              
CODEFLG1 DS    XL1                 ACTION CODE BIT FLAG 1                       
CDNEWSYS EQU   X'80'                   'D' - NEW SYSTEM                         
CDNTACTV EQU   X'01'                   'X' - SYSTEM DEACTIVATED                 
*                                                                               
ACTNFLG2 DS    XL1                 SAME BIT DEFINITION AS CODEFLG2              
CODEFLG2 DS    XL1                 ACTION CODE BIT FLAG 2                       
CDGRPCOD EQU   X'80'                   'B' - GROUP CODE                         
CDEIXSTA EQU   X'40'                   'K' - ELECTRONIC INVOICING               
CDREPURP EQU   X'20'                   'A' - REPURPOSE DEACTIV SYSCODE          
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
IN_SYSCD DS    XL2                 SYSTEM CODE (CABLE HEADEND)                  
IN_ACTCD DS    CL1                 ACTION CODE                                  
*                                                                               
ACTHDEND DS    CL5                 ACTION CABLE HEADEND (LAST USED)             
SYSHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
NETHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
CBLHDEND DS    CL5                 CABLE HEADEND (CURRENT)                      
SVDHDEND DS    CL5                 STATION CALL LETTERS                         
NUMMRKT  DS    CL4                 MKT NUM EBCDIC (BASED ON ALPHA MKT)          
PRVMRKT  DS    CL4                 PREVIOUS MARKET OF THE STATION               
TODAYDTE DS    CL6                 TODAY'S DATE EBCDIC YY/MM/DD                 
*                                                                               
SPTSENUM DS    XL1                 SPOT SENUM                                   
XCPTCLT  DS    CL3                 EXCEPTION CLIENT RECORD, 000 = REG.          
*                                                                               
REPRPMKT DS    XL2                 MKT OF REPURPOSED SYSCODE                    
TOP24    DC    XL3'00'             TOP 24 NETWORKS                              
CBLSEQ   DC    XL206'00'            CABLE SEQ                                   
WKTOP24  DC    XL3'00'             WORKING FIELD FOR TOP 24 NETWORKS            
SVTOP24  DC    XL3'00'             SAVED TOP 24 WORKING FIELD                   
WKCBLSEQ DC    XL206'00'            WORKING FIELD FOR CABLE SEQ                 
*                                                                               
ELEM     DS    CL128                                                            
MYCABTAB DS    CL384                                                            
MYCABEND DC    X'000000'                                                        
*                                                                               
INPTLINE DS    0CL4004             INPUT LINE FROM THE FILE                     
INPTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
INPTDATA DS    CL4000                  DATA FOR THIS LINE                       
*                                                                               
SPOTREC  DS    CL4000                                                           
SPOTREC2 DS    CL4000                                                           
*                                                                               
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
*SPGENAGY                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
*CTGENFILE                                                                      
*CTGENSTAD                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENSTAD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPUD02 01/13/20'                                      
         END                                                                    
