*          DATA SET RESFM1E    AT LEVEL 180 AS OF 01/22/13                      
*PHASE T8181EA,*                                                                
         TITLE 'T8181E - RESFM1E - SPEC BUY LIST MAINT'                         
*********************************************************************           
*                                                                   *           
*        RESFM1E (T8181E) --- SPEC BUY LIST MAINT                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUL20/01 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* MAR08/04 (BU ) --- CODE/CODE TYPE LENGTH TEST                     *           
*                                                                   *           
* SEP17/04 (BU ) --- PERMIT PROCESSING OF TWO-CHAR BUY CODES        *           
*                    ENTERED PRIOR TO MAR8/04 (GRANDFATHER THEM)    *           
*                                                                   *           
* JUL09/12 (BOB) --- HAVE GETFACTS PASS BACK ADDRESS OF FAFACTS     *           
*                                                                   *           
*                                                                   *           
*                    *****  END TOMBSTONE  *****                    *           
*********************************************************************           
*                                                                               
T8181E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**181E**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
*                                                                               
*   IF ACTION BRINGS UP 80 SCREEN, NO SELECT ACTION COLUMN IS                   
*        PRESENT, AND ACTION CHECK SHOULD BE SKIPPED.                           
*                                                                               
         CLC   =C'ADD',CONACT      'ADD' ACTION FROM SCREEN?                    
         BE    VKEY0005            YES                                          
         CLC   =C'CHA',CONACT      'CHANGE' ACTION FROM SCREEN?                 
         BE    VKEY0005            YES                                          
         CLC   =C'DIS',CONACT      'DISPLAY' ACTION FROM SCREEN?                
         BE    VKEY0005            YES                                          
*                                                                               
*   IF ACTION COLUMN IS PRESENT, ACTIONS MUST BE PROHIBIED FROM IT.             
*                                                                               
         GOTO1 CHECKACT            CHECK ACTION COLUMN FOR ENTRY                
         BNZ   VERR0015                                                         
*                                                                               
VKEY0005 EQU   *                                                                
         MVI   ATTCODE,0           SET FLAG TO 'CODE TYPE'                      
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEY0100                                                         
         LA    R2,SBMATTH          SET A(CODE TYPE OPTION FIELD)                
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BE    VKEY0010            NO                                           
         CLI   8(R2),C'T'          YES - 'CODE TYPE'?                           
         BE    VKEY0010            YES - SET OKAY                               
         CLI   8(R2),C'C'          NO  - 'CODE'?                                
         BNE   VERR0002            NO  - SEND ERROR MESSAGE                     
         MVI   ATTCODE,1           YES - SET FLAG TO 'CODE'                     
VKEY0010 EQU   *                                                                
         LA    R2,SBMSIDH          SET A(INPUT FIELD)                           
         CLI   ACTNUM,ACTADD       IS THIS AN ADD?                              
         BNE   VKEY0020            NO                                           
         CLI   5(R2),0                                                          
         BE    VERR0014                                                         
         GOTO1 RCDEXIST,DMCB,(1,0) DOES RECORD EXIST?                           
         BZ    VKEY0060            NO                                           
         B     VERR0008            YES - CAN'T ADD AGAIN                        
VKEY0020 EQU   *                                                                
         CLI   ACTNUM,ACTDIS       IS THIS A DISPLAY?                           
         BE    VKEY0040            NO                                           
         CLI   ACTNUM,ACTCHA       IS THIS A CHANGE?                            
         BNE   VKEY0060            NO                                           
VKEY0040 EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    VERR0014                                                         
         GOTO1 RCDEXIST,DMCB,(1,0) DOES RECORD EXIST?                           
         BNZ   VKEY0060            YES - CAN BE DISPLAYED/CHANGED               
         B     VERR0010            NO  - DOESN'T EXIST                          
VKEY0060 EQU   *                                                                
         MVC   KEY,KEYSAVE         RELOAD KEY                                   
         B     VKEY0900            EXIT                                         
VKEY0100 EQU   *                                                                
         MVC   SBUDESC,SPACES      CLEAR FILTER DESCRIPTION                     
         LA    R2,SBULEVH          SET A(RECORD TYPE INDICATOR)                 
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0160            NO  - NO LABEL IN FIELD                      
         CLI   8(R2),C'T'          YES - CODE TYPE REQUESTED?                   
         BE    VKEY0140            YES                                          
         CLI   8(R2),C'C'          NO  - CODE REQUESTED?                        
         BNE   VERR0002            NO  - ERROR                                  
         MVI   ATTCODE,1           YES - SET TO 'CODE'                          
VKEY0140 EQU   *                                                                
         MVC   SBUDESC(9),=C'CODE TYPE'                                         
         CLI   ATTCODE,0           CODE TYPES SELECTED?                         
         BE    VKEY0160            YES                                          
         MVC   SBUDESC(9),=C'CODE     '                                         
VKEY0160 EQU   *                                                                
         OI    SBUDESCH+6,X'80'    TURN ON TRANSMIT BIT                         
VKEY0900 EQU   *                                                                
         SPACE 1                                                                
         XIT1                                                                   
         EJECT                                                                  
CHECKACT NTR1                                                                   
         LA    R2,SBUSELH          CHECK IF ACTION COLUMN HAS INPUTS            
CHEC0020 DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    CHEC0060                                                         
         CLC   =C'CHA',8(R2)       ACTION CHANGE                                
         BE    CHEC0040                                                         
         CLI   8(R2),C'C'                                                       
         BE    CHEC0040                                                         
         CLC   =C'SEL',8(R2)       ACTION SELECT                                
         BE    CHEC0040                                                         
         CLI   8(R2),C'S'                                                       
         BE    CHEC0040                                                         
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CHEC0120            EXIT                                         
CHEC0040 EQU   *                                                                
         SR    R0,R0               EXIT CC ZERO                                 
         B     CHEC0120                                                         
CHEC0060 DS    0H                                                               
         LA    RF,SBULSELH-SBUSELH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,SBULAST                                                       
         CR    R2,R1                                                            
         BL    CHEC0020                                                         
         SR    R0,R0               EXIT CC ZERO                                 
CHEC0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     EQU   *                                                                
         GOTO1 GETFACT,DMCB,0                                                   
*                                                                               
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   USERLUID,FASYM                                                   
*                                                                               
         DROP  RF                                                               
         CLI   ATTCODE,1           FLAG SET TO 'CODE'?                          
         BE    VREC0060            YES                                          
         LA    R2,SBMSIDH          NO  - SET A(SPECIAL BUY CODE)                
         CLI   5(R2),2             CODE TYPE MUST BE 2 CHARACTERS               
         BNE   VERR0004            HIGH - ERROR                                 
         LA    R2,SBMATT1H         NO CODE TYPES MAY BE ENTERED                 
         CLI   5(R2),0                                                          
         BH    VERR0006            NO CODE PERMITTED                            
         LA    R2,SBMATT2H                                                      
         CLI   5(R2),0                                                          
         BH    VERR0006                                                         
         LA    R2,SBMATT3H                                                      
         CLI   5(R2),0                                                          
         BH    VERR0006                                                         
*                                                                               
         B     VREC0200            CONTINUE TO PROCESS                          
*                                                                               
VREC0060 EQU   *                                                                
*                                                                               
*   CODE BEING ENTERED:  CHECK FOR EXISTENCE OF CODE TYPES                      
*                                                                               
         LA    R2,SBMSIDH          SET A(SPECIAL BUY CODE)                      
*                                                                               
*   FOR CHANGES, IGNORE THE LENGTH OF THE CODE BEING CHANGED.  THIS             
*        WILL GRANDFATHER IN THOSE TWO-CHARACTER CODES ENTERED                  
*        PRIOR TO MAR8/04, WHEN THE THREE-CHARACTER CODE BECAME                 
*        MANDATORY.                                                             
*                                                                               
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    VREC0065            YES - ALREADY ON FILE:                       
*                                     ACCEPT LENGTH AS ENTERED                  
         CLI   ACTNUM,ACTSEL       SELECT ACTION?                               
         BE    VREC0065            YES - ALREADY ON FILE:                       
*                                     ACCEPT LENGTH AS ENTERED                  
         CLI   5(R2),3             CODE  MUST BE 3 CHARACTERS                   
         BNE   VERR0005            HIGH - ERROR                                 
VREC0065 EQU   *                                                                
         XC    SAVEATTS,SAVEATTS   CLEAR SAVE CODE TYPE AREA                    
         LA    R3,SAVEATTS         SET A(AREA)                                  
         LA    R2,SBMATT1H         NO CODE TYPES MAY BE ENTERED                 
         CLI   5(R2),0                                                          
         BE    VREC0070            NO CODE PERMITTED                            
         GOTO1 RCDEXIST,DMCB,0     DOES CODE TYPE EXIST?                        
         BZ    VERR0012            NO  - ERROR                                  
         MVC   0(2,R3),8(R2)       SAVE CODE TYPE IN STRING                     
         LA    R3,2(R3)                                                         
VREC0070 EQU   *                                                                
         LA    R2,SBMATT2H                                                      
         CLI   5(R2),0                                                          
         BE    VREC0080                                                         
         GOTO1 RCDEXIST,DMCB,0     DOES CODE TYPE EXIST?                        
         BZ    VERR0012            NO  - ERROR                                  
         MVC   0(2,R3),8(R2)       SAVE CODE TYPE IN STRING                     
         LA    R3,2(R3)                                                         
VREC0080 EQU   *                                                                
         LA    R2,SBMATT3H                                                      
         CLI   5(R2),0                                                          
         BE    VREC0090                                                         
         GOTO1 RCDEXIST,DMCB,0     DOES CODE TYPE EXIST?                        
         BZ    VERR0012            NO  - ERROR                                  
         MVC   0(2,R3),8(R2)       SAVE CODE TYPE IN STRING                     
         LA    R3,2(R3)                                                         
VREC0090 EQU   *                                                                
VREC0200 EQU   *                                                                
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BNE   VREC0220            NO  - ADD ACTION                             
         L     R6,AIO              SET A(RECORD IN PROCESS)                     
         USING RSPBREC,R6          USE EXISTING ELEMENT IN RECORD               
         LA    R6,RSPBELEM                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
         B     VREC0240                                                         
VREC0220 EQU   *                   BUILD A NEW ELEMENT FOR ADD                  
         XC    ELEM,ELEM           CLEAR ELEMENT BUILD AREA                     
         LA    R6,ELEM             BUILD DESCRIPTOR ELEMENT                     
VREC0240 EQU   *                                                                
         USING RSPBELEM,R6                                                      
         MVI   RSPBCODE,X'01'      SET DESCRIPTOR ELEMENT CODE                  
         LA    RF,RSPBELLE                                                      
         STC   RF,RSPBELLN         INSERT ELEMENT LENGTH                        
         MVC   RSPBDESC,SBMXNAM    INSERT DESCRIPTOR                            
         MVC   RSPBLUID,USERLUID   INSERT LUID OF SIGNON                        
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BE    VREC0260            YES - SET UP BOTH DATES                      
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSPBCDAT)                                
         B     VREC0280                                                         
VREC0260 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSPBODAT)                                
*                                  SET TODAY'S DATE AS 'CREATE DATE'            
         MVC   RSPBCDAT,RSPBODAT   SET LAST CHANGE = CREATE DATE                
VREC0280 EQU   *                                                                
         MVC   RSPCTYPS,SAVEATTS   INSERT CODE TYPES                            
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    VREC0300            YES - ELEMENT ALREADY IN RECORD              
         GOTO1 ADDELEM                                                          
VREC0300 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
*                                                                               
SAVEATTS DS    CL6                                                              
USERLUID DS    CL8                                                              
         DS    0H                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DKEY     DS    0H                                                               
         L     R4,AIO              RECORD SELECTED                              
         EJECT                                                                  
DREC     DS    0H                                                               
*                                                                               
*   CLEAR PRIOR DATA FROM SCREEN ON DISPLAY                                     
*                                                                               
         XC    SBMATT,SBMATT                                                    
         XC    SBMTYPE,SBMTYPE                                                  
         XC    SBMSID,SBMSID                                                    
         XC    SBMXNAM,SBMXNAM                                                  
         XC    SBMATT1,SBMATT1                                                  
         XC    SBMNAM1,SBMNAM1                                                  
         XC    SBMATT2,SBMATT2                                                  
         XC    SBMNAM2,SBMNAM2                                                  
         XC    SBMATT3,SBMATT3                                                  
         XC    SBMNAM3,SBMNAM3                                                  
         XC    SBMDENT,SBMDENT                                                  
         XC    SBMDUPD,SBMDUPD                                                  
         XC    SBMLUID,SBMLUID                                                  
*                                                                               
         L     R4,AIO              RECORD SELECTED                              
         USING RSPBREC,R4                                                       
         MVI   SBMATT,C'T'         SET TO 'CODE TYPE'                           
         MVC   SBMTYPE(10),=C'CODE TYPE '                                       
         CLI   RSPBKTCD,0          CODE TYPE RECORD?                            
         BE    DKEY0020            YES                                          
         MVI   SBMATT,C'C'         NO  - SET TO 'CODE'                          
         MVC   SBMTYPE(10),=C'CODE      '                                       
DKEY0020 EQU   *                                                                
         MVC   SBMSID(3),RSPBKCOD INSERT CODE                                   
         MVC   SBMXNAM(20),RSPBDESC                                             
         MVC   SBMLUID(8),RSPBLUID                                              
         MVC   SBMATT1(2),RSPCTYPS                                              
         LA    R1,SBMNAM1                                                       
         LA    R2,RSPCTYPS                                                      
         BAS   RE,ATTNAME                                                       
*                                  INSERT FIRST CODE TYPE                       
*                                                                               
*   NEED CODE HERE TO LOOK UP EXPANSION                                         
*                                                                               
         MVC   SBMATT2(2),RSPCTYPS+2                                            
         LA    R1,SBMNAM2                                                       
         LA    R2,RSPCTYPS+2                                                    
         BAS   RE,ATTNAME                                                       
*                                  INSERT 2ND   CODE TYPE                       
*                                                                               
*   NEED CODE HERE TO LOOK UP EXPANSION                                         
*                                                                               
         MVC   SBMATT3(2),RSPCTYPS+4                                            
         LA    R1,SBMNAM3                                                       
         LA    R2,RSPCTYPS+4                                                    
         BAS   RE,ATTNAME                                                       
*                                  INSERT THIRD CODE TYPE                       
*                                                                               
*   NEED CODE HERE TO LOOK UP EXPANSION                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,RSPBODAT),(5,SBMDENT)                             
         GOTO1 DATCON,DMCB,(3,RSPBCDAT),(5,SBMDUPD)                             
         BAS   RE,ATTNAME          DISPLAY CODE TYPE NAMES, IF ANY              
         MVC   SBMLAST+1(2),=X'0101'                                            
*                                  RETRANSMIT THE ENTIRE SCREEN                 
         SPACE 1                                                                
DRXIT    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   ATTNAME:  RETRIEVE CODE TYPE NAME FOR REDISPLAY                             
*        R1 -> SCREENFIELD FOR OUTPUT                                           
*        R2 -> CODE TYPE IN RECORD                                              
ATTNAME  NTR1                                                                   
         L     RF,AIO2                                                          
         ST    RF,AIO              RESET A(IO AREA) - USE # 2                   
         CLC   0(2,R2),SPACES      ANYTHING IN CODE TYPE?                       
         BNH   ATTN0200            NO  - EXIT                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'4B'           INSERT RECORD TYPE                           
         MVC   KEY+21(2),AGENCY    INSERT AGENCY CODE                           
         MVI   KEY+23,0            SET FLAG TO 'CODE TYPE'                      
         MVC   KEY+24(2),0(R2)     INSERT CODE TYPE CODE                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    ATTN0040            YES                                          
         MVC   0(20,R1),=C'** KEY NOT FOUND ** '                                
         B     ATTN0200                                                         
ATTN0040 EQU   *                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSPBREC,R6                                                       
         MVC   0(20,R1),RSPBDESC   INSERT DESCRIPTION INTO SCREEN               
         DROP  R6                                                               
ATTN0200 EQU   *                                                                
         L     RF,AIO1                                                          
         ST    RF,AIO              RESET A(IO AREA) TO # 1                      
         XIT1                                                                   
         EJECT                                                                  
                                                                                
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LIST0020                                                         
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
LIST0020 LA    R4,KEY                                                           
         USING RSPBKEY,R4                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LIST0040                                                         
         MVI   RSPBKTYP,X'4B'                                                   
         MVC   RSPBKREP,AGENCY                                                  
         MVC   SAVEKEY,KEY                                                      
LIST0040 GOTO1 HIGH                                                             
LIST0060 CLC   KEY(23),SAVEKEY     COMPARE THROUGH REP CODE                     
         BNE   LIST0800            DONE                                         
LIST0080 EQU   *                                                                
         LA    R2,SBULEVH          SET A(RECORD TYPE INDICATOR)                 
         CLI   5(R2),0             ANY INPUT?                                   
         BE    LIST0140            NO  - NO RECORD TYPE FILTERING               
         CLI   8(R2),C'T'          YES - CODE TYPE REQUESTED?                   
         BE    LIST0100            YES - CHECK KEY FOR CODE TYPE                
         CLI   8(R2),C'C'          NO  - CODE REQUESTED?                        
         BE    LIST0120            YES - CHECK KEY FOR CODE                     
         DC    H'0'                UNRECOGNIZED ENTRY                           
LIST0100 EQU   *                                                                
*                                                                               
         CLI   RSPBKTCD,0          CODE TYPE RECORD?                            
         BE    LIST0140            YES - DISPLAY                                
         B     LIST0220            NO  - SKIP THIS RECORD                       
*                                                                               
LIST0120 EQU   *                                                                
*                                                                               
         CLI   RSPBKTCD,1          CODE RECORD?                                 
         BE    LIST0140            YES - DISPLAY                                
         B     LIST0220            NO  - SKIP THIS RECORD                       
*                                                                               
         DROP  R4                                                               
*                                                                               
LIST0140 EQU   *                                                                
         GOTO1 GETREC              RETRIEVE RECORD INDICATED                    
         L     R3,AIO              SET A(RECORD RETRIEVED)                      
         USING RSPBREC,R3          SET USING                                    
         LA    R2,P                SET A(PRINT LINE)                            
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSPEC,RSPBKCOD      INSERT CODE                                  
         MVC   LFLAG(4),=C'TYPE'   INSERT 'CODE TYPE' LABEL                     
         CLI   RSPBKTCD,0          CODE TYPE?                                   
         BE    LIST0160            YES                                          
         MVC   LFLAG(4),=C'CODE'   NO  - INSERT 'CODE' LABEL                    
LIST0160 EQU   *                                                                
         MVC   LEXNAM,RSPBDESC     INSERT DESCRIPTIVE NAME                      
         GOTO1 DATCON,DMCB,(3,RSPBODAT),(5,LDENT)                               
         GOTO1 DATCON,DMCB,(3,RSPBCDAT),(5,LDCHA)                               
         CLI   RSPBKTCD,1          CODE RECORD?                                 
         BNE   LIST0180            NO  -                                        
         MVC   LDTYP1,RSPCTYPS     YES - DISPLAY CODE TYPES                     
         MVC   LDTYP2,RSPCTYPS+2                                                
                                                                                
*                                                                               
LIST0180 CLI   MODE,PRINTREP                                                    
         BNE   LIST0200                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LIST0220                                                         
         SPACE 1                                                                
LIST0200 EQU   *                   FOR LIST                                     
         MVC   LISTAR,P                                                         
         GOTO1 LISTMON                                                          
         SPACE 1                                                                
LIST0220 GOTO1 SEQ                 NEXT RECORD                                  
         LA    R4,KEY                                                           
         B     LIST0060                                                         
         SPACE 1                                                                
LIST0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
VERR0002 EQU   *                                                                
         MVC   CONHEAD+10(L'ATTNG),ATTNG                                        
         B     MYEND                                                            
VERR0004 EQU   *                                                                
         MVC   CONHEAD+10(L'ATTBIG),ATTBIG                                      
         B     MYEND                                                            
VERR0005 EQU   *                                                                
         MVC   CONHEAD+10(L'ATTBIG2),ATTBIG2                                    
         B     MYEND                                                            
VERR0006 EQU   *                                                                
         MVC   CONHEAD+10(L'NOATT),NOATT                                        
         B     MYEND                                                            
VERR0008 EQU   *                                                                
         MVC   CONHEAD+10(L'EXIST),EXIST                                        
         B     MYEND                                                            
VERR0010 EQU   *                                                                
         MVC   CONHEAD+10(L'NOEXIST),NOEXIST                                    
         B     MYEND                                                            
VERR0012 EQU   *                                                                
         MVC   CONHEAD+10(L'BADATT),BADATT                                      
         B     MYEND                                                            
VERR0014 EQU   *                                                                
         MVC   CONHEAD+10(L'NEEDCODE),NEEDCODE                                  
         B     MYEND                                                            
VERR0015 EQU   *                                                                
         MVC   CONHEAD+10(L'ACTNOGUD),ACTNOGUD                                  
         B     MYEND                                                            
ATTNG    DC    C'INVALID:  MUST BE C/A/BLANK'                                   
ATTBIG   DC    C'INVALID:  CODE TYPE MUST HAVE 2 CHARACTERS'                    
ATTBIG2  DC    C'INVALID:  CODE MUST HAVE 3 CHARACTERS'                         
NOATT    DC    C'INVALID:  MUST BE BLANK WHEN CODE TYPE=T'                      
EXIST    DC    C'INVALID:  IDENTIFIER ALREADY ON FILE'                          
NOEXIST  DC    C'INVALID:  IDENTIFIER NOT FOUND ON FILE'                        
BADATT   DC    C'INVALID:  CODE TYPE NOT FOUND ON FILE'                         
NEEDCODE DC    C'INVALID:  ADD/CHANGE REQUIRE CODE'                             
ACTNOGUD DC    C'INVALID:  ACTION CODE NOT PERMITTED'                           
         EJECT                                                                  
*                                                                               
*   RCDEXIST:  GENERAL KEY READER FOR CODES.                                    
*        R2 -> SCREEN FIELD HEADER                                              
*        P1/BYTE1:  1  =  USE VALUE OF ATTCODE AS RECORD TYPE                   
*                   0  =  FORCE TO 'CODE TYPE' (ZERO)                           
*                                                                               
RCDEXIST NTR1                                                                   
         MVC   KEYCODE,0(R1)       SAVE CALL PARAMETER                          
         XC    KEY,KEY             CLEAR KEY OUT                                
         MVI   KEY,X'4B'           SET KEY TYPE                                 
         MVC   KEY+21(2),AGENCY    INSERT REP CODE                              
         CLI   KEYCODE,0           FORCE TO 'CODE TYPE'?                        
         BE    REXE0020            YES                                          
         MVC   KEY+23(1),ATTCODE   NO  - INSERT RECORD TYPE                     
REXE0020 EQU   *                                                                
         ZIC   RF,5(R2)            TAKE LENGTH OF INPUT                         
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         EX    RF,REXE0800         INSERT CODE INTO KEY                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    REXE0100            YES - EXIT CC NOT ZERO                       
         SR    R0,R0               NO  - EXIT CC ZERO                           
         B     REXE0120                                                         
REXE0100 EQU   *                                                                
         LTR   RB,RB               EXIT CC NOT ZERO                             
REXE0120 EQU   *                                                                
         XIT1                                                                   
REXE0800 MVC   KEY+24(0),8(R2)     INSERT ID BY LENGTH                          
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,40,C'SPECIAL BUY CODES       '                                
         SSPEC H2,40,C'---------------------------------'                       
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R3,H1+64                                                         
         MVC   H8(L'HEADING),HEADING                                            
         MVC   H8+38(L'HEADING2),HEADING2                                       
         MVC   H9(84),DASH                                                      
         XIT1                                                                   
         EJECT                                                                  
DASH     DC    84C'-'                                                           
HEADING  DC    CL37'*************************************'                      
HEADING2 DC    CL46'**********************************************'             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
*                                                                               
MYERR    GOTO1 MYERROR                                                          
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* MY OWN ERROR MESSAGES                                                         
         SPACE 2                                                                
LISTD    DSECT                                                                  
         DS    CL3                 +0                                           
LSPEC    DS    CL3                 +3                                           
         DS    CL07                +6                                           
LFLAG    DS    CL4                 +13                                          
         DS    CL3                 +17                                          
LEXNAM   DS    CL20                +20                                          
         DS    CL7                 +40                                          
LDENT    DS    CL8                 +47                                          
         DS    CL2                 +55                                          
LDCHA    DS    CL8                 +57                                          
         DS    CL1                 +65                                          
LDTYP1   DS    CL2                 +66                                          
         DS    CL1                 +68                                          
LDTYP2   DS    CL2                 +69                                          
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMF8D                                                                      
* REGENSWI                                                                      
* REGENSTA                                                                      
* REGENSAL                                                                      
* RESFMWORKD                                                                    
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM80D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM81D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
FLAGS    DS    XL1                 INDICATORS                                   
ATTCODE  DS    XL1                 0 = CODE TYPE (1 CHAR)                       
*                                  1 = CODE      (3 CHAR)                       
*                                                                               
KEYCODE  DS    XL1                 PARAMETER PASSED BY CALL TO                  
*                                     RCDEXIST ROUTINE                          
*                                                                               
SAVEKEY  DS    CL27                                                             
SAVE2KEY DS    CL27                                                             
OLDKEY   DS    CL27                                                             
ELEMSAVE DS    CL256                                                            
BLOC     DS    CL750               WORK AREA                                    
         SPACE 4                                                                
REPREC   DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENSPEC         SPECIAL BUY CODE RECORD                      
       ++INCLUDE FAFACTS                                                        
*                                                                               
         CSECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'180RESFM1E   01/22/13'                                      
         END                                                                    
