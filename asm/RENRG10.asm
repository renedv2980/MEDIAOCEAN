*          DATA SET RENRG10    AT LEVEL 020 AS OF 05/01/02                      
*          DATA SET RENRG02    AT LEVEL 018 AS OF 05/09/97                      
*PHASE T83010B,*                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
         TITLE 'T83010 RRGON REC PASS NRGON DATA TO WRITER'                     
***********************************************************************         
* PROFILE BIT USE:                                                    *         
* BYTE 0, X'80'  -  SUPPRESS BUDGET COLUMN                            *         
* BYTE 0, X'40'  -  SUPPRESS BUDGET IF REQUEST IS FROM STATION SIDE.  *         
* BYTE 0,X'20'   -  USE STRAIGHT MONTH TO MONTH, NOT 4/5 WK PACING    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 -                                                         *         
*        R3 -                                                         *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG                                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 -                                                         *         
*        R8 - SECOND BASE                                             *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO RRGWRI DSECT FROM THE WRITER                 *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
***********************************************************************         
T83010   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 2500,T83010,R8,RR=R2                                             
         L     RA,0(R1)                                                         
         USING RRGWRI,RA                                                        
         CLC   =C'**WRIT**',RRGWID                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING GEND,RC                                                          
         ST    RD,BASERD                                                        
         LA    R9,IO                                                            
         LA    R9,4016(,R9)        2 2000 BYTE I/O + 2 8 BYTE LABELS            
         USING SYSD,R9                                                          
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         SPACE                                                                  
         CLI   RRGWOFFL,C'Y'                                                    
         B     *+8                                                              
         BAS   RE,PRTRRGW                                                       
         SPACE                                                                  
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    *+14                                                             
         SPACE                                                                  
         OC    RRGWDAT1,RRGWDAT1   ALREADY PASSING DATA                         
         BNZ   LREC0010                                                         
         SPACE                                                                  
         MVI   RRGWERR,0                                                        
         MVI   RRGWSTAT,0                                                       
         SPACE                                                                  
         LR    RE,RC                                                            
         LA    RF,1500                                                          
         SLL   RF,3                                                             
         XCEF                                                                   
         ST    R2,RELO                                                          
*        ST    R9,SYSPARMS                                                      
         ST    RC,SVRC                                                          
         LA    R0,2                ONLY 2 I/O AREAS                             
         LA    R1,IO-8                                                          
         LA    RE,X'F1'                                                         
         LA    RF,AIO1                                                          
GNINIT10 MVC   0(8,R1),=C'**I/O1**'                                             
         STC   RE,5(R1)                                                         
         LA    R1,8(,R1)                                                        
         ST    R1,0(,RF)                                                        
         LA    R1,2000(,R1)                                                     
         LA    RE,1(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,GNINIT10                                                      
         MVC   AIO,AIO1                                                         
         MVC   KEY-8(8),=C'**KEYS**'                                            
         MVC   DUB-8(8),=C'**DUB***'                                            
         MVC   ADDAY-8(8),=C'*EXTRNS*'                                          
         MVC   BOOKVAL-8(8),=C'*CORERES'                                        
         MVC   DATADISP-8(8),=C'*SYSCON*'                                       
         SPACE                                                                  
         CLI   RRGWOFFL,C'Y'       IF OFFLINE, OPEN FILE                        
         BNE   INIT                                                             
         SPACE                                                                  
         L     R2,AIO1                                                          
         GOTO1 RRGWDMGR,DMCB,=C'DMOPEN',=C'REP',RRGLST,(R2)                     
         B     INIT                                                             
         DS    0D                                                               
RRGLST   DC    CL8'NRRGNEW'                                                     
         DC    C'X'                                                             
         SPACE 3                                                                
EXIT     DS   0H                                                                
         CLI   RRGWOFFL,C'Y'                                                    
         B     *+8                                                              
         BAS   RE,PRTRRGW                                                       
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INITIALIZE ROUTINE                                                     
*                                                                               
INIT     DS   0H                                                                
         OI    DISPFLAG,OPTRACE                                                 
         MVI   COMPBUDG,C'Y'       SET 'RETRIEVE COMPANY BUDGET' ON             
         CLI   LISTEND,C'N'        PASSING DATA FROM THIS PASS?                 
         BE    LREC0010             YES                                         
         CLI   LISTEND,C'Y'        END OF DATA FROM THIS PASS?                  
         BE    LREC0260             YES                                         
         SPACE                                                                  
* CLEAR SAVED SETS AREAS *                                                      
         SPACE                                                                  
         XC    SET1CDE(6),SET1CDE  ALL SET CODES                                
         LR    R0,R9                                                            
         AH    R0,=AL2(SET1TAB-SYSD)                                            
         LA    R1,(L'SET1TAB*3)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
*                                                                               
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
*                                                                               
         XC    AFSTFLD,AFSTFLD                                                  
*                                                                               
* CLEAR ALL FILTERS                                                             
*                                                                               
         XC    QREGION(QLIST-QREGION),QREGION                                   
*                                                                               
         XC    ELEM,ELEM           BUILD 1ST PART OF RRGON KEY IN ELEM          
         SPACE                                                                  
         MVC   AGENCY,RRGWREP                                                   
         SPACE                                                                  
         MVC   QSTART,RRGWSTDT                                                  
         MVC   QEND,RRGWEDDT                                                    
         SPACE                                                                  
         BAS   RE,RSETRRGO         SWITCH BACK TO RRGON FILE VALUES             
         SPACE                                                                  
         XC    KEY,KEY             READ RRGON HEADER RECORD                     
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVI   ROKHD1+1,X'01'                                                   
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
         SPACE                                                                  
         CLC   ROKEY,KEY                                                        
         BNE   VKNODATA                                                         
         L     R7,AIO                                                           
         MVC   SVTYPES,RODHDOPT    SAVE DOLLAR TYPES AVAILABLE                  
         SPACE                                                                  
         LA    R0,L'SVTYPES                                                     
         LA    R1,SVTYPES                                                       
         SPACE                                                                  
         CLI   RRGWDOLT,0           SPECIFIC DOLLAR TYPE REQUESTED?             
         BE    INIT0060             NO, USE WHATEVER ON FILE                    
         SPACE                                                                  
INIT0050 CLC   RRGWDOLT,0(R1)       THIS A VALID TYPE FOR THIS REP              
         BE    INIT0070             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,INIT0050                                                      
         MVI   RRGWERR,RRGWER0F                                                 
         B     ERREXIT                                                          
         SPACE                                                                  
VKNODATA MVI   RRGWERR,RRGWER10                                                 
         SPACE                                                                  
ERREXIT  MVI   RRGWSTAT,01                                                      
         B     EXIT                                                             
         SPACE                                                                  
INIT0060 CLI   0(R1),0             THIS A VALID TYPE                            
         BNE   INIT0070             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,INIT0060                                                      
         DC    H'0'                                                             
INIT0070 MVC   TYPEKEY,0(R1)       SAVE WHATEVER IT IS                          
         MVC   RRGWDOLT,0(R1)                                                   
         SPACE                                                                  
         CLI   TYPEKEY,C'A'        ALL DOLLARS BECOMES NULL                     
         BNE   *+8                  NO                                          
         MVI   TYPEKEY,0                                                        
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(3),RRGWDT1     MOVE IN PASSED DATA TYPES                    
         MVC   ELEM+8(24),RRGWDA1                                               
*                                                                               
* NEED TO FILL FILTER FIELDS FROM RRGWRI DSECT                                  
*                                                                               
         LA    R2,RRGWDT1          FIELD TYPE                                   
         LA    R3,RRGWDA1          FIELD DATA                                   
         LA    R0,3                                                             
INIT0100 CLI   0(R3),0             ANY DATA TO FILTER                           
         BE    INIT0150             NO                                          
         SPACE                                                                  
         L     R1,=A(FILTAB)                                                    
         SPACE                                                                  
INIT0104 CLC   0(1,R1),0(R2)                                                    
         BE    INIT0106                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),0                                                          
         BNE   INIT0104                                                         
         DC    H'0'                                                             
INIT0106 LH    RE,2(R1)                                                         
         AR    RE,R9                                                            
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,INITMVC                                                       
         CLI   0(R2),QLSTA                                                      
         BNE   INIT0110                                                         
         SPACE                                                                  
         LA    R1,4(,RE)                                                        
         CLI   3(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         SPACE                                                                  
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R3)                                                    
         CLI   4(R3),C' '                                                       
         BNE   INIT0108                                                         
         MVC   1(2,R1),=C'TV'                                                   
         B     INIT0150                                                         
         SPACE                                                                  
INIT0108 DS    0H                                                               
         CLI   4(R3),C'A'                                                       
         BE    *+12                                                             
         CLI   4(R3),C'A'                                                       
         BNE   *+12                                                             
         MVI   2(R1),C'M'                                                       
         B     INIT0150                                                         
         SPACE                                                                  
         CLC   4(R3),C'C'                                                       
         BNE   INIT0150                                                         
         GOTO1 =A(GETCOMBO),RR=RELO    YES-GET THE COMBINED STATIONS            
         B     INIT0150                                                         
         SPACE                                                                  
INITMVC  MVC   0(0,RE),0(R3)                                                    
         SPACE                                                                  
INIT0110 CLI   0(R2),QLAGY                                                      
         BNE   INIT0150                                                         
         SPACE                                                                  
         ST    R0,FULL                                                          
         SPACE                                                                  
         MVC   QAGY,BLANKS                                                      
         LA    R0,6                                                             
         LA    RE,QAGY                                                          
         LR    RF,R3                                                            
INIT0112 CLI   0(RF),C'-'                                                       
         BE    INIT0114                                                         
         CLI   0(RF),C' '                                                       
         BNH   INIT0118                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,INIT0112                                                      
INIT0114 LA    RE,QAGY+4                                                        
         LA    RF,1(,RF)                                                        
         LA    R0,2                                                             
         SPACE                                                                  
INIT0116 CLI   0(RF),C' '                                                       
         BNH   INIT0118                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,INIT0116                                                      
         SPACE                                                                  
INIT0118 L     R0,FULL                                                          
         SPACE                                                                  
INIT0150 LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         CLI   0(R2),0             END OF DATA TYPES                            
         BE    INIT0200                                                         
         BCT   R0,INIT0100                                                      
         SPACE                                                                  
* FOR SETS, NEED TO ACCESS FILTAB, AND SPREAD DATA AND TYPES *                  
* AND USE CODE BELOW                                                            
         SPACE                                                                  
INIT0200 LA    R4,ELEM                                                          
         LA    R5,ELEM+8                                                        
         OC    QGROUP,QGROUP                                                    
         BZ    INIT0310                                                         
         CLI   QGROUP,C'*'         SETS                                         
         BE    INIT0300             YES                                         
         CLI   QGROUP+1,0                                                       
         BNE   INIT0300                                                         
         MVI   0(R4),QLGRGRP       GRGRP (1 CHARACTER)                          
         MVC   0(1,R5),QGROUP                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
         B     INIT0310                                                         
*                                                                               
INIT0300 MVI   0(R4),QLGRP         GROUP (2 CHARACTERS)                         
         MVC   0(2,R5),QGROUP                                                   
         SPACE                                                                  
         CLI   QGROUP,C'*'         SET                                          
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0310 OC    QSTA,QSTA                                                        
         BZ    INIT0320                                                         
         MVI   ROUNDSW,C'N'                                                     
         MVI   0(R4),QLSTA         STATION                                      
         MVC   0(7,R5),QSTA                                                     
         SPACE                                                                  
         CLI   QSTA,C'*'                                                        
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         CLI   NCOMBOS,0           LIST=COMBO                                   
         BE    INIT0314             NO                                          
*                                                                               
         MVC   0(7,R5),COMBOSTA    USE FIRST COMBO STATION                      
         XC    QSTA,QSTA                                                        
*                                                                               
INIT0314 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0320 OC    QREGION,QREGION                                                  
         BZ    INIT0330                                                         
         MVI   0(R4),3             REGION                                       
         MVC   0(2,R5),QREGION                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0330 OC    QOFF,QOFF                                                        
         BZ    INIT0340                                                         
         MVI   ROUNDSW,C'N'                                                     
         MVI   0(R4),4             OFFICE                                       
         MVC   0(2,R5),QOFF                                                     
         SPACE                                                                  
         CLI   QOFF,C'*'                                                        
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0340 OC    QTEAM,QTEAM                                                      
         BZ    INIT0344                                                         
         MVI   0(R4),QLTEM         TEAM                                         
         MVC   0(2,R5),QTEAM                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0344 OC    QADV,QADV                                                        
         BZ    INIT0346                                                         
         MVI   0(R4),QLADV         ADVERTISER                                   
         MVC   0(L'QADV,R5),QADV                                                
         SPACE                                                                  
         CLI   QADV,C'*'                                                        
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0346 OC    QAGY,QAGY                                                        
         BZ    INIT0350                                                         
         MVI   0(R4),QLAGY         AGENCY                                       
         MVC   0(L'QAGY,R5),QAGY                                                
         SPACE                                                                  
         CLI   QAGY,C'*'                                                        
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0350 OC    QAFF,QAFF                                                        
         BZ    INIT0360                                                         
         MVI   0(R4),QLAFF         AFFILIATE                                    
         MVC   0(3,R5),QAFF                                                     
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0360 OC    QCLASS,QCLASS                                                    
         BZ    INIT0370                                                         
         MVI   0(R4),QLCLS         CLASS                                        
         MVC   0(2,R5),QCLASS                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0370 OC    QCTGY,QCTGY                                                      
         BZ    INIT0380                                                         
         MVI   0(R4),QLCAT         CATEGORY                                     
         MVC   0(2,R5),QCTGY                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0380 CLI   QSTATY,0                                                         
         BE    INIT0390                                                         
         MVI   0(R4),QLSTY         STATION TYPE                                 
         MVC   0(1,R5),QSTATY                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0390 OC    QTVB,QTVB                                                        
         BZ    INIT0400                                                         
         MVI   0(R4),QLTVB         TVB                                          
         MVC   0(2,R5),QTVB                                                     
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0400 OC    QOWNER,QOWNER                                                    
         BZ    INIT0410                                                         
         MVI   0(R4),QLOWN         OWNER                                        
         MVC   0(3,R5),QOWNER                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0410 OC    QCONTY,QCONTY                                                    
         BZ    INIT0420                                                         
         MVI   0(R4),QLCON         CONTRACT TYPE                                
         MVC   0(L'QCONTY,R5),QCONTY                                            
         SPACE                                                                  
         CLI   QCONTY,C'*'                                                      
         BNE   INIT0414                                                         
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         CLI   QCONTY+1,C'&&'      THIS SPECIAL HARD CODE CONTRACT TYPE         
         BNE   INIT0414                                                         
         SPACE                                                                  
         OI    DISPFLAG,OPCONDCT   SET ON HARDCODE QLCON/QLDCT                  
         SPACE                                                                  
INIT0414 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
         SPACE                                                                  
INIT0420 CLI   QRANK,0                                                          
         BE    INIT0430                                                         
         MVI   0(R4),QLRNK         MARKET RANK                                  
         MVC   0(1,R5),QRANK                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0430 OC    QMKT,QMKT                                                        
         BE    INIT0434                                                         
         MVI   0(R4),QLMKT         MARKET                                       
         MVC   0(4,R5),QMKT                                                     
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0434 OC    QDCT,QDCT                                                        
         BE    INIT0440                                                         
         TM    DISPFLAG,OPCONDCT   HARDCODE QLCON/QLDCT                         
         BZ    *+12                 YES, CAN'T ENTER DCT                        
*                                                                               
         MVI   RRGWERR,RRGWER02    CAN'T ENTER DCT WITH HARDCODE                
         B     ERREXIT                                                          
*                                                                               
         SPACE                                                                  
         MVI   0(R4),QLDCT         DEVELOPMENTAL CONTRACT TYPE                  
         MVC   0(L'QDCT,R5),QDCT                                                
         SPACE                                                                  
         CLI   QDCT,C'*'                                                        
         BNE   *+8                                                              
         BAS   RE,SETK             GO SET KEY FROM TABLE                        
         SPACE                                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
INIT0440 MVC   DUB,ELEM                SAVE POSSIBLE SETS                       
         MVC   SETKEY,ELEM                                                      
         NC    ELEM(5),=X'7F7F7F7F7F'  SET OFF POSSIBLE SETS                    
         SPACE                                                                  
         CLI   ELEM,QLGRGRP        TEST GRGRP IN KEY                            
         BE    INIT0442                                                         
         CLI   ELEM,QLGRP          TEST GRP IN KEY                              
         BNE   INIT0450                                                         
*                                                                               
INIT0442 CLI   ELEM+1,0            AND OTHERS ALSO IN KEY                       
         BE    INIT0450             NO, LEAVE IT ALONE                          
*                                                                               
*                                                                               
         MVC   ELEM(7),ELEM+1                                                   
         MVI   ELEM+7,0                                                         
         MVC   SETKEY,DUB+1                                                     
         BCTR  R4,0                                                             
         MVC   ELEM+8(24),ELEM+16                                               
         XC    ELEM+40(8),ELEM+40                                               
*                                                                               
INIT0450 LA    RE,ELEM+L'ROKDTLTY                                               
         CR    R4,RE               CHECK NO MORE THAN MAX ROWS                  
         BNH   *+6                                                              
*                                                                               
         DC    H'0'                                                             
*                                                                               
         SPACE                                                                  
         CLI   ELEM,0              CHECK FOR NO ROWS                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
INIT0452 MVC   SVKEY(L'ROKDTLTY),ELEM   SAVE THE KEY                            
         MVC   SVKEY+L'ROKDTLTY(24),ELEM+8                                      
*                                                                               
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
*                                                                               
         XC    KEY,KEY             READ RRGON RECORD TYPE HEADER                
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKHD2RQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVI   ROKHD2+1,X'02'                                                   
         MVC   ROKHD2TY,SVKEY                                                   
         SPACE                                                                  
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
         CLI   SVKEY+1,0           MORE THAN ONE DATA TYPE                      
         BE    NOENTERR             NO, BAD REQUEST                             
         CLI   SVKEY+2,0           THREE ENTRIES                                
         BNE   INIT0460             YES                                         
         SPACE                                                                  
* SWAP ENTRIES                                                                  
         SPACE                                                                  
         MVC   BYTE,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),ROKHD2TY+1                                           
         MVC   ROKHD2TY+1(1),BYTE                                               
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
NOENTERR MVI   RRGWERR,RRGWER01    COMBO NOT IN TABLE                           
         B     ERREXIT                                                          
         SPACE                                                                  
VKPERERR MVI   RRGWERR,RRGWER09    REQUEST DATES NOT ON FILE                    
         B     ERREXIT                                                          
         SPACE                                                                  
INIT0460 DS    0H                                                               
         MVC   FULL,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),FULL                                                 
         MVC   ROKHD2TY+1(1),FULL+2                                             
         MVC   ROKHD2TY+2(1),FULL+1                                             
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
         MVC   FULL,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),FULL+1                                               
         MVC   ROKHD2TY+1(1),FULL                                               
         MVC   ROKHD2TY+2(1),FULL+2                                             
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
         MVC   FULL,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),FULL+1                                               
         MVC   ROKHD2TY+1(1),FULL+2                                             
         MVC   ROKHD2TY+2(1),FULL                                               
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
         MVC   FULL,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),FULL+2                                               
         MVC   ROKHD2TY+1(1),FULL                                               
         MVC   ROKHD2TY+2(1),FULL+1                                             
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BE    INIT0500                                                         
         MVC   FULL,ROKHD2TY                                                    
         MVC   ROKHD2TY(1),FULL+2                                               
         MVC   ROKHD2TY+1(1),FULL+1                                             
         MVC   ROKHD2TY+2(1),FULL                                               
         BAS   RE,VALCOM           GO CHECK OUT AS IS                           
         BNE   NOENTERR                                                         
         B     INIT0500                                                         
         SPACE                                                                  
* CHECK OUT LCOMBOS TABLE FOR VALID ENTRY                                       
         SPACE                                                                  
VALCOM   DS   0H                                                                
         L     R1,=A(LCOMBOS)      YES - FIND RECORD TYPE FOR LIST              
         A     R1,RELO                                                          
         MVI   BYTE,0                                                           
         USING LCOMBOSD,R1                                                      
*                                                                               
VALCOM20 DS   0H                                                                
*        CLC   ROKHD2TY,LFTRFLDS   ENTRY EQUAL TO FILTERS ENTERED               
         CLC   ROKHD2TY,LALTKEY    ENTRY EQUAL TO FILTERS ENTERED               
         BER   RE                                                               
*                                                                               
         LA    R1,LCNEXT                                                        
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   VALCOM20             NOT YET                                     
*                                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
INIT0500 DS   0H                                                                
*        MVC   ROKHD2TY,LALTKEY    MOVE IN KEY STRUCTURE TO USE                 
         DROP  R1                                                               
         SPACE                                                                  
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
         SPACE                                                                  
         MVC   KEY,0(R7)                                                        
         MVI   TRCTYPE,01                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   ROKEY(ROKHD2ST-ROKEY),KEY                                        
         BE    *+12                                                             
*                                                                               
         MVI   RRGWERR,RRGWER08    DATA TYPE COMBO NOT ON FILE                  
         B     ERREXIT                                                          
         SPACE                                                                  
INIT0502 MVI   YRSW,0                                                           
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BNE   *+14                                                             
         OC    RRGWSTDT,RRGWSTDT   ANY START DATE?                              
         BZ    INIT0504                                                         
*                                                                               
         CLC   RRGWSTDT,ROKHD2EN   START VS FILE END                            
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
*                                                                               
         CLC   RRGWEDDT,ROKHD2ST       END VS FILE START                        
         BNL   INIT0504                                                         
*                                                                               
         MVC   DUB(2),RRGWSTDT                                                  
         MVI   DUB+2,15                                                         
         GOTO1 RRGWDATC,DMCB,(3,DUB),(0,WORK)                                   
         GOTO1 RRGWADAY,(R1),WORK,WORK+6,F'365'                                 
         GOTO1 RRGWDATC,(R1),(0,WORK+6),(3,FULL)                                
*                                  WITHIN FILE PERIOD                           
         CLC   FULL(2),ROKHD2EN     START VS FILE END                           
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
         MVC   FULL+1(1),QEND+1                                                 
         CLC   FULL(2),ROKHD2ST     END VS FILE START                           
         BL    VKPERERR            ENDS BEFORE FILE STARTS = ERROR              
         MVC   QSTART(1),FULL                                                   
         MVC   QEND(1),FULL                                                     
         MVI   YRSW,C'P'           SET TO PRIOR YEAR                            
         SPACE                                                                  
INIT0504 CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    EXIT                                                             
         SPACE                                                                  
         MVC   QAFF,RRGWAFF                                                     
         MVC   QTVB,RRGWTVB                                                     
         MVC   QRANK,RRGRNK                                                     
         MVC   QOWNER,RRGOWN                                                    
         MVC   QGROUP,RRGGRP                                                    
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLRQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         SPACE                                                                  
         CLI   NCOMBOS,0           TEST COMBINED STATIONS                       
         BE    INIT0530             NO                                          
         SPACE                                                                  
         LA    RE,ROKDTLTY         YES-FIND DISPLACEMENT INTO KEY OF            
         LA    RF,ROKDTLVL             STATION VALUE                            
         LA    R0,L'ROKDTLTY                                                    
*                                                                               
INIT0520 CLI   0(RE),2                                                          
         BE    INIT0524                                                         
         LA    RE,1(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,INIT0520                                                      
*                                  WITHIN FILE PERIOD                           
         MVI   RRGWERR,RRGWER0A    REQUESTED COMBO STATIONS NOT ON FILE         
         B     ERREXIT                                                          
*                                  WITHIN FILE PERIOD                           
INIT0524 SR    RF,R7                                                            
         ST    RF,STADISP                                                       
         LA    R4,COMBOSTA         BRANCH THROUGH STATIONS UNTIL AN             
         ZIC   R5,NCOMBOS          RRGON RECORD IS FOUND                        
*                                                                               
INIT0530 DS    0H                                                               
         L     R7,AIO1                                                          
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,(R7)                     
*                                  READ FIRST RRGON DETAIL RECORD               
         MVC   KEY,0(R7)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,02                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   ROKDTLTY,SVKEY      SEE IF RIGHT DATA TYPES                      
         BNE   VKRECERR                                                         
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),ROKEY                                        
         BE    INIT0538                                                         
*                                                                               
         CLI   NCOMBOS,0           NOT FOUND - TEST COMBINED STATIONS           
         BE    INIT0531             NO, SEE IF SETS                             
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BNE   INIT0531             YES, CHECK OUT AS SETS                      
         SPACE                                                                  
         LA    R4,7(R4)            YES-TRY NEXT COMBINED STATION                
         BCT   R5,*+8                                                           
         B     VKRECERR                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         L     R1,STADISP                                                       
         LA    R1,KEY(R1)                                                       
         MVC   0(7,R1),0(R4)                                                    
         B     INIT0530                                                         
         SPACE                                                                  
INIT0531 DS   0H                                                                
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BE    VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
         BAS   RE,CSETL            SEE IF LEADING NON-SET FIELDS OK             
         BNE   VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
         LA    R3,3                                                             
         LA    R2,KEY+ROKDTLTY-ROKEY                                            
         LA    R6,KEY+ROKDTLVL-ROKEY                                            
INIT0532 LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
INIT0533 CLC   0(1,R2),0(R1)                                                    
         BE    INIT0537                                                         
         LA    R1,L'SET1CDE(,R1)                                                
         BCT   R0,INIT0533                                                      
         SPACE                                                                  
         L     RF,=A(FILTAB)       CHECK NON-SET DATA                           
         A     RF,RELO                                                          
         SPACE                                                                  
INIT0534 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'FILTAB(RF)                                                  
         B     INIT0534                                                         
         SPACE                                                                  
         LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,INITCLCB         TEST FILTER IS SET (COMP TO SPACES)          
         BH    INIT0535                                                         
         SPACE                                                                  
         CLI   0(R2),QLSTA                                                      
         BNE   INIT053F                                                         
         CLI   NCOMBOS,0                                                        
         BE    INIT053F                                                         
         LA    R4,COMBOSTA         BRANCH THROUGH STATIONS UNTIL AN             
         ZIC   R5,NCOMBOS          RRGON RECORD IS FOUND                        
INIT053D CLC   0(7,R6),0(R4)                                                    
         BE    INIT0536                                                         
         BL    INIT053E                                                         
         LA    R4,7(,R4)                                                        
         BCT   R5,INIT053D                                                      
         B     VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
INIT053E MVC   0(7,R6),0(R4)                                                    
         B     INIT0530                                                         
         SPACE                                                                  
INIT053F DC    H'0'                                                             
         SPACE                                                                  
INIT0535 EX    RE,INITCLCD         SEE IF EQUAL                                 
         BE    INIT0536                                                         
         SPACE                                                                  
* NEED TO FIND PREV SET AND BUMP IT                                             
         SPACE                                                                  
         GOTO1 =A(PSET),RR=RELO                                                 
         BE    INIT0530                                                         
*                                  WITHIN FILE PERIOD                           
VKRECERR MVI   RRGWERR,RRGWER0B    REQUESTED DATA TYPES NOT ON FILE             
         B     ERREXIT                                                          
*                                  WITHIN FILE PERIOD                           
         SPACE                                                                  
INITCLCB CLC   0(0,R1),BLANKS                                                   
INITCLCD CLC   0(0,R1),0(R6)                                                    
         SPACE                                                                  
INIT0536 LA    R2,1(,R2)           NEXT KEY FLD                                 
         LA    R6,8(,R6)           NEXT KEY FLD                                 
         CLI   0(R2),0             ANOTHER CODE                                 
         BE    INIT0538             NO                                          
         BCT   R3,INIT0532                                                      
         SPACE                                                                  
         B     INIT0538                                                         
         SPACE                                                                  
INIT0537 DS   0H                                                                
         SPACE                                                                  
         LA    R0,KEY+5                                                         
         ST    R0,SETKEYAD                                                      
         SPACE                                                                  
         GOTO1 =A(FSET),RR=RELO    FILTER ON SET                                
         BNE   INIT0530             GO TRY RDHI                                 
         LTR   R3,R3               ANY MORE FIELDS                              
         BZ    INIT0538            NO                                           
         B     INIT0536                                                         
*                                                                               
INIT0538 DS   0H                                                                
         CLI   NCOMBOS,0           TEST COMBO STATION LIST                      
         BE    INIT0540                                                         
         STC   R5,NCOMBOS          YES-SAVE N'ACTIVE STATIONS                   
         ST    R4,ACOMBOS                   AND A(FIRST ACTIVE ONE)             
*                                                                               
INIT0540 DS   0H                                                                
*                                                                               
         CLC   QSTART(1),QEND      TEST FOR PERIOD START/END YEARS SAME         
         BNE   INIT0570                                                         
         MVC   FULL(2),QSTART      YES - SET UP WEEKS PER MONTH                 
         MVI   FULL+2,15                                                        
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   INIT0556                                                         
         ZIC   R0,FULL                                                          
         BCTR  R0,0                                                             
         STC   R0,FULL                                                          
         SPACE                                                                  
INIT0556 GOTO1 RRGWDATC,DMCB,(3,FULL),(0,WORK)                                  
         MVC   FULL(2),WORK        SAVE CURRENT YEAR IN FULL                    
         LA    R4,CURWKS           WEEKS/MONTH CURRENT YEAR                     
         LA    R5,CURWKSY          YTD WEEKS CURRENT YEAR                       
         SR    R0,R0                                                            
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'          START FROM JANUARY                           
*                                                                               
INIT0560 OI    HALF+1,X'0F'                                                     
         UNPK  WORK+2(2),HALF                                                   
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+6,0,0,RR=RELO                        
         IC    R0,0(R1)            WEEKS PER BROADCAST MONTH                    
         AR    R6,R0                                                            
         STC   R0,0(R4)                                                         
         STC   R6,0(R5)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         AP    HALF,=P'1'          NEXT MONTH                                   
         CP    HALF,=P'12'         TEST DONE WHOLE YEAR                         
         BNH   INIT0560                                                         
         CLC   WORK(2),FULL        YES -                                        
         BNE   INIT0570                                                         
         PACK  DUB,WORK(2)         NOW DO PRIOR YEAR                            
         SP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         LA    R4,PRIWKS                                                        
         LA    R5,PRIWKSY                                                       
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'                                                       
         B     INIT0560                                                         
*                                                                               
INIT0570 DS   0H                                                                
         XC    KEY,KEY             SET FOR INIT                                 
         EJECT                                                                  
* READ ALL RECORDS REQUESTED                                                    
*                                                                               
LREC0010 DS   0H                                                                
         OC    KEY(L'ROKEY),KEY    TEST KEY IS ZERO                             
         BZ    LREC0020            YES-FIRST TIME THROUGH                       
         LA    R7,KEY              NO-PASSING RECS TO WRITER                    
         USING RORECD,R7                                                        
         MVI   ROKDTLYM+2,X'FF'    NEXT MONTH                                   
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,03                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         CLC   LSTKEY,KEY+ROKDTLTY-ROKEY                                        
         BNE   LREC0260                                                         
         B     LT200                                                            
         SPACE                                                                  
* CLEAR SAVED CODE TABLE *                                                      
         SPACE                                                                  
LREC0020 DS   0H                                                                
         L     R1,=A(LCOMBOS)                                                   
         A     R1,RELO                                                          
         USING LCOMBOSD,R1                                                      
         SPACE                                                                  
LREC0030 CLI   0(R1),X'FF'         FIND THE KEY COMBINATION                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LALTKEY,SVKEY       ENTRY EQUAL TO FILTERS ENTERED               
         BE    LREC0060                                                         
*                                                                               
LREC0050 DS   0H                                                                
         LA    R1,LCNEXT                                                        
         B     LREC0030                                                         
*                                                                               
LREC0060 DS   0H                                                                
         MVC   LSTKEY,SVKEY        SAVE LIST KEY                                
         DROP  R1                                                               
         SPACE                                                                  
         MVI   LISTEND,C'N'                                                     
         MVI   FOUNDSET,C'N'                                                    
         MVI   FIRSTR,C'Y'                                                      
         LA    R7,KEY              SET FIRST PART OF KEY                        
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLRQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY,LSTKEY                                                  
         MVC   ROKDTLVL(24),ELEM+8                                              
*                                                                               
LREC0100 DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,04                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
*                                                                               
LREC0110 CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         CLC   LSTKEY,KEY+ROKDTLTY-ROKEY                                        
         BNE   LREC0260                                                         
         SPACE                                                                  
         L     R7,AIO              ADDRESS THE RECORD                           
*                                                                               
         LA    R0,3                CHECK RECORD AGAINST REQUEST FILTERS         
         LA    R2,ROKDTLTY                                                      
         LA    R3,ROKDTLVL                                                      
         LA    R4,RODATA-ROKDTLVL-1                                             
         LA    R5,RODATA-ROKDTLVL-9                                             
         LA    R6,KEY+ROKDTLVL-ROKEY                                            
*                                                                               
LREC0130 CLI   0(R2),0                                                          
         BE    LREC0210                                                         
         CLC   0(8,R3),BLANKS      YES-TEST SIGNIFICANT VALUE                   
         BNH   LREC0190            NO-SKIP                                      
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
LREC0140 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    LREC0142                                                         
         LA    RF,L'FILTAB(RF)                                                  
         B     LREC0140                                                         
         SPACE                                                                  
LREC0142 LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,LREX1            TEST FILTER IS SET (COMP TO SPACES)          
         BNH   LREC0160             NO                                          
         SPACE                                                                  
         CLI   0(R1),C'*'          IS THIS A SETS REQUEST?                      
         BNE   LREC0144             NO                                          
         SPACE                                                                  
         LA    RF,5(,R7)                                                        
         ST    RF,SETKEYAD                                                      
         SPACE                                                                  
         GOTO1 =A(FSET),RR=RELO    FILTER ON SET                                
         BE    LREC0200                                                         
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
         SPACE                                                                  
         SPACE                                                                  
LREC0144 EX    RE,LREX2            YES-TEST RECORD VALUE AGAINST FILTER         
         BE    LREC0160            EQUAL-OK                                     
         BH    LREC0150            HIGH                                         
         XC    0(8,R6),0(R6)       LOW-SKIP TO FILTER VALUE                     
         EX    RE,LREX3                                                         
         SPACE                                                                  
         EX    R5,LREX4            NEXT FIELD TO NULLS                          
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0150 DS   0H                                                                
         EX    R4,LREX5            NO-SKIP                                      
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
         SPACE                                                                  
LREC0160 CLI   0(R2),QLOFF         YES-TEST ON OFFICE NOW                       
         BNE   LREC0170                                                         
         OC    0(2,R3),0(R3)       YES-TEST NULL OFFICE                         
         BZ    LREC0190            YES-SKIP                                     
         OC    QREGION,QREGION     TEST REGION FILTER                           
         BZ    LREC0200                                                         
         BAS   RE,CHKRGN           YES-CHECK OFFICE IN REGION                   
         BNE   LREC0190            NO-SKIP                                      
         B     LREC0200                                                         
*                                                                               
LREC0170 DS   0H                                                                
         CLI   0(R2),QLSTA         YES-TEST ON STATION NOW                      
         BNE   LREC0200                                                         
*                                                                               
         CLI   NCOMBOS,0                                                        
         BE    LREC0174                                                         
         LA    RE,COMBOSTA          ADDRESS OF COMBO STATION TABLE              
         ZIC   RF,NCOMBOS           N'STATIONS                                  
LREC0172 CLC   0(7,R3),0(RE)       THIS ONE OF THE ONES WANTED                  
         BE    LREC0176             YES                                         
         LA    RE,7(,RE)                                                        
         BCT   RF,LREC0172                                                      
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'11'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         B     LREC0110            NO-SKIP                                      
*                                                                               
LREC0174 DS    0H                                                               
         CLC   =C'PV',AGENCY       THIS PETRY                                   
         BE    *+12                 YES                                         
         CLI   QGROUP+1,C' '       YES-TEST SUBGROUP FILTER                     
         BNH   LREC0176                                                         
         BAS   RE,CHKGRP           YES-CHECK STATION IN GROUP                   
         BNE   LREC0190            NO-SKIP                                      
         SPACE                                                                  
LREC0176 OC    QAFF,QAFF           TEST AFFILIATE FILTER                        
         BZ    *+12                                                             
         BAS   RE,AFFCHK           YES-CHECK STATION IN AFFILIATE               
         BNE   LREC0190                                                         
         OC    QTVB,QTVB           TEST TVB FILTER                              
         BZ    *+12                                                             
         BAS   RE,TVBCHK           YES-CHECK STATION IN TVB REGION              
         BNE   LREC0190                                                         
         CLI   QRANK,0             TEST MARKET RANK FILTER                      
         BE    *+12                                                             
         BAS   RE,CHKRNK           YES-CHECK STATION IN MKT RANK                
         BNE   LREC0190                                                         
         OC    QOWNER,QOWNER       TEST OWNER FILTER                            
         BZ    *+12                                                             
         BAS   RE,OWNCHK           YES-CHECK STATION IN OWNERSHIP               
         BNE   LREC0190                                                         
******** OC    QMKT,QMKT           TEST MARKET FILTER                           
******** BZ    *+12                                                             
******** BAS   RE,CHKMKT           YES-CHECK STATION IN MARKET                  
******** BNE   LREC0190                                                         
         OC    QTEAM,QTEAM         TEST TEAM FILTER                             
         BZ    LREC0178            NO                                           
         OC    QOFF,QOFF           YES-OFFICE FILTER MUST BE SET                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CHKTEM           CHECK STATION IN TEAM                        
         BNE   LREC0190            NO-SKIP                                      
         SPACE                                                                  
* GET ALL DATA FROM STATION MASTER                                              
         SPACE                                                                  
LREC0178 DS    0H                                                               
         BAS   RE,GETSTA           CK STA MASTER AVAIL                          
         L     R4,AIO2                                                          
         USING RSTAD,R4                                                         
         MVC   RRGWSAFF,RSTAAFFL                                                
         MVC   RRGWSTVB,RSTATVB                                                 
         MVC   RRGWSRNK,RSTARANK                                                
         MVC   RRGWSOWN,RSTAOWN                                                 
         B     LREC0200                                                         
*                                                                               
LREC0180 DS   0H                                                                
         CLI   0(R2),QLSTY         YES-TEST ON STATION TYPE NOW                 
         BNE   LREC0200                                                         
         CLI   0(R3),C'4'          YES-IGNORE 'ALL' STATION TYPES               
         BNE   LREC0200                                                         
*                                                                               
LREC0190 EX    R5,LREX6            SKIP - FORCE NEXT ROW VALUE                  
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0200 LA    R2,1(R2)            NEXT ROW TYPE                                
         LA    R3,8(R3)                                                         
         LR    R4,R5                                                            
         SH    R5,=H'8'                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,LREC0130                                                      
*                                                                               
LREC0210 ZIC   R1,LSTDISP                                                       
         LA    R1,ROREC(R1)        R1=A(LIST LINE VALUE)                        
         MVC   LSTVAL,0(R1)        SAVE THE VALUE                               
         CLI   LSTTYPE,QLSTY       TEST LISTING STATION TYPES                   
         BNE   LREC0220                                                         
         SPACE                                                                  
         BAS   RE,DSTY             DISPLAY STATION TYPE                         
         SPACE                                                                  
LREC0220 BAS   RE,LRTOTAL          TOTAL ALL THE MONTHS                         
         BNE   LREC0110            NO RECORDS FOUND                             
         SPACE                                                                  
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BE    LREC0230             NO                                          
         SPACE                                                                  
         MVI   FOUNDSET,C'Y'       SET WAS FOUND                                
         SPACE                                                                  
         BAS   RE,CSETL            SEE IF END OF DATA                           
         BE    LREC0110             NO                                          
         SPACE                                                                  
LREC0230 CLI   LISTEND,C'Y'                                                     
         BNE   *+12                                                             
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BNE   LREC0260             DONE                                        
*                                                                               
         SPACE                                                                  
LREC0236 MVI   FIRSTR,C'N'                                                      
         SPACE                                                                  
LREC0250 DS   0H                                                                
         CLI   LISTEND,C'Y'                                                     
         BE    LREC0260             DONE                                        
         MVI   FOUNDSET,C'N'       SET OFF FOUND                                
         SPACE                                                                  
         CLI   SET1CDE,0           DOING SETS                                   
         BE    LREC0110             NO, LIST NEXT RECORD                        
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BE    LREC0100                      SAME, DO READ HIGH                 
         SPACE                                                                  
* AT END OF DATA REQUESTED, CLEAR 'TO' FIELDS, SET END                          
         SPACE                                                                  
LREC0260 DS   0H                                                                
         XC    RRGWDAT1(RRGWPCTB-RRGWDAT1),RRGWDAT1                             
         XC    RRGWSTAD,RRGWSTAD                                                
         XC    RRGWOREG,RRGWOREG                                                
         XC    RRGWYRMO,RRGWYRMO                                                
         XC    RRGWDFLD,RRGWDFLD                                                
         MVI   RRGWSTAT,X'FF'      SET END OF LIST                              
         MVI   RRGWERR,0                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
LREX1    CLC   0(0,R1),BLANKS      EXECUTED INSTRUCTIONS                        
LREX2    CLC   0(0,R3),0(R1)                                                    
LREX3    MVC   0(0,R6),0(R1)                                                    
LREX4    XC    8(0,R6),8(R6)                                                    
LREX5    MVC   0(0,R6),XFF                                                      
LREX6    MVC   8(0,R6),XFF                                                      
         EJECT                                                                  
*                                                                               
*                                                                               
*        ROUTINE TO BUILD TOTAL LINE FOR LISTRECS                               
*        LSTKEYEX = LENGTH FOR KEY COMPARE                                      
*                                                                               
LRTOTAL  ST    RE,SVREGE                                                        
*        ZIC   R4,LSTKEYEX                                                      
         MVC   SVKEY,KEY           SAVE KEY OF FIRST MONTH RECORD               
         SPACE                                                                  
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
         LA    R6,RODPER           PERIOD COLS                                  
*                                                                               
LT010    CLC   ROKDTLYM(1),QSTART  CHECK THE YEAR                               
         BNL   LT020                                                            
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'12'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
*        EX    R4,LTCOMPK                                                       
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LT900                END OF DATA                                 
         B     LT010                                                            
*                                                                               
LT020    CLC   ROKDTLYM(1),QSTART                                               
         BH    LT800               YEAR HIGH - NO DATA                          
         CLI   ROKDTLYM+1,0        CHECK FOR PRIOR MONTH                        
         BNE   LT040                                                            
*                                                                               
LT030    MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,X'13'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
*        EX    R4,LTCOMPK                                                       
         SPACE                                                                  
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LT900                END OF DATA                                 
         B     LT020                                                            
*                                                                               
LT040    CLC   ROKDTLYM,QSTART     CHECK MON AGAINST START MON                  
         BL    LT030               LOW - GET NEXT                               
         CLC   ROKDTLYM,QEND       CHECK MON AGAINS END MON                     
         BH    LT800               HIGH - NO DATA                               
*                                                                               
LT050    L     R7,AIO                                                           
*        MVC   RRGWDT1(3),ROKDTLTY    DATA TYPES                                
*        MVC   RRGWDAT1(24),ROKDTLVL  3 FLDS OF 8 BYTES DATA CONTENT            
         MVC   RRGWRADR(4),AIO1       REC ADDR                                  
         MVC   RRGWYRMO(2),ROKDTLYM                                             
         MVC   RRGWPRBL(8),RODPPBLG                                             
         MVC   RRGWPFIN(4),RODPPFIN                                             
         MVC   RRGWCBUD(4),RODPCBUD                                             
         MVC   RRGWCBK(8),RODWK                                                 
         SPACE                                                                  
         OC    RRGWDFLD,RRGWDFLD   ANY DATA TO SEND?                            
         BZ    LT030                NO, GET NEXT                                
         SPACE                                                                  
         LA    R2,3                                                             
         LA    R3,RRGWDT1                                                       
         LA    R4,RRGWDAT1                                                      
LT070    LA    R0,3                                                             
         LA    RE,ROKDTLTY                                                      
         LA    RF,ROKDTLVL                                                      
         SPACE                                                                  
LT080    CLC   0(1,R3),0(RE)                                                    
         BE    LT084                                                            
         LA    RE,1(,RE)                                                        
         LA    RF,8(,RF)                                                        
         BCT   R0,LT080                                                         
         SPACE                                                                  
         CLI   0(R3),QLGRP                                                      
         BNE   LT082                                                            
         MVC   0(2,R4),QGROUP                                                   
         B     LT094                                                            
         SPACE                                                                  
LT082    CLI   0(R3),QLGRGRP                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R4),QGROUP                                                   
         B     LT094                                                            
         SPACE                                                                  
LT084    CLI   0(R3),QLSTA                                                      
         BNE   LT090                                                            
         MVC   0(4,R4),0(RF)                                                    
         SPACE                                                                  
         LA    R1,5(,RF)                                                        
         CLI   3(R4),C'-'                                                       
         BNE   LT086                                                            
         BCTR  R1,0                                                             
         MVI   3(R4),C' '                                                       
         SPACE                                                                  
LT086    MVC   4(1,R4),0(R1)                                                    
         CLI   4(R4),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(R4),C' '                                                       
         B     LT094                                                            
         SPACE                                                                  
LT090    MVC   0(8,R4),0(RF)                                                    
         SPACE                                                                  
LT094    LA    R3,1(,R3)                                                        
         LA    R4,8(,R4)                                                        
         CLI   0(R3),0                                                          
         BE    LT100                                                            
         BCT   R2,LT070                                                         
LT100    DS   0H                                                                
         MVC   RRGWSAFF,SVAFF                                                   
         MVC   RRGWSOWN,SVOWN                                                   
         MVC   RRGWSTVB,SVTVB                                                   
         MVC   RRGWSRNK,SVRNK                                                   
         B     EXIT                                                             
*                                                                               
LT200    EQU *                                                                  
*        MVC   KEYSAVE,KEY                                                      
*        GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
*        GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'RRGNEW',KEY,AIO1                     
*        SPACE                                                                  
*        L     R1,AIO1                                                          
*        MVC   KEY,0(R1)                                                        
*        SPACE                                                                  
*        CLI   TRCTYPE,X'14'                                                    
*        BE    LT220                                                            
*        MVI   TRCTYPE,X'14'                                                    
*        GOTO1 =A(TRACE),RR=RELO                                                
LT220    EQU *                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0260                                                         
         SPACE                                                                  
         ZIC   R4,LSTKEYEX                                                      
*        EX    R4,LTCOMPK                                                       
         CLC   KEY(30),KEYSAVE     CK FOR CHANGE IN KEY                         
         BNE   LT999                DONE                                        
         CLC   ROKDTLYM,QEND       CHECK REACHED END MONTH                      
         BNH   LT050                                                            
         LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,6                                                        
         GOTO1 =A(TRACE),RR=RELO                                                
         B     LT999                                                            
*                                                                               
LT800    LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'RRGNEW',KEY,AIO1                     
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         MVI   TRCTYPE,7                                                        
         GOTO1 =A(TRACE),RR=RELO                                                
*                                                                               
LT900    LTR   RB,RB               CC NE - NO RECORDS FOUND                     
         B     LTX                                                              
*                                                                               
LT999    CR    RB,RB               CC EQ - RECORDS FOUND                        
*                                                                               
LTX      L     RE,SVREGE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*TCOMPK  CLC   KEY(0),KEYSAVE      ** EXECUTED                                  
         EJECT                                                                  
CHKRGN   LR    R0,RE                                                            
         BAS   RE,SETREPFL         REPFILE VALUES                               
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),0(R3)                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO,DMWORK            
         L     R1,AIO                                                           
         USING ROFFD,R1                                                         
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
         CLC   ROFFREG,QREGION     CHECK THE OFFICE REGION                      
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
* CHANGE STATION TYPE FROM 1/2/3 TO READABLE                                    
         SPACE                                                                  
DSTY     MVC   LSTVAL,BLANKS       YES-CONVERT STATION TYPE CODE                
         MVC   LSTVAL(3),=C'NEW'                                                
         CLI   0(R1),C'2'                                                       
         BER   RE                                                               
         MVC   LSTVAL(3),=C'OLD'                                                
         CLI   0(R1),C'3'                                                       
         BER   RE                                                               
         MVC   LSTVAL(4),=C'COMP'                                               
         CLI   0(R1),C'1'                                                       
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* CK IF KEY CHANGE IS END, OR NEEDS NEXT SET MEMBER *                           
* 3 KEY FIELDS ARE CHECKED FOR NON-SET FIELDS ONLY  *                           
* IF ANY ARE UNEQUAL, END OF LIST (NE RET CODE)     *                           
         SPACE                                                                  
CSETL    NTR1                                                                   
         LA    R2,KEY+3                                                         
         LA    RE,KEY+6                                                         
         LA    RF,KEYSAVE+6                                                     
         SPACE                                                                  
CSETL020 LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
         SPACE                                                                  
CSETL040 CLC   0(1,R2),0(R1)       THIS A SET CODE                              
         BE    EXIT                                                             
         LA    R1,L'SET1CDE(,R1)                                                
         BCT   R0,CSETL040                                                      
         SPACE                                                                  
         OC    0(8,RF),0(RF)       THIS INITIAL KEY                             
         BZ    EXIT                 YES                                         
         SPACE                                                                  
         CLI   0(R2),QLSTA         THIS STATION FIELD                           
         BNE   CSETL50              NO                                          
         CLI   NCOMBOS,0           THIS COMBO STATION                           
         BE    CSETL50              NO                                          
         CR    RB,RB                                                            
         B     EXIT                                                             
         SPACE                                                                  
CSETL50  CLC   0(8,RE),0(RF)       CHANGE IN NON-SET KEY                        
         BNE   EXIT                 YES                                         
         LA    RE,8(,RE)                                                        
         LA    RF,8(,RF)                                                        
         LA    R2,1(,R2)                                                        
         B     CSETL020                                                         
         EJECT                                                                  
* FILL IN INITIAL KEY FROM SET TABLE                                            
         SPACE                                                                  
SETK     NTR1                                                                   
         LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AH    RF,=AL2(SET1TAB-SYSD)                                            
SETK20   CLC   0(1,R4),0(RE)       THIS THE CODE                                
         BE    SETK40                                                           
         LA    RE,L'SET1CDE(,RE)   NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,SETK20                                                        
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
SETK40   ZIC   R1,1(RE)            GET CODE LEN                                 
         BCTR  R1,0                                                             
         EX    R1,SETKMVC                                                       
         CLI   0(R4),QLGRP         THIS G/S                                     
         BNE   SETK60                                                           
         CLI   1(R5),C' '          THIS A BLANK                                 
         BH    SETKX                                                            
         MVI   1(R5),0                                                          
         MVI   0(R4),QLGRGRP                                                    
         B     SETKX                                                            
         SPACE                                                                  
SETK60   CLI   0(R4),QLSTA         THIS STATION                                 
         BNE   SETK70                                                           
         LA    R1,3(,R5)                                                        
         CLI   3(R5),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(RF)                                                    
         CLI   1(R1),C' '                                                       
         BH    SETK64                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     SETKX                                                            
SETK64   MVI   2(R1),C'M'                                                       
         B     SETKX                                                            
         SPACE                                                                  
SETK70   CLI   0(R4),QLCON         THIS CONTRACT TYPE                           
         BNE   SETK80                                                           
         OI    1(R5),X'40'         SET ON SPACE IN SECOND BYTE                  
         B     SETKX                                                            
         SPACE                                                                  
SETK80   TM    DISPFLAG,OPCONDCT   LOOKING FOR HARD CODE CON/DCT                
         BZ    SETKX                NO                                          
         CLI   0(R2),QLDCT         THIS DEV CON TYP                             
         BNE   SETKX                                                            
         XC    0(8,R5),0(R5)                                                    
         SPACE                                                                  
SETKX    OI    0(R4),SET           SET UP FOR TABLE LOOK-UP                     
         B     EXIT                                                             
         SPACE                                                                  
SETKMVC  MVC   0(0,R5),0(RF)                                                    
         EJECT                                                                  
* CHECK FILTERS BY READING THE STATION REC & COMPARING TO IT *                  
         SPACE                                                                  
CHKGRP   NTR1  ,                                                                
         BAS   RE,GETSTA                                                        
         L     R4,AIO2                                                          
         USING RSTAD,R4                                                         
         CLI   QGROUP,C'*'                                                      
         BE    CHKGRP20                                                         
         SPACE                                                                  
         CLI   QGROUP+1,C' '                                                    
         BNH   CHKGRP10                                                         
         SPACE                                                                  
         CLC   RSTAGRUP,QGROUP     CHECK THE STATION GROUP                      
         B     EXIT                                                             
         SPACE                                                                  
CHKGRP10 CLC   RSTAGRUP(1),QGROUP  CHECK THE STATION GROUP                      
         B     EXIT                                                             
         SPACE                                                                  
CHKGRP20 LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AH    RF,=AL2(SET1TAB-SYSD)                                            
CHKGRP30 CLI   0(RE),QLGRP       THIS THE CODE                                  
         BE    CHKGRP34                                                         
         LA    RE,L'SET1CDE(,RE)    NEXT CODE                                   
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,CHKGRP30                                                      
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
CHKGRP34 ZIC   R1,1(RE)            GET THE LENGTH                               
         SPACE                                                                  
CHKGRP36 CLC   RSTAGRUP,0(RF)      CHECK THE STATION GROUP                      
         BE    EXIT                                                             
         LA    RF,0(R1,RF)                                                      
         OC    0(2,RF),0(RF)       ANOTHER ENTRY                                
         BNZ   CHKGRP36                                                         
         LTR   RB,RB               SET COND CODE                                
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE                                                                  
AFFCHK   NTR1                                                                   
         MVI   DUB+2,1             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
TVBCHK   NTR1  ,                                                                
         MVI   DUB+2,2             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
OWNCHK   NTR1                                                                   
         MVI   DUB+2,3             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
MKTCHK   NTR1  ,                                                                
         BAS   RE,GETSTA                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,8            LOOK FOR EXTRA DESCRIPTION ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING RSTAXXEL,R6                                                      
         CLC   RSTAMKTC,QMKT       CHECK THE STATION'S MARKET                   
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 1                                                                
CHKRNK   NTR1  ,                                                                
         MVI   DUB+2,4             SET COMPARISON FLAG                          
ALLCHK   GOTO1 =A(GESTATS),RR=RELO                                              
         B     EXIT                                                             
         SPACE 1                                                                
CHKTEM   NTR1  ,                                                                
         BAS   RE,GETSTA                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,4            LOOK FOR OFFICE/TEAM ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING RSTAOTEL,R6                                                      
*                                                                               
CT010    CLC   RSTAOTOF,QOFF       CHECK THE OFFICE                             
         BE    CT020                                                            
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         B     CT010                                                            
*                                                                               
CT020    CLC   RSTAOTTM,QTEAM      CHECK THE TEAM                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
GETSTA   LR    R0,RE               GET STATION RECORD                           
         BAS   RE,SETSTA                                                        
         L     R1,AIO2                                                          
         CLI   0(R1),2             TEST RECORD ALREADY IN CORE                  
         BNE   *+14                                                             
         CLC   STATION,22(R1)                                                   
         BE    GETSTAX             YES                                          
         BAS   RE,SETREPFL         REPFILE VALUES                               
         MVI   BYTE,0                                                           
*                                                                               
GETSTA1  XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RST2KEY,R4                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   RST2KSTA,STATION                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         SPACE                                                                  
GETSTA2  CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
         BE    GETSTA4                                                          
         CLI   BYTE,0              TEST SECOND SEARCH                           
         BNE   *+14                                                             
         CLC   AGENCY,=C'SJ'       OR AGENCY IS SJR (SJR MUST GET               
         BNE   *+12                                  SJR RECORDS)               
*                                                                               
         MVI   RRGWERR,RRGWER0E    STATION MASTER NOT ON FILE                   
         B     ERREXIT                                                          
*                                                                               
         MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
         B     GETSTA1                FIRST POINTER                             
*                                                                               
GETSTA4  CLC   RST2KREP,AGENCY     AGENCIES MATCH - FINE                        
         BE    GETSTA6                                                          
         CLI   BYTE,0              TEST FIRST SEARCH                            
         BE    *+14                YES-TRY TO FIND AGENCY MATCH                 
         CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
         BNE   GETSTA6                                                          
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
         SPACE                                                                  
         L     R1,AIO1                                                          
         MVC   KEY,0(R1)                                                        
         SPACE                                                                  
         B     GETSTA2                                                          
         DROP  R4                                                               
*                                                                               
GETSTA6  DS   0H                                                                
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO2,DMWORK           
         SPACE                                                                  
         L     R6,AIO2                                                          
         USING RSTAREC,R6                                                       
         MVC   SVAFF,RSTAAFFL                                                   
         MVC   SVTVB,RSTATVB                                                    
         MVC   SVOWN,RSTAOWN                                                    
         MVC   SVGRP,RSTAGRUP                                                   
         MVC   SVRNK,RSTARANK                                                   
         SPACE                                                                  
         BAS   RE,RSETRRGO                                                      
*                                                                               
GETSTAX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R6                                                               
*                                                                               
SETSTA   MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BE    *+10                                                             
         MVC   STATION+4(1),0(RF)                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CALCULATE A PERCENTAGE                                      
*        REG 4 AS A PERCENT OF R3 - RESULT IN R5                                
*        CONDITION CODE SET TO NOT EQUAL IF PCT IS GT 999                       
*                                                                               
PCTCOMP  DS    0H                                                               
         SR    R5,R5                                                            
         LTR   R3,R3                                                            
         BZR   RE                                                               
         ST    R4,DUB              SAVE R4                                      
         CVD   R3,PARAS                                                         
         MP    PARAS(8),=P'10'                                                  
         CVD   R4,PARAS+8                                                       
         CP    PARAS(8),PARAS+8(8)                                              
         BNH   PC010               PCT GT 999                                   
         LA    R5,2000                                                          
         MR    R4,R4                                                            
         DR    R4,R3                                                            
         LTR   R5,R5                                                            
         BM    *+8                                                              
         A     R5,=F'1'                                                         
         SRA   R5,1                                                             
         L     R4,DUB              RESTORE R4                                   
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
PC010    LTR   RB,RB               CC NE                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO SET UP REP FILE VALUES                                      
*                                                                               
SETREPFL DS    0H                                                               
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVC   LKEY,=H'27'                                                      
         MVI   USEIO,C'N'                                                       
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*        ROUTINE TO RESET RRGON FILE VALUES                                     
*                                                                               
RSETRRGO DS    0H                                                               
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
         EJECT                                                                  
ROUND    DS    0H                                                               
*                                                                               
*        ROUTINE TO ROUND UNITS TO THOUSANDS                                    
*        INPUT:  R5 = UNITS                                                     
*        OUTPUT: R5 = ROUNDED TO THOUSANDS                                      
*                CC = 0 IF ROUNDED IS ZERO                                      
*                                                                               
         LR    R0,R5                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'1000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         LTR   R5,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
* PRINT RRGW DSECT IF OFFLINE                                                   
         SPACE                                                                  
PRTRRGW  NTR1                                                                   
         SPACE                                                                  
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BE    PRTWX                                                            
         SPACE                                                                  
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         SPACE                                                                  
PRTW010  LA    R0,RRGWRILE(,RA)                                                 
         LA    R2,RRGWRI                                                        
         SPACE                                                                  
PRTW020  LA    R3,P                                                             
         LA    R4,P+80                                                          
         SPACE                                                                  
         LA    R5,2                                                             
PRTW030  LA    R6,4                                                             
         SPACE                                                                  
         MVC   0(16,R4),0(R2)                                                   
         SPACE                                                                  
PRTW040  GOTO1 RRGWHEXO,DMCB,(R2),(R3),4                                        
         LA    R2,4(,R2)                                                        
         LA    R3,9(,R3)                                                        
         BCT   R6,PRTW040                                                       
         LA    R3,1(,R3)                                                        
         LA    R4,17(,R4)                                                       
         BCT   R5,PRTW030                                                       
         SPACE                                                                  
         GOTO1 RRGWPRNT,DMCB,P-1,=C'BL01'  PRINT LINE                           
         SPACE                                                                  
         CR    R0,R2                                                            
         BH    PRTW020                                                          
PRTWX    DS    0H                                                               
         XIT1                                                                   
         DROP  R7                                                               
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
XFF      DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
*                                                                               
BLANKS   DC    CL80' '                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
* TABLE OF REQUEST FILTERS                                                      
*                                                                               
*        DEFINITION                                                             
*   BYTE 0 - REQUEST TYPE - CHECKED AGAINST FIRST 3 BYTES OF KEY                
*        1 - LENGTH OF FILTER FIELD                                             
*        2 - DISPLACEMENT ADDRESS OF FILTER FIELD                               
*                                                                               
         DS    0F                                                               
FILTAB   DS    0XL4                                                             
         DC    AL1(QLSTA),AL1(L'QSTA),AL2(QSTA-SYSD)                            
         DC    AL1(QLREG),AL1(L'QREGION),AL2(QREGION-SYSD)                      
         DC    AL1(QLOFF),AL1(L'QOFF),AL2(QOFF-SYSD)                            
         DC    AL1(QLTEM),AL1(L'QTEAM),AL2(QTEAM-SYSD)                          
         DC    AL1(QLGRP),AL1(L'QGROUP),AL2(QGROUP-SYSD)                        
         DC    AL1(QLADV),AL1(L'QADV),AL2(QADV-SYSD)                            
         DC    AL1(QLAGY),AL1(L'QAGY),AL2(QAGY-SYSD)                            
         DC    AL1(QLAFF),AL1(L'QAFF),AL2(QAFF-SYSD)                            
         DC    AL1(QLCLS),AL1(L'QCLASS),AL2(QCLASS-SYSD)                        
         DC    AL1(QLCAT),AL1(L'QCTGY),AL2(QCTGY-SYSD)                          
         DC    AL1(QLSTY),AL1(L'QSTATY),AL2(QSTATY-SYSD)                        
         DC    AL1(QLTVB),AL1(L'QTVB),AL2(QTVB-SYSD)                            
         DC    AL1(QLOWN),AL1(L'QOWNER),AL2(QOWNER-SYSD)                        
         DC    AL1(QLGRGRP),AL1(1),AL2(QGROUP-SYSD)                             
         DC    AL1(QLRNK),AL1(L'QRANK),AL2(QRANK-SYSD)                          
         DC    AL1(QLCON),AL1(L'QCONTY),AL2(QCONTY-SYSD)                        
         DC    AL1(QLMKT),AL1(L'QMKT),AL2(QMKT-SYSD)                            
         DC    AL1(QLDCT),AL1(L'QDCT),AL2(QDCT-SYSD)                            
         DC    AL1(0)                                                           
         EJECT                                                                  
       ++INCLUDE RENRGLTBL                                                      
         EJECT                                                                  
* ROUTINE TO CHECK IF -CM COULD BE COMBO STATION                                
* IF SO, BUILD LIST OF COMBINED STATIONS                                        
*                                                                               
         SPACE                                                                  
         DS    0F                                                               
GETCOMBO NMOD1 0,**GTCO**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         LA    R1,COMBREPS         CHECK THE REP                                
*                                                                               
GETCOMB2 CLI   0(R1),0                                                          
         BE    GETCOMBX                                                         
         CLC   AGENCY,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,2(R1)                                                         
         B     GETCOMB2                                                         
         MVC   KEY,SVSTAKEY        READ STATION RECORD                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO,DMWORK            
         L     R4,AIO                                                           
         USING RSTAD,R4                                                         
         LA    R3,RSTAELEM         LOOK FOR COMBINED STATION ELEMENTS           
         SR    RE,RE                                                            
         LA    R1,COMBOSTA                                                      
         SR    R5,R5                                                            
         XC    COMBOSTA,COMBOSTA                                                
*                                                                               
GETCOMB4 CLI   0(R3),0                                                          
         BE    GETCOMB8                                                         
         CLI   0(R3),X'0A'                                                      
         BNE   GETCOMB6                                                         
         USING RSTACSEL,R3                                                      
         CLI   RSTACPRF,C'-'       STATION MINUS'D OUT?                         
         BE    GETCOMB6            YES - SKIP IT                                
         OC    COMBOSTA,COMBOSTA   FIRST STATION IS REQUESTED STATION           
         BNZ   *+18                                                             
         MVC   COMBOSTA,QSTA                                                    
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
         MVC   0(4,R1),RSTACS      FOUND-SAVE THE STATION                       
         LA    RF,4(R1)                                                         
         CLI   3(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         MVI   6(R1),0                                                          
         MVC   0(3,RF),=C'-TV'                                                  
         CLI   RSTACS+4,C'T'                                                    
         BE    *+14                                                             
         MVC   1(1,RF),RSTACS+4                                                 
         MVI   2(RF),C'M'                                                       
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
GETCOMB6 IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     GETCOMB4                                                         
*                                                                               
GETCOMB8 STC   R5,NCOMBOS          SAVE N'COMBO STATIONS                        
         CLI   NCOMBOS,CSTAMAX                                                  
         BNH   *+6                                                              
         DC    H'0'                BOMB IF TOO MANY                             
         SPACE                                                                  
* GET QSORT ADDRESS                                                             
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A50'  QSORT IS T00A50                          
         GOTO1 RRGWCALL,DMCB                                                    
         ICM   RF,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,COMBOSTA,(R5),7,7,0                                    
*                                                                               
GETCOMBX XIT1                                                                   
         DROP  R3,R4,RB,RC                                                      
         SPACE                                                                  
COMBREPS DC    CL2'IR'             TABLE OF REPS THAT COULD HAVE                
         DC    CL2'TO'             COMBO STATIONS                               
         DC    CL2'D4'             NEW DARNY                                    
         DC    CL2'I1'                                                          
         DC    CL2'HN'                                                          
         DC    CL2'MG'                                                          
         DC    CL2'DI'                                                          
         DC    CL2'GP'                                                          
         DC    CL2'I2'                                                          
         DC    CL2'I8'                                                          
         DC    CL2'I9'                                                          
         DC    CL2'CN'                                                          
         DC    CL2'CM'                                                          
         DC    CL2'IF'             NEW INFINITY                                 
         DC    CL2'AQ'             NEW ALLIED RADIO PARTNERS                    
         DC    CL2'S1'             NEW SHAMROCK                                 
         DC    CL2'CM'             NEW CONCERT MUSIC                            
         DC    CL2'SJ'                                                          
* KATZ REPS ADDED                                                               
         DC    CL2'BF'                                                          
         DC    CL2'CR'                                                          
         DC    CL2'EA'                                                          
         DC    CL2'KF'                                                          
         DC    CL2'KU'                                                          
         DC    CL2'K4'                                                          
         DC    CL2'RS'                                                          
         DC    CL2'S3'                                                          
*                                                                               
         DC    X'00'                                                            
         LTORG                                                                  
**                                                                              
*   NEED TO SAVE VALUE IN FIRST PARAM AS POINTER TO COMPARE                     
*                                                                               
GESTATS  NMOD1 0,**GEST**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
*                                  ONLY CLEAR 1ST 2 CHARS OF DUB                
*                                     FLAG IS IN 3RD POSITION                   
         XC    DUB(2),DUB          SET DUB = ZERO:  EQUAL                       
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVC   LKEY,=H'27'                                                      
         MVI   USEIO,C'N'                                                       
         MVI   BYTE,0                                                           
*                                                                               
         SPACE                                                                  
         MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BE    *+10                                                             
         MVC   STATION+4(1),0(RF)                                               
         SPACE                                                                  
         L     R1,AIO2                                                          
         CLI   0(R1),2             TEST RECORD ALREADY IN CORE                  
         BNE   GEST0040                                                         
         CLC   STATION,22(R1)                                                   
         BE    GEST0160            YES                                          
*                                                                               
*   IF ALREADY IN CORE, BRANCH TO COMPARISON ROUTINE RATHER                     
*      THAN EXIT....                                                            
*                                                                               
GEST0040 DS   0H                                                                
         XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RST2KEY,R4                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   RST2KSTA,STATION                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 RRGWDMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                  
*                                                                               
GEST0060 CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
         BE    GEST0120                                                         
         CLI   BYTE,0              TEST SECOND SEARCH                           
         BNE   GEST0080                                                         
         CLC   AGENCY,=C'SJ'       OR AGENCY IS SJR (SJR MUST GET               
         BNE   GEST0100                              SJR RECORDS)               
*                                                                               
GEST0080 DS   0H                                                                
         MVI   RRGWERR,RRGWER0E    STATION MASTER NOT ON FILE                   
         MVI   RRGWSTAT,01         SET ERROR                                    
         L     RD,BASERD                                                        
         XIT1                                                                   
*                                                                               
GEST0100 DS   0H                                                                
         MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
         B     GEST0040               FIRST POINTER                             
*                                                                               
GEST0120 DS   0H                                                                
         CLC   RST2KREP,AGENCY     AGENCIES MATCH - FINE                        
         BE    GEST0150                                                         
         CLI   BYTE,0              TEST FIRST SEARCH                            
         BE    GEST0140            YES-TRY TO FIND AGENCY MATCH                 
         CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
         BNE   GEST0150                                                         
GEST0140 DS   0H                                                                
         GOTO1 RRGWDMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO2,DMWORK           
         SPACE                                                                  
         B     GEST0060                                                         
*                                                                               
GEST0150 DS   0H                                                                
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
*                                                                               
         DROP  R4                                                               
*                                                                               
GEST0160 L     R5,AIO2                                                          
         USING RSTAD,R5                                                         
*                                                                               
**  DO COMPARISON BASED ON VALUE PASSED IN...                                   
*                                                                               
         CLI   DUB+2,1             COMPARE AFFILIATE?                           
         BNE   GEST0162            NO                                           
         CLC   RSTAAFFL,QAFF       CHECK THE STATION'S AFFILIATE                
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0162 DS   0H                                                                
         CLI   DUB+2,2             COMPARE TVB REGION?                          
         BNE   GEST0164            NO                                           
         CLC   RSTATVB,QTVB        CHECK THE STATION'S TVB REGION               
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0164 DS   0H                                                                
         CLI   DUB+2,3             COMPARE OWNER?                               
         BNE   GEST0166            NO                                           
         CLC   RSTAOWN,QOWNER      SAME OWNER?                                  
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
         SPACE                                                                  
*                                  MUST BE RANK COMPARE!!                       
GEST0166 DS   0H                                                                
         CLC   RSTARANK,QRANK      CHECK THE STATION'S MARKET RANK              
         BE    GEST0200            YES -                                        
         DROP  R5                                                               
GEST0172 DS   0H                                                                
         CLC   =C'IR',AGENCY       CROSS-COMPANY AGENCY?                        
         BNE   GEST0180            NO  - EXIT                                   
         GOTO1 RRGWDMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEYSAVE              
*                                  GET NEXT RECORD IN LINE                      
         CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
*                                  SAME STATION FOUND?                          
         BNE   GEST0180            NO  - TREAT AS NOT FOUND                     
         OC    KEY+RST2KEND-RST2KEY(3),KEY+RST2KEND-RST2KEY                     
*                                  IS THERE A LEAVE DATE?                       
         BZ    GEST0150            NO  - GO BACK AND COMPARE                    
*                                  YES - TREAT AS 'NOT FOUND'                   
GEST0180 DS   0H                                                                
         MVI   DUB,1               SET DUB NOT = ZERO:  NOT EQUAL               
GEST0200 DS   0H                                                                
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         CLC   DUB(1),DUB+1        SET FINAL CONDITION CODE                     
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* FILTER ON SETS HERE *                                                         
         SPACE                                                                  
FSET     NMOD1 0,**FSET**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         LA    R3,SET1CDE                                                       
         LR    R4,R9                                                            
         AH    R4,=AL2(SET1TAB-SYSD)                                            
         LA    R0,3                                                             
FSET10   CLC   0(1,R2),0(R3)       LOOK FOR SET CODE                            
         BE    FSET20                                                           
         LA    R3,L'SET1CDE(,R3)                                                
         LA    R4,L'SET1TAB(,R4)                                                
         BCT   R0,FSET10                                                        
         DC    H'0'                                                             
FSET20   ZIC   R5,1(R3)            GET LENGTH OF ENTRY                          
         BCTR  R5,0                                                             
         CLI   0(R4),0             END OF TABLE                                 
         BNE   FSET30                                                           
         DC    H'0'                EMPTY TABLE - NO WAY                         
         SPACE                                                                  
FSET30   CLI   0(R4),0             END OF TABLE                                 
         BE    FSET90                                                           
         SPACE                                                                  
         CLI   0(R3),QLSTA         STATION COMPARE DIFFERENT                    
         BNE   FSET50                                                           
         CLC   0(3,R6),0(R4)       1ST 3 OF STATION                             
         BL    FSET38                                                           
         BH    FSET56                                                           
         SPACE                                                                  
         MVC   ACTUAL,3(R6)                                                     
         LA    R1,4(,R6)                                                        
         CLI   3(R6),C'-'                                                       
         BNE   FSET32                                                           
         BCTR  R1,0                                                             
         MVI   ACTUAL,C' '                                                      
         SPACE                                                                  
FSET32   CLC   ACTUAL,3(R4)                                                     
         BL    FSET38                                                           
         BH    FSET56                                                           
         SPACE                                                                  
         MVC   ACTUAL,1(R1)                                                     
         CLI   ACTUAL,C'T'                                                      
         BNE   *+8                                                              
         MVI   ACTUAL,C' '                                                      
         CLC   ACTUAL,4(R4)        TEST BAND                                    
         BL    FSET38                                                           
         BH    FSET56                                                           
         B     FSETEQ                                                           
         SPACE                                                                  
FSET38   MVC   0(4,R6),0(R4)                                                    
         LA    R1,3(,R6)                                                        
         CLI   3(R4),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         SPACE                                                                  
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R4)                                                    
         CLI   1(R1),C' '                                                       
         BH    FSET39                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     FSET64                                                           
FSET39   MVI   2(R1),C'M'                                                       
         B     FSET64                                                           
         SPACE                                                                  
FSET50   EX    R5,FSETCLC          SEE IF A MATCH                               
         SPACE                                                                  
FSET54   BE    FSETEQ                                                           
         BL    FSET60                                                           
         SPACE                                                                  
* NOW LOOK AT NEXT ENTRY IN TABLE                                               
         SPACE                                                                  
FSET56   LA    R4,1(R4,R5)                                                      
         B     FSET30                                                           
         SPACE                                                                  
FSET60   EX    R5,FSETMVC                                                       
         SPACE                                                                  
* SET REST OF KEY TO 1ST OF TABLE FOR SETS, OR FILTER VALUE                     
         SPACE                                                                  
FSET64   LA    R2,1(,R2)                                                        
         LA    R6,8(,R6)                                                        
         C     R2,SETKEYAD                                                      
         BH    FSETNE                                                           
         SPACE                                                                  
         CLI   0(R2),0                                                          
         BE    FSETNE                                                           
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
FSET70   CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    FSET74                                                           
         LA    RF,L'FILTAB(RF)                                                  
         B     FSET70                                                           
         SPACE                                                                  
FSET74   LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,FSET1            TEST FILTER IS SET (COMP TO SPACES)          
         BH    *+14                 YES                                         
         XC    0(8,R6),0(R6)                                                    
         B     FSET64                                                           
         SPACE                                                                  
         CLI   0(R1),C'*'          IS THIS A SETS REQUEST?                      
         BNE   FSET78               NO                                          
         SPACE                                                                  
         BAS   RE,SETM             SET TO FIRST OF SET                          
         B     FSET64                                                           
         SPACE                                                                  
FSET78   EX    RE,FSET3            MOVE IN FILTER VALUE                         
         B     FSET64                                                           
         SPACE                                                                  
FSET90   MVI   0(R6),X'FF'                                                      
         SPACE                                                                  
FSETNE   XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         LTR   RB,RB                                                            
         B     FSETX                                                            
         SPACE                                                                  
FSETEQ   CR    RB,RB                                                            
         SPACE                                                                  
FSETX    XIT1                                                                   
         SPACE                                                                  
FSETCLC  CLC   0(0,R6),0(R4)                                                    
FSETMVC  MVC   0(0,R6),0(R4)                                                    
FSET1    CLC   0(0,R1),BLANKS      SEE IF FILTER IS SET                         
FSET3    MVC   0(0,R6),0(R1)                                                    
         EJECT                                                                  
* FILL IN INITIAL KEY FROM SET TABLE                                            
         SPACE                                                                  
SETM     NTR1                                                                   
         LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AH    RF,=AL2(SET1TAB-SYSD)                                            
SETM20   CLC   0(1,R2),0(RE)       THIS THE CODE                                
         BE    SETM40                                                           
         LA    RE,L'SET1CDE(,RE)    NEXT CODE                                   
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,SETM20                                                        
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
SETM40   ZIC   R1,1(RE)            GET CODE LEN                                 
         BCTR  R1,0                                                             
         EX    R1,SETMMVC                                                       
         SPACE                                                                  
         CLI   0(R2),QLSTA         THIS STATION                                 
         BNE   SETM50                                                           
         LA    R1,3(,R6)                                                        
         CLI   3(R6),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(RF)                                                    
         CLI   1(R1),C' '                                                       
         BH    SETM44                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     SETMX                                                            
SETM44   MVI   2(R1),C'M'                                                       
         B     SETMX                                                            
         SPACE                                                                  
SETM50   CLI   0(R2),QLCON         THIS CONTRACT TYPE                           
         BNE   SETMX                                                            
         OI    1(R6),X'40'                                                      
         SPACE                                                                  
SETMX    XIT1                                                                   
         SPACE                                                                  
SETMMVC  MVC   0(0,R6),0(RF)                                                    
         EJECT                                                                  
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* FIND PREVIOUS SET, AND SET TO NEXT VALUE IN TABLE *                           
* IF AT END OF TABLE, SET 1ST BYTE TO FF            *                           
         SPACE                                                                  
PSET     NMOD1 0,**PSET**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         MVC   KEY,KEYSAVE         SET TO FILTER                                
         SPACE                                                                  
         LR    R4,R2               SAVE ADDR OF 'CURR' DATA TYPE                
         BCTR  R4,0                GET PREV                                     
         SPACE                                                                  
PSET10   ST    R4,FULL             NEXT TRY                                     
         LA    R0,3                                                             
         LA    R2,KEY+ROKDTLTY-ROKEY                                            
         LA    R3,KEY+ROKDTLVL-ROKEY                                            
PSET20   CLC   0(1,R4),0(R2)       THIS THE CODE                                
         BE    PSET24                                                           
         LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R0,PSET20                                                        
         SPACE                                                                  
PSET22   L     R4,FULL             GET THIS                                     
         BCTR  R4,0                GO BACK                                      
         LA    RF,KEY+3                                                         
         CR    R4,RF               GOING TOO FAR?                               
         BL    PSETNE               YES, ALL DONE                               
         B     PSET10                                                           
         SPACE                                                                  
PSET24   MVC   LASTSET,0(R4)                                                    
         STM   R2,R3,DUB                                                        
         SPACE                                                                  
         LA    RF,3                                                             
         LA    R4,SET1CDE                                                       
         LR    R5,R9                                                            
         AH    R5,=AL2(SET1TAB-SYSD)                                            
PSET26   CLC   LASTSET,0(R4)          THIS THE CODE                             
         BE    PSET30                                                           
         LA    R4,L'SET1CDE(,R4)      NEXT CODE                                 
         LA    R5,L'SET1TAB(,R5)      NEXT TABLE                                
         BCT   RF,PSET26                                                        
         B     PSET22                 NOT THERE GO BACK AGAIN                   
         SPACE                                                                  
PSET30   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
PSET40   CLI   LASTSET,QLSTA       COMPARE IS DIFFERENT                         
         BNE   PSET50                                                           
         CLC   0(3,R5),0(R3)       1ST 3 OF STATION                             
         BNE   PSET52                                                           
         SPACE                                                                  
         MVC   ACTUAL,3(R3)                                                     
         LA    RE,4(,R3)                                                        
         CLI   3(R3),C'-'                                                       
         BNE   PSET44                                                           
         BCTR  RE,0                                                             
         MVI   ACTUAL,C' '                                                      
         SPACE                                                                  
PSET44   CLC   ACTUAL,3(R5)                                                     
         BNE   PSET52                                                           
         SPACE                                                                  
         MVC   ACTUAL,1(RE)                                                     
         CLI   ACTUAL,C'T'                                                      
         BNE   *+8                                                              
         MVI   ACTUAL,C' '                                                      
         CLC   ACTUAL,4(R5)        TEST BAND                                    
         BE    PSET54                                                           
         B     PSET52                                                           
         SPACE                                                                  
PSET50   EX    R1,PSETCLC                                                       
         BE    PSET54                                                           
PSET52   LA    R5,1(R1,R5)                                                      
         CLI   0(R5),0             SEE IF END OF TABLE                          
         BNE   PSET40               NOT YET                                     
         B     PSET22              LOOK FOR PREVIOUS SET                        
         SPACE                                                                  
PSET54   LA    R5,1(R1,R5)                                                      
         CLI   0(R5),0             END OF TABLE                                 
         BE    PSET22               SEE IF PREVIOUS SET TO THIS                 
         SPACE                                                                  
         EX    R1,PSETMVC                                                       
         SPACE                                                                  
         CLI   LASTSET,QLSTA                                                    
         BNE   PSET70                                                           
         LA    RE,3(,R3)                                                        
         CLI   3(R3),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    RE,1(,RE)                                                        
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),4(R5)                                                    
         CLI   1(RE),C' '                                                       
         BH    PSET64                                                           
         MVC   1(2,RE),=C'TV'                                                   
         B     PSETEQ                                                           
PSET64   MVI   2(RE),C'M'                                                       
         B     PSETEQ                                                           
         SPACE                                                                  
PSET70   CLI   LASTSET,QLCON       CONTRACT TYPE                                
         BNE   PSETEQ                                                           
         OI    1(R3),X'40'                                                      
         SPACE                                                                  
PSETEQ   CR    RB,RB                                                            
         B     PSETX                                                            
PSETNE   LTR   RB,RB                                                            
PSETX    XIT1                                                                   
PSETCLC  CLC   0(0,R3),0(R5)                                                    
PSETMVC  MVC   0(0,R3),0(R5)                                                    
         DROP  RB,RC                                                            
         EJECT                                                                  
* TRACE I/O                                                                     
         SPACE                                                                  
TRACE    DS   0H                                                                
         CLI   RRGWCNTL,C'V'       VALIDATE ONLY?                               
         BR    RE                                                               
TRAC     NMOD1 0,**TRAC**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         TM    DISPFLAG,OPTRACE                                                 
         BZ    TRACEX                                                           
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVI   P+1,C'S'                                                         
         CLI   TRCTYPE,16                                                       
         BH    *+8                                                              
         MVI   P+1,C'H'                                                         
         SPACE                                                                  
         LA    R0,TRCTABCT                                                      
         LA    R1,TRCTABL                                                       
TRC010   CLC   TRCTYPE,0(R1)                                                    
         BE    TRC020                                                           
         LA    R1,11(,R1)                                                       
         BCT   R0,TRC010                                                        
         DC    H'0'                                                             
         SPACE                                                                  
TRC020   MVC   P+3(2),1(R1)                                                     
         MVC   P+6(8),3(R1)                                                     
         SPACE                                                                  
         MVC   P+15(3),=C'KEYSAVE'                                              
         MVC   P+23(3),KEY                                                      
         GOTO1 RRGWHEXO,DMCB,KEY+3,P+27,3,0,0                                   
         MVC   P+34(24),KEY+6                                                   
         GOTO1 RRGWHEXO,DMCB,KEY+30,P+59,2,0,0                                  
         SPACE                                                                  
         L     R4,AIO                                                           
         ICM   R0,15,RODPPBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+64),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         ICM   R0,15,RODPCBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+75),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         ICM   R0,15,RODPPFIN-ROKEY(R4)                                         
         EDIT  (R0),(10,P+86),0,COMMAS=YES,MINUS=YES                            
         SPACE                                                                  
         GOTO1 RRGWPRNT,DMCB,P-1,=C'BL01'  PRINT LINE                           
         SPACE                                                                  
         MVC   P+15(7),=C'KEYSAVE'                                              
         MVC   P+23(3),KEYSAVE                                                  
         GOTO1 RRGWHEXO,DMCB,KEY+3,P+27,3,0,0                                   
         MVC   P+34(24),KEYSAVE+6                                               
         GOTO1 (RF),(R1),KEYSAVE+30,P+59,2,0,0                                  
         GOTO1 RRGWPRNT,(R1),P-1,=C'BL01'  PRINT LINE                           
TRACEX   XIT1                                                                   
         SPACE                                                                  
TRCTABL  DC    X'01',CL10'01INIT0500'                                           
         DC    X'02',CL10'02INIT0530'                                           
         DC    X'03',CL10'03LREC CON'                                           
         DC    X'04',CL10'04LREC0100'                                           
         DC    X'05',CL10'05AFT FSET'                                           
         DC    X'06',CL10'06LT200'                                              
         DC    X'07',CL10'07LT800'                                              
         DC    X'08',CL10'08LMON0030'                                           
         DC    X'09',CL10'09CSETM300'                                           
         DC    X'0A',CL10'0ADR020'                                              
         DC    X'0B',CL10'0BCSETD240'                                           
         DC    X'0C',CL10'0CLMON0254'                                           
         DC    X'11',CL10'11LREC0172'                                           
         DC    X'12',CL10'12LT010'                                              
         DC    X'13',CL10'13LT030'                                              
         DC    X'14',CL10'14LT050'                                              
         DC    X'15',CL10'15LMON0046'                                           
         DC    X'16',CL10'16LMON0160'                                           
         DC    X'17',CL10'17LMON0210'                                           
         DC    X'18',CL10'18XPLAN'                                              
         DC    X'19',CL10'19XPLAN'                                              
         DC    X'1A',CL10'1AXPLAN'                                              
         DC    X'1B',CL10'1BXPLAN'                                              
         DC    X'1C',CL10'1CXPLAN'                                              
TRCTABCT EQU   (*-TRCTABL)/9                                                    
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* DSECT FOR LCOMBOS - LIST OF VALID FILTERS & KEY COMBINATIONS TABLE            
         SPACE                                                                  
LCOMBOSD DSECT                                                                  
LFTRFLDS DS    XL3                                                              
LSTTYP   DS    XL1                                                              
LFLAG    DS    XL1                                                              
LALTKEY  DS    XL3                                                              
LCNEXT   EQU   *                                                                
         SPACE 3                                                                
       ++INCLUDE RENRGWRI                                                       
         SPACE 3                                                                
RORECD   DSECT                                                                  
*                                                                               
       ++INCLUDE REGENRRGOC                                                     
         EJECT                                                                  
ROFFD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RSTAD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
RBUDD    DSECT                                                                  
       ++INCLUDE REGENBUD                                                       
         SPACE 1                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
         SPACE                                                                  
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE RENRGWKD                                                       
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   APPLORG                                                          
         DS    0F                                                               
ACOMBOS  DS    A                                                                
STADISP  DS    F                                                                
SVREGS   DS    2F                                                               
SVR5R6   DS    2F                                                               
SVREGE   DS    F                                                                
RELO     DS    A                                                                
COMBOSTA DS    (CSTAMAX)CL7                                                     
CSTAMAX  EQU   8                                                                
NCOMBOS  DS    X                                                                
FIRSTSTA DS    C                                                                
LASTLN   DS    C                                                                
ERREX2SW DS    C                                                                
LISTEND  DS    C                                                                
FIRSTR   DS    C                                                                
OFFSTASW DS    C                                                                
ROUNDSW  DS    C                                                                
MONTH    DS    X                                                                
LIMACC   DS    X                                                                
LIMACOFF EQU   X'80'                                                            
LIMACSTA EQU   X'40'                                                            
LSTTYPE  DS    X                   TYPE DATA BEING LISTED (ADV/AGY/STA)         
LSTTYPLN DS    X                   LEN OF DATA LISTED                           
LSTKEY   DS    XL(L'ROKDTLTY)                                                   
LSTKEYEX DS    XL1                                                              
LSTDISP  DS    XL1                                                              
DISPFLAG DS    XL1                 DISPLAY FLAG                                 
DISPBOOK EQU   X'80'                                                            
DISPCONF EQU   X'40'                                                            
DISPDIR  EQU   X'20'                                                            
DISPUNCN EQU   X'10'                                                            
OPTRACE  EQU   X'08'               OFFLINE TRACE                                
OPCONDCT EQU   X'04'               CONTYPE/DEVTYPE HARD CODE ON                 
         SPACE                                                                  
SVTYPES  DS    CL5                 POSSIBLE TYPES ON THIS REPS FILE             
         SPACE                                                                  
TYPEKEY  DS    CL1                 NULL= ALL DOLLARS                            
*                                  C = CONFIRMED                                
*                                  U = UNCONFIRMED                              
*                                  D = DIRECT CONFIRMED                         
*                                  R = DIRECT                                   
LSTVAL   DS    CL8                                                              
         SPACE                                                                  
WARNDATE DS    CL8                                                              
SVGRP    DS    CL2                                                              
SVAFF    DS    CL3                                                              
SVOWN    DS    CL3                                                              
SVTVB    DS    CL2                                                              
SVRNK    DS    CL1                                                              
STATION  DS    CL5                                                              
CURWKS   DS    XL12                                                             
CURWKSY  DS    XL12                                                             
PRIWKS   DS    XL12                                                             
PRIWKSY  DS    XL12                                                             
COMPBUDG DS    CL1                 COMPANY BUDGET FLAG                          
*                                  N  =  DON'T TRY TO RETRIEVE IT               
SETKEY   DS    XL3                 KEY OF FILTERS AND POSSIBLE L=               
*                                  WITH X'80' SET ON IF SETS                    
TRCTYPE  DS    XL1                                                              
FOUNDSET DS    CL1                                                              
FULLSTER DS    CL1                 Y IF FULL STEREO, N IF EMULATOR              
SETKEYAD DS    A                                                                
         SPACE                                                                  
         DS    CL37                SPARE                                        
         DS    CL1                                                              
P        DS    CL132                                                            
ENDSYSD  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RENRG10   05/01/02'                                      
         END                                                                    
