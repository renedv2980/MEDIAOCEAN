*          DATA SET NEBUY17S   AT LEVEL 041 AS OF 05/01/02                      
*PHASE T31117A,+0                                                               
*INCLUDE NETINTG                                                                
         TITLE 'NETPAK BUY PROGRAM - PACKAGE UPDATE - T31117'                   
T31117   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PKUP**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         ST    RB,MYBASE                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
*                                                                               
PU       L     R4,APACKREC                                                      
         USING NPRECD,R4                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BZ    PU2                                                              
         MVI   FERN,PAKLERR                                                     
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
PU2      BAS   RE,ACTED            EDIT ACTION FIELD FOR REASON CODE            
         TM    MODE,DISPLAY        TEST FOR CHANGE IN HEADLINE FIELDS           
         BZ    *+8                 NO                                           
         OI    MODE,FIRST                                                       
         TM    SVLMODE,EOF         TEST FOR EOF ON LAST TRANSACTION             
         BZ    *+8                                                              
         OI    MODE,FIRST                                                       
         BAS   RE,EDIT                                                          
         TM    MODE,FIRST          TEST FOR STARTING UPDATE                     
         BZ    PU20                NO-CONTINUE UPDATE IN PROGRESS               
         BAS   RE,CLRSAVE          YES                                          
*                                                                               
PU4      MVC   NBAIO,AIOAREA1      RE-READ THE PACKAGE RECORD                   
         MVI   NBRESUME,NBPROCPK                                                
         MVC   NBACOM,ACOMFACS                                                  
PU5      GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCPK                                                  
         BNE   PU5                                                              
         SPACE 1                                                                
* PROCESS KEYWORD STACK AND UPDATE PACKAGE RECORD ACCORDINGLY                   
*                                                                               
PU6      L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         ZIC   R2,KEYCOUNT                                                      
         LA    R3,KEYSTACK                                                      
         USING KEYTABD,R3                                                       
*                                                                               
PU7      TM    KEYPKCTL,NOUPDATE   TEST IF KEY UPDATES PACKAGE                  
         BO    PU9                                                              
         TM    KEYPKCTL,ROUTINE    TEST IF KEY HAS UPDATE ROUTINE               
         BZ    PU8                                                              
         SR    RF,RF                                                            
         ICM   RF,3,KEYPACK                                                     
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         MVI   PACKUP,YES                                                       
         B     PU9                                                              
*                                                                               
PU8      TM    KEYPKCTL,DATADISP                                                
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   PACKUP,YES                                                       
         SR    RE,RE                                                            
         ICM   RE,3,KEYPACK                                                     
         LA    RE,0(R4,RE)         POINT RE TO FIELD ON RECORD                  
         ZIC   R1,KEYDLEN                                                       
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,3,KEYDATA                                                     
         LA    RF,TEMPD(RF)        POINT RF AT PARAMETER DATA                   
         EX    R1,*+8                                                           
         B     PU9                                                              
         MVC   0(0,RE),0(RF)       MOVE VALUE TO RECORD                         
*                                                                               
PU9      LA    R3,KEYTABL(R3)                                                   
         BCT   R2,PU7                                                           
         SPACE 1                                                                
* UPDATE PACKAGE RECORD                                                         
*                                                                               
PU10     CLI   PACKUP,YES          TEST FOR PACKAGE UPDATED                     
         BNE   PU11                NO                                           
         MVC   NPAKACTD,TODAYC     SET LAST CHANGE DATE                         
         MVI   NPAKACTA,C'C'                                                    
*                                                                               
         XC    KEY,KEY             UPDATE PACKAGE                               
         MVC   KEY(NDIRLEN),NBKEY  EXTRACT KEY AND DA FROM BLOCK                
         LA    RE,NBKEY                                                         
         MVC   NDXDA,NUDA-NUKEY(RE)                                             
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         SPACE 2                                                                
* INITIALIZE NETBLOCK FOR READ OF UNITS BY PROGRAM                              
*                                                                               
PU11     MVC   NBSELDP,NBACTDP                                                  
         MVC   NBSELSTR,ESTSTART                                                
         MVC   NBSELEND,ESTEND                                                  
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'Q'          PROGRAM ORDER                                
         MVI   NBUSER+13,NO                                                     
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBRESUME,0                                                       
*                                                                               
         BAS   RE,SAVBLOCK                                                      
         SPACE 1                                                                
* FOR FIRST TIME, READ FOR FIRST UNIT                                           
*                                                                               
PU12     GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR FIRST UNIT                          
         BE    PU30                YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BNE   PU12                                                             
         MVC   BUYMSG(L'NOUNITS),NOUNITS                                        
         MVI   MODE,0                                                           
         BAS   RE,CLRSAVE                                                       
         MVI   SVLMODE,EOF                                                      
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         GOTO1 VEXIT                                                            
         SPACE 2                                                                
* RESTORE NETBLOCK AND CONTINUE READ                                            
*                                                                               
PU20     BAS   RE,RESTBLK                                                       
         MVI   FRSTSW,YES                                                       
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVI   NBFUNCT,NBFRDHI                                                  
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
*                                                                               
         LA    R2,PUPYESH          EDIT UPDATE FIELD                            
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         MVI   FERN,INVERR                                                      
         MVC   UPSW,FLD                                                         
         EX    R1,YESCOMP                                                       
         BE    *+12                                                             
         EX    R1,NOCOMP                                                        
         BNE   ERROR                                                            
         SPACE 1                                                                
* LOOP PROCESSING - SAVE NETBLOCK, READ RECORD, AND UPDATE                      
*                                                                               
PU22     BAS   RE,SAVBLOCK                                                      
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    PU30                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   PU22                                                             
         CLI   FRSTSW,YES          TEST FOR FIRST UNIT (RE-READ)                
         BE    *+12                YES-SKIP PROGRAM BREAK CHECK                 
         TM    NBSUBMSK,NBSBMPRG   TEST FOR PROGRAM BREAK                       
         BO    PU30                YES                                          
*                                                                               
         MVI   FRSTSW,NO                                                        
         CLI   UPSW,NO                                                          
         BE    PU22                                                             
*                                                                               
         LA    R2,UNBLK            INITIALIZE UNIT EDIT BLOCK                   
         USING UNBLOCKD,R2                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,NBAIO                                                     
         MVC   UNALOCAL,AIOAREA4                                                
         OC    HUTYP,HUTYP                                                      
         BZ    *+8                                                              
         OI    UNBITOPT,X'20'                                                   
         GOTO1 VEDIT,DMCB,(C'I',UNBLK)                                          
         SPACE 1                                                                
* PROCESS KEYWORD STACK AND UPDATE THE UNIT RECORD                              
*                                                                               
         ZIC   R2,KEYCOUNT                                                      
         LA    R3,KEYSTACK                                                      
*                                                                               
PU23     TM    KEYUNCTL,NOUPDATE                                                
         BO    PU25                                                             
         TM    KEYUNCTL,NOUPPAID   NO UPDATE FOR PAID UNITS                     
         BZ    PU23A                                                            
         OC    NBPAYTGR(16),NBPAYTGR                                            
         BNZ   PU25                                                             
PU23A    TM    KEYUNCTL,ROUTINE                                                 
         BZ    PU24                                                             
         SR    RF,RF                                                            
         ICM   RF,3,KEYUNIT                                                     
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     PU25                                                             
*                                                                               
PU24     TM    KEYUNCTL,DATADISP                                                
         BO    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,KEYUNIT                                                     
         A     RE,NBAIO            POINT TO FIELD ON RECORD                     
         ZIC   R1,KEYDLEN                                                       
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,3,KEYDATA                                                     
         LA    RF,TEMPD(RF)                                                     
         EX    R1,*+8                                                           
         B     PU25                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
PU25     LA    R3,KEYTABL(R3)                                                   
         BCT   R2,PU23                                                          
         SPACE 1                                                                
* UPDATE ACTIVITY ELEMENT AND WRITE RECORD BACK                                 
*                                                                               
PU26     GOTO1 VEDIT,DMCB,(C'F',UNBLK)                                          
*                                                                               
         XC    KEY,KEY                                                          
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         LA    RE,NBKEY                                                         
         MVC   KEY(NDIRLEN),NBKEY                                               
         MVC   NDXDA,NUDA-NUKEY(RE)                                             
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         L     R1,PRGUNITS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PRGUNITS                                                      
         B     PU22                GET NEXT RECORD                              
         SPACE 2                                                                
* EOF/NEW PROGRAM PROCESSING                                                    
*                                                                               
PU30     XC    PUPPRGD,PUPPRGD     CLEAR PROGRAM DESCRIPTION                    
         OI    PUPPRGDH+6,X'80'    XMIT                                         
         XC    PUPSUM,PUPSUM       CLEAR SUMMARY LINE                           
         OI    PUPSUMH+6,X'80'                                                  
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    PU35                                                             
         L     R3,PRGUNITS                                                      
         TM    MODE,FIRST                                                       
         BO    PU32                                                             
         LA    R2,PUPSUM                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0               UPDATE OUTPUT POINTER                        
         MVC   1(17,R2),=C'UNITS UPDATED FOR'                                   
         LA    R2,19(R2)                                                        
         MVC   0(L'SVPROG,R2),SVPROG    LAST PROGRAM                            
         SPACE                                                                  
PU32     A     R3,SVUNITS          UPDATE COUNT OF CHANGED UNITS                
         ST    R3,SVUNITS                                                       
         MVC   PROG,NBACTPRG                                                    
         MVC   SVPROG,PROG         GET PROGRAM                                  
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         CLI   FERN,0                                                           
         BNE   PU34                                                             
*                                                                               
         MVC   PUPPRGD(L'PROG),PROG                                             
         LA    R0,L'PROG                                                        
         LA    R2,PUPPRGD+L'PROG-1                                              
         CLI   0(R2),C' '          FIND LAST CHARACTER OF PROGRAM CODE          
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
         LA    R2,1(R2)                                                         
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)                                                         
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   0(L'NPGNAME,R2),NPGNAME                                          
         DROP  RE                                                               
         SPACE                                                                  
PU34     LA    R2,PUPYESH                                                       
         ST    R2,FADDR                                                         
         MVC   8(3,R2),=C'YES'     XMIT 'YES' IN COPY                           
         OI    6(R2),X'81'         MODIFIED NEXT TIME                           
         MVC   BUYMSG(4),=C'NEXT'                                               
         LA    R3,BUYMSG+5                                                      
         TM    MODE,FIRST                                                       
         BZ    *+14                                                             
         MVC   BUYMSG(5),=C'FIRST'                                              
         LA    R3,BUYMSG+6                                                      
         MVC   0(13,R3),=C'PROGRAM SHOWN'                                       
         LA    R3,14(R3)                                                        
         MVI   0(R3),DASH                                                       
         LA    R3,2(R3)                                                         
         MVC   0(21,R3),=C'ENTER ''YES'' TO UPDATE'                             
         MVI   SVLMODE,PROCESS                                                  
         B     PUX                                                              
         SPACE                                                                  
PU35     L     R3,PRGUNITS                                                      
         A     R3,SVUNITS                                                       
         EDIT  (R3),(4,PUPSUM),ALIGN=LEFT                                       
         LA    R2,PUPSUM                                                        
         AR    R2,R0                                                            
         MVC   1(25,R2),=C'UNITS UPDATED FOR PACKAGE'                           
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'EOFMSG),EOFMSG                                          
         BAS   RE,CLRSAVE                                                       
         MVI   SVLMODE,EOF         FORCE STARTING OVER                          
         GOTO1 VBLDRQST                                                         
         SPACE                                                                  
PUX      NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
         SPACE 2                                                                
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO '                                                   
         EJECT                                                                  
* SUB-ROUTINE TO EDIT NEW DATA FIELD (KEY=PARAMETER VALUE,...)                  
*                                                                               
EDIT     NTR1                                                                   
         LA    R2,PUPNEWH          TEST FOR NO DATA IN FIELD                    
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FNDX,0              INITIALIZE ERROR FIELD INDEX                 
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
EDIT2    XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUAL         LOOK FOR EQUALS SIGN                         
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,0                                                         
         BNE   EDIT4                                                            
         CLI   FSTOP,X'FF'         TEST FOR EOF                                 
         BE    EDITX               YES                                          
         B     ERROR                                                            
*                                                                               
EDIT4    ZIC   R1,FNDX             UPDATE SUB-FIELD INDEX                       
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLI   FLDH+5,L'KEYNAME    VALIDATE THE KEYWORD                         
         BH    ERROR                                                            
         LA    R0,KEYS                                                          
         LA    R3,KEYTAB                                                        
         USING KEYTABD,R3                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
EDIT6    CLC   FLDH+5(1),KEYMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    EDIT6A              NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),KEYNAME                                                   
         BE    EDIT7               VALID KEYWORD                                
EDIT6A   LA    R3,KEYTABL(R3)                                                   
         BCT   R0,EDIT6                                                         
         MVC   XTRA(7),=C'KEYWORD'                                              
         B     ERROR                                                            
*                                                                               
EDIT7    GOTO1 ADDKEY,KEYTABD                                                   
         XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,KEYAVAL        GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     EDIT2                                                            
*                                                                               
EDITX    B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MAINTAIN ACTIVE KEYWORD STACK                                  
* AT ENTRY R1 PINTS TO NEW ENTRY                                                
*                                                                               
         USING KEYTABD,R1                                                       
ADDKEY   ST    RE,SAVEREG                                                       
         LA    RE,KEYSTACK                                                      
ADDKEY1  OC    0(KEYTABL,RE),0(RE)                                              
         BZ    ADDKEY2                                                          
         CLC   KEYNUM,KEYNUM-KEYTABD(RE)     TEST DUPLICATE KEY                 
         BE    ADDKEYR                                                          
         LA    RE,KEYTABL(RE)                                                   
         B     ADDKEY1                                                          
*                                                                               
ADDKEYR  MVC   XTRA(17),=C'DUPLICATE KEYWORD'                                   
         B     ERROR                                                            
*                                                                               
ADDKEY2  MVC   0(KEYTABL,RE),KEYTABD                                            
         ZIC   RF,KEYCOUNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,KEYCOUNT                                                      
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINES TO VALIDATE KEYWORD PARAMETER VALUES                             
*                                                                               
*VALINT   ST    RE,SAVEREG          GET INTEGRATION                             
*         CLI   FLDH+5,0            TEST FOR VALUE FOUND                        
*         BE    ERROR               NO                                          
*         CLI   FLDH+5,3                                                        
*         BNE   IN100                                                           
*         CLC   =C'TBL',FLD         USE INTEGRATION TABLES                      
*         BNE   IN100                                                           
*         MVI   INTTBLSW,C'Y'                                                   
*         B     VALEXIT                                                         
*                                                                               
*IN100    ZIC   R0,FLDH+5                                                       
*         GOTO1 VCASHVAL,DMCB,FLD,(R0)                                          
*         CLI   0(R1),X'FF'                                                     
*         BE    ERROR                                                           
*         MVC   INTEG,4(R1)                                                     
*         B     VALEXIT                                                         
         SPACE 2                                                                
VALINT   ST    RE,SAVEREG          GET INTEGRATION                              
         MVI   INTSTAT,0                                                        
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR                                                            
         CLC   =C'TBL',FLD                                                      
         BNE   IN100                                                            
         MVI   INTTBLSW,C'Y'                                                    
         B     VALEXIT                                                          
*                                                                               
IN100    GOTO1 VSCANNER,DMCB,FLDH,(2,INTGBLK),C',=,/'                           
         MVI   FERN,INVERR                                                      
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BZ    ERROR                                                            
         LA    R2,INTGBLK                                                       
*                                                                               
         ZIC   R0,0(R2)                                                         
         GOTO1 VCASHVAL,DMCB,12(R2),(R0)                                        
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         MVC   INTEG,DMCB+4                                                     
         SPACE                                                                  
INTED2   CLI   1(R2),0                                                          
         BE    VALEXIT                                                          
         CLI   1(R2),1                                                          
         BNE   ERROR                                                            
         CLI   22(R2),C'N'         TEST FOR NON-COMM. INT.                      
         BNE   ERROR                                                            
         MVI   INTSTAT,X'04'                                                    
         B     VALEXIT                                                          
*                                                                               
VALUNIV  NTR1                                                                   
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         CLI   FLDH+5,4            TEST MAX LEN OF 4                            
         BH    ERROR                                                            
         TM    FLDH+4,X'08'                                                     
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         MVC   UNIVCODE,DUB+5      TO ISOLATE UNIVERSE CODE AS PWO              
*                                                                               
         LA    R2,UNIVBLK                                                       
         USING GUVD,R2                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,AGYALPH                                                   
         MVC   GUVCODE,UNIVCODE                                                 
         LA    R1,UNIVEL                                                        
         ST    R1,GUVAOUT                                                       
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVNETWK,VNETWEEK                                                
         NI    NUUNST2,X'FF'-X'08' TURN OFF CABLE UNIV                          
         GOTO1 VGETNUN,DMCB,GUVBLOCK                                            
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    NUUNST2,X'08'                                                    
         CLI   GUVERROR,0                                                       
         BE    EXXMOD                                                           
         MVI   FERN,UNIVERR                                                     
         B     ERROR                                                            
         SPACE 2                                                                
VALPG    ST    RE,SAVEREG                                                       
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,(4,FLD),(R0)                                       
****     GOTO1 VCASHVAL,DMCB,FLD,(R0)                                           
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         MVC   PNGUAR,4(R1)                                                     
***      MVC   PGUAR,6(R1)                                                      
         B     VALEXIT                                                          
         SPACE 2                                                                
VALSRP   NTR1                                                                   
         XC    SPREP,SPREP                                                      
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    EXXMOD              NO, EXIT                                     
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BO    *+12                YES                                          
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         CVB   R0,DUB                                                           
         LTR   R0,R0               REP CODE CANNOT BE ZERO                      
         BZ    ERROR                                                            
         CH    R0,=H'999'                                                       
         BH    ERROR                                                            
         STH   R0,SPREP            SAVE BINARY VALUE                            
         OI    DUB+7,X'0F'         CREATE 3 CHARACTER REP NUMBER                
         UNPK  THREE,DUB                                                        
         SPACE                                                                  
         XC    KEY,KEY             NOW VALIDATE THE REP                         
         LA    RE,KEY                                                           
         USING REPRECD,RE                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,THREE                                                    
         MVC   REPKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,AIOAREA2                                  
         CLC   KEY(L'REPKEY),KEYSAVE  TEST IF REP FOUND                         
         BE    EXXMOD                 YES                                       
         MVI   FERN,REPERR                                                      
         B     ERROR                                                            
         DROP  RE                                                               
         SPACE 2                                                                
VALPOST  ST    RE,SAVEREG                                                       
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         CLI   FLDH+5,1            1 BYTE                                       
         BE    *+12                YES                                          
VM100    MVI   FERN,INVPMETR                                                    
         B     ERROR                                                            
*                                                                               
         CLI   FLD,C'C'                                                         
         BE    VM200                                                            
         CLI   FLD,C'A'                                                         
         BE    VM200                                                            
         CLI   FLD,C'D'                                                         
         BE    VM200                                                            
         CLI   FLD,C'Z'                                                         
         BE    VM200                                                            
         CLI   FLD,C'I'                                                         
         BNE   VM100                                                            
VM200    MVC   POSTD,FLD                                                        
         B     VALEXIT                                                          
         SPACE 2                                                                
VALDG    NTR1                                                                   
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         MVI   FLEN,0              RE-EDIT FIELD                                
         XC    FTERM,FTERM                                                      
         MVI   FTERM,SLASH                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
*                                                                               
         LA    R2,DBLOCKA                                                       
         USING DBLOCKD,R2                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'EVN'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'V'                                                    
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CDEMOVAL,DMCB,FLDH,(1,FULL),DBLOCKA                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         MVC   DCAT,FULL+2         EXTRACT CATEGORY                             
*                                                                               
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         GET FACTOR VALUE                             
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,(4,FLD),(R0)                                       
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         MVC   DNGUAR,4(R1)                                                     
***      MVC   DGUAR,6(R1)                                                      
         B     EXXMOD                                                           
         SPACE 2                                                                
VALUPCT  ST    RE,SAVEREG          GET UNIVERSE PERCENTAGE                      
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         CLI   FLDH+5,6                                                         
         BH    ERROR                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R0)                                           
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         MVC   UNIVPCT,6(R1)                                                    
         B     VALEXIT                                                          
         SPACE 2                                                                
VALPAUD  ST    RE,SAVEREG          GET UNIVERSE PERCENTAGE                      
         XC    PAUDTOT,PAUDTOT                                                  
         GOTO1 VSCANNER,DMCB,(20,FLDH),(1,WORK),C',=,/'                         
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK,0              AUDIT CODE NOT MORE THEN 4                   
         BE    VALEXIT                                                          
         CLI   WORK,4              AUDIT CODE NOT MORE THEN 4                   
         BH    ERROR                                                            
         CLI   WORK+1,16           AUDIT COMMENT NOT MORE THEN 16               
         BH    ERROR                                                            
         MVC   PAUDG(4),WORK+12                                                 
         MVC   PAUDC(16),WORK+22                                                
         B     VALEXIT                                                          
         SPACE                                                                  
VALPFIL  ST    RE,SAVEREG          PACKAGE FILTER                               
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    ERROR               NO                                           
         CLI   FLDH+5,6                                                         
         BH    ERROR                                                            
         CLI   FLDH+5,4                                                         
         BNE   VP100                                                            
         CLC   =C'*DEL',FLD                                                     
         BE    VP200                                                            
*                                                                               
VP100    MVC   XTRA(15),=C'-,A-Z,1-9 VALID'                                     
         ZIC   RE,FLDH+5                                                        
         LA    RF,FLD                                                           
FILTR10  CLI   0(RF),C'-'                                                       
         BE    FILTR12                                                          
         CLI   0(RF),C'A'          BETWEEN A - 9                                
         BL    ERROR                                                            
         CLI   0(RF),C'9'                                                       
         BH    ERROR                                                            
FILTR12  LA    RF,1(RF)                                                         
         BCT   RE,FILTR10                                                       
         XC    XTRA,SPACES                                                      
*                                                                               
VP200    MVC   PFILTR,FLD                                                       
         B     VALEXIT                                                          
*                                                                               
VALHADJ  ST    RE,SAVEREG          HUT ADJUSTMANT PERCENT                       
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,6                                                         
         BH    ERROR                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R0)                                           
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    ERROR                                                            
*                                                                               
         MVC   HUADJ,6(R1)                                                      
         B     VALEXIT                                                          
         SPACE                                                                  
VALHTYP  ST    RE,SAVEREG          HUT TYPE                                     
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,1                                                         
         BH    ERROR                                                            
         CLI   FLD,C'A'                                                         
         BE    *+20                                                             
         CLI   FLD,C'D'                                                         
         BE    *+12                                                             
         CLI   FLD,C'I'                                                         
         BNE   ERROR                                                            
         MVC   HUTYP,FLD                                                        
         B     VALEXIT                                                          
         SPACE                                                                  
VALHTAVG ST    RE,SAVEREG          HUT TYPE                                     
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,1                                                         
         BH    ERROR                                                            
         CLI   FLD,C'Q'                                                         
         BE    *+20                                                             
         CLI   FLD,C'M'                                                         
         BE    *+12                                                             
         CLI   FLD,C'W'                                                         
         BNE   ERROR                                                            
         MVC   HUTAVG,FLD                                                       
         B     VALEXIT                                                          
         SPACE 2                                                                
VALBTYP  ST    RE,SAVEREG          HUT TYPE                                     
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,1                                                         
         BH    ERROR                                                            
         CLI   FLD,C'S'                                                         
         BE    *+20                                                             
         CLI   FLD,C'O'                                                         
         BE    *+12                                                             
         CLI   FLD,C'U'                                                         
         BNE   ERROR                                                            
         MVC   BUYTYPE,FLD                                                      
         B     VALEXIT                                                          
         SPACE 2                                                                
VALEXIT  L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINES TO UPDATE UNIT RECORDS                                           
*                                                                               
UPUNIV   ST    RE,SAVEREG                                                       
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         MVC   NUUNCODE,UNIVCODE                                                
         MVI   ELCODE,X'31'                                                     
         BAS   RE,DELEL                                                         
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,UNIVEL,0                        
         B     UPEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
UPPG     ST    RE,SAVEREG                                                       
         MVI   ELCODE,X'B3'        DELETE PACKAGE GUARANTEE ELEMENT             
***      MVI   ELCODE,X'B1'        DELETE PACKAGE GUARANTEE ELEMENT             
         BAS   RE,DELEL                                                         
         OC    PNGUAR,PNGUAR       TEST FOR ZERO FACTOR                         
***      OC    PGUAR,PGUAR         TEST FOR ZERO FACTOR                         
         BZ    UPPGX               YES-ONLY DELETE FACTOR                       
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING NUNGUD,RE                                                        
         MVI   NUNGUEL,X'B3'                                                    
         MVI   NUNGULEN,NUNGUELN                                                
         MVC   NUNGUFAC,PNGUAR                                                  
***      USING NUGUAD,RE                                                        
***      MVI   NUGUAEL,X'B1'                                                    
***      MVI   NUGUALEN,NUGUAELN                                                
***      MVC   NUGUAFAC,PGUAR                                                   
         BAS   RE,PUTEL                                                         
*                                                                               
UPPGX    B     UPEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
UPDG     ST    RE,SAVEREG                                                       
         MVI   ELCODE,X'B4'                                                     
         BAS   RE,DELEL                                                         
         OC    DNGUAR,DNGUAR                                                    
***      OC    DGUAR,DGUAR                                                      
         BZ    UPDGX                                                            
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING NUNDGD,RE                                                        
         MVI   NUNDGEL,X'B4'                                                    
         MVI   NUNDGLEN,NUNDGELN                                                
         MVC   NUNDGDEM+2(1),DCAT                                               
         MVC   NUNDGFAC,DNGUAR                                                  
         BAS   RE,PUTEL                                                         
*                                                                               
UPDGX    B     UPEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
UAUD     ST    RE,SAVEREG                                                       
         MVI   ELCODE,X'09'                                                     
         BAS   RE,DELEL                                                         
         OC    PAUDG,PAUDG                                                      
         BZ    UAUDX                                                            
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING NUAUDD,RE                                                        
         MVI   NUAUDEL,X'09'                                                    
         MVI   NUAUDLEN,NUAUDELN                                                
         MVC   NUAUDGRP(4),PAUDG                                                
         MVC   NUAUDCOM(16),PAUDC                                               
         BAS   RE,PUTEL                                                         
*                                                                               
UAUDX    B     UPEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
UFIL     ST    RE,SAVEREG                                                       
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'08',AIOAREA1),(1,=C'K')            
         CLC   =C'*DEL',PFILTR                                                  
         BE    UPFLX                                                            
*                                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(3),=XL3'080BD2'                                             
         MVC   WORK+3(6),PFILTR                                                 
         BAS   RE,PUTEL                                                         
*                                                                               
UPFLX    B     UPEXIT                                                           
         SPACE 1                                                                
UPBTYP   ST    RE,SAVEREG                                                       
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'60',AIOAREA1),(1,=C'F')            
*                                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(3),=XL3'6004C6'                                             
         MVC   WORK+3(1),BUYTYPE                                                
         BAS   RE,PUTEL                                                         
*                                                                               
UPBTYPX  B     UPEXIT                                                           
         SPACE 1                                                                
UPOST    ST    RE,SAVEREG                                                       
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         MVC   NUPOSTDT,POSTD                                                   
*                                                                               
UPPOST   B     UPEXIT                                                           
         SPACE 1                                                                
USRP     ST    RE,SAVEREG                                                       
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         MVC   NUSREP,SPREP                                                     
*                                                                               
UPSRX    B     UPEXIT                                                           
         SPACE                                                                  
UPEXIT   L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 1                                                                
         DROP  RE                                                               
         SPACE 1                                                                
UPINTG   NTR1                                                                   
         L     R2,NBAIO            SET UP RF TO UNIT RECORD                     
         USING NURECD,R2                                                        
         NI    NUPACKST,X'FB'       UNSET NET INTEGRATION                       
         OC    NUPACKST,INTSTAT     RESET INT STATUS                            
         CLI   INTTBLSW,C'Y'                                                    
         BE    *+14                                                             
         MVC   NUINTEG,INTEG       USE INPUT DOLLARS                            
         B     UPINTEX                                                          
*                                                                               
         XC    INTGBLK,INTGBLK                                                  
         LA    RE,INTGBLK          SET UP INTEG BLOCK                           
         USING NIBLKD,RE                                                        
         MVC   NIBAIO,AIOAREA3     A(2K BLOCK)                                  
         ST    R6,NIBNETB          A(NETBLOCK)                                  
         LA    R1,SVINTTBL                                                      
         ST    R1,NIBAIO2          OUTPUT AREA (70 BYTES - IN 2ND TWA)          
         MVC   NIBDSKSV,SVLSTIDA                                                
         MVC   NIBAM,NBACTAM                                                    
         MVC   NIBNTWK,NBACTNET                                                 
         MVC   NIBDAY,NUDAY                                                     
         MVC   NIBDPT,NBACTDP                                                   
         MVC   NIBTIME,NUTIME                                                   
         MVC   NIBPROG,NUKPROG                                                  
         MVC   NIBSTRT,NUKDATE     2 BYTE DATE                                  
         GOTO1 =V(NETINTG),DMCB,INTGBLK,RR=MYRELO                               
         BNZ   INTER                                                            
         L     RE,0(R1)            SET UP INTEG BLOCK                           
         MVC   SVLSTIDA,NIBDSKSV                                                
         MVC   NUINTEG,NIBRATE                                                  
         SPACE 1                                                                
         DROP  RE,R2                                                            
         SPACE 1                                                                
UPINTEX  B     EXXMOD                                                           
         SPACE                                                                  
INTER    MVI   FERN,INTGNF                                                      
         B     ERROR                                                            
         SPACE 1                                                                
UHTYPE   ST    RE,SAVEREG                                                       
         MVC   PROG,NBACTPRG                                                    
*                                                                               
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         MVC   NUHUTTYP,HUTYP                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VEDIT,DMCB,(C'D',UNBLK)                                          
         LA    RE,UNBLK            INITIALIZE UNIT EDIT BLOCK                   
         USING UNBLOCKD,RE                                                      
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
UPTYPX   B     UPEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
UHTAVG   ST    RE,SAVEREG                                                       
         MVC   PROG,NBACTPRG                                                    
*                                                                               
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         MVC   NUHUTAVE,HUTAVG                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VEDIT,DMCB,(C'D',UNBLK)                                          
         LA    RE,UNBLK            INITIALIZE UNIT EDIT BLOCK                   
         USING UNBLOCKD,RE                                                      
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
UPAVGX   B     UPEXIT                                                           
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINES TO UPDATE PACKAGE RECORDS                                        
*                                                                               
PAKINTG  ST    RE,SAVEREG                                                       
         L     RE,NBAIO                                                         
         USING NPRECD,RE                                                        
         NI    NPAKSTAT,X'FB'       UNSET NET INTEGRATION                       
         OC    NPAKSTAT,INTSTAT     RESET INT STATUS                            
         NI    NPAKCNTL,X'7F'       RESET TABLE INTG SETTING                    
         CLI   INTTBLSW,C'Y'                                                    
         BE    *+14                                                             
         MVC   NPAKINT,INTEG       USE INPUT DOLLARS                            
         B     PAKEXIT                                                          
*                                                                               
         XC    NPAKINT,NPAKINT                                                  
         OI    NPAKCNTL,X'80'      USE INTEG TABLE DOLLARS                      
         DROP  RE                                                               
*                                                                               
         XC    INTGBLK,INTGBLK                                                  
         LA    RE,INTGBLK          SET UP INTEG BLOCK                           
         USING NIBLKD,RE                                                        
         MVC   NIBAIO,AIOAREA3     A(2K BLOCK)                                  
         ST    R6,NIBNETB          A(NETBLOCK)                                  
         LA    RF,SVINTTBL                                                      
         ST    RF,NIBAIO2          OUTPUT AREA (70 BYTES - IN 2ND TWA)          
         OI    NIBCNTL,X'80'       JUST READ FOR RECORDS                        
         MVC   NIBAM,NBACTAM                                                    
         MVC   NIBNTWK,NBACTNET                                                 
         MVC   NIBSTRT(4),ESTS     2 BYTE DATE RANGE                            
         GOTO1 =V(NETINTG),DMCB,INTGBLK,RR=MYRELO                               
         BNZ   INTER                                                            
         SPACE 1                                                                
         DROP  RE                                                               
         SPACE 1                                                                
PAKEXIT  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(ELCODE,NBAIO),0                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,WORK,0                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD FOR REASON CODE                              
*                                                                               
ACTED    NTR1                                                                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTED20             NO, REASON CODE NOT INPUTTED                 
*                                                                               
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ACTED20             NO, REASON CODE NOT INPUTTED                 
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
*                                                                               
ACTED20  MVI   FERN,AUDITERR                                                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    ACTED30             YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTEDX                                                           
         B     ERROR                                                            
ACTED30  OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR                                                            
*                                                                               
ACTEDX   B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR TWA SAVE AREA                                            
*                                                                               
CLRSAVE  ST    RE,SAVEREG          SAVE RETURN POINT                            
         LA    RE,SVDATA                                                        
         LA    RF,SVDATAL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE SAVED BLOCK                                            
*                                                                               
RESTBLK  ST    RE,SAVEREG                                                       
         LA    RE,NEBLOCKA                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE NETBLOCK IN TWA SAVE AREA                                 
*                                                                               
SAVBLOCK ST    RE,SAVEREG                                                       
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NEBLOCKA                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* TABLE OF KEYWORDS AND THEIR ASSOCIATED VALUES (COVERED BY KEYTABD)            
*                                                                               
KEYTAB   DS    0CL(KEYTABL)                                                     
*                                                                               
         DC    CL6'INT',AL1(INT),AL1(3)                                         
         DC    AL3(VALINT-T31117)                                               
         DC    AL1(L'INTEG)                                                     
         DC    AL2(INTEG-TEMPD)                                                 
         DC    AL1(ROUTINE)                                                     
         DC    AL2(PAKINTG-T31117)                                              
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPINTG-T31117)                                               
*                                                                               
         DC    CL6'UNIV',AL1(UNIV),AL1(3)                                       
         DC    AL3(VALUNIV-T31117)                                              
         DC    AL1(L'UNIVCODE)                                                  
         DC    AL2(UNIVCODE-TEMPD)                                              
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKUNCD-NPRECD)                                             
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPUNIV-T31117)                                               
*                                                                               
         DC    CL6'PGUAR',AL1(PG),AL1(2)                                        
         DC    AL3(VALPG-T31117)                                                
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(NOUPDATE)                                                    
         DC    AL2(0)                                                           
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPPG-T31117)                                                 
*                                                                               
         DC    CL6'DGUAR',AL1(DG),AL1(2)                                        
         DC    AL3(VALDG-T31117)                                                
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(NOUPDATE)                                                    
         DC    AL2(0)                                                           
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPDG-T31117)                                                 
*                                                                               
         DC    CL6'UPER',AL1(UPCT),AL1(2)                                       
         DC    AL3(VALUPCT-T31117)                                              
         DC    AL1(L'UNIVPCT)                                                   
         DC    AL2(UNIVPCT-TEMPD)                                               
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKUNIV-NPRECD)                                             
         DC    AL1(DATADISP)                                                    
         DC    AL2(NUUNIV-NURECD)                                               
*                                                                               
         DC    CL6'PFIL',AL1(PFLT),AL1(2)                                       
         DC    AL3(VALPFIL-T31117)                                              
         DC    AL1(L'PFILTR)                                                    
         DC    AL2(PFILTR-TEMPD)                                                
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UFIL-T31117)                                                 
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UFIL-T31117)                                                 
*                                                                               
         DC    CL6'AUDGRP',AL1(PAUDT),AL1(2)                                    
         DC    AL3(VALPAUD-T31117)                                              
         DC    AL1(L'PAUDTOT)                                                   
         DC    AL2(PAUDG-TEMPD)                                                 
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UAUD-T31117)                                                 
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UAUD-T31117)                                                 
*                                                                               
         DC    CL6'SREP',AL1(SREP),AL1(2)                                       
         DC    AL3(VALSRP-T31117)                                               
         DC    AL1(L'SPREP)                                                     
         DC    AL2(SPREP-TEMPD)                                                 
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKSREP-NPRECD)                                             
         DC    AL1(ROUTINE+NOUPPAID)                                            
         DC    AL2(USRP-T31117)                                                 
*                                                                               
         DC    CL6'POST',AL1(POST),AL1(2)                                       
         DC    AL3(VALPOST-T31117)                                              
         DC    AL1(L'POSTD)                                                     
         DC    AL2(POSTD-TEMPD)                                                 
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKPDT-NPRECD)                                              
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPOST-T31117)                                                
*                                                                               
         DC    CL6'HADJ',AL1(HADJ),AL1(3)                                       
         DC    AL3(VALHADJ-T31117)                                              
         DC    AL1(L'HUADJ)                                                     
         DC    AL2(HUADJ-TEMPD)                                                 
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKHPCT-NPRECD)                                             
         DC    AL1(DATADISP)                                                    
         DC    AL2(NUHUTPCT-NURECD)                                             
*                                                                               
         DC    CL6'HTYPE',AL1(HTYP),AL1(2)                                      
         DC    AL3(VALHTYP-T31117)                                              
         DC    AL1(L'HUTYP)                                                     
         DC    AL2(HUTYP-TEMPD)                                                 
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKHTYP-NPRECD)                                             
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UHTYPE-T31117)                                               
*                                                                               
         DC    CL6'HAVG',AL1(HAVG),AL1(3)                                       
         DC    AL3(VALHTAVG-T31117)                                             
         DC    AL1(L'HUTAVG)                                                    
         DC    AL2(HUTAVG-TEMPD)                                                
         DC    AL1(DATADISP)                                                    
         DC    AL2(NPAKHUTA-NPRECD)                                             
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UHTAVG-T31117)                                               
*                                                                               
         DC    CL6'BTYPE',AL1(BTYP),AL1(2)                                      
         DC    AL3(VALBTYP-T31117)                                              
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(NOUPDATE)                                                    
         DC    AL2(0)                                                           
         DC    AL1(ROUTINE)                                                     
         DC    AL2(UPBTYP-T31117)                                               
*                                                                               
KEYS     EQU   (*-KEYTAB)/KEYTABL                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EOFMSG   DC    C'** UPDATE COMPLETED - ENTER NEXT ACTION **'                    
NOUNITS  DC    C'** ERROR - NO UNITS IN PACKAGE **'                             
UNTFILE  DC    CL8'UNTFILE'                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* PACKAGE UPDATE SCREEN                                                         
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF6D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0D                                                               
SVLMODE  DS    X                   LAST TIME PROCESS MODE                       
SVPROG   DS    CL6                                                              
SVUNITS  DS    F                                                                
         DS    0D                                                               
SVNBLOCK DS    XL(NEBLOCKL)        SAVED NETBLOCK                               
SVINTTBL DS    CL70                STORE INTEG TABLES HERE                      
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
MYBASE   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    V                                                                
*                                                                               
PRGUNITS DS    F                                                                
*                                                                               
UPSW     DS    C                                                                
FRSTSW   DS    C                                                                
PACKUP   DS    C                                                                
*                                                                               
INTEG    DS    XL4                                                              
INTSTAT  DS    XL1                                                              
UNIVCODE DS    CL2                                                              
UNIVEL   DS    CL100                                                            
INTGBLK  DS    CL100               INTEGRATION BLOCK AREA                       
PNGUAR   DS    XL4                 NEW PACKAGE GUARANTEE                        
PGUAR    DS    XL2                                                              
DCAT     DS    X                                                                
DNGUAR   DS    XL4                 NEW DEMO GUARANTEE                           
DGUAR    DS    XL2                                                              
UNIVPCT  DS    XL2                                                              
PFILTR   DS    CL6                                                              
PAUDTOT  DS    0CL20                                                            
PAUDG    DS    CL4                                                              
PAUDC    DS    CL16                                                             
SPREP    DS    H                                                                
POSTD    DS    CL1                                                              
SVLSTIDA DS    A                   A(OF PREVIOUS INTRG REC DKA)                 
INTTBLSW DS    CL1                 USING TABLE INTEG                            
HUADJ    DS    H                   HUT PCT ADJ                                  
HUTYP    DS    CL1                 HUT TYPE123                                  
HUTAVG   DS    CL1                 HUT AVERAGE                                  
BUYTYPE  DS    CL1                 BUY TYPE                                     
*                                                                               
UNIVBLK  DS    CL(L'GUVBLOCK)                                                   
*                                                                               
UNBLK    DS    CL(L'UNBLOCK)       UNIT EDIT MAINTENANCE BLOCK                  
*                                                                               
KEYCOUNT DS    X                                                                
KEYSTACK DS    (KEYS)CL(KEYTABL)                                                
         SPACE 2                                                                
* DSECT TO COVER KEYWORD TABLE ENTRIES (KEYTAB)                                 
*                                                                               
KEYTABD  DSECT                                                                  
KEYNAME  DS    CL6                 KEYWORD                                      
KEYNUM   DS    X                   KEYWORD DATA NUMBER                          
KEYMINL  DS    X                   MININUM LENGTH OF INPUT                      
KEYAVAL  DS    AL3                 A(PARAMETER VALIDATION ROUTINE)              
KEYDLEN  DS    X                   PARAMETER DATA LENGTH                        
KEYDATA  DS    AL2                 DISP. TO DATA IN WORKING STORAGE             
KEYPKCTL DS    B                   CONTROL BITS FOR KEYPACK DATA                
*                                  X'80'=DISPLACEMENT TO ROUTINE                
*                                  X'40'=DISPLACEMENT INTO RECORD               
*                                  X'20'=KEY DOES NOT UPDATE PACKAGE            
*                                  X'10'=NO UPDATE PAID OR BILLED UNITS         
KEYPACK  DS    AL2                                                              
KEYUNCTL DS    B                   KEYUNIT CONTROL BITS (SEE KEYPKCTL)          
KEYUNIT  DS    AL2                                                              
KEYTABL  EQU   *-KEYTABD                                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SLASH    EQU   C'/'                                                             
EQUAL    EQU   C'='                                                             
ROUTINE  EQU   X'80'                                                            
DATADISP EQU   X'40'                                                            
NOUPDATE EQU   X'20'                                                            
NOUPPAID EQU   X'10'                                                            
PROCESS  EQU   X'01'                                                            
EOF      EQU   X'02'                                                            
*                                  DATA TYPE EQUATES                            
INT      EQU   1                   INTEGRATION                                  
UNIV     EQU   2                   UNIVERSE                                     
PG       EQU   3                   PACKAGE GUARANTEE FACTOR                     
DG       EQU   4                   DEMO GUARANTEE FACTOR                        
UPCT     EQU   5                   UNIVERSE PERCENT                             
PFLT     EQU   6                   PACKAGE FILTER                               
SREP     EQU   7                   SPECIAL REP                                  
POST     EQU   8                   POSTING DATA                                 
HADJ     EQU   9                   HUT PERCENT ADJUSEMENT                       
HTYP     EQU   10                  HUT TYPE                                     
HAVG     EQU   11                  HUT AVERAGE                                  
PAUDT    EQU   12                  PACKAGE AUDIT                                
BTYP     EQU   13                  BUY TYPE                                     
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NEGETNUND                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGETNUND                                                      
         PRINT ON                                                               
         SPACE 2                                                                
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NEINTGBLK                                                                     
*        PRINT OFF                                                              
       ++INCLUDE NEINTGBLK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041NEBUY17S  05/01/02'                                      
         END                                                                    
