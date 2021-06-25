*          DATA SET CTREP9802  AT LEVEL 014 AS OF 05/01/02                      
*PHASE CT9802A                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'CT9802 - DLBL/EXTENT GENERATOR'                                 
CT9802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*DEGEN*                                                        
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING CT9802+4096,RC                                                   
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    DG10                                                             
         CLI   MODE,REQFRST                                                     
         BE    DG15                                                             
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
* TEST TO CLOSE OUTPUT FILE                                                     
         CP    RUNERRS,=P'0'                                                    
         BE    DG2                                                              
         MVC   P(43),=C'** CT98 ** ERRORS PRESENT - DO NOT CONTINUE'            
         GOTO1 LOGIO,DMCB,1,(43,P)                                              
         GOTO1 REPORT                                                           
         CANCEL                                                                 
*                                                                               
DG2      DS    0H                                                               
         CLI   DG100+1,0                                                        
         BE    EXIT                                                             
         MVC   P(2),=C'/*'                                                      
         LA    R0,P                                                             
         LA    R1,CARDOUT                                                       
         PUT   (1),(0)                                                          
         LA    R1,CARDOUT                                                       
         CLOSER (1)                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
DG10     LA    R1,VSCANNER                                                      
         LA    R0,2                                                             
*                                                                               
DG12     LA    RE,RELO                                                          
         S     RE,RELO                                                          
         A     RE,0(R1)                                                         
         ST    RE,0(R1)                                                         
*                                                                               
         LA    R1,4(R1)                                                         
         BCT   R0,DG12                                                          
         MVC   PAGE,=H'1'                                                       
         B     EXIT                                                             
RELO     DC    A(*)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
DG15     DS    0H                                                               
         LA    R0,8                                                             
         L     R1,ASYSTAB                                                       
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   STDLBLSW,0                                                       
         MVI   ISW,0                                                            
         ZAP   SYSNUM,=P'0'                                                     
         CLC   QOPTIONS(3),SPACES  TEST STARTING SYSNUM SPECIFIED               
         BE    *+10                                                             
         PACK  SYSNUM,QOPTIONS(3)  SAVE IT                                      
         ZAP   ERRORS,=P'0'                                                     
         MVC   P(10),QSRTAREA      PRINT BOOK NAME                              
         GOTO1 REPORT                                                           
* READ BOOK                                                                     
         XC    BUFF(25),BUFF                                                    
         MVI   BUFF,C'L'                                                        
         MVC   BUFF+14(10),QSRTAREA                                             
         BAS   RE,GETCARD                                                       
         MVI   ERRCD,NOBOOK                                                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         CLC   =C' CATALP',CARD                                                 
         BNE   DG22                                                             
*                                                                               
DG20     BAS   RE,GETCARD                                                       
         TM    8(R1),X'80'         TEST E-O-F                                   
         BZ    DG22                                                             
         CLI   ISW,0               TEST IN 'INCLUDE' MODE                       
         BE    DG30                NO-DONE                                      
         MVI   ISW,0               YES-RESET                                    
         B     DG20                                                             
*                                                                               
DG22     CLI   CARD,C'I'                                                        
         BNE   DG24                                                             
         CLI   ISW,C'I'            TEST 'I' MODE ALREADY                        
         BE    DG20                YES-IGNORE                                   
         MVI   ISW,C'I'                                                         
* SET TO READ INCLUDED BOOK IN BUFF2                                            
         MVC   P(80),CARD                                                       
         GOTO1 REPORT                                                           
         XC    BUFF2(25),BUFF2                                                  
         MVI   BUFF2,C'L'                                                       
         MVC   BUFF2+14(10),CARD+2                                              
         BAS   RE,GETCARD                                                       
         MVI   ERRCD,NOINCL                                                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         CLC   =C' CATALP',CARD                                                 
         BE    DG20                                                             
         B     DG22                                                             
DG24     CLI   CARD,C'X'                                                        
         BNE   DG26                                                             
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(4,SCANBLK)                          
* EDIT SYSNUM                                                                   
         LA    R4,SCANBLK                                                       
         MVI   ERRCD,BADSYS                                                     
         CLI   0(R4),6                                                          
         BNE   ERROR                                                            
         CLC   =C'SYS',12(R4)                                                   
         BNE   ERROR                                                            
         MVC   DUB(3),=3C'0'                                                    
         MVZ   DUB(3),15(R4)                                                    
         CLC   DUB(3),=3C'0'                                                    
         BNE   ERROR                                                            
         PACK  DUB,15(3,R4)                                                     
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         CP    DUB,=P'60'                                                       
         BH    ERROR                                                            
         CVB   RE,DUB                                                           
         SLL   RE,3                X 8                                          
         A     RE,ASYSTAB                                                       
         MVI   ERRCD,DUPSYS                                                     
         CLC   0(6,RE),SCANBLK+44  TEST SAME VOLID                              
         BE    *+14                YES                                          
         OC    0(6,RE),0(RE)                                                    
         BNZ   ERROR                                                            
         MVC   0(6,RE),SCANBLK+44  MOVE VOLID FROM 2ND SCANBLK ENTRY            
* EDIT VOLID                                                                    
         LA    R4,32(R4)                                                        
         MVI   ERRCD,BADVOLID                                                   
         CLI   0(R4),6                                                          
         BNE   ERROR                                                            
* EDIT START TRACK                                                              
         LA    R4,32(R4)                                                        
         MVI   ERRCD,BADTRK                                                     
         OC    4(4,R4),4(R4)                                                    
         BZ    ERROR                                                            
* EDIT NUMBER OF TRACKS                                                         
         LA    R4,32(R4)                                                        
         MVI   ERRCD,BADNTRKS                                                   
         OC    4(4,R4),4(R4)                                                    
         BZ    ERROR                                                            
         MVC   P+10(80),CARD                                                    
         GOTO1 REPORT                                                           
         B     DG20                                                             
DG26     CLC   =C'// DLBL',CARD                                                 
         BE    DG20                                                             
         CLI   CARD,C'D'                                                        
         BNE   DG28                                                             
* EDIT 'D' CARD TO SAVE FILE NAME                                               
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(1,SCANBLK)                          
         MVC   P(10),SCANBLK+12                                                 
         B     DG20                                                             
*                                                                               
DG28     CLC   =C'$/ EXTENT',CARD                                               
         BNE   DG29                                                             
* CHECK FOR NUMERIC SYSNUM AND SAVE HIGHEST                                     
         MVC   DUB(3),=3C'0'                                                    
         MVZ   DUB(3),CARD+13                                                   
         CLC   DUB(3),=3C'0'                                                    
         BNE   DG20                                                             
         PACK  DUB,CARD+13(3)                                                   
         CP    DUB,SYSNUM                                                       
         BNH   *+10                                                             
         ZAP   SYSNUM,DUB                                                       
         B     DG20                                                             
*                                                                               
DG29     DS    0H                                                               
         CLC   =C'// PAUSE',CARD                                                
         BE    DG20                                                             
         CLC   =C'// OPTION STDLABEL',CARD                                      
         BNE   *+8                                                              
         MVI   STDLBLSW,C'Y'                                                    
*                                                                               
         CLC   =C'//',CARD                                                      
         BE    DG20                                                             
*                                                                               
         MVI   ERRCD,BADCARD                                                    
         B     ERROR                                                            
         SPACE 2                                                                
GETCARD  LR    R0,RE                                                            
         LA    RE,BUFF                                                          
         CLI   ISW,C'I'                                                         
         BNE   *+8                                                              
         LA    RE,BUFF2                                                         
         ST    RE,DMCB                                                          
         GOTO1 GETBOOK,DMCB,,CARD,DATAMGR                                       
         MVC   CARD+72(10),SPACES                                               
         LR    RE,R0                                                            
         CLC   CARD(80),SPACES                                                  
         BE    GETCARD             IGNORE BLANK CARDS                           
         BR    RE                                                               
         EJECT                                                                  
* ALL EXTENT CARDS NOW READ                                                     
* ASSIGN NEW SYS NUMBERS                                                        
*                                                                               
DG30     DS    0H                                                               
         CP    ERRORS,=P'0'                                                     
         BE    DG31                                                             
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(20),=C'** ERRORS PRESENT **'                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
DG31     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
* STARTING SYSNUM IS HIGHER OF SYS010 OR                                        
* 1 MORE THAN HIGHEST HARD-ASSIGNED SYSNUM                                      
         CP    SYSNUM,=P'9'                                                     
         BH    *+10                                                             
         ZAP   SYSNUM,=P'9'                                                     
         AP    SYSNUM,=P'1'                                                     
         L     RE,ASYSTAB                                                       
         LA    R0,256                                                           
DG32     OC    0(6,RE),0(RE)                                                    
         BZ    DG34                                                             
         ZAP   6(2,RE),SYSNUM                                                   
         AP    SYSNUM,=P'1'                                                     
DG34     LA    RE,8(RE)                                                         
         BCT   R0,DG32                                                          
         SPACE 2                                                                
* NOW SET UP TO RE-READ BOOK AND GENERATE OUTPUT                                
*                                                                               
         MVI   BUFF+24,0           RESET SUB-REF                                
         MVI   PROCSW,0                                                         
         BAS   RE,GETCARD                                                       
         CLC   =C' CATALP',CARD                                                 
         BNE   DG40                                                             
         MVI   PROCSW,C'P'                                                      
         B     DG100                                                            
*                                                                               
DG40     CLI   CARD,C'D'                                                        
         BNE   DG50                                                             
* DO SCAN TO FIND NUMBER OF FIELDS                                              
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(2,SCANBLK)                          
         ZAP   XTNTNUM,=P'0'                                                    
         LA    R1,PSECOND                                                       
         MVC   0(7,R1),=C'// DLBL'                                              
         MVC   8(70,R1),CARD+2     CARD HAS 72 DATA COLS                        
         LA    R1,77(R1)           POINT TO LAST DATA COL                       
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         CLI   DMCB+4,1            TEST ONLY ONE FIELD PRESENT                  
         BNE   *+12                                                             
         MVI   1(R1),C','          NEED AN EXTRA COMMA                          
         LA    R1,1(R1)                                                         
         MVC   1(10,R1),=C',99/365,DA'                                          
         B     DG100                                                            
*                                                                               
DG50     CLI   CARD,C'X'                                                        
         BNE   DG60                                                             
* GENERATE EXTENT                                                               
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(8,SCANBLK)                          
         LA    R4,SCANBLK                                                       
         LA    R5,PSECOND                                                       
         MVC   0(80,R5),SPACES                                                  
         MVC   0(17,R5),=C'// EXTENT SYS000,'                                   
         PACK  DUB,CARD+5(3)       GET SYSNUM                                   
         CVB   RE,DUB                                                           
         SLL   RE,3                                                             
         A     RE,ASYSTAB                                                       
         OI    7(RE),X'0F'                                                      
         UNPK  13(3,R5),6(2,RE)    UNPACK NEW SYSNUM                            
*                                                                               
         LA    R4,32(R4)                                                        
         LA    R5,17(R5)                                                        
         MVC   0(6,R5),12(R4)      VOLID                                        
         MVI   6(R5),C','                                                       
*                                                                               
         LA    R5,7(R5)                                                         
         MVC   0(4,R5),=C'1,0,'    EXTENT TYPE/SEQ                              
         OI    XTNTNUM+1,X'0F'                                                  
         UNPK  2(1,R5),XTNTNUM                                                  
         AP    XTNTNUM,=P'1'                                                    
*                                                                               
         LA    R4,32(R4)                                                        
         LA    R5,4(R5)                                                         
         L     R0,4(R4)            START TRK                                    
         EDIT  (R0),(5,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         L     R0,4(R4)                                                         
         LA    R2,PSECOND+58                                                    
         BAS   RE,DGCCHH                                                        
*                                                                               
         LA    R4,32(R4)                                                        
         L     R0,4(R4)            NUMBER OF TRACKS                             
         EDIT  (R0),(5,(R5)),ALIGN=LEFT                                         
*                                                                               
         L     R0,4(R4)                                                         
         SH    R4,=H'32'                                                        
         A     R0,4(R4)                                                         
         BCTR  R0,0                                                             
         MVI   PSECOND+64,C'-'                                                  
         LA    R2,PSECOND+65                                                    
         BAS   RE,DGCCHH                                                        
         B     DG100                                                            
*                                                                               
DG60     CLI   CARD,C'I'                                                        
         BNE   DG70                                                             
         CLI   ISW,C'I'            TEST 'I' MODE ALREADY                        
         BE    DG112               YES - IGNORE NESTED INCLUDES                 
         MVI   ISW,C'I'            SET MODE SWITCH                              
* SET TO READ INCLUDE                                                           
         XC    BUFF2(25),BUFF2                                                  
         MVI   BUFF2,C'L'                                                       
         MVC   BUFF2+14(10),CARD+2                                              
         BAS   RE,GETCARD                                                       
         CLC   =C' CATALP',CARD    IGNORE CATALP CARDS IN INCLUDES              
         BE    *-10                                                             
         B     DG40                ELSE PROCESS CARD                            
*                                                                               
DG70     CLC   =C'// DLBL',CARD                                                 
         BNE   DG80                                                             
         ZAP   XTNTNUM,=P'0'                                                    
         B     DG100                                                            
*                                                                               
DG80     CLC   =C'$/ EXTENT',CARD                                               
         BNE   *+8                                                              
         MVI   CARD,C'/'                                                        
*                                                                               
         CLC   =C'// EXTENT',CARD                                               
         BNE   DG90                                                             
         GOTO1 VSCANNER,DMCB,(C'C',CARD+10),(8,SCANBLK)                         
         LA    R4,SCANBLK+128      POINT TO FIELD 5 (START TRACK)               
         OC    4(4,R4),4(R4)                                                    
         BZ    DG100                                                            
         OC    36(4,R4),36(R4)     TEST NUM TRKS                                
         BZ    DG100                                                            
         L     R0,4(R4)                                                         
         LA    R2,CARD+58                                                       
         BAS   RE,DGCCHH                                                        
         A     R0,36(R4)                                                        
         BCTR  R0,0                                                             
         MVI   CARD+64,C'-'                                                     
         LA    R2,CARD+65                                                       
         BAS   RE,DGCCHH                                                        
         B     DG100                                                            
*                                                                               
DG90     CLC   =C'// ',CARD                                                     
         BE    DG100                                                            
         DC    H'0'                 UNKNOWN CARD                                
         EJECT                                                                  
DG100    BC    0,DG105                                                          
         OI    *-3,X'F0'                                                        
         LA    R1,CARDOUT                                                       
         OPENR (1)                                                              
*                                                                               
DG105    DS    0H                                                               
         LA    R0,PSECOND                                                       
         CLC   PSECOND,SPACES                                                   
         BNE   *+8                                                              
         LA    R0,CARD                                                          
         LA    R1,CARDOUT                                                       
         PUT   (1),(0)                                                          
*                                                                               
DG110    MVC   P(10),BUFF+14       BOOK NAME                                    
         CLI   ISW,C'I'                                                         
         BNE   *+10                                                             
         MVC   P(10),BUFF2+14                                                   
         MVC   P+11(40),CARD                                                    
         MVC   P+52(80),PSECOND                                                 
         CLC   PSECOND,SPACES      IF EQUAL, NO DATA WAS GENERATED              
         BNE   *+14                IE - CARD WAS A PASS-THROUGH                 
         MVI   P+51,C'='           PRINT A FLAG                                 
         MVC   P+52(80),CARD       AND PRINT CARD ON RIGHT TOO                  
         MVC   PSECOND,SPACES                                                   
         GOTO1 REPORT                                                           
*                                                                               
DG112    BAS   RE,GETCARD                                                       
         TM    8(R1),X'80'                                                      
         BO    DG114                                                            
         CLI   8(R1),0                                                          
         BE    DG40                                                             
         DC    H'0'                                                             
*                                                                               
DG114    CLI   ISW,C'I'            TEST 'I' MODE E-O-F                          
         BNE   *+12                NO - REAL E-O-F                              
         MVI   ISW,0               RESET SWITCH                                 
         B     DG112               AND CONTINUE                                 
         EJECT                                                                  
* GENERATE ASSGN STMTS                                                          
* UNLESS STANDARD LABELS ARE BEING GENERATED                                    
         CLI   STDLBLSW,C'Y'                                                    
         BE    EXIT                                                             
         L     R4,ASYSTAB                                                       
         LA    R5,256                                                           
DG122    OC    0(6,R4),0(R4)                                                    
         BZ    DG124                                                            
         MVC   PSECOND(35),=C'// ASSGN SYS000,DISK,VOL=DVS100,SHR'              
         OI    7(R4),X'0F'                                                      
         UNPK  PSECOND+12(3),6(2,R4)                                            
         MVC   PSECOND+25(6),0(R4)                                              
         LA    R0,PSECOND                                                       
         LA    R1,CARDOUT                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   P+52(80),PSECOND                                                 
         MVC   PSECOND,SPACES                                                   
         GOTO1 REPORT                                                           
*                                                                               
DG124    LA    R4,8(R4)                                                         
         BCT   R5,DG122                                                         
*                                                                               
         CLI   PROCSW,C'P'                                                      
         BNE   DG130                                                            
* NEED /+ TO END PROC                                                           
         MVC   PSECOND(2),=C'/+'                                                
         LA    R0,PSECOND                                                       
         LA    R1,CARDOUT                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   P+52(80),PSECOND                                                 
         MVC   PSECOND,SPACES                                                   
         GOTO1 REPORT                                                           
*                                                                               
DG130    B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO CONVERT RELATIVE TRACK TO CCHH FOR DENNIS BROOME                   
*                                                                               
DGCCHH   NTR1                                                                   
         LA    RE,12               ASSUME 3340                                  
* FOLLOWING CODE ASSUMES SOME METHOD IN ASSIGNING VOLID'S                       
         LA    R4,SCANBLK+32       POINT TO VOLID                               
         CLI   12(R4),C'D'                                                      
         BNE   DGCCHH2                                                          
         CLI   14(R4),C'5'                                                      
         BNE   *+8                                                              
         LA    RE,30               ASSUME 3350                                  
*                                                                               
DGCCHH2  DS    0H                                                               
         SRDA  R0,32                                                            
         DR    R0,RE                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
         MVI   3(R2),C'/'                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R2),DUB                                                      
         B     EXIT                                                             
         EJECT                                                                  
NOBOOK   EQU   1                                                                
NOXTNT   EQU   2                                                                
BADCARD  EQU   3                                                                
BADOVSYS EQU   4                                                                
BADSYS   EQU   5                                                                
BADVOLID EQU   6                                                                
DUPSYS   EQU   7                                                                
BADTRK   EQU   8                                                                
BADNTRKS EQU   9                                                                
NOINCL   EQU   10                                                               
         SPACE 2                                                                
ERROR    MVC   P+10(80),CARD                                                    
         MVC   PSECOND,SPACES                                                   
         GOTO1 REPORT                                                           
         MVC   P+60(11),=C'** ERROR **'                                         
         LA    R4,ERRTAB                                                        
ERROR2   CLC   0(1,R4),ERRCD                                                    
         BE    ERROR4                                                           
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'FF'                                                      
         BNE   ERROR2                                                           
         DC    H'0'                                                             
ERROR4   ZIC   R5,1(R4)            GET LEN                                      
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+72(0),2(R4)                                                    
         GOTO1 REPORT                                                           
         AP    ERRORS,=P'1'                                                     
         AP    RUNERRS,=P'1'                                                    
         B     DG20                PROCESS NEXT CARD                            
         EJECT                                                                  
ERRTAB   DS    0C                                                               
ERR1     DC    AL1(NOBOOK),AL1(ERR2-ERR1)                                       
         DC    C'BOOK NOT FOUND'                                                
ERR2     DC    AL1(NOXTNT),AL1(ERR3-ERR2)                                       
         DC    C'MISSING EXTENT STATEMENT'                                      
ERR3     DC    AL1(BADCARD),AL1(ERR4-ERR3)                                      
         DC    C'INVALID CARD TYPE'                                             
ERR4     DC    AL1(BADOVSYS),AL1(ERR5-ERR4)                                     
         DC    C'INVALID SYSNUM OVERRIDE'                                       
ERR5     DC    AL1(BADSYS),AL1(ERR6-ERR5)                                       
         DC    C'TOO MANY SYS NUMBERS GENERATED'                                
ERR6     DC    AL1(BADVOLID),AL1(ERR7-ERR6)                                     
         DC    C'VOLID NOT 6 CHARACTERS'                                        
ERR7     DC    AL1(DUPSYS),AL1(ERR8-ERR7)                                       
         DC    C'SYSNUM HAS TWO DIFFERENT VOL IDS'                              
ERR8     DC    AL1(BADTRK),AL1(ERR9-ERR8)                                       
         DC    C'START TRACK NOT VALID'                                         
ERR9     DC    AL1(BADNTRKS),AL1(ERR10-ERR9)                                    
         DC    C'NUMBER OF TRACKS NOT VALID'                                    
ERR10    DC    AL1(NOINCL),AL1(ERR11-ERR10)                                     
         DC    C'INCLUDE NOT FOUND'                                             
ERR11    EQU   *                                                                
*                                                                               
ERRX     DC    X'FF'                                                            
         EJECT                                                                  
CARDOUT  DTFMT BLKSIZE=80,RECFORM=FIXUNB,FILABL=NO,DEVADDR=SYS005,     X        
               IOAREA1=CARDIO,TYPEFLE=OUTPUT,WORKA=YES                          
CARDIO   DS    CL80                                                             
         SPACE 2                                                                
SYSNUM   DC    PL2'0'                                                           
XTNTNUM  DC    PL2'0'                                                           
ERRORS   DC    PL2'0'                                                           
RUNERRS  DC    PL2'0'                                                           
ERRCD    DS    C                                                                
ISW      DS    C                                                                
PROCSW   DS    C                                                                
STDLBLSW DS    C                                                                
* ADCONS BELOW ARE RELOCATED AT RUNFRST                                         
VSCANNER DC    V(SCANNER)                                                       
ASYSTAB  DC    A(SYSTAB)                                                        
*                                                                               
CARD     DC    CL160' '                                                         
SCANBLK  DS    8CL32                                                            
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
BUFF     DS    1112C                                                            
         DS    0D                                                               
         DC    C'* IBUFF*'                                                      
BUFF2    DS    1112C                                                            
         DS    0D                                                               
SYSTAB   DS    256CL8                                                           
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
 END                                                                            
