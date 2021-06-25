*          DATA SET NERES07    AT LEVEL 025 AS OF 01/30/06                      
*PHASE T32107A,*                                                                
*INCLUDE NETUNBK                                                                
T32107   TITLE '-   CORRECTED PROGRAMS LISTING'                                 
T32107   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COED**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDR                    
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   GETBRD,DMCB         SAVE MODULE ADDRESS                          
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 3                                                                
VKEY     EQU   *                                                                
         LA    R2,CORSRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'40'        ALLOW ALL NTI                                
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,CORBOOKH         VALIDATE BOOK                                
         MVI   MAX,8                                                            
         GOTO1 VVALBOOK                                                         
         SPACE 1                                                                
VKEY10   LA    R2,CORNETH          VALIDATE NETWORKS                            
         LA    R3,8                MAXIMUM=8                                    
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         LA    R4,7(R4)            NEXT ENTRY IN NETSAVE                        
VKEY30   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY20                                                           
         B     VKEY30                                                           
         SPACE 1                                                                
VKEY40   LA    R2,COROPTNH         VALIDATE OPTIONS                             
         BAS   RE,VALOPT                                                        
         LA    R2,CORTITLH         SET USER'S OWN NAME                          
         GOTO1 VVALTITL                                                         
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 2                                                                
VALOPT   NTR1                                                                   
         MVI   GAP,1               INITIALIZE GAP                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOPT                                                           
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
OPT2     EQU   *                                                                
OPT6     CLC   12(2,R3),=C'S  '    S=2 IS ONLY OPTION CURRENTLY                 
         BNE   OPT10                                                            
         MVI   GAP,2                                                            
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT10    B     BADOPT                                                           
         SPACE 1                                                                
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
         EJECT                                                                  
*              PRINT REPORT - INITIALIZE                                        
         SPACE 3                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   RESTITLE,=CL40'CORRECTED PROGRAMS LISTING'                       
*                                                                               
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         MVI   DBFUNCT,DBGETNTI                                                 
         XC    DBSELTIM,DBSELTIM                                                
         MVI   DBSELDUR,X'FF'      ALL DURATIONS (INCL 15 MIN & UNDER)          
*                                                                               
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 3                                                                
CONIO    EQU   *                                                                
         LA    R2,BOOKS            R2 - ORIGINATING BOOK(S)                     
CIO10    MVC   WORKBOOK,1(R2)                                                   
CIO15    LA    R3,NETSAVE          R3 - NETWORK(S)                              
         MVC   CURRSTA,0(R3)                                                    
         CLC   CURRSTA(4),=C'ZZZ ' ALL STATIONS                                 
         BNE   *+10                                                             
         MVC   CURRSTA(4),=C'A   '                                              
         SPACE 2                                                                
CIO20    LA    R6,KEE                                                           
         XC    KEE,KEE                                                          
         USING CHKEY,R6                                                         
         MVI   CHCODE,CHCODEQU     -C-                                          
         MVI   CHMEDIA,C'N'        -N-                                          
         MVI   CHSRC,C'N'          -N-                                          
         MVC   CHOBOOK,WORKBOOK    ORIGINATING BOOK                             
         MVC   CHSTAT,CURRSTA      NETWORK                                      
         MVI   CHSTAT+4,C'T'                                                    
         MVC   CHBTYP,CURRSTA+4    BOOK TYPE                                    
         CLI   CURRSTA+4,C'S'      SYNDICATORS                                  
         BNE   *+12                                                             
         MVI   CHBTYP,C'A'                                                      
         MVI   CHSTAT+4,C'S'                                                    
*                                                                               
         CLI   CHBTYP,C'A'         NETWORK BOOK TYPES                           
         BH    *+8                                                              
         MVI   CHBTYP,C'A'                                                      
CIO28    BAS   RE,HI                                                            
         B     CIO32                                                            
*                                                                               
CIO30    BAS   RE,SQ                                                            
CIO32    BNE   CIO60                                                            
*                                                                               
CIO40    CLC   KEE(CHDW-CHKEY),KEESAVE TEST THRU STATION AND BOOK TYPE          
         BNE   CIO60                                                            
         CLI   CHRTYP,PMCODEQU     -Q- POINTERS ONLY                            
         BNE   CIO30                                                            
         CLC   DBSELPRG,CHPNUM     ONLY 1 LINE PER PROGRAM                      
         BE    CIO30                                                            
*                                                                               
CIO50    EQU   *                   KEY FOUND                                    
         MVC   DBSELSTA,CHSTAT                                                  
         MVC   DBSELBK,CHRBOOK                                                  
         MVC   DBSELPRG,CHPNUM                                                  
         MVC   DBBTYPE,CHBTYP                                                   
         MVI   DBMODE,DBMDSEQ                                                   
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL                                         
*                                                                               
         ZIC   RE,KEE+22                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEE+22                                                        
         B     CIO28               USE READ HIGH                                
*                                                                               
         EJECT                                                                  
CIO60    EQU   *                   TEST FOR MORE BOOKS AND/OR NETWORKS          
         CLC   0(5,R3),=C'ZZZ S'   ALL SYNDICATORS                              
         BNE   CI61                                                             
         LA    R6,KEE                                                           
         XC    KEE,KEE                                                          
         USING CHKEY,R6                                                         
         MVI   CHCODE,CHCODEQU     -C-                                          
         MVI   CHMEDIA,C'N'        -N-                                          
         MVI   CHSRC,C'N'          -N-                                          
         MVC   CHOBOOK,WORKBOOK    ORIGINATING BOOK                             
         MVC   CHSTAT,CURRSTA      NETWORK                                      
         MVI   CHSTAT+4,X'FF'                                                   
         MVI   CHBTYP,X'FF'        BOOK TYPE                                    
         BAS   RE,HI                                                            
         BNE   CI61                                                             
         CLC   KEE(CHSTAT-CHKEY),KEESAVE                                        
         BNE   CI61                                                             
         MVC   CURRSTA,KEE+CHSTAT-CHKEY                                         
         MVI   CURRSTA+4,C'S'                                                   
         B     CIO20                                                            
         SPACE 2                                                                
CI61     LA    R3,7(R3)                                                         
         MVC   CURRSTA,0(R3)                                                    
         CLI   0(R3),0             TEST NETWORK                                 
         BNE   CIO20                                                            
*                                                                               
         TM    0(R2),X'01'         TEST RANGE OF BOOKS                          
         BNO   CIO80                                                            
         CLC   WORKBOOK,4(R2)      HAS RANGE BEEN DEPLETED                      
         BE    CIO70                                                            
         BAS   RE,BUMPWEEK                                                      
         B     CIO15               NEXT BOOK IN RANGE                           
*                                                                               
CIO70    LA    R2,3(R2)            POINT PAST RANGE                             
*                                                                               
CIO80    LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)       TEST NEXT BOOK ENTRY                         
         BNE   CIO10               NEXT BOOK                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A RECORD                                                 
         SPACE 1                                                                
FILL     NTR1                                                                   
         LA    R3,BUFF                                                          
         MVC   0(110,R3),SPACES                                                 
         MVC   110(110,R3),SPACES                                               
*                                                                               
*                                                                               
*        GOTO1 NETUNBK,DMCB,(C'W',PDSBOOK),DUB,GETDAY,ADDAY,GETBRD              
*                                                                               
         PRINT GEN                                                              
         GOTO1 =V(NETUNBK),DMCB,(C'W',CHOBOOK),WORK,GETDAY,ADDAY,GETBRDX        
               ,RR=RELO                                                         
         PRINT NOGEN                                                            
         MVC   MONDATE,WORK                                                     
         GOTO1 DATCON,DMCB,(0,MONDATE),(8,WORK)                                 
         CLC   LASTBOOK,WORK       DON'T REPEAT ORIGINATING BOOK                
         BE    FILL20                                                           
         MVC   LASTBOOK,WORK                                                    
*                                  ---ORIGINATING BOOK---                       
         MVC   00(8,R3),WORK                                                    
*                                                                               
FILL20   GOTO1 VDISPNET                                                         
         CLI   WORK+3,C'A'         CHANGE ASCRIBED TO BLANK                     
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
*                                  ---NETWORK---                                
         MVC   12(4,R3),WORK                                                    
*                                                                               
         GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+2       EXTRACT START TIME                           
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,(R1),DUB,WORK                                             
*                                  ---START TIME---                             
         MVC   36(6,R3),WORK                                                    
*                                                                               
         CLI   COROPT,C'Y'         TEST NEED FOR CORRECTION IND.                
         B     FILL30              (BYPASS FOR NOW)                             
         GOTO1 DEFINE,PARAS,=C'CORR',DBLOCKD,WORK                               
         CLC   WORK(16),SPACES                                                  
         BE    FILL30                                                           
         MVI   26(R3),C'A'         ADD                                          
         TM    WORK,X'01'                                                       
         BNO   FILL30                                                           
         MVI   26(R3),C'C'         CHANGE                                       
*                                                                               
FILL30   DS    0H                                                               
**       GOTO1 NETUNWK,DMCB,CHRBOOK,WORK,GETDAY,ADDAY                           
*                                                                               
         GOTO1 =V(NETUNBK),DMCB,(C'W',CHRBOOK),WORK,GETDAY,ADDAY,GETBRDX        
               ,RR=RELO                                                         
         MVC   MONDATE,WORK                                                     
         GOTO1 DEFINE,PARAS,=C'DAY',DBLOCKD,WORK                                
*                                  ---3 BYTE ALPHA DAY---                       
         MVC   32(3,R3),WORK+2                                                  
*                                                                               
         ZIC   R2,WORK+1                                                        
         LTR   R2,R2               0 = M-F                                      
         BZ    *+12                                                             
         CH    R2,=H'8'            8 = M-S                                      
         BL    *+8                                                              
         LA    R2,1                M-F OR M-S = MON                             
         BCTR  R2,0                CHANGE TO 0 THRU 6                           
         GOTO1 ADDAY,DMCB,MONDATE,WORK,(R2)                                     
         MVC   MONDATE,WORK                                                     
         GOTO1 DATCON,DMCB,(0,MONDATE),(8,WORK)                                 
*                                  ---REVISION DATE---                          
         MVC   17(8,R3),WORK                                                    
*                                                                               
         GOTO1 DEFINE,PARAS,=C'NTI',DBLOCK,WORK                                 
*                                  ---NTI CODE---                               
         MVC   26(5,R3),WORK                                                    
*                                                                               
FILL40   GOTO1 (RF),(R1),=C'PROGRAM',DBLOCKD,WORK                               
*                                  ---PROGRAM NAME---                           
         MVC   42(16,R3),WORK                                                   
         GOTO1 (RF),(R1),=C'EPISODE',DBLOCKD,WORK                               
         CLC   WORK(16),SPACES                                                  
         BE    FILL50                                                           
*                                  ---EPISODE TITLE---                          
         MVC   110+42(16,R3),WORK  (ON 2ND LINE)                                
         SPACE 1                                                                
FILL50   EQU   *                                                                
         MVC   P(110),0(R3)                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         CLC   110(110,R3),SPACES                                               
         BE    FILL60                                                           
         MVC   P(110),110(R3)                                                   
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
FILL60   CLI   GAP,2                                                            
         BNE   PREPX                                                            
         MVC   P(110),SPACES                                                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 1                                                                
PREPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT NEXT NTI WEEK NO.                          
         SPACE 3                                                                
BUMPWEEK NTR1                                                                   
         LA    R1,BUMPLIST                                                      
         SPACE 1                                                                
BUMPW2   CLC   0(1,R1),WORKBOOK+1  LOOK UP WEEK IN LIST                         
         BE    BUMPW4                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   BUMPW2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
BUMPW4   MVC   WORKBOOK+1(1),1(R1)                                              
         CLI   WORKBOOK+1,1        HAVE WE STARTED A NEW YEAR                   
         BNE   BUMPW6                                                           
         ZIC   RF,WORKBOOK         THEN BUMP THE YEAR                           
         LA    RF,1(RF)                                                         
         STC   RF,WORKBOOK                                                      
         SPACE 1                                                                
BUMPW6   CLI   ALTOPT,C'Y'         ALTERNATING OPTION                           
         BNE   XIT                                                              
         CLC   WORKBOOK,4(R2)      MAKE SURE WE DON'T GO PAST END               
         BE    XIT                                                              
         MVC   WORKBOOK+1(1),2(R1)                                              
         CLI   WORKBOOK+1,1        HAVE WE STARTED A NEW YEAR                   
         BNE   XIT                                                              
         ZIC   RF,WORKBOOK         THEN BUMP THE YEAR                           
         LA    RF,1(RF)                                                         
         STC   RF,WORKBOOK                                                      
         B     XIT                                                              
         SPACE 2                                                                
BUMPLIST DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,49)                   
         DC    AL1(17,18,19,20,21,22,23,24,50)                                  
         DC    AL1(25,26,27,28,29,30,31,32,51)                                  
         DC    AL1(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52)          
         DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,49)                   
         DC    AL1(17,18,19,20,21,22,23,24,50)                                  
         DC    AL1(25,26,27,28,29,30,31,32,51)                                  
         DC    AL1(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52)          
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*              FILE I/O ROUTINES                                                
*                                                                               
HI       NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         MVC   KEESAVE,KEE                                                      
         B     IO20                                                             
*                                                                               
SQ       NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
         MVC   KEESAVE,KEE                                                      
*                                                                               
IO20     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(1,COMMAND),NTIDIR,KEE,IO                           
         CLI   8(R1),0             TEST EOF OR NOT FOUND                        
         BNE   IOEXIT                                                           
         MVC   KEE,IO                                                           
         B     IOEXIT              RETURN FOR CHKEY                             
*                                                                               
         OC    KEE+19(4),KEY+19    TEST IF INDEX D/A PRESENT                    
         BZ    IOEXIT                                                           
*                                                                               
         XC    IO(L'KEE),IO                                                     
         MVC   IO(18),KEY                                                       
         GOTO1 DATAMGR,DMCB,(1,COMMAND),NTIFIL,KEE+19,IO                        
         CLI   8(R1),0             SET CC                                       
*                                                                               
IOEXIT   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*              HEADLINE HOOK - CONTROL                                          
         SPACE 3                                                                
HOOK     NTR1                                                                   
         BAS   RE,HOOKHEAD                                                      
         GOTO1 VRESHEAD                                                         
*                                  PRINT COPYRIGHT INFO                         
         MVC   WORK(40),=CL40'RTG/IMP-(C) YYYY NIELSEN MEDIA'                   
         GOTO1 DATCON,DMCB,(5,DUB),(20,DUB)                                     
         MVC   WORK+12(4),DUB      PUT CURRENT YEAR HERE                        
         GOTO1 CENTER,DMCB,WORK,40 CENTERED                                     
         LA    R2,H4+30                                                         
         CLI   HOWWIDE,132         MAY BE WIDE PRINT                            
         BNE   *+8                                                              
         LA    R2,H4+40                                                         
         MVC   0(40,R2),WORK                                                    
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
*              HOOK - DEAL WITH HEADINGS                                        
         SPACE 3                                                                
HOOKHEAD NTR1                                                                   
         MVC   P(8),LASTBOOK       FORCE ORIGINATING BOOK AT TOP                
         MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
*                                                                               
         MVC   RESTITA(L'HEAD01),HEAD01                                         
         MVC   RESTITB(L'HEAD02),HEAD02                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
         SPACE 3                                                                
*              CONSTANTS                                                        
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION '                                   
         SPACE 1                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              LITERAL POOL                                                     
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
*              CONSTANTS                                                        
*                                                                               
NTIDIR   DC    C'NTIDIR  '                                                      
NTIFIL   DC    C'NTIFIL  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMOPEN   DC    C'DMOPEN  '                                                      
*                                                                               
HEAD01   DC    C'CORRECTION  NET. REVISED  NTI.  DAY TIME  PROGRAM'             
HEAD02   DC    C'   BOOK           DATE     NO.                   '             
*                                                                               
         EJECT                                                                  
*              DEDEMFILE                                                        
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*              NERESALL                                                         
         PRINT OFF                                                              
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*              DSECT TO COVER SCREEN                                            
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF7D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
KEE      DS    CL23                                                             
KEESAVE  DS    CL23                                                             
*                                                                               
CURRSTA  DS    CL5                                                              
WORKBOOK DS    XL2                                                              
MONDATE  DS    CL6                                                              
GETBRD   DS    V                                                                
LASTBOOK DC    CL8' '                                                           
*                                                                               
DUMMY    DS    CL2                                                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NERES07   01/30/06'                                      
         END                                                                    
