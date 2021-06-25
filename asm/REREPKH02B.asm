*          DATA SET REREPKH02B AT LEVEL 048 AS OF 05/01/02                      
*PHASE REKH02B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPKH02 (REKH02) - FIND KATZ CONFIRMED ORDERS'                
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPKH02 -- FIND KATZ CONFIRMED ORDERS                   *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* TEMP VERSION                                                     *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =  STATION DISPLAY                               *            
*     QUESTOR+1   =  OFFICE DISPLAY                                *            
*     QUESTOR+2   =  AGENCY DISPLAY                                *            
*     QUESTOR+3   =  ADVERTISER DISPLAY                            *            
*     QUESTOR+4   =  SALESPERSON DISPLAY                           *            
*     QUESTOR+5   =  PRODUCT DISPLAY                               *            
*     QUESTOR+6   =  CONTRACT DISPLAY                              *            
*     QUESTOR+7   =  BUY DISPLAY                                   *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKH02   CSECT                                                                  
         NMOD1 0,**RESW**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
*                                                                               
*        GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1800000,4000000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   AGRPAREA,P2         A(GROUP DIFF TABLE)                          
         MVC   ANEXTGRP,P2         A(NEXT GROUP DIFF SLOT)                      
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         LA    RF,200(RF)                                                       
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
         A     RF,=F'1200'         LEAVE ROOM FOR 200 ENTRIES                   
*                                     6 CHARS * 200 SLOTS                       
         ST    RF,ACOMAREA         A(COMMISSION/BUDGET TABLE)                   
         ST    RF,ANEXTCOM         A(NEXT COMM/BUDGET SLOT)                     
         A     RF,=F'500'          LEAVE ROOM FOR 70 ENTRIES                    
*                                     7 CHARS * 70 SLOTS + SPARE                
         ST    RF,ASTNAREA         A(STATION TABLE)                             
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XCEF  SALBUF,450          CLEAR SAL BUFFER                             
         XCEF  PRDBUF,700          CLEAR PRD BUFFER                             
         XCEF  ADVBUF,2000         CLEAR ADV BUFFER                             
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    OTHERCTR,OTHERCTR                                                
         XC    ADVCTR,ADVCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  CONTPROC:                                                                    
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,REPS                                                          
         LA    R4,2                                                             
         LA    R3,KEY                                                           
         USING RECD,R3                                                          
                                                                                
CONT005  XC    KEY,KEY                                                          
         MVI   RCONQTYP,X'9C'                                                   
         MVC   RCONQREP,0(R2)                                                   
         MVC   RCONQOFF,=C'NY'                                                  
         GOTO1 HIGHDIR                                                          
         B     CONT020                                                          
         DROP  R3                                                               
*                                                                               
CONTSEQ  GOTO1 SEQDIR                                                           
*                                                                               
CONT020  CLC   KEY(RCONQOFF-RCONREC),KEYSAVE                                    
         BNE   CONT500                                                          
*                                                                               
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R6,REC                                                           
                                                                                
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CONTSEQ                                                          
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    CONTSEQ                                                          
         DROP  R6                                                               
*                                                                               
         LA    R6,REC                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CONTSEQ                                                          
         USING RCONSEND,R6                                                      
         OC    RCONSSDT,RCONSSDT                                                
         BZ    CONTSEQ                                                          
         CLC   RCONSSDT,=X'C22D'   LAST SENT GE 01/13                           
         BL    CONTSEQ                                                          
         CLC   RCONSSDT,=X'C22F'   LAST SENT LE 01/16                           
         BH    CONTSEQ                                                          
         CLC   =X'183C',RCONSSID   SELNY1                                       
         BE    CONT025                                                          
         CLC   =X'183D',RCONSSID   SELNY2                                       
         BE    CONT025                                                          
         CLC   =X'183E',RCONSSID   SELNY3                                       
         BE    CONT025                                                          
         CLC   =X'183F',RCONSSID   SELNY4                                       
         BE    CONT025                                                          
         CLC   =X'1840',RCONSSID   SELNY5                                       
         BE    CONT025                                                          
         CLC   =X'1841',RCONSSID   SELNY6                                       
         BE    CONT025                                                          
         CLC   =X'1842',RCONSSID   SELNY7                                       
         BNE   CONTSEQ                                                          
         DROP  R6                                                               
*                                                                               
CONT025  DS    0H                                                               
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   P+1(4),=C'CON:'                                                  
         MVC   P+6(2),RCONKREP                                                  
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CONT028                                                          
         USING RCONSEND,R6                                                      
         MVI   P+8,C'1'                                                         
         CLC   =X'183C',RCONSSID   SELNY1                                       
         BE    CONT028                                                          
         MVI   P+8,C'2'                                                         
         CLC   =X'183D',RCONSSID   SELNY2                                       
         BE    CONT028                                                          
         MVI   P+8,C'3'                                                         
         CLC   =X'183E',RCONSSID   SELNY3                                       
         BE    CONT028                                                          
         MVI   P+8,C'4'                                                         
         CLC   =X'183F',RCONSSID   SELNY4                                       
         BE    CONT028                                                          
         MVI   P+8,C'5'                                                         
         CLC   =X'1840',RCONSSID   SELNY5                                       
         BE    CONT028                                                          
         MVI   P+8,C'6'                                                         
         CLC   =X'1841',RCONSSID   SELNY6                                       
         BE    CONT028                                                          
         MVI   P+8,C'7'                                                         
         CLC   =X'1842',RCONSSID   SELNY7                                       
         BE    CONT028                                                          
*                                                                               
         MVI   P+8,C'?'                                                         
         DROP  R6                                                               
*                                                                               
CONT028  DS    0H                                                               
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   P+10(2),RCONKOFF                                                 
         MVC   P+14(2),RCONKGRP                                                 
         MVC   P+18(4),RCONKSTA                                                 
         MVI   P+22,C'-'                                                        
         MVC   P+23(1),RCONKSTA+4                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,P+28),ALIGN=LEFT                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,REC                                                           
                                                                                
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    CONTSEQ                                                          
*                                                                               
         TM    RCONCONF,X'40'                                                   
         BZ    CONT030                                                          
         MVC   P+40(13),=C'CONFIRMED NOW'                                       
         B     CONT035                                                          
*                                                                               
CONT030  DS    0H                                                               
         TM    RCONCONF,X'20'                                                   
         BZ    CONT040                                                          
*                                                                               
         MVC   P+55(20),=C'CONFIRMED PREVIOUSLY'                                
*                                                                               
CONT035  DS    0H                                                               
         MVC   P+80(9),=C'LAST CNF:'                                            
*                                                                               
         LA    R6,REC                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         GOTO1 DATCON,DMCB,(2,RCONSSDT),(5,P+90)                                
         DROP  R6                                                               
*                                                                               
CONT040  DS    0H                                                               
         GOTO1 REPORT                                                           
         B     CONTSEQ                                                          
*                                                                               
CONT500  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         LA    R2,2(R2)                                                         
         BCT   R4,CONT005                                                       
*                                                                               
CONTX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
REPS     DC    CL2'SZ'                                                          
*REPS     DC    CL12'BFCREAKUKFK4'                                              
ERRMSG1  DC    CL13'** MISSING **'                                              
*                                                                               
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
*        MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ABLDAREA DS    A                                                                
AGRPAREA DS    A                                                                
ANEXTGRP DS    A                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
ACOMAREA DS    A                                                                
ANEXTCOM DS    A                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
NUMBLD   DS    F                                                                
NUMCONS  DS    F                                                                
NUMBUYS  DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
ADVCTR   DS    F                                                                
SALCTR   DS    F                                                                
PRDCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'KH'             NEW REP CODE                                 
FLAGBYTS DS    0CL12               FLAGS                                        
BOTHOPEN DS    CL1                 NEITHER STATION LEFT                         
HNOPEN   DS    CL1                                                              
DIOPEN   DS    CL1                                                              
BOTHLEFT DS    CL1                                                              
         DS    CL8                 SPARE                                        
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
SAVEKEY  DS    CL(L'KEY)                                                        
KEY2     DS    CL(L'KEY)                                                        
LASTADV  DS    CL4                                                              
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         DS    0D                                                               
SALBUF   DS    CL450               AREA FOR SALESPERSON BUFFER                  
         DS    0D                                                               
PRDBUF   DS    CL700               AREA FOR PRODUCT BUFFER                      
         DS    0D                                                               
ADVBUF   DS    CL2000              AREA FOR ADVERTISER BUFFER                   
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
*  INCLUDE REGENEOM                EOM RECORD                                   
*  INCLUDE REGENDPT                DAYPART RECORD                               
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         PRINT OFF                                                              
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTY          K TYPE RECORD                                
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
PLINE    DSECT                          PRINTLINE DSECT                         
         DS    CL2                                                              
PREP     DS    CL2                                                              
         DS    CL2                                                              
PADV     DS    CL4                                                              
         DS    CL2                                                              
PMSG     DS    CL15                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048REREPKH02B05/01/02'                                      
         END                                                                    
