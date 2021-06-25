*          DATA SET REREPMSTR  AT LEVEL 140 AS OF 06/26/03                      
*PHASE REST02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'REREPMSTR - MSTREET FILE COMPARISON'                            
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPMSTR -- COMPARE MSTREET FILE TO EXISTING STATIONS    *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JUN26/03 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REST02   CSECT                                                                  
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (INTAPE1,(INPUT))   TAPE1:  STATION LIST                         
*                                                                               
         BAS   RE,INITIAL                                                       
*                                                                               
         CLOSE (INTAPE1,REWIND)                                                 
*                                                                               
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'***COMPARISON RUN  COMPLETED***'                      
         GOTO1 REPORT                                                           
*                                                                               
MAIN0100 EQU   *                                                                
         XIT1                      EXIT                                         
         EJECT                                                                  
*                                                                               
******************************************************************              
*   INITIALIZATION  - ALL PROCESSING IS DONE FROM THIS AREA                     
*                                                                               
******************************************************************              
INITIAL  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     R5,ATREC                                                         
         GET   INTAPE1,(R5)        READ TAPE INTO TAPE RECORD                   
*                                  SKIP FIRST RECORD                            
INIT0020 EQU   *                                                                
         GET   INTAPE1,(R5)        READ TAPE INTO TAPE RECORD                   
**       MVC   P+1(07),=C'TAPE IN'                                              
**       MVC   P+8(32),TRECORD                                                  
**       GOTO1 REPORT                                                           
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*                                                                               
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,INITMOV1         MOVE BY LENGTH                               
         B     INIT0040                                                         
INITMOV1 MVC   MYWORK+8(0),4(R5)                                                
INIT0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
**       MVC   P+1(15),=C'MYWORK  RECORD:'                                      
**       MVC   P+20(80),MYWORK                                                  
**       GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,SCANQUOT                                                      
**       MVC   P+1(13),=C'QUOTE RECORD:'                                        
**       MVC   P+14(80),MYWORK                                                  
**       GOTO1 REPORT                                                           
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,(30,MYWORK),AWORKBLK,C',=,='                    
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   INIT0080                                                         
         DC    H'0'                                                             
*                                                                               
INIT0080 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
**       MVC   P+1(12),=C'SCANNER OKAY'                                         
**       GOTO1 REPORT                                                           
**       L     R4,AWORKBLK         A(SCANNER OUTPUT)                            
**       LA    RF,256                                                           
**       GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         MVC   MSTRSTAT,64(RF)     SAVE MSTREET STATION                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                                                        
         MVI   KEY+1,X'08'         ESTABLISH UID PASSIVE KEY                    
         MVC   KEY+14(2),12(RF)    INSERT REP CODE                              
         ZICM  R2,108(RF),4        GET BINARY VALUE OF UID                      
         EDIT  (R2),(6,KEY+16),FILL=0,ZERO=NOBLANK                              
*                                                                               
***      MVC   P+1(15),=C'8308 KEY      :'                                      
***      MVC   P+20(27),KEY                                                     
***      GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HIGHDIR             READ FOR KEY                                 
         CLC   KEY(22),KEYSAVE     UID ON FILE FOR REP?                         
         BE    INIT0200            YES                                          
         MVC   P+1(20),=C'UID NOT ON FILE FOR:'                                 
         MVC   P+44(2),KEYSAVE+14  INSERT REP                                   
         MVC   P+50(6),KEYSAVE+16  INSERT UID                                   
         MVC   P+60(5),MSTRSTAT    INSERT STATION                               
         GOTO1 REPORT                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),KEYSAVE+14   INSERT REP CODE                           
         MVC   KEY+22(5),MSTRSTAT  INSERT STATION                               
         GOTO1 HIGHDIR             READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BE    INIT0100            YES                                          
         MVC   P+11(19),=C'(     ) NOT ON FILE'                                 
         MVC   P+12(5),MSTRSTAT                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     INIT0020            GO BACK FOR NEXT                             
INIT0100 EQU   *                                                                
         GOTO1 GETRECRD                                                         
         LA    R6,RECORD                                                        
         USING RSTAREC,R6                                                       
         LA    R3,RSTAELEM         SET A(01 ELEMENT)                            
**>>                                                                            
INIT0120 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   INIT0140            NO                                           
         MVC   P+10(35),=C'NO UNIQUE ID ELEMENT W/THIS STATION'                 
         GOTO1 REPORT                                                           
         B     INIT0180                                                         
INIT0140 EQU   *                                                                
         CLI   0(R3),X'2A'                                                      
         BE    INIT0160                                                         
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     INIT0120                                                         
INIT0160 EQU   *                                                                
         USING RSTAUIEL,R3                                                      
         MVC   P+11(22),=C'UNIQUE ID FOR (     ):'                              
         MVC   P+26(5),MSTRSTAT                                                 
         MVC   P+33(6),RSTAUIST                                                 
         GOTO1 REPORT                                                           
         DROP  R3,R6                                                            
INIT0180 EQU   *                                                                
         GOTO1 REPORT                                                           
         B     INIT0020                                                         
**>>                                                                            
INIT0200 EQU   *                                                                
         CLC   KEY+22(5),MSTRSTAT  UID ON FILE: SAME STATION?                   
         BE    INIT0020            YES - GO BACK FOR NEXT                       
         MVC   P+1(29),=C'UID ON FILE, STATION DIFFERS:'                        
         MVC   P+34(2),KEY+14      INSERT REP                                   
         MVC   P+40(6),KEY+16      INSERT UID                                   
         MVC   P+50(4),=C'MST='                                                 
         MVC   P+55(5),MSTRSTAT    INSERT MSTREET STATION                       
         MVC   P+62(4),=C'DDS='                                                 
         MVC   P+67(5),KEY+22      INSERT KEY STATION                           
         GOTO1 REPORT                                                           
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         LA    R6,RECORD                                                        
         USING RSTAREC,R6                                                       
         LA    R3,RSTAELEM         SET A(01 ELEMENT)                            
INIT0220 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   INIT0240            NO                                           
         MVC   P+10(25),=C'NO SUPPORTING DATA FOR THIS STATION'                 
         GOTO1 REPORT                                                           
         B     INIT0280                                                         
INIT0240 EQU   *                                                                
         CLI   0(R3),X'F1'                                                      
         BE    INIT0260                                                         
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     INIT0220                                                         
INIT0260 EQU   *                                                                
         USING RSTACTEL,R3                                                      
         MVC   P+11(06),=C'ADDED:'                                              
         GOTO1 DATCON,DMCB,(3,RSTACTAD),(5,P+18)                                
         MVC   P+30(8),RSTACTAI                                                 
         MVC   P+41(06),=C'CHGED:'                                              
         GOTO1 DATCON,DMCB,(3,RSTACTCD),(5,P+48)                                
         MVC   P+60(8),RSTACTCI                                                 
         DROP  R3,R6                                                            
         GOTO1 REPORT                                                           
INIT0280 EQU   *                                                                
         GOTO1 REPORT                                                           
         B     INIT0020                                                         
INIT0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*   SCANQUOT:  REMOVE ',' FROM " INPUT                                          
******************************************************************              
SCANQUOT NTR1                                                                   
         LA    RF,MYWORK+8           SET A(INPUT)                               
SQUO0020 EQU   *                                                                
         CLI   0(RF),0             BINARY ZERO FOUND?                           
         BE    SQUO0800            YES - FINISHED                               
         CLI   0(RF),C'"'          DOUBLE QUOTE FOUND?                          
         BNE   SQUO0030            NO                                           
***      MVI   0(RF),C' '          YES - REPLACE WITH SPACE                     
         B     SQUO0040            YES                                          
SQUO0030 EQU   *                                                                
         LA    RF,1(RF)            NO  - GO BACK FOR NEXT                       
         B     SQUO0020                                                         
SQUO0040 EQU   *                                                                
         LA    RF,1(RF)            PICK UP NEXT CHARACTER                       
SQUO0060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD REACHED?                       
         BNE   *+6                 NO                                           
         DC    H'0'                YES - MUST FIND SECOND "                     
         CLI   0(RF),C','                                                       
         BE    SQUO0080            COMMA FOUND                                  
         CLI   0(RF),C'"'                                                       
         BNE   SQUO0040            GO BACK FOR NEXT CHAR                        
***      MVI   0(RF),C' '          REPLACE " WITH SPACE                         
         B     SQUO0800            LAST " FOUND - FINISHED                      
SQUO0080 EQU   *                                                                
         MVI   0(RF),X'40'         REPLACE ',' WITH SPACE                       
         B     SQUO0040            GO BACK FOR NEXT                             
SQUO0800 EQU   *                                                                
         XIT1                                                                   
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
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               RECORD,(0,DMWORK)                                                
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   =X'05721739',KEY+12                                              
*        BNE   TEST0030                                                         
*        MVC   P+1(09),=C'CONTRACT:'                                            
*        MVC   P+12(4),DMCB+8                                                   
*        MVC   P+20(34),KEY                                                     
*        GOTO1 REPORT                                                           
*EST0030 EQU   *                                                                
*                                                                               
         TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'02'        TEST FOR RECORD DELETED                      
         BO    EQXIT               DELETE SET - PROCESS                         
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
         XIT1                                                                   
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         XIT1                                                                   
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
         XIT1                                                                   
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
         XIT1    .1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6                               
FOXES    DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
DUMMYREP DS    CL14                DUMMY 'STATION TABLE' ENTRY                  
*                                     WHEN STATION NOT IN LIST                  
MYWORK   DS    CL64                                                             
MYWORK2  DS    CL64                                                             
ELEM     DS    CL64                                                             
MSTRSTAT DS    CL5                                                              
*                                                                               
SAVEKEY  DS    CL(L'KEY)                                                        
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         DS    0H                                                               
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE1  DCB   DDNAME=INTAPE1,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=INIT0400                             
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
RECORD   DS    CL4008              AREA FOR RECORD: 4K CONTRACT                 
         DS    0D                                                               
         DS    F                                                                
TRECORD  DS    CL1000              READ AREA FOR TAPE                           
         DS    0D                                                               
ATREC    DC    A(TRECORD-4)                                                     
WORKBLK  DS    CL1000                                                           
AWORKBLK DC    A(WORKBLK)                                                       
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
*&&DO                                                                           
DISPTOTS NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(30),=C'STATIONS      PROCESSED      :'                       
         EDIT  STAREAD,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
RECD     DSECT                                                                  
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140REREPMSTR 06/26/03'                                      
         END                                                                    
