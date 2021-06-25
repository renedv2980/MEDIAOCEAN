*          DATA SET REREPUT02  AT LEVEL 028 AS OF 05/01/02                      
*PHASE REUT02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPUT02 (REUT02) - EXTRACT ADV/AGY RECS FROM UT->K3'          
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPUT02 -- EXTRACTS ADV/AGY RECS FROM UT TO K3          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* OCT19/95 (SKU) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
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
RERT02   CSECT                                                                  
         NMOD1 0,**RERT**,R8,R9,RR=R5                                           
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
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY RECORDS                       
*                                                                               
*        GOTO1 ADVPROC,DMCB,(RC)   PROCESS ADVERTISER RECORDS                   
*                                                                               
MAINX    DS    0H                                                               
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
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
                                                                                
         XCEF  ADVBUF,2000         CLEAR ADV BUFFER                             
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    BUYCTR,BUYCTR                                                    
         XC    OTHERCTR,OTHERCTR                                                
         XC    ADVCTR,ADVCTR                                                    
         XC    AGYCTR,AGYCTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    PRDCTR,PRDCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  AGYPROC: EXTRACT UT AGENCY RECS AS SPECIFIED BY TABLE AND  *                 
*           WRITE THEM OUT WITH NEW REP CODE K3                  *              
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         MVI   KEYTYPE,X'0A'                                                    
                                                                                
AGY03    DS    0H                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE                                                   
         GOTO1 HIGHDIR                                                          
         B     AGY10                                                            
*                                                                               
AGY05    DS    0H                                                               
         GOTO1 SEQDIR                                                           
         CLC   KEY(1),KEYTYPE                                                   
         BNE   AGY20                                                            
                                                                                
AGY10    DS    0H                                                               
         CLC   KEY+25(2),=C'UT'                                                 
         BNE   AGY05                                                            
*                                                                               
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RAGYREC,R6                                                       
         MVC   REC-4(2),RAGYLEN    INSERT LENGTH FOR PUT                        
         MVC   RAGYKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+2,C'Y'                                                   
         BNE   AGY20                                                            
         MVC   P+1(8),=C'AGY REC:'                                              
         CLI   KEY,X'1A'                                                        
         BNE   *+8                                                              
         MVI   P+3,C'2'                                                         
*                                                                               
         MVC   P+10(4),RAGYKAGY                                                 
         MVI   P+14,C'-'                                                        
         MVC   P+15(2),RAGYKAOF                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
         B     AGY05                                                            
                                                                                
AGY20    DS    0H                                                               
         CLI   KEYTYPE,X'0A'                                                    
         BNE   AGYX                                                             
         MVI   KEYTYPE,X'1A'                                                    
         B     AGY03                                                            
                                                                                
AGYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  ADVPROC: EXTRACT UT ADV RECS AS SPECIFIED BY TABLE AND     *                 
*           WRITE THEM OUT WITH NEW REP CODE K3                  *              
******************************************************************              
ADVPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     ADV20                                                            
                                                                                
ADV10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
ADV20    DS    0H                                                               
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         CLI   KEY,X'08'                                                        
         BNE   ADVX                                                             
         CLC   =C'UT',RADVKREP                                                  
         BNE   ADV10                                                            
         DROP  R6                                                               
                                                                                
         GOTO1 GETRECRD                                                         
                                                                                
         LA    R6,REC                                                           
         USING RADVREC,R6                                                       
         MVC   REC-4(2),RADVLEN    INSERT LENGTH FOR PUT                        
         MVC   RADVKREP,NEWREP     NEW REP CODE                                 
                                                                                
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         CLI   QUESTOR+3,C'Y'                                                   
         BNE   ADV30                                                            
         MVC   P+1(8),=C'ADV REC:'                                              
         MVC   P+10(4),RADVKADV                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
                                                                                
ADV30    DS    0H                                                               
         B     ADV10                                                            
                                                                                
ADVX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
*                                                                               
         MVC   P+1(24),=C'AGENCY RECS   PROCESSED:'                             
         EDIT  AGYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(24),=C'ADVS          PROCESSED:'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
PUTRECS  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         B     EXIT                                                             
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
AGYCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'K3'             NEW REP CODE                                 
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
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
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
         ORG   RECORD                                                           
       ++INCLUDE REGENDCT          DCT RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENRDA          RDA RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REREPUT02 05/01/02'                                      
         END                                                                    
