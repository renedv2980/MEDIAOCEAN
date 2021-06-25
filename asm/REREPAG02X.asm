*          DATA SET REREPAG02X AT LEVEL 062 AS OF 05/01/02                      
*          DATA SET REREPAG02X AT LEVEL 060 AS OF 02/19/96                      
*PHASE REAG02A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPEDAGY (REEDAGY) FIND ALL EDI AGENCY CODES'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPEDAGY -- FIND ALL EDI AGENCY CODES                   *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* 19FEB96 HAPPY CHINESE NEW YEAR!                                  *            
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
REEDAGY  CSECT                                                                  
         NMOD1 0,**REAG**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 MAINPROC,DMCB,(RC)  PROCESS DARE X'51' RECORDS                   
*                                                                               
         GOTO1 AGYPROC,DMCB,(RC)   PROCESS AGENCY X'9A' RECORDS                 
*                                                                               
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*  CONTPROC:                                                                    
******************************************************************              
MAINPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RE,AGYTABLE                                                      
         L     RF,=F'11000'                                                     
         XCEF                                                                   
*                                                                               
         LA    R3,KEY                                                           
         USING RECD,R3                                                          
                                                                                
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,=C'SZ'                                                  
         GOTO1 HIGHDIR                                                          
         B     MAIN020                                                          
         DROP  R3                                                               
*                                                                               
MAINSEQ  GOTO1 SEQDIR                                                           
*                                                                               
MAIN020  DS    0H                                                               
         CLC   KEY(RDARKSTA-RDARREC),KEYSAVE                                    
         BNE   MAINX                                                            
*                                                                               
         L     RF,DARECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DARECTR                                                       
*                                                                               
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R6,REC                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BZ    MAINSEQ                                                          
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
KEYD     USING RCONPTYP,KEY                                                     
         MVI   KEYD.RCONPTYP,X'8C'                                              
         MVC   KEYD.RCONPREP,=C'SZ'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   KEYD.RCONPCON,WORK                                               
         DROP  KEYD                                                             
*                                                                               
         GOTO1 HIGHDIR                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    MAIN050                                                          
         GOTO1 HEXOUT,DMCB,RDARREP#,P,4,=C'TOG'                                 
         MVC   P+10(22),=C'<== CONTRACT NOT FOUND'                              
         GOTO1 REPORT                                                           
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGHDIR                                                          
         B     MAINSEQ                                                          
*                                                                               
MAIN050  DS    0H                                                               
         L     RF,DARE2CTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DARE2CTR                                                      
         MVC   ENTRY(5),RDARKAGY                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   ENTRY+5(6),RCONKAGY                                              
         DROP  R6                                                               
*                                                                               
         LA    R2,AGYTABLE                                                      
MAIN060  CLI   0(R2),0                                                          
         BE    MAIN065                                                          
         CLC   ENTRY,0(R2)                                                      
         BE    MAIN070                                                          
         LA    R2,L'ENTRY(R2)                                                   
         B     MAIN060                                                          
MAIN065  MVC   0(L'ENTRY,R2),ENTRY                                              
*                                                                               
MAIN070  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGHDIR                                                          
         B     MAINSEQ                                                          
*                                                                               
MAINX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
******************************************************************              
*  CONTPROC:                                                                    
******************************************************************              
AGYPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,AGYTABLE                                                      
         MVC   P(19),=C'EDI AGY     DDS AGY'                                    
         GOTO1 REPORT                                                           
         MVC   P(19),=C'-------------------'                                    
         GOTO1 REPORT                                                           
AGY10    DS    0H                                                               
         MVC   P(3),0(R2)                                                       
         MVI   P+3,C'-'                                                         
         MVC   P+4(2),3(R2)                                                     
         MVC   P+12(6),5(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,L'ENTRY(R2)                                                   
         CLI   0(R2),0                                                          
         BE    AGYX                                                             
         B     AGY10                                                            
*                                                                               
AGYX     DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(27),=C'TOTAL 51 RECORDS PROCESSED:'                          
         EDIT  DARECTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(27),=C'TOTAL CONTRACTS  PROCESSED:'                          
         EDIT  DARE2CTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
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
DARECTR  DS    F                                                                
DARE2CTR DS    F                                                                
CONCTR   DS    F                                                                
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
ENTRY    DS    CL11                                                             
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2008              AREA FOR RECORD                              
         DS    0D                                                               
AGYTABLE DS    CL11000             AREA FOR AGENCY BUFFER                       
*                                                                               
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         PRINT OFF                                                              
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062REREPAG02X05/01/02'                                      
         END                                                                    
