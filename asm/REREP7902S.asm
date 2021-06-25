*          DATA SET REREP7902S AT LEVEL 133 AS OF 08/02/96                      
*PHASE RE7902A,+0                                                               
***********************************************************************         
* HISTORY:                                                                      
*                                                                               
* 23JUL96 (SPE) ORIGINATION                                                     
*                                                                     *         
* PROGRAM TO LIST CLASS BY CATEGORY WITH OPTION TO SHOW ADVERTISER              
*                          ** END TOMBSTONE **                        *         
***********************************************************************         
         TITLE 'CLASS/CATEGORY LISTING'                                         
RE7902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7902,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(CLASREAD) CLASS/CATEGORY PRINT                  
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    FOR FORM ONLY                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
CLASREAD NTR1                                                                   
         XC    KEY,KEY                                                          
         XC    RCLSKEY,RCLSKEY                                                  
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0      OPEN SORT                    
         MVI   RCLSKTYP,X'0D'      INSERT CLASS FILE ID CODE                    
         MVC   RCLSKREP,RCREPFL    MOVE IN REP CODE                             
         MVC   KEY(27),RCLSREC     INSERT KEY (1ST 27 BYTES OF REC)             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     CLAS0040                                                         
CLAS0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
CLAS0040 EQU   *                                                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   READCAT                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,RCLSREC,     X        
               DMWORK                                                           
         LA    R6,RCLSREC          LOAD ADDR OF CLASS REC INTO REG6             
         MVI   STYPE,C'1'          SET TYPE AS 1, CLASS REC                     
         MVC   SCLASS,RCLSKCLS     MOVE CLASS CODE TO SORT                      
         MVC   SCAT,SPACES         MOVE SPACES TO CAT CODE                      
         MVC   SCLNAME,RCLSNAME    MOVE CLASSNAME TO SORT                       
         MVC   SCTNAME,SPACES      MOVE SPACES TO CAT NAME                      
         MVC   SADNAME,SPACES      MOVE SPACES TO ADV NAME                      
         MVC   SADV,SPACES         MOVE SPACES TO ADV CODE                      
OUTSRTA  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                                                               
*   TEST                                                                        
*********MVC   P+1(08),=C'SRTOUT A'                                             
*********MVC   P+10(89),SORTREC                                                 
*********GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   SORTREC,SPACES                                                   
         B     CLAS0020                                                         
*                                                                               
*                                                                               
READCAT  EQU   *                                                                
         SR    R6,R6                                                            
         XC    RCTGKEY,RCTGKEY                                                  
         LA    R5,CLCTTAB          LOAD ADDRESS OF TABLE INTO R5                
         MVI   RCTGKTYP,X'0F'      INSERT CATEGORY ID CODE                      
         MVC   RCTGKREP,RCREPFL    INSERT REP CODE                              
         MVC   KEY(27),RCTGREC     INSERT KEY (1ST 27 BYTES OF REC)             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     CATE0040                                                         
CATE0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
CATE0040 EQU   *                                                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   READADV                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,RCTGREC,     X        
               DMWORK                                                           
         LA    R6,RCTGREC          LOAD ADDR OF CATEG REC INTO REG6             
         MVI   STYPE,C'2'          SET TYPE AS 2, CATEGORY REC                  
         MVC   SCLASS,RCTGCLSS     MOVE CLASS CODE TO SORT                      
         MVC   SCAT,RCTGKCTG       MOVE CATEG CODE TO SORT                      
         MVC   SCLNAME,SPACES      MOVE SPACES TO CLASS NAME                    
         MVC   SADNAME,SPACES      MOVE SPACES TO ADV NAME                      
         MVC   SADV,SPACES         MOVE SPACES TO ADV CODE                      
         MVC   SCTNAME,RCTGNAME    MOVE CATEG NAME TO SORT                      
OUTSRTB  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*   TEST                                                                        
*********MVC   P+1(08),=C'SRTOUT B'                                             
*********MVC   P+10(89),SORTREC                                                 
*********GOTO1 REPORT                                                           
*   TEST END                                                                    
         MVC   SORTREC,SPACES                                                   
         CLI   QOPTION1,C'Y'       IS THE ADV OPTION REQUESTED?                 
         BNE   NOLOAD              IF NOT BYPASS THE LOAD                       
*********LA    R5,CLCTTAB          LOAD ADDRESS OF TABLE INTO R5                
         MVC   TABCL,RCTGCLSS      MOVE CLASS CODE TO TAB SETUP                 
         MVC   TABCT,RCTGKCTG      MOVE CAT CODE TO TAB SETUP                   
         MVC   0(4,R5),TABENT      MOVE SETUP TO TABLE                          
         LA    R5,4(R5)            INCR ADDR OF R5 BY LENGTH OF ENTRY           
NOLOAD   EQU   *                                                                
         B     CATE0020            GO GET THE NEXT CATEGORY RECORD              
*                                                                               
READADV  EQU   *                                                                
         CLI   QOPTION1,C'Y'       IS THE ADV OPTION REQUESTED?                 
         BNE   SORTIN              IF NOT BYPASS READING ADV FILE               
         SR    R6,R6                                                            
         XC    KEY,KEY                                                          
         XC    RADVKEY,RADVKEY     CLEAR KEY                                    
         MVI   RADVKTYP,X'08'      MOVE IN RECORD ID                            
*********MVC   RADVKREP,RCREPFL    MOVE REP CODE FORM CONTROL                   
         MVC   KEY(27),RADVREC     INSERT KEY (1ST 27 BYTES OF REC)             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     ADVR0040                                                         
ADVR0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
ADVR0040 EQU   *                                                                
*********CLC   KEY(27),KEYSAVE                                                  
         CLI   KEY,X'08'           IS IT AN ADV REC?                            
         BNE   SORTIN              IF NOT GET OUT                               
         CLC   KEY+25(2),RCREPFL   IS THE REPCODE = REP REQ                     
         BNE   ADVR0020            IF NOT GET ANOTHER ADV REC                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,RADVREC,     X        
               DMWORK                                                           
         LA    R6,RADVREC          LOAD ADDR OF ADVRS REC INTO REG6             
         MVI   STYPE,C'3'          SET TYPE AS 2, CATEGORY REC                  
         MVC   SADV,RADVPADV       MOVE ADV CODE TO SORT                        
         MVC   SCAT,RADVCATG       MOVE CATEG CODE TO SORT                      
         MVC   SCLNAME,SPACES      MOVE SPACES TO CLASS NAME                    
         MVC   SCTNAME,SPACES      MOVE SPACES TO CATEG NAME                    
         MVC   SADNAME,RADVNAME    MOVE ADVRS NAME TO SORT                      
*                                                                               
*****  TABLE LOOKUP FOR ADVERTISER FILE CLASS AND CATEGORY CODES                
*                                                                               
         SR    R5,R5                                                            
         LA    R5,CLCTTAB                                                       
TLOOP    EQU   *                                                                
         MVC   TABENT,0(R5)        MOVE ENTRY TO TABENT FOR COMPARE             
*********CLC   2(4,R5),RADVCATG    COMPARE CAT FROM TABLE TO FILE               
         CLC   TABCT,RADVCATG      COMPARE CAT FROM TABLE TO FILE               
         BNE   DOOLOOP             IF NOT = INCREMENT AND CONTINUE              
         MVC   SCLASS,TABCL        MOVE ENTRY CLASS TO SORT                     
         B     OUTSRTC             GO WRITE SORT                                
DOOLOOP  EQU   *                                                                
         LA    R5,4(R5)            INCREM R5 BY SIZE OF ENTRY                   
         B     TLOOP                                                            
*                                                                               
OUTSRTC  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   SORTREC,SPACES                                                   
         B     ADVR0020            GO GET THE NEXT ADVERTISER RECORD            
         EJECT                                                                  
SORTIN   EQU   *                                                                
         SR    R6,R6                                                            
         CLI   QOPTION1,C'Y'                                                    
         BNE   GETSORT                                                          
         MVI   RCSUBPRG,1                                                       
GETSORT  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET' GET SORT REC                                 
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    XIT                                                              
         MVC   SORTREC,0(R6)                                                    
         CLI   STYPE,C'1'          IS IT A CLASS RECORD?                        
         BE    WITCLASS            IF CLASS REC SET UP STORE                    
         B     HEDBACK             IF NOT GO TO HEDBACK                         
WITCLASS EQU   *                                                                
         MVC   CLCODE,SCLASS       SAVE CLASS CODE IN STORAGE                   
         MVC   CLNAME,SCLNAME      SAVE CLASS NAME IN STORAGE                   
         MVI   NEWCLASS,C'Y'       SIGNIFY A NEW CLASS                          
         B     GETSORT             GO READ ANOTHER REC                          
HEDBACK  EQU   *                                                                
         CLI   NEWCLASS,C'Y'       CHECK IF NEW CLASS                           
         BNE   JUSTCAT             IF NOT JUST PRINT CATEGORY DATA              
         BAS   RE,HEADER           DO HEADER AND RETURN                         
*********MVC   P+2(2),CLCODE       MOVE CLASS CODE TO PRINT                     
*********MVC   P+9(21),CLNAME      MOVE CLASS NAME TO PRINT                     
         MVI   NEWCLASS,C'F'       NOT A NEW CLASS                              
JUSTCAT  EQU   *                                                                
         CLI   STYPE,C'3'          IS RECORD AN ADV REC?                        
         BE    JUSTADV             IF YES GO TO JUST PRINT ADV INFO             
         CLI   NEWCLASS,C'Y'       CHECK IF NEW CLASS                           
         BE    BPSPACE             IF NEW CLASS DO NOT PRT XTRA LINE            
         CLI   QOPTION1,C'Y'       IS IT AN ADV OPTION?                         
         BNE   BPSPACE             IF NOT BYPASS PRINING BLANK LINE             
         MVC   P,BLANKS            PRINT EXTRA LINE                             
         GOTO1 REPORT                                                           
BPSPACE  EQU   *                                                                
         CLI   NEWCLASS,C'F'       CHECK IF NEW CLASS                           
         BNE   NOCLASS             IF NEW CLASS DO NOT PRT XTRA LINE            
         MVC   P+2(2),CLCODE       MOVE CLASS CODE TO PRINT                     
         MVC   P+9(21),CLNAME      MOVE CLASS NAME TO PRINT                     
         MVI   NEWCLASS,C'N'       NOT A NEW CLASS                              
NOCLASS  EQU   *                                                                
         MVC   P+35(2),SCAT        MOVE CAT CODE TO PRINT                       
         MVC   P+41(30),SCTNAME    MOVE CAT NAME TO PRINT                       
         MVC   CTCODE,SCAT         SAVE CATEG CODE IN STORAGE                   
         MVC   CTNAME,SCTNAME      SAVE CATEG NAME IN STORAGE                   
         GOTO1 REPORT                                                           
         CLI   QOPTION1,C'Y'       IS IT AN ADV OPTION?                         
         BNE   GETSORT             BYPASS PRINING BLANK LINE                    
         MVC   P+35(2),=2C'-'      MOVE - TO PRINT FOR UNDER LINE               
         MVC   P+41(30),=30C'-'    MOVE - TO PRINT FOR UNDER LINE               
         CLI   NEWCLASS,C'Y'       CHECK IF NEW CLASS                           
         BNE   JUSPRCAT            IF NOT JUST PRINT CATEGORY DATA              
         MVC   P+2(2),=2C'-'       MOVE ALL FOR UNDER LINE                      
         MVC   P+9(21),=21C'-'     MOVE ALL FOR UNDER LINE                      
JUSPRCAT GOTO1 REPORT                                                           
JUSTADV  EQU   *                                                                
         ZIC   R7,LINE             PUT LINE IN REG7                             
         LA    R8,55               PUT 50 INTO REG8                             
         CR    R7,R8               COMPARE REG 7 TO REG 8                       
         BL    NOPGBRK             IF R7 IS LOWER DO NOT FORCE HEAD             
         BAS   RE,HEADER           DO HEADER AND RETURN                         
         MVC   P+2(2),CLCODE       MOVE CLASS CODE TO PRINT                     
         MVC   P+9(21),CLNAME      MOVE CLASS NAME TO PRINT                     
         MVC   P+35(2),CTCODE      MOVE CAT CODE TO PRINT                       
         MVC   P+41(30),CTNAME     MOVE CAT NAME TO PRINT                       
         GOTO1 REPORT                                                           
         MVC   P+2(2),=2C'-'       MOVE ALL FOR UNDER LINE                      
         MVC   P+9(21),=21C'-'     MOVE ALL FOR UNDER LINE                      
         MVC   P+35(2),=2C'-'      MOVE - TO PRINT FOR UNDER LINE               
         MVC   P+41(30),=30C'-'    MOVE - TO PRINT FOR UNDER LINE               
         GOTO1 REPORT                                                           
*                                                                               
NOPGBRK  EQU   *                                                                
         CLI   STYPE,C'3'          IS RECORD AN ADV REC?                        
         BNE   GETSORT             IF NOT GET ANOTHER REC                       
         MVC   P+34(4),SADV        MOVE ADV CODE TO PRINT                       
         MVC   P+41(20),SADNAME    MOVE ADV NAME TO PRINT                       
         GOTO1 REPORT                                                           
         B     GETSORT                                                          
*                                                                               
HEADER   NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE HEADING                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
XIT      XIT1                                                                   
END      XBASE                                                                  
         LTORG                                                                  
PMWORK   DS    CL20                                                             
RELO     DS    A                                                                
         DS    F                                                                
LIMIT    DC    CL1' '                                                           
ONE      DC    PL1'1'                                                           
BLANKS   DC    132CL1' '                                                        
LCNTR    DC    PL2'00'                                                          
RCNTR    DC    PL2'00'                                                          
PNO      DS    PL2                                                              
CLCODE   DS    CL2                 CLASS CODE STORE                             
CLNAME   DS    CL30                CLASS NAME STORE                             
CTCODE   DS    CL2                 CATEGORY CODE STORE                          
CTNAME   DS    CL30                CATEGORY NAME STORE                          
NEWCLASS DS    CL1                 Y=NEWCLASS N=NOT NEW                         
NEWCAT   DS    CL1                 Y=NEWCAT   N=NOT NEW                         
PATRN    DS    CL3                                                              
MASK     DC    X'202020'                                                        
ELCODE   DS    X                                                                
SAVEKEY  DS    CL32                                                             
*                                                                               
*                                                                               
CLCTTAB  DS    150CL4              CLASS/CAT TABLE 150 TIMES 4 BYTE             
*                                                                               
TABENT   DS    0CL4                                                             
TABCL    DS    CL2                                                              
TABCT    DS    CL2                                                              
*                                                                               
*                                                                               
*********** SORTRECORD FORMAT IS BELOW                                          
SORTREC  DS    0CL89                                                            
SCLASS   DS    CL2                 CLASS CODE                                   
SCAT     DS    CL2                 CATEGORY CODE                                
STYPE    DS    CL1                 SORT TYPE 1=CLASS 2=CAT 3=ADV                
SCLNAME  DS    CL30                CLASS NAME                                   
SCTNAME  DS    CL30                CATEGORY NAME                                
SADNAME  DS    CL20                ADVERTISER NAME                              
SADV     DS    CL4                 ADVERTISER CODE                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,05,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=89'                                    
*                                                                               
IOAREA   DS    CL1000                                                           
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133REREP7902S08/02/96'                                      
         END                                                                    
