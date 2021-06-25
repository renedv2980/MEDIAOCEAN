*          DATA SET PRSFM14DIC AT LEVEL 108 AS OF 09/14/94                      
*          DATA SET PRSFM14DICDIC AT LEVEL 099 AS OF 08/17/94                   
*PHASE T41C14B                                                                  
*        TITLE 'PRSFM14DIC USE ENTRY RECORDS TO CREATE PWRTB ENTRIES'           
         TITLE 'PRSFM14DIC USE ENTRY RECORDS TO CREATE PWRTB ENTRIES'           
*                                                                               
T41C14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T41C14*,R7,RR=RE                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,WRKRELO          SAVE RELOCATION FACTOR                       
         ST    RC,WRKWORKA         SAVE WORKAREA ADDRESS                        
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
*                                                                               
         LA    RF,KEY              SET I/O AREA FOR KEYS                        
         ST    RF,AIO                                                           
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     EXIT                NON-USABLE MODE                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'PRSFM14DIC CREATE PWRTB CODE - VALKEY'                          
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       DS    0H                                                               
*                                                                               
*        OPEN CONTROL SYSTEM FILES                                              
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE OPEN CONTROL SYS FILES           
         BNE   VKOPCTLX                                                         
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RF,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RF,WRKUTLA          SAVE ADDRESS                                 
         MVC   WRKSYS,4(RF)        SAVE CURRENT SYSTEM ID                       
         MVI   4(RF),X'0A'         SET FOR CONTROL SYSTEM                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',CONFILES,DMWORK              
*                                                                               
         L     RF,WRKUTLA          SWITCH BACK TO CURRENT SYSTEM                
         MVC   4(1,RF),WRKSYS                                                   
*                                                                               
         B     VKOPCTLX                                                         
*                                                                               
CONFILES DS    0D                  CONTROL SYSTEM FILE LIST                     
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFILE'                                                    
         DC    CL8'X'              END OF LIST                                  
*                                                                               
VKOPCTLX DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'PRSFM14DIC CREATE PWRTB CODE - PRNTREP'                         
***********************************************************************         
*                                                                     *         
*        PRINT REPORT - IN THIS CASE WE ARE UPDATING CEPRECS          *         
*                       ON CTFILE                                     *         
*                       DO THIS BY CREATING A NEW DUMPTAPE TO BE      *         
*                       FED INTO LOAD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PR       DS    0H                                                               
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         MVI   USEIO,C'Y'          INDICATE USER DOES IO                        
         MVI   IOOPT,C'Y'          OVERLAY DOES IO                              
*                                                                               
         MVC   AIO,AIO3            SET IOAREA                                   
*                                                                               
         OPEN  (DICFILE,OUTPUT)    OPEN OUTPUT TAPE                             
*                                                                               
         GOTO1 =A(SWTCNTL),RR=WRKRELO SWITCH TO CONTROL SYSTEM                  
*                                                                               
         XC    KEY,KEY             SET TO FIND FIRST ENTRY RECORD               
         LA    R4,KEY                                                           
         USING DICKEYD,R4          ESTABLISH DICTIONARY RECORD KEY              
*                                                                               
         MVI   DICKTYP,DICKTYPQ    SET FOR DICTIONARY RECORDS                   
         MVC   DICCODE,=CL8'PRWRI' SET FOR PWR DICTIONARY                       
         MVC   DICENTRY,=CL8'#'    BYPASSS $ ENTRIES                            
*                                                                               
         MVC   KEYSAVE,KEY         SAVE START KEY                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEY,KEY,AIO2                  
*                                                                               
PRRDLOOP DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        DONE AT END OF FILE                          
         BO    PRRDDONE                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING DICKEYD,R4          ESTABLISH DICTIONARY RECORD KEY              
         CLC   DICKEY(DICENTRY-DICKEY),KEYSAVE                                  
         BNE   PRRDDONE            DONE AT END OF DICTIONARY                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFILE',KEY+36,AIO,AIO2              
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         TITLE 'PRSFM14DIC CREATE PWRTB CODE - PRCTLOOP'                        
***********************************************************************         
*                                                                     *         
*        CREATE PWRTB ENTRY IN OUTPUT DATASET                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDC     DS    0H                                                               
*                                                                               
         MVC   RPENTRY,DICENTRY    ENTRY ID IN TITLE                            
*                                                                               
         MVC   RPLABEL,DICENTRY    PRINT PWRTB LABEL                            
*                                                                               
         LA    R6,DICFIRST         ESTABLISH FIRST ELEMENT AS                   
         USING DENAMD,R6           DESCRIPTION ELEMENT                          
         SR    RF,RF                                                            
*                                                                               
         CLI   DENAMEL,0           DONE AT END OF RECORD                        
         BE    PRDCNAMX                                                         
         CLI   DENAMEL,X'02'       FIND NAME ELEMENT                            
         BE    *+16                                                             
         IC    RF,DENAMLEN         ELEMENT LENGTH                               
         LA    R6,DENAMD(RF)       BUMP TO NEXT ELEMENT                         
         B     *-24                                                             
*                                                                               
         XC    RPDESC,RPDESC       INIT DESCRIPTION OUTPUT AREA                 
*                                                                               
         IC    RF,DENAMLEN         ELEMENT LENGTH                               
         SH    RF,=Y(DENAME-DENAMD) DESCRIPTION LENGTH                          
         BNP   PRDCNAMX            SKIP IF NO DEACRIPTION                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RPDESC(0),DENAME    PRINT DESCRIPTION                            
*                                                                               
PRDCNAMX DS    0H                                                               
*                                                                               
         MVC   RPCODE+8(2),SPACES  KILL '),'                                    
*                                                                               
         MVC   RPCODE,DICENTRY     ACTUAL ENTRY                                 
*                                                                               
         LA    R1,RPCODE+8         FIND LAST SPACE                              
*                                                                               
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(2,R1),=C'),'      CLOSE BRACKETS                               
*                                                                               
         LA    R3,RPLINEN          NUMBER OF LINES TO PRINT                     
         LA    R5,RPLINES          START OF LINES                               
*                                                                               
PRDCWRLP DS    0H                                                               
*                                                                               
         PUT   DICFILE,(R5)        PUT A LINE                                   
*                                                                               
         LA    R5,L'RPLINE1(R5)    NEXT LINE                                    
         BCT   R3,PRDCWRLP                                                      
*                                                                               
PRDCWRDN DS    0H                                                               
*                                                                               
PRRDCONT DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEY,KEY,AIO2                  
*                                                                               
         B     PRRDLOOP                                                         
*                                                                               
PRRDDONE DS    0H                                                               
*                                                                               
         CLOSE DICFILE             CLOSE OUTPUT TAPE                            
*                                                                               
PRX      DS    0H                                                               
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
*                                                                               
         GOTO1 =A(SWTBACK),RR=WRKRELO SWITCH BACK TO USER SYSTEM                
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'PRSFM14DIC - MERGE EDR PUB FILE - DCBS'                         
***********************************************************************         
*                                                                     *         
*        DCB FOR EDR FILE AND OUTPUT TAPE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DICFILE  DCB   DDNAME=DICFILE,DSORG=PS,MACRF=(PM),EODAD=PRRDDONE,      X        
               RECFM=FB,BUFNO=2,BLKSIZE=720,LRECL=80                            
*                                                                               
         TITLE 'PRWRI13 - REPORT SPECS - EDRSPECS'                              
***********************************************************************         
*                                                                     *         
*        REPORT SPECS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
EDRSPECS DS    0H                                                               
*                                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,113,PAGE                                                      
*                                                                               
         SSPEC H2,53,C'EDR/CTFILE MERGE'                                        
         SSPEC H3,53,C'----------------'                                        
*                                                                               
         SSPEC H5,1,C'EDR PUB NAME'                                             
         SSPEC H5,32,C'EDR EDITION'                                             
         SSPEC H5,63,C'EDR RATE CARD'                                           
         SSPEC H5,79,C'MEDIA'                                                   
         SSPEC H5,85,C'PUB SEQ'                                                 
         SSPEC H5,93,C'EDN SEQ'                                                 
         SSPEC H5,102,C' PAKID'                                                 
*                                                                               
         SSPEC H6,1,C'------------'                                             
         SSPEC H6,32,C'-----------'                                             
         SSPEC H6,63,C'-------------'                                           
         SSPEC H6,79,C'-----'                                                   
         SSPEC H6,85,C'-------'                                                 
         SSPEC H6,93,C'-------'                                                 
         SSPEC H6,102,C' -----'                                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DS    0D                                                               
*                                                                               
RECSPECS DS    0H                  SPECS FOR RECORD COUNTS SPECS                
*                                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,113,PAGE                                                      
         SPACE 1                                                                
         SSPEC H2,53,C'EDR/CTFILE MERGE'                                        
         SSPEC H3,53,C'----------------'                                        
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
DASHS    DC    64C'-'              DASHES                                       
         DS    0F                                                               
         LTORG                                                                  
         TITLE 'PRWRI13 - SWITCH BACK TO USER SYSTEM - SWTBACK'                 
***********************************************************************         
*                                                                     *         
*        SWITCH BACK TO USER SYSTEM                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTBACK  NMOD1 0,**#SBK                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVC   USEIO,SVUSEIO       SET USER I/O OPTION                          
         MVC   SYSDIR,SVSYSDIR     SET FILE NAMES                               
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI   SET DISPLACEMENT OF DATA IN RECORD           
         MVC   LKEY,SVLKEY         SET KEY LENGTH                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),SVSYS       SYSTEM ID                                    
         MVC   FILENAME,SVFILENM                                                
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTBACK1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVC   4(1,RF),WRKSYS         RESTORE CURENT SYSTEM ID                  
         B     SWTBACKX                                                         
*                                                                               
SWTBACK1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB         SWITCH BACK TO USER SYSTEM                   
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTBACKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI13 - SWITCH TO CONTROL SYSTEM - SWTCNTL'                   
***********************************************************************         
*                                                                     *         
*        SWITCH TO CONTROL SYSTEM                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
SWTCNTL  NMOD1 0,**#SCT                                                         
*                                                                               
         L     RC,WRKWORKA         RESTORE WORKAREA ADDRESS                     
*                                                                               
         MVI   USEIO,C'Y'          USER DOES I/O                                
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFF-LINE                                  
         BNE   SWTCNTL1                                                         
*                                                                               
         L     RF,WRKUTLA             POINT TO UTL                              
         MVI   4(RF),X'0A'            SET TO CONTROL SYSTEM                     
         B     SWTCNTLX                                                         
*                                                                               
SWTCNTL1 DS    0H                                                               
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0  SWITCH TO CONTROL SYSTEM              
         CLI   DMCB+4,0            MUST WORK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWTCNTLX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRSFM14DIC - REPORT OUTPUT LINES - RPLINE'                      
***********************************************************************         
*                                                                     *         
*        REPORT LAYOUT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RPLINES  DS    0D                  START OF REPORT LINES                        
*                                                                               
RPLINE1  DC    CL80'*'                                                          
*                                                                               
         ORG                                                                    
*                                                                               
RPLINE2  DC    CL80'*'                                                          
         ORG   RPLINE2+8                                                        
RPENTRY  DS    CL8                 ENTRY NAME                                   
         DC    C' - '                                                           
RPDESC   DS    CL60                                                             
*                                                                               
         ORG                                                                    
*                                                                               
RPLINE3  DC    CL80'*'                                                          
*                                                                               
         ORG                                                                    
*                                                                               
RPLINE4  DC    CL80' '                                                          
         ORG   RPLINE4                                                          
RPLABEL  DS    CL8                 LABEL                                        
         ORG   RPLINE4+10                                                       
         DC    C'PWRTB'                                                         
         ORG   RPLINE4+16                                                       
         DC    C'DESC=(,,,'                                                     
RPCODE   DS    CL8                                                              
         ORG   RPLINE4+71                                                       
         DC    C'*'                CONTINUATION MARK                            
*                                                                               
         ORG                                                                    
*                                                                               
RPLINE5  DC    CL80' '                                                          
         ORG   RPLINE5+16                                                       
         DC    C'MENUM=,'                                                       
         ORG   RPLINE5+71                                                       
         DC    C'*'                CONTINUATION MARK                            
         ORG                                                                    
*                                                                               
RPLINE6  DC    CL80' '                                                          
         ORG   RPLINE6+16                                                       
         DC    C'FIELDM=,'                                                      
         ORG   RPLINE6+71                                                       
         DC    C'*'                CONTINUATION MARK                            
         ORG                                                                    
*                                                                               
RPLINE7  DC    CL80' '                                                          
         ORG   RPLINE7+16                                                       
         DC    C'MEDIAM=,'                                                      
         ORG   RPLINE7+71                                                       
         DC    C'*'                CONTINUATION MARK                            
         ORG                                                                    
*                                                                               
RPLINE8  DC    CL80' '                                                          
         ORG   RPLINE8+16                                                       
         DC    C'DOCM=,'                                                        
         ORG   RPLINE8+71                                                       
         DC    C'*'                CONTINUATION MARK                            
         ORG                                                                    
*                                                                               
RPLINE9  DC    CL80' '                                                          
         ORG   RPLINE9+16                                                       
         DC    C'FLAVORM='                                                      
*                                                                               
         ORG                                                                    
*                                                                               
RPLINEN  EQU   (*-RPLINES)/L'RPLINE1  NUMBER OF LINES                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD4D                                                       
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
*                                                                               
WRKRELO  DS    A                   THIS MODULE'S RELO FACTOR                    
WRKWORKA DS    A                   COMMON WORKAREA ADDRESS                      
WRKUTLA  DS    A                   A(UTL)                                       
WRKSYS   DS    XL1                 CURRENT SYSTEM SE NUMBER                     
*                                    DON'T PAGE                                 
         EJECT                                                                  
       ++INCLUDE CTGENDIC                                                       
         EJECT                                                                  
*       ++INCLUDE PRGENFILE                                                     
*       ++INCLUDE PRGLOBEQUS                                                    
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE DDMASTD                                                       
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108PRSFM14DIC09/14/94'                                      
         END                                                                    
