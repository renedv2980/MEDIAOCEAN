*          DATA SET REREP3C02  AT LEVEL 184 AS OF 11/08/02                      
*PHASE RE3C02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RE3C02 - REREP3C02 - MEDIA OCEAN SUPPORT FEED'                  
**********************************************************************          
*********************************************************************           
*                                                                   *           
*   REREP3C02 - RE3C02 - MEDIA OCEAN SUPPORT FEED: BDE              *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*   MAY02/02 (BU ) --- ORIGINAL ENTRY                               *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         ENTRY RE3C02                                                           
RE3C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3C02,RR=RE                                                 
*                                                                               
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         STM   R2,RC,SAVEREGS                                                   
         EJECT                                                                  
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   PAGE,1                                                           
         MVI   MAXLINES,58                                                      
*                                                                               
*   TEST                                                                        
***      MVC   P+1(05),=C'MODE='                                                
***      MVC   P+6(1),MODE                                                      
***      GOTO1 REPORT                                                           
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
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
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
         DC    AL1(REQFRST),AL3(INITIAL)  ALL PROCESSING DONE HERE              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
***      DC    AL1(PROCCONT),AL3(POST)                                          
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
         BAS   RE,RETFILE          DETERMINE OUTPUT FILE DSN                    
*                                                                               
*   TEST                                                                        
**       MVC   P+1(10),=C'FILE DSN :'                                           
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
         OPEN  (EXFILE1,(OUTPUT))  OPEN EXFILE1                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*   TEST                                                                        
**       MVC   P+1(10),=C'FILE OPEN:'                                           
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*   ADD A SINGLE RECORD TO THE OUTPUT FILE                                      
*                                                                               
         LA    R3,EXFILE1          A(EXFILE1)                                   
         LA    R6,DUMMYREC         A(OUTPUT RECORD)                             
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'REC OUT  :'                                           
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLOSE (EXFILE1,)          CLOSE INTERIM FILE                           
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'FILE CLOS:'                                           
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
         GOTO1 =A(EDICT),DMCB,RETAREA    OUTPUT EDICT HEADER                    
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(10),=C'EDICT OUT:'                                           
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVI   FORCEHED,C'N'       DON'T PAGE BREAK                             
         MVC   P+01(26),=C'SUPPORT FILE PRODUCED    :'                          
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(13),=C'RETURNED NAME'                                        
         MVC   P+15(44),RETAREA                                                 
         GOTO1 REPORT                                                           
         B     INIT0200                                                         
                                                                                
INIT0200 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
RETFILE  NTR1                                                                   
*                                                                               
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
         XIT1                                                                   
*                                                                               
PARMLST  CALL  ,(DDNAME,RETAREA),MF=L                                           
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
DDNAME   DC    CL8'EXFILE1'                                                     
RETAREA  DS    CL44                                                             
         EJECT                                                                  
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
EDICT    NTR1                                                                   
                                                                                
         L     R2,0(R1)            SET A(RETURN AREA FOR GEN FILENAME)          
*                                     DO NOT REUSE R2                           
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
***>>>   MVC   P+9(11),=C'EDICT=*BIAS'                                          
         MVC   P+9(10),=C'EDICT=SUPX'                                           
*                                                                               
         MVC   P+34(4),=C'W  D'    WIDE REPORT - 132 CHARS                      
*                                     D = ?? (YI NEEDS IT)                      
         MVC   P+54(2),=C'MO'      SET TO MEDIA OCEAN                           
*                                  SEND SPECIAL PRINT LINE                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         MVC   P,SPACES            CLEAR PRINTLINE                              
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
                                                                                
         MVC   EDIPROG,=C'SUP'     MEDIA OCEAN FILE XFER: SUPPORT               
                                                                                
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(32),=C'SUB MEDIA OCEAN SUPPORT DOWNLOAD'               
                                                                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(04),=C'DSN '                                           
         MVC   EDISYST+09(44),0(R2)                                             
*                                  INSERT FILE NAME CREATED,                    
*                                     WITH GENERATION NUMBER                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(04),=C'FIL '                                           
         MVC   EDISYST+09(44),0(R2)                                             
*                                  INSERT FILE NAME CREATED,                    
*                                     WITH GENERATION NUMBER                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(07),=C'EXT TXT'                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RECORDS FOR THIS FILE ARE BUILT IN MYP, AND ARE OUTPUT                      
*        DIRECTLY FROM THERE.  NOTHING FANCY.                                   
*                                                                               
EXFILE1  DCB   DDNAME=EXFILE1,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02048,                                            X        
               BLKSIZE=08120,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
XCOMFACS DS    A                                                                
AIOAREA  DS    A                                                                
RELO     DS    F                                                                
COMMAND  DS    CL8                                                              
SAVEREGS DS    11F                                                              
         DS    0F                                                               
DUMMYREC DC    XL02'14'            LENGTH OF RECORD+4                           
         DC    XL02'00'                                                         
         DC    CL16'99999END OF DATA'                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**       INCLUDE FALINKBLK                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
*                                                                               
*                                                                               
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184REREP3C02 11/08/02'                                      
         END                                                                    
**  INSERT END  >>                                                              
