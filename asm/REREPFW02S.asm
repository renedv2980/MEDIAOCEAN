*          DATA SET REREPFW02S AT LEVEL 160 AS OF 09/23/03                      
*PHASE REFW02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REREPFW02 (REFW02) - UID PRINT ROUTINE            '             
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPFW02 -- SCAN ALL UID CODES, COMPARE TO CONTROL       *            
*                      FILE, REPORT DIFFERENCES                    *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* SEP22/03 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =  DO FIRST N RECORDS                            *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REFW02   CSECT                                                                  
         NMOD1 0,**FW02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
SW10     DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION RECORDS                      
*                                                                               
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP                                     
                                                                                
*                                                                               
*   OPEN CONTROL FILE                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AREC,0                                             
*                                                                               
         LA    RF,REC                                                           
         ST    RF,AREC             SAVE A(IO AREA FOR CONTROL RECS)             
*                                                                               
         B     EXIT                                                             
AREC     DS    A                                                                
         EJECT                                                                  
******************************************************************              
*  STATPROC: EXTRACT STATION RECORDS FOR KR, KH, CR              *              
*           WRITE OUT WITH NEW REP CODES                         *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTUKTYP,X'83'      SET PASSIVE KEY TYPE                         
         MVI   RSTUKSTP,X'08'      SET PASSIVE KEY SUBTYPE                      
         GOTO1 HIGHDIR                                                          
         B     STAT0040                                                         
STAT0020 EQU   *                                                                
         GOTO1 SEQDIR                                                           
STAT0040 EQU   *                                                                
*                                                                               
*   RUN FIRST 500 STATIONS ONLY                                                 
*                                                                               
                                                                                
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         CLI   QUESTOR+0,C'Y'      DO FIRST N RECORDS?                          
         BNE   STAT0050            NO                                           
         CLC   STACTR,=F'500'                                                   
         BH    STAT0800                                                         
STAT0050 EQU   *                                                                
         CLC   KEY(2),=X'8308'     STA UID PASSIVE KEY?                         
         BNE   STAT0800            NO  - FINISHED                               
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(8),=C'STA KEY:'                                              
**       MVC   P+10(27),KEY                                                     
**       GOTO1 REPORT                                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'99'          SET UP FOR CONTROL READ                      
         MVC   WORK+19(6),RSTUKUID                                              
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(8),=C'UID KEY:'                                              
**       MVC   P+10(25),WORK                                                    
**       GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                     
         L     R5,AREC                                                          
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(16),=C'99 KEY RETURNED:'                                     
**       MVC   P+20(25),WORK                                                    
**       MVC   P+50(25),0(R5)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         CLC   WORK(25),0(R5)      KEY FOUND?                                   
         BE    STAT0060            YES -                                        
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(16),=C'99 KEY NOT FND :'                                     
**       GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*   UID KEY NOT FOUND ON FILE. SET UP A 9A KEY TO CHECK                         
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'9A'          SET UP FOR CONTROL READ                      
         MVI   WORK+17,C'R'        SET TO 'RADIO'                               
         MVC   WORK+18(5),RSTUKSTA                                              
*                                                                               
*   ACTIVE PASSIVE WILL HAVE X'0000' AS DATE.  IF NOT THERE,                    
*        THERE IS NO ACTIVE PASSIVE FOR THIS STATION.                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                     
         CLC   WORK(25),0(R5)      KEY FOUND?                                   
         BE    STAT0120            YES -                                        
*                                                                               
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(14),=C'COMPARE: WORK='                                       
**       MVC   P+15(25),WORK                                                    
**       MVC   P+41(05),=C'REG5='                                               
**       MVC   P+46(25),0(R5)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         B     STAT0160            YES -                                        
*                                                                               
STAT0060 EQU   *                                                                
*                                                                               
**       GOTO1 REPORT                                                           
**       MVC   P+1(13),=C'99 KEY FOUND:'                                        
**       GOTO1 REPORT                                                           
*                                                                               
         LA    R5,28(R5)                                                        
         USING CRCLD,R5                                                         
*                                                                               
*   COMPARE THE STATION IN THE UID WITH THE 8308 STATION                        
*                                                                               
         CLC   CRCLCLL(4),RSTUKSTA REP UID FOR SAME STATION?                    
         BNE   STAT0080            NO  - STATION NOT FOUND                      
         CLC   CRCLCLL+5(1),RSTUKSTA+4   SAME MEDIA?                            
         BE    STAT0020            YES - GO BACK FOR NEXT                       
STAT0080 EQU   *                                                                
         MVC   P+1(29),=C'UID ON FILE, STATION DIFFERS:'                        
         MVC   P+34(2),RSTUKREP                                                 
         MVC   P+38(6),RSTUKUID                                                 
         MVC   P+46(4),=C'DDS='                                                 
         MVC   P+50(4),RSTUKSTA    DDS STATION                                  
         MVC   P+55(1),RSTUKSTA+4  DDS STATION MEDIA                            
         MVC   P+58(4),=C'MST='                                                 
         MVC   P+62(6),CRCLCLL     CONTROL STATION                              
         BAS   RE,STATHIST         CHECK HISTORY                                
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     STAT0020            GO BACK FOR NEXT                             
         DROP  R5                                                               
STAT0120 EQU   *                                                                
*                                                                               
*   UID NOT ON FILE.  STATION FOUND VIA 9A PASSIVE KEY BY CALLS                 
*                                                                               
         USING CT9ARECD,R5                                                      
*                                                                               
         MVC   P+1(20),=C'UID NOT ON FILE FOR:'                                 
         MVC   P+34(2),RSTUKREP                                                 
         MVC   P+38(6),RSTUKUID                                                 
         MVC   P+58(5),RSTUKSTA    DDS STATION                                  
         MVC   P+66(17),=C'UID FOR (XXXXX): '                                   
         MVC   P+75(5),RSTUKSTA                                                 
         MVC   P+84(6),CT9AUID                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     STAT0020                                                         
         DROP  R5                                                               
STAT0160 EQU   *                                                                
*                                                                               
*   UID NOT ON FILE.  STATION NOT FOUND VIA 9A PASSIVE KEY BY CALLS             
*                                                                               
         MVC   P+1(20),=C'UID NOT ON FILE FOR:'                                 
         MVC   P+34(2),RSTUKREP                                                 
         MVC   P+38(6),RSTUKUID                                                 
         MVC   P+58(5),RSTUKSTA    DDS STATION                                  
         MVC   P+66(34),=C'STATION NOT ACTIVE ON CURRENT FILE'                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     STAT0020                                                         
STAT0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  STATHIST:  CHECK HISTORY FOR THIS STATION                     *              
*                                                                *              
******************************************************************              
*                                                                               
STATHIST NTR1                                                                   
STHI0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    STHI0800            YES                                          
         CLI   0(R5),3             END OF RECORD?                               
         BE    STHI0040            YES                                          
         ZIC   RF,1(R5)            GET ELEMENT LENGTH                           
         AR    R5,RF                                                            
         B     STHI0020                                                         
STHI0040 EQU   *                                                                
         USING CRCHD,R5                                                         
         CLC   CRCHHST1,SPACES     ANY HISTORY IN 1ST SLOT                      
         BNH   STHI0800            NO                                           
         MVC   P+70(03),=C'H1='                                                 
         MVC   P+73(04),CRCHHST1                                                
         MVC   P+78(01),CRCHHST1+4                                              
         GOTO1 DATCON,DMCB,(3,CRCHHDT1),(5,P+81)                                
         CLC   CRCHHST2,SPACES     ANY HISTORY IN 2ND SLOT                      
         BNH   STHI0800            NO                                           
         MVC   P+91(03),=C'H2='                                                 
         MVC   P+94(04),CRCHHST2                                                
         MVC   P+99(01),CRCHHST2+4                                              
         GOTO1 DATCON,DMCB,(3,CRCHHDT2),(5,P+102)                               
STHI0800 EQU   *                                                                
         XIT1                                                                   
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
NEWREP   DS    CL2                 NEW REP CODE                                 
OLDREP   DS    CL2                 OLD REP CODE                                 
REPCODE  DS    CL2                                                              
STACTR   DS    F                                                                
SAVEKEY  DS    CL(L'KEY)                                                        
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4100              AREA FOR STATION RECORD                      
         DS    0D                                                               
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL4100                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE CTGENRAD          CONTROL RADIO RECORDS                        
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160REREPFW02S09/23/03'                                      
         END                                                                    
