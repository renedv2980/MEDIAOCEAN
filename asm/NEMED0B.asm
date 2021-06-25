*          DATA SET NEMED0B    AT LEVEL 104 AS OF 05/01/02                      
*PHASE T31E0BA,+0                                                               
*INCLUDE COVAIL                                                                 
*                                                                               
         TITLE 'T31EOB - MKT FIX REPORT'                                        
*****************************************************************               
* MARKET FIX REPORT                                                             
*                                                                               
* CHANGES MKT NUMBER ON UNIT RECORDS                                            
* DELETES OLD PROGRAM REC KEY AND ADDS ONE WITH NEW MKT NUMBER                  
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E0B   CSECT                                                                  
         NMOD1 0,**MKFX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3          ARGS TO PRINT                                
         USING THISD,R7                                                         
                                                                                
         CLI   MODE,VALKEY                                                      
         BE    VREC                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PRTREC                                                           
XIT      XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
VREC     MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                                                               
         LA    R2,SPLCLIH                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    SKIPDDS                                                          
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   EDINV                                                            
SKIPDDS  NETGO NVCLIALL,DMCB                                                    
         CLC   =C'ALL',SPLCLI                                                   
         BNE   EDINV                                                            
*                                                                               
*                                                                               
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB                                                    
         CLC   =C'ALL',SPLPRO                                                   
         BNE   EDINV                                                            
*                                                                               
         LA    R2,SPLESTH                                                       
         NETGO NVESTALL,DMCB                                                    
         CLC   =C'ALL',SPLEST                                                   
         BNE   EDINV                                                            
                                                                                
         LA    R2,SPLTSTH                                                       
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   EDINV                                                            
         MVC   TEST,8(R2)          TEST RUN?                                    
*                                                                               
* LOAD REQUESTED NETWORKS AND NEW MKT NUMBERS TO TABLE                          
         LA    R2,SPLNETH        POSITION CURSOR FOR ERROR CHK                  
         LA    R3,SWTBL          R3 -> TABLE OF TO BE SWITCHED NETS             
         USING SWTD,R3                                                          
EDIT10   CLI   5(R2),0                                                          
         BE    EDINV                                                            
         MVC   SWNET,8(R2)      STATION                                         
         OC    SWNET,SPACES                                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   EDINV                                                            
         NETGO NVGETFLD,DMCB,=F'9999'  RETURNS BINARY # IN R0                   
*                                      9999 IS MAX #                            
         STCM  R0,3,SWMKT         NEW MKT NUMBER                                
*                                                                               
*CHECK THAT STATION EXISTS                                                      
*                                                                               
         LA    R2,SPLNETH                                                       
         BAS   RE,SETSTAT                                                       
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVC   STAKEY+1(STAKEYLN-1),STAKEY                                      
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'N'        MEDIA                                        
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKCALL(4),SWNET                                                
         MVC   STAKAGY,NBSELAGY    AGENCY                                       
         MVC   STAKESV,KEY         SAVE THIS KEY                                
         GOTO1 HIGH                                                             
         CLC   KEY(STAKEYLN),KEYSAVE                                            
         BNE   EDINV                                                            
         DROP  R4                                                               
                                                                                
* NOW CHECK THAT REQUESTED MKT NUMBER DOES NOT ALREADY EXIST                    
         ZIC   R1,0(R2)            BUMP CURSOR FOR ERROR CHK                    
         AR    R2,R1                                                            
*                                                                               
         LA    R4,KEY                                                           
         USING MKTREC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVC   MKTKEY+1(MKTKEYLN-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                   RECORD TYPE                      
         MVI   MKTKMED,C'N'                    MEDIA                            
         EDIT  (B2,SWMKT),(4,MKTKMKT),FILL=0    MKT NUMBER                      
         MVC   MKTSV,MKTKMKT       SAVE EBCDIC MKT NUMBER                       
         MVC   MKTKAGY,NBSELAGY                AGENCY                           
         GOTO1 HIGH                                                             
         CLC   KEY(MKTKEYLN),KEYSAVE                                            
         BE    MKTERR                                                           
         DROP  R4                                                               
*                                                                               
         BAS   RE,SETUNT         RESET FILE FOR UNIT READ                       
         B     EDITEND           ONLY ONE STATION AT A TIME                     
*                                SINCE DIFFERENT NETWORKS CAN                   
*                                HAVE SAME PROGRAM CODES                        
                                                                                
         LA    R3,SWLEN(R3)            BUMP STATION TABLE                       
                                                                                
EDIT15   ZIC   R1,0(R2)            GET NEXT STATION/MKTNUM                      
         AR    R2,R1                                                            
         CLI   0(R2),9                                                          
         BNH   EDITEND             EOF                                          
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    EDIT15              SKIP IT                                      
         B     EDIT10                                                           
*                                                                               
*                                                                               
EDINV    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
MKTERR   MVC   CONHEAD(29),=C'*** STATION/MARKET EXISTS ***'                    
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
*                                                                               
                                                                                
EDITEND  LA    R2,SPLCLIH                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
SETSTAT  NTR1                                                                   
         MVC   LKEY,=H'15'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         MVC   SYSFIL,=C'STATION '                                              
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,NBAIO                                                        
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
*                                                                               
SETUNT   NTR1                                                                   
         MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVI   USEIO,C'N'                                                       
         MVC   AIO,NBAIO                                                        
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         EJECT                                                                  
* DO MY OWN READING/WRITING OF UNIT RECORDS                                     
* PUT CHANGED MKT/PROG CODES TO TABLE                                           
* AFTER UNITS, READ/WRITE PROGRAM RECORDS                                       
*                                                                               
PRTREC   DS    0H                                                               
*  GET STORAGE                                                                  
         LA    RE,NETS                MAX # OF NETWORKS                         
         LA    R3,STATLEN             LENGTH OF STATION DATA                    
         MR    R2,RE                                                            
         LA    RE,MAXPROG             # OF PROGRAMS PER STATION                 
         MR    R2,RE                                                            
         ST    R3,DMCB+4              R3->LEN OF REQUESTED STORAGE              
         ST    R3,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,ASTATBLN         L'TABLE                                      
         ST    RE,ASTATBL          TABLE ADDR                                   
         XCEF  (RE),(R3)           CLEAR TABLE                                  
********************************                                                
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         USING NURECD,R2                                                        
         MVI   NUKTYPE,X'04'       UNITS                                        
         MVC   NUKAM,NBACTAM                                                    
         GOTO1 HIGH                                                             
         B     P12                                                              
                                                                                
PSEQ     GOTO1 SEQ                                                              
*                                                                               
P12      CLC   KEY(2),KEYSAVE                                                   
         BNE   P50                                                              
         LA    R3,SWTBL            STATION/MKTNUM                               
         USING SWTD,R3                                                          
*                                                                               
P20      CLC   NUKNET,SWNET        NETWORK MATCH?                               
         BE    P30                 YES                                          
         LA    R3,SWLEN(R3)                                                     
         CLI   0(R3),X'FF'         EOF                                          
         BE    PSEQ                YES                                          
         B     P20                 NO                                           
*                                                                               
P30      GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL            R2-> UNIT KEY                                
         BE    P32                                                              
                                                                                
         MVC   P+1(13),=C'NO 01 ELEMENT'     **  HIGHLY UNLIKELY                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PSEQ                          **  AND GET NEXT REC               
                                                                                
P32      EQU   *                                                                
         USING NUMAINEL,R4         R4 -> UNIT REC X'01' ELEM                    
*                                                                               
************************************************************                    
* LOAD MKT/PROGRAM TO TABLE FOR PROGRAM REC SWITCHES                            
                                                                                
                                                                                
         L     R5,ASTATBL          TABLE OF SWITCHED STATIONS                   
         USING STATD,R5                                                         
         L     RE,ASTATBLN         LENGTH OF TABLE                              
         LA    RF,STATLEN          LENGTH OF ONE ENTRY                          
         SR    RE,RF               LEAVE LAST ENTRY EMPTY                       
         AR    RE,R5               RE -> END OF TABLE                           
P35      CR    R5,RE               EXCEEDED TABLE?                              
         BNH   *+6                                                              
         DC    H'0'                NEED MORE ROOM                               
         OC    STMKTNUM(8),STMKTNUM    IN CASE MKT=0                            
         BZ    P38                                                              
         CLC   STMKTNUM,NUMARKET   SAME MKT NUMBER?                             
         BNE   P37                                                              
         CLC   STPROG,NUKPROG      SAME PROGRAM?                                
         BNE   P37                                                              
         CLC   STNET,NUKNET        SAME NETWORK?                                
         BE    P37X                ALREADY HAVE IT                              
P37      LA    R5,STATLEN(R5)      BUMP TO NEXT STATION SLOT                    
         B     P35                                                              
P38      MVC   STMKTNUM,NUMARKET   SET MKT NUMBER                               
         MVC   STPROG,NUKPROG      SET PROGRAM CODE                             
         MVC   STNEWMKT,SWMKT      SET NEW MKT NUMBER                           
         MVC   STNET,NUKNET        STATION                                      
P37X     EQU   *                                                                
*********************************************************                       
                                                                                
* NOW FINISH UNIT REC                                                           
         MVC   NUMARKET,SWMKT      SET NEW MKT TO UNIT                          
         CLI   TEST,C'Y'           TEST RUN?                                    
         BE    P40                 YES                                          
         GOTO1 PUTREC              NO/PUTREC                                    
P40      L     R1,UNITCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,UNITCNT                                                       
         B     PSEQ                GET NEXT REC                                 
                                                                                
**************************************************************                  
* NOW HANDLE PROGRAM RECORDS                                                    
         DROP  R2,R3,R4,R5                                                      
P50      DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         L     R3,ASTATBL           TABLE OF MKT/PROGS                          
         USING STATD,R3                                                         
         OC    STMKTNUM(8),STMKTNUM   ANYTHING? (MKT#/PROGCODE)                 
         BZ    XPRT                NOPE                                         
P55      XC    KEY,KEY             YES                                          
         USING NPGKEY,R2                                                        
         MVC   0(2,R2),=X'0D20'             REC TYPE                            
P57      MVC   NPGKAM,NBACTAM               AGY                                 
         MVC   NPGKNET,STMKTNUM                                                 
         MVC   NPGKPROG,STPROG                                                  
P68      GOTO1 HIGH                                                             
         B     P72                                                              
P70      GOTO1 SEQ                                                              
*                                                                               
P72      CLC   KEY(11),KEYSAVE          AGY/MKT MATCH ?                         
         BNE   P80                                                              
         MVC   KEYSV,KEY           SAVE THE KEY                                 
         GOTO1 GETREC                                                           
*                                                                               
         DROP  R2                                                               
         USING NPGKEY,R4                                                        
         L     R4,NBAIO                                                         
         MVC   P+1(4),STNET            NETWORK                                  
         EDIT  (B2,NPGKNET),(4,P+7)    OLD MKT NUMBER                           
         MVC   P+12(6),NPGKPROG         PROGRAM                                 
         MVC   NPGKNET,STNEWMKT        NEW MARKET NUMBER                        
         EDIT  (B2,NPGKNET),(4,P+19)                                            
         GOTO1 SPOOL,DMCB,(R8)         WRITE TO PRINT LINE                      
*                                                                               
         CLI   TEST,C'Y'           TEST RUN?                                    
         BE    SKIPADD                                                          
         XC    MYDM(96),MYDM                                                    
         LA    R4,KEY+14                                                        
         L     R5,NBAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE ',(R4),(R5),MYDM,0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DISKSV,KEY+14       SAVE DISK ADDRESS                            
         DROP  R4                                                               
SKIPADD  EQU   *                                                                
* - HANDLE PASSIVE POINTER                                                      
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BNE   P80                                                              
         USING NPGELEM,R4                                                       
         LA    R2,KEY                                                           
         USING NPPKTYP,R2                                                       
         XC    KEY,KEY                                                          
         L     R5,NBAIO                                                         
         MVC   KEY(13),0(R5)           SET KEY                                  
         MVC   KEY(2),=X'0DA0'     PASSIVE POINTER                              
         MVC   NPPKDAY,NPGRDAY                                                  
         MVC   NPPKTIME,NPGTIME                                                 
         MVC   NPPKUNIQ,NPGUNIQ   UNIQUE CODE                                   
         MVC   NPPKDA,DISKSV       DISK ADDRESS                                 
         BAS   RE,ADDIR            AND WRITE IT                                 
                                                                                
*                                                                               
P75      MVC   KEY,KEYSV          RESET KEY                                     
         GOTO1 HIGH               RESET SEQ                                     
         B     P70                                                              
*                                                                               
P80      LA    R3,STATLEN(R3)      BUMP TO NEXT MKT SWITCH                      
         OC    STMKTNUM(8),STMKTNUM   EOF?                                      
         BNZ   P55                 NO                                           
*                                                                               
         DROP  R2                                                               
         BAS   RE,SETSTAT          SET TO READ STATION FILE                     
         MVC   KEY,STAKESV         READ STATION REC                             
         GOTO1 HIGH                                                             
         CLC   KEY(STAKEYLN),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,NBAIO                                                         
         USING STAREC,R2                                                        
         MVC   SMKT,MKTSV          SET NEW MARKET                               
         CLI   TEST,C'Y'                                                        
         BE    SKPADD                                                           
         L     R2,NBAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION ',KEY,(R2),0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
SKPADD   EQU   *                                                                
*                                                                               
         MVC   P+1(14),=C'UNITS SWITCHED'                                       
         EDIT  (B4,UNITCNT),(7,P+16)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XPRT                                                             
*                                                                               
XPRT     OC    ASTATBL,ASTATBL          FREE COVAIL STORAGE?                    
         BZ    XIT                                                              
         L     R2,ASTATBL                                                       
         L     R3,ASTATBLN                                                      
         GOTO1 =V(COVAIL),DMCB,=C'FREE',(R2),(R3)                               
         B     XIT                                                              
*                                                                               
                                                                                
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
ADDIR    NTR1                                                                   
         CLI   TEST,C'Y'                                                        
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SWTBL    DS    CL(SWLEN*NETS)        STATIONS TO BE SWITCHED                    
SWEND    DC    X'FFFF'                                                          
*                                                                               
STATBLNE EQU   STATLEN*NETS               STATIONS SWITCHED                     
*                                         (DATA LENGTH X # NETWORKS)            
NETS     EQU   5                           NUMBER OF NETWORKS                   
MAXPROG  EQU   3000                        MAX PROGS PER NETWORK                
                                                                                
SWTD     DSECT                     STATIONS TO BE SWITCHED                      
SWNET    DS    CL4                 STATION                                      
SWMKT    DS    CL2                 NEW MKT NUMBER                               
SWLEN    EQU   *-SWNET                                                          
*                                                                               
STATD    DSECT                     LIST OF SWITCHED STATIONS                    
STMKTNUM DS    CL2                 MKT NUMBER                                   
STPROG   DS    CL6                 PROGRAM CODE                                 
STNEWMKT DS    CL2                 NEW MKT NUMBER                               
STNET    DS    CL4                 NETWORK                                      
STATLEN  EQU   *-STMKTNUM                                                       
*                                                                               
THISD    DSECT                WORK DSECT                                        
TEST     DS    CL1                                                              
PASSIVES DS    CL1                                                              
KEYSV    DS    CL27                                                             
STAKESV  DS    CL15                                                             
MKTSV    DS    CL4                                                              
DISKSV   DS    CL4                                                              
ASTATBL  DS    A                                                                
ASTATBLN DS    F                                                                
UNITCNT  DS    F                                                                
MYDM     DS    CL96                                                             
                                                                                
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPROG                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104NEMED0B   05/01/02'                                      
         END                                                                    
