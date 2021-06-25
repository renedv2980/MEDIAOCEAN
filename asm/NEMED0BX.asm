*          DATA SET NEMED0BX   AT LEVEL 008 AS OF 05/01/02                      
*          DATA SET NEMED0B    AT LEVEL 046 AS OF 03/16/00                      
*PHASE T31E0BA,+0                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
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
         NETGO NVCLIALL,DMCB                                                    
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
                                                                                
         LA    R2,SPLTST                                                        
         CLI   0(R2),C'Y'          TEST RUN?                                    
         BNE   *+8                                                              
         MVI   TEST,C'Y'                                                        
*                                                                               
* LOAD REQUESTED NETWORKS AND NEW MKT NUMBERS TO TABLE                          
         LA    R2,SPLNETH                                                       
         LA    R3,SWTBL          73 -> TABLE OF TO BE SWITCHED NETS             
         USING SWTD,R3                                                          
EDIT10   CLI   5(R2),0                                                          
         BE    EDITEND                                                          
         MVC   SWNET,8(R2)      STATION                                         
         OC    SWNET,SPACES                                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   EDINV                                                            
         NETGO NVGETFLD,DMCB       RETURNS BINARY # IN R0                       
         STCM  R0,3,SWNMKT         NEW MKT NUMBER                               
                                                                                
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
*                                                                               
                                                                                
EDITEND  LA    R2,SPLCLIH                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
* DO MY OWN READING/WRITING OF UNIT RECORDS                                     
* PUT CHANGED MKT/PROG CODES TO TABLE                                           
* AFTER UNITS, READ/WRITE PROGRAM RECORDS                                       
*                                                                               
PRTREC   DS    0H                                                               
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
         GOTO1 =V(PRNTBL),DMCB,=C'ERROR',NBAIO,C'DUMP',40,=C'1D'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PSEQ                          **  AND GET NEXT REC               
                                                                                
P32      EQU   *                                                                
         USING NUMAINEL,R4         R4 -> UNIT REC X'01' ELEM                    
*                                                                               
************************************************************                    
* LOAD OLD/NEW MKT TO TABLE FOR PROGRAM REC SWITCHES                            
                                                                                
         MVC   WORK(2),NUMARKET    OLD MKT NUMBER                               
         MVC   WORK+2(2),SWNMKT    NEW MKT NUMBER                               
                                                                                
         LA    R5,PSTATBL          TABLE OF SWITCHED STATIONS                   
P35      OC    0(2,R5),0(R5)                                                    
         BZ    P38                                                              
         CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TBL FULL!                                    
         CLC   0(4,R5),WORK        ALREADY IN TABLE                             
         BE    LOADX                                                            
         LA    R5,4(R5)                                                         
         B     P35                                                              
P38      MVC   0(4,R5),WORK        SET OLE/NEW MKT                              
LOADX    EQU   *                                                                
                                                                                
* NOW FINISH UNIT REC                                                           
         MVC   NUMARKET,SWNMKT     SET NEW MKT TO UNIT                          
         CLI   TEST,C'Y'           TEST RUN?                                    
         BE    P40                 YES                                          
         GOTO1 PUTREC              NO/PUTREC                                    
P40      L     R1,UNITCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,UNITCNT                                                       
         B     PSEQ                GET NEXT REC                                 
                                                                                
**************************************************************                  
* NOW HANDLE PROGRAM RECORDS                                                    
         DROP  R2,R3,R4                                                         
P50      DS    0H                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         LA    R5,PSTATBL           TABLE OF OLD/NEW MKTS                       
         OC    0(4,R5),0(R5)       ANYTHING?                                    
         BZ    XPRT                NOPE                                         
P60      XC    KEY,KEY             YES                                          
         USING NPGKEY,R2                                                        
         MVC   0(2,R2),=X'0D20'             REC TYPE                            
         CLI   PASSIVES,C'Y'                                                    
         BNE   *+10                                                             
         MVC   0(2,R2),=X'0DA0'                                                 
         MVC   NPGKAM,NBACTAM               AGY                                 
         MVC   NPGKNET,0(R5)                MKT NUMBER                          
         GOTO1 HIGH                                                             
         B     P72                                                              
P70      GOTO1 SEQ                                                              
*                                                                               
P72      CLC   KEY(5),KEYSAVE          AGY/MKT MATCH ?                          
         BNE   P80                                                              
         MVC   KEYSV,KEY           SAVE THE KEY                                 
         OI    KEY+13,X'80'        MARK IT DELETE                               
         BAS   RE,WRTDIR           AND WRITE IT                                 
***      GOTO1 =V(PRNTBL),DMCB,=C'DEL',KEY,C'DUMP',20,=C'1D'                    
         EDIT  (B2,NPGKNET),(4,P+1),FLOAT=0                                     
         MVC   P+6(6),NPGKPROG                                                  
         NI    KEY+13,X'FF'-X'80'  REMOVE DELETE                                
         MVC   NPGKNET,2(R5)       SET NEW MARKET NUMBER                        
         EDIT  (B2,NPGKNET),(4,P+13),FLOAT=0                                    
         CLI   PASSIVES,C'Y'       IF PASSIVE KEY                               
         BE    SKIPSPL             DON'T PRINT IT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
SKIPSPL  EQU   *                                                                
***      GOTO1 =V(PRNTBL),DMCB,=C'ADD',KEY,C'DUMP',20,=C'1D'                    
         BAS   RE,ADDIR           AND WRITE IT                                  
         MVC   KEY,KEYSV           RESET KEY                                    
         GOTO1 HIGH                IF DELETING KEYS THIS HIGH GETS              
*                                  NEXT KEY                                     
         CLI   TEST,C'Y'           IF TEST - NO DELETE                          
         BE    P70                 SO READ SEQ                                  
         B     P72                                                              
*                                                                               
P80      LA    R5,4(R5)      BUMP TO NEXT OLD/NEW MKT                           
         OC    0(4,R5),0(R5)       EOF?                                         
         BNZ   P60                 NO                                           
         CLI   PASSIVES,C'Y'       HAVE WE HANDLED PASSIVE KEY?                 
         BE    XPRT                                                             
         MVI   PASSIVES,C'Y'                                                    
         B     P50                 AND DO PASSIVES                              
*                                                                               
*                                                                               
XPRT     MVC   P+1(13),=C'UNITS CHANGED'                                        
         EDIT  (B4,UNITCNT),(7,P+15)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
                                                                                
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
WRTDIR   NTR1                                                                   
         CLI   TEST,C'Y'                                                        
         BE    XIT                                                              
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         XIT1                                                                   
ADDIR    NTR1                                                                   
         CLI   TEST,C'Y'                                                        
         BE    XIT                                                              
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 DATAMGR,DMCB,=C'DMADD',FILENAME,KEY,KEY                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BUILDS LIST OF OLD/NEW MKT NUMBERS AS APPLICATION                             
* READS THROUGH UNITS 200 IS MAX OF OLD/NEW MKT NUMBERS                         
PSTATBL  DS    CL800        (CL2 OLDMKT# CL2 NEWMKT#)X 200                      
         DC    X'FFFF'                                                          
*                                                                               
SWTBL    DS    CL(SWLEN*NETS)        STATIONS TO BE SWITCHED                    
SWEND    DC    X'FFFF'                                                          
*                                         (DATA LENGTH X # NETWORKS)            
NETS     EQU   5                           NUMBER OF NETWORKS                   
                                                                                
SWTD     DSECT                     STATIONS TO BE SWITCHED                      
SWNET    DS    CL4                 STATION                                      
SWNMKT   DS    CL2                 NEW MKT NUMBER                               
SWLEN    EQU   *-SWNET                                                          
*                                                                               
*                                                                               
THISD    DSECT                WORK DSECT                                        
TEST     DS    CL1                                                              
PASSIVES DS    CL1                                                              
KEYSV    DS    CL27                                                             
UNITCNT  DS    F                                                                
                                                                                
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPROG                                                      
         PRINT ON                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NEMED0BX  05/01/02'                                      
         END                                                                    
