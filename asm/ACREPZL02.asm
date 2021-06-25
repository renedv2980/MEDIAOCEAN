*          DATA SET ACREPZL02  AT LEVEL 006 AS OF 03/23/15                      
*PHASE ACZL02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'PERSON RECORD REPORT'                                           
ACZL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZL**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACZL02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
REQF     DS    0H                                                               
*                                                                               
         USING ACCOMPD,R4                                                       
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
*                                                                               
         LA    RF,ONERTAB                                                       
         ST    RF,AONERTAB                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
LDGF     DS    0H                                                               
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARACHY ELEMENT                           
         MVC   LEVELA,ACHRLEVA     LEVEL LENGTHS                                
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
*                                                                               
         MVC   LVLALEN,ACHRLEVA                                                 
         ZIC   R0,LEVELA                                                        
         ZIC   RF,LEVELB                                                        
         SR    RF,R0                                                            
         STC   RF,LVLBLEN                                                       
*                                                                               
         IC    R0,LEVELB                                                        
         IC    RF,LEVELC                                                        
         SR    RF,R0                                                            
         STC   RF,LVLCLEN                                                       
*                                                                               
         IC    R0,LEVELC                                                        
         IC    RF,LEVELD                                                        
         SR    RF,R0                                                            
         STC   RF,LVLDLEN                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS AN ACCOUNT                                     *         
***********************************************************************         
*                                                                               
         USING ONERD,R2                                                         
PACC     L     R3,ADACC                                                         
         L     R2,AONERTAB                                                      
*                                                                               
         SR    R1,R1               EXCLUDE OVERHEAD ACCOUNTS                    
         IC    R1,LVLALEN          FIRST LEVEL ALL NINES?                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R3),NINES                                                    
         BE    XIT                                                              
*                                                                               
         LA    RF,3(R3)            2ND LEVEL ALL NINES?                         
         IC    R1,LEVELA                                                        
         AR    RF,R1                                                            
         IC    R1,LVLBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),NINES                                                    
         BE    XIT                                                              
*                                                                               
         LA    RF,3(R3)            3RD LEVEL HAS 3 NINES?                       
         IC    R1,LEVELC                                                        
         AR    RF,R1                                                            
         CLC   0(3,RF),NINES                                                    
         BE    XIT                                                              
*                                                                               
         MVC   ONERACC,3(R3)       SAVE ACCOUNT IN TABLE                        
         MVI   ONERUSED,0          RESET USED BYTE                              
         LA    R2,ONERLN(R2)                                                    
         MVI   0(R2),X'FF'         MARK EOT                                     
         ST    R2,AONERTAB                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
         USING PERRECD,R3                                                       
REQL     DS    0H                                                               
         MVI   FIRST,C'Y'                                                       
*                                                                               
         L     R3,AIO1                                                          
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,RCCOMPFL    COMPANY CODE                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO1,AIO1                             
         B     REQL15                                                           
REQL10   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO1,AIO1                             
REQL15   TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    REQL100             NEXT ONE                                     
*                                                                               
REQL20   DS    0H                                                               
         CLI   PERKTYP,PERKTYPQ    STILL LOOKING AT PERSON RECDS                
         BNE   REQL100                                                          
         CLC   PERKCPY,RCCOMPFL                                                 
         BNE   REQL100                                                          
         LR    R4,R3                                                            
         USING LOCELD,R4                                                        
         MVI   ELCODE,LOCELQ       X'83'                                        
         BAS   RE,GETEL                                                         
REQL25   BNE   REQL99              CAN'T FIND IT, NEXT ONE                      
*                                                                               
         LA    R2,SVPCODE          BUILD 1R ACCOUNT CODE                        
         MVC   SVPCODE,SPACES                                                   
         ZIC   RF,LVLALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),LOCOFF                                                   
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RF,LVLBLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),LOCDEPT                                                  
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RF,LVLCLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),LOCSUB                                                   
         AR    R2,RF                                                            
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RF,LVLDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PERKCODE                                                 
*                                                                               
         LA    R2,ONERTAB                                                       
REQL40   CLI   0(R2),X'FF'                                                      
         BE    REQL90                                                           
         CLC   ONERACC,SVPCODE                                                  
         BE    REQL50                                                           
         LA    R2,ONERLN(R2)                                                    
         B     REQL40                                                           
*                                                                               
REQL50   MVI   ONERUSED,X'01'      MARK USED                                    
REQL90   BAS   RE,NEXTEL           LOOK FOR NEXT ONE                            
         B     REQL25              LOOK FOR NEXT ONE                            
REQL99   B     REQL10              LOOK FOR NEXT RECORD                         
*                                                                               
REQL100  DS    0H                                                               
         ZAP   MISSCNT,=P'0'                                                    
         LA    R2,ONERTAB                                                       
REQL110  CLI   0(R2),X'FF'                                                      
         BE    REQL300                                                          
         CLI   ONERUSED,0                                                       
         BNE   REQL120                                                          
         MVC   P+1(12),ONERACC                                                  
         GOTO1 ACREPORT                                                         
         AP    MISSCNT,=P'1'                                                    
REQL120  LA    R2,ONERLN(R2)                                                    
         B     REQL110                                                          
*                                                                               
REQL300  DS    0H                                                               
         MVC   P(15),=CL15'TOTAL MISSING:'                                      
         EDIT  MISSCNT,(5,P+18),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              EXTERNAL ADDRESS LIST                                  *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    V(HELLO)                                                         
         DC    A(IO1)                                                           
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
EFFS     DC    48X'FF'                                                          
NINES    DC    12C'9'                                                           
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCBIG   DC    CL8'ACCBIG  '                                                    
*                                                                               
FIRST    DC    CL1' '              FIRST TIME PRINTING                          
SVPCODE  DC    CL12' '                                                          
SVOFFICE DC    CL12' '             SAVE OFFICE/DEPT                             
SVHIRED  DC    CL8' '                                                           
SVTERMD  DC    CL8' '                                                           
TMPOFFIC DC    CL12' '                                                          
TMPPCODE DC    CL12' '                                                          
*                                                                               
OFFORDR  DC    CL20'OFFICE/DEPT ORDER'                                          
PERORDR  DC    CL20'PERSON CODE ORDER'                                          
NAMORDR  DC    CL20'PERSON NAME ORDER'                                          
*                                                                               
SORTCARD DC    C'SORT FIELDS=(1,48,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=F,LENGTH=99 '                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              1R TABLE                                               *         
***********************************************************************         
*                                                                               
ONERTAB  DC    9000C' '                                                         
         DC    9000C' '                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD DSECT                                      *         
***********************************************************************         
SRECD    DSECT                                                                  
SRKEY    DS    0CL48                                                            
SRACCNT  DS    CL12                ACCOUNT                                      
SRLASTN  DS    CL18                PERSON LAST NAME  / FOR SORTING              
SRFIRSTN DS    CL18                PERSON FIRST NAME / FOR SORTING              
SRPNAME  DS    CL36                PERSON NAME                                  
SRLOCATN DS    CL5                 LOCATION                                     
SRFILTER DS    CL5                 FILTERS                                      
SRTSLOCK DS    CL3                 TIME SHEET LOCK DATE                         
SRSTATUS DS    CL1                 STATUS BYTE (FROM X'56' ELEM)                
SRCSTAT  DS    CL1                 ACTIVE/TERM/ETC (FROM X'56')                 
*                                                                               
SREND    DS    0C                                                               
SRECLNQ  EQU   *-SRECD                                                          
*                                                                               
         ORG   SRKEY               SORT ORDER BY PERSON NAME                    
SRLASTN3 DS    CL18                PERSON LAST NAME                             
SRFIRST3 DS    CL18                PERSON FIRST NAME                            
SRACCNT3 DS    CL12                OFFICE/DEPT/SUB-DEPT                         
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0H                                                               
         DS    CL1                                                              
PLINCODE DS    CL14                STAFF CODE                                   
         DS    CL2                                                              
PLINFILT DS    CL5                 FILTERS                                      
         DS    CL2                                                              
PLINNAME DS    CL36                NAME                                         
         DS    CL2                                                              
PLINLOCN DS    CL5                 LOCATION                                     
         DS    CL2                                                              
PLINHDTE DS    CL8                 HIRE DATE                                    
         DS    CL2                                                              
PLINTDTE DS    CL8                 TERM DATE                                    
         DS    CL2                                                              
PLINSTAT DS    CL1                 STATUS                                       
         DS    CL2                                                              
PLINEXEC DS    CL1                 EXECUTIVE                                    
         DS    CL2                                                              
PLINPROD DS    CL1                 PROD REQUIRED                                
         DS    CL2                                                              
PLINJOB  DS    CL1                 JOB REQUIRED                                 
         DS    CL2                                                              
PLINACT  DS    CL1                 ACTUAL AS STANDARD HOURS                     
         DS    CL2                                                              
PLINLDTE DS    CL8                 LOCK DATE                                    
         DS    CL2                                                              
PLINSDTE DS    CL8                 START DATE                                   
         DS    CL2                                                              
PLINEDTE DS    CL8                 END DATE                                     
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACZL02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
PRNTBL   DS    A                   PRNTBL                                       
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
CHOPCON  DS    A                   CHOPPER                                      
HELLO    DS    A                   HELLO                                        
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
ELCODE   DS    XL1                                                              
LAP      DS    XL1                                                              
COMMAND  DS    CL8                 USED IN DATAMGR CALL                         
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
ELEM     DS    CL255               ELEMENT BUFFER                               
*                                                                               
SVACT    DS    CL12                                                             
SVPERSN  DS    CL12                                                             
SVKEY    DS    CL42                                                             
LASTTOT  DS    PL8                                                              
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
RECORD   DS    XL1                 RECORD TYPE                                  
RECSTAFF EQU   X'10'               1R STAFF RECORD                              
RECPERSN EQU   X'01'               X'0F' PERSON RECORD                          
RECHIST  EQU   X'02'               SALARY HISTORY RECORD                        
*                                                                               
ACTION   DS    XL1                 MODE FOR CURRENT INPUT RECORD                
BUILD    EQU   X'80'               BUILD A NEW RECORD                           
UPDATE   EQU   X'40'               UPDATE AN OLD RECORD                         
*                                                                               
LVLALEN  DS    XL1                                                              
LVLBLEN  DS    XL1                                                              
LVLCLEN  DS    XL1                                                              
LVLDLEN  DS    XL1                                                              
*                                                                               
MISSCNT  DS    PL6                                                              
AONERTAB DS    A                                                                
SREC     DS    CL(SRECLNQ)                                                      
WRK2     DS    CL120                                                            
         EJECT                                                                  
ONERD    DSECT                                                                  
ONERACC  DS    CL12                1R ACCOUNT                                   
ONERUSED DS    X                   WAS ACCOUNT USED?  NON-ZERO=YES              
ONERLN   EQU   *-ONERD                                                          
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPZL02 03/23/15'                                      
         END                                                                    
