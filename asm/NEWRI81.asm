*          DATA SET NEWRI81    AT LEVEL 178 AS OF 05/04/07                      
*PHASE T32081C,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE SORTER                                                                 
*                                                                               
         TITLE 'T32081 - NETWORK UNIT I2 REQUESTOR'                             
************************************************************                    
*                                                                               
* THIS REPORT READS RECOVERY FILE AND GENERATES I2 REQUEST                      
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32081   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32081*,RA,R8                                                 
         USING T32081,RB,RA,R8                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         L     R1,ANETWS1        ANETWS1 AND 2  FOR CLIENT RECORD               
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         DROP  R5                                                               
*                                                                               
         L     R5,=A(MYIO)                                                      
         USING RECD,R5                                                          
         LA    RE,28(R5)           POINT PAST RECV HEADER                       
         ST    RE,AUNITREC         UNIT RECORD POINTER                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
*                                                                               
EXIT     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         BAS   RE,INIT         INITIALIZE                                       
         BAS   RE,NET1         READ RECOVERY                                    
         CLOSE (RECVIN)        CLOSE RECOVERY                                   
         FREEPOOL RECVIN                                                        
*                                                                               
         OPEN  (I2RECOV,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,SORTRECS         SORT THE RECORDS                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
ADDS     EQU   3                                                                
*                                                                               
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
* - RUN DATE                                                                    
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYC)     COMPRESSED                      
         GOTO1 DATCON,DMCB,(2,TODAYC),(3,TODAYB)     BINARY                     
         GOTO1 DATCON,DMCB,(2,TODAYC),(X'20',WORK)   YYMMDD PRINTABLE           
                                                                                
         MVC   CHANGDAT(2),WORK+2                                               
         MVI   CHANGDAT+2,C'/'                                                  
         MVC   CHANGDAT+3(2),WORK+4                                             
         MVI   CHANGDAT+5,C'/'                                                  
         MVC   CHANGDAT+6(2),WORK                                               
                                                                                
                                                                                
         MVI   MYSPACES,X'40'                                                   
         MVC   MYSPACES+1(L'MYSPACES-1),MYSPACES                                
         MVC   MYP,MYSPACES                                                     
         LA    RE,WHOTBL                                                        
         LA    RF,L'WHOTBL                                                      
         XCEF                                                                   
         MVI   WHOTBLX,X'FF'                                                    
         OPEN  (RECVIN,(INPUT))                                                 
INITX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************                        
* GETS RECORDS FROM THE RECOVERY FILE                                           
* IF SIGNIFICANT CHANGE TO REC UPDATE HISTORY RECORD                            
*                                                                               
********************************************************                        
         SPACE 2                                                                
NET1     NTR1                                                                   
*                                                                               
GET      L     R5,=A(MYIO)         REESTABLISH R5                               
         USING RECD,R5                                                          
*                                                                               
         LA    R1,RECVIN                                                        
         L     R0,=A(MYIO)                                                      
         PRINT GEN                                                              
         GET   (1),(0)                                                          
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   RFILTY,X'2A'       TEST UNTFILE                                  
         BNE   GET                                                              
*                                                                               
*  RECOVERY FILE CAN HAVE EARLY MORNING OFF-LINE CHANGES ON IT                  
*  WITH DATE OF PREVIOUS DAY,  ONN-LINE CHANGES HAVE TODAY'S DATE.              
*  SO BEST TO SKIP CHECK BELOW SINCE WE NEED OFF-LINE CHANGES ALSO.             
*                                                                               
         CLI   RTASKID,X'FF'       BACKED OUT AFTER A DUMP                      
         BE    GET                 YES/SKIP IT                                  
                                                                                
         CLI   RPRG,X'01'          PFM ?                                        
         BE    GET                 SKIP IT                                      
                                                                                
         L     R6,AUNITREC         POINT TO UNIT RECORD - R6                    
         USING NURECD,R6                                                        
         CLI   NUKTYPE,X'04'       TEST UNIT RECORD                             
         BNE   GET                                                              
         CLI   NUKSUB,X'C1'        SKIP TRAFFIC RECS                            
         BNL   GET                                                              
*                                                                               
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1                           
         CLI   NUKAM,X'E3'         GMMNY?                                       
         BNE   GET                                                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,WORK)     BINARY                      
         CLC   WORK(2),TODAYB      IF UNIT IS PAST THIS MONTH                   
         BH    GET                 THEN SKIP                                    
*                                                                               
* SET ZERO ELEMENT CODE AT E-O-R                                                
*                                                                               
         LH    R1,REC-4                                                         
         LA    R1,REC-4(R1)                                                     
         XC    0(2,R1),0(R1)                                                    
*                                                                               
NET10    CLI   RERUNTST,C'Y'       ARE WE DOING A RERUN?                        
         BNE   NET13                                                            
         CLC   RECVHDR,RERUNREC     HAVE WE REACHED KEY FOR RERUN ?             
         BNE   GET                 NO                                           
         MVI   RERUNTST,0          YES, SO HANDLE ALLRECS FROM HERE ON          
*                                                                               
NET13    L     R6,AUNITREC                                                      
         USING NURECD,R6                                                        
*                                                                               
         NI    MYFLAG,X'FF'-CLEARED                                             
         BAS   RE,CHKCLEAR         CHECK IF UNIT IS FULLY CLEARED               
         TM    MYFLAG,CLEARED      IS UNIT FULLY CLEARED?                       
         BO    GET                 YES - SKIP IT                                
*                                                                               
         NI    MYFLAG,X'FF'-UNALLOC                                             
         BAS   RE,CHKALLOC         CHECK IF UNIT IS ALLOCATED                   
         TM    MYFLAG,UNALLOC                                                   
         BO    GET                                                              
*                                                                               
         BAS   RE,GETSTTYP         GET STATION TYPE FROM UNIT REC               
*                                                                               
         MVC   RECDAT,RDATE        SAVE RECOVERY DATE                           
         MVC   RECTIME,RTIME       SAVE RECOVERY TIME                           
         MVC   RECFILTY,RECVHDR                                                 
*                                                                               
         CLI   RRECTY,COPY         TEST IF COPY                                 
         BNE   NET13B                                                           
*&&DO                                                                           
         CLC   NUKEY,=X'0493B0B7CB0100C5E2D7D5C3D3D3F0F4F20C02C3'               
         BNE   *+8                                                              
         BAS   RE,PRNTREC                                                       
*&&                                                                             
         MVC   RERUNREC,RECVHDR    FOR RERUN CHECKING IF DUMP                   
         B     NET15                                                            
*                                                                               
NET13B   CLI   RRECTY,CHANGE       TEST IF CHA                                  
         BNE   NET13C                                                           
         TM    NURSTAT,X'80'       IS CHANGE DELETED?                           
         BO    GET                 ..YES SKIP THIS                              
*&&DO                                                                           
         CLC   NUKEY,=X'0493B0B7CB0100C5E2D7D5C3D3D3F0F4F20C02C3'               
         BNE   *+8                                                              
         BAS   RE,PRNTREC                                                       
*&&                                                                             
         B     NET20                                                            
                                                                                
NET13C   MVC   RERUNREC,RECVHDR    FOR RERUN CHECKING IF DUMP                   
         CLI   RRECTY,ADDS         TEST IF ADD                                  
         BE    NET30                                                            
         B     GET                 IF NEITHER COPY/CHG/ADD - THEN SKIP          
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************                     
* - COPY                                                                        
***********************************************************                     
NET15    DS    0H                                                               
         MVC   PREVIOUS,RECFILTY   SAVE KEY OF CURRENT COPY                     
         MVI   PREVREC,COPY        SET FLAG                                     
*                                                                               
NET17    DS    0H                  PROCESS CURRENT COPY                         
         L     R4,=A(COPYTBL)                                                   
         BAS   RE,BLDTBL                                                        
         B     GET                                                              
         EJECT                                                                  
***********************************************************                     
* - CHANGE                                                                      
***********************************************************                     
NET20    DS    0H                                                               
         L     R1,=A(MYIO)               YES- CHECK IF OFFLINE CHANGE           
         USING RECD,R1                                                          
         CLI   RPRG,0                    OFFLINE CHANGE?                        
         BE    GET                       SKIP                                   
         DROP  R1                                                               
*                                                                               
         CLI   PREVREC,COPY       PREVIOUS REC MUST BE A COPY                   
         BNE   NET30               TREAT AS ADD                                 
*                                                                               
NET22    L     R4,=A(CHANGTBL)                                                  
         BAS   RE,BLDTBL           PUT UNIT DATA TO CHANGTBL                    
         BAS   RE,COMPARCT         COMPARE COPY TO CHANGE                       
         CLI   UPDTFLG,C'Y'        UPDATED?                                     
         BNE   NET24                                                            
*                                                                               
         XC    MYP,MYP                                                          
         MVC   MYP,MYSPACES                                                     
*!!!     MVC   MYP(20),=C'CHANGED UNIT        '                                 
         BAS   RE,ADDUNIT          ADD THIS UNIT TO SORT TABLE                  
*&&DO                                                                           
         L     RF,NBAIO                                                         
         CLC   0(20,RF),=X'0493B0B7CB0100C5E2D7D5C3D3D3F0F4F20C02C3'            
         BNE   NET24                                                            
*                                                                               
         L     R4,=A(COPYTBL)      R4 -> COPY                                   
         LA    R4,39(R4)                                                        
         MVC   MYP(15),=C'COPY TABLE     '                                      
         GOTO1 HEXOUT,DMCB,(R4),MYP+16,22                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         L     R4,=A(CHANGTBL)      R4 -> CHANGE                                
         LA    R4,39(R4)                                                        
         MVC   MYP(15),=C'CHA  TABLE     '                                      
         GOTO1 HEXOUT,DMCB,(R4),MYP+16,22                                       
         BAS   RE,PRINTIT                                                       
*&&                                                                             
NET24    MVI   PREVREC,0           CLEAR PREVREC FLAG                           
         B     GET                 GET NEXT RECORD                              
         EJECT                                                                  
*********************************************************                       
* - ADD                                                                         
*********************************************************                       
NET30    DS    0H                                                               
         BAS   RE,FILLDBLK                                                      
*                                                                               
         XC    MYP,MYP                                                          
         MVC   MYP,MYSPACES                                                     
*!!!     MVC   MYP(20),=C'ADDED UNIT          '                                 
         BAS   RE,ADDUNIT          ADD THIS UNIT TO SORT TABLE                  
*                                                                               
NET50    MVI   PREVREC,0           CLEAR PREVREC FLAG                           
         B     GET                                                              
         EJECT                                                                  
**************************************************                              
* - COMPARE COPY TO CHANGE                                                      
* - IF NOT =, ADD CHANGE TO SORT TABLE                                          
* - R5 -> NEW DATA,  R4 - > OLD DATE                                            
***************************************************                             
COMPARCT NTR1                                                                   
         MVI   UPDTFLG,0                                                        
*                                                                               
         L     R4,=A(COPYTBL)      R4 -> COPY                                   
         L     R5,=A(CHANGTBL)      R5 -> CHANGE                                
         LA    R5,UNITACST-UNUNTKEY(R5)   POINT R5 TO START OF DATA             
         USING UNITDATD,R4                                                      
*                                                                               
         CLC   UNITACST,0(R5)      ACTUAL COST                                  
         BNE   CMP50                                                            
         LA    R5,L'UNITACST(R5)                                                
*                                                                               
         CLC   UNITICST,0(R5)      INTEGRATED COST                              
         BNE   CMP50                                                            
         LA    R5,L'UNITICST(R5)                                                
*                                                                               
         CLC   UNITPRD,0(R5)       PRODUCT                                      
         BNE   CMP50                                                            
         LA    R5,L'UNITPRD(R5)                                                 
*                                                                               
         CLC   UNITDATE,0(R5)      DATE                                         
         BNE   CMP50                                                            
         LA    R5,L'UNITDATE(R5)                                                
*                                                                               
         CLC   UNITDAY,0(R5)       DAY                                          
         BNE   CMP50                                                            
         LA    R5,L'UNITDAY(R5)                                                 
*                                                                               
         CLC   UNITTIME,0(R5)      TIME                                         
         BNE   CMP50                                                            
         LA    R5,L'UNITTIME(R5)                                                
*                                                                               
         CLC   UNITLEN,0(R5)       LENGTH                                       
         BE    CMPEXIT                                                          
         LA    R5,L'UNITLEN(R5)                                                 
*                                                                               
CMP50    DS    0H                                                               
         MVI   UPDTFLG,C'Y'        ADD THIS TO SORT TABLE                       
*                                                                               
CMPEXIT  B     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* ADD UNIT ENTRY TO SORT TABLE                                                  
*********************************************************                       
ADDUNIT  NTR1                                                                   
         XC    SEQREC,SEQREC                                                    
         LA    R4,SEQREC                                                        
         USING SEQRECD,R4                                                       
*                                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         MVC   SEQAGY,NUALPHA      ALPHA AGENCY                                 
         MVI   SEQMED,C'N'         MEDIA                                        
         GOTO1 CLUNPK,DMCB,(CPROF+6,NUKCLT),SEQCLT                              
*                                                                               
         MVC   SEQPRD,=C'POL'      ALWAYS PRODUCT POL                           
*                                                                               
****>    BAS   RE,GETPRD           GET PRODUCT INFO                             
****>    MVC   SEQPGR,PRODGRP      PRODUCT GROUP CODE                           
****>    MVC   SEQPRD,PRODCODE     PRODUCT CODE                                 
****>    MVC   SEQPRD2,PRODCOD2    PIGGY BACK PRODUCT CODE                      
*                                                                               
         MVC   SEQSTA(4),NUKNET    STATION                                      
         MVI   SEQSTA+4,C'N'                                                    
*                                                                               
         MVC   SEQEST,=C'NO '      AND ALWAYS ESTIMATE NO                       
****>    CLI   STATYPE,C'C'                                                     
****>    BE    ADDU20                                                           
*                                                                               
****>    EDIT  NUKEST,SEQEST,FILL=0      ESTIMATE                               
*                                                                               
ADDU20   DS    0H                                                               
         MVC   SEQCOMP,STATYPE     STATION TYPE                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(10,WORK)                                
*                                                                               
         MVC   SEQSTART(2),WORK+6  YY - CALENDAR MOS                            
         MVC   SEQSTART+2(2),WORK  MM                                           
*                                                                               
         CLI   STATYPE,C'N'                                                     
         BE    ADDU30                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(0,WORK)                                 
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                  
         GOTO1 DATCON,DMCB,(0,WORK+12),(10,WORK)                                
*                                                                               
         MVC   SEQSTART(2),WORK+6  YY - BROADCAST MOS                           
         MVC   SEQSTART+2(2),WORK  MM                                           
*                                                                               
ADDU30   DS    0H                                                               
         MVC   SEQPAY,PAYDISC      PAY DISCREPENCY                              
*                                                                               
         TM    MYFLAG,HASINT       UNIT HAVE INTEGRATION?                       
         BZ    *+8                                                              
         MVI   SEQINT,C'Y'                                                      
*                                                                               
         OC    SEQREC,MYSPACES                                                  
         MVC   SEQKEY,NUKEY                                                     
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SEQREC                                   
*                                                                               
ADDUNITX DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
PRNTKEY  NTR1                                                                   
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
*!!      MVC   MYP(20),=C'ADD THIS TO SORT    '                                 
         GOTO1 HEXOUT,DMCB,NUKEY,MYP+20,20                                      
         BAS   RE,PRINTIT                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
PRNTREC  NTR1                                                                   
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(R6),(27,20),VPRINT,HEXOUT                       
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
********************************************************                        
*                                                                               
* GET PRODUCT INFORMATION                                                       
*                                                                               
*****  NO LONGER USED                                                           
*****  ONLY PRODUCT=POL NOW                                                     
*                                                                               
*********************************************************                       
GETPRD   NTR1                                                                   
         XC    PRODCODE,PRODCODE                                                
         XC    PRODCOD2,PRODCOD2                                                
         XC    PRODGRP,PRODGRP                                                  
         XC    SVPRDGRP,SVPRDGRP                                                
*                                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING CLTHDR,R4                                                        
*                                                                               
         LA    R4,CLIST                                                         
         LA    R3,220                                                           
*                                                                               
GPRD10   DS    0H                                                               
         CLC   3(1,R4),NBPRD       FOUND PRODUCT?                               
         BNE   *+14                                                             
         MVC   PRODCODE,0(R4)                                                   
         B     GPRD30                                                           
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT ENTRY IN CLIST                  
         BCT   R3,GPRD10                                                        
*                                                                               
         LA    R4,CLIST2                                                        
         LA    R3,35                                                            
*                                                                               
GPRD15   DS    0H                                                               
         CLC   3(1,R4),NBPRD       FOUND PRODUCT?                               
         BNE   *+14                                                             
         MVC   PRODCODE,0(R4)                                                   
         B     GPRD30                                                           
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT ENTRY IN CLIST                  
         BCT   R3,GPRD15                                                        
         B     GETPRDX                                                          
*                                                                               
GPRD30   DS    0H                                                               
         CLI   NBPRD2,0                                                         
         BE    GPRD50                                                           
*                                                                               
         L     R4,=A(MYIO2)                                                     
         LA    R4,CLIST                                                         
         LA    R3,220                                                           
*                                                                               
GPRD40   DS    0H                                                               
         CLC   3(1,R4),NBPRD2      FOUND PRODUCT?                               
         BNE   *+14                                                             
         MVC   PRODCOD2,0(R4)                                                   
         B     GETPRDX                                                          
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT ENTRY IN CLIST                  
         BCT   R3,GPRD40                                                        
*                                                                               
         LA    R4,CLIST2                                                        
         LA    R3,35                                                            
*                                                                               
GPRD45   DS    0H                                                               
         CLC   3(1,R4),NBPRD2      FOUND PRODUCT?                               
         BNE   *+14                                                             
         MVC   PRODCOD2,0(R4)                                                   
         B     GETPRDX                                                          
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT ENTRY IN CLIST                  
         BCT   R3,GPRD45                                                        
         DROP  R4                                                               
*                                                                               
GPRD50   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),PRODCODE   PRODUCT CODE                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING PRDHDR,R4                                                        
*                                                                               
         LA    R5,PGRP1                                                         
         CLI   0(R5),C'P'                                                       
         BE    GPRD60                                                           
*                                                                               
         LA    R5,PGRP2                                                         
         CLI   0(R5),C'P'                                                       
         BE    GPRD60                                                           
*                                                                               
         LA    R5,PGRP3                                                         
         CLI   0(R5),C'P'                                                       
         BE    GPRD60                                                           
*                                                                               
         LA    R5,PGRP4                                                         
         CLI   0(R5),C'P'                                                       
         BE    GPRD60                                                           
*                                                                               
         LA    R5,PGRP5                                                         
         CLI   0(R5),C'P'                                                       
         BNE   GETPRDX                                                          
         DROP  R4                                                               
*                                                                               
GPRD60   DS    0H                                                               
         MVC   SVPRDGRP,0(R5)      SAVE AWAY PRODUCT GROUP FOR LOOKUP           
         MVI   PRODGRP,C'P'        PRODUCT GROUP                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),NUKAM                                                   
         MVC   KEY+3(2),NUKCLT                                                  
         MVI   KEY+5,C'P'          GET '0000' ID REC FOR DISPLAY LENGTH         
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING PRGRECD,R4                                                       
*                                                                               
         LA    R4,PRGEL            1ST ELEMENT                                  
         DROP  R4                                                               
*                                                                               
         USING PRGEL01,R4                                                       
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   RF,PRGBK2LN                                                      
         AR    R1,RF               GET LENGTH OF ID                             
         STC   R1,WORK+12             LENGTH TO DISPLAY                         
         DROP  R4                                                               
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),SVPRDGRP+1  GROUP ID                                     
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         ZIC   R1,WORK+12                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRODCODE,WORK                                                    
         OC    PRODCODE,MYSPACES                                                
*                                                                               
GETPRDX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
********************************************************                        
*                                                                               
* GET STATION TYPE FROM UNIT REC                                                
*                                                                               
*********************************************************                       
GETSTTYP NTR1                                                                   
         XC    STATYPE,STATYPE                                                  
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   GSTTYPX                                                          
         USING NUSDRD,R6                                                        
*                                                                               
         MVC   STATYPE,NUSTATYP    STATION TYPE                                 
*                                                                               
GSTTYPX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
********************************************************                        
* GET RECORDS FROM SORTER                                                       
*********************************************************                       
SORTRECS NTR1                                                                   
         XC    PREVI2,PREVI2                                                    
         MVC   PREVI2,MYSPACES                                                  
*                                                                               
SRECS10  DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,4(R1)            A(SORTED RECORD)                             
         CHI   R6,0                FINISHED?                                    
         BE    SRECS50                                                          
*                                                                               
         NI    MYFLAG,X'FF'-HASINT                                              
*                                                                               
         USING SEQRECD,R6                                                       
*&&DO                                                                           
         XC    MYP,MYP                                                          
         MVC   MYP(L'SEQAGY),SEQAGY                                             
         MVC   MYP+3(L'SEQMED),SEQMED                                           
         MVC   MYP+5(L'SEQCLT),SEQCLT                                           
         MVC   MYP+10(L'SEQPGR),SEQPGR                                          
         MVC   MYP+12(L'SEQPRD),SEQPRD                                          
         MVC   MYP+17(L'SEQSTA),SEQSTA                                          
         MVC   MYP+25(L'SEQEST),SEQEST                                          
         MVC   MYP+30(L'SEQPRD2),SEQPRD2                                        
         MVC   MYP+35(L'SEQSTART),SEQSTART                                      
         MVC   MYP+42(L'SEQCOMP),SEQCOMP                                        
         MVC   MYP+44(L'SEQPAY),SEQPAY                                          
*                                                                               
         OC    MYP,MYSPACES                                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,SEQKEY,MYP+50,L'SEQKEY                               
         BAS   RE,PRINTIT                                                       
*&&                                                                             
         LA    R5,MYP                                                           
         USING ZRECD,R5                                                         
*                                                                               
         XC    MYP,MYP                                                          
         MVC   ZCODE,=C'I2'                                                     
         MVC   ZAGY,SEQAGY         AGENCY                                       
         MVC   ZMED,SEQMED         MEDIA                                        
         MVC   ZCLT,SEQCLT         CLIENT                                       
         MVC   ZPRD,SEQPRD         PRODUCT                                      
         MVC   ZSTA,SEQSTA         STATION                                      
         MVC   ZEST,SEQEST         ESTIMATE                                     
         MVC   ZPRD2,SEQPRD2       PIGGYBACK PRODUCT                            
         MVC   ZSTART,SEQSTART     MONTH OF SERVICE                             
         MVC   ZCOMPARE,SEQCOMP    STATION TYPE                                 
         MVI   ZOPT5,C'Y'          OPTION 5                                     
         MVC   ZUESTOR,=CL12'I2RECOVERY  '                                      
*                                                                               
         OC    MYP,MYSPACES                                                     
*                                                                               
         CLI   SEQINT,C'Y'         DOES THIS HAVE INTEGRATION?                  
         BNE   *+8                                                              
         OI    MYFLAG,HASINT                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,SEQKEY,MYP+85,L'SEQKEY                               
*                                                                               
****>    MVC   ZPGR,SEQPGR         PRODUCT GROUP (LOWER CASE 'P')               
****>    CLI   SEQPGR,C'P'                                                      
****>    BNE   *+8                                                              
****>    NI    ZPGR,X'FF'-X'40'    MUST BE LOWER CASE                           
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
         CLC   MYP(80),PREVI2                                                   
         BE    SRECS10                                                          
         MVC   PREVI2,MYP                                                       
*                                                                               
         TM    MYFLAG,HASINT       INTEGRATION?                                 
         BZ    SRECS30                                                          
         CLI   ZCOMPARE,C'N'       NETWORK?                                     
         BE    SRECS30                                                          
         MVI   ZCONTREQ,C'*'                                                    
*                                                                               
*                                                                               
* IF REQUEST HAS INTEGRATION THEN,                                              
*    IF NETWORK = GENERATE COST REQUEST                                         
*                 GENERATE 2ND REQUEST WITH 2ND REQUEST CARD W/ 'O'             
*                                                                               
*    ELSE       = GENERATE COST REQUEST WITH 2ND REQUEST CARD W/ 'I'            
*                                                                               
*                                                                               
SRECS30  DS    0H                                                               
         L     R1,=A(I2RECOV)                                                   
         LA    R0,MYP                                                           
         PUT   (1),(0)                                                          
*                                                                               
         TM    MYFLAG,HASINT       DOES THIS HAVE INTEGRATION?                  
         BZ    SRECS10                                                          
         CLI   ZCOMPARE,C'N'       NETWORK?                                     
         BE    SRECS40                                                          
*                                                                               
         XC    MYP,MYP             NON - NETWORK                                
         OC    MYP,MYSPACES        GENERATE COST REQUEST 2ND CARD W/ I          
         MVI   ZSTA+4,C'I'                                                      
*                                                                               
         L     R1,=A(I2RECOV)                                                   
         LA    R0,MYP                                                           
         PUT   (1),(0)                                                          
         B     SRECS10                                                          
*                                                                               
SRECS40  DS    0H                                                               
         MVI   ZCONTREQ,C'*'       GENERATE 2ND REQUEST                         
         L     R1,=A(I2RECOV)                                                   
         LA    R0,MYP                                                           
         PUT   (1),(0)                                                          
*                                                                               
         XC    MYP,MYP                                                          
         OC    MYP,MYSPACES                                                     
         MVI   ZSTA+4,C'O'         WITH 2ND REQUEST CARD OF INT ONLY            
*                                                                               
         L     R1,=A(I2RECOV)                                                   
         LA    R0,MYP                                                           
         PUT   (1),(0)                                                          
         B     SRECS10                                                          
*                                                                               
SRECS50  DS    0H                                                               
         XC    MYP,MYP                                                          
         MVC   MYP(80),MYSPACES                                                 
         MVC   MYP(2),=C'/*'      END REQUEST                                   
*                                                                               
         L     R1,=A(I2RECOV)                                                   
         LA    R0,MYP                                                           
         PUT   (1),(0)                                                          
*                                                                               
         CLOSE (I2RECOV)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SORTRX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
********************************************************                        
* CHECK IF UNIT IS ALLOCATED                                                    
*********************************************************                       
CHKALLOC NTR1                                                                   
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         CLI   NUPRD,0                                                          
         BNE   CHKALLX                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKALLX                                                          
*                                                                               
         OI    MYFLAG,UNALLOC      UNIT IS UNALLOCATED                          
*                                                                               
CHKALLX  DS    0H                                                               
         B     EXIT                                                             
********************************************************                        
*                                                                               
* CHECK IF UNIT IS FULLY CLEARED                                                
*                                                                               
*********************************************************                       
CHKCLEAR NTR1                                                                   
         XC    PATOTAL,PATOTAL                                                  
         XC    PITOTAL,PITOTAL                                                  
         MVI   PAYDISC,C'B'        DEFAULT                                      
*                                                                               
         NI    MYFLAG,X'FF'-(NOTACT+NOTINT)                                     
         NI    MYFLAG,X'FF'-HASINT                                              
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'12'        GET PAY ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   CCX                                                              
*                                                                               
         B     CC20                                                             
         USING NUBILD,R6                                                        
*                                                                               
CC10     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CC50                                                             
*                                                                               
CC20     DS    0H                                                               
         CLI   NUBILTYP,C'T'       ONLY GET TIME AND INTEGRATION                
         BE    CC30                                                             
         CLI   NUBILTYP,C'I'                                                    
         BE    CC40                                                             
         B     CC10                                                             
*                                                                               
CC30     DS    0H                                                               
         L     R4,NUBILGRS                                                      
         A     R4,PATOTAL                                                       
         ST    R4,PATOTAL                                                       
         B     CC10                                                             
*                                                                               
CC40     DS    0H                                                               
         OI    MYFLAG,HASINT       UNIT HAS INTEGRATION                         
         L     R4,NUBILGRS                                                      
         A     R4,PITOTAL                                                       
         ST    R4,PITOTAL                                                       
         B     CC10                                                             
*                                                                               
CC50     DS    0H                                                               
         L     R4,NBACTUAL                                                      
         L     R5,PATOTAL                                                       
         CR    R4,R5                                                            
         BE    *+8                                                              
         OI    MYFLAG,NOTACT                                                    
*                                                                               
         L     R4,NBINTEG                                                       
         L     R5,PITOTAL                                                       
         CR    R4,R5                                                            
         BE    *+8                                                              
         OI    MYFLAG,NOTINT                                                    
*                                                                               
CC70     DS    0H                                                               
         TM    MYFLAG,NOTACT                                                    
         BO    CC80                                                             
         TM    MYFLAG,NOTINT                                                    
         BO    CC80                                                             
         OI    MYFLAG,CLEARED      UNIT IS FULLY CLEARED                        
         B     CCX                                                              
*                                                                               
CC80     DS    0H                                                               
         TM    MYFLAG,NOTACT                                                    
         BZ    *+12                                                             
         TM    MYFLAG,NOTINT                                                    
         BO    CCX                                                              
*                                                                               
         MVI   PAYDISC,C'A'        DEFAULT TO JUST ACTUAL NOT EQUAL             
         TM    MYFLAG,NOTINT                                                    
         BNO   *+8                                                              
         MVI   PAYDISC,C'I'        INTEG NOT EQUAL                              
*                                                                               
CCX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************                           
* PULLS DATA FROM NETBLOCK AND UNIT RECORD          *                           
* R4 ->  AREA FILLED WITH UNIT DATA                 *                           
*****************************************************                           
BLDTBL NTR1                                                                     
         LR    R5,R4               NOTE R5 KEEPS POINTER TO START               
*                                  OF TABLE                                     
         LR    RE,R4               CLEAR TABLE                                  
         LA    RF,UNITLENE                                                      
         XCEF                                                                   
                                                                                
         USING UNITDATD,R4                                                      
                                                                                
* - CALL NETVALUE TO FILL NETBLOCK                                              
         BAS   RE,FILLDBLK                                                      
                                                                                
* - SAVE UNIT FIELDS                                                            
                                                                                
         L     R1,NBAIO                                                         
         MVC   UNUNTKEY,0(R1)         SAVE UNIT KEY                             
         MVC   UNRECTM,RECTIME        TIME OF CHANGE                            
         MVC   UNRECDAT,RECDAT        DATE OF CHANGE                            
*                                                                               
         MVC   UNITACST,NBACTUAL      ACTUAL COST                               
         MVC   UNITICST,NBINTEG       INTEGRATION COST                          
*                                                                               
         MVC   UNITPRD(2),NBPRD       PRODUCTS (PRD1 AND PRD2)                  
         CLI   NBPRDNO,0              IS IT MULTIPLE PRODUCTS?                  
         BE    *+10                                                             
         MVC   UNITPRD,NBPRDLST       MULTIPLE PRODUCTS                         
*                                                                               
         MVC   UNITDATE,NBACTDAT      DATE                                      
         MVC   UNITDAY,NBDAY          DAY                                       
         MVC   UNITTIME,NBTIME        TIME                                      
         MVC   UNITLEN,NBLEN          LENGTH                                    
*                                                                               
BTX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
FILLDBLK NTR1                                                                   
         MVC   NBAIO,AUNITREC       SET NBAIO TO START OF UNIT REC              
         MVI   NBESTOPT,0           NO ESTIMATED DEMOS                          
         MVI   NBACTOPT,0           NO ACTUAL DEMOS                             
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
FILLX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*****************************************                                       
*                                                                               
* SET TODAY'S DATE TO HISTORY RECORD                                            
*                                                                               
*****************************************                                       
ACTIVDAT NTR1                                                                   
         L     R2,=A(MYIO2)        GET HISTORY RECORD                           
         USING NHRECD,R2                                                        
         MVC   NHCHGDAT,TODAYC     TODAY'S COMPRESSED DATE                      
         MVC   NHCHGTIM,RECTIME    TIME OF CHANGE                               
         MVC   NHPKG,NBPACK        PACKAGE                                      
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
MYHIGH   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRALL                                                           
                                                                                
MYSEQ    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
                                                                                
MYDWRT   NTR1                                                                   
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     DIRALL                                                           
                                                                                
DIRALL   DS    0H                                                               
         CLI   FILE,0              WAS REP DIR OVERRIDEN ?                      
         BE    *+6                 NO                                           
         DC    H'0'                SHOULD NOT BE HERE                           
         MVC   FILE,=CL8'UNTDIR'   DEFAULT                                      
         ZIC   R4,UPDTBIT          SET OPTIONAL BIT                             
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,KEY,KEY,0                       
         MVI   FILE,0              CLEAR FILE FOR REP DEFAULT                   
         B     DDRECX                                                           
*                                                                               
MYGET    NTR1                                                                   
         LA    RF,=C'GETREC'                                                    
         ZIC   R4,UPDTBIT          SET OPTIONAL READ FOR UPDATE                 
         B     DDREC5                                                           
MYADD    NTR1                                                                   
         LA    RF,=C'ADDREC'                                                    
         B     DDREC5                                                           
MYPUT    NTR1                                                                   
         LA    RF,=C'PUTREC'                                                    
         B     DDREC5                                                           
*                                                                               
DDREC5   ST    RF,DMCB                                                          
         CLI   FILE,0              OVERIDE DEFAULT UNT FILE?                    
         BNE   DDREC7                                                           
         MVC   FILE,=CL8'UNTFILE'  NO                                           
         L     R3,=A(MYIO2)        HIST RECS READ INTO MYIO2                    
         LA    R2,KEY+21                                                        
         B     DDREC10                                                          
                                                                                
DDREC7   MVC   FILE,=CL8'SPTFILE'                                               
         LA    R2,KEY+14                                                        
         L     R3,=A(MYIO2)         SPOT RECS READ INTO MYIO2                   
         B     DDREC10                                                          
                                                                                
DDREC10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,,FILE,(R2),(R3),MYDMWORK,0                          
         MVI   UPDTBIT,0           DEFAULT NOT READ FOR UPDATE                  
         MVI   FILE,0              DEFAULT UNT FILE                             
*                                                                               
DDRECX   CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
         DS    0F                                                               
FILE     DS    CL8                                                              
MYDMWORK DS    CL96                                                             
UPDTBIT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
* - PRINTD ONLY HAS ONE PRINT LINE/USE MY OWN                                   
PRINTIT  NTR1                                                                   
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     EXIT                                                             
         DROP  R5                                                               
                                                                                
* - GET NAME TO MATCH PERSONAL ID                                               
* - EXPECTS - WORK(2) HAS AGENCY  WORK+2(2) HAS PERSONAL ID                     
* - RETURNS - NAME(8) IN WORK                                                   
OWHO     NTR1                                                                   
*                                                                               
         LA    R3,WHOTBL           IS NAME IN TABLE ?                           
         CLI   0(R3),0             FIRST TIME?                                  
         BE    OWHO05              YES                                          
                                                                                
OWHO00   CLC   WORK(4),0(R3)                                                    
         BNE   OWHO01                                                           
         MVC   WORK(8),4(R3)       YES                                          
         B     OWHOX                                                            
                                                                                
OWHO01   LA    R3,12(R3)           NO - BUMP TO NEXT ENTRY                      
         CLI   0(R3),0             ROOM IN TABLE                                
         BE    OWHO05              YES - ADD NEW ID/NAME HERE                   
         CLI   0(R3),X'FF'         NO MORE ROOM IN TABLE                        
         BNE   OWHO00                                                           
                                                                                
* - TABLE IS FULL - MOVE TBL DOWN AND DELETE LAST ENTRY                         
         LA    RF,WHOTBL               POINT RF TO LAST ENTRY                   
         LA    RF,(L'WHOTBL-12)(RF)                                             
         LA    R1,(L'WHOTBL-12)        R1 = LENGTH OF WHOTBL-12                 
         LA    RE,WHOTBL                                                        
         LA    RE,(L'WHOTBL-24)(RE)    POINT RE TO PENULTIMATE ENTRY            
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R3,WHOTBL            POINT R3 TO TBL START FOR NEW ENTRY         
*                                                                               
OWHO05   MVC   MYKEY,KEY           SAVE KEY                                     
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,WORK                                                     
         MVC   CT0KNUM,WORK+2                                                   
         L     R6,=A(MYIO3)                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   OWHO30                                                           
*                                                                               
         LA    RE,28(R6)                                                        
         SR    R0,R0                                                            
OWHO10   CLI   0(RE),0                                                          
         BE    OWHO30                                                           
         CLI   0(RE),X'C3'                                                      
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     OWHO10                                                           
         MVC   0(4,R3),WORK      ADD TO TABLE AGY/ID                            
         MVC   4(8,R3),2(RE)                  NAME                              
         MVC   WORK(8),2(RE)       PASS BACK TO CALLER                          
         B     OWHO40                                                           
                                                                                
OWHO30   MVC   WORK(8),=C'????????'                                             
                                                                                
OWHO40   DS    0H                                                               
         MVC   KEY,MYKEY           RESET KEY                                    
OWHOX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=6100,                                             X        
               MACRF=GM,                                               X        
               EODAD=EXIT                                                       
               SPACE                                                            
         SPACE                                                                  
UTL      DC    F'0',X'00'                                                       
SSB      DC    F'2'                                                             
*                                                                               
I2RECOV  DCB   DDNAME=I2RECOV,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
*                                                                               
* - COPY WITH NO CHANGE                                                         
COPYERR  NTR1                                                                   
         MVC   MYP(16),=C'COPY - NO CHANGE'                                     
         GOTO1 HEXOUT,DMCB,PREVIOUS,MYP+20,44                                   
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - CHANGE WITH NO PREVIOUS COPY                                                
CHANGERR NTR1                                                                   
         MVC   MYP(16),=C'CHANGE - NO COPY'                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - UNIT REC WITH HIST FLAG DELETED - SHOULD NOT BE !!!                         
DELETERR NTR1                                                                   
         MVC   MYP(16),=C'HIST DELETE ERR '                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SPROG 1,2                                                              
         SSPEC H1,1,C'CLIENT NAME'                                              
         SSPEC H2,1,C'AUDIT GROUP'                                              
         SSPEC H3,1,C'NETWORK'                                                  
         SSPEC H4,1,C'CHANGE DATE'                                              
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,120,PAGE                                                      
         SSPEC H1,50,C'NOTICE OF CHANGE'                                        
         DC    X'00'                                                            
                                                                                
HDRTN    NTR1                                                                   
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   H1+12(3),SRTKCLT                                                 
         MVC   H2+12(4),SRTKGRP                                                 
         MVC   H3+12(4),SRTKNET                                                 
         MVC   H4+12(8),CHANGDAT                                                
HDRTNX   B     EXIT                                                             
         DROP  R1                                                               
                                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,29,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=50'                                    
*                                                                               
         DC    CL8'**COPY**'                                                    
COPYTBL  DS    CL(UNITLENE)        COPY UNIT HIST DATA                          
*                                                                               
         DC    CL8'**CHANG*'                                                    
CHANGTBL DS    CL(UNITLENE)        CHANGED UNIT HIST DATA                       
*                                                                               
*                                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    0D                                                               
         DS    6100C                                                            
MYIOLNE  EQU   *-MYIO                                                           
                                                                                
         DC    CL8'**MYIO2*'                                                    
MYIO2    DS    0D                                                               
         DS    4048C                                                            
MYIO2LNE EQU   *-MYIO2                                                          
                                                                                
         DC    CL8'**MYIO3*'                                                    
MYIO3    DS    0D                                                               
         DS    4048C                                                            
MYIO3LNE EQU   *-MYIO3                                                          
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
UNITDATD DSECT                     UNIT DATA DSECT                              
*                                                                               
UNSTARTD DS    0C                                                               
UNUNTKEY DS    CL20                KEY OF COPY/CHANGE UNIT                      
UNRECTM  DS    XL4                 RECOVERY HEADER TIME                         
UNRECDAT DS    XL3                 RECOVERY HEADER DATE                         
UNREAS   DS    CL4                 REASON CODE                                  
UNUSER   DS    CL8                 USER CODE                                    
*                                                                               
UNITACST DS    XL4                 ACTUAL COST                                  
UNITICST DS    XL4                 INTEGRATED COST                              
UNITPRD  DS    XL6                 PRODUCT (MAX OF 6)                           
UNITDATE DS    XL2                 UNIT DATE                                    
UNITDAY  DS    XL1                 UNIT DAY                                     
UNITTIME DS    XL4                 START-END TIME                               
UNITLEN  DS    XL1                 LENGTH                                       
*                                                                               
UNITLENE EQU   *-UNSTARTD                                                       
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1    *** FROM EDIT MODULE - DO NOT MOVE                        
ONECLT   DS    CL2    ***                                                       
*                                                                               
AUNITREC DS    A                   UNIT RECORD POINTER                          
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVCLT2  DS    CL2                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
SAVEDEM  DS    CL6                                                              
USERNUM  DS    CL2                                                              
PWORK    DS    PL16                                                             
PREVIOUS DS    CL44                                                             
RECFILTY DS    CL100               RECOVERY HEADER+PART OF RECORD               
RECDAT   DS    CL3                 RECOVERY DATE                                
RECTIME  DS    CL4                 RECOVERY TIME                                
                                                                                
* FIELDS BELOW USED TO RERUN FILE IF HISTORY DIES                               
RERUNTST DS    CL1                 Y=RERUN                                      
RERUNREC DS    CL24                RECOVERY HEADER SAVE                         
*                                                                               
*                                                                               
*                                                                               
AGYNAMSV DS    CL33                                                             
AGYADRSV DS    CL33                                                             
PKCLISV  DS    CL2                 CLIENT FILTER                                
CLIENTNM DS    CL20                                                             
PRODNAME DS    CL20                                                             
ESTNAME  DS    CL20                                                             
*                                                                               
RCVRYSV  DS    CL60                                                             
TODAYB   DS    CL3                 BINARY                                       
TODAYC   DS    CL2                 COMPRESSED                                   
PREVFLG  DS    CL1                                                              
UPDTFLG  DS    CL1                                                              
*                                                                               
MYFLAG   DS    CL1                                                              
CLEARED  EQU   X'01'               IS UNIT FULLY CLEARED?                       
NOTACT   EQU   X'02'               PAY AND ACTUAL NOT EQUAL                     
NOTINT   EQU   X'04'               PAY AND INTEG NOT EQUAL                      
UNALLOC  EQU   X'08'               UNIT UNALLOCATED                             
HASINT   EQU   X'10'               UNIT HAS INTEGRATION                         
*                                                                               
WRITEFLG DS    CL1                                                              
SVSRTCLT DS    CL3                                                              
CHANGDAT DS    CL8                                                              
SAVAMC   DS    CL3                 SAVE AGY/MED/CLT OF HIST REC                 
MYKEY    DS    CL40                                                             
                                                                                
MYP      DS    CL132                                                            
MYSPACES DS    CL132                                                            
*                                                                               
WHOTBL   DS    CL(12*50)           ROOM FOR 50 ID(2)/AGY(2)/NAME(8)             
WHOTBLX  DS    CL1                 END OF TABLE = X'FF'                         
*                                                                               
*********************************************************                       
*                                                                               
SRTTBL   DS    0CL200    *** NOTE HARD CODED                                    
SRTKCLT  DS    CL3               * CLIENT           HIST KEY                    
SRTKGRP  DS    CL4               * AUDIT GROUP                                  
SRTKNET  DS    CL4               * NETWORK          HIST KEY                    
SRTKPRG  DS    CL6               * PROGRAM          HIST KEY                    
SRTKDAT  DS    CL2               * AIR DATE         HIST KEY                    
SRTKEST  DS    CL1               * ESTIMATE         HIST KEY                    
SRTKSUB  DS    CL1               * SUB LINE         HIST KEY                    
SRTKCHD  DS    CL2               * CHANGEDATE                                   
SRTKTIM  DS    CL4               * CHANGETIME                                   
         DS    CL3               * SPARE                                        
* SORT KEY = 30                                                                 
*                                                                               
SRDATA   DS    0CL2                DATA FIELDS START HERE                       
SRUNTDAT DS    CL2                 DATE                                         
SRPRGNM  DS    CL16                PROGRAM NAME                                 
SRROT    DS    CL1                 ROTATION                                     
SRTIM    DS    CL4                 TIME                                         
SRLEN    DS    CL1                 LENGTH                                       
SRCOST   DS    CL4                 ACTUAL                                       
SRPROD   DS    CL6                 PRODUCT                                      
SRREASON DS    CL3                 REASON                                       
SRSTATUS DS    CL1                 STATUS BYTE                                  
*                                  X'01' = REMOVED MKGD/MSD ELEM                
SRCOMN   DS    CL60                COMMENT                                      
SRBUYER  DS    CL8                 USER CODE                                    
SRMKGMSD DS    CL35                MAKEGOOD/MISSED                              
SRPRMT   DS    CL1                 PREEMPT C'Y'                                 
SRTAMC   DS    CL3                 AGY/MEDIA/CLI                                
         DS    CL24                SPARE                                        
SRTYPE   DS    CL1                 A=ADD/D=DELETE                               
SRTBLEN  EQU   *-SRTTBL            TOTAL SORT REC LENGTH                        
SRTDLEN  EQU   *-SRDATA            DATA LENGTH                                  
*                                                                               
PATOTAL  DS    F                   PAID ACTUALS                                 
PITOTAL  DS    F                   PAID INTEGRATION                             
*                                                                               
STATYPE  DS    CL1                 STATION TYPE (N,C,S)                         
PAYDISC  DS    CL1                 DISCREPENCY IN PAY                           
*                                  A = ACTUAL BUT NOT INTEG                     
*                                  I = INTEG BUT NOT ACTUAL                     
*                                  N = NEITHER ACTUAL OR INTEG                  
*                                                                               
PRODGRP  DS    CL1                 LOWER CASE P IF PRODUCT GROUP                
PRODCODE DS    CL3                 3 CHAR PRODUCT CODE                          
PRODCOD2 DS    CL3                 3 CHAR PRODUCT CODE                          
*                                                                               
SVPRDGRP DS    XL3                 PRODUCT GROUP FOR LOOKUP                     
*                                                                               
MOS      DS    CL6                                                              
*                                                                               
SEQREC   DS    XL50                                                             
*                                                                               
PREVI2   DS    CL132               PREVIOUS I2 REQUEST                          
*                                                                               
         PRINT ON                                                               
MYWRKDLE EQU   *-MYWORKD                                                        
*                                                                               
*                                                                               
SEQRECD  DSECT                                                                  
SEQAGY   DS    CL2                 AGENCY                                       
SEQMED   DS    CL1                 MEDIA                                        
SEQCLT   DS    CL3                 CLIENT                                       
SEQPGR   DS    CL1                 LOWER CASE P FOR PROD GROUP                  
SEQPRD   DS    CL3                 PRODUCT MNEMONIC                             
SEQSTA   DS    CL5                 STATION CALL LETTERS                         
SEQEST   DS    CL3                 ESTIMATE NUMBER (CABLE = 'NO')               
SEQPRD2  DS    CL3                 PIGGYBACK PRODUCT                            
SEQSTART DS    CL6                 MONTH OF SERVICE (YYMM__)                    
SEQCOMP  DS    CL1                 STATION TYPE                                 
SEQPAY   DS    CL1                 DISCREPENCIES IN THE PAY                     
*                                  A = ACTUAL BUT NOT INTEG                     
*                                  B = BOTH ACTUAL AND INTEG                    
*                                  I = INTEG BUT NOT ACTUAL                     
*                                                                               
SEQINT   DS    CL1                 Y = DOES THIS HAVE INTEGRATION               
SEQKEY   DS    XL20                UNIT KEY                                     
*                                                                               
SEQRECLN EQU   *-SEQMED                                                         
*                                                                               
*!!!!ZCTL     DS    XL26                                                        
ZRECD    DSECT                                                                  
ZAREA    DS    0CL80   COLUMN                                                   
ZPROG    DS    0CL2    ------                                                   
ZCODE    DS    CL2        1        PROGRAM CODE                                 
ZAGY     DS    CL2        3        AGENCY CODE                                  
ZMED     DS    CL1        5        MEDIA CODE (R/T)                             
ZCLT     DS    CL3        6        CLIENT CODE                                  
ZPGR     DS    CL1        9        PROCESS BY DIVISION                          
ZMGR     DS    CL1       10        PROCESS BY DISTRICT                          
ZCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
ZBYID    EQU   ZCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
ZPRD     DS    CL3       12        PRODUCT MNEMONIC                             
ZMKT     DS    CL4       15        MARKET NUMBER                                
ZSTA     DS    CL5       19        STATION CALL LETTERS                         
ZEST     DS    CL3       24        ESTIMATE NUMBER                              
ZESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
         DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
ZCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
ZSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
ZENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   *-3                                                              
ZPRD2    DS    CL3       35        PIGGYBACK PARTNER                            
ZSTART   DS    CL6       38        REQUEST START DATE                           
ZEND     DS    0CL6      44        REQUEST END DATE                             
ZTODAY   DS    CL6       44                                                     
ZBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
ZHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
ZRERATE  DS    CL1       56        RERATE TYPE                                  
ZCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
ZAFFIL   DS    CL1       58        AFFILIATION FILTER                           
ZPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
ZDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
ZDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
ZOPT1    DS    CL1       62        OPTION 1                                     
ZOPT2    DS    CL1       63        OPTION 2                                     
ZOPT3    DS    CL1       64        OPTION 3                                     
ZOPT4    DS    CL1       65        OPTION 4                                     
ZOPT5    DS    CL1       66        OPTION 5                                     
ZGRP     DS    CL2       67        GROUP                                        
*!!!ZFILTER  EQU   QGRP                FILTER TYPE/VALUE                        
ZUESTOR  DS    CL12      69        REQUESTOR NAME                               
*                                                                               
Z2AREA   DS    0CL80                                                            
         DS    CL21                                                             
Z2CALAND DS    CL1       21         BROADCAST/CALANDER MONTH                    
Z2MATCH  DS    CL1       22         MATCH INT AND ACTUAL/ OR JUST INT           
         DS    CL58                                                             
         EJECT                                                                  
         ORG   ZAREA+57                                                         
ZCMRCL   DS    CL8       58        COMMERCIAL FILTER                            
         ORG   ZAREA+29                                                         
ZREP     DS    CL3       30        DISCREPANCY REP                              
         ORG                                                                    
         PRINT OFF                                                              
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE NEGENHIST                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'178NEWRI81   05/04/07'                                      
         END                                                                    
