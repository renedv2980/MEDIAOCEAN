*          DATA SET REREPACTS  AT LEVEL 028 AS OF 05/01/02                      
*PHASE REACTA                                                                   
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE DATCON                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'REACT - REP FILE PROGRAM ACTIVITY REPORT'                       
         PRINT NOGEN                                                            
*******************************************************************             
*                                                                 *             
*        REREPACT (REACT) --- REP FILE ACTIVITY REPORT            *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 04DEC90 (EFJ) --- ORIGINAL DEVELOPMENT                          *             
*                                                                 *             
* 23APR92 (SKU) --- ADD ADDRESS FOR AGENCY                        *             
*                                                                 *             
* 10AUG92 (SKU) --- ADD LEAVE DATE AND 'CONTRACT=NO' STATUS TO    *             
*                   STATION RECORD PRINT                          *             
*                                                                 *             
* 16FEB93 (BU ) --- UPGRADE UTL ACCESS FOR > 1 REP SYSTEM #       *             
*                                                                 *             
* 29JUL96 (BU ) --- CLEAR 'SCANNER' ARGUMENTS                     *             
*                                                                 *             
* 11JUN97 (BU ) --- PUT IN FIX TO SKIP CBSNY/SYS# 6490 (X'159A')  *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
         ENTRY UTL                                                              
REACT    CSECT                                                                  
         NBASE 0,**REACT*,=V(REGSAVE),RA                                        
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         BAS   RE,GETCARD          PARSE INPUT CARDS                            
         BAS   RE,FOPEN            OPEN FILES                                   
         BAS   RE,RDRCV            RD RECV TAPE                                 
         BAS   RE,PROCESS          PROCESS RECV RECS                            
         B     EXXBASE             GO HOME                                      
         EJECT                                                                  
*                                                                               
* ROUTINE TO PARSE CONTROL CARDS - IE: REP=PP,PQ=IDIDIDID                       
GETCARD  NTR1                                                                   
         L     R2,AREPTBL                                                       
         USING REPTBLD,R2                                                       
GC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         EOF?                                         
         BE    GCX                                                              
         CLC   =C'REP=',CARD                                                    
         BNE   GC20                                                             
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),(2,WORK),C',=,='                    
         CLI   DMCB+4,2                                                         
         BE    *+6                                                              
         DC    H'0'                PQ PARAM NOT PRESENT                         
         LA    R3,WORK             SACNNER BLOCK                                
         CLC   =C'REP',12(R3)                                                   
         BE    *+6                                                              
         DC    H'0'                INVALID PARAM                                
         MVC   RTREP,22(R3)        SAVE REP POWER CODE                          
         LA    R3,32(R3)           NEXT SCANNER BLOCK                           
         CLC   =C'PQ',12(R3)                                                    
         BE    *+6                                                              
         DC    H'0'                INVALID PARAM                                
         MVC   RTPQ,22(R3)         SAVE PQ ID                                   
         XC    RTRC,RTRC           CLEAR COUNTER                                
         LA    R2,RTLEN(R2)        NEXT TABLE ENTRY                             
         B     GC10                                                             
GC20     EQU   *                                                                
         CLC   =C'ID=',CARD                                                     
         BNE   GC30                                                             
         MVC   SAVNAME,CARD+3                                                   
         B     GC10                                                             
GC30     EQU   *                                                                
         CLC   =C'LOCAL=',CARD                                                  
         BNE   GC10                                                             
         MVC   SAVLOCAL,CARD+6                                                  
         B     GC10                                                             
GCX      DS    0H                                                               
         MVI   0(R2),X'FF'         SET END-OF-TABLE MARKER                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO OPEN FILES                                                         
FOPEN    NTR1                                                                   
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,IO                   
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         LA    R1,IO                                                            
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,REC                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO READ RECOVERY TAPE INTO SORTER                                     
RDRCV    NTR1                                                                   
RR10     DS    0H                                                               
         XC    SORTKEY(36),SORTKEY                                              
         XCEF  REC,4200                                                         
         LA    R0,RSPARE                                                        
         GET   RECVIN,(R0)                                                      
*                                                                               
* CHECK FOR REP-FILE AND CHG/ADD                                                
         CLI   RFILTY,X'82'        REPFILE?                                     
         BNE   RR10                                                             
         CLI   RRECTY,1            COPY?                                        
         BE    RR10                NOT INTERESTED                               
*                                                                               
* CHECK THAT THIS IS REC-TYPE WE'RE INTERESTED IN                               
         LA    R2,RECTBL                                                        
         CLC   RKEY(1),8(R2)       ONE WE WANT?                                 
         BE    RR20                                                             
         LA    R2,L'RECTBL(R2)                                                  
         CLI   0(R2),X'FF'         END-OF-TABLE?                                
         BNE   *-18                                                             
         B     RR10                NOT ONE WE WANT                              
*                                                                               
* CHECK THAT THIS ADD/CHANGE WAS MADE BY AGY WE'RE INTERESTED IN                
RR20     L     R3,AREPTBL                                                       
         USING REPTBLD,R3                                                       
         ZIC   R4,9(R2)            R4=DISP TO REP CODE                          
         LA    R4,RKEY(R4)         R4 TO REP CODE                               
         CLC   RTREP,0(R4)                                                      
         BE    RR30                                                             
         LA    R3,RTLEN(R3)                                                     
         CLI   0(R3),X'FF'                                                      
         BNE   *-18                                                             
         B     RR10                NOT ONE WE WANT                              
*                                                                               
* BUILD SORTKEY AND PUT REC TO SORTER                                           
RR30     EQU   *                                                                
         CLI   8(R2),2             STATION RECORD?                              
         BNE   RR32                NO                                           
*                                                                               
*   STATION RECORD FOUND:  IF 'MASTER STATION' RECORD, IGNORE IT                
*        THIS IS A HARD-CODED TEST, BECAUSE JOB DUMPED.  PROPERLY,              
*        THE REP RECORD SHOULD BE READ, AND CHECKED TO SEE IF THIS              
*        IS A MASTER REP.  BUT BECAUSE THIS IS A VERY SMALL LIST,               
*        AND EXCEEDINGLY UNLIKELY TO CHANGE, I PLUGGED IT IN THIS               
*        WAY.  NOT TOO ELEGANT.    BILL UHR/JUN29/99                            
*                                                                               
         CLC   =C'MS',RTREP        MASTER STATION RECORD?                       
         BE    RR10                YES - SKIP IT                                
         CLC   =C'K3',RTREP        MASTER STATION RECORD?                       
         BE    RR10                YES - SKIP IT                                
         CLC   =C'MR',RTREP        MASTER STATION RECORD?                       
         BE    RR10                YES - SKIP IT                                
         CLC   =C'IR',RTREP        MASTER STATION RECORD?                       
         BE    RR10                YES - SKIP IT                                
RR32     EQU   *                                                                
         MVC   SREP,RTREP                                                       
         MVC   SKEY,RKEY                                                        
         MVC   SDATE,RDATE                                                      
         MVC   STIME,RTIME                                                      
*                                                                               
*   TEST OUTPUT                                                                 
*        MVC   P+1(128),SORTKEY                                                 
*        GOTO1 =V(PRINTER)                                                      
*   TEST OUTPUT END                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         B     RR10                                                             
         DROP  R3                                                               
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE (RECVIN)                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO PROCESS SORTER RECS                                                
PROCESS  NTR1                                                                   
         MVC   TITLE,=CL60'F I L E  A C T I V I T Y  R E P O R T'               
         MVC   MID2(L'DHEAD),DHEAD                                              
*                                                                               
* GET RECORD FROM SORTER                                                        
P05      DS    0H                                                               
         XCEF  SORTKEY,4272                                                     
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R0,15,DMCB+4        'FROM' ADDRESS                               
         BZ    PX                  NO MORE RECS                                 
*                                                                               
*   TEST                                                                        
*        MVC   P+1(128),0(R0)                                                   
*        GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         LH    R1,=Y(SORTLEN)      'FROM' LENGTH                                
         LA    RE,SORTKEY          'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
*                                                                               
* CK IF SAME REP - IF NOT, OPEN PQ...                                           
         CLC   OLDREP,SREP                                                      
         BE    *+8                                                              
         BAS   RE,OPENPQ                                                        
*                                                                               
* FIND REC TYPE IN RECTBL                                                       
         LA    R2,RECTBL                                                        
         CLC   RKEY(1),8(R2)       ONE WE WANT?                                 
         BE    *+18                                                             
         LA    R2,L'RECTBL(R2)                                                  
         CLI   0(R2),X'FF'         END-OF-TABLE?                                
         BNE   *-18                                                             
         DC    H'0'                IT BETTER BE HERE                            
         MVC   P(8),0(R2)          REC TYPE                                     
*                                                                               
* PICK UP RECORD KEY AND DATA                                                   
         L     RE,10(R2)           A(REC-TYPE ROUTINE)                          
         BR    RE                                                               
STA      DS    0H                                                               
         MVC   P+11(4),RSTAKSTA                                                 
         LA    R3,P+14             DISPLAY BAND                                 
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'-'                                                       
         MVC   2(1,R3),RSTAKSTA+4                                               
         CLI   2(R3),C' '          TV???                                        
         BNE   *+8                                                              
         MVI   2(R3),C'T'                                                       
*                                                                               
         MVC   P+74(20),RSTAMKT    PICK UP MARKET AND INTERFACE CODES           
         LA    R6,RKEY                                                          
         MVI   DATADISP+1,34                                                    
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   STA10                                                            
         USING RSTAXXEL,R6                                                      
         MVC   P+94(10),RSTAOSI                                                 
         DROP  R6                                                               
*                                                                               
STA10    DS    0H                  SHOW LEAVE DATE                              
         LA    R6,RKEY                                                          
         USING RSTAREC,R6                                                       
         OC    RSTAEND,RSTAEND                                                  
         BZ    STA20                                                            
         GOTO1 =V(DATCON),DMCB,(X'03',RSTAEND),(8,P+106),0                      
*                                                                               
STA20    DS    0H                  SHOW STATION STATUS                          
         TM    RSTASTAT,X'40'                                                   
         BZ    P10                                                              
         MVC   P+116(11),=C'CONTRACT=NO'                                        
         B     P10                                                              
         DROP  R6                                                               
*                                                                               
SAL      DS    0H                                                               
         MVC   P+11(3),RSALKSAL                                                 
         MVC   P+74(20),RSALNAME   SALESMAN NAME                                
         B     P10                                                              
*                                                                               
GRP      DS    0H                                                               
         MVC   P+11(2),RGRPKGRP                                                 
         MVC   P+74(10),RGRPNAME   GROUP NAME                                   
         MVC   P+87(10),RGRPSBNM   SUB-GROUP NAME                               
         B     P10                                                              
*                                                                               
ADV      DS    0H                                                               
         MVC   P+11(4),RADVKADV                                                 
         MVC   P+74(20),RADVNAME   ADVERTISER NAME                              
         B     P10                                                              
*                                                                               
PRD      DS    0H                                                               
         MVC   P+11(4),RPRDKADV                                                 
         MVC   P+16(3),RPRDKPRD                                                 
         MVC   P+74(20),RPRDNAME   PRODUCT NAME                                 
         B     P10                                                              
*                                                                               
AGY      DS    0H                                                               
         MVC   P+11(4),RAGYKAGY                                                 
         MVC   P+16(2),RAGYKAOF                                                 
         MVC   P+74(20),RAGYNAM1   AGENCY NAME                                  
         MVC   P+74(20),RAGYNAM1   AGENCY NAME                                  
         MVC   P+95(20),RAGYADD2   AGENCY ADDRESS                               
         MVC   P+116(2),RAGYSTAT   STATE CODE                                   
         MVC   P+119(10),RAGYZIP   ZIP CODE                                     
         B     P10                                                              
*                                                                               
* GET CHANGE DATE                                                               
P10      DS    0H                                                               
         GOTO1 =V(DATCON),DMCB,(X'03',RDATE),(10,P+22),0                        
*                                                                               
* GET CHANGE/ADD TIME                                                           
         MVC   DUB(4),RTIME                                                     
         AP    DUB(4),=P'80000'       ADJUST SYS TIME TO REAL TIME              
         UNPK  WORK(6),DUB(4)                                                   
         MVC   P+33(2),WORK                                                     
         MVI   P+35,C'.'                                                        
         MVC   P+36(2),WORK+2                                                   
*                                                                               
* PICK UP ACTION (ADD OR CHANGE)                                                
         CLI   RRECTY,2            CHANGE?                                      
         BNE   *+14                                                             
         MVC   P+43(6),=C'CHANGE'                                               
         B     *+10                                                             
         MVC   P+43(3),=C'ADD'                                                  
*                                                                               
* SHOW SIGN-ON ID                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,RUSER                                                    
         MVC   SAVUTL,UTL+4        SAVE PRESENT UTL SE                          
         MVI   UTL+4,X'0A'         INSERT UTL SE OF CONTROL                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
         CLC   KEY(25),IO          GET ONE WANTED?                              
         BE    P15                                                              
*        DC    H'0'                                                             
         MVC   P+52(8),=C'USER NOF'                                             
         MVC   UTL+4(1),SAVUTL                                                  
         B     P17                                                              
P15      EQU   *                                                                
         MVC   UTL+4(1),SAVUTL                                                  
         LA    R6,IO                                                            
         MVI   DATADISP+1,28                                                    
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+52(8),2(R6)                                                    
         DROP  R3                                                               
P17      EQU   *                                                                
*                                                                               
* SHOW TERMINAL-ID (LUID)                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTTREC,R3                                                        
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTREC+23(2),RTRM                                                
         MVC   SAVUTL,UTL+4        SAVE PRESENT UTL SE                          
         MVI   UTL+4,X'0A'         INSERT UTL SE OF CONTROL                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
         CLC   KEY(25),IO          GET ONE WANTED?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UTL+4(1),SAVUTL                                                  
         LA    R6,IO                                                            
         MVI   DATADISP+1,28                                                    
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL                                                         
         BNE   P20                 CK X'03'                                     
         MVC   P+63(8),3(R6)                                                    
         B     P30                                                              
P20      MVI   ELCODE,X'03'        USE X'03' IF NO X'26'                        
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+63(8),2(R6)                                                    
         DROP  R3                                                               
P30      DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
         B     P05                                                              
PX       DS    0H                                                               
         CLI   CLSPQ,0             WAS ANY PQ OPENED?                           
         BE    PX10                NO                                           
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
PX10     GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO OPEN (AND CLOSE) PQ AND GET REP NAME FROM REP REC                  
OPENPQ   NTR1                                                                   
         CLI   CLSPQ,0             IS PQ OPEN? (FIRST TIME)                     
         BE    OP10                                                             
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
*                                                                               
* FIND WHICH REP                                                                
OP10     DS    0H                                                               
         MVI   CLSPQ,1             SET PQ OPEN                                  
         L     R3,AREPTBL                                                       
         USING REPTBLD,R3                                                       
         CLC   RTREP,SREP                                                       
         BE    *+18                                                             
         LA    R3,RTLEN(R3)                                                     
         CLI   0(R3),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
*                                                                               
* GET REP NAME FOR REPORT HDR                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,01                                                           
         MVC   KEY+25(2),RTREP                                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 =V(DATAMGR),DMCB,(DMINBITS,=C'DMRDHI'),=C'REPDIR',KEY,  X        
               KEY                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(DMINBITS,=C'GETREC'),=C'REPFILE',     X        
               KEY+28,IO,(0,DMWORK)                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         USING REPD,R6                                                          
         MVC   MID1+39(33),RREPNAME                                             
         DROP  R6                                                               
*                                                                               
* GET USER-ID # TO FIND WHICH PQ                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,RTPQ                                                      
         OC    CTIKID,SPACES                                                    
         DROP  R3,R4                                                            
         MVC   SAVUTL,UTL+4        SAVE PRESENT UTL SE                          
         MVI   UTL+4,X'0A'         INSERT UTL SE OF CONTROL                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
         CLC   KEY(25),IO          GET ONE WANTED?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UTL+4(1),SAVUTL                                                  
         LA    R6,IO                                                            
         MVI   DATADISP+1,28                                                    
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SAVLOCAL,C'Y'       LOCAL TEST/NO REMOTE?                        
         BE    OPX                 YES                                          
         L     R8,VREMOTEC                                                      
         USING REMOTED,R8                                                       
         MVC   REMOTAOP,=V(PQOPEN)                                              
         MVC   REMOTABF,=V(PQBUFF)                                              
         MVC   REMOTADM,=V(DATAMGR)                                             
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTDST,2(R6)                                                   
         MVC   REMOTSYS(3),=C'ACT'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
         MVC   REMOTJID,=C'ACT'    ACTIVITY REPORT                              
         DROP  R8                                                               
*                                                                               
OPX      DS    0H                                                               
         MVC   OLDREP,SREP                                                      
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DHEAD    DC    C'REC TYPE   REC KEY    DATE       TIME      ACTION   USX        
               ER       TERMINAL   RECORD DATA'                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* EXIT AND XBASE                                                                
EXIT     XIT1                                                                   
EXXBASE  XBASE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,36,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=2172'                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4200,             X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(REACT,65000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
******** DATA AND DSECTS                                                        
DUB      DS    D                                                                
DMCB     DS    6F                                                               
KEY      DS    XL32                                                             
KEYSAVE  DS    XL32                                                             
DMWORK   DS    XL96                                                             
CARD     DS    CL80                                                             
WORK     DS    CL64                                                             
BYTE     DS    X                                                                
OLDREP   DC    CL2'  '                                                          
CLSPQ    DC    X'00'               CLOSE PQ FLAG                                
VREMOTEC DC    V(REMOTEC)                                                       
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
AREPTBL  DC    A(REPTBL)                                                        
*                                                                               
DMINBITS DS    X                                                                
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
UTL      DC    F'0',X'0A'                                                       
SAVNAME  DS    CL10                                                             
SAVLOCAL DS    CL1                                                              
SAVUTL   DS    CL1                                                              
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
         DC    C'*SORTKEY'                                                      
SORTKEY  EQU   *                                                                
SREP     DS    XL2       +00       REP                                          
SKEY     DS    XL27      +02       RECORD KEY                                   
SDATE    DS    XL3       +29       UPDATE DATE (BIN)                            
STIME    DS    XL4       +32       UPDATE TIME (0HHMMSS+ OR HHMMSSNN)           
*                                                                               
         DC    C'*RECVREC'                                                      
RSPARE   DS    XL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    0CL27                                                            
REC      DS    4200C                                                            
*                                                                               
SORTLEN  EQU   *-SORTKEY                                                        
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
         ORG   REC                                                              
       ++INCLUDE REGENSTA                                                       
         ORG   REC                                                              
       ++INCLUDE REGENSAL                                                       
         ORG   REC                                                              
       ++INCLUDE REGENADV                                                       
         ORG   REC                                                              
       ++INCLUDE REGENAGY                                                       
         ORG   REC                                                              
       ++INCLUDE REGENPRD                                                       
         ORG   REC                                                              
       ++INCLUDE REGENGRP                                                       
         ORG                                                                    
         PRINT ON                                                               
*                                                                               
* RECORD TYPE TABLE                                                             
         DC    C'*RECTBL*'                                                      
RECTBL   DS    0CL14                                                            
         DC    CL8'STATION ',AL1(02),AL1(RSTAKREP-RSTAREC),AL4(STA)             
         DC    CL8'SALESMAN',AL1(06),AL1(RSALKREP-RSALREC),AL4(SAL)             
         DC    CL8'GROUP   ',AL1(07),AL1(RGRPKREP-RGRPREC),AL4(GRP)             
         DC    CL8'ADVERTIS',AL1(08),AL1(RADVKREP-RADVREC),AL4(ADV)             
         DC    CL8'PRODUCT ',AL1(09),AL1(RPRDKREP-RPRDREC),AL4(PRD)             
         DC    CL8'AGENCY  ',AL1(10),AL1(RAGYKREP-RAGYREC),AL4(AGY)             
         DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
         DC    C'***IO***'                                                      
IO       DS    CL1000                                                           
*                                                                               
         DC    C'*REPTBL*'                                                      
REPTBL   DS    126CL(RTLEN)                                                     
*                                                                               
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*                                                                               
* DDREMOTED                                                                     
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* REGENREP                                                                      
         PRINT OFF                                                              
REPD     DSECT                                                                  
       ++INCLUDE REGENREP                                                       
         PRINT ON                                                               
*                                                                               
* REPTBL DSECT                                                                  
REPTBLD  DSECT                                                                  
RTREP    DS    CL2                 REP CODE                                     
RTPQ     DS    CL8                 PRINT QUE ID                                 
RTRC     DS    XL4                 CHANGED/ADDED RECORD COUNT                   
RTLEN    EQU   *-REPTBLD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REREPACTS 05/01/02'                                      
         END                                                                    
