*          DATA SET DDPQTAPE   AT LEVEL 039 AS OF 05/01/02                      
*PHASE PQTAPE                                                                   
*INCLUDE PQGETLIN                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DYNALLOC                                                               
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'DDPQTAPE - TAPES FROM PRINT Q'                                  
PQTAPE   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE MYEND-MYSTART,PQTAPE,=V(REGSAVE),R9,R8                           
         USING MYD,RC                                                           
         SPACE 1                                                                
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
         SPACE 1                                                                
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE+18(24),=C'PRINT Q TO TAPE TRANSFER'                        
         GOTO1 =V(DATCON),DMCB,(5,0),(0,ETODAY)                                 
         EJECT                                                                  
*              READ CONTROL CARDS                                               
         SPACE 3                                                                
CREAD    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         CLC   C(2),=C'/*'                                                      
         BE    GOODBYE                                                          
         CLC   C(6),=C'PQKEY='                                                  
         BE    CPQK                                                             
         CLC   C(5),=C'CLOSE'                                                   
         BE    CCLOSE                                                           
         CLC   C(4),=C'OPEN'                                                    
         BE    COPEN                                                            
         CLC   C(5),=C'TRACE='                                                  
         BE    CTRACE                                                           
         CLC   C(4),=C'LIST'                                                    
         BE    CLIST                                                            
         CLC   C(5),=C'COUNT'                                                   
         BE    CCOUNT                                                           
         CLC   C(5),=C'BLOCK'                                                   
         BE    CBLOCK                                                           
         CLC   C(5),=C'LRECL'                                                   
         BE    CLRECL                                                           
         CLC   C(5),=C'TRANS'                                                   
         BE    CTRANS                                                           
         CLC   C(6),=C'HEADER'                                                  
         BE    CHEADER                                                          
         CLC   C(12),=C'TAPE=SEAGRAM'                                           
         BE    CTAPESEA                                                         
         MVC   P(50),=CL50'*** ABOVE CARD NOT RECOGNIZED ***'                   
         B     CREAD                                                            
         SPACE 1                                                                
CPQK     LA    R4,PQKEY            PQKEY=UUUUUSSSRRRRR                          
         USING UKRECD,R4                                                        
         PACK  DUB,C+6(5)          USER ID.                                     
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   UKSRCID,DUB                                                      
         MVC   UKSUBID,C+6+5       SUB-ID                                       
         PACK  DUB,C+14(5)         REPORT ID                                    
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   UKREPNO,DUB                                                      
         DROP  R4                                                               
         BAS   RE,READPQ                                                        
         B     CREAD                                                            
         SPACE 1                                                                
CTRACE   PACK  TRALIMIT,C+6(5)     TRACE=NNNNN CONTROL                          
         ZAP   TRACOUNT,=P'0'                                                   
         B     CREAD                                                            
         SPACE 1                                                                
CLIST    MVC   LISTOPT,C+5         LIST=Y/N                                     
         B     CREAD                                                            
         SPACE 1                                                                
CCOUNT   MVI   COUNTING,C'Y'       NOTE WE ARE COUNTING                         
         ZAP   RECCOUNT,=P'0'                                                   
         ZAP   HASHTOT,=P'0'                                                    
         B     CREAD                                                            
         SPACE 1                                                                
CBLOCK   PACK  DUB,C+6(5)          BLOCK SIZE SPECIFIED                         
         CVB   R1,DUB                                                           
         L     RF,ATAPE                                                         
         STH   R1,62(RF)                                                        
         B     CREAD                                                            
         SPACE 1                                                                
CLRECL   PACK  DUB,C+6(5)          LRECL SIZE SPECIFIED                         
         CVB   R1,DUB                                                           
         L     RF,ATAPE                                                         
         STH   R1,82(RF)                                                        
         B     CREAD                                                            
         SPACE 1                                                                
CTRANS   MVI   COUNTING,C'N'       NOTE WE ARE NOW TRANSLATING                  
         ZAP   RECCOUNT,=P'0'                                                   
         ZAP   HASHTOT,=P'0'                                                    
         B     CREAD                                                            
         SPACE 1                                                                
CHEADER  MVC   HEADDATA,C+7        SAVE HEADER DATA                             
         BAS   RE,DOHEADER         TIME TO PROCESS A HEADER                     
         B     CREAD                                                            
         SPACE 1                                                                
CTAPESEA L     R1,=A(SGTAPE)       TAPE=SEAGRAMS                                
         ST    R1,ATAPE                                                         
         MVC   DYNPRFIX,=C'SG'                                                  
         B     CREAD                                                            
         SPACE 1                                                                
COPEN    BAS   RE,OPENTAPE                                                      
         B     CREAD                                                            
         SPACE 1                                                                
CCLOSE   BAS   RE,CLOSTAPE                                                      
         B     CREAD                                                            
         EJECT                                                                  
*              ROUTINES TO READ A PRINTQ FILE                                   
         SPACE 3                                                                
READPQ   NTR1                                                                   
         MVI   INPROG,C'N'                                                      
         BAS   RE,TAPCLEAR                                                      
         SPACE 1                                                                
READPQ2  GOTO1 =V(PQGETLIN),DMCB,PQKEY,PQRECCON                                 
         CLI   DMCB,0              LINE FOUND?                                  
         BE    READPQ4             YES                                          
         CLI   DMCB,2              END OF FILE?                                 
         BE    XIT                 YES                                          
         CLI   DMCB,1              REPORT NOT FOUND?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    PQKEY+5(2),PQKEY+5  OR, IF 'ALL' REQUESTED,                      
         BZ    XIT                                                              
         SPACE 1                                                                
READPQ3  MVC   P(16),=C'REPORT NOT FOUND'                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         SPACE 1                                                                
READPQ4  MVC   P,PQREC                                                          
         CP    TRACOUNT,TRALIMIT                                                
         BNL   *+8                                                              
         BAS   RE,PRINTEM                                                       
         CLI   PQREC,C'"'          LOOKING FOR DOWNLOAD LINES                   
         BNE   READPQ2                                                          
         BAS   RE,STRIP            STRIP OUT ACTUAL DATA                        
         CLI   INPROG,C'Y'         MAY NOT HAVE FINISHED YET                    
         BE    READPQ2                                                          
         CLI   COUNTING,C'Y'       MAY BE COUNTING                              
         BE    READPQ6             IF NOT, WE'RE TRANSLATING                    
         BAS   RE,PUTTAPE                                                       
         BAS   RE,TAPCLEAR                                                      
         B     READPQ2                                                          
         SPACE 1                                                                
READPQ6  AP    RECCOUNT,=P'1'      BUMP RECORD COUNT                            
*****    AP    HASHTOT,DUB         ADD IN HASH TOTALS                           
         BAS   RE,TAPCLEAR                                                      
         B     READPQ2                                                          
         EJECT                                                                  
*              ROUTINE TO STRIP DATA OUT FROM PQREC                             
         SPACE 3                                                                
STRIP    NTR1                                                                   
         LA    R3,TAPEIO           POSITION R3 TO CURRENT POINTER               
         MVI   999(R3),X'FF'       PROTECT AGAINST GOING TOO FAR                
         CLI   INPROG,C'Y'                                                      
         BNE   *+8                                                              
         L     R3,CURRPNTR                                                      
         LA    R2,PQREC                                                         
         MVI   199(R2),X'FF'                                                    
         SPACE 1                                                                
STRIP2   MVI   INPROG,C'N'         NOT IN PROGRESS IF EOR                       
         CLI   0(R2),C';'          INDICATED BY ';'                             
         BE    XIT                                                              
         MVI   INPROG,C'Y'         IN PROGRESS IF NO DATA                       
         ST    R3,CURRPNTR                                                      
         CLI   0(R2),C'"'          INDICATED BY ';'                             
         BNE   XIT                                                              
         LA    R2,1(R2)            GET PAST INITIAL "                           
         SPACE 1                                                                
STRIP4   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),C'"'          DID WE GET TO END OF FIELD?                  
         BNE   STRIP6                                                           
         LA    R2,1(R2)            BUMP TO SPACE BETWEEN FIELDS                 
         CLI   0(R2),C' '                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,1(R2)            BUMP TO START OF NEXT FIELD                  
         B     STRIP2                                                           
         SPACE 1                                                                
STRIP6   MVC   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         B     STRIP4                                                           
         EJECT                                                                  
*              HANDLE THE HEADER RECORD                                         
         SPACE 3                                                                
DOHEADER NTR1                                                                   
         LA    R2,TAPEIO                                                        
         BAS   RE,TAPCLEAR                                                      
         SPACE 1                                                                
         USING SGHEAD,R2                                                        
SGHEADER MVC   SGFTYPE,HEADDATA                                                 
         MVC   SGPRECT,=C'0000000'                                              
         AP    RECCOUNT,=P'1'      SEAGRAM HAVE COUNT INCLUDE HEADER            
         EDIT  (P8,RECCOUNT),(8,SGCOUNT),FILL=0                                 
         MVC   SGCREATE,ETODAY                                                  
******** MVC   SGTDATE,HEADDATA+1                                               
         MVC   SGTDATE,SGCREATE    COPY YYMM FROM TODAY'S DATE                  
         EDIT  (P8,HASHTOT),(11,SGHASH),FILL=0                                  
         BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         EJECT                                                                  
*              TAPE HANDLING                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         ZIC   RF,DLFILNUM         PICK UP FILE NUMBER                          
         AI    DLFILNUM,1                                                       
         MVC   WORK(20),=CL20'TALTAPE.TA0XXDS1'                                 
         MVC   WORK+11(2),DYNPRFIX                                              
*******  GOTO1 =V(DYNALLOC),DMCB,(0,DYNPRFIX),((RF),WORK)                       
         L     R2,ATAPE                                                         
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         EDIT  (P6,TAPCOUNT),(7,P)                                              
         MVC   P+8(12),=C'TAPE RECORDS'                                         
         BAS   RE,PRINTEM                                                       
         L     R2,ATAPE                                                         
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         AP    TAPCOUNT,=P'1'                                                   
         AP    TRACOUNT,=P'1'                                                   
         CP    TRACOUNT,TRALIMIT                                                
         BH    PUTTAPE2                                                         
         BAS   RE,TRACETAP                                                      
         SPACE 1                                                                
PUTTAPE2 CLI   LISTOPT,C'N'                                                     
         BE    PUTTAPE4                                                         
         MVC   P,SPACES                                                         
         MVC   P(19),=C'*** TAPE RECORD ***'                                    
         MVC   P+20(100),TAPEIO                                                 
         BAS   RE,PRINTEM                                                       
         CLI   LISTOPT,C'A'                                                     
         BNE   PUTTAPE4                                                         
         MVC   P+20(100),TAPEIO+100                                             
         BAS   RE,PRINTEM                                                       
         MVC   P+20(40),TAPEIO+200                                              
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRINTEM                                                       
         SPACE 1                                                                
PUTTAPE4 L     R2,ATAPE                                                         
         LA    R0,TAPEIO                                                        
         PUT   (2),(0)                                                          
         B     XIT                                                              
         SPACE 1                                                                
ATAPE    DC    A(SGTAPE)                                                        
DYNPRFIX DC    C'DL'               THIS MUST STAY WITH                          
         DC    C'TAPE  '           FOLLOWING DC STATEMENT                       
         EJECT                                                                  
*              UTILITIES                                                        
         SPACE 3                                                                
TRACETAP NTR1                                                                   
         MVI   P,C'='                                                           
         MVC   P+1(131),P                                                       
         MVC   P+20(11),=C'OUTPUT TAPE'                                         
         BAS   RE,PRINTEM                                                       
         MVC   P(80),TAPEIO                                                     
         BAS   RE,PRINTEM                                                       
         MVC   P(80),TAPEIO+80                                                  
         BAS   RE,PRINTEM                                                       
         MVC   P(80),TAPEIO+160                                                 
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         SPACE 1                                                                
PRINTEM  NTR1                                                                   
         GOTO1 =V(PRINTER)         PRINT A LINE FROM P                          
         B     XIT                                                              
         SPACE 1                                                                
TAPCLEAR NTR1                      CLEAR TAPE IO TO SPACES                      
         CLC   DYNPRFIX,=C'SG'     SEAGRAM NEEDS SPACES                         
         BE    TAPSPACE                                                         
         B     TAPXC               DEFAULT TO BINARY ZEROS                      
TAPSPACE LA    R1,TAPEIO                                                        
         LA    R0,10                                                            
         SPACE 1                                                                
TAPSP2   MVC   0(100,R1),SPACES                                                 
         LA    R1,100(R1)                                                       
         BCT   R0,TAPSP2                                                        
         B     XIT                                                              
         SPACE 1                                                                
TAPXC    LA    RE,TAPEIO                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
GOODBYE  XBASE                                                                  
         EJECT                                                                  
*              CONSTANTS & LTORG                                                
         SPACE 3                                                                
TAPCOUNT DC    PL6'0'                                                           
TRACOUNT DC    PL6'0'                                                           
TRALIMIT DC    PL6'0'                                                           
INPROG   DC    C'N'                                                             
LISTOPT  DC    C'Y'                                                             
DLFILNUM DC    X'00'                                                            
DUMPLIST DS    0F                                                               
         DC    A(PQTAPE),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'07'          FOR DATAMGR (TALENT SYSTEM)                  
         SPACE 3                                                                
         PRINT GEN                                                              
*              SEAGRAMS TAPE                                                    
SGTAPE   DCB   DDNAME=SGTAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=240,BLKSIZE=2400,BUFNO=2                          
         PRINT NOGEN                                                            
         EJECT                                                                  
*              MY DSECT AREA                                                    
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
DUB      DS    D                                                                
ETODAY   DS    CL6                                                              
RECCOUNT DS    PL8                                                              
HASHTOT  DS    PL8                                                              
COUNTING DS    CL1                                                              
CURRPNTR DS    F                                                                
C        DS    CL80                                                             
HEADDATA DS    CL72                                                             
PQKEY    DS    XL7                 PQ KEY                                       
         SPACE 1                                                                
PQRECCON DS    CL1                 KEEP THIS BEFORE PQREC                       
PQREC    DS    CL200               PQ RECORD                                    
TAPEIO   DS    1000C                                                            
MYEND    DS    0D                                                               
         SPACE 3                                                                
*                                  DSECT FOF SEAGRAM HEADER RECORD              
         SPACE 1                                                                
SGHEAD   DSECT                                                                  
SGFTYPE  DS    CL1                                                              
SGPRECT  DS    CL7                                                              
SGCOUNT  DS    CL8                                                              
SGCREATE DS    CL6                                                              
SGTDATE  DS    CL4                                                              
SGHASH   DS    CL11                                                             
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039DDPQTAPE  05/01/02'                                      
         END                                                                    
