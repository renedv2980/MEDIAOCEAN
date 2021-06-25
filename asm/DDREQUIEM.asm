*          DATA SET DDREQUIEM  AT LEVEL 001 AS OF 03/27/13                      
*PHASE REQUIEMA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
                                                                                
***********************************************************************         
* PROGRAM REBUILDS REQUEST DISK FILE FROM RECOVERY TAPE               *         
* CAN ALSO BE USED TO RESTORE ACCDAY FILES                            *         
* TARGET DDNAME IS ALWAYS 'GRECO'                                     *         
* FILENAME PARAMETER IS USED TO FIND TARGET FILE IN SYSFLES           *         
***********************************************************************         
         TITLE 'RECONSTRUCT REQUEST FILE FROM RECOVERY TAPE'                    
REQUIEM  CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,*REQUIEM,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)      RA=(PRINTER REGISTER)                         
         USING DPRINT,RA                                                        
         MVC   TITLE(20),=CL20'REQUIEM STATISTICS'                              
*                                                                               
         L     R7,=A(TAPEIOA)     R7=A(TAPE RECORD)                             
         LA    R8,4(R7)                                                         
         USING RECVHDR,R8         R8=A(RECOVERY FILE HEADER)                    
         B     CARD                                                             
*                                                                               
EXIT     XBASE RC=COND,RL=2                                                     
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE PARAMETER CARDS                                   *         
***********************************************************************         
CARD     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         MVC   P(72),C                                                          
         CLC   C(2),=C'/*'                                                      
         BE    OPEN                                                             
*                                                                               
CARD1    CLC   C(6),=CL6'DDSIO='   SET DDSIO LOAD MODULE                        
         BNE   CARD2                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         B     CARDOK                                                           
*                                                                               
CARD2    CLC   C(7),=CL7'DSPACE='  SET DSPACE ID                                
         BNE   CARD3                                                            
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),C+7                                        
         B     CARDOK                                                           
*                                                                               
CARD3    CLC   C(6),=CL6'WRITE='   SET WRITE=N/Y - DEFAULT IS WRITE=Y           
         BNE   CARD4                                                            
         CLI   C+6,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+6,C'N'                                                         
         BNE   CARDERR                                                          
         MVC   WRITE,C+6                                                        
         B     CARDOK                                                           
*                                                                               
CARD4    CLC   C(6),=CL6'TRACE='   SET TRACE=N/Y - DEFAULT IS TRACE=N           
         BNE   CARD5                                                            
         CLI   C+6,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+6,C'N'                                                         
         BNE   CARDERR                                                          
         MVC   TRACE,C+6                                                        
         B     CARDOK                                                           
*                                                                               
CARD5    MVC   FILENAME,C          VALIDATE FILE NAME                           
         BAS   RE,FNDDTF                                                        
         BNE   CARDERR1                                                         
*                                                                               
CARDOK   GOTO1 =V(PRINTER)                                                      
         B     CARD                                                             
*                                                                               
CARDERR  MVC   P+19(30),=CL30'**ERROR** INVALID PARAMETER'                      
         GOTO1 =V(PRINTER)                                                      
         MVC   COND,=AL2(12)                                                    
         B     EXIT                                                             
*                                                                               
CARDERR1 MVC   P+19(30),=CL30'**ERROR** INVALID FILE NAME'                      
         GOTO1 =V(PRINTER)                                                      
         MVC   COND,=AL2(12)                                                    
         B     EXIT                                                             
*                                                                               
CARDERR3 MVC   P+19(30),=CL30'**ERROR** MISSING FILE NAME'                      
         GOTO1 =V(PRINTER)                                                      
         MVC   COND,=AL2(12)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN INPUT TAPE AND SORTER AND NAMED DISK REQUEST FILE              *         
***********************************************************************         
OPEN     LT    R3,ADTF             R3=A(FILE DTF)                               
         BZ    CARDERR3                                                         
         USING DTFPHD,R3                                                        
*                                                                               
         LA    R9,RCVTAPE          R9=A(INPUT RECOVERY FILE TAPE)               
         OPEN  ((R9),(INPUT))                                                   
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         XC    PARA(24),PARA       OPEN NAMED DISK FILE                         
         MVC   PARA+12(4),ADTF                                                  
         GOTO1 =V(DADDS),PARA,A(DAOPEN)                                         
*                                                                               
         LA    R1,28(R7)           BUMP PAST LENGTH AND RECOVERY HEADER         
         ST    R1,P2               P2=A(RECORD)                                 
         XC    P3,P3               P3=LENGTH                                    
         MVC   P4,ADTF             P4=A(DTF)                                    
         LA    R1,P6                                                            
         ST    R1,P5               P5=A(DISK ADDRESS)                           
*                                                                               
         MVC   BYTE,DTFTYPE        GET DEVICE TYPE                              
         NI    BYTE,X'03'                                                       
         LLC   R1,BYTE                                                          
         SLL   R1,2                                                             
         LA    R1,FRSTTAB(R1)      INDEX INTO FIRST DISK ADDR TABLE             
         MVC   FRSTDA,0(R1)                                                     
         MVC   DNEXT,FRSTDA        SET FIRST DISK ADDRESS                       
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
* GET RECORDS FROM INPUT TAPE AND WRITE TO SORTER                     *         
* KEY IS THE FIRST 9 BYTES OF MODIFIED RECOVERY HEADER                *         
* XL4 DISK ADDRESS IN RECOVERY FILE RECORD                            *         
* XL4 SEQUENCE NUMBER                                                 *         
* XL1 RECORD TYPE 01=CPY,02=CHG,03=ADD                                *         
* XL1 TRAILER FLAG X'04' RECORD HAS TRAILER                           *         
***********************************************************************         
GET      GET   (R9),(R7)                                                        
         CLC   RFILTY,FILNUM       MATCH ON SELECTED FILE NUMBER                
         BNE   GET                                                              
*                                                                               
GET2     CLI   RRECTY,1            IGNORE COPIES                                
         BNE   GET3                                                             
         AP    NCOPIES,=P'1'                                                    
         B     GET                                                              
*                                                                               
GET3     MVC   BYTE,RRECTY         SAVE REC TYPE                                
         MVC   BYTE1,RTIME         SAVE TRAILER FLAG                            
         MVC   FULL,RVCHR          SAVE DISK ADDRESS                            
         XC    4(24,R7),4(R7)      CLEAR OUT RECOVERY HEADER                    
*                                                                               
         MVC   4(4,R7),FULL        SET DISK ADDRESS                             
         MVC   8(4,R7),SEQ         SET SEQUENCE NUM                             
         MVC   12(1,R7),BYTE       SET REC TYPE                                 
         MVC   13(1,R7),BYTE1      SET TRAILER FLAG                             
         L     RF,SEQ                                                           
         AHI   RF,1                                                             
         ST    RF,SEQ              BUMP SEQUENCE NUM                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R7)                                     
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
* SORT OUTPUT PROCESSING                                              *         
***********************************************************************         
ENDIN    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R7,15,4(R1)         R7=A(RECORD)                                 
         BZ    EOF                                                              
         LA    R8,4(R7)            R8=A(MODIFIED RECOVERY HDR)                  
*                                                                               
ENDIN1   CLI   12(R7),3            TEST ADD                                     
         BNE   ENDIN2                                                           
         AP    NADDS,=P'1'                                                      
         MVC   P1,=A(WTCKD)        SET TO ADD RECORD                            
         B     ENDIN4                                                           
*                                                                               
ENDIN2   CLI   12(R7),2            TEST COPY                                    
         BNE   ENDIN                                                            
         AP    NCHANGE,=P'1'       CHANGE                                       
         MVC   P1,=A(WTID)                                                      
*                                                                               
ENDIN4   LA    RE,28(R7)           SET TO WRITE FROM REQUEST HEADER             
         ST    RE,P2                                                            
         LH    RF,0(R7)            GET TOTAL RECOVERY RECORD LEN                
         AHI   RF,-28              LESS LEN+HDR                                 
         SR    R0,R0                                                            
         TM    13(R7),X'40'        TEST IF RECOVERY TRAILER                     
         BZ    ENDIN5                                                           
         LR    RE,R7                                                            
         AH    RE,0(R7)                                                         
         AHI   RE,-1               POINT TO LAST BYTE OF RECOVERY REC           
         IC    R0,0(RE)            GET LEN OF RECOVERY REC TRAILER              
ENDIN5   SR    RF,R0                                                            
         STH   RF,P3+2             SET RECORD LENGTH                            
*                                                                               
ENDIN6   CLI   WRITE,C'Y'          ADD/WRITE RECORD TO FILE                     
         BNE   ENDIN7                                                           
         GOTO1 =V(DADDS),P1                                                     
         OC    P3(2),P3                                                         
         JNZ   *+2                 DIE IF DISK ERROR                            
*                                                                               
ENDIN7   LA    R1,P1               TEST TO PRINT REQUEST RECORD                 
         CLI   TRACE,C'Y'                                                       
         BNE   ENDIN                                                            
         TM    FLAG,X'01'          TEST FIRST TRACE LINE                        
         BO    ENDIN8                                                           
         OI    FLAG,X'01'                                                       
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ENDIN8   MVC   P(3),=C'ADD'        SET ACTION                                   
         CLC   P1,=A(WTID)                                                      
         BNE   *+10                                                             
         MVC   P(3),=C'CHG'                                                     
         LH    R0,P3+2             SET RECORD LENGTH                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(4),DUB                                                       
         MVC   FULL,DNEXT          SET DISK ADDRESS                             
         CLI   WRITE,C'Y'                                                       
         BE    *+10                                                             
         MVC   FULL,4(R7)                                                       
         GOTO1 =V(HEXOUT),PARA,FULL,P+9,4,=C'MIX'                               
         CLI   WRITE,C'Y'                                                       
         BE    ENDIN9                                                           
         MVI   P+4,C'*'            COMMENT OUT WRITE ACTIONS                    
         MVI   P+9,C'*'                                                         
*                                                                               
ENDIN9   L     R5,P2               R5=A(RECORD)                                 
         LR    R6,R5                                                            
         AH    R6,P3+2             R6=A(END OF RECORD)                          
         TM    FILFIND1,SFREQ      TEST IF FILE IS A REQUEST FILE               
         BZ    ENDIN11             NO                                           
         LA    R5,80(R5)           R5=A(FIRST REQUEST CARD)                     
*                                                                               
ENDIN10  MVC   P+19(80),0(R5)      PRINT REQUEST CARDS                          
         GOTO1 =V(PRINTER)                                                      
         LA    R5,80(R5)                                                        
         CR    R5,R6                                                            
         BL    ENDIN10                                                          
         B     ENDIN                                                            
*                                                                               
ENDIN11  GOTO1 =V(HEXOUT),PARA,(R5),P+19,40,=C'MIX'                             
         GOTO1 =V(PRINTER)                                                      
         B     ENDIN                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT NAME AND NUMBER OF RECORDS ADDED TO RESTORED DISK FILE        *         
***********************************************************************         
EOF      MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'NAME OF RESTORED FILE = '                               
         MVC   P+24(8),FILENAME                                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'NUMBER OF ADDS = '                                      
         EDIT  (P6,NADDS),(7,P+17),ALIGN=LEFT                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(20),=C'NUMBER OF CHANGES = '                                   
         EDIT  (P6,NCHANGE),(7,P+20),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         AP    NADDS,NCOPIES                                                    
         AP    NADDS,NCHANGE                                                    
         MVC   P(25),=C'TOTAL INPUT RECORDS = '                                 
         EDIT  (P6,NADDS),(7,P+22),ALIGN=LEFT                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOF1     MVC   P6,DNEXT            SET ADDRESS OF LAST TRACK USED               
*                                                                               
EOF2     CLI   WRITE,C'Y'          ERASE REST OF FILE                           
         BNE   EOF3                                                             
         MVC   P1,=A(WTERASE)                                                   
         GOTO1 =V(DADDS),P1                                                     
*                                                                               
EOF3     CLOSE ((R9),)             CLOSE FILE                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEARCH SYSFLES TO LOCATE FILE AND SET A(DTF)                        *         
***********************************************************************         
FNDDTF   NTR1                                                                   
         XC    ADTF,ADTF                                                        
         GOTO1 =V(DMOD000),PARA,A(FINDSYS),(1,0)                                
         L     R1,4(R1)                                                         
         USING SYSFLSTD,R1                                                      
*                                                                               
FNDDTF1  LLC   R0,SYSF#FLS+1       R0=NUMBER OF FILES                           
         LA    R1,SYSFLIST         R1=A(FILE LIST ENTRY)                        
*                                                                               
FNDDTF2  TM    SYSFIND1,SFRCV+SFPRTQ+SFHDR+SFISF                                
         BNZ   FNDDTF3                                                          
         TM    SYSFIND2,SFWRKF+SFALIAS+SFWRKZ                                   
         BNZ   FNDDTF3                                                          
         SR    RE,RE               RE=A(DTF)                                    
         ICM   RE,7,SYSFADTF                                                    
         CLC   22(8,RE),C          MATCH FILE NAME TO INPUT VALUE               
         BE    FNDDTF5                                                          
FNDDTF3  LA    R1,SYSFLNQ(R1)      BUMP TO NEXT FILE IN LIST                    
         BCT   R0,FNDDTF2                                                       
*                                                                               
FNDDTF4  CLI   0(R1),X'FF'         TEST ANY MORE SYSTEMS                        
         BNE   FNDDTF1                                                          
         CR    RE,RB               EXIT WITH CC=NEQ IF NOT FOUND                
         B     FNDDTFX                                                          
*                                                                               
FNDDTF5  MVC   22(8,RE),=CL8'GRECO'     MOVE OVERRIDE DDNAME TO DTF             
         MVC   FILFIND1,SYSFIND1   SAVE FILE TYPE                               
         MVC   FILNUM,SYSFILE#     SAVE FILE NUMBER                             
         ST    RE,ADTF             SAVE DTF ADDRESS                             
         CR    RE,RE               EXIT WITH CC=EQL IF FILE FOUND               
*                                                                               
FNDDTFX  XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* RECOVERY TAPE DCB CONSTANTS AND WORKING STORAGE                     *         
***********************************************************************         
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,MACRF=(GM),            X        
               BLKSIZE=0,LRECL=8200,BUFNO=2,EODAD=ENDIN                         
*                                                                               
FRSTTAB  DS    0F                                                               
         DC    X'00010000'         16-BIT TRACK1/BLOCK0/RECORD0                 
         DC    X'00004000'         18-BIT TRACK1/BLOCK0/RECORD0                 
         DC    X'00001000'         20-BIT TRACK1/BLOCK0/RECORD0                 
         DC    X'00000400'         22-BIT TRACK1/BLOCK0/RECORD0                 
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1 '                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(2100,,,,) '                           
*                                                                               
FILENAME DC    CL8' '                                                           
ADTF     DC    A(0)                                                             
HIGHDA   DC    F'0'                                                             
FRSTDA   DC    F'0'                                                             
FILNUM   DC    X'00'                                                            
FILFIND1 DC    X'00'                                                            
*                                                                               
FLAG     DC    X'00'                                                            
FLAG1    DC    X'00'                                                            
*                                                                               
SEQ      DC    F'1'                                                             
COND     DC    AL2(0)                                                           
WRITE    DC    C'Y'                                                             
TRACE    DC    C'N'                                                             
NADDS    DC    PL6'0'                                                           
NCOPIES  DC    PL6'0'                                                           
NCHANGE  DC    PL6'0'                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
WORK     DS    CL32                                                             
C        DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*SSBSSB*'                                                    
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
         DS    0D                                                               
         DC    CL8'*UTLUTL*'                                                    
UTL      DC    F'0',X'00',XL251'00'                                             
         DS    0D                                                               
         DC    CL8'*IOAIOA*'                                                    
TAPEIOA  DS    0D                                                               
         DC    8200X'00'                                                        
         EJECT                                                                  
RHDR     DSECT                                                                  
*DMRCVRHDR                                                                      
       ++INCLUDE DMRCVRHDR                                                      
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
*DMSYSFD                                                                        
       ++INCLUDE DMSYSFD                                                        
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDREQUIEM 03/27/13'                                      
         END                                                                    
