*          DATA SET DDREQUIEM2 AT LEVEL 029 AS OF 05/01/02                      
*PHASE REQUIEM2,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
         SPACE 2                                                                
*******************************************************************             
* PROGRAM RESTORES REQUEST FILES,ACCDAY FILES, ETC.               *             
* TARGET DDNAME IS ALWAYS 'GRECO'                                 *             
* FILENAME PARAMETER IS USED TO FIND RECOVERY FILE CODE IN        *             
*     SYSFLES LIST                                                *             
*******************************************************************             
REQUIEM  TITLE 'GENERALIZED FILE RECONSTRUCT PROGRAM'                           
REQUIEM  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*REQUIEM,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R7,=A(TAPIO)                                                     
         LA    R8,4(R7)                                                         
         USING RECVHDR,R8                                                       
*                                                                               
         XC    DUB,DUB                                                          
         ST    RB,DUB                                                           
         L     R0,=V(STXITER)                                                   
         ST    R0,DUB+4                                                         
         OI    DUB+4,X'80'         SET E-O-L FLAG                               
         GOTO1 =V(STXITER),P1,DUB                                               
         EJECT                                                                  
* INITIALIZATION                                                                
         SPACE 1                                                                
         MVC   TITLE(15),=C'RUN DIAGNOSTICS'                                    
         LA    R9,RECTAPE                                                       
         OPEN  ((R9),(INPUT))                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     CARD                                                             
*                                                                               
SORTCARD DC    C'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1 '                         
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
CARD     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         SPACE 2                                                                
         BAS   RE,FNDDTF                                                        
         EJECT                                                                  
* OPEN FILE AND SET UP PARAMETER LIST *                                         
         SPACE 1                                                                
OPEN     L     R1,ADTF                                                          
         USING DTFPHD,R1                                                        
         MVC   DNEXT,=X'00010000'                                               
         LA    R2,DMTX                                                          
         XC    PARA(24),PARA                                                    
         MVC   PARA+12(4),ADTF                                                  
         GOTO1 =V(DADDS),PARA,A(DAOPEN)                                         
         LA    R1,28(R7)                                                        
         ST    R1,P2               A(RECORD)                                    
         XC    P3,P3               LENGTH                                       
         MVC   P4,ADTF             A(DTF)                                       
         LA    R1,P6                                                            
         ST    R1,P5               A(DISK ADDRESS)                              
*                                                                               
         L     R1,ADTF                                                          
         USING DTFPHD,R1                                                        
         MVC   DNEXT,=X'00010000'  INSURE WRITE AT START OF FILE                
         B     GET                                                              
         EJECT                                                                  
* LOCATE DTF DETAILS *                                                          
         SPACE 1                                                                
FNDDTF   NTR1                                                                   
*                                                                               
         GOTO1 =V(DMOD000),PARA,A(FINDSYS),(1,0)                                
         L     R1,4(R1)                                                         
*                                                                               
FNDDTF10 SR    R0,R0                                                            
         IC    R0,3(R1)            GET NUMBER OF FILES THIS SYS                 
         LA    R1,4(R1)            POINT TO FIRST ENTRY                         
*                                                                               
FNDDTF20 L     RE,4(R1)                                                         
         CLC   22(7,RE),C                                                       
         BE    FNDDTFX                                                          
         LA    R1,8(R1)                                                         
         BCT   R0,FNDDTF20                                                      
         SPACE 1                                                                
* END OF LIST FOR THIS SYSTEM                                                   
         SPACE 1                                                                
         CLI   0(R1),X'FF'         TEST ANY MORE SYSTEMS                        
         BNE   FNDDTF10                                                         
*                                                                               
         MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'* ERROR * FILE NOT FOUND'                            
         MVC   P+32(78),C                                                       
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         DC    H'0'                                                             
*                                                                               
FNDDTFX  MVC   22(7,RE),=CL7'GRECO  '  MOVE OVERRIDE DDNAME TO DCB              
         MVC   FILNUMB,3(R1)           PASS RECOVERY FILE CODE                  
         MVC   FILTYPE,0(R1)                                                    
         ST    RE,ADTF                 PASS DTF ADDRESS                         
         B     XIT                                                              
         EJECT                                                                  
* MAIN TAPE READ LOOP *                                                         
         SPACE 1                                                                
GET      GET   (R9),(R7)                                                        
         CLC   RFILTY,FILNUMB      MATCH ON SELECTED FILE                       
         BNE   GET                                                              
*                                                                               
GET2     CLI   RRECTY,1            IGNORE COPIES                                
         BE    GET                                                              
*                                                                               
         MVC   BYTE,RRECTY         SAVE REC TYPE                                
         MVC   FULL,RVCHR          SAVE DISK ADDRESS                            
         XC    4(24,R7),4(R7)      CLEAR OUT RECOVERY HEADER                    
*                                                                               
         MVC   4(4,R7),FULL        SET DISK ADDRESS                             
         MVC   8(4,R7),SEQ         SET SEQUENCE NUM                             
         L     RF,SEQ                                                           
         LA    RF,1(RF)                                                         
         ST    RF,SEQ                                                           
         MVC   12(1,R7),BYTE        SET REC TYPE                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R7)                                     
         B     GET                                                              
         EJECT                                                                  
* SORT OUTPUT PROCESSING *                                                      
         SPACE 1                                                                
ENDIN    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R7,15,4(R1)                                                      
         BZ    EOF                                                              
*                                                                               
         CLI   12(R7),3            TEST ADD                                     
         BNE   ENDIN2                                                           
         AP    NADDS,=P'1'                                                      
         MVC   P1,=A(WTCKD)        SET TO ADD RECORD                            
         B     ENDIN4                                                           
         SPACE 1                                                                
ENDIN2   DS    0H                                                               
         AP    NCHANGE,=P'1'                                                    
         MVC   P1,=A(WTID)                                                      
*                                                                               
ENDIN4   LA    RE,28(R7)           SET TO WRITE FROM REQHDR                     
         ST    RE,P2                                                            
*                                                                               
         LH    R0,0(R7)                                                         
         SH    R0,=H'28'                                                        
         STH   R0,P3+2             RECORD LENGTH                                
*                                                                               
ENDIN6   GOTO1 =V(DADDS),P1                                                     
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   P6,4(R7)                                                         
         BE    ENDIN                                                            
         BL    *+6                                                              
         DC    H'0'                DIE IF RECORD IN WRONG PLACE                 
         CLC   P1,=A(WTCKD)                                                     
         BE    ENDIN6                                                           
         DC    H'0'                                                             
         EJECT                                                                  
* WRAP UP REPORT                                                                
         SPACE 1                                                                
EOF      MVI   SPACING+3,C'2'                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'NAME OF RESTORED FILE = '                               
         MVC   P+24(7),C                                                        
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         MVC   P(20),=C'NUMBER OF CHANGES = '                                   
         EDIT  (P6,NCHANGE),(7,P+20),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'NUMBER OF ADDS = '                                      
         EDIT  (P6,NADDS),(7,P+17),ALIGN=LEFT                                   
         GOTO1 =V(PRINTER)                                                      
         AP    NADDS,NCOPIES                                                    
         AP    NADDS,NCHANGE                                                    
         MVC   P(25),=C'TOTAL RECORDS FOR FILE = '                              
         EDIT  (P6,NADDS),(7,P+25),ALIGN=LEFT                                   
*                                                                               
         L     R1,ADTF                                                          
         USING DTFPHD,R1                                                        
         MVC   P6,DNEXT            SET ADDRESS OF LAST TRACK USED               
         DROP  R1                                                               
         MVC   P1,=A(WTERASE)                                                   
         GOTO1 =V(DADDS),P1                                                     
         CLOSE ((R9),)                                                          
         XBASE                                                                  
         EJECT                                                                  
*  DTF AND WORK SPACES FOR PROGRAM                                              
         SPACE 1                                                                
RECTAPE  DCB   DDNAME=RECTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08200,                                            X        
               BLKSIZE=08204,                                          X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
         SPACE 2                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
SEQ      DC    F'1'                                                             
WORK     DS    CL32                                                             
PARA     DS    6F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
ADTF     DS    F                                                                
NADDS    DC    PL6'0'                                                           
NCOPIES  DC    PL6'0'                                                           
NCHANGE  DC    PL6'0'                                                           
FILNUMB  DS    CL1                                                              
FILTYPE  DC    X'00'                                                            
HIGHDA   DC    F'0'                                                             
C        DC    CL80' '                                                          
FFS      DC    106X'FF'                                                         
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'**TAPIO*'                                                    
TAPIO    DS    0D                                                               
         DS    8200C                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
MYDSECT  DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMGREQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029DDREQUIEM205/01/02'                                      
         END                                                                    
