*          DATA SET CTTERMCOPY AT LEVEL 021 AS OF 05/01/02                      
*PHASE CTTERMA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DUMPOUT                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'TERMINAL RECORD COPY PROGRAM'                                   
*********************************************************************           
*                                                                   *           
* PROGRAM COPIES TERMINAL RECORDS FROM                              *           
* ONE LINEID/POLL TO ANOTHER.                                       *           
*                                                                   *           
* INPUT CARDS ARE OF THE FORMAT LNIDP1=P2                           *           
*  (EXAMPLE:  DDM3C1=C2)                                            *           
*                                                                   *           
* NOTE:  INPUT CARDS MUST BE SORTED.                                *           
*                                                                   *           
*********************************************************************           
         SPACE 2                                                                
CTTERM   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,CTTERM,=V(REGSAVE),RA                                          
*                                                                               
         XC    DUB,DUB                                                          
         ST    RB,DUB                                                           
         L     R4,=V(STXITER)                                                   
         ST    R4,DUB+4                                                         
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8           R8=A(PRINT CSECT)                            
         MVC   TITLE(19),=C'INPUT CONTROL CARDS'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  (TINT,INPUT)                                                     
         OPEN  (TOUT,OUTPUT)                                                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         EJECT                                                                  
NEXTREC  GET   TINT,IO             GET ALL RECORDS BEFORE 'T' RECORDS           
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   IO+4,C'T'           TEST TERMINAL RECORD                         
         BE    TERMHEAD            YES                                          
*                                                                               
         PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
         B     NEXTREC                                                          
         EJECT                                                                  
TERMHEAD LA    R6,IO+4             POINT R6 TO BEGINNING OF RECORD              
         USING CTTREC,R6                                                        
         OC    CTTKSPAR(24),CTTKSPAR                                            
         BZ    *+6                 MAKE SURE THIS IS TERMINAL HEADER            
         DC    H'0'                                                             
*                                                                               
         LH    R1,IO                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TERMREC(0),IO       SAVE THIS RECORD                             
*                                                                               
         BAS   RE,GETEL            GET POINTER ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HIGHTERM,2(R6)      SAVE HIGH TERMINAL NUMBER                    
         LA    R6,IO+4             RESTORE R6                                   
         SPACE 3                                                                
TERMPASS GET   TINT,IO                                                          
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   CTTKTYP,C'T'        MAKE SURE THERE ARE 'T' RECORDS              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    CTTKSPAR(22),CTTKSPAR                                            
         BNZ   GETCARD             LINE-ID/ADDRESS IS IN KEY                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
         B     TERMPASS                                                         
         EJECT                                                                  
GETCARD  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'      TEST FOR END OF CARDS                        
         BNE   GETCRD10            NO                                           
*                                                                               
GETCRD5  GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
*                                                                               
         GET   TINT,IO             PUT REMAINDER OF 'T' RECS TO SORTER          
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   CTTKTYP,C'T'        MAKE SURE THERE ARE 'T' RECORDS              
         BE    GETCRD5                                                          
         B     WRAPUP                                                           
*                                                                               
GETCRD10 MVC   P(80),CARD          PRINT THE CARD                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD+6,C'='         TEST VALID SYNTAX                            
         BNE   CARDERR                                                          
         CLC   CARD+9(71),SPACES                                                
         BNE   CARDERR                                                          
         CLC   OLDCARD(6),CARD     CARDS MUST BE IN SORTED ORDER                
         BNL   CARDERR                                                          
         MVC   OLDCARD,CARD        SAVE THIS CARD                               
         B     PROCESS                                                          
*                                                                               
CARDERR  MVC   P+10(42),=C'ERROR - INVALID SYNTAX OR CARDS NOT SORTED'          
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         EJECT                                                                  
PROCESS  LA    R6,IO+4             POINT R6 TO BEGINNING OF RECORD              
         CLI   CTTKTYP,C'T'        TEST TERMINAL RECORD                         
         BNE   WRAPUP              NO                                           
*                                                                               
         CLC   CTTKLINE(6),CARD    TEST MATCH ON LINE-ID/POLL                   
         BH    GETCARD             GET ANOTHER INPUT CARD                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
         CLC   CTTKLINE(6),CARD    TEST MATCH ON LINE-ID/POLL                   
         BL    PROC50              NO MATCH ON THIS RECORD                      
*                                                                               
         MVC   CTTKADDR(2),CARD+7  PUT NEW POLL IN RECORD                       
         MVC   ELEM+2(18),CTTKLINE                                              
*                                                                               
         BAS   RE,GETEL            FIND POINTER ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R1,HIGHTERM         INCREMENT HIGH TERMINAL NUMBER               
         LA    R1,1(R1)                                                         
         STH   R1,HIGHTERM                                                      
         STCM  R1,3,2(R6)          PUT TERMINAL NUMBER IN ELEMENT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
         L     R1,NUMADD                                                        
         LA    R1,1(R1)            INCREMENT ADDED COUNTER                      
         ST    R1,NUMADD                                                        
*                                                                               
         LA    R6,IO+4             BUILD PASSIVE KEY                            
         XC    CTTKSPAR(22),CTTKSPAR                                            
         MVC   CTTKEY+23(2),HIGHTERM                                            
         BAS   RE,GETEL            POINTER ELEMENT GETS DELETED                 
         BE    *+6                 AND READDED                                  
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(6),=X'001C001903E8'                                         
         GOTO1 =V(RECUP),DMCB,(X'FE',IO+4),(R6),0,WORK                          
         GOTO1 =V(RECUP),DMCB,(X'FE',IO+4),ELEM,(R6),WORK                       
         LH    R1,IO                                                            
         LA    R1,16(R1)           ADJUST VARIABLE RECORD LENGTH                
         STH   R1,IO                                                            
         GOTO1 =V(SORTER),DMCB,=C'PUT',IO                                       
         L     R1,NUMADD                                                        
         LA    R1,1(R1)            INCREMENT ADDED COUNTER                      
         ST    R1,NUMADD                                                        
*                                                                               
PROC50   GET   TINT,IO             READ NEXT RECORD                             
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
         B     PROCESS                                                          
         EJECT                                                                  
WRAPUP   LA    R6,TERMREC+4                                                     
         BAS   RE,GETEL            GET POINTER ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(2,R6),HIGHTERM    WRITE HIGH TERMINAL NUMBER                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',TERMREC                                  
*                                                                               
WRAP10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4    TEST ANY MORE RECORDS                        
         BZ    WRAP20              NO                                           
         L     R6,DMCB+4                                                        
         CLC   SAVEKEY,4(R6)       TEST DUPLICATE KEY                           
         BL    WRAP15              NO                                           
         BE    *+6                 YES                                          
         DC    H'0'                OH BOY, IS THIS FUCKED UP                    
*                                                                               
         MVC   P(24),=C'DUPLICATE RECORD DROPPED'                               
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRNTBL),DMCB,0,SAVEKEY,C'DUMP',25,=X'00C4'                    
         GOTO1 =V(PRNTBL),DMCB,0,4(R6),C'DUMP',25,=X'01C4'                      
         B     WRAP10                                                           
*                                                                               
WRAP15   MVC   SAVEKEY,4(R6)                                                    
         PUT   TOUT,(R6)           WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
         B     WRAP10                                                           
*                                                                               
WRAP20   PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
*                                                                               
         GET   TINT,IO             GET ALL REMAINING RECORDS                    
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
         B     WRAP20                                                           
         EJECT                                                                  
WRAP30   MVC   P(27),=C'NUMBER OF INPUT RECORDS:   '                            
         EDIT  NUMIN,(8,P+37),ZERO=NOBLANK                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'NUMBER OF ADDED RECORDS:   '                            
         EDIT  NUMADD,(8,P+37),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'NUMBER OF OUTPUT RECORDS:  '                            
         EDIT  NUMOUT,(8,P+37),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EXIT     CLOSE (TINT,)                                                          
         CLOSE (TOUT,)                                                          
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=WRAP30,           *        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2048'                                  
         SPACE 3                                                                
         GETEL (R6),28,ELCODE                                                   
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
NUMIN    DC    F'0'                                                             
NUMOUT   DC    F'0'                                                             
NUMADD   DC    F'0'                                                             
HIGHTERM DS    H                                                                
WORK     DS    XL17                                                             
SAVEKEY  DC    XL25'00'                                                         
ELCODE   DC    X'03'                                                            
ELEM     DC    XL20'0314000000000000000000000000000000000000'                   
CARD     DS    CL80                                                             
OLDCARD  DC    CL80' '                                                          
*                                                                               
         DS    0D                                                               
         DC    C'**TREC**'                                                      
TERMREC  DC    XL256'00'                                                        
*                                                                               
         DC    C'***IO***'                                                      
IO       DS    CL8000                                                           
         SPACE 3                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021CTTERMCOPY05/01/02'                                      
         END                                                                    
