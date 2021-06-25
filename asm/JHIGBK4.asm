*          DATA SET JHIGBK4    AT LEVEL 159 AS OF 10/12/00                      
*PHASE JHIGBK4                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'JHIGBOOK -- BOOKSTORE'                                          
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
JHIGBOOK CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*JHIGBOOK,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(JHIGBOOK),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
         LA    R3,INTAB                                                         
         USING BOOKD,R3                                                         
         LA    R6,0                      RECORD COUNTER                         
         XC    QTYTOTA(12),QTYTOTA                                              
         XC    QTYTOTB(12),QTYTOTB                                              
         XC    QTYGND,QTYGND                                                    
         XC    WHOLEGND,WHOLEGND                                                
         XC    RETGND,RETGND                                                    
         OPEN  FIL1                                                             
*                                                                               
GET      DS    0H                                                               
         GET   FIL1,IOAREA                                                      
         AHI   R6,1                      INCREMENT COUNTER                      
         LA    R4,IOAREA                                                        
         MVC   BAUTHOR,26(R4)            AUTHOR                                 
         MVC   BTITLE,0(R4)              TITLE                                  
         MVC   BID,55(R4)                ID CODE                                
         SR    R5,R5                                                            
         LA    R2,4                      SET COUNTER                            
*                                                                               
CONV1    ZIC   R9,60(R4)                 CONVERT COST TO HEX                    
         SHI   R9,X'F0'                                                         
         MHI   R5,10                                                            
         AR    R5,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,CONV1                                                         
         SHI   R4,4                      RESET R4                               
*                                                                               
         STCM  R5,3,BCOST                WHOLESALE COST                         
         MVC   BTYPE,59(R4)              BOOK TYPE                              
         SR    R5,R5                                                            
         LA    R2,3                      SET COUNTER                            
*                                                                               
CONV2    ZIC   R9,64(R4)                 CONVERT QUANTITY TO HEX                
         SHI   R9,X'F0'                                                         
         MHI   R5,10                                                            
         AR    R5,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,CONV2                                                         
         SHI   R4,3                      RESET R4                               
*                                                                               
         STC   R5,BQTY                   QUANTITY                               
*                                                                               
         CLC   68(1,R4),=C'P'            CHECK FOR COVER TYPE                   
         BE    PAPER                                                            
HARD     LA    R9,16                     X'10' REPRESENTS HARD COVER            
         B     DISC                                                             
PAPER    LA    R9,0                      X'00' REPRESENTS PAPERBACK             
*                                                                               
DISC     CLC   67(1,R4),=C'Y'            CHECK FOR DISCOUNT                     
         BNE   SKIP                                                             
         AHI   R9,1                      1 IN LOW NIBBLE REPS DISCOUNT          
*                                                                               
SKIP     STC   R9,BFLAGS                 COVER TYPE AND DISCOUNT                
         LA    R3,BOOKDLQ(R3)            GO TO NEXT LINE IN TABLE               
         B     GET                       GET NEXT CARD                          
*                                                                               
SORT     DS    0H                                                               
         CLOSE FIL1                                                             
         GOTO1 =V(XSORT),DMCB,(1,INTAB),(R6),BOOKDLQ,                  X        
               L'BCOST,BCOST-BOOKD                                              
         LA    R3,INTAB                  POINT TO START OF INTAB                
         BAS   RE,GETRANK                RANK RECORDS BY DESCENDING $           
         GOTO1 =V(XSORT),DMCB,(0,INTAB),(R6),BOOKDLQ,                  X        
               L'BAUTHOR+L'BTITLE,BAUTHOR-BOOKD                                 
         GOTO1 =V(XSORT),DMCB,(0,INTAB),(R6),BOOKDLQ,L'BTYPE,          X        
               BTYPE-BOOKD                                                      
*                                                                               
PRINT    DS    0H                                                               
         BAS   RE,HEADING                PRINT HEADING FOR REPORT               
         LA    R3,INTAB                  GO TO FIRST RECORD                     
         MVC   PREVTYPE,BTYPE            INIT TO FIRST AUTHOR                   
         MVC   PREVAUTH,BAUTHOR          AND BOOK TYPE                          
PRINT2   DS    0H                                                               
         CLC   BAUTHOR,PREVAUTH                                                 
         BE    HERE                                                             
         BAS   RE,SUBTOTA                                                       
         CLC   BTYPE,PREVTYPE                                                   
         BNE   HERE                                                             
         BAS   RE,HEADING                                                       
HERE     CLC   BTYPE,PREVTYPE                                                   
         BE    CONT                                                             
         BAS   RE,SUBTOTB                IF NEW TYPE, DISPLAY SUBTOTS           
         BAS   RE,HEADING                                                       
*                                                                               
CONT     DS    0H                        PRINT FIELDS                           
         MVC   P+4(1),BTYPE                                                     
         MVC   P+11(L'BAUTHOR),BAUTHOR                                          
         MVC   P+38(L'BTITLE),BTITLE                                            
         MVC   P+66(L'BID),BID                                                  
         MVC   BYTE,BFLAGS               CHECK HIGH NIBBLE OF BYTE              
         NC    BYTE,CMASK                                                       
         BNZ   HARDC                     IF ON, THEN HARD COVER                 
PAPERC   MVC   P+77(1),=C'P'             OTHERWISE PAPERBACK                    
         B     *+10                                                             
HARDC    MVC   P+77(1),=C'H'                                                    
         DS    0H                                                               
         MVC   BYTE,BFLAGS               CHECK LOW NIBBLE OF BYTE               
         NC    BYTE,DMASK                                                       
         BNZ   YESDISC                   IF ON, THEN DISCOUNT                   
NODISC   MVC   P+86(1),=C'N'                                                    
         B     *+10                                                             
YESDISC  MVC   P+86(1),=C'Y'                                                    
         EDIT  (1,BQTY),(3,P+90)                                                
         EDIT  (2,BCOST),(6,P+96),2,FLOAT=$,ALIGN=RIGHT                         
         SR    R8,R8                                                            
         SR    R9,R9                                                            
         IC    R8,BQTY                   GET QUANTITY                           
         BAS   RE,CALCQTY                CALCULATE TOTAL QUANTITY               
         ICM   R9,3,BCOST                GET COST                               
         MR    R8,R8                     TOTAL WHOLE $ IS IN R9                 
         EDIT  (R9),(10,P+105),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                 
         BAS   RE,CALCTOT1               CALCULATE TOTAL WHOLE COST             
         LA    R7,2                                                             
         SR    R8,R8                                                            
         DR    R8,R7                     DIVIDE R9 BY 2                         
         MHI   R9,3                                                             
         LTR   R8,R8                                                            
         BZ    SKIPADD                                                          
         AHI   R9,2                      ADD 2 IF REMAINDER                     
SKIPADD  EDIT  (R9),(10,P+118),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                 
         EDIT  (1,BRANK),(2,P+130)                                              
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,CALCTOT2               CALCULATE TOTAL RET COST               
         MVC   PREVTYPE,BTYPE            SAVE PREVIOUS BOOK TYPE                
         MVC   PREVAUTH,BAUTHOR          AND AUTHOR                             
         LA    R3,BOOKDLQ(R3)                                                   
         BCT   R6,PRINT2                                                        
         BAS   RE,SUBTOTA                                                       
         BAS   RE,SUBTOTB                PRINT OUT SUBTOTALS                    
         BAS   RE,GNDTOT                 PRINT OUT GRAND TOTALS                 
         B     DONE                                                             
*                                                                               
DONE     DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
*                                                                               
HEADING  DS    0H                                                               
         NTR1                                                                   
         MVC   P(9),=C'BOOK TYPE'        PRINT FIRST LINE OF HEADING            
         MVC   P+21(6),=C'AUTHOR'                                               
         MVC   P+48(5),=C'TITLE'                                                
         MVC   P+67(2),=C'ID'                                                   
         MVC   P+72(10),=C'COVER TYPE'                                          
         MVC   P+84(4),=C'DISC'                                                 
         MVC   P+90(3),=C'QTY'                                                  
         MVC   P+95(7),=C'WHOLE $'                                              
         MVC   P+104(11),=C'TOT WHOLE $'                                        
         MVC   P+118(9),=C'TOT RET $'                                           
         MVC   P+129(3),=C'RNK'                                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P(9),DASHES               UNDERLINE HEADING                      
         MVC   P+11(L'BAUTHOR),DASHES                                           
         MVC   P+38(L'BTITLE),DASHES                                            
         MVC   P+66(L'BID),DASHES                                               
         MVC   P+72(10),DASHES                                                  
         MVC   P+84(4),DASHES                                                   
         MVC   P+90(3),DASHES                                                   
         MVC   P+95(7),DASHES                                                   
         MVC   P+104(11),DASHES                                                 
         MVC   P+117(11),DASHES                                                 
         MVC   P+129(3),DASHES                                                  
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
CALCQTY  NTR1                            ADD CURRENT QTY TO TOTAL               
         L     R2,QTYTOTA                                                       
         AR    R2,R8                                                            
         ST    R2,QTYTOTA                                                       
         L     R3,QTYTOTB                                                       
         AR    R3,R8                                                            
         ST    R3,QTYTOTB                                                       
         XIT1                                                                   
*                                                                               
CALCTOT1 NTR1                            ADD CURRENT W-SALE $ TO TOTAL          
         L     R3,WHOLETA                                                       
         AR    R3,R9                                                            
         ST    R3,WHOLETA                                                       
         L     R2,WHOLETB                                                       
         AR    R2,R9                                                            
         ST    R2,WHOLETB                                                       
         XIT1                                                                   
*                                                                               
CALCTOT2 NTR1                            ADD CURRENT RETAIL $ TO TOTAL          
         L     R3,RETTOTA                                                       
         AR    R3,R9                                                            
         ST    R3,RETTOTA                                                       
         L     R2,RETTOTB                                                       
         AR    R2,R9                                                            
         ST    R2,RETTOTB                                                       
         XIT1                                                                   
*                                                                               
SUBTOTA  NTR1                                                                   
         L     R2,QTYTOTA                                                       
         L     R3,WHOLETA                                                       
         L     R4,RETTOTA                                                       
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,SUBLINE                PRINT SUBTOTAL HEADER                  
         MVC   P+2(6),=C'AUTHOR'                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(10),=C'SUBTOTALS:'                                             
         EDIT  (R2),(5,P+14)                                                    
         EDIT  (R3),(11,P+21),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                  
         EDIT  (R4),(11,P+34),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                  
         GOTO1 =V(PRINTER)               PRINT SUBTOTALS                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)               PRINT BLANK LINE                       
         XC    QTYTOTA(12),QTYTOTA       CLEAR SUBTOTALS                        
         XIT1                                                                   
SUBTOTB  NTR1                                                                   
         L     R2,QTYTOTB                                                       
         L     R3,WHOLETB                                                       
         L     R4,RETTOTB                                                       
         GOTO1 =V(PRINTER)               PRINT BLANK LINE                       
         BAS   RE,SUBLINE                PRINT SUBTOTAL HEADER                  
         MVC   P+3(4),=C'TYPE'                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(10),=C'SUBTOTALS:'                                             
         EDIT  (R2),(5,P+14)                                                    
         EDIT  (R3),(11,P+21),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                  
         EDIT  (R4),(11,P+34),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT                  
         GOTO1 =V(PRINTER)               PRINT SUBTOTALS                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)               PRINT BLANK LINE                       
         L     R5,QTYGND                                                        
         AR    R5,R2                     ADD TO GRAND TOTALS                    
         ST    R5,QTYGND                                                        
         L     R5,WHOLEGND                                                      
         AR    R5,R3                                                            
         ST    R5,WHOLEGND                                                      
         L     R5,RETGND                                                        
         AR    R5,R4                                                            
         ST    R5,RETGND                                                        
         XC    QTYTOTB(12),QTYTOTB       CLEAR SUBTOTALS                        
         XIT1                                                                   
*                                                                               
GNDTOT   NTR1                                                                   
         GOTO1 =V(PRINTER)               PRINT BLANK LINE                       
         MVC   P+15(3),=C'QTY'                                                  
         MVC   P+21(11),=C'TOT WHOLE $'                                         
         MVC   P+35(9),=C'TOT RET $'                                            
         GOTO1 =V(PRINTER)               PRINT GRAND TOTAL HEADER               
         MVC   P+14(5),DASHES                                                   
         MVC   P+21(11),DASHES                                                  
         MVC   P+34(11),DASHES                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'GRAND TOTALS:'                                          
         EDIT  (4,QTYGND),(5,P+14)                                              
         EDIT  (4,WHOLEGND),(11,P+21),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT          
         EDIT  (4,RETGND),(11,P+34),2,FLOAT=$,COMMAS=YES,ALIGN=RIGHT            
         GOTO1 =V(PRINTER)               PRINT GRAND TOTALS                     
         XIT1                                                                   
*                                                                               
SUBLINE  NTR1                                                                   
         MVC   P,DASHES                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P+15(3),=C'QTY'                                                  
         MVC   P+21(11),=C'TOT WHOLE $'                                         
         MVC   P+35(9),=C'TOT RET $'                                            
         GOTO1 =V(PRINTER)               PRINT SUBTOTAL HEADER                  
         MVC   P+14(5),DASHES                                                   
         MVC   P+21(11),DASHES                                                  
         MVC   P+34(11),DASHES                                                  
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
GETRANK  NTR1                                                                   
         LA    R4,PREVREC                                                       
PREV     USING BOOKD,R4                                                         
         LA    R5,1                                                             
         STC   R5,BRANK                  GIVE HIGHEST RANK TO LOW $             
         MVC   0(BOOKDLQ,R4),0(R3)       MAKE COPY OF RECORD                    
         AHI   R5,1                      INCREMENT COUNTER                      
LOOP     LA    R3,BOOKDLQ(R3)            GO TO NEXT RECORD                      
         CLC   PREV.BCOST,BCOST          LOOK FOR CHANGE IN COST                
         BNE   ELSE                                                             
         MVC   BRANK,PREV.BRANK          GIVE SAME RANK                         
         B     *+8                                                              
ELSE     STC   R5,BRANK                  GIVE NEXT RANK                         
         MVC   0(BOOKDLQ,R4),0(R3)       MAKE COPY OF RECORD                    
         AHI   R5,1                      INCREMENT COUNTER                      
         CR    R5,R6                                                            
         BNH   LOOP                                                             
         DROP  R4                                                               
         DROP  R3                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
FIL1     DCB   DSORG=PS,RECFM=FB,MACRF=GM,BLKSIZE=700,LRECL=70,        X        
               DDNAME=BOOKFILE,EODAD=SORT                                       
DUB      DS    D                                                                
OUTP     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
QTYTOTA  DS    F                                                                
WHOLETA  DS    F                                                                
RETTOTA  DS    F                                                                
QTYTOTB  DS    F                                                                
WHOLETB  DS    F                                                                
RETTOTB  DS    F                                                                
QTYGND   DS    F                                                                
WHOLEGND DS    F                                                                
RETGND   DS    F                                                                
BYTE     DS    X                                                                
PREVTYPE DS    C                                                                
PREVAUTH DS    CL25                                                             
IOAREA   DS    CL80                                                             
WORK     DS    CL64                                                             
DASHES   DC    132C'-'                                                          
CMASK    DC    X'10'                                                            
DMASK    DC    X'01'                                                            
PREVREC  DS    CL(BOOKDLQ)                                                      
         LTORG                                                                  
INTAB    DS    50CL(BOOKDLQ)                                                    
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
*                                                                               
BOOKD    DSECT                                                                  
*                                                                               
BAUTHOR  DS    CL25                      AUTHOR                                 
BTITLE   DS    CL26                      TITLE                                  
BID      DS    CL4                       ID CODE                                
BCOST    DS    XL2                       WHOLESALE COST (IN CENTS)              
BTYPE    DS    C                         TYPE                                   
BQTY     DS    X                         QUANTITY                               
BFLAGS   DS    X                         VARIOUS                                
BRANK    DS    X                         RANK BY WHOLESALE COST                 
BOOKDLQ  EQU   *-BOOKD                                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159JHIGBK4   10/12/00'                                      
         END                                                                    
