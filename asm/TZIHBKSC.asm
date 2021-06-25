*          DATA SET TZIHBKSC   AT LEVEL 138 AS OF 10/03/00                      
*PHASE TZIHBKSC                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'TESTFIB -- FIBONACCI SEQUENCE'                                  
***********************************************************************         
*                                                                               
* THE ASSIGNMENT IS TO PRINT THE FIRST 25 FIBONACCI NUMBERS, ONE PER            
* PRINT LINE.  DON'T WORRY ABOUT SUPPRESSING LEADING ZEROES.                    
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(FIB)' --                 
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTFIB  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTFIB,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTFIB),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* * YOUR CODE HERE *                                                            
MAIN     DS    0H                                                               
* ENHANCEMENT ONE ADDED                                                         
         LA    R2,STORAGE          R2->NEXT EMPTY ENTRY IN TABLE                
         USING BOOKD,R2                                                         
         LA    R7,P                                                             
         USING PRND,R7                                                          
                                                                                
         SR    R3,R3               RECORD COUNTER                               
MAINRD   DS    0H                                                               
* IT IS ASSUMED THAT RECORD LIST ENDS WITH /* AND HAS NO ERRORS IN IT           
         CHI   R3,MAXNOREC         CHECK FOR MAX NO OF RECORDS                  
         BH    TOOMANY             # OF RECORDS EXCEEDS MAX - ERROR             
         GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                       
         CLC   =C'/*',IO           SEE IF TERMINATOR IS READ IN                 
         BE    EMAINRD                                                          
         BAS   RE,CPYREC           CALL ROUTINE TO STORE RECORD                 
         AHI   R2,RECLEN           POINT R2 TO NEXT ENTRY IN TABLE              
         AHI   R3,1                COUNT THE RECORD                             
         B     MAINRD                                                           
EMAINRD  DS    0H                                                               
         MVI   0(R2),X'FF'         MARK END OF TABLE                            
                                                                                
*    CALL THE   SORTING FUNCTION THREE TIMES FROM LEAST TO                      
*    MOST IMPORTANT KEY                                                         
         GOTO1 =V(XSORT),DMCB,STORAGE,(R3),RECLEN,L'BTITLE,            +        
               BTITLE-BOOKD                                                     
         GOTO1 =V(XSORT),DMCB,STORAGE,(R3),RECLEN,L'BAUTHOR,           +        
               BAUTHOR-BOOKD                                                    
         GOTO1 =V(XSORT),DMCB,STORAGE,(R3),RECLEN,L'BTYPE,BTYPE-BOOKD           
                                                                                
         LA    R2,STORAGE          POINT R2 TO BEGINNING OF STORAGE             
         SR    R3,R3               R3 HAS TOTAL QUANTITY                        
         SR    R4,R4               R4 HAS TOTAL WHOLESALE COST                  
         SR    R5,R5               R5 HAS TOTAL RETAIL COST                     
                                                                                
         MVC   CURTYPE,BTYPE       RECORD BOOK TYPE WHICH IS WORKED ON          
         MVC   CURAUTH,BAUTHOR     RECORD BOOK AUTHOR                           
                                                                                
* PRINT OUT THE TITLE LINE                                                      
                                                                                
         MVC  PTYPE(4),=C'TYPE'                                                 
         MVC  PAUTHOR(6),=C'AUTHOR'                                             
         MVC  PTITLE(5),=C'TITLE'                                               
         MVC  PID(2),=C'ID'                                                     
         MVC  PCOVER(4),=C'COVR'                                                
         MVC  PDISC(4),=C'DISC'                                                 
         MVC  PQTY(3),=C'QTY'                                                   
         MVC  PWSCOST(4),=C'COST'                                               
         MVC  PTWSCOST(4),=C'COST'                                              
         MVC  PTRCOST(4),=C'COST'                                               
         L    RF,=V(PRINTER)                                                    
         BASR RE,RF                                                             
                                                                                
MAINPR   CLI   0(R2),X'FF'                                                      
         BE    EMAINPR             FINISH, IF END OF TABLE                      
                                                                                
         CLC   CURTYPE,BTYPE       IF TYPE CHANGED, PRINT OUT TOTALS            
         BE    MAIN20                                                           
         MVC   CURTYPE,BTYPE                                                    
         MVC   CURAUTH,BAUTHOR     REMEMBER CURRENT AUTHOR                      
         BAS   RE,PRNTOT           CALL TOTALS-PRINTING ROUTINE                 
         BAS   RE,BPRNTOT          TOTALS-PRINTING ROUTINE FOR BOOK TYP         
         B     MAIN30                                                           
                                                                                
MAIN20   DS    0H                                                               
         CLC   CURAUTH,BAUTHOR     IF AUTHOR CHANGED, PRINT OUT TOTALS          
         BE    MAINCONT            IF NO, CHECK IF BOOK TYPE CHANGED            
         MVC   CURTYPE,BTYPE       REMEMBER CURRENT TYPE                        
         MVC   CURAUTH,BAUTHOR     REMEMBER CURRENT AUTHOR                      
         BAS   RE,PRNTOT           CALL TOTALS-PRINTING ROUTINE                 
                                                                                
MAIN30   SR    R3,R3               INITIALIZE REGISTERS FOR SUBTOTALS           
         SR    R4,R4               <                                            
         SR    R5,R5               <                                            
                                                                                
                                                                                
MAINCONT DS    0H                                                               
         BAS   RE,FORMATP          PUT WHATEVER IS NEEDED ON PRINTLINE          
                                                                                
         ZIC   R9,BQTY                                                          
         AR    R3,R9               SUM UP THE QUANTITY                          
         A     R4,FULL             SUM UP TOT WHOLES. COST                      
         A     R5,FULL2            SUM UP TOT RETAIL COST                       
                                                                                
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
                                                                                
         AHI   R2,RECLEN           BUMP TO NEXT RECORD                          
         B     MAINPR                                                           
EMAINPR  DS    0H                                                               
         BAS   RE,PRNTOT           PRINT TOTAL FOR LAST AUTHOR                  
         BAS   RE,BPRNTOT           PRINT TOTAL FOR LAST BOOK TYPE              
                                                                                
* PRINT GRAND TOTALS                                                            
         MVC   PAUTHOR(20),=CL20'GRAND TOTAL'                                   
         EDIT  GTQTY,(L'PQTY,PQTY)                                              
         EDIT  GTWSC,(L'PTWSCOST,PTWSCOST),2,FLOAT=$                            
         EDIT  GTRC,(L'PTRCOST,PTRCOST),2,FLOAT=$                               
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
                                                                                
         B     END                 SKIP OVER ERROR MESSAGE                      
TOOMANY  DS    0H                  PRINT OUT ERROR MESSAGE                      
         MVC   P(50),=CL20'ERROR:    INPUT FILE HAS TOO MANY RECORDS'           
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         B     END                 BRANCH AROUND EXIT                           
                                                                                
EXIT     DS    0H                                                               
         XIT1                      XIT1 MACRO FOR SUBROUTINES                   
                                                                                
END      DS    0H                                                               
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
CPYREC    NTR1                                                                  
* COPIES  RECORD THAT SITS IN IO INTO STORAGE AREA                              
* R2 MUST POINT TO BEGINNING OF STORAGE TABLE                                   
          LA    R5,IO                                                           
          USING INPUTD,R5                                                       
                                                                                
          MVC   BAUTHOR,IAUTHOR                                                 
          MVC   BTITLE,ITITLE                                                   
          MVC   BID,IBID                                                        
          MVC   BTYPE,ITYPE                                                     
          MVC   BQTY,IQTY                                                       
                                                                                
          PACK  DUB,ICOST(L'ICOST)                                              
          CVB   R6,DUB                                                          
          STCM  R6,3,BCOST                                                      
                                                                                
          PACK  DUB,IQTY(L'IQTY)                                                
          CVB   R6,DUB                                                          
          STC   R6,BQTY                                                         
                                                                                
          XC    BFLAGS,BFLAGS       INITIALIZE FLAGS TO ZERO                    
                                                                                
          CLI   ICOVER,C'P'         PROCESS PAPERBACK FLAG                      
          BNE   CPY10               HARDCOVER IS DEFAULT - DO NEXT FLAG         
          OI    BFLAGS,PPBMQ        SET 'PAPERBACK' BIT                         
                                                                                
CPY10     DS    0H                                                              
          CLI   IDISC,C'Y'          PROCESS DISCOUNT FLAG                       
          BNE   CPY20                                                           
          OI    BFLAGS,DISCMQ       SET 'DISCOUNT' BIT                          
                                                                                
CPY20     DS    0H                                                              
          DROP  R5                                                              
          B     EXIT                                                            
          EJECT                                                                 
                                                                                
FORMATP   NTR1                                                                  
* R2 MUST POINT TO BEGINNING OF RECORD                                          
* ITEMS ARE COPIED FROM STORAGE TABLE INTO PRINT LINE                           
                                                                                
                                                                                
          MVC   PTYPE,BTYPE         COPY ITEMS                                  
          MVC   PAUTHOR,BAUTHOR                                                 
          MVC   PTITLE,BTITLE                                                   
          MVC   PID,BID                                                         
                                                                                
          TM    BFLAGS,DISCMQ       TEST IF DISCOUNT APPLIES                    
          BZ    FORMP10                                                         
          MVI   PDISC,C'Y'          IF APPLICABLE, PRINT 'Y'                    
                                                                                
          B     FORMP30             SKIP PRINTING 'N'                           
                                                                                
FORMP10   DS    0H                                                              
          MVI   PDISC,C'N'          IF NOT, PRINT 'N'                           
                                                                                
FORMP30   DS    0H                                                              
                                                                                
          TM    BFLAGS,PPBMQ        SEE IF COVER IS PAPERBACK                   
          BZ    FORMP20                                                         
          MVI   PCOVER,C'P'         IF PAPERBACK PRINT 'P'                      
                                                                                
          B     FORMP40             SKIP PRINTING 'H'                           
                                                                                
FORMP20   DS    0H                                                              
          MVI   PCOVER,C'H'         IF NOT PAPERBACK = HARDCOVER                
                                                                                
FORMP40   DS    0H                                                              
                                                                                
* VALUES FOR QUANTITY AND COSTS ARE STORED IN BINARY AND NEED EDITING           
          EDIT  BQTY,(L'PQTY,PQTY)                                              
          EDIT  BCOST,(L'PWSCOST,PWSCOST),2,FLOAT=$                             
          BAS   RE,GETCOST                                                      
          EDIT  FULL,(L'PTWSCOST,PTWSCOST),2,FLOAT=$                            
          EDIT  FULL2,(L'PTRCOST,PTRCOST),2,FLOAT=$                             
                                                                                
          B     EXIT                                                            
          EJECT                                                                 
                                                                                
                                                                                
GETCOST   NTR1                                                                  
* R2 MUST POINT TO BEGINNING OF REC                                             
*VALUEOF TOTAL WHOLESALE COST IS RETURNED IN MEMORY LOC FULL                    
*VALUEOF TOTAL RETAIL COST IS RETURNED IN MEMORY LOC FULL2                      
                                                                                
          ZIC   R5,BQTY             INSERT QUANTITY IN R5                       
          MH    R5,BCOST            MULTIPLY BY COST                            
          ST    R5,FULL             STORE                                       
          SR    R4,R4               INITIALIZE EVEN-ODD PAIR                    
          LA    R3,2                < EQUIVALENT TO X1.5                        
          DR    R4,R3               <                                           
          MHI   R5,3                <                                           
          ST    R5,FULL2                                                        
          B     EXIT                                                            
                                                                                
PRNTOT    NTR1                                                                  
* SUBROUTINE FOR PRINTING OUT TOTALS                                            
* TOTALS ARE PRINTED FOR: QUANTITY, WHOLESALE AND RETAIL COSTS                  
                                                                                
          MVC   PAUTHOR(20),=CL20'TOTAL'                                        
          EDIT  (R3),(L'PQTY,PQTY)                                              
          EDIT  (R4),(L'PTWSCOST,PTWSCOST),2,FLOAT=$                            
          EDIT  (R5),(L'PTRCOST,PTRCOST),2,FLOAT=$                              
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
                                                                                
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
                                                                                
          A     R3,BGTQTY            ADD TO GRAND TOTAL                         
          ST    R3,BGTQTY            STORE GRAND TOTAL                          
          A     R4,BGTWSC            <                                          
          ST    R4,BGTWSC            <                                          
          A     R5,BGTRC             <                                          
          ST    R5,BGTRC             <                                          
                                                                                
          B     EXIT                                                            
          EJECT                                                                 
                                                                                
BPRNTOT    NTR1                                                                 
* SUBROUTINE FOR PRINTING OUT TOTALS BY BOOK TYPE                               
* TOTALS ARE PRINTED FOR: QUANTITY, WHOLESALE AND RETAIL COSTS                  
                                                                                
          MVC   PAUTHOR(25),=CL20'TOTAL FOR THIS BOOK TYPE'                     
          EDIT  BGTQTY,(L'PQTY,PQTY)                                            
          EDIT  BGTWSC,(L'PTWSCOST,PTWSCOST),2,FLOAT=$                          
          EDIT  BGTRC,(L'PTRCOST,PTRCOST),2,FLOAT=$                             
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
                                                                                
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
                                                                                
          A     R3,BGTQTY            ADD TO GRAND TOTAL FOR BOOK TYPE           
          ST    R3,BGTQTY            STORE GRAND TOTAL                          
          A     R4,BGTWSC            <                                          
          ST    R4,BGTWSC            <                                          
          A     R5,BGTRC             <                                          
          ST    R5,BGTRC             <                                          
                                                                                
          L     R3,GTQTY             ADD TO GRAND TOTAL                         
          A     R3,BGTQTY             ADD TO GRAND TOTAL                        
          ST    R3,GTQTY             STORE GRAND TOTAL                          
          L     R4,GTWSC             <                                          
          A     R4,BGTWSC             <                                         
          ST    R4,GTWSC             <                                          
          L     R5,GTRC              <                                          
          A     R5,BGTRC              <                                         
          ST    R5,GTRC              <                                          
          B     EXIT                                                            
          EJECT                                                                 
                                                                                
*                                                                               
DUB       DS    D                                                               
FULL      DS    F                                                               
FULL2     DS    F                                                               
GTQTY     DC    F'0'                GRAND TOTAL QUANTITY                        
GTWSC     DC    F'0'                GRAND TOTAL WHOLESALE COST                  
GTRC      DC    F'0'                GRAND TOTAL RETAIL COST                     
BGTQTY    DC    F'0'               GRAND TOTAL QUANTITIES BY BOOK TYPE          
BGTWSC    DC    F'0'               <                                            
BGTRC     DC    F'0'               <                                            
DMCB     DS    6F                                                               
HALF      DS    H                                                               
WORK      DS    CL54                                                            
IO        DS    CL80                                                            
CURTYPE   DS    C                   VAR FOR STORING BOOK TYPE PROCESSED         
CURAUTH   DS    CL(L'BAUTHOR)                                                   
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
STORAGE   DS    (MAXNOREC)CL(RECLEN)                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
*                                                                               
MAXNOREC  EQU   50                  MAX NUMBER OF RECORDS                       
*                                                                               
INPUTD    DSECT                                                                 
*                                                                               
ITITLE    DS    CL26                                                            
IAUTHOR   DS    CL26                                                            
          DS    CL3                 SPARE                                       
IBID      DS    CL4                                                             
ITYPE     DS    C                                                               
ICOST     DS    CL4                                                             
IQTY      DS    CL3                                                             
IDISC     DS    C                                                               
ICOVER    DS    C                                                               
          DS    CL11                SPARE                                       
INRECLEN  EQU   *-INPUTD                                                        
*                                                                               
*                                                                               
BOOKD     DSECT                                                                 
*                                                                               
BAUTHOR   DS    CL26                                                            
BTITLE    DS    CL25                                                            
BID       DS    CL4                                                             
BCOST     DS    XL2                                                             
BTYPE     DS    C                                                               
BQTY      DS    X                                                               
BFLAGS    DS    X                                                               
* BIT#    VALUE MEANING                                                         
*    8    1/0   PAPERBACK/HARDCOVER                                             
*    7    1/0   DISCOUNT/NONE                                                   
RECLEN    EQU   *-BOOKD                                                         
*                                                                               
PRND      DSECT                                                                 
*                                                                               
PTYPE     DS    C                                                               
          DS    (SPC)C                                                          
PAUTHOR   DS    CL26                                                            
          DS    (SPC)C                                                          
PTITLE    DS    CL25                                                            
          DS    (SPC)C                                                          
PID       DS    CL4                                                             
          DS    (SPC)C                                                          
PCOVER    DS    C                                                               
          DS    (SPC)C                                                          
PDISC     DS    C                                                               
          DS    (SPC)C                                                          
PQTY      DS    CL5                                                             
          DS    (SPC)C                                                          
PWSCOST   DS    CL10                                                            
          DS    (SPC)C                                                          
PTWSCOST  DS    CL10                                                            
          DS    (SPC)C                                                          
PTRCOST   DS    CL10                                                            
          DS    (SPC)C                                                          
PRECLEN   EQU   *-PRND                                                          
SPC       EQU   4                   SPACE BETWEEN ITEMS IN OUTPUT               
*                                                                               
PPBMQ     EQU   X'80'               MASK TO SET/RESET PAPERBACK BIT             
DISCMQ    EQU   X'40'               MASK TO SET/RESET DISCOUNT BIT              
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'138TZIHBKSC  10/03/00'                                      
         END                                                                    
