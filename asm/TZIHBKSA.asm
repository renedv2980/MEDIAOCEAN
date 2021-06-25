*          DATA SET TZIHBKSA   AT LEVEL 113 AS OF 09/28/00                      
*PHASE TZIHBKSA                                                                 
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
MAIN      DS    0H                                                              
*                                                                               
          LA    R2,STORAGE          R2->NEXT EMPTY ENTRY IN TABLE               
          USING BOOKD,R2                                                        
          SR    R3,R3               RECORD COUNTER                              
READIN    DS    0H                  INPUT LIST MUST END WITH /*                 
          GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                      
          CLC   =C'/*',IO           SEE IF TERMINATOR IS READ IN                
          BE    EREADIN                                                         
          CHI   R3,MAXNOREC                                                     
          BNL   TOOMANY             THEN # OF RECORDS EXCEEDS MAX               
          BAS   RE,CPYREC                                                       
          AHI   R2,RECLEN           POINT R2 TO NEXT ENTRY IN TABLE             
          AHI   R3,1                COUNT THE RECORD                            
          B     READIN                                                          
EREADIN   DS    0H                                                              
          MVI   0(R2),X'FF'         MARK END OF TABLE                           
*                                                                               
          GOTO1 =V(XSORT),DMCB,STORAGE,(R3),RECLEN,L'BTYPE,BTYPE-BOOKD          
*                                                                               
          LA    R2,STORAGE                                                      
          LA    R3,0                R3 HAS TOTAL QUANTITY                       
          LA    R4,0                R4 HAS TOTAL WHOLESALE COST                 
          LA    R5,0                R5 HAS TOTAL RETAIL COST                    
                                                                                
          MVC   CURTYPE,BTYPE                                                   
                                                                                
PLOOP     CLI   0(R2),X'FF'                                                     
          BE    EPLOOP              FINISH, IF END OF TABLE                     
                                                                                
          CLC   CURTYPE,BTYPE       IF TYPE CHANGED, PRINT OUT TOTALS           
          BE    CONTINUE                                                        
          MVC   CURTYPE,BTYPE       REMEMBER CURRENT TYPE                       
          BAS   RE,PRNTOT                                                       
          SR    R3,R3               INITIALIZE REGISTERS                        
          SR    R4,R4                                                           
          SR    R5,R5                                                           
                                                                                
CONTINUE  DS    0H                                                              
          BAS   RE,FORMATP                                                      
                                                                                
          ZIC   R9,BQTY                                                         
          AR    R3,R9               SUM UP THE QUANTITY                         
          A     R4,FULL             SUM UP TOT WHOLES. COST                     
          A     R5,FULL2            SUM UP TOT RETAIL COST                      
                                                                                
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          AHI   R2,RECLEN                                                       
          B     PLOOP                                                           
EPLOOP    DS    0H                                                              
          BAS   RE,PRNTOT                                                       
*                                                                               
          LA    R7,P                                                            
          USING PRND,R7                                                         
          MVC   PAUTHOR(20),=CL20'GRAND TOTAL'                                  
          EDIT  GTQTY,(L'PQTY,PQTY)                                             
          EDIT  GTWSC,(L'PTWSCOST,PTWSCOST),2,FLOAT=$                           
          EDIT  GTRC,(L'PTRCOST,PTRCOST),2,FLOAT=$                              
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          DROP  R7                                                              
*                                                                               
          B     END                                                             
TOOMANY   DS    0H                                                              
          MVC   P(50),=CL20'ERROR:    INPUT FILE HAS TOO MANY RECORDS'          
END       DS    0H                                                              
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
CPYREC    NTR1                                                                  
* COPIES  RECRD INTO STORAGE AREA                                               
* R2 MUST POINT TO BEGINNING OF REC                                             
          LA    R5,IO                                                           
          USING INPUTD,R5                                                       
*                                                                               
          MVC   BAUTHOR,IAUTHOR                                                 
          MVC   BTITLE,ITITLE                                                   
          MVC   BID,IBID                                                        
          MVC   BTYPE,ITYPE                                                     
          MVC   BQTY,IQTY                                                       
*                                                                               
          PACK  DUB,ICOST(L'ICOST)                                              
          CVB   R6,DUB                                                          
          STCM  R6,3,BCOST                                                      
                                                                                
          PACK  DUB,IQTY(L'IQTY)                                                
          CVB   R6,DUB                                                          
          STC   R6,BQTY                                                         
                                                                                
          XC    BFLAGS,BFLAGS                                                   
                                                                                
          CLI   ICOVER,C'P'                                                     
          BNE   CPY10                                                           
          OI    BFLAGS,PPBMQ                                                    
CPY10     DS    0H                                                              
          CLI   IDISC,C'Y'                                                      
          BNE   CPY20                                                           
          OI    BFLAGS,DISCMQ                                                   
CPY20     DS    0H                                                              
*                                                                               
          DROP  R5                                                              
          XIT1                                                                  
*                                                                               
FORMATP   NTR1                                                                  
* R2 MUST POINT TO BEGINNING OF REC                                             
*                                                                               
          LA    R4,P                                                            
          USING PRND,R4                                                         
*                                                                               
          MVC   PTYPE,BTYPE                                                     
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
                                                                                
          EDIT  BQTY,(L'PQTY,PQTY)                                              
          EDIT  BCOST,(L'PWSCOST,PWSCOST),2,FLOAT=$                             
          BAS   RE,GETCOST                                                      
          EDIT  FULL,(L'PTWSCOST,PTWSCOST),2,FLOAT=$                            
          EDIT  FULL2,(L'PTRCOST,PTRCOST),2,FLOAT=$                             
*                                                                               
          DROP  R4                                                              
          XIT1                                                                  
*                                                                               
GETCOST   NTR1                                                                  
* R2 MUST POINT TO BEGINNING OF REC                                             
*VALUEOF TOTAL WHOLESALE COST IS RETURNED IN MEMORY LOC FULL                    
*VALUEOF TOTAL RETAIL COST IS RETURNED IN MEMORY LOC FULL2                      
*                                                                               
          ZIC   R5,BQTY             INSERT QUANTITY IN R5                       
          MH    R5,BCOST            MULTIPLY BY COST                            
          ST    R5,FULL             STORE                                       
          SR    R4,R4               INITIALIZE EVEN-ODD PAIR                    
          LA    R3,2                < EQUIVALENT TO X1.5                        
          DR    R4,R3               <                                           
          MHI   R5,3                <                                           
          ST    R5,FULL2                                                        
          XIT1                                                                  
*                                                                               
PRNTOT    NTR1                                                                  
*                                                                               
          LA    R7,P                                                            
          USING PRND,R7                                                         
                                                                                
          MVC   PAUTHOR(20),=CL20'TOTAL'                                        
          EDIT  (R3),(L'PQTY,PQTY)                                              
          EDIT  (R4),(L'PTWSCOST,PTWSCOST),2,FLOAT=$                            
          EDIT  (R5),(L'PTRCOST,PTRCOST),2,FLOAT=$                              
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          A     R3,GTQTY            ADD TO GRAND TOTAL                          
          ST    R3,GTQTY            STORE GRAND TOTAL                           
          A     R4,GTWSC                                                        
          ST    R4,GTWSC                                                        
          A     R5,GTRC                                                         
          ST    R5,GTRC                                                         
          DROP  R7                                                              
          XIT1                                                                  
*                                                                               
DUB       DS    D                                                               
FULL      DS    F                                                               
FULL2     DS    F                                                               
GTQTY     DC    F'0'                GRAND TOTAL QUANTITY                        
GTWSC     DC    F'0'                GRAND TOTAL WHOLESALE COST                  
GTRC      DC    F'0'                GRAND TOTAL RETAIL COST                     
DMCB     DS    6F                                                               
HALF      DS    H                                                               
WORK      DS    CL54                                                            
IO        DS    CL80                                                            
CURTYPE   DS    C                                                               
NR        DC    X'00'               ACTUAL NO OF RECS, INITIALLY=0              
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
IUNUSED   DS    CL3                                                             
IBID      DS    CL4                                                             
ITYPE     DS    C                                                               
ICOST     DS    CL4                                                             
IQTY      DS    CL3                                                             
IDISC     DS    C                                                               
ICOVER    DS    C                                                               
IUNUSED2  DS    CL11                                                            
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
SPC       EQU   2                   SPACE BETWEEN ITEMS IN OUTPUT               
*                                                                               
PPBMQ     EQU   X'80'               MASK TO SET/RESET PAPERBACK BIT             
DISCMQ    EQU   X'40'               MASK TO SET/RESET DISCOUNT BIT              
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113TZIHBKSA  09/28/00'                                      
         END                                                                    
