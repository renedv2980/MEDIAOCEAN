*          DATA SET CTCONSCR2  AT LEVEL 004 AS OF 08/17/00                      
*PHASE CTSCR2A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE CARDS                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
***********************************************************************         
*  TTL:      CTCONSCR2:  CONTROL FILE LIBRARY SCRIPT RECORDS          *         
*  PURPOSE:  COPY A SCRIPT RECORD FROM ONE CTFILE TO ANOTHER CTFILE   *         
*                                                                     *         
***********************************************************************         
CTSCR2   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CTSCR2*,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
                                                                                
DUMPLIST DS    0F                                                               
         DC    A(CTSCR2),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
MAIN     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',                +        
               =C'UCTFILE UCTRCVR X',IO                                         
         MVI   DATADISP+1,28                                                    
         MVI   ENQFLAG,C'N'                                                     
         MVI   TRACE,C'N'                                                       
         OPEN  (TIN,INPUT)         QSAM MACRO                                   
         LA    R2,RECLEN                                                        
         LA    R4,IO2                                                           
         USING CTLREC,R4                                                        
                                                                                
M10      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'       GET CARD                      
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
                                                                                
         LA    R1,CARD             GET CARD                                     
         CLC   =C'/*',0(R1)                                                     
         BE    M25                                                              
         CLC   =C'TRACE=',0(R1)    TRACE CARD IS ONLY VALID                     
         BE    M20                                                              
         MVC   P(7),=C'INVALID'                                                 
         GOTO1 =V(PRINTER)                                                      
         B     M10                 CHECK NEXT ONE                               
                                                                                
M20      LA    R1,6(R1)            LENGTH OF "TRACE="                           
         CLI   0(R1),C'Y'          TRACE=Y?                                     
         BNE   M10                                                              
         MVI   TRACE,C'Y'                                                       
         B     M10                                                              
                                                                                
M25      GET   TIN,(R2)                                                         
                                                                                
         CLI   IO,X'FF'            CHECK IF LAST SCRIPT W/ SAME NAME            
         BE    M25                                                              
                                                                                
         MVC   KEY,IO              KEY OF RECORD READ                           
         BAS   RE,ENQ              ENQ CONTROL FILE                             
         GOTO1 =V(DATAMGR),DMCB,(X'80',=C'DMRDHI'),=C'CTFILE',KEY,IO2           
         B     M35                                                              
                                                                                
M30      GOTO1 =V(DATAMGR),DMCB,(X'80',=C'DMRSEQ'),=C'CTFILE',KEY,IO2           
M35      CLC   IO2(CTLKSUB-CTLKEY),KEY                                          
         BNE   M40                                                              
                                                                                
         L     R1,COUNT            COUNT NUMBER OF RECORDS DELETING             
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
                                                                                
         LA    R4,IO2                                                           
         OI    CTLSTAT,X'80'  MARK ALL EXISTING ONES FOR DELETION               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT '),=C'CTFILE',KEY,IO2,     +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    M30                                                              
         DC    H'0'                DIE                                          
                                                                                
M40      LA    R4,KEY                                                           
         MVC   P(L'CTLKNAME),CTLKNAME  NAME OF SCRIPT                           
                                                                                
         CLI   TRACE,C'N'                                                       
         BE    M50                                                              
                                                                                
         OC    COUNT,COUNT         CHECK IF ZERO                                
         BZ    M45                                                              
         L     R1,COUNT            SUBTRACT 1 RECORD FOR HEADER                 
         BCTR  R1,0                                                             
         ST    R1,COUNT                                                         
                                                                                
M45      MVC   P+15(13),=C'LINES DELETED'                                       
         EDIT  (4,COUNT),(8,P+35),ZERO=NOBLANK                                  
         GOTO1 =V(PRINTER)                                                      
         XC    COUNT,COUNT                                                      
                                                                                
M50      CLI   IO,X'FF'            CHECK IF LAST SCRIPT W/ SAME NAME            
         BNE   M60                                                              
         BAS   RE,DEQ                                                           
                                                                                
         OC    COUNT,COUNT         CHECK IF ZERO                                
         BZ    M55                                                              
         L     R1,COUNT            SUBTRACT 1 RECORD FOR HEADER                 
         BCTR  R1,0                                                             
         ST    R1,COUNT                                                         
                                                                                
M55      CLI   TRACE,C'N'                                                       
         BE    M25                                                              
                                                                                
         LA    R4,KEY                                                           
                                                                                
         MVC   P(L'CTLKNAME),CTLKNAME  NAME OF SCRIPT                           
         MVC   P+15(11),=C'LINES ADDED'                                         
         EDIT  (4,COUNT),(8,P+35),ZERO=NOBLANK                                  
         GOTO1 =V(PRINTER)                                                      
         XC    COUNT,COUNT                                                      
         B     M25                 GET NEXT SET                                 
                                                                                
M60      MVC   KEY,IO              KEY OF RECORD READ FROM DATA SET             
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'DMREAD'),=C'CTFILE',KEY,IO2,0         
         TM    DMCB+8,X'10'        RECORD NOT FOUND                             
         BNZ   M65                                                              
         CLI   DMCB+8,X'02'        RECORD IS DELETED                            
         BE    M70                                                              
         DC    H'0'                                                             
                                                                                
M65      MVC   KEY,IO              KEY OF RECORD READ                           
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMADD '),=C'CTFILE',KEY,IO,      +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    M75                                                              
         DC    H'0'                DIE                                          
                                                                                
M70      MVC   KEY,IO              KEY OF RECORD READ                           
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT '),=C'CTFILE',KEY,IO,      +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    M75                                                              
         DC    H'0'                DIE                                          
                                                                                
M75      L     R1,COUNT            COUNT NUMBER OF RECORDS                      
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
                                                                                
         GET   TIN,(R2)                                                         
         B     M50                                                              
                                                                                
M80      BAS   RE,DEQ              JUST IN CASE                                 
         CLOSE TIN                                                              
                                                                                
         XBASE                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ENQ      NTR1                                                                   
                                                                                
         CLI   ENQFLAG,C'Y'                                                     
         BE    EXIT                                                             
                                                                                
*&&UK*&& MVC   ENQCMND,=C'ENQDEQ  '                                             
*&&US*&& MVC   ENQCMND,=C'ENQCTL  '                                             
         GOTO1 =V(DATAMGR),DMCB,(0,ENQCMND),(C'T',=C'CTRL')                     
         TM    8(R1),X'01'                                                      
         BO    EXIT                SYSTEM IS ALREADY ENQUEUED BY ME             
         GOTO1 =V(DATAMGR),DMCB,(0,ENQCMND),(C'E',=C'CTRL')                     
                                                                                
         MVI   ENQFLAG,C'Y'                                                     
         B     EXIT                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DEQ      NTR1                                                                   
         CLI   ENQFLAG,C'N'                                                     
         BE    EXIT                                                             
         GOTO1 =V(DATAMGR),DMCB,(0,ENQCMND),(C'D',=C'CTRL')                     
         MVI   ENQFLAG,C'N'                                                     
EXIT     XIT1                                                                   
                                                                                
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TIN      DCB   DDNAME=TIN,DSORG=PS,MACRF=GM,EODAD=M80,                 +        
               RECFM=VB,LRECL=2048                                              
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRACE    DS    C                                                                
ENQFLAG  DS    C                                                                
ENQCMND  DS    CL8                                                              
CARD     DS    CL80                                                             
COUNT    DS    F                                                                
DUB      DS    D                                                                
WORK     DS    CL17                                                             
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
IO2      DS    XL2000              IO AREA                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTCONSCR2 08/17/00'                                      
         END                                                                    
