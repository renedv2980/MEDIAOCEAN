*          DATA SET CTCONSCR1  AT LEVEL 003 AS OF 08/17/00                      
*PHASE CTSCR1A                                                                  
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
*  TTL:      CTCONSCR1:  CONTROL FILE LIBRARY SCRIPT RECORDS          *         
*  PURPOSE:  COPY A SCRIPT RECORD FROM ONE CTFILE TO ANOTHER CTFILE   *         
*                                                                     *         
***********************************************************************         
CTSCR1   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CTSCR1*,=V(REGSAVE)                                           
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
         DC    A(CTSCR1),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
MAIN     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',=C'NCTFILE X',  +        
               IO,0                                                             
         MVI   DATADISP+1,28                                                    
         OPEN  (TOUT,OUTPUT)       QSAM MACRO                                   
                                                                                
M10      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'       GET CARD                      
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
                                                                                
         LA    R1,CARD             GET NAME OF SCRIPT                           
         CLC   =C'/*',0(R1)                                                     
         BE    M60                                                              
M20      CLC   =C'SCRIPT=',0(R1)   SCRIPT= IS ONLY VALID                        
         BE    M25                                                              
         MVC   P(7),=C'INVALID'                                                 
         GOTO1 =V(PRINTER)                                                      
         B     M10                 CHECK NEXT ONE                               
                                                                                
M25      LA    R1,7(R1)            LENGTH OF "SCRIPT="                          
         MVC   SCRIPT,0(R1)                                                     
                                                                                
         XC    KEY,KEY             BUILD KEY                                    
         LA    R2,KEY                                                           
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ    TYPE L                                       
         MVI   CTLKSCR,CTLKSCRQ    SCRIPT RECORD                                
         MVC   CTLKNAME,SCRIPT                                                  
         DROP  R2                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
         LA    R6,IO               POINT AT REC READ                            
         B     M40                                                              
                                                                                
M30      GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
         USING CTLREC,R6                                                        
M40      LA    R2,RECLEN                                                        
         CLC   0(CTLKSUB-CTLKEY,R6),KEYSAVE                                     
         BNE   M50                                                              
                                                                                
         MVC   0(2,R2),CTLLEN      GET RECORD LEN FOR PUT                       
         LA    R1,4                ADD 4 TO COVER 2 BYTES LEN + ...             
         AH    R1,0(R2)            2 BYTES OF NULLS                             
         STH   R1,0(R2)                                                         
         PUT   TOUT,(R2)                                                        
                                                                                
         L     R1,COUNT            COUNT NUMBER OF RECORDS                      
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     M30                                                              
                                                                                
M50      MVC   P(11),=C'LINES FOUND'                                            
                                                                                
         OC    COUNT,COUNT         CHECK IF ZERO                                
         BZ    M55                                                              
         L     R1,COUNT            SUBTRACT 1 RECORD FOR HEADER                 
         BCTR  R1,0                                                             
         ST    R1,COUNT                                                         
                                                                                
M55      EDIT  (4,COUNT),(8,P+15),ZERO=NOBLANK                                  
         GOTO1 =V(PRINTER)                                                      
         XC    COUNT,COUNT         CLEAR COUNT                                  
                                                                                
         LA    R1,5                                                             
         STH   R1,0(R2)                                                         
         MVI   4(R2),X'FF'                                                      
         PUT   TOUT,(R2)           ADD END OF SCRIPT RECORD                     
         B     M10                                                              
                                                                                
M60      DS    0H                                                               
         CLOSE TOUT                                                             
                                                                                
         DROP  R6                                                               
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=2048                                              
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
CARD     DS    CL80                                                             
SCRIPT   DS    CL10                                                             
COUNT    DS    F                                                                
DUB      DS    D                                                                
WORK     DS    CL17                                                             
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
DMCB     DS    6F                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTCONSCR1 08/17/00'                                      
         END                                                                    
