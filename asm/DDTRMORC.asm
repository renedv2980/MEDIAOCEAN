*          DATA SET DDTRMORC   AT LEVEL 049 AS OF 04/18/01                      
*PHASE RMORCA                                                                   
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'GENERAL TAPE FIX '                                              
         SPACE 1                                                                
CH4FIX   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**RFIX**,=V(REGSAVE),RA,R9                                     
         B     MAIN                                                             
*                                                                               
XBASE    XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        INITIALISATION                                               *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
         OPEN  (OLDTAPE,INPUT)     OPEN ALL FILES                               
         OPEN  (NEWTAPE,OUTPUT)                                                 
*                                                                               
         LA    R3,IOAREA1                                                       
         LA    R3,4(R3)                                                         
NEXTREC  XC    IOAREA1(255),IOAREA1                                             
         GET   OLDTAPE,IOAREA1     GET A RECORD                                 
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,IOAREA1                                                     
         LA    RF,IOAREA1+4                                                     
*                                                                               
NEXT010  CLC   0(7,RF),=X'2307E3F6F2F1C2'                                       
         BE    PUTIT                                                            
         LA    RF,1(RF)                                                         
         BCT   R1,NEXT010                                                       
         B     NEXTREC                                                          
*                                                                               
PUTIT    PUT   NEWTAPE,IOAREA1     PUT A RECORD                                 
         B     NEXTREC                                                          
***************************************                                         
*                                     *                                         
***************************************                                         
*                                                                               
EODT1    MVI   EOF,C'Y'                                                         
         B     CLOSEALL                                                         
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES AND EXIT                               *                   
*************************************************************                   
         SPACE 1                                                                
CLOSEALL CLOSE OLDTAPE                                                          
         CLOSE NEWTAPE                                                          
         B     XBASE                                                            
         LTORG                                                                  
         SPACE                                                                  
VSORTER  DC    V(SORTER)                                                        
         EJECT                                                                  
DMWORK   DS    12D                 WORKING STORAGE                              
DUB      DS    D                                                                
DUB1     DS    D                                                                
TSAVE    DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
SAVERE   DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
CNTL     DS    X                                                                
EOF      DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
WORK     DS    CL250                                                            
OLDBOOK  DS    CL8                                                              
*                                                                               
FIVE     DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        TABLES AND CONSTANTS                                         *         
***********************************************************************         
         SPACE 1                                                                
GO       DC    C'N'                                                             
MYSPACES DC    CL132' '                                                         
DASHES   DC    132C'-'                                                          
FFS      DS    0XL64                                                            
         DC    64X'FF'                                                          
DOTS     DC    32C'.'                                                           
SORTCARD DC    C'SORT FIELDS=(17,20,BI,A,5,9,BI,A) ' NAME,YR/WK/DY/TIME         
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(64,,,37,48) '                            
         EJECT                                                                  
***********************************************************************         
*        DCBS                                                         *         
***********************************************************************         
         SPACE 1                                                                
OLDTAPE  DCB   DDNAME=OLDTAPE,DSORG=PS,MACRF=(GM),EODAD=EODT1                   
NEWTAPE  DCB   DDNAME=NEWTAPE,DSORG=PS,MACRF=(PM)                               
         EJECT                                                                  
***********************************************************************         
*        OUT OF RANGE WORKING STORAGE                                 *         
***********************************************************************         
         SPACE 1                                                                
IOAREA1  DS    4096C                                                            
*                                                                               
IOAREA2  DS    4096C                                                            
         EJECT                                                                  
**********************************************************************          
*        OTHER DSECTS                                                *          
**********************************************************************          
         SPACE 1                                                                
O4DATA   DSECT                                                                  
O4DLEN   DS    XL2                                                              
         DS    XL2                                                              
O4DKEY   DS    0CL9                                                             
O4DYEAR  DS    XL2                 YEAR                                         
O4DWEEK  DS    XL2                 ISO WEEK                                     
O4DDAY   DS    XL1                 DAY NUMBER 1 - 7                             
O4DTIME  DS    XL4                 TIME HHMM                                    
O4DPLEN  DS    XL3                 DURATION MINS                                
O4DPNAME DS    CL20                PROGRAMME NAME                               
O4DLENQ  EQU   *-O4DATA                                                         
         SPACE 1                                                                
N4DATA   DSECT                                                                  
N4DLEN   DS    XL2                                                              
         DS    XL2                                                              
N4DKEY   DS    0CL9                                                             
N4DYEAR  DS    XL2                 YEAR                                         
N4DWEEK  DS    XL2                 ISO WEEK                                     
N4DDAY   DS    XL1                 DAY NUMBER 1 - 7                             
N4DTIME  DS    XL4                 TIME HHMM                                    
N4DPLEN  DS    XL3                 DURATION MINS                                
N4DPNAME DS    CL20                PROGRAMME NAME                               
N4DLENQ  EQU   *-N4DATA                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049DDTRMORC  04/18/01'                                      
         END                                                                    
