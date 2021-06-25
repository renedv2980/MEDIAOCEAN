*          DATA SET DDTRMOREZ  AT LEVEL 020 AS OF 05/04/00                      
*PHASE RMOREZ,*                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
         SPACE 1                                                                
         TITLE 'GENERAL TAPE FIX '                                              
         EJECT                                                                  
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
         OPEN  (SYSPRINT,OUTPUT)                                                
*                                                                               
NEXTREC  GET   OLDTAPE             GET A RECORD                                 
         LR    R2,R1                                                            
         CLC   8(8,R2),=C'*EOFEOF*'                                             
         BE    NEXT02                                                           
         CLC   8(8,R2),=C'*SOFSOF*'                                             
         BNE   NEXT01                                                           
*                                                                               
         XC    FILSIZE,FILSIZE                                                  
         MVC   HALF,16(R2)         GET USERID                                   
         NC    HALF,=X'0007'                                                    
         LH    R3,HALF                                                          
         MH    R3,=H'24'                                                        
         LA    R3,WRKFTAB(R3)      POINT R3 TO WRKFTAB                          
         B     NEXTREC                                                          
*                                                                               
NEXT01   SR    R1,R1               R1=REC LEN                                   
         ICM   R1,3,0(R2)                                                       
         A     R1,FILSIZE          ADD TO FILSIZE                               
         ST    R1,FILSIZE                                                       
         B     NEXTREC             NEXT                                         
*                                                                               
NEXT02   L     R1,0(R3)            BUMP NUMBER OF FILES                         
         LA    R1,1(R1)                                                         
         ST    R1,0(R3)                                                         
         crap                                                                   
*                                                                               
         L     R1,FILSIZE          FILESIZE/32K                                 
         SRL   R1,16                                                            
         C     R1,=H'4'            4 IS MAX                                     
         BL    *+8                                                              
         LA    R1,4                                                             
         SLL   R1,2                R1=R1*4                                      
         LA    R1,4(R3,R1)                                                      
         L     RF,0(R1)            BUMP SIZE COUNT                              
         LA    RF,1(RF)                                                         
         ST    RF,0(R1)                                                         
         B     NEXTREC             NEXT                                         
*                                                                               
EODT1    MVI   EOF,C'Y'                                                         
         B     PRINTOUT                                                         
         EJECT                                                                  
*************************************************************                   
*        PRINT OUTPUT                                       *                   
*************************************************************                   
         SPACE 1                                                                
PRINTOUT MVC   PLINE,MYSPACES                                                   
         MVC   PLINE,HEAD1                                                      
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,HEAD2                                                      
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
         LA       R3,WRKF1                                                      
         MVC      PLINE+01(5),=C'WRKF1'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF2                                                      
         MVC      PLINE+01(5),=C'WRKF2'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF3                                                      
         MVC      PLINE+01(5),=C'WRKF3'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF4                                                      
         MVC      PLINE+01(5),=C'WRKF4'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF5                                                      
         MVC      PLINE+01(5),=C'WRKF5'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF6                                                      
         MVC      PLINE+01(5),=C'WRKF6'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF7                                                      
         MVC      PLINE+01(5),=C'WRKF7'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
*                                                                               
         LA       R3,WRKF8                                                      
         MVC      PLINE+01(5),=C'WRKF8'                                         
         EDIT     (B4,0(R3)),(7,PLINE+8),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT     (B4,4(R3)),(7,PLINE+15),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,8(R3)),(7,PLINE+22),ZERO=NOBLANK,ALIGN=LEFT               
         EDIT     (B4,12(R3)),(7,PLINE+29),ZERO=NOBLANK,ALIGN=LEFT              
         EDIT     (B4,16(R3)),(7,PLINE+36),ZERO=NOBLANK,ALIGN=LEFT              
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE,MYSPACES                                                   
*                                                                               
         B     CLOSEALL                                                         
         EJECT                                                                  
*************************************************************                   
*        ACCUMULATORS                                       *                   
*************************************************************                   
         SPACE 1                                                                
                                                                                
FILSIZE  DC    F'0'                                                             
*                                                                               
WRKFTAB  EQU   *                                                                
WRKF1    DC    F'0'                NUMBER                                       
WRKF1C   DC    F'0'                0-32K                                        
         DC    F'0'                32-64                                        
         DC    F'0'                64-96                                        
         DC    F'0'                128-160                                      
         DC    F'0'                160>                                         
*                                                                               
*                                                                               
WRKF2    DC    F'0'                                                             
WRKF2C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF3    DC    F'0'                                                             
WRKF3C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF4    DC    F'0'                                                             
WRKF4C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF5    DC    F'0'                                                             
WRKF5C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF6    DC    F'0'                                                             
WRKF6C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF7    DC    F'0'                                                             
WRKF7C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
WRKF8    DC    F'0'                                                             
WRKF8C   DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES AND EXIT                               *                   
*************************************************************                   
         SPACE 1                                                                
CLOSEALL CLOSE OLDTAPE                                                          
         CLOSE SYSPRINT                                                         
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
*                                                                               
PLINE    DS    CL132                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES AND CONSTANTS                                         *         
***********************************************************************         
         SPACE 1                                                                
MYSPACES DC    CL132' '                                                         
DASHES   DC    132C'-'                                                          
FFS      DS    0XL64                                                            
         DC    64X'FF'                                                          
DOTS     DC    32C'.'                                                           
SORTCARD DC    C'SORT FIELDS=(17,20,BI,A,5,9,BI,A) ' NAME,YR/WK/DY/TIME         
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(64,,,37,48) '                            
*                                                                               
HEAD1    DC    CL132' WRKF   Files >32k   >64k   >96k   >128k  >160k'           
HEAD2    DC    CL132' ----------------------------------------------'           
         EJECT                                                                  
***********************************************************************         
*        DCBS                                                         *         
***********************************************************************         
         SPACE 1                                                                
OLDTAPE  DCB   DDNAME=OLDTAPE,DSORG=PS,MACRF=(GL),EODAD=EODT1                   
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(133)          
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
         EJECT                                                                  
         DCBD  DSORG=QS,DEVD=DA                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DDTRMOREZ 05/04/00'                                      
         END                                                                    
