*          DATA SET WHTEST     AT LEVEL 032 AS OF 08/19/00                      
*PHASE WHTEST,*                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE NETWEEK                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PARSNIP                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE TICTOC                                                                 
WHTEST   TITLE 'WHTEST - MAKE FAKE XSPFILE RECORDS'                             
WHTEST   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,WHTEST,=V(REGSAVE),R9                                          
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
         L     RE,=V(ADDAY)                                                     
         ST    RE,VADDAY                                                        
         L     RE,=V(GETDAY)                                                    
         ST    RE,VGETDAY                                                       
*                                                                               
****     GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(WHTEST,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         MVC   FULL,=X'00061A80'                                                
         XR    R0,R0                                                            
         L     R1,FULL                                                          
         M     R0,=F'1000000'                                                   
         STM   R0,R1,DUB                                                        
         GOTO1 =V(HEXOUT),DMCB,DUB,P,8                                          
         GOTO1 =V(PRINTER)                                                      
         LM    R0,R1,DUB                                                        
         MVC   FULL,=X'0002F3DC'                                                
         L     RE,FULL                                                          
         DR    R0,RE                                                            
         STM   R0,R1,DUB                                                        
         GOTO1 =V(HEXOUT),DMCB,DUB,P,8                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     MAINX                                                            
         EJECT                                                                  
MAINX    XBASE                                                                  
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* DATA SETS                                                                     
***********************************************************************         
DUB      DS    D                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
WORK     DS    CL64                                                             
ELEM     DS    XL256                                                            
*                                                                               
PCKOF16B DS    PL16                                                             
PCKOF08B DS    PL8                                                              
*                                                                               
OPUTAREA DS    0CL4004             OUTPUT LINE FOR THE FILE                     
OPUTLGTH DS    XL2                     LENGTH OF DATA FOR THIS LINE             
         DS    XL2                     FOR QSAM MACRO                           
OPUTDATA DS    CL4000                  DATA FOR THIS LINE                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032WHTEST    08/19/00'                                      
         END                                                                    
