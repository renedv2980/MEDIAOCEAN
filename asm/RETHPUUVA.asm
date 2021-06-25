*          DATA SET RETHPUUVA  AT LEVEL 017 AS OF 08/31/00                      
*          DATA SET RETHPUUVA  AT LEVEL 016 AS OF 02/21/95                      
*PHASE ATHPURGA ATHPURGE                                                        
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ATHPURGE - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
*  EXTERN TO PURGE ATHENA RECORDS                                               
***********************************************************************         
*                                                                     *         
* ONE-SHOT TO BLOW AWAY ALL ATHENA RECORDS FOR UNIVISION...           *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
ATHPURGE CSECT                                                                  
         NMOD1 WORKX-WORKD,ATHPRGE,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         MVC   VPRINTER,16(R1)                                                  
         MVC   VCPRINT,20(R1)                                                   
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            DMXKEEP RECORD EXIT                          
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            DMXPURGE RECORD EXIT                         
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         SPACE 1                                                                
         L     R3,AREC                                                          
         USING RATNRECD,R3                                                      
DR10     DS    0H                                                               
         CLI   0(R3),X'27'         IS THIS AN ATHENA REC?                       
         BNE   DMXKEEP             NO, KEEP                                     
         CLC   RATNKREP,=C'UV'     UNIVISION?                                   
         BE    DMXPURGE            YES - PURGE                                  
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
         SPACE 2                                                                
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER MODULE WORKING STORAGE                                         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENATNA                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RETHPUUVA 08/31/00'                                      
         END                                                                    
